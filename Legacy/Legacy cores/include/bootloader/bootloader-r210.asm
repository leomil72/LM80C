; ------------------------------------------------------------------------------
; LM80C - BOOTLOADER - R2.10
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. Code and computer schematics are released under
; the therms of the GNU GPL License 3.0 and in the form of "as is", without no
; kind of warranty: you can use them at your own risk.
; You are free to use them for any non-commercial use: you are only asked to
; maintain the copyright notices, include this advice and the note to the 
; attribution of the original version to Leonardo Miliani, if you intend to
; redistribuite them.
; https://www.leonardomiliani.com
; 
; Please support me by visiting the following links:
; Main project page: https://www.leonardomiliani.com
; Schematics and code: https://github.com/leomil72/LM80C
; Videos about the computer: https://www.youtube.com/user/leomil72/videos
; Hackaday page: https://hackaday.io/project/165246-lm80c-color-computer
; ------------------------------------------------------------------------------
; Code Revision:
; R1.1   - 20190518
; R1.2   - 20190521 - Video cursor management - preliminary
; R1.3   - 20190601 - Cursor management integrated into VDP module
; R1.4   - 20190606 - Removed messages about wrong HW systems
; R1.9   - 20190620 - Aligned release version with firmware #
; R2.0   - 20190714 - Added SREG & SSTAT to write to/read from PSG
; R2.1   - 20190818 - Added SOUND command to play simple tones and VOLUME command
; R2.1a  - 20190908 - Cursor management improvements
; R2.2   - 20190920 - Fixed cursor bug within SCREEN statement; new command PAUSE
; R2.3   - 20190930 - Fixed bugs in SOUND command
; R2.4   - 20191013 - Added new graphic chars and reorganized previous ones
; R2.4a  - 20191015 - More graphic chars
; R2.5   - 20191026 - Revision of init PSG code; revision of serial buffer exp. code;
;                     fixed a bug into the video buffer manager
; R2.6   - 20191102 - New function INKEY to read a key without a prompt;
;                     source code cleaning
; R2.7   - 20191116 - Fixed a bug into the INKEY code
; R2.8   - 20191207 - Minor bug fixes; added support for built-in keyboard;
;                     revision of some char codes;
; R2.8.1 - 20191208 - Introduced support for SHIFT key for uppercase letters & alternate chars
; R2.8.2 - 20191215 - Fixed a bug introduced with 2.8.1 that lead to wrong functioning of
;                     several BASIC statements (system tick counter, Locate, etc..)
; R2.9   - 20191222 - Code cleaning; improved SOUND statement; revision of PSG code;
;                     revision of release notes; add support for cursor keys & cursor movements
; R2.10  - 20191226 - SIO init code cleaning & improved support for serial RX; added extended
;                     char codes (128-255) for 6x8 fonts; removed double chars in 8x8 fonts
;
; ------------------------------------------------------------------------------

; ADDRESS DECODING (bits A6/A5/A4)
; 0000xxxx : PIO
; 0001xxxx : CTC
; 0010xxxx : SIO
; 0011xxxx : VDP
; 0100xxxx : PSG

; labels defining for PIO (Parallel Input/Output)
PIO_DA      equ 00000000b
PIO_DB      equ 00000001b
PIO_CA      equ 00000010b
PIO_CB      equ 00000011b

; label defining for CTC (Counter Timer Circuit)
CTC_CH0     equ 00010000b
CTC_CH1     equ 00010001b
CTC_CH2     equ 00010010b
CTC_CH3     equ 00010011b

;label defining for SIO (Serial Input/Output)
SIO_CA      equ 00100010b
SIO_CB      equ 00100011b
SIO_DA      equ 00100000b
SIO_DB      equ 00100001b

;label defining for VDP (Video Display Processor)
VDP_DAT     equ 00110000b
VDP_SET     equ 00110010b

; label defining for PSG (Programmable Sound Generator)
PSG_REG     equ 01000000b
PSG_DAT     equ 01000001b

; Interrupt-driven serial I/O interface lead by the Z80 SIO to run modified
; NASCOM Basic 4.7 - Full input buffering with incoming data hardware handshaking
; Handshake shows full before the buffer is totally filled to allow run-on from the sender

SER_BUFSIZE     equ     $3f
SER_FULLSIZE    equ     $30
SER_EMPTYSIZE   equ     $05

SERBUF_START    equ     $8000   ; RAM starts here
serInPtr        equ     SERBUF_START + SER_BUFSIZE
serRdPtr        equ     serInPtr+2
serBufUsed      equ     serRdPtr+2
basicStarted    equ     serBufUsed+1
bufWrap         equ     (SERBUF_START + SER_BUFSIZE) & $ff
TEMPSTACK       equ     CURPOS - 3 ; top of BASIC line input buffer so is "free ram" when BASIC resets

;------------------------------------------------------------------------------
; BASE MEMORY - RESET LOCATION - $0000
; the CPU jumps to 0000h after a reset
                org     $00
RST00:          di                      ; be sure that INTs are disabled
                jp      INIT_HW         ; jump to system initialization

;------------------------------------------------------------------------------
; send a character over serial
                org     $08
RST08:          jp      TXA

;------------------------------------------------------------------------------
; interrupt vector when SIO has a char available in its buffer
                org     $0C
                defw    RX_CHA_AVAILABLE

;------------------------------------------------------------------------------
; interrupt vector for SIO special conditions (i.e. buf overrun)
                org     $0E
                defw    SPEC_RX_CONDITON

;------------------------------------------------------------------------------
; receive a character over serial
                org     $10
RST10:          jp      RXA

;------------------------------------------------------------------------------
; interrupt vector for CH3 Timer - used for 100ths/s counter
                org     $16
                defw    CH3_TIMER

;------------------------------------------------------------------------------
; check serial status

                org     $18
RST18:          jp      CKINCHAR

;------------------------------------------------------------------------------
; interrupt routine for NMI (actually NOT used)
                org     $66
                retn                    ; return from NMI

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; interrupt driven routine to get chars from Z80 SIO
                org     $100
RX_CHA_AVAILABLE:
                push    AF              ; store A
                push    HL              ; and HL
                call    A_RTS_OFF
                in      A,(SIO_DA)      ; read char from RX buffer into A
                ld      (TMPKEYBFR),A   ; store it into the temp key buffer
                call    CHARINTOBFR     ; sub-routine to store the char
                pop     HL              ; recover HL
                pop     AF              ; and A
                ei                      ; re-enable interrupts
                reti                    ; and exit

; put a char into the input buffer, char is into A
; this sub is called both from the ISR "RX_CHA_AVAILABLE" and from the ISR
; that scans the built-in keyboard
CHARINTOBFR:    push    AF              ; store it
                ld      A,(serBufUsed)  ; load buffer size
                cp      SER_BUFSIZE     ; if buffer is not full
                jr      NZ,NOTFULL      ; then store the char
                pop     AF              ; else drop it
                jr      CHARINTOBFREXT  ; and exit
NOTFULL:        ld      HL,(serInPtr)   ; buffer is not full, can store the char
                inc     HL              ; load pointer to find first free cell
                ld      A,L             ; only check low byte because buffer<256
                cp      bufWrap         ; check if the pointer is at the last cell
                jr      NZ,NOTWRAP      ; if not then continue
                ld      HL,SERBUF_START ; else load the address of the first cell
NOTWRAP:        ld      (serInPtr),HL   ; store the new pointer
                pop     AF              ; then recover the char
                ld      (HL),A          ; and store it in the appropriate cell
                ld      A,(serBufUsed)  ; load the size of the serial buffer
                inc     A               ; increment it
                ld      (serBufUsed),A  ; and store the new size
                cp      SER_FULLSIZE    ; check if serial buffer is full
                jr      C,CHARINTOBFREXT; exit if buffer is not full
                call    A_RTS_OFF       ; else stop receiving further chars
CHARINTOBFREXT: ret                     ; return to caller

;-------------------------------------------------------------------------------
; Z80 SIO MANAGEMENT
;-------------------------------------------------------------------------------
; disable RTS:
; by resetting RTS bit (set to 0), the RTS line is disabled (HIGH)
A_RTS_OFF:
                push    AF              ; store A
                ld      A,00000101b     ; write into WR0: select WR5
                out     (SIO_CA),A
                ld      A,11101000b     ; 8 bits/TX char; TX enable; RTS disable
                out     (SIO_CA),A
                pop     AF              ; retrieve A
                ret                     ; exit

; enable RTS
; by setting RTS bit (set to 1), the RTS line is enabled (LOW)
A_RTS_ON:
                push    AF              ; store A
                ld      A,00000101b     ; write into WR0: select WR5
                out     (SIO_CA),A
                ld      A,11101010b     ; 8 bits/TX char; TX enable; RTS enable
                out     (SIO_CA),A
                pop     AF              ; retrieve A
                ret                     ; return

; disable SIO RX channel A
SIO_A_DI:
                push    AF              ; store A
                ld      A,00000011b     ; write into WR0: select WR3
                out     (SIO_CA),A
                ld      A,00001100b     ; write into WR3: RX disable;
                out     (SIO_CA),A
                pop     AF              ; retrieve A
                ret                     ; return

; enable SIO RX channel A
SIO_A_EI:
                push    AF              ; store A
                ld      A,00000011b     ; write into WR0: select WR3
                out     (SIO_CA),A
                ld      A,11000001b     ; 8 bits/RX char; auto enable OFF; RX enable
                out     (SIO_CA),A
                pop     AF              ; retrieve A
                ret

; special SIO condition (i.e., buffer overrun)
SPEC_RX_CONDITON:
                push    BC              ; if buffer overrun then show an error
                ld      B,$80           ; LED on
                ld      C,PIO_DA
                out     (C),B
                pop     BC
                ei
                reti

;------------------------------------------------------------------------------
; retrieve A char from the serial buffer
RXA:
WAITFORCHAR:    ld      A,(serBufUsed)  ; load the buffer size
                cp      $00             ; check if it's 0 (empty)
                jr      Z,WAITFORCHAR   ; if it's empty, wait for a char
                di                      ; disable interrupts
                push    HL              ; store HL
                ld      HL,(serRdPtr)   ; load pointer to first available char
                inc     HL              ; increment it (go to the next char)
                ld      A,L             ; check if the end of the buffer has been reached
                cp      bufWrap         ; (only check low byte because buffer<256)
                jr      NZ,NOTRDWRAP    ; if not, jump straight
                ld      HL,SERBUF_START ; else reload the starting address of the buffer
NOTRDWRAP:      ld      (serRdPtr),HL   ; store new pointer to the next char to read
                ld      A,(serBufUsed)  ; load buffer size
                dec     A               ; decrement it
                ld      (serBufUsed),A  ; and store the new size
                cp      SER_EMPTYSIZE   ; check if serial buffer can be considered empty
                jr      nc,RXA_EXIT     ; if not empty yet, then exit
                call    A_RTS_ON        ; else re-enable receiving chars
RXA_EXIT:       ld      A,(HL)          ; recover the char and return it into A
                pop     HL              ; recover H
                ei                      ; re-enable interrupts
                ret                     ; return

;------------------------------------------------------------------------------
; sends a char over the serial
TXA:            di                      ; disable INTs
                out     (SIO_DA),A      ; send char to the SIO
                ld      (CHR4VID),A     ; store char
                ld      A,(PRNTVIDEO)   ; load status of print-on-video
                cp      $01             ; is the print on video on?
                jr      NZ,TXA_EXIT     ; no, jump over
                call    CHAR2VID        ; yes, send char to video buffer
TXA_EXIT:       call    TX_EMP          ; wait for outgoing char to be sent
                ei                      ; enable INTs
                ret                     ; return

;------------------------------------------------------------------------------
; wait until outgoing serial has been sent
TX_EMP:         sub     A               ; set A to 0
                inc     A               ; set A to 1
                out     (SIO_CA),A      ; write to WR0, select RR1
                in      A,(SIO_CA)      ; read RR1 register
                bit     0,A             ; check if all chars have been sent
                jp      Z,TX_EMP        ; if not (bit 0 = 0) then retrieve
                ret                     ; else exit


;------------------------------------------------------------------------------
; check if there is some chars into the buffer
CKINCHAR        ld      A,(serBufUsed)  ; load char in buffer
                cp      $00             ; compare to 0
                ret                     ; return

;------------------------------------------------------------------------------
; print a text from memory, and terminate when $00 is found
RAWPRINT:       ld      A,(HL)          ; load character from memory cell pointed by HL
                or      A               ; is it $00 (end string)?
                ret     Z               ; Yes, then return
                rst     $08             ; No, print it
                inc     HL              ; and select the next one
                jr      RAWPRINT        ; repeat

;-------------------------------------------------
; Interrupt service routine (ISR) for CH3 timer
; this is used to increment the 100ths of a second counter and for cursor flashing
CH3_TIMER:      push    AF              ; save regs. A,
                push    BC              ; BC,
                push    DE              ; DE,
                push    HL              ; HL
                xor     A
                ld      HL,TMRCNT       ; load starting address of the timer
                ld      B,$04           ; 4 bytes to check
INCTMR3:        inc     (HL)            ; increment timer
                cp      (HL)            ; compare (HL) with A (is it 0?)
                jr      NZ,CHKCRSR      ; if not zero then exit (finished increment)
                inc     HL              ; if yes, there was an overflow, so increment next byte
                djnz    INCTMR3         ; repeat for 4 bytes
CHKCRSR:        ld      A,(CRSR_STATE)  ; now, check the cursor
                cp      $00             ; cursor off?
                call    NZ,FLASHCURSOR  ; no, so flash the cursor
                call    MNGSNDS         ; call the tone managemenet
                call    KEYBOARD        ; read the keyboard inputs
                pop     HL              ; retrieve HL,
                pop     DE              ; DE,
                pop     BC              ; BC,
                pop     AF              ; and A
                ei                      ; re-enable interrupts
                reti                    ; exit from ISR

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; HARDWARE INITIALISATION
; first run - setup HW & SW
;
INIT_HW:        ld      HL,TEMPSTACK    ; load temp stack pointer
                ld      SP,HL           ; set stack to temp stack pointer
                ld      HL,SERBUF_START ; set beginning of serial buffer
                ld      (serInPtr),HL   ; for incoming chars to store into buffer
                ld      (serRdPtr),HL   ; and for chars to be read from buffer
                xor     A               ; reset A
                ld      (serBufUsed),A  ; actual buffer size is 0
                call    HELLOWRLD       ; little serial blink with LEDs
                call    initCTC         ; configure CTC, then...
                call    initSIO         ; ...configure SIO, then...
                call    initPSG         ; ...configure PSG
                ld      DE,$0101        ; D must be set to 1 (to show the computer name), while E chooses the video mode:
                                        ; 0: text; 1:graphics 1; 2:graphics 2; 3:multicolor
                call    initVDP         ; set video display
                xor     A
                ld      I,A             ; set high byte of interrupt vectors to point to page 0
                im      2               ; interrupt mode 2
                ei                      ; enable interrupts
                call    DELAY           ; little delay to give the SIO the time to receive spare chars
                                        ;
EMPTY_SER_BUF:  call    CKINCHAR        ; check if something has arrived into the serial buffer
                jr      Z,PRINTWELCOME  ; no, it's empty: jump over
                call    RXA             ; yes, there is something, so discard it
                jr      EMPTY_SER_BUF   ; retry until buffer is empty
PRINTWELCOME:   ld      A,$01           ; activate the
                ld      (PRNTVIDEO),A   ; buffer video
                ld      HL,MSGTXT1      ; sign-on message
                call    RAWPRINT        ; print message
                ld      A,(basicStarted); check if BASIC is already started
                cp      'Y'             ; to see if this is a power-up
                jr      NZ,COLDSTART    ; if not, then do a COLD start
                ld      HL,MSGTXT2      ; print message to choose start
                call    CURSOR_ON       ; enable cursor
                call    RAWPRINT
CORW:           call    RXA             ; wait for a char
                and     11011111b       ; only UPPERCASE char
                cp      'C'             ; cold start?
                jr      NZ,CHECKWARM    ; no, let's check for warm start
                call    ECHOCAR         ; echoes the char
COLDSTART:      ld      A,'Y'           ; yes, set the "BASIC started" flag
                ld      (basicStarted),A
                jp      COLD            ; start BASIC COLD
CHECKWARM:      cp      'W'
                jr      NZ,CORW         ; char not recognized, wait again
                call    ECHOCAR         ; echoes the char
                jp      WARM            ; start BASIC WARM

DELAY:          ld      B,$00           ; give the SIO...
WASTETIME       nop                     ; ...some time to...
                djnz    WASTETIME       ; ...receive spare chars
                ret
;-------------------------------------------------------------------------------
ECHOCAR:        rst     $08             ; echoes back the pressed key,
                ld      A,CR            ; then CR
                rst     $08
                ld      A,LF            ; and LF chars
                rst     $08
                ret
;-------------------------------------------------------------------------------
; little serial blink with LEDs
HELLOWRLD:
                ld      C,9             ; 8 LEDs to be turned off + 1 more step to turn off the last LED
                ld      A,11001111b     ; set PIO port B to mode 4 (mode control)
                out     (PIO_CB),A
                ld      A,$00           ; set pins of PIO port B to OUTPUT
                out     (PIO_CB),A
                ld      A,$01           ; LSB on
LEDLIGHT:       out     (PIO_DB),A      ; turn LEDs on/off for a "Supercar" sequence
                ld      E,$20           ; little delay
DEC_E:          ld      B,$00
COUNTER:        djnz    COUNTER
                dec     E
                jr      NZ,DEC_E        ; finish delay
                sla     A               ; shift reg.A to left 1 bit
                dec     C               ; next LED
                jr      NZ,LEDLIGHT     ; all LEDs done? no, repeat
                ret

;-------------------------------------------------------------------------------
; Z80 SIO setting up
initSIO:
                ;set up TX and RX:
                ; the followings are settings for channel A
                ld      HL,SIO_A_SETS   ; settings for SIO ch. A
                ld      B,$06           ; 6 bytes to send
                ld      C,SIO_CA        ; I/O address of SIO ch.A
                otir                    ; send bytes to SIO
                ; the following are settings for channel B (don't need to load HL since settings are contigous)
                ld      B,$04           ; other 4 bytes to send
                ld      C,SIO_CB        ; I/O address of SIO ch.B
                otir                    ; send bytes to SIO
                ; the following are settings for channel A
                ld      A,$01           ; write into WR0: select WR1
                out     (SIO_CA),A
                ld      A,00011000b     ; interrupts on every RX char; parity is no special condition;
                                        ; buffer overrun is special condition
                out     (SIO_CA),A
                call    SIO_A_EI        ; enable RX on SIO channel A
                ret

SIO_A_SETS:     defb    00110000b       ; write into WR0: error reset, select WR0
                defb    00011000b       ; write into WR0: channel reset
                defb    00000100b       ; write into WR0: select WR4
                defb    01000100b       ; write into WR4: presc. 16x, 1 stop bit, no parity
                defb    00000101b       ; write into WR0: select WR5
                defb    11101000b       ; write into WR5: DTR on, TX 8 bits, BREAK off, TX on, RTS off
SIO_B_SETS:     defb    00000001b       ; write into WR0: select WR1
                defb    00000100b       ; write into WR0: status affects interrupt vectors
                defb    00000010b       ; write into WR0: select WR2
                defb    $00             ; write into WR2: set interrupt vector, but bits D3/D2/D1 of this vector
                                        ; will be affected by the channel & condition that raised the interrupt
                                        ; (see datasheet): in our example, 0x0C for Ch.A receiving A char, 0x0E
                                        ; for special conditions
;------------------------------------------------------------------------------
; Z80 CTC setting up
initCTC:
;init CH0
;CH0 provides to SIO SERIAL A the RX/TX clock
                ld      A,01000111b     ; interrupt off, counter mode, prsc=16 (doesn't matter), ext. start,
                                        ; start upon loading time constant, time constant follows, sw reset, command word
                out     (CTC_CH0),A
                ld      A,$06           ; time constant 6
                out     (CTC_CH0),A
; TO0 output frequency=INPUT CLK/time constant
; which results in 1,843,200/6 = 307,200 Hz because the CTC needs that RX/TX
; clock is 16 times the requested baud rate (in our case, 19,200 x 16 = 307,200 Hz)

;CH1 & CH2 disabled (they are not used yet in this program)
                ld      A,00000011b     ; interrupt off, timer mode, prescaler=16, don't care ext. TRG edge,
                                        ; start timer on loading constant, no time constant follows, software reset, command word
                out     (CTC_CH1),A     ; set CH1
                out     (CTC_CH2),A     ; set CH2

;init CH3
;CH3 divides CPU CLK by 144*256 providing an interrupt signal at 100 Hz (1/100 sec).
;f = CPU_CLK/(144*256) => 3,686,400 / ( 36,864 ) => 100Hz
                ld      A,10100111b     ; interrupt on; timer mode; prescaler=256; don't care ext; automatic trigger;
                                        ; time constant follows; cont. operation; command word
                out     (CTC_CH3),A
                ld      A,0x90          ; time constant - 90$ (144d)
                out     (CTC_CH3),A
                ld      A,00010000b     ; D7..D3 provide the first part of the int vector (in our case, $10), followed by
                                        ; D2..D1, provided by the CTC (they point to the channel), d0=interrupt word
                                        ; so int vector is 00010xx00
                out     (CTC_CH0),A     ; send to CTC
                ; reset cells of 100ths of a second counter
                xor     A               ; reset A
                ld      HL,TMRCNT       ; load TMR pointer
                ld      B,4             ; 4 memory cells
RESTMR:         ld      (HL),A          ; reset n-cell of TMR
                inc     HL
                djnz    RESTMR          ; repeat for 4 cells
                ret

;------------------------------------------------------------------------------
MSGTXT1:        defm    "LM80C by Leonardo Miliani",CR,LF
                defm    "Firmware R2.10",CR,LF,0
MSGTXT2:        defb    CR,LF
                defm    "<C>old or <W>arm start? ",0
