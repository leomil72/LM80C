; ------------------------------------------------------------------------------
; LM80C - BOOTLOADER - R2.0
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Code Revision:
; R1.1 - 20190518
; R1.2 - 20190521 - Video cursor management - preliminary
; R1.3 - 20190601 - Cursor management integrated into VDP module
; R1.4 - 20190606 - Removed messages about wrong HW systems
; R1.9 - 20190620 - Aligned release version with firmware #
; R2.0 - 20190714 - Added SREG & SSTAT to write to/read from PSG
;
; ------------------------------------------------------------------------------

; ADDRESS DECODING (bits A6/A5/A4)
; x000xxxx : PIO
; x001xxxx : CTC
; x010xxxx : SIO
; x011xxxx : VDP
; x100xxxx : PSG

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
                org $00
RST00:          di           ; be sure that INTs are disabled
                jp INIT_HW   ; jump to system initialization

;------------------------------------------------------------------------------
; send a character over serial
                org $08
RST08:          jp TXA

;------------------------------------------------------------------------------
; interrupt vector when SIO has a char available in its buffer
                org $0C
                defw RX_CHA_AVAILABLE

;------------------------------------------------------------------------------
; interrupt vector for SIO special conditions (i.e. buf overrun)
                org $0E
                defw SPEC_RX_CONDITON

;------------------------------------------------------------------------------
; receive a character over serial
                org $10
RST10:          jp RXA

;------------------------------------------------------------------------------
; interrupt vector for CH3 Timer - used for 100ths/s counter
                org $16
                defw CH3_TIMER

;------------------------------------------------------------------------------
; check serial status

                org $18
RST18:          jp CKINCHAR

;------------------------------------------------------------------------------
; interrupt routine for NMI (actually NOT used)
                org $66
                ei                  ; re-enable interrupts
                retn                ; return from NMI
;------------------------------------------------------------------------------
; interrupt driven routine to get chars from Z80 SIO
RX_CHA_AVAILABLE:
                push af             ; store A & HL
                push hl
                in a,(SIO_DA)       ; read char from RX buffer into A
                push af             ; store it
                ld a,(serBufUsed)   ; load buffer size
                cp SER_BUFSIZE      ; if buffer is not full
                jr nz,NOTFULL       ; then store the char
                pop af              ; else drop it
                jr RX_CH_AV_EXIT    ; and exit
NOTFULL:        ld hl,(serInPtr)    ; buffer is not full, can store the char
                inc hl              ; load pointer to find first free cell
                ld a,l              ; only check low byte because buffer<256
                cp bufWrap          ; check if the pointer is at the last cell
                jr nz,NOTWRAP       ; if not then continue
                ld hl,SERBUF_START  ; else load the address of the first cell
NOTWRAP:        ld (serInPtr),hl    ; store the new pointer
                pop af              ; then recover the char
                ld (hl),a           ; and store it in the appropriate cell
                ld a,(serBufUsed)   ; load the size of the serial buffer
                inc a               ; increment it
                ld (serBufUsed),a   ; and store the new size
                cp SER_FULLSIZE     ; check if serial buffer is full
                jr c,RX_CH_AV_EXIT  ; exit if buffer is not full
                call A_RTS_OFF      ; else stop receiving further chars
RX_CH_AV_EXIT:  pop hl              ; recover HL & A
                pop af
                ei                  ; re-enable interrupts
                reti                ; and exit

;-------------------------------------------------------------------------------
; Z80 SIO MANAGEMENT
;-------------------------------------------------------------------------------
; disable RTS:
; by resetting RTS bit (set to 0), the RTS line is disabled (HIGH)
A_RTS_OFF:
                push af             ; store A
                ld a,00000101b      ; write into WR0: select WR5
                out (SIO_CA),a
                ld a,11101000b      ; 8 bits/TX char; TX enable; RTS disable
                out (SIO_CA),a
                pop af              ; retrieve A
                ret                 ; exit

; enable RTS
; by setting RTS bit (set to 1), the RTS line is enabled (LOW)
A_RTS_ON:
                push af             ; store A
                ld a,00000101b      ; write into WR0: select WR5
                out (SIO_CA),a
                ld a,11101010b      ; 8 bits/TX char; TX enable; RTS enable
                out (SIO_CA),a
                pop af              ; retrieve A
                ret                 ; return

; disable SIO RX channel A
SIO_A_DI:
                push af             ; store A
                ld a,00000011b      ; write into WR0: select WR3
                out (SIO_CA),a
                ld a,00001100b      ; write into WR3: RX disable;
                out (SIO_CA),a
                pop af              ; retrieve A
                ret                 ; return

; enable SIO RX channel A
SIO_A_EI:
                push af             ; store A
                ld a,00000011b      ; write into WR0: select WR3
                out (SIO_CA),a
                ld a,11000001b      ; 8 bits/RX char; auto enable OFF; RX enable
                out (SIO_CA),a
                pop af              ; retrieve A
                ret

; special SIO condition (i.e., buffer overrun)
SPEC_RX_CONDITON:
                jp 0000h            ; if buffer overrun then restart the system

;------------------------------------------------------------------------------
; retrieve a char from the serial buffer
RXA:
WAITFORCHAR:    ld a,(serBufUsed)   ; load the buffer size
                cp $00              ; check if is 0 (empty)
                jr z,WAITFORCHAR    ; if it's empty, wait for a char
                di                  ; disable interrupts
                push hl             ; store HL
                ld hl,(serRdPtr)    ; load pointer to first available char
                inc hl              ; increment it (go to the next char)
                ld a,l              ; check if the end of the buffer has been reached
                cp bufWrap          ; (only check low byte because buffer<256)
                jr nz,NOTRDWRAP     ; if not, jump straight
                ld hl,SERBUF_START  ; else reload the starting address of the buffer
NOTRDWRAP:      ld (serRdPtr),hl    ; store new pointer to the next char to read
                ld a,(serBufUsed)   ; load buffer size
                dec a               ; decrement it
                ld (serBufUsed),a   ; and store the new size
                cp SER_EMPTYSIZE    ; check if serial buffer can be considered empty
                jr nc,RXA_EXIT      ; if not empty yet, then exit
                call A_RTS_ON       ; else re-enable receiving chars
RXA_EXIT:       ld a,(hl)           ; recover the char and return it into A
                pop hl              ; recover H
                ei                  ; re-enable interrupts
                ret                 ; return

;------------------------------------------------------------------------------
; sends a char over the serial
TXA:            di                  ; disable INTs
                out (SIO_DA),a      ; send char to the SIO
                ld (CHR4VID),a      ; store char
                ld a,(PRNTVIDEO)    ; load status of print-on-video
                cp $01              ; is the print on video on?
                jr nz,TXA_EXIT      ; no, jump over
                call CHAR2VID       ; yes, send char to video buffer
TXA_EXIT:       call TX_EMP         ; wait for outgoing char to be sent
                ei                  ; enable INTs
                ret                 ; return

;------------------------------------------------------------------------------
; wait until outgoing serial has been sent
TX_EMP:         sub a               ; set A to 0
                inc a               ; set A to 1
                out (SIO_CA),a      ; write to WR0, select RR1
                in a,(SIO_CA)       ; read RR1 register
                bit 0,a             ; check if all chars have been sent
                jp z,TX_EMP         ; if not (bit 0 = 0) then retrieve
                ret                 ; else exit


;------------------------------------------------------------------------------
; check if there is some chars into the buffer
CKINCHAR        ld a,(serBufUsed)   ; load char in buffer
                cp $00              ; compare to 0
                ret                 ; return

;------------------------------------------------------------------------------
; print a text from memory, and terminate when $00 is found
RAWPRINT:       ld a,(hl)           ; load character from memory cell pointed by HL
                or a                ; is it $00 (end string)?
                ret z               ; Yes, then return
                rst $08             ; No, print it
                inc hl              ; and select the next one
                jr RAWPRINT         ; repeat

;-------------------------------------------------
; Interrupt service routine (ISR) for CH3 timer
; this is used to increment the 100ths of a second counter and for cursor flashing
CH3_TIMER:      push af             ; save regs. A,
                push bc             ; BC,
                push de             ; DE,
                push hl             ; HL,
                xor a               ; clear A and flags
                ld hl,TMRCNT        ; load starting address of the timer
                ld b,$04            ; 4 bytes to check
INCTMR3:        inc (hl)            ; increment timer
                cp (hl)             ; compare (hl) with A (is it 0?)
                jr nz,CHKCRSR       ; if not zero then exit (finished increment)
                inc hl              ; if yes, there was an overflow, so increment next byte
                djnz INCTMR3        ; repeat for 4 bytes
CHKCRSR:        ld a,(CRSR_STATE)   ; now, check the cursor
                cp $00              ; cursor off?
                jr z,EXCH3T         ; yes, then exit
                call FLASHCURSOR    ; flash the cursor
EXCH3T:         pop hl              ; recover HL
                pop de              ; recover DE
                pop bc              ; BC,
                pop af              ; and A
                ei                  ; re-enable interrupts
                reti                ; exit from ISR

;------------------------------------------------------------------------------
; HARDWARE INITIALISATION
; first run - setup HW & SW
;
INIT_HW:        ld hl,TEMPSTACK         ; load temp stack pointer
                ld sp,hl                ; set stack to temp stack pointer
                ld hl,SERBUF_START      ; set beginning of serial buffer
                ld (serInPtr),hl        ; for incoming chars to store into buffer
                ld (serRdPtr),hl        ; and for chars to be read from buffer
                xor a                   ; reset A
                ld (serBufUsed),a       ; actual buffer size is 0
                call HELLOWRLD          ; little serial blink with LEDs
                call initCTC            ; configure CTC, then...
                call initSIO            ; configure SIO
                ld de,$0101             ; D must be set to 1 (to show the computer name), while E chooses the video mode:
                                        ; 0: text; 1:graphics 1; 2:graphics 2; 3:multicolor
                call initVDP            ; set video display
                xor a                   ; set high byte of interrupt vectors
                ld i,a                  ; to point to page 0
                im 2                    ; interrupt mode 2
                ei                      ; enable interrupts
                ld b,$00                ; give the SIO...
WASTETIME       nop                     ; ...some time to...
                djnz WASTETIME          ; ...receive spare chars
                                        ;
EMPTY_SER_BUF:  call CKINCHAR           ; check if something has arrived into the serial buffer
                jr z,PRINTWELCOME       ; no, it's empty: jump over
                call RXA                ; yes, there is something, so discard it
                jr EMPTY_SER_BUF        ; retry until buffer is empty
PRINTWELCOME:   ld a,$01                ; activate the
                ld (PRNTVIDEO),a        ; buffer video
                call CURSOR_OFF         ; cursor off
                ld hl,MSGTXT1           ; sign-on message
                call RAWPRINT           ; print message
                call CURSOR_ON          ; set cursor on
                ld a,(basicStarted)     ; check if BASIC is already started
                cp 'Y'                  ; to see if this is a power-up
                jr nz,COLDSTART         ; if not, then do a COLD start
                ld hl, MSGTXT2          ; print message to choose start
                call RAWPRINT

CORW:           call RXA                ; wait for a char
                and 11011111b           ; only UPPERCASE char
                cp 'C'                  ; cold start?
                jr nz,CHECKWARM         ; no, let's check for warm start
COLDSTART:      call ECHOCAR            ; echoes the char
                ld a,'Y'                ; yes, set the "BASIC started" flag
                ld (basicStarted),a
                jp COLD                 ; start BASIC COLD
CHECKWARM:      cp 'W'
                jr nz,CORW              ; char not recognized, wait again
                call ECHOCAR            ; echoes the char
                jp WARM                 ; start BASIC WARM

;-------------------------------------------------------------------------------
ECHOCAR:        rst $08                 ; echoes back the pressed key,
                ld a,CR                 ; then CR
                rst $08
                ld a,LF                 ; and LF chars
                rst $08
                ret
;-------------------------------------------------------------------------------
; little serial blink with LEDs
HELLOWRLD:
                ld c,9                  ; 8 LEDs to turn off (1 more step to turn off the last LED)
                ld a,11001111b          ; set PIO port B to mode 4 (mode control)
                out (PIO_CB),a
                ld a,$00                ; set pins of PIO port B to OUTPUT
                out (PIO_CB),a
                ld a,$01                ; LSB on
LEDLIGHT:       out (PIO_DB),a          ; turn LEDs on/off for a "Supercar" sequence
                ld e,$20                ; little delay
DEC_E:          ld b,$00
COUNTER:        djnz COUNTER
                dec e
                jr nz,DEC_E             ; finish delay
                sla a                   ; shift reg.A to left 1 bit
                dec c                   ; next LED
                jr nz,LEDLIGHT          ; all LEDs done? no, repeat
                ret

;-------------------------------------------------------------------------------
; Z80 SIO setting up
initSIO:
                ;set up TX and RX:
                ; the followings are settings for channel A
                ld a,00110000b          ; write into WR0: error reset, select WR0
                out (SIO_CA),a
                ld a,00011000b          ; write into WR0: channel reset
                out (SIO_CA),a
                ld a,00000100b          ; write into WR0: select WR4
                out (SIO_CA),a
                ld a,01000100b          ; write into WR4: presc. 16x, 1 stop bit, no parity
                out (SIO_CA),a
                ld a,00000101b          ; write into WR0: select WR5
                out (SIO_CA),a
                ld a,11101000b          ; write into WR5: DTR on, TX 8 bits, BREAK off, TX on, RTS off
                out (SIO_CA),a
                ; the following are settings for channel B
                ld a,00000001b          ; write into WR0: select WR1
                out (SIO_CB),a
                ld a,00000100b          ; write into WR0: status affects interrupt vectors
                out (SIO_CB),a
                ld a,00000010b          ; write into WR0: select WR2
                out (SIO_CB),a
                ld a,$00                ; write into WR2: set interrupt vector, but bits D3/D2/D1 of this vector
                                        ; will be affected by the channel & condition that raised the interrupt
                                        ; (see datasheet): in our example, 0x0C for Ch.A receiving a char, 0x0E
                                        ; for special conditions
                out (SIO_CB),a
                ; the following are settings for channel A
                ld a,$01                ; write into WR0: select WR1
                out (SIO_CA),a
                ld a,00011000b          ; interrupts on every RX char; parity is no special condition;
                                        ; buffer overrun is special condition
                out (SIO_CA),a
                call SIO_A_EI           ; enable RX on SIO channel A
                ret

;------------------------------------------------------------------------------
; Z80 CTC setting up
initCTC:
;init CH0
;CH0 provides to SIO SERIAL A the RX/TX clock
                ld a,01000111b          ; interrupt off, counter mode, prsc=16 (doesn't matter), ext. start,
                                        ; start upon loading time constant, time constant follows, sw reset, command word
                out (CTC_CH0),a
                ld a,$06                ; time constant 6
                out (CTC_CH0),a
; TO0 output frequency=INPUT CLK/time constant
; which results in 1,843,200/6 = 307,200 Hz because the CTC needs that RX/TX
; clock is 16 times the requested baud rate (in our case, 19,200 x 16 = 307,200 Hz)

;CH1 & CH2 disabled (they are not used yet in this program)
                ld a,00000011b      ; interrupt off, timer mode, prescaler=16, don't care ext. TRG edge,
                                    ; start timer on loading constant, no time constant follows, software reset, command word
                out (CTC_CH1),a     ; set CH1
                out (CTC_CH2),a     ; set CH2

;init CH3
;CH3 divides CPU CLK by 144*256 providing an interrupt signal at 100 Hz (1/100 sec).
;f = CPU_CLK/(144*256) => 3,686,400 / ( 36,864 ) => 100Hz
                ld a,10100111b      ; interrupt on; timer mode; prescaler=256; don't care ext; automatic trigger;
                                    ; time constant follows; cont. operation; command word
                out (CTC_CH3),a
                ld a,0x90           ; time constant - 90$ equals to 144d
                out (CTC_CH3),a
                ld a,00010000b      ; D7..D3 provide the first part of the int vector (in our case, $10), followed by
                                    ; D2..D1, provided by the CTC (they point to the channel), d0=interrupt word
                                    ; so int vector is 00010xx00
                out (CTC_CH0),a     ; send to CTC
                ; reset cells of 100ths of a second counter
                xor a               ; reset A
                ld hl,TMRCNT        ; load TMR pointer
                ld b,4              ; 4 memory cells
RESTMR:         ld (hl),a           ; reset n-cell of TMR
                inc hl
                djnz RESTMR         ; repeat for 4 cells
                ret

;------------------------------------------------------------------------------
MSGTXT1:        defm "LM80C by Leonardo Miliani",CR,LF
                defm "Firmware R1.9",CR,LF,0
MSGTXT2:        defb CR,LF
                defm "<C>old or <W>arm start? ",0
