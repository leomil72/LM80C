; ------------------------------------------------------------------------------
; LM80C - BOOTLOADER - 1.07
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

; ADDRESS DECODING (bits A6/A5/A4)
; 0000xxxx : PIO
; 0001xxxx : CTC
; 0010xxxx : SIO
; 0011xxxx : VDP
; 0100xxxx : PSG
; 0101xxxx : C.F.


; ------------------------------------------------------------------------------
;               EQUATES
; ------------------------------------------------------------------------------
; label defining for PIO (Parallel Input/Output)
PIO_DA          equ %00000000
PIO_DB          equ %00000001
PIO_CA          equ %00000010
PIO_CB          equ %00000011

; label defining for CTC (Counter Timer Circuit)
CTC_CH0         equ %00010000
CTC_CH1         equ %00010001
CTC_CH2         equ %00010010
CTC_CH3         equ %00010011

;label defining for SIO (Serial Input/Output)
SIO_CA          equ %00100010
SIO_CB          equ %00100011
SIO_DA          equ %00100000
SIO_DB          equ %00100001

;label defining for VDP (Video Display Processor)
VDP_DAT         equ %00110000
VDP_SET         equ %00110001

; label defining for PSG (Programmable Sound Generator)
PSG_REG         equ %01000000
PSG_DAT         equ %01000001

; Interrupt-driven serial I/O interface lead by the Z80 SIO to run modified
; NASCOM Basic 4.7 - Full input buffering with incoming data hardware handshaking
; Handshake shows full before the buffer is totally filled to allow run-on from the sender
SER_BUFSIZE     equ     $58
SER_FULLSIZE    equ     $50
SER_EMPTYSIZE   equ     $05


;------------------------------------------------------------------------------
;                                F I R M W A R E
;------------------------------------------------------------------------------
; BASE MEMORY - RESET LOCATION ($0000) -> the CPU jumps to $0000 after a reset
                org     $0000
RST00:          di                      ; be sure that INTs are disabled
                jp      ROM2RAM         ; jump to ROM/RAM switcher
    
;------------------------------------------------------------------------------
; interrupt vector when SIO ch.B has a char available in its buffer
                ;$0004
                defw    RX_CHB_AVAIL

;------------------------------------------------------------------------------
; interrupt vector for SIO ch.B special conditions (i.e. buf overrun)
                ;$0006
                defw    SPEC_RXB_CNDT

;------------------------------------------------------------------------------
; send a character over serial ch. A
                ;$0008
RST08:          jp      TXA
                BLOCK   1,$FF   ; filler

;------------------------------------------------------------------------------
; interrupt vector when SIO ch.A has a char available in its buffer
                ;$000C
                defw    RX_CHA_AVAIL

;------------------------------------------------------------------------------
; interrupt vector for SIO ch.A special conditions (i.e. buf overrun)
                ;$000E
                defw    SPEC_RXA_CNDT

;------------------------------------------------------------------------------
; receive a character over serial ch. A
                ;$0010
RST10:          jp      RXA
                BLOCK   5,$FF   ; filler
;------------------------------------------------------------------------------
; check buffer state

                ;$0018
RST18:          jp      CKINCHAR
                BLOCK   $25,$FF ; filler
;------------------------------------------------------------------------------
; interrupt vectors for CTC 
                ;$0040               ; for CH0 Timer - used in BASIC by serial 1 for bps
                defw    CTC0IV
                ;$0042               ; for CH1 Timer - used in BASIC by serial 2 for bps
                defw    CTC1IV
                ;$0044               ; for CH2 timer - unused
                defw    CTC2IV
                ;$0046               ; for CH3 Timer - used by FW for 100ths/s counter
                defw    CTC3IV
                BLOCK   $1E,$FF ; filler
;------------------------------------------------------------------------------
; interrupt routine for NMI
                ;$0066
                jp      NMIUSR              ; jump to execute NMI service routine
                BLOCK   $27,$FF ; filler
;------------------------------------------------------------------------------

                ;$0090
                defb    $4C,$4D,$38,$30,$43,$20,$36,$34
                defb    $4B,$20,$43,$4F,$4C,$4F,$52,$00
                defb    $43,$4F,$4D,$50,$55,$54,$45,$52
                defb    $20,$28,$32,$30,$32,$31,$29,$00
                defb    $44,$65,$73,$69,$67,$6E,$65,$64
                defb    $20,$62,$79,$00,$00,$00,$00,$00
                defb    $4C,$65,$6F,$6E,$61,$72,$64,$6F
                defb    $20,$4D,$69,$6C,$69,$61,$6E,$69
FWVER:          defm    "FW ",VERSION,$20,__DATE__,$20,__TIME__,$00
FMVEREND:       equ     $
                BLOCK   $100-FMVEREND,$FF   ; filler
;------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;               Z80 SIO CH. A MANAGEMENT
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; interrupt driven routine to get chars from Z80 SIO ch.A
; this is the only serial channel that can print received chars onto the screen
                ;$0100
RX_CHA_AVAIL:   push    AF              ; store A
                push    HL              ; and HL
                call    A_RTS_OFF       ; disable RTS line
                in      A,(SIO_DA)      ; read char from RX buffer into A
                ld      (TMPKEYBFR),A   ; store it into the temp key buffer
                call    CHARINTOBFR     ; sub-routine to put the char into the input buffer
                jp      NC,LVRXCHA      ; if buffer is full, then leave without doing anything else
                ld      A,(TMPKEYBFR)   ; retrieve char
                ld      (CHR4VID),A     ; write into buffer for video printing
                cp      CR              ; is it RETURN?
                jp      Z,CNTRXCHA      ; yes, continue
                cp      $20             ; is it another control char (code < 32)?
                jp      C,LVRXCHA       ; yes, leave w/o printing it on video nor sending back to serial
CNTRXCHA:       push    AF              ; store char
                xor     A
                ld      (KBDNPT),A      ; a char from serial is like a char printed by BASIC
                ld      A,(PRNTVIDEO)   ; load status of print-on-video
                cp      $01             ; is the print on video on?
                call    Z,CHAR2VID      ; yes, print on screen
                pop     AF              ; retrieve char
                call    TXA             ; send back to serial
LVRXCHA:        pop     HL              ; retrieve HL
                pop     AF              ; and A
                ei                      ; re-enable interrupts
                reti                    ; and exit

;-------------------------------------------------------------------------------
; special SIO ch.A condition (i.e., buffer overrun)
; if buffer overruns then show an error, empty the RX buffer and send
; a break char
SPEC_RXA_CNDT:  push    AF              ; store AF
                call    A_RTS_OFF       ; disable RTS
                call    SIO_A_DI        ; disable RX on ch. A
                ld      A,(SERIALS_EN)  ; load serial status
                res     2,A             ; disable RX on port 1
                ld      (SERIALS_EN),A  ; store new serial status 
                in      A,(PIO_DB)      ; read status LEDs
                set     4,A             ; set 5th pin ON
                out     (PIO_DB),A      ; send new setting
                ld      A,%00110000     ; write into WR0: error reset, select WR0
                out     (SIO_CA),A      ; send command to SIO
EMPTYCHABFR:    xor     A
                out     (SIO_CA),A      ; write to WR0, select RR0
                in      A,(SIO_CA)      ; read RR0 register
                and     $01             ; check if input buffer if empty
                jp      Z,CHABFREMPTY   ; if yes (bit 0 = 0) then leave
                in      A,(SIO_DA)      ; read chars
                jr      EMPTYCHABFR     ; repeat
CHABFREMPTY:    pop     AF              ; retrieve AF
                ld      HL,SOERR        ; return point set to Serial Buffer Overrun routine
                ex      (SP),HL         ; store onto stack
                ei                      ; re-enable interrupts
                reti                    ; return from interrupt and execute code at SOERR 

;-------------------------------------------------------------------------------
;               Z80 SIO CH. A MANAGEMENT
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; interrupt driven routine to get chars from Z80 SIO ch.B
RX_CHB_AVAIL:   ei
                reti

;-------------------------------------------------------------------------------
; special SIO ch.A condition (i.e., buffer overrun)
; if buffer overruns then show an error, empty the RX buffer and send
; a break char
SPEC_RXB_CNDT:  push    AF              ; store A
                call    B_RTS_OFF       ; disable RTS
                call    SIO_B_DI        ; disable RX on ch.B
                ld      A,(SERIALS_EN)  ; load serial status
                res     3,A             ; disable RX on port 2
                ld      (SERIALS_EN),A  ; store new serial status 
                in      A,(PIO_DB)      ; read status LEDs
                set     5,A             ; set 5th pin ON
                out     (PIO_DB),A      ; send new setting
                ld      A,%00110000     ; write into WR0: error reset, select WR0
                out     (SIO_CB),A      ; send command to SIO
EMPTYCHBBFR:    xor     A
                out     (SIO_CB),A      ; write to WR0, select RR0
                in      A,(SIO_CB)      ; read RR0 register
                and     $01             ; check if input buffer if empty
                jp      Z,CHABFREMPTY   ; if yes (bit 0 = 0) then jump to run "exit" code
                in      A,(SIO_DB)      ; read chars
                jr      EMPTYCHBBFR     ; repeat


;-------------------------------------------------------------------------------
;               Z80 SIO MANAGEMENT
;-------------------------------------------------------------------------------
; disable RTS:
; by resetting RTS bit (set to 0), the RTS line is disabled (HIGH)
A_RTS_OFF:      push    BC              ; store BC
                ld      C,SIO_CA        ; select channel A
                ld      A,(SERABITS)    ; load data serial bits for ch.A
                jr      SIO_RTS_OFF
B_RTS_OFF:      push    BC              ; store BC
                ld      C,SIO_CB        ; select channel B
                ld      A,(SERBBITS)    ; load data serial bits for ch.B
SIO_RTS_OFF:    srl     A               ; position data bits in bits #5&6
                and     %01100000       ; get only bits #5&6
                ld      B,A             ; store data bits
                ld      A,%00000101     ; write into WR0: select WR5
                out     (C),A
                ld      A,%10001000     ; enable DTR (b7) and TX (b4), disable RTS (b1)
                or      B               ; set data bits
                out     (C),A           ; send setting
                pop     BC              ; retrieve BC
                ret                     ; exit

;-------------------------------------------------------------------------------
; enable RTS
; by setting RTS bit (set to 1), the RTS line is enabled (LOW)
A_RTS_ON:       push    BC              ; store BC
                ld      C,SIO_CA        ; select channel A
                ld      A,(SERABITS)    ; load data serial bits for ch.A
                jr      SIO_RTS_ON
B_RTS_ON:       push    BC              ; store BC
                ld      C,SIO_CB        ; select channel B
                ld      A,(SERBBITS)    ; load data serial bits for ch.B
SIO_RTS_ON:     srl     A               ; position data bits in bits #5&6
                and     %01100000       ; get only bits #5&6
                ld      B,A             ; store data bits
                ld      A,%00000101     ; write into WR0: select WR5
                out     (C),A
                ld      A,%10001010     ; enable DTR (b7), TX (b4), and RTS (b1)
                or      B               ; set data bits
                out     (C),A           ; send setting
                pop     BC              ; retrieve BC
                ret                     ; return

;-------------------------------------------------------------------------------
; disable SIO RX channel
SIO_A_DI:       push    BC              ; store BC
                ld      C,SIO_CA        ; SIO channel A
                ld      A,(SERABITS)    ; load data serial bits for ch.A
                jr      SIO_RXDI        ; jump to disable RX
SIO_B_DI:       push    BC              ; store BC
                ld      C,SIO_CB        ; SIO channel B
                ld      A,(SERBBITS)    ; load data serial bits for ch.B
SIO_RXDI:       ld      B,A             ; store data bits
                ld      A,%00000011     ; write into WR0: select WR3
                out     (C),A
                ld      A,B             ; retrieve data bits; RX disabled; auto enable is OFF
                out     (C),A
                pop     BC              ; retrieve BC
                ret                     ; return

;-------------------------------------------------------------------------------
; enable SIO RX channel
SIO_A_EI:       push    BC              ; store BC
                ld      C,SIO_CA
                ld      A,(SERABITS)    ; load data serial bits for ch.A
                jr      SIO_RXEN
SIO_B_EI:       push    BC              ; store BC
                ld      C,SIO_CB
                ld      A,(SERBBITS)    ; load data serial bits for ch.B
SIO_RXEN:       ld      B,A             ; store data bits
                ld      A,%00000011     ; write into WR0: select WR3
                out     (C),A           ; select register
                ld      A,B             ; retrieve data bits
                set     0,A             ; set RX enabled; auto enable is OFF
                out     (C),A           ; send settings to SIO
                pop     BC              ; retrieve BC
                ret

;------------------------------------------------------------------------------
; put a char into the input buffer, char is into A
; this sub is called both from the ISR "RX_CHA_AVAIL" and when
; the RETURN key is pressed on the keyboard
CHARINTOBFR:    push    AF              ; store it
                ld      A,(serBufUsed)  ; load buffer size
                cp      SER_BUFSIZE     ; if buffer is not full
                jp      C,NOTFULL       ; then store the char
                pop     AF              ; else drop it
                ret                     ; and exit
NOTFULL:        ld      HL,(serInPtr)   ; buffer is not full, can store the char
                inc     HL              ; load pointer to find first free cell
                ld      A,L             ; only check low byte because buffer<256
                cp      bufWrap         ; check if the pointer is at the last cell
                jr      NZ,NOTWRAP      ; if not then continue
                ld      HL,SERBUF_START ; else load the address of the first cell
NOTWRAP:        ld      (serInPtr),HL   ; store the new pointer
                pop     AF              ; then retrieve the char...
                ld      (HL),A          ; ...and store it in the appropriate cell
                ld      HL,serBufUsed   ; size of the input buffer
                inc     (HL)            ; increment it
                ld      A,SER_FULLSIZE  ; input buffer capacity
                cp      (HL)            ; check if input buffer is full
                ret     C               ; exit if buffer is not full
                ld      A,(SERIALS_EN)  ; check if serial 1 is open
                rra                     ; bit 0 into Carry: if Carry is 1 then serial 0 is open and...
                call    C,A_RTS_OFF     ; ...receiving further chars must be stopped
                scf                     ; set Carry flag, because  we must inform that the char has been added before to disable RTS
                ret


;-------------------------------------------------------------------------------
; retrieve a char from the input buffer
RXA:            ld      A,(serBufUsed)  ; load the buffer size
                and     A               ; check if it's 0 (empty)
                jp      Z,RXA           ; if it's empty, wait for a char
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
                cp      SER_EMPTYSIZE   ; check if input buffer can be considered empty
                jr      NC,RXA_EXIT     ; if not empty yet, then exit
                ld      A,(SERIALS_EN)  ; load serial state
                xor     %00000101       ; check if serial 1 is open and RX enabled
                call    Z,A_RTS_ON      ; yes, set RTS on
RXA_EXIT:       ld      A,(HL)          ; recover the char and return it into A
                pop     HL              ; retrieve HL
                ei                      ; re-enable interrupts
                ret                     ; return

;------------------------------------------------------------------------------
; sends a char over the serial - char is into A
TXA:            push    AF              ; store AF
                push    BC              ; store BC
                ld      B,A             ; store char
                ld      A,(SERIALS_EN)  ; load serial status
                xor     %00000101       ; check if serial 1 is open and RX is enabled 
                jr      NZ,TXA_EXIT     ; no, jump over
                ld      A,B             ; retrieve char
                out     (SIO_DA),A      ; send char to the SIO
                call    TX_EMP          ; wait for outgoing char to be sent
TXA_EXIT:       pop     BC              ; retrieve BC
                pop     AF              ; retrieve AF
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
CKINCHAR:       ld      A,(serBufUsed)  ; load buffer size
                and     A               ; compare to 0
                ret                     ; return

;------------------------------------------------------------------------------
; print a text from memory, and terminate when $00 is found
RAWPRINT:       ld      A,(HL)          ; load character from memory cell pointed by HL
                or      A               ; is it $00 (end string)?
                ret     Z               ; Yes, then return
                ld      (CHR4VID),A     ; store char
                di
                call    CHAR2VID        ; and send it to screen
                ei
                inc     HL              ; and select the next one
                jr      RAWPRINT        ; repeat

;-------------------------------------------------
; Interrupt service routine (ISR) for CH3 timer
; this is used to increment the 100ths of a second counter and for cursor flashing
CH3_TIMER:      push    AF              ; save regs. A,
                push    BC              ; BC,
                push    DE              ; DE,
                push    HL              ; HL
                ld      HL,TMRCNT       ; load starting address of the timer
                ld      B,$04           ; 4 bytes to check
INCTMR3:        inc     (HL)            ; increment timer
                jr      NZ,CHKCRSR      ; if not zero then exit (finished increment)
                inc     HL              ; if yes, there was an overflow, so increment next byte
                djnz    INCTMR3         ; repeat for 4 bytes
CHKCRSR:        call    FLASHCURSOR     ; call the flashing cursor routine
                call    MNGSNDS         ; call the tone managemenet
                ld      A,(TMRCNT)      ; check for keyboard management
                rra                     ; bit 0 = 1 ?
                call    NC,KEYBOARD     ; no, so read the keyboard inputs
                pop     HL              ; retrieve HL,
                pop     DE              ; DE,
                pop     BC              ; BC,
                pop     AF              ; and A
                ei                      ; re-enable interrupts
                reti                    ; exit from ISR

;------------------------------------------------------------------------------
;               HARDWARE INITIALISATION
;------------------------------------------------------------------------------
; first run - setup HW & SW
; (on LN80C 64K runs from RAM)
INIT_HW:        ld      HL,TEMPSTACK    ; load temp stack pointer
INIT_HW2:       ld      SP,HL           ; set stack to temp stack pointer
                ld      HL,SERBUF_START ; set beginning of input buffer
                ld      (serInPtr),HL   ; for incoming chars to store into buffer
                ld      (serRdPtr),HL   ; and for chars to be read from buffer
                xor     A               ; reset A
                ld      (serBufUsed),A  ; actual buffer size is 0
                ld      (SERIALS_EN),A  ; set serial ports status to OFF
                call    initCTC         ; configure CTC, then...
                call    initPSG         ; ...configure PSG
                call    SHOW_LOGO       ; show computer logo
                ld      E,$01           ; E chooses the video mode (graphics 1)
                call    initVDP         ; set video display
                xor     A
                ld      I,A             ; set high byte of interrupt vectors to point to page 0
                im      2               ; interrupt mode 2
                ei                      ; enable interrupts
                ; print system messages
                xor     A               ; A=0 so...
                ld      (KBDNPT),A      ; ...inputs don't come from keyboard
                inc     A               ; A=1...
                ld      (PRNTVIDEO),A   ; ...to activate the print-on-video
                ld      HL,MSGTXT1      ; sign-on message
                call    RAWPRINT        ; print message
                ld      A,(basicStarted); check if BASIC is already started
                cp      'Y'             ; to see if this is a power-up
                jr      NZ,COLDSTART    ; if not, then do a COLD start
                ld      HL,MSGTXT2      ; message to choose kind of start
                call    RAWPRINT        ; print message
                call    CURSOR_ON       ; enable cursor
                xor     A
                ld      (PRNTVIDEO),A   ; disable print-on-video
CORW:           call    RXA             ; look for a pressed key
                and     %11011111       ; only UPPERCASE char
                cp      'C'             ; cold start?
                jr      NZ,CHECKWARM    ; no, let's check for warm start
                call    ECHO_CHAR       ; echoes the char
COLDSTART:      ld      A,'Y'           ; yes, set the "BASIC started" flag
                ld      (basicStarted),A
                call    CURSOR_OFF      ; disable cursor
                jp      COLD            ; start BASIC COLD
CHECKWARM:      cp      'W'
                jr      NZ,CORW         ; char not recognized, wait again
                call    ECHO_CHAR       ; echoes the char
                call    CURSOR_OFF      ; disable cursor
                jp      WARM            ; start BASIC WARM

;-------------------------------------------------------------------------------
; send back char received through ch. A
ECHO_CHAR:      ld      (CHR4VID),A     ; set char for video printing
                xor     A
                ld      (KBDNPT),A      ; input is not from keyboard
                di                      ; disable INTs
                call    CHAR2VID        ; echoes back the pressed key,
                ld      A,CR            ; then set a CR
                ld      (CHR4VID),A     ; set char for video printing
                call    CHAR2VID        ; and send it to screen
                ei                      ; re-enable INTs
                ld      A,$01
                ld      (PRNTVIDEO),A   ; re-enable video printing
                ret                     ; return to caller


;-------------------------------------------------------------------------------
; Z80 SIO default settings for serial channels
SIO_A_SETS:     defb    %00110000       ; write into WR0: error reset, select WR0
                defb    %00011000       ; write into WR0: channel reset
                defb    %00000100       ; write into WR0: select WR4
                defb    %01000100       ; write into WR4: presc. 16x, 1 stop bit, no parity
                defb    %00000101       ; write into WR0: select WR5
                defb    %11101000       ; write into WR5: DTR on, TX 8 bits, BREAK off, TX on, RTS off
SIO_B_SETS:     defb    %00000001       ; write into WR0: select WR1
                defb    %00000100       ; write into WR1: status affects interrupt vectors
                defb    %00000010       ; write into WR0: select WR2
                defb    %00000000       ; write into WR2: set interrupt vector, but bits D3/D2/D1 of this vector
                                        ; will be affected by the channel & condition that raised the interrupt
                                        ; (see datasheet): in our example, 0x0C for Ch.A receiving A char, 0x0E
                                        ; for special conditions
;------------------------------------------------------------------------------
;               Z80 CTC SETTING UP
;------------------------------------------------------------------------------
initCTC:
                ld      HL,CTCCONF      ; CTC configuration
                ld      DE,CTC0IV       ; CTC interrupt vector table
                ld      BC,$000C        ; 12 bytes
                ldir                    ; copy data
;CH0, CH1, & CH2 disabled
                ld      A,%00000011     ; interrupt off, timer mode, prescaler=16, don't care ext. TRG edge,
                                        ; start timer on loading constant, no time constant follows, software reset, command word
                out     (CTC_CH0),A     ; set CH0
                out     (CTC_CH1),A     ; set CH1
                out     (CTC_CH2),A     ; set CH2
;init CH3
;CH3 divides CPU CLK by 144*256 providing an interrupt signal at 100 Hz (1/100 sec).
;f = CPU_CLK/(144*256) => 3,686,400 / ( 36,864 ) => 100Hz
                ld      A,%10100111     ; interrupt on; timer mode; prescaler=256; don't care ext; automatic trigger;
                                        ; time constant follows; cont. operation; command word
                out     (CTC_CH3),A     ; send to CH3
                ld      A,$90           ; time constant - 90$ (144d)
                out     (CTC_CH3),A     ; send to CH3
                ld      A,%01000000     ; D7..D3 provide the first part of the int vector (in our case, $0100), followed by
                                        ; D2..D1, provided by the CTC (they point to the channel), D0=interrupt word
                                        ; so int vector is 01000xx00
                out     (CTC_CH0),A     ; send to CTC
                ; reset cells of 100ths of a second counter
                xor     A               ; reset A
                ld      HL,TMRCNT       ; load TMR pointer
                ld      B,$04           ; 4 memory cells
RESTMR:         ld      (HL),A          ; reset n-cell of TMR
                inc     HL              ; next cell
                djnz    RESTMR          ; repeat for 4 cells
                ret


;------------------------------------------------------------------------------
; jump table for CHx interrupts
CTCCONF:        defb    $FB,$ED,$4D     ; CTC0 interrupt vector (ei; reti)
                defb    $FB,$ED,$4D     ; CTC1 interrupt vector (ei; reti)
                defb    $FB,$ED,$4D     ; CTC2 interrupt vector (ei; reti)
                jp      CH3_TIMER       ; CTC3 interrupt vector (jump to execute sys-tick timer)


;------------------------------------------------------------------------------
; welcome messages
MSGTXT1:        
                defm    "    LM80C 64K Color Computer",CR
                defm    " by Leonardo Miliani * FW R",VERSION,CR,0
MSGTXT2:        defb    CR
                defm    "   <C>old or <W>arm start? ",0
