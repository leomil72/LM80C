; ------------------------------------------------------------------------------
; LM80C - BOOTLOADER
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Coding/Editing/Compiling:
; Original init code for MC68B05 by Grant Searle
; Original SIO/CTC init code by Mario Blunk
; Modified and adapted by Leonardo Miliani
;
; Edited with Atom Editor
;
; Compiled with ZASM assembler 4.2.4 on MacOS
; https://k1.spdns.de/Develop/Projects/zasm-4.0/Distributions/
; ------------------------------------------------------------------------------
; Copyright notes:
; Parts of the code (c) Grant Searle - free for non commercial use
; Please include this advice and the note to the attribution of the original
; version to Grant Searle if you intend to redistribuite it
; http://searle.hostei.com/grant/index.html
; eMail: home.micros01@btinternet.com
;
; Parts of the code (c) Mario Blunk
; http://www.train­z.de
;
; Parts of the code by Leonardo Miliani
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Revisions:
; 0.1 - 20190223 - First draft
; 0.2 - 20190224 - First working version
; 0.3 - 20190225 - Second working release
; 1.0 - 20190227 - First stable release
; ------------------------------------------------------------------------------

; label defining for CTC
CTC_CH0     equ 00010000b
CTC_CH1     equ 00010001b
CTC_CH2     equ 00010010b
CTC_CH3     equ 00010011b

;label defining for SIO
SIO_CA      equ 00100010b
SIO_CB      equ 00100011b
SIO_DA      equ 00100000b
SIO_DB      equ 00100001b

; this line instructs the assembler to prepare a file for a ROM target
; meaning that blank cells will be filled up with 0xff
#target rom

; Interrupt-driven serial I/O interface lead by the Z80 SIO to run modified
; NASCOM Basic 4.7 - Full input buffering with incoming data hardware handshaking
; Handshake shows full before the buffer is totally filled to allow run-on from the sender

SER_BUFSIZE     equ     $3f
SER_FULLSIZE    equ     $30
SER_EMPTYSIZE   equ     $05

SERBUF_START    equ     $8000
serInPtr        equ     SERBUF_START + SER_BUFSIZE
serRdPtr        equ     serInPtr+2
serBufUsed      equ     serRdPtr+2
basicStarted    equ     serBufUsed+1
bufWrap         equ     (SERBUF_START + SER_BUFSIZE) & $ff
TEMPSTACK       equ     $80ed ; top of BASIC line input buffer so is "free ram" when BASIC resets

CR              equ     $0d
LF              equ     $0a

; this line instructs the assembler to compile while taking account that code
; starts at a specific location and that is a certain amount of bytes in size
#code BOOT, 0000h, 0200h

;------------------------------------------------------------------------------
; BASE MEMORY - RESET LOCATION - $0000
; the CPU jumps to 0000h after a reset
                org $00
RST00:          di
                jp INIT     ; jump to first start config

;------------------------------------------------------------------------------
; send a character over serial
                org $08
RST08:          jp TXA

;------------------------------------------------------------------------------
; interrupt vector when SIO has a char available in its buffer
                org $0c
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
; check serial status

                org $18
RST18:          jp CKINCHAR

;------------------------------------------------------------------------------
; interrupt driven routine to get chars from Z80 SIO
RX_CHA_AVAILABLE:
                push af             ; store A & HL
                push hl
                in a,(SIO_DA)       ; read char from RX buffer into A
                push af             ; store it
                ld a,(serBufUsed)   ; load buffer size
                cp SER_BUFSIZE      ; if buffer is not full
                jr nz,notFull       ; then store the char
                pop af              ; else drop it
                jr RX_CH_AV_EXIT    ; and exit

notFull:        ld hl,(serInPtr)    ; buffer is not full, can store the char
                inc hl              ; load pointer to find first free cell
                ld a,l              ; only check low byte because buffer<256
                cp bufWrap          ; check if the pointer is at the last cell
                jr nz,notWrap       ; if not then continue
                ld hl,SERBUF_START  ; else load the address of the first cell
notWrap:        ld (serInPtr),hl    ; store the new pointer
                pop af              ; then recover the char
                ld (hl),a           ; and store it in the appropriate cell
                ld a,(serBufUsed)   ; load the size of the serial buffer
                inc a               ; increment it
                ld (serBufUsed),a   ; and store the new size
                cp SER_FULLSIZE     ; check if serial buffer is full
                jr c,RX_CH_AV_EXIT  ; exit if buffer is not full
                call A_RTS_OFF      ; else stop receiving further chars

RX_CH_AV_EXIT:  pop hl              ; recover H & A
                pop af
                ei                  ; re-enable interrupts
                reti                ; and exit

;-------------------------------------------------------------------------------
; Z80 SIO management

; disable RT
; by resetting RTS bit (set to 0), the RTS line is disabled (HIGH)
A_RTS_OFF:
                push af             ; store A
                ld a,00000101b      ; write into WR0: select WR5
                out (SIO_CA),a
                ld a,11101000b      ; 8 bits/TX char; TX enable; RTS disable
                out (SIO_CA),a
                pop af              ; retrieve A
                ret                 ; exit

; enable RT
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
                jp $0000            ; if buffer overrun then restart the system

;------------------------------------------------------------------------------
; retrieve a char from the serial buffer
RXA:
waitForChar:    ld a,(serBufUsed)  ; load the buffer size
                cp $00             ; check if is 0 (empty)
                jr z,waitForChar    ; if it's empty, wait for a char

                di                  ; disable interrupts
                push hl             ; store HL
                ld hl,(serRdPtr)    ; load pointer to first available char
                inc hl              ; increment it (go to the next char)
                ld a,l              ; check if the end of the buffer has been reached
                cp bufWrap          ; (only check low byte because buffer<256)
                jr nz,notRdWrap     ; if not, jump straight
                ld hl,SERBUF_START  ; else reload the starting address of the buffer
notRdWrap:      ld (serRdPtr),hl    ; store new pointer to the next char to read
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
TXA:            out (SIO_DA),a      ; send chat to the SIO
                call TX_EMP         ; wait for outgoing char to be sent
                ret                 ; return

;------------------------------------------------------------------------------
TX_EMP:
; wait until outgoing serial has been sent
                sub a               ; set A to 0
                inc a               ; set A to 1
                out (SIO_CA),a      ; write to WR0, select RR1
                in a,(SIO_CA)       ; read RR1 register
                bit 0,a             ; check if all chars have been sent
                jp z,TX_EMP         ; if not (bit 0 = 0) then retrieve
                ret                 ; else exit


;------------------------------------------------------------------------------
CKINCHAR        ld a,(serBufUsed)   ; load char in buffer
                cp $00              ; compare to 0
                ret                 ; return

;------------------------------------------------------------------------------
PRINT:          ld a,(hl)           ; load character from memory cell pointed by HL
                or a                ; is it $00 (end string)?
                ret z               ; Yes, then return
                rst $08             ; No, print it
                inc hl              ; and select the next one
                jr PRINT            ; repeat

;------------------------------------------------------------------------------
INIT:           ; first run - setup HW & SW
                ld hl,TEMPSTACK         ; load temp stack pointer
                ld sp,hl                ; set stack to temp stack pointer
                ld hl,SERBUF_START      ; set beginning of serial buffer
                ld (serInPtr),hl        ; for incoming chars to store into buffer
                ld (serRdPtr),hl        ; and for chars to be read from buffer
                xor a                   ; set A to 0
                ld (serBufUsed),a       ; clear actual buffer size
                call setCTC             ; configure CTC, then...
                call setSIO             ; configure SIO
                xor a,a                 ; set high byte of interrupt vectors
                ld i,a                  ; to point to page 0
                im 2                    ; interrupt mode 2
                ei                      ; enable interrupts

                ld hl,MSGTXT1           ; sign-on message
                call PRINT              ; print messega
                ld a,(basicStarted)     ; check if BASIC is already started
                cp 'Y'                  ; to see if this is a power-up
                jr nz,COLDSTART         ; if not, then do a COLD start
                ld hl, MSGTXT2          ; print message to choose start
                call PRINT

CORW:           call RXA                ; wait for a char
                and 11011111b           ; only UPPERCASE char
                cp 'C'                  ; cold start?
                jr nz,CHECKWARM         ; no, let's check for warm start
COLDSTART:      call ECHOCAR            ; echoes the char
                ld a,'Y'                ; yes, set the "BASIC started" flag
                ld (basicStarted),a
                jp COLDSTART_ADR        ; start BASIC COLD

CHECKWARM:      cp 'W'
                jr nz,CORW              ; char not recognized, wait again
                call ECHOCAR            ; echoes the char
                jp WARMSTART_ADR        ; start BASIC WARM

;-------------------------------------------------------------------------------
ECHOCAR:        rst $08                 ; echoes back the pressed key,
                ld a,CR                 ; the CR
                rst $08
                ld a,LF                 ; and the LF chars
                rst $08
                ret
;-------------------------------------------------------------------------------
; Z80 SIO setting up
setSIO:
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
setCTC:
;init CH0
;CH0 provides to SIO SERIAL A the RX/TX clock
                ld a,01000111b          ; interrupt off, counter mode, prsc=16 (doesn't matter), ext. start,
                                        ; start upon loading time constant, time constant follows, sw reset, command word
                out (CTC_CH0),a
                ld a,$06                ; time constant 6
                out (CTC_CH0),a
                ret
; TO0 output frequency=INPUT CLK/time constant
; which results in 1,843,200/6 = 307,200 Hz because the CTC needs that RX/TX
; clock is 16 times the requested baud rate (in our case, 19,200 x 16 = 307,200 Hz)

;------------------------------------------------------------------------------
MSGTXT1:        defm "Z80 SBC by Grant Searle",CR,LF
                defm "LM80C bootloader by Leonardo Miliani",CR,LF,0
MSGTXT2:        defb CR,LF
                defm "|C|old or |W|arm start? ",0

COLDSTART_ADR:  equ $
WARMSTART_ADR:  equ COLDSTART_ADR + $03
