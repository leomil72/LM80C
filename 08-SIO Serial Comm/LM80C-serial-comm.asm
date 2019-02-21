; ------------------------------------------------------------------------------
; LM80C - SERIAL COMM test
; ------------------------------------------------------------------------------
; This code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Written by Leonardo Miliani
; Based on code samples from "How To Program the Z80 Periphery Tutorial" by Mario Blunk
; Edited with Atom Editor
; Compiled with ZASM assembler 4.2.4 on MacOS
; ------------------------------------------------------------------------------
; Released under the terms of GPL v.3 or any successive release
; ------------------------------------------------------------------------------
; Revisions:
; 0.1 - 20190209 - First version
; 0.2 - 20190219 - Second version - almost working
; 1.0 - 20190220 - Stable version
; ------------------------------------------------------------------------------

; ADDRESS DECODING
; A4/A5
; 0000xxxx : PIO
; 0001xxxx : CTC
; 0010xxxx : SIO

; label defining for CTC
CTC_CH0     equ 00010000b
CTC_CH1     equ 00010001b
CTC_CH2     equ 00010010b
CTC_CH3     equ 00010011b

; labels defining for PIO
PIO_DA      equ 00000000b
PIO_DB      equ 00000001b
PIO_CA      equ 00000010b
PIO_CB      equ 00000011b

;label defining for SIO
SIO_CA      equ 00100010b
SIO_CB      equ 00100011b
SIO_DA      equ 00100000b
SIO_DB      equ 00100001b

RAMCELL     equ 0x8000

; this line instructs the assembler to prepare a file for a ROM target
; meaning that blank cells will be filled up with 0xff
#target rom

; this line instructs the assembler to compile while taking account of
; the start of the code fixed at 0x0000h and that the file shall be
; XXXX bytes in size
#code BOOT, 0000h, 0200h

reset:  ; this corresponds to the RESET vector (the CPU jumps to 0000h after a reset)
        jp main

        org 0ch
        defw RX_CHA_AVAILABLE
        org 0Eh
        defw SPEC_RX_CONDITON

;-------------------------------------------------------------------------------
; MAIN
;-------------------------------------------------------------------------------

        org 0100h
main:
        ld sp,0xffff        ; set the stack pointer to the end of the RAM

        ld d, 0x80
        call delay          ; little delay

        call setPIO         ; set PIO
        call setCTC         ; set CTC
        call setSIO         ; set SIO

        ld a,0              ; set high byte of interrupt vectors to point to page 0
        ld i,a
        im 2                ; set int mode 2
        ei                  ; enable interupts

        ld a,00000000b      ; load initial LED pattern into RAM
        ld (RAMCELL),a

repeat:                     ; this does a simple feedback for the user
        ld a,(RAMCELL)      ; load the byte from RAM
        inc a
        cp 01000000b
        jr nz,storeByte
        ld a,00000000b
storeByte:
        out (PIO_DB),a      ; send the pattern to the PIO
        ld (RAMCELL),a      ; write the new pattern into RAM for later reload

        ld d,0xff           ; a little delay
        call delay
        jp repeat           ; endless loop

;-------------------------------------------------------------------------------
; Subroutines
;-------------------------------------------------------------------------------
setPIO:
;program the PIO
        ld a,11001111b      ; mode 3 (bit control)
        out (PIO_CB),a
        ld a,00000000b      ; set pins of port B to OUTPUT
        out (PIO_CB),a
        ret

;-------------------------------------------------------------------------------
setSIO:
;program the SIO
        ;set up TX and RX:
        ; the followings are settings for channel A
        ld a,00110000b      ; write into WR0: error reset, select WR0
        out (SIO_CA),a
        ld a,00011000b      ; write into WR0: channel reset
        out (SIO_CA),a
        ld a,00000100b      ; write into WR0: select WR4
        out (SIO_CA),a
        ld a,01000100b      ; write into WR4: presc. 16x, 1 stop bit, no parity
        out (SIO_CA),a
        ld a,00000101b      ; write into WR0: select WR5
        out (SIO_CA),a
        ld a,11101000b      ; write into WR5: DTR on, TX 8 bits, BREAK off, TX on, RTS off
        out (SIO_CA),a
        ; the following are settings for channel B
        ld a,00000001b      ; write into WR0: select WR1
        out (SIO_CB),a
        ld a,00000100b      ; write into WR0: status affects interrupt vectors
        out (SIO_CB),a
        ld a,00000010b      ; write into WR0: select WR2
        out (SIO_CB),a
        ld a,0h             ; write into WR2: set interrupt vector, but bits D3/D2/D1 of this vector
                            ; will be affected by the channel & condition that raised the interrupt
                            ; (see datasheet): in our example, 0x0C for Ch.A receiving a char, 0x0E
                            ; for special conditions
        out (SIO_CB),a
        ; the following are settings for channel A
        ld a,01h            ; write into WR0: select WR1
        out (SIO_CA),a
        ld a,00011000b      ; interrupts on every RX char; parity is no special condition;
                            ; buffer overrun is special condition
        out (SIO_CA),a
SIO_A_EI:
        ;enable SIO channel A RX
        ld a,00000011b      ; write into WR0: select WR3
        out (SIO_CA),a
        ld a,11000001b      ; 8 bits/RX char; auto enable OFF; RX enable
        out (SIO_CA),a
        ret

;-------------------------------------------------
setCTC:
;init CH0
;CH0 provides to SIO SERIAL A the RX/TX clock
        ld a,01000111b      ; interrupt off, counter mode, prsc=16 (doesn't matter), ext. start,
                            ; start upon loading time constant, time constant follows, sw reset, command word
        out (CTC_CH0),a
        ld a,0x06           ; time constant 6
        out (CTC_CH0),a
                            ; TO0 output frequency=INPUT CLK/time constant
                            ; which results in 1,843,200/6 = 307,200 Hz because the CTC is set to need RX/TX
                            ; clock 16 times the requested baud rate (in our case, 19,200 x 16 = 307,200 Hz)
;init CH1
;CH1 divides CPU CLK by (256*256) providing a clock signal at TO1. TO1 is connected to TRG2.
; T01 outputs f= CPU_CLK/(256*256) => 3.68MHz / ( 256 * 256 ) => 56.15Hz
        ld a,00100111b      ; interrupt off; timer mode; prescaler=256; don't care ext; automatic trigger;
                            ; time constant follows; cont. operation; command word
        out (CTC_CH1),a
        ld a,0x00           ; time constant ( 0 -> 256 )
        out (CTC_CH1),a

;init CH2
;CH2 divides CLK/TRG2 clock providing a clock signal at TO2.
; T02 outputs f= CLK/TRG / 56 => 56.15Hz / 56 => 1.002Hz ~ 1s
        ld a,01000111b      ; interrupt off, counter mode, prescaler=16 (doesn't matter), ext. start,
                            ; start upon loading time constant, time constant follows,sw reset, command word
        out (CTC_CH2),a
        ld a,0x38           ; time constant 56d
        out (CTC_CH2),a     ; loaded into channel 2

;init CH3
;CH3 disabled (not used in this program)
        ld a,00000011b      ; interrupt off, timer mode, prescaler=16, don't care ext. TRG edge,
                            ; start timer on loading constant, no time constant follows, software reset, command word
        out (CTC_CH3),a     ; CH3 is halted

        ret

;-------------------------------------------------
delay:  ; routine to add a programmable delay (set by value stored in D)
        push bc
loop1:
        ld b,0xff
loop2:
        djnz loop2
        dec d
        jp nz, loop1
        pop bc
        ret

;-------------------------------------------------------------------------------
; serial management

A_RTS_OFF:
        ld a,00000101b      ; write into WR0: select WR5
        out (SIO_CA),a
        ld a,11101000b      ; 8 bits/TX char; TX enable; RTS disable
        out (SIO_CA),a
        ret

A_RTS_ON:
        ld a,00000101b      ; write into WR0: select WR5
        out (SIO_CA),a
        ld a,11101010b      ; 8 bits/TX char; TX enable; RTS enable
        out (SIO_CA),a
        ret

SIO_A_DI:
        ;disable SIO channel A RX
        ld a,00000011b      ; write into WR0: select WR3
        out (SIO_CA),a
        ld a,00001100b      ; write into WR3: RX disable;
        out (SIO_CA),a
        ret

RX_CHA_AVAILABLE:
        push af             ; backup AF
        call A_RTS_OFF      ; disable RTS
        in a,(SIO_DA)       ; read RX character into A
        out (SIO_DA),a      ; echo char to transmitter
        call TX_EMP         ; wait for outgoing char to be sent
        ;call RX_EMP         ; flush receive buffer
        ld a,(RAMCELL)      ; change the pattern to show to the external
        xor a,11000000b     ; world that this ISR was honored
        ld (RAMCELL),a
        call A_RTS_ON       ; enable again RTS
        pop af
        ei
        reti

SPEC_RX_CONDITON:
        jp 0000h            ; if buffer overrun then restart the program

TX_EMP:
        ; check for TX buffer empty
        sub a
        inc a
        out (SIO_CA),a
        in a,(SIO_CA)
        bit 0,a
        jp z,TX_EMP
        ret

RX_EMP:
        ;check for RX buffer empty
        ;modifies A
        sub a               ;clear a, write into WR0: select RR0
        out (SIO_CA),a
        in a,(SIO_CA)       ;read RRx
        bit 0,a
        ret z               ;if any rx char left in rx buffer
        in a,(SIO_DA)       ;read that char
        jp RX_EMP
