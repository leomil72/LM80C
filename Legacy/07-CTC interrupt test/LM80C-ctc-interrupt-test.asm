; ------------------------------------------------------------------------------
; LM80C - CTC interrupts test
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
; 0.1 - 20190211 - First version
; 1.0 - 20190212 - Code revision - stable version
; ------------------------------------------------------------------------------

; label defining for CTC
CH0         equ 00010000b
CH1         equ 00010001b
CH2         equ 00010010b
CH3         equ 00010011b

; labels defining for PIO
DATAREGA    equ 00000000b
DATAREGB    equ 00000001b
CTRLREGA    equ 00000010b
CTRLREGB    equ 00000011b

RAMCELL     equ 0x8000

; this line instructs the assembler to prepare a file for a ROM target
; meaning that blank cells will be filled up with 0xff
#target rom

; this line instructs the assembler to compile while taking account of
; the start of the code that must be set to 0x0000h and that the file has a fixed size
#code BOOT, 0000h, 0200h

;--------------------------------------------------
reset:  ; this corresponds to the RESET vector (the CPU jumps to 0000h after a reset)

        jp MAIN

        ; interrupt vector for CH2 Timer
        org 14h
        defw CH2_TIMER

;---------------------------------------------------
; Main code
        org 0100h           ; main code starts at $0100
MAIN:
        ld sp,0xffff        ; set the stack pointer to top of RAM

        ld d,0x80
        call delay          ; little delay

        call SET_PIO        ; set the PIO
        call SET_CTC        ; set the CTC

        xor a,a             ; clear reg. A
        out (DATAREGB),a    ; turn off the LEDs
        ld i,a              ; set most significant bits of interrupt vector to $0000
        ld (RAMCELL),a      ; reset the seconds' counter into RAM
        im 2                ; interrupt mode 2
        ei                  ; enable interrupts

DO_NOTHING:                 ; this is the main loop: the CPU simply does nothing...
        ld d,0x80
        call delay
        jp DO_NOTHING

;-------------------------------------------------
; Interrupt service routine (ISR) for CH2 timer
CH2_TIMER:
        di                  ; disable interrupts
        push af             ; save reg. A
        ld a,(RAMCELL)      ; load the timer from RAM
        inc a               ; increment it
        ld (RAMCELL),a      ; write the new value
        out (DATAREGB),a    ; send it to the PIO
        pop af              ; recover reg. A
        ei                  ; re-enable interrupts
        reti                ; exit from ISR

;-------------------------------------------------
SET_CTC:
;init CH0 & CH3
;CH0 & CH3 disabled
        ld a,00000011b      ; interrupt off, timer mode, prescaler=16, don't care ext. TRG edge,
                            ; start timer on loading constant, no time constant follows, software reset, command word
        out (CH0),a         ; CH0 doesn't run
        out (CH3),a         ; CH3 doesn't run

;init CH1
;CH1 divides CPU CLK by (256*256) providing a clock signal at TO1. TO1 is connected to TRG2.
;T01 outputs f= CPU_CLK/(256*256) => 3.68MHz / ( 256 * 256 ) => 56.15Hz
        ld a,00100111b      ; interrupt off; timer mode; prescaler=256; don't care ext; automatic trigger;
                            ; time constant follows; cont. operation; command word
        out (CH1),a
        ld a,0x00           ; time constant - 0 stands for 256
        out (CH1),a

;init CH2
;CH2 divides CLK/TRG2 clock providing a clock signal at TO2.
; T02 outputs f= CLK/TRG / 56 => 56.15Hz / 56 => 1.002Hz ~ 1s
        ld a,11000111b      ; interrupt on, counter mode, prescaler=16 (doesn't matter), ext. start,
                            ; start upon loading time constant, time constant follows,sw reset, command word
        out (CH2),a
        ld a,0x38           ; time constant 56d
        out (CH2),a         ; loaded into channel 2
        ld a,00010000b      ; D7..D3 provide the first part of the int vector (in our case, $10), followed by
                            ; D2..D1, provided by the CTC (they point to the channel), d0=interrupt word
        out (CH0),a         ; send to CTC

;-------------------------------------------------
;program the PIO
SET_PIO:
        ld a,11001111b      ; mode 3 (bit control)
        out (CTRLREGB),a
        ld a,00000000b      ; set pins of port B to OUTPUT
        out (CTRLREGB),a
        ret

;-------------------------------------------------
delay:                      ; routine to add a programmable delay (set by value stored in D)
        push bc
loop1:
        ld b,0xff
loop2:
        djnz loop2
        dec d
        jp nz, loop1
        pop bc
        ret
