; ---------------------------------------------------------
; LM80C - PIO - test
; This code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ---------------------------------------------------------
; Code: by Leonardo Miliani
;
; Revisions:
; 0.1 - 20190207 - First version - working
;
; ---------------------------------------------------------
;

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
; the start of the code fixed at 0x0000h and that the file shall be
; 0xff bytes in size
#code BOOT, 0000h, 00ffh

reset:  ; this corresponds to the RESET vector (the CPU jumps to 0000h after a reset)

        ld sp,0xffff        ; set the stack pointer

        ld d, 0x80
        call delay          ; little delay

        call setCtc         ; set CTC

        ;program the PIO
        ld a,11001111b      ; mode 3 (bit control)
        out (CTRLREGB),a
        ld a,00000000b      ; set pins of port B to OUTPUT
        out (CTRLREGB),a
        ld a, 10101010b     ; write a byte into RAM
        ld (RAMCELL), a

noexit:
        xor a,a             ; clear register A
        ld a,(RAMCELL)      ; load the byte from RAM
        ; send the pattern to the PIO
        out (DATAREGB),a
        xor a,11111111b     ; invert the bits of the byte
        ld (RAMCELL),a      ; write the new value

        ; a little delay
        ld d,0x80
        call delay
        jp noexit

;-------------------------------------------------
setCtc:
;init CH3
;CH3 disabled
        ld a,00000011b      ; interrupt off, timer mode, prescaler=16, don't care ext. TRG edge,
                            ; start timer on loading constant, no time constant follows, software reset, command word
        out (CH3),a         ; CH3 is halted

;init CH0
;CH0 divides CPU CLK by (256*256) providing a clock signal at TO0. TO0 is connected to TRG1.
; T01 outputs f= CPU_CLK/(256*256) => 3.68MHz / ( 256 * 256 ) => 56.15Hz
        ld a,00100111b      ; interrupt off; timer mode; prescaler=256; don't care ext; automatic trigger;
                            ; time constant follows; cont. operation; command word
        out (CH0),a
        ld a,0xff           ; time constant
        out (CH0),a

;init CH1
;CH1 divides CLK/TRG1 clock providing a clock signal at TO1.
; T01 outputs f= CLK/TRG / 56 => 56.15Hz / 56 => 1.002Hz ~ 1s
        ld a,01000111b      ; interrupt off, counter mode, prescaler=16 (doesn't matter), ext. start,
                            ; start upon loading time constant, time constant follows,sw reset, command word
        out (CH1),a
        ld A,0x38           ; time constant 56d
        out (CH1),a         ; loaded into channel 1                
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
