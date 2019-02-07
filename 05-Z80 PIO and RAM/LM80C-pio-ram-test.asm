; ---------------------------------------------------------
; LM80C - PIO - test
; This code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ---------------------------------------------------------
; Code: by Leonardo Miliani
;
; Revisions:
; 0.1 - 20190206 - First version - working
;
; ---------------------------------------------------------
;

; labels defining
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

;-------------------------------------------------------------------
delay:  ; routine to add a programmable delay
        ; a little delay
        push bc
loop1:
        ld b,0xff
loop2:
        djnz loop2
        dec d
        jp nz, loop1
        pop bc
        ret
