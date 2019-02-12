; ---------------------------------------------------------
; LM80C - PIO - test
; This code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ---------------------------------------------------------
; Code by Leonardo Miliani
; Compiler: ZASM assembler 4.2.4-macos10.12
;
; Revisions:
; 0.1 - 20190206 - First version - working
; 1.0 - 20190212 - Code revision - stable version
;
; ---------------------------------------------------------
;

; labels defining
DATAREGA    equ 00000000b
DATAREGB    equ 00000001b
CTRLREGA    equ 00000010b
CTRLREGB    equ 00000011b

RAMCELL     equ 0x8000

; this directive instructs the assembler to prepare a file for a ROM target
; meaning that blank cells will be filled up with 0xff
#target rom

; this directive instructs the assembler to compile while taking account of
; the start of the code that must be set to 0x0000h and that the file has a fixed size
#code BOOT, 0000h, 00ffh

reset:  ; this corresponds to the RESET vector (the CPU jumps to 0000h after a reset)

        ld sp,0xffff        ; set the stack pointer
        ld d, 0x80
        call delay          ; little delay

        ; let's program the PIO
        ld a,11001111b      ; mode 3 (bit control)
        out (CTRLREGB),a
        ld a,00000000b      ; set pins of port B to OUTPUT
        out (CTRLREGB),a
        ld a, 10101010b     ; store the initial pattern into RAM
        ld (RAMCELL), a

noexit:
        xor a,a             ; clear register A (to be sure that the CPU will read from RAM)
        ld a,(RAMCELL)      ; load the byte from RAM
        out (DATAREGB),a    ; send the pattern to the PIO
        xor a,11111111b     ; invert the bits of the byte
        ld (RAMCELL),a      ; write the new value

        ld d,0x80           ; a little delay
        call delay
        jp noexit           ; repeat

;-------------------------------------------------------------------
delay:  ; routine to add a programmable delay
        ; set by reg. B
        push bc
loop1:
        ld b,0xff
loop2:
        djnz loop2
        dec d
        jp nz, loop1
        pop bc
        ret
