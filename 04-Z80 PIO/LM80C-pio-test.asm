; ---------------------------------------------------------
; LM80C - PIO - test
; This code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ---------------------------------------------------------
; Code: by Leonardo Miliani
;
; Revisions:
; 0.1 - 20190202 - First version - just a test
; 0.2 - 20190206 - Stable version - working
;
; ---------------------------------------------------------
;


; labels defining
DATAREGA    equ 00000000b
DATAREGB    equ 00000001b
CTRLREGA    equ 00000010b
CTRLREGB    equ 00000011b

; this line instructs the assembler to prepare a file for a ROM target
; meaning that blank cells will be filled up with 0xff
#target rom

; this line instructs the assembler to compile while taking account of
; the start of the code fixed at 0x0000h and that the file shall be
; 0xff bytes in size
#code BOOT, 0000h, 00ffh

reset:  ; this corresponds to the RESET vector (0000h)

        ; a little delay
        ld d, 0xff
loop3:
        ld b,0xff
loop4:
        djnz loop4
        dec d
        jp nz, loop3

        ;program the PIO
        ld a,11001111b      ; mode 3 (bit control)
        out (CTRLREGB),a
        ld a,00000000b      ; set pins of port B to OUTPUT
        out (CTRLREGB),a

        ld e,0x00           ; this is the pattern (starts with all LEDs set to OFF)

noexit:
        ; send the pattern to the PIO
        ld a,e
        out (DATAREGB),a
        ; a little delay
        ld d,0x80
loop1:
        ld b,0xff
loop2:
        djnz loop2
        dec d
        jp nz, loop1
        ; increment the pattern, so that it cycles from 0 to 255 repeatedly
        inc e
        ; sent the new pattern to the PIO
        ld a,e
        out (DATAREGB),a
        ; loop
        jp noexit
