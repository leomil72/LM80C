; ------------------------------------------------------------------------------
; LM80C - UTILITY ROUTINES - R1.1
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
;
; *ALS are routines from "Z80 Assembly Language Subroutines" by Lance
; A. Leventhal and Winthrop Saville - Ed. Osborne/McGraw-Hill (1983)
;
; * WKT are routines from WikiTI:
; http://wikiti.brandonw.net/index.php?title=WikiTI_Home
;
; * LAC are routines from Learn@Cemetch
; https://learn.cemetech.net/index.php/Main_Page
;
; ------------------------------------------------------------------------------
; Code Revision:
; R1.0 - 20200110 - First release: 16-bit comparision/multiplication/negation
; R1.1 - 20200413 - Second release: added ABS(HL) 
;
; ------------------------------------------------------------------------------

; compare two 16-bit registers, HL (minuend) and DE (subtrahend)
; values can be both signed or unsigned words
; inputs: HL, DE
; destroys: A,F,HL
; returns: if both registers are 2's complement, use Z and S flags;
; otherwise:
; if HL=DE: Z,P,NC  - Z=1, S=0; C=0
; if HL>DE: NZ,P,NC - Z=0, S=0; C=0
; if HL<DE: NZ,M,C  - Z=0, S=1; C=1
; Source: ALS

CMP16:  or      A           ; clear CARRY
        sbc     HL,DE       ; subtract DE from HL
        ret     PO          ; return if no overflow
        ld      A,H         ; overflow - invert SIGN flag
        rra                 ; save CARRY flag in bit 7
        xor     %01000000   ; complement bit 6 (SIGN bit)
        scf                 ; ensure a Non-Zero result
        adc     A,A         ; restore CARRY, complemented SIGN
                            ; ZERO flag = 0 for sure
        ret                 ; return

; ----------------------------------------------------------------------

; multiply 2 signed/unsigned 16-bit words and return a 16-bit
; signed/unsigned product
; inputs: HL (multiplicand); DE (multiplier)
; destroys: A,F
; returns: HL (product)
; Source: ALS

; initialize partial product, bit count
MUL16:  push    BC
        ld      C,L         ; BC = multiplier
        ld      B,H
        ld      HL,0        ; product = 0
        ld      A,$0F       ; count = bit lenght - 1 (16-1)
        ; shift-and-add algorithm
        ; if MSB of multiplier is 1, add multiplicand to partial product
        ; shift partial product, multiplier left 1 bit
MLP:    sla     E           ; shift multiplier left 1 bit
        rl      D
        jr      NC,MLP1     ; jump if MSB of multiplier = 0
        add     HL,BC       ; add multiplicand to partial product
MLP1:   add     HL,HL       ; shift partial product left
        dec     A
        jr      NZ,MLP      ; continue until count = 0
        ; add multiplicand one last time if MSB of multiplier is 1
        or      D           ; sign flag = MSB of multiplier
        jp      P,EXMUL16   ; exit if MSB of multiplier is 0
        add     HL,BC       ; add multiplicand to product
EXMUL16:pop     BC
        ret

; ----------------------------------------------------------------------
; absolute value of HL (same applies to other 16-bit register pairs)
; also, invert value of HL (or any other 16-bit register, just adjust the code)
;
; inputs: HL
; destroys: A
; operation: ABS(HL)
; returns: HL with no sign or negated
; Source: WKT

absHL:  bit     7,H
        ret     Z
negHL:  xor     A
        sub     L
        ld      L,A
        sbc     A,A
        sub     H
        ld      H,A
        ret

; ------------------------------------------------------------------------------

; 8/8 division
; INPUT: D (dividend), E (divisor)
; OPERATION: D/E
; OUTPUT: D (quotient), A (remainder)
DIV_8_8:    xor     A
            push    BC
            ld      B,08h
DIV_8_8LOOP:sla     D
            rla
            cp      E
            jr      C,$+4
            sub     E
            inc     D
            djnz    DIV_8_8LOOP
            pop     BC
            ret

; ----------------------------------------------------------------------
; divide a 16-bit number by an 8-bit number
; (16/8 division)
;
; inputs: HL (Dividend), C (divisor)
; destroys: A
; OPERATION: HL/C
; returns: HL (quotient), A (remainder)
; source: WKT

DIV_16_8:   xor     A
            ld      B,16
DIV_16_8LP: add     HL,HL
            rla
            jr      C,$+5
            cp      C
            jr      C,$+4
            sub     C
            inc     L
            djnz    DIV_16_8LP
            ret