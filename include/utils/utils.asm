; ------------------------------------------------------------------------------
; LM80C - UTILITY ROUTINES - R1.0
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
; ------------------------------------------------------------------------------
; Code Revision:
; R1.0 - 2020xxxx - First release: 16-bit comparision/multiplication/negation
;
; ------------------------------------------------------------------------------

; compare two 16-bit registers, HL (minuend) and DE (subtrahend)
; values can be both signed or unsigned words
; inputs: HL, DE
; destroys: A,F
; returns: if both registers are 2's complement, use Z and S flags;
; otherwise:
; if HL=DE: Z,P,NC  - Z=1, S=0; C=0
; if HL>DE: NZ,P,NC - Z=0, S=0; C=0
; if HL<DE: NZ,M,C  - Z=0, S=1; C=1
; Source: ALS

CMP16:  or      A           ; clear CARRY
        sbc     HL,DE       ; subract DE from HL
        ret     PO          ; return if no overflow
        ld      A,H         ; overflow - invert SIGN flag
        rra                 ; save CARRY flag in bit 7
        xor     01000000b   ; complement bit 6 (SIGN bit)
        scf                 ; ensure a Non-Zero result
        adc     A,A         ; restore CARRY, complemented SIGN
                            ; ZERO flag = 0 for sure
        ret                 ; return

; ----------------------------------------------------------------------

; multiply 2 signed or unsigned 16-bit wors and return a 
; 16-bit signed or unsigned product
; inputs: HL (multiplicand); DE (multiplier)
; destroys: A,F
; returns: HL (product)
; Source: ALS

; initialize partial product, bit count
MUL16:  push    BC
        ld      C,L         ; BC = multiplier
        ld      B,H
        ld      HL,0        ; product = 0
        ld      A,15        ; count = bit lenght - 1
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

;-----------------------------------------------------------------------
; absolute value of a 16-bit register
; 
; inputs: HL, signed
; destroys: A
; outputs: HL, negated
; Source: WKT

negHL:  xor     A
        sub     L
        ld      L,A
        sbc     A,A
        sub     H
        ld      H,A
	    ret