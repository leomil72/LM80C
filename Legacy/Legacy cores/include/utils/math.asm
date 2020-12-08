; ------------------------------------------------------------------------------
; LM80C - UTILITY ROUTINES - R1.0
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
;
; Routines come from WikiTI (URL: wikiti.brandonw.net)
;
; ------------------------------------------------------------------------------
; Code Revision:
; R1.0 - 2019xxxx - First release - Division utilities
;
; ------------------------------------------------------------------------------

; 8/8 division
; INPUT: D (dividend), E (divisor)
; OPERATION: D/E
; OUTPUT: D (quotient), A (remainder)
DIV_8_8:    xor     a
            ld      b,08h
DIV_8_8LOOP:sla     d
            rla
            cp      e
            jr      c,$+4
            sub     e
            inc     d
            djnz    DIV_8_8LOOP
            ret
; ------------------------------------------------------------------------------

; 16/8 division
; INPUT: HL (Dividend), C (divisor)
; OPERATION: HL/C
; OUTPUT: HL (quotient), A (remainder)

DIV_16_8:   xor     a
            ld      b,16
DIV_16_8LP: add     hl,hl
            rla
            jr      c,$+5
            cp      c
            jr      c,$+4
            sub     c
            inc     l
            djnz    DIV_16_8LP
            ret
; ------------------------------------------------------------------------------

; 16/16 division
; INPUT: AC (Dividend), DE (divisor)
; OPERATION: AC/DE
; OUTPUT: AC (quotient), HL (remainder)

DIV_16_16:  ld      hl,0
            ld      b,16
DIV_16_16LP:sll     c
            rla
            adc     hl,hl
            sbc     hl,de
            jr      nc,$+4
            add     hl,de
            dec     c
            djnz    DIV_16_16LP
            ret
; ------------------------------------------------------------------------------

; 24/8 division
; INPUT: EHL (Dividend), D (divisor)
; OPERATION: EHL/D
; OUTPUT: EHL (quotient), A (remainder)

DIV_24_8:   xor     a
            ld      b,24
DIV_24_8LP: add     hl,hl
            rl      e
            rla
            jr      c,$+5
            cp      d
            jr      c,$+4
            sub     d
            inc     l
            djnz    DIV_24_8LP
            ret
; ------------------------------------------------------------------------------

; 32/8 division
; INPUT: DEHL (Dividend), C (divisor)
; OPERATION: DEHL/C
; OUTPUT: DEHL (quotient), A (remainder)

DIV_32_8:   xor     a
            ld      b,32
DIV_32_8LP: add     hl,hl
            rl      e
            rl      d
            rla
            jr      c,$+5
            cp      c
            jr      c,$+4
            sub     c
            inc     l
            djnz    DIV_32_8LP
            ret
; ------------------------------------------------------------------------------

; 32/16 division
; INPUT: ACIX (Dividend), DE (divisor)
; OPERATION: ACIX/DE
; OUTPUT: ACIX (quotient), HL (remainder)

DIV_32_16:  ld      hl,0
            ld      b,32
DIV_32_16LP:add     ix,ix
            rl      c
            rla
            adc     hl,hl
            jr      c,DIV_32_16OF
            sbc     hl,de
            jr      nc,DIV_32_16SB
            add     hl,de
            djnz    DIV_32_16LP
            ret
DIV_32_16OF:or      a ;overflow
            sbc     hl,de
DIV_32_16SB:inc     ix ; set bit
            djnz    DIV_32_16LP
            ret
; ------------------------------------------------------------------------------

; Rounded 16/8 division
; INPUT: HL (Dividend), C (divisor)
; OPERATION: HL\C
; OUTPUT: HL (rounded quotient), A (twice pre-rounded remainder)

ROUND_DIV:  xor     a
            ld      b,16
RND_DIV_LP: add     hl,hl
            rla
            jr      c,$+5
            cp      c
            jr      c,$+4
            sub     c
            inc     l
            djnz    RND_DIV_LP
            add     a,a ; this part is the rounding
            cp      c
            ret     c
            inc     hl
            ret
; ------------------------------------------------------------------------------

; 8*8 multiplication
; INPUT: H (multiplicand), E (multiplier)
; OPERATION: H*E
; OUTPUT: HL (product)

MULT_8_8:   ld      l,0
            ld      d,l
            sla     h ; optimised 1st iteration
            jr      nc,$+3
            ld      l,e
            ld      b,7
MULT_8_8LP: add     hl,hl
            jr      nc,$+3
            add     hl,de
            djnz    MULT_8_8LP
            ret
; ------------------------------------------------------------------------------

; 16*8 multiplication
; INPUT: DE (multiplicand), A (multiplier)
; OPERATION: DE*A
; OUTPUT: AHL (product)
; DESTROY: C

MULT_16_8:  ld      c,0
            ld      h,c
            ld      l,h
            add     a,a ; optimised 1st iteration
            jr      nc,$+4
            ld      h,d
            ld      l,e
            ld      b,7
MULT_16_8LP:add     hl,hl
            rla
            jr      nc,$+4
            add     hl,de
            adc     a,c ; (adc a, 0) since C is unused, we set it to zero and so we can save 1 byte and up to 3 T-states per iteration
            djnz    MULT_16_8LP
            ret
 ; -----------------------------------------------------------------------------

 ; 16*16 multiplication
 ; INPUT: BC (multiplicand), DE (multiplier)
 ; OPERATION: BC*DE
 ; OUTPUT: DEHL (product)

MULT_16_16: ld      hl,0
            sla     e ; optimised 1st iteration
            rl      d
            jr      nc,$+4
            ld      h,b
            ld      l,c
            ld      a,15
MULT_16_16L:add     hl,hl
            rl      e
            rl      d
            jr      nc,$+6
            add     hl,bc
            jr      nc,$+3
            inc     de
            dec     a
            jr      nz,MULT_16_16L
            ret
