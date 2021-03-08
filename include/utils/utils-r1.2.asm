; ------------------------------------------------------------------------------
; LM80C - UTILITY ROUTINES - R1.2
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
; R1.2 - 20200131 - Added 32/16 bit multiplication/division and converter to
;                   transform a 32-bit value into ASCII representation
;
; ------------------------------------------------------------------------------

; compare two 16-bit registers, HL (minuend) and DE (subtrahend)
; values can be both signed or unsigned words
; inputs: HL, DE
; destroys: A,F,HL
;
; returns: Z=1 if HL = DE
; for UNSIGNED: C=1 if HL<DE  //  C=0 if HL>DE
; for SIGNED:   S=1 (M) if HL<DE  //  S=0 (P) if HL>DE
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

; multiply 2 unsigned 16-bit words and return a 32-bit unsigned product
; inputs: BC (multiplicand); DE (multiplier)
; destroys: A,F
; operation: BC * DE
; returns: DEHL (product)
; Source: WKT

MUL_U32:ld      HL,$0000        ; reset HL
        sla     E		; optimised 1st iteration
        rl      D
        jr      NC,MU32_1       ; if no Carry then jump over
        ld      H,B
        ld      L,C
MU32_1: ld      A,$0F
MUL_32L:add     HL,HL           ; main loop
        rl      E
        rl      D
        jr      NC,MU32_2
        add     HL,BC
        jr      NC,MU32_2
        inc     DE
MU32_2: dec     A
        jr      NZ,MUL_32L
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
; destroys: A, B
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
        
; ----------------------------------------------------------------------
; divide a 16-bit number by a 16-bit number
; (16/16 division)
;
; inputs: AC (Dividend), DE (divisor)
; destroys: HL,A,C
; OPERATION: AC/DE
; returns: AC (quotient), HL (remainder)
; source: WKT
DIV_16_16:  ld      HL, 0
            ld      B, 16
DV16_16_LP: sla     C
            set     0,C         ; this simulates the SLL undocumented instruction
            rla
            adc     HL,HL
            sbc     HL,DE
            jr      NC, $+4
            add     HL,DE
            dec     C
            djnz    DV16_16_LP
            ret


; ----------------------------------------------------------------------
; divide a 32-bit number by a 16 bit-number
; (32/16 division)
;
; inputs: ACIX (Dividend), DE (divisor)
; destroys: HL,IX,BC
; OPERATION: ACIX/DE
; returns: ACIX (quotient), HL (remainder)
; source: WKT

DIV_32_16:  ld      HL,0
            ld      B,32
DIV_32_16LP:add     IX,IX
            rl      C
            rla
            adc     HL,HL
            jr      C,DIV_32_16OF
            sbc     HL,DE
            jr      NC,DIV_32_16SB
            add     HL,DE
            djnz    DIV_32_16LP
            ret
DIV_32_16OF:or      A                   ; overflow
            sbc     HL,DE
DIV_32_16SB:inc     IX                  ; set bit
            djnz    DIV_32_16LP
            ret


; ----------------------------------------------------------------------
; convert a 32-bit number in ASCII string (terminated by '0')
;
; inputs: DEIX (Value), IY (dest. address in memory)
; destroys: AF, BC, DE, HL, IX
; outputs: IY (last char in dest. string)
; source: MSX Forum

CLCN32T:    defw    1,0,10,0,100,0,1000,0,10000,0
            defw    $86A0,$1,$4240,$F,$9680,$98,$E100,$5F5,$CA00,$3B9A
CLCN32Z:    defs    4

CLCN32:     ld      (CLCN32Z),IX
            ld      (CLCN32Z+$02),DE
            ld      IX,CLCN32T+$24
            ld      B,$09
            ld      C,$00
CLCN321:    ld      A,"0"
            or      A
CLCN322:    ld      E,(IX+$00)
            ld      D,(IX+$01)
            ld      HL,(CLCN32Z)
            sbc     HL,DE
            ld      (CLCN32Z),HL
            ld      E,(IX+$02)
            ld      D,(IX+$03)
            ld      HL,(CLCN32Z+$02)
            sbc     HL,DE
            ld      (CLCN32Z+$02),HL
            jr      C,CLCN325
            inc     C
            inc     A
            jr      CLCN322
CLCN325:    ld      E,(IX+$00)
            ld      D,(IX+$01)
            ld      HL,(CLCN32Z)
            add     HL,DE
            ld      (CLCN32Z),HL
            ld      E,(IX+$02)
            ld      D,(IX+$03)
            ld      HL,(CLCN32Z+$02)
            adc     HL,DE
            ld      (CLCN32Z+$02),HL
            ld      DE,-4
            add     IX,DE
            inc     C
            dec     C
            jr      Z,CLCN323
            ld      (IY+$00),A
            inc     IY
CLCN323:    djnz    CLCN321
            ld      A,(CLCN32Z)
            add     "0"
            ld      (IY+$00),A
            ld      (IY+$01),0
            ret