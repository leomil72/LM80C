; ------------------------------------------------------------------------------
; LM80C - PSG ROUTINES - R2.8.1
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Code Revision:
; R2.1   - 20190818 - Added SOUND command to play simple tones and VOLUME command
; R2.1a  - 20190908 - Cursor management improvements
; R2.2   - 20190920 - Fixed cursor bug within SCREEN statement; new command PAUSE
; R2.3   - 20190930 - Fixed bugs in SOUND command
; R2.4   - 20191013 - Added new graphic chars and reorganized previous ones
; R2.4a  - 20191015 - More graphic chars
; R2.5   - 20191026 - Revision of init PSG code; revision of serial buffer exp. code;
;                     fixed a bug into the video buffer manager
; R2.6   - 20191102 - New function INKEY to read a key without a prompt;
;                     source code cleaning
; R2.7   - 20191116 - Fixed a bug into the INKEY code
; R2.8   - 20191207 - Minor bug fixes; added support for built-in keyboard;
;                     revision of some char codes;
; R2.8.1 - 20191208 - Introduced support for SHIFT key for uppercase letters & alternate chars
;
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; configure the PSG
initPSG:        ld      B,$10           ; 15 registers (16th reg. is not set, since it's set to input)
                ld      HL,SNDREGCFG    ; start address of register settings
                ld      D,$00           ; first register
RSTPSG:         ld      A,D             ; register value
                call    SETSNDREG       ; select register
                ld      A,(HL)          ; load value
                call    WRTSNDREG       ; write to register
                inc     D               ; next register
                inc     HL              ; next value
                djnz    RSTPSG          ; repeat for each register
                ld      HL,CHASNDDTN    ; clear channel duration registers
                ld      B,$03           ; 3 registers (3x 2 bytes = 6 cells)
EMPTSNDBFR:     xor     A               ; reset A
                ld      (HL),A          ; reset MSB cell
                inc     HL
                ld      (HL),A          ; reset LSB cell
                inc     HL
                djnz    EMPTSNDBFR      ; repeat
                ld      (LASTKEYPRSD),A ; reset last key pressed
                ld      (TMPKEYBFR),A   ; reset temp key buffer
                ld      (CONTROLKEYS),A ; reset control key flags
                ret                     ; return to caller

SNDREGCFG:      defb $00,$00,$00,$00,$00,$00,$00,01111111b
                defb $00,$00,$00,$00,$00,$00,$ff,$ff  ; 16th reg. is not set since it's set to input
                ; reg. 7: set I/O ch.A to OUTPUT, I/O ch.B to INPUT; set noise to OFF; set audio to OFF


; play a welcome beep
WLCMBEEP:       ld      A,$07           ; register 7
                call    SETSNDREG
                ld      A,01111011b     ; play on channel C
                call    WRTSNDREG
                ld      A,$0A           ; register 10
                call    SETSNDREG
                ld      A,$0F           ; volume to 15 on channel C
                call    WRTSNDREG
                ld      A,$04           ; register 4
                call    SETSNDREG
                ld      A,$60           ; low byte of tone
                call    WRTSNDREG
                ld      A,$05           ; register 5
                call    SETSNDREG
                xor     A               ; high byte of tone
                call    WRTSNDREG
                ld      A,$0A           ; register 10
                call    SETSNDREG
                ld      A,$0F           ; set volume to 15
                call    WRTSNDREG
                ret                     ; return to caller


; shut off the welcome beep
NOBEEP:         ld      A,$04           ; register 4
                call    SETSNDREG
                xor     A               ; set low byte of tone to 0
                call    WRTSNDREG
                ld      A,$0A           ; register 10
                call    SETSNDREG
                xor     A               ; set high byte of tone to 0
                call    WRTSNDREG
                ld      A,$07           ; register 7
                call    SETSNDREG
                ld      A,01111111b     ; disable output on channels
                call    WRTSNDREG
                ret                     ; return to caller

; select register on PSG
SETSNDREG:      ld      C,PSG_REG       ; PSG register port
                out     (C),A           ; set register
                ret                     ; return to caller

; send data to PSG
WRTSNDREG:      ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; send data
                ret                     ; return to caller

; manage the sounds' duration: each time this subroutine is called, it
; decrements the single sound durations (measured in ms) and eventually
; shut off the audio channel whose counter has reached 0.
; (this sub-routine is called by CH3 timer ISR)
MNGSNDS:        push    ix              ; store IX
                ld      ix,CHASNDDTN    ; starting address of tones duration
                ld      b,03H           ; 3 channels to check
                ld      h,01h           ; channel A=>bit 1 into mixer, B=>bit 2, C=>bit 3
CHKSNDCH:       ld      e,(ix+0)        ; load LSB into E
                ld      d,(ix+1)        ; load MSB into D
                ld      a,e             ; load E into A
                or      d               ; do E OR D, to check that DE=0 (if just a bit is 1, the result will be <> 0)
                jr      z,CNTCHKSND     ; yes, jump over
                dec     de              ; no, so decrement DE
                ld      a,e             ; reload E into A...
                ld      (ix+0),e        ; store new...
                ld      (ix+1),d        ; ...duration and...
                or      d               ; ...do another check to see if DE=0
                jr      nz,CNTCHKSND    ; no, so jump over
                                        ; yes, let's shut down the corresponding channel
                                        ; to shut down a tone we set 0 into tone register
                                        ; and disable the channel into mixer
                ld      a,03h           ; three channels
                sub     b               ; find current channel (0->A, 1->B, 2->C)
                add     a,a             ; and find first register (A=>0, B=>2, C=>4)
                ld      c,PSG_REG       ; PSG register selector port
                out     (c),a           ; select first tone register of channel
                ld      l,00h           ; value 0 into L
                ld      c,PSG_DAT       ; PSG data selector port
                out     (c),l           ; write 0 into register
                ld      c,PSG_REG       ; PSG register selector port
                inc     a               ; next tone register
                out     (c),a           ; select second tone register of channel
                ld      c,PSG_DAT       ; PSG data selector port
                out     (c),l           ; write 0 into register
                ld      a,$07           ; mixer register
                ld      c,PSG_REG       ; PSG register selector port
                out     (c),a           ; set mixer register
                ld      c,PSG_DAT       ; PSG data port
                in      a,(c)           ; load current value
                or      h               ; set off the channel into the mixer (remember that 1=OFF)
                out     (c),a           ; send new value for the mixer
CNTCHKSND:      inc     ix              ; set for...
                inc     ix              ; ...next channel...
                sla     h               ; shift left H 1 bit
                djnz    CHKSNDCH        ; repeat for 3 channels
                pop     ix              ; restore IX
                ret                     ; return to caller

; read the keyboard matrix to look for a key pressure
KEYBOARD:       ld      C,PSG_REG       ; PSG register port
                ld      B,$07           ; set register #7...
                out     (C),B           ; ...to work with
                nop
                in      A,(C)           ; read register #7
                set     6,A             ; port A set to output
                res     7,A             ; port B set to input
                out     (C),B           ; set register #7
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; set I/O ports w/o altering the rest of the mixer
                ld      D,$0E           ; reg #14
                ld      C,PSG_REG       ; PSG register port
                out     (C),D           ; select reg #14
                ld      A,11111101b     ; select SHIFT row
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; activate SHIFT row
                ld      D,$0F           ; register #15 (port B)
                ld      C,PSG_REG       ; PSG register port
                out     (C),D           ; select reg. 15 (port B)
                nop
                in      A,(C)           ; read register #15
                bit     3,A             ; test if SHIFT key is pressed (4th bit is reset)
                jr      NZ,CHECKKBD     ; no, so make a normal reading
                ld      HL,CONTROLKEYS  ; control key flags
                set     0,(HL)          ; set SHIFT flag
CHECKKBD:       ld      B,$08           ; 8 lines
                ld      A,01111111b     ; start from the last line of the matrix
RPTKBDRD:       ld      D,$0E           ; register #14 (port A)
                ld      C,PSG_REG       ; PSG register port
                out     (C),D           ; select reg. #14
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; activate 1 line (active line is grounded, i.e. with a LOW signal)
                ld      E,A             ; save current line into E
                ld      D,$0F           ; register #15 (port B)
                ld      C,PSG_REG       ; PSG register port
                out     (C),D           ; select reg. 15 (port B)
                nop
                in      A,(C)           ; read register #15
                cp      $FF             ; is there any line set to 0?
                jr      NZ,CHECKCTRLKEYS; yes, first check if a control key was pressed
                ld      A,E             ; no, so load current output port
                rrca                    ; rotate right by 1 (go to the next one)
                djnz    RPTKBDRD        ; repeat for 8 lines
                xor     A               ; if exit from here, no key has been pressed...
                ld      (LASTKEYPRSD),A ; ...so reset the last key cell...
                ld      (CONTROLKEYS),A ; reset contro key flags
                jr      LVKBRDCHK       ; ...and leave
CHECKCTRLKEYS:  ld      (TMPBFR1),A     ; store A
                ld      A,B             ; copy B into A
                cp      $02             ; is it the line of the SHIFT?
                ld      A,(TMPBFR1)     ; retrieve A
                jr      NZ,KEYFOUND     ; no, continue
                bit     3,A             ; check SHIFT bit line
                jr      NZ,KEYFOUND     ; no SHIFT, continue checking
                set     3,A             ; yes, it's the SHIFT. So remove SHIFT bit
                cp      $FF             ; after deleting the SHIFT bit, is there any other bit selected?
                jr      NZ,KEYFOUND     ; yes, go to check which one
                ld      A,E             ; load current output port
                rrca                    ; rotate right by 1
                djnz    RPTKBDRD        ; repeat for 8 lines
                xor     A               ; if exit from here, no key has been pressed...
                ld      (LASTKEYPRSD),A ; ...so reset the last key cell...
                ld      (CONTROLKEYS),A ; reset contro key flags
                jr      LVKBRDCHK       ; ...and leave
KEYFOUND:       ld      E,$FF           ; counter
CHKLN:          inc     E               ; E goes from 0 to 7
                srl     A               ; is the first bit reset? (we're looking for a "0", meaning grounded line)
                jr      C,CHKLN         ; no, check next bit
                ld      HL,CONTROLKEYS  ; check if there is the...
                bit     0,(HL)          ; ...SHIFT flag set
                ld      HL,KBMTX        ; load normal keymap
                jr      Z,NORMAP        ; SHIFT not pressed
                ld      HL,KBMTXSFT     ; load SHIFTed keymap
NORMAP:         dec     B               ; decrement row # (rows go from 0 to 7)
                ld      C,B             ; move B into C and...
                sla     C               ; ...multiply it...
                sla     C               ; ...by 8 to find...
                sla     C               ; ...the current row into the matrix
                ld      B,$00           ; reset B
                add     HL,BC           ; find the address of the current row
                ld      D,$00           ; reset D
                add     HL,DE           ; find the current column
                ld      A,(LASTKEYPRSD) ; load the last key pressed
                cp      (HL)            ; is it the same key?
                jr      Z,LVKBRDCHK     ; yes, so do nothinh
                ld      A,(HL)          ; no, load it...
                ld      (LASTKEYPRSD),A ; ...store it...
                ld      (TMPKEYBFR),A
                call    CHARINTOBFR     ; ... and insert char into the buffer
                xor     A
                ld      (CONTROLKEYS),A ; reset contro key flags
LVKBRDCHK:      ret                     ; return to caller: the current key code is into A (0 means no key pressed)               

; key codes
KBMTX:          defb '1',12,14,3,' ',16,'q','2'         ; 12=CLEAR/HOME  14=CTRL  3=RUN/STOP(same as CTRL-C)  16= C=
                defb '3','w','a',20,'z','s','e','4'     ; 20=SHIFT
                defb '5','r','d','x','c','f','t','6'
                defb '7','y','g','v','b','h','u','8'
                defb '9','i','j','n','m','k','o','0'
                defb 31,'p','l',',','.',':','-',30      ; 31=CURSOR DOWN  30=CURSOR UP
                defb 28,'*',';','/',27,'=','+',29       ; 28=CURSOR LEFT  27=ESCAPE  29=CURSOR RIGHT
                defb 8,13,95,'@',1,2,4,24               ; 8=DEL(backspace)  13=RETURN  95=€  1=F1  2=F2  4=F3  24=HELP

; shifted codes - not all the keys have the shifted version
KBMTXSFT:       defb '!',12,14,3,' ',16,'Q',34          ; 34="
                defb '#','W','A',20,'Z','S','E','$'     ; 20=SHIFT
                defb '%','R','D','X','C','F','T','&'
                defb 39,'Y','G','V','B','H','U','('     ; 39='
                defb ')','I','J','N','M','K','O',94     ; 94=^
                defb 31,'P','L','<','>','[','-',30
                defb 28,'*',']','?',27,'=','+',29
                defb 8,13,95,'@',5,6,22,23              ; 5=F4  6=F5  22=F6  23=F7