; ------------------------------------------------------------------------------
; LM80C - PSG ROUTINES - R2.12
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. Code and computer schematics are released under
; the therms of the GNU GPL License 3.0 and in the form of "as is", without no
; kind of warranty: you can use them at your own risk.
; You are free to use them for any non-commercial use: you are only asked to
; maintain the copyright notices, include this advice and the note to the 
; attribution of the original version to Leonardo Miliani, if you intend to
; redistribuite them.
; https://www.leonardomiliani.com
; 
; Please support me by visiting the following links:
; Main project page: https://www.leonardomiliani.com
; Schematics and code: https://github.com/leomil72/LM80C
; Videos about the computer: https://www.youtube.com/user/leomil72/videos
; Hackaday page: https://hackaday.io/project/165246-lm80c-color-computer
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
; R2.8.2 - 20191215 - Fixed a bug introduced with 2.8.1 that lead to wrong functioning of
;                     several BASIC statements (system tick counter, Locate, etc..)
; R2.9   - 20191222 - Code cleaning; improved SOUND statement; revision of PSG code;
;                     revision of release notes; add support for cursor keys & cursor movements
; R2.10  - 20191226 - SIO init code cleaning & improved support for serial RX; added extended
;                     char codes (128-255) for 6x8 fonts; removed double chars in 8x8 fonts
; R2.11  - 20200110 - Set graphics 2 VRAM in a better way; fixed TAB() function; new SCREEN 4 mode;
;                     new PLOT, DRAW, and CIRCLE commands
; R2.12  - 20200124 - Code optimizing; fixed a bug into the CIRCLE routine; new splash screen
;                     with a graphic logo
;
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; configure the PSG
initPSG:        ld      HL,CHASNDDTN    ; starting address of sound & keyboard RAM registers
                ld      B,$0A           ; # of PSG sound & keyboard registers
                xor     A               ; reset A
EMPTSNDBFR:     ld      (HL),A          ; reset RAM register
                inc     HL              ; next register
                djnz    EMPTSNDBFR      ; repeat
CLRPSGREGS:     ld      B,$10           ; 16 registers to set
                ld      HL,SNDREGCFG    ; starting address of register settings
                ld      D,$00           ; first register
RSTPSG:         ld      A,D             ; register value
                call    SETSNDREG       ; select register
                ld      A,(HL)          ; load value
                call    WRTSNDREG       ; write to register
                inc     D               ; next register
                inc     HL              ; next value
                djnz    RSTPSG          ; repeat for each register
                ret                     ; return to caller

SNDREGCFG:      defb $00,$00,$00,$00,$00,$00,$00,01111111b
                defb $00,$00,$00,$00,$00,$00,$ff,$ff
                ; reg. 7: set I/O ch.A to OUTPUT, I/O ch.B to INPUT; set noise to OFF; set audio to OFF


; routines to play a welcome beep on channel C (fq. 4000) and to shut it off
WLCMBEEP:       ld      HL,WLCBPDAT     ; data address
                jp      SENDSND
NOBEEP:         ld      HL,NOBPDAT      ; data address
SENDSND:        push    BC
                ld      B,$04           ; 4 pairs
RPTWLCMBP:      ld      A,(HL)          ; read register #
                call    SETSNDREG
                inc     HL              ; next cell
                ld      A,(HL)          ; read value
                call    WRTSNDREG
                inc     HL
                djnz    RPTWLCMBP       ; repeat
                pop     BC
                ret                     ; return to caller
WLCBPDAT:       defb    $07,01111011b,$04,$56,$05,$00,$0A,$0F
NOBPDAT:        defb    $04,$00,$05,$00,$0A,$00,$07,01111111b


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
MNGSNDS:        push    IX              ; store IX
                ld      IX,CHASNDDTN    ; starting address of tones duration
                ld      B,03H           ; 3 channels to check
                ld      H,01h           ; channel A=>bit 1 into mixer, B=>bit 2, C=>bit 3
CHKSNDCH:       ld      E,(IX+0)        ; load LSB into E
                ld      D,(IX+1)        ; load MSB into D
                ld      A,E             ; load E into A
                or      D               ; do E OR D, to check that DE=0 (if just a bit is 1, the result will be <> 0)
                jr      Z,CNTCHKSND     ; yes, jump over
                dec     de              ; no, so decrement DE
                ld      A,E             ; reload E into A...
                ld      (IX+0),E        ; store new...
                ld      (IX+1),D        ; ...duration and...
                or      D               ; ...do another check to see if DE=0
                jr      NZ,CNTCHKSND    ; no, so jump over
                                        ; yes, let's shut down the corresponding channel
                                        ; to shut down a tone we set 0 into tone register
                                        ; and disable the channel into mixer
                ld      A,03h           ; three channels
                sub     B               ; find current channel (0->A, 1->B, 2->C)
                add     A,A             ; and find first register (A=>0, B=>2, C=>4)
                ld      C,PSG_REG       ; PSG register selector port
                out     (C),A           ; select first tone register of channel
                ld      L,$00           ; value 0 into L
                ld      C,PSG_DAT       ; PSG data selector port
                out     (C),L           ; write 0 into register
                ld      C,PSG_REG       ; PSG register selector port
                inc     A               ; next tone register
                out     (C),A           ; select second tone register of channel
                ld      C,PSG_DAT       ; PSG data selector port
                out     (C),L           ; write 0 into register
                ld      A,$07           ; mixer register
                ld      C,PSG_REG       ; PSG register selector port
                out     (C),A           ; set mixer register
                ld      C,PSG_DAT       ; PSG data port
                in      A,(C)           ; load current value
                or      H               ; set off the channel into the mixer (remember that 1=OFF)
                out     (C),A           ; send new value for the mixer
CNTCHKSND:      inc     IX              ; set for...
                inc     IX              ; ...next channel...
                sla     H               ; shift left H 1 bit
                djnz    CHKSNDCH        ; repeat for 3 channels
                pop     IX              ; restore IX
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
                jr      NZ,CHECKALT     ; no, so go on
                ld      HL,CONTROLKEYS  ; control key flags
                set     0,(HL)          ; set SHIFT flag
                res     2,(HL)          ; reset ALT bit -> at the moment ALT+SHIFT is NOT supported
CHECKALT:       ld      D,$0E           ; reg #14
                ld      C,PSG_REG       ; PSG register port
                out     (C),D           ; select reg #14
                ld      A,11111110b     ; select ALT row
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; activate ALT row
                ld      D,$0F           ; register #15 (port B)
                ld      C,PSG_REG       ; PSG register port
                out     (C),D           ; select reg. 15 (port B)
                nop
                in      A,(C)           ; read register #15
                bit     2,A             ; test if ALT key is pressed (3rd bit is reset)
                jr      NZ,CHECKKBD     ; no, so make a normal reading
                ld      HL,CONTROLKEYS  ; control key flags
                set     2,(HL)          ; set ALT flag
                res     0,(HL)          ; reset SHIFT bit -> at the moment ALT+SHIFT is NOT supported
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
                ld      (CONTROLKEYS),A ; reset control key flags
                jr      LVKBRDCHK       ; ...and leave
CHECKCTRLKEYS:  ld      (KBTMP),A       ; store A
                ld      A,B             ; copy B into A
                cp      $02             ; is it the line of the SHIFT?
                ld      A,(KBTMP)       ; retrieve A
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
                jr      Z,CHKALT        ; SHIFT not pressed, check ALT
                ld      HL,KBMTXSFT     ; load SHIFTed keymap
                jr      NORMAP          ; go on
CHKALT:         bit     2,(HL)          ; check if there is the ALT flag
                jr      Z,NORMAP        ; no, go on
                ld      HL,KBMTXALT     ; load ALTed map
NORMAP:         dec     B               ; decrement row # (rows go from 0 to 7)
                ld      C,B             ; move B into C and...
                sla     C               ; ...multiply it...
                sla     C               ; ...by 8 to find...
                sla     C               ; ...the current row into the matrix
                ld      B,$00           ; reset B
                add     HL,BC           ; find the address of the current row
                ld      D,B             ; reset D
                add     HL,DE           ; find the current column
                ld      A,(LASTKEYPRSD) ; load the last key pressed
                cp      (HL)            ; is it the same key?
                jr      Z,LVKBRDCHK     ; yes, so do nothing
                ld      A,(HL)          ; no, load it...
                ld      (LASTKEYPRSD),A ; ...store it...
                ld      (TMPKEYBFR),A   ; ...also into the INKEY buffer...
                call    CHARINTOBFR     ; ...and insert char into the input buffer
                xor     A
                ld      (CONTROLKEYS),A ; reset contro key flags
LVKBRDCHK:      ret                     ; return to caller: the current key code is into A
                                        ; (0 means no key pressed)               

; key codes
KBMTX:          defb '1',25,14,3,' ',16,'q','2'         ; 25=HOME  14=CTRL  3=RUN/STOP(same as CTRL-C)  16= C=
                defb '3','w','a',20,'z','s','e','4'     ; 20=SHIFT
                defb '5','r','d','x','c','f','t','6'
                defb '7','y','g','v','b','h','u','8'
                defb '9','i','j','n','m','k','o','0'
                defb 31,'p','l',',','.',':','-',30      ; 31=CURSOR DOWN  30=CURSOR UP
                defb 28,'*',';','/',27,'=','+',29       ; 28=CURSOR LEFT  27=ESCAPE  29=CURSOR RIGHT
                defb 8,13,252,'@',1,2,4,24              ; 8=DEL(backspace)  13=RETURN  252=£  1=F1  2=F2  4=F3  24=HELP

; shifted codes - not all the keys have the shifted version
KBMTXSFT:       defb '!',12,14,3,' ',16,'Q',34          ; 12=CLEAR  14=CTRL  3=RUN/STOP(same as CTRL-C)  16= C=   34="
                defb '#','W','A',20,'Z','S','E','$'     ; 20=SHIFT
                defb '%','R','D','X','C','F','T','&'
                defb 39,'Y','G','V','B','H','U','('     ; 39='
                defb ')','I','J','N','M','K','O',94     ; 94=^
                defb 31,'P','L','<','>','[','_',30      ; 31=CURSOR DOWN  30=CURSOR UP
                defb 28,'*',']','?',27,212,'+',29       ; 28=CURSOR LEFT  27=ESCAPE  212=π  29=CURSOR RIGHT
                defb 8,13,211,'@',5,6,22,23             ; 211=€  5=F4  6=F5  22=F6  23=F7

; ALT (C=) codes - not all the keys have the alt-ed version
KBMTXALT:       defb '1',12,14,3,' ',16,222,'2'         ; 12=CLEAR  14=CTRL  3=RUN/STOP(same as CTRL-C)  16= C=  34="
                defb '3',221,133,20,131,130,165,'4'     ; 20=SHIFT
                defb '4',162,166,132,157,163,168,'6'
                defb '7',171,169,161,158,172,213,'8'    ;
                defb '8',214,216,159,160,215,149,'0'    ;
                defb 31,150,138,'<','>',127,'-',30      ; 31=CURSOR DOWN  127={  30=CURSOR UP
                defb 28,'*',125,'?',27,209,148,29       ; 28=CURSOR LEFT  125=}  27=ESCAPE  212=π  29=CURSOR RIGHT
                defb 8,13,224,137,5,6,22,23             ; 8=DEL(backspace)  13=RETURN  252=£  5=F4  6=F5  22=F6  23=F7