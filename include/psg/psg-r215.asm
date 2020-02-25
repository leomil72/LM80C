; ------------------------------------------------------------------------------
; LM80C - PSG ROUTINES - R2.15
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
; R2.13  - 20200127 - Implemented ALT & CTRL keys to print graphic chars with keyboard;
;                     code improvements; faster cursor flashing
; R2.14  - 20200203 - Better CLS code (no more color flashes in graphics modes); added SYS command
; R2.15  - 20200225 - Now the computer starts as a stand-alone system, with serial disabled;
;                     new SERIAL command
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


; routines to play a welcome beep on channel C (tone 4010) and to shut it off
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
                ld      B,$03           ; 3 channels to check
                ld      H,$01           ; channel A=>bit 1 into mixer, B=>bit 2, C=>bit 3
CHKSNDCH:       ld      E,(IX+0)        ; load LSB into E
                ld      D,(IX+1)        ; load MSB into D
                ld      A,E             ; load E into A
                or      D               ; do E OR D, to check that DE=0 (if just a bit is 1, the result will be <> 0)
                jr      Z,CNTCHKSND     ; yes, jump over
                dec     DE              ; no, so decrement DE
                ld      A,E             ; reload E into A...
                ld      (IX+0),E        ; store new...
                ld      (IX+1),D        ; ...duration and...
                or      D               ; ...do another check to see if DE=0
                jr      NZ,CNTCHKSND    ; no, so jump over
                                        ; if yes, let's shut down the corresponding channel
                                        ; to shut down a tone we set 0 into tone register
                                        ; and disable the channel into mixer
                ld      A,$03           ; three channels
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

; read a specific row of the keyboard matrix, set by A
; return reading into A
READKBLN:       push    BC
                ld      B,$0E           ; reg #14
                ld      C,PSG_REG       ; PSG register port
                out     (C),B           ; select reg #14
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; activate SHIFT row
                ld      B,$0F           ; register #15 (port B)
                ld      C,PSG_REG       ; PSG register port
                out     (C),B           ; select reg. 15 (port B)
                in      A,(C)           ; read register #15
                pop     BC
                ret

; read the keyboard matrix to look for a key pressure
KEYBOARD:       ld      C,PSG_REG       ; PSG register port
                ld      B,$07           ; set register #7...
                out     (C),B           ; ...to work with
                in      A,(C)           ; read register #7
                set     6,A             ; port A set to output
                res     7,A             ; port B set to input
                out     (C),B           ; set register #7
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; set I/O ports w/o altering the rest of the mixer
CHECKSHIFT:     ld      A,11111101b     ; select SHIFT row
                call    READKBLN        ; read row
                bit     3,A             ; test if SHIFT key is pressed (4th bit is reset)
                jr      NZ,CHECKALT     ; no, so go on
                ld      HL,CONTROLKEYS  ; control key flags
                ld      (HL),00000001b  ; set SHIFT flag, reset CTRL & ALT flags (currently multiply control keys are NOT supported)
CHECKALT:       ld      A,11111110b     ; select ALT row
                call    READKBLN        ; read ALT row
                bit     5,A             ; test if ALT key is pressed (5th bit is reset)
                jr      NZ,CHECKCTRL    ; no, so go on
                ld      HL,CONTROLKEYS  ; control key flags
                ld      (HL),00000100b  ; set ALT flag, reset SHIFT & CTRL flag (currently multiply control keys are NOT supported)
CHECKCTRL:      ld      A,11111110b     ; select CTRL row
                call    READKBLN        ; read CTRL row
                bit     2,A             ; test if CTRL key is pressed (3rd bit is reset)
                jr      NZ,CHECKKBD     ; no, so make a normal reading
                ld      HL,CONTROLKEYS  ; control key flags
                ld      (HL),00000010b  ; set CTRL flag, reset SHIFT & ALT flags (currently multiply control keys are NOT supported)
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
                jr      Z,NOKEYPRSD     ; no, go to the next row
CHECKCTRLKEYS:  ld      (KBTMP),A       ; yes, check if a control key was pressed. First, store current row
                ld      A,B             ; copy current row (B) into A
                cp      $02             ; is it the row of the SHIFT?
                jr      NZ,TESTALT      ; no, continue checking the other control keys
                ld      A,(KBTMP)       ; yes, retrieve current row data
                bit     3,A             ; check SHIFT bit line
                jr      NZ,FINDKEY      ; no SHIFT, continue checking
                set     3,A             ; yes, it's the SHIFT. So remove SHIFT bit
                cp      $FF             ; after deleting the SHIFT bit, is there any other bit selected?
                jr      NZ,FINDKEY      ; yes, go to check which one
                jr      NOKEYPRSD       ; no, go to next row        
TESTALT:        cp      $01             ; is it the line of ALT & CTRL?
                ld      A,(KBTMP)       ; retrieve current row data
                jr      NZ,FINDKEY      ; no, continue
                bit     5,A             ; yes, check ALT bit line
                jr      NZ,TESTCTRL     ; no ALT, continue checking
                set     5,A             ; yes, it's the ALT. So remove ALT bit
TESTCTRL:       bit     2,A             ; check CTRL bit line
                jr      NZ,ENDCTRLCK    ; no CTRL, continue checking
                set     2,A             ; delete CTRL bit flag
ENDCTRLCK:      cp      $FF             ; after deleting the ALT & CTRL bits, is there any other bit selected?
                jr      NZ,FINDKEY      ; yes, go to check which one
NOKEYPRSD:      ld      A,E             ; no key pressed, load current output port
                rrca                    ; rotate right by 1
                djnz    RPTKBDRD        ; repeat for 8 lines
                xor     A               ; if exit from here, no key has been pressed...
                ld      (LASTKEYPRSD),A ; ...so reset the last key cell...
                ld      (CONTROLKEYS),A ; reset contro key flags
                ret                     ; ...and leave
FINDKEY:        ld      E,$FF           ; counter
CHKLN:          inc     E               ; E goes from 0 to 7
                srl     A               ; is the first bit reset? (we're looking for a "0", meaning grounded line)
                jr      C,CHKLN         ; no, check next bit
LOADKEYMAP:     ld      A,(CONTROLKEYS) ; load control key flags
                ld      HL,KBMAP        ; normal keymap
                cp      $01             ; SHIFT flag?
                jr      NZ,CHKCTRL      ; no
                ld      HL,KBMAP_SFT    ; SHIFT keymap
                jr      LOADMAP
CHKCTRL:        cp      $02             ; CTRL flag?
                jr      NZ,CHKALT       ; no
                ld      HL,KBMAP_CTRL   ; CTRL map
                jr      LOADMAP
CHKALT:         cp      $04             ; ALT flag?
                jr      NZ,LOADMAP      ; no
                ld      HL,KBMAP_ALT
LOADMAP:        dec     B               ; decrement row # (rows go from 0 to 7)
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
KBMAP:          defb '1',25,14,3,' ',16,'q','2'         ; 25=HOME  14=CTRL  3=RUN/STOP 16=C=
                defb '3','w','a',20,'z','s','e','4'     ; 20=SHIFT
                defb '5','r','d','x','c','f','t','6'
                defb '7','y','g','v','b','h','u','8'
                defb '9','i','j','n','m','k','o','0'
                defb 31,'p','l',',','.',':','-',30      ; 31=CURSOR DOWN  30=CURSOR UP
                defb 28,'*',';','/',27,'=','+',29       ; 28=CURSOR LEFT  27=ESCAPE  29=CURSOR RIGHT
                defb 8,13,252,'@',1,2,4,24              ; 8=DEL(backspace)  13=RETURN  252=£  1=F1  2=F2  4=F3  24=HELP

; shifted codes - not all the keys have the shifted version
KBMAP_SFT:      defb '!',12,14,3,' ',16,'Q',34          ; 12=CLEAR  14=CTRL  3=RUN/STOP 16=C=   34="
                defb '#','W','A',20,'Z','S','E','$'     ; 20=SHIFT
                defb '%','R','D','X','C','F','T','&'
                defb 39,'Y','G','V','B','H','U','('     ; 39='
                defb ')','I','J','N','M','K','O',94     ; 94=^
                defb 31,'P','L','<','>','[','_',30      ; 31=CURSOR DOWN  30=CURSOR UP
                defb 28,'*',']','?',27,198,'+',29       ; 28=CURSOR LEFT  27=ESCAPE  29=CURSOR RIGHT
                defb 8,13,211,'@',5,6,22,23             ; 211=€  5=F4  6=F5  22=F6  23=F7

; ALT (C=) codes - not all the keys have the alt-ed version
KBMAP_ALT:      defb '1',12,14,3,' ',16,222,196         ; 12=CLEAR  14=CTRL  3=RUN/STOP  16=C=  34="
                defb '3',221,133,20,131,130,165,'4'     ; 20=SHIFT
                defb '5',162,166,132,157,163,168,'6'
                defb '7',171,169,161,158,172,213,'8'    ;
                defb '9',214,216,159,160,215,135,195    ;
                defb 31,136,138,193,192,123,144,30      ; 31=CURSOR DOWN  123={  30=CURSOR UP
                defb 28,143,125,254,27,209,148,29       ; 28=CURSOR LEFT  125=}  27=ESCAPE  29=CURSOR RIGHT
                defb 8,13,224,137,5,6,22,23             ; 8=DEL(backspace)  13=RETURN  252=£  5=F4  6=F5  22=F6  23=F7

; CTRL codes - not all the keys have the control-ed version
KBMAP_CTRL:     defb '1',25,14,3,' ',16,154,'2'         ; 25=HOME  14=CTRL  3=RUN/STOP  16=C=
                defb '3',156,149,20,152,150,153,'4'     ; 20=SHIFT
                defb '5',155,176,151,177,175,165,'6'
                defb '7',166,168,178,179,169,167,'8'
                defb '9',184,170,172,171,181,164,'0'
                defb 31,163,173,',','.',':',186,30      ; 31=CURSOR DOWN  30=CURSOR UP
                defb 28,225,';','/',27,212,185,29       ; 28=CURSOR LEFT  27=ESCAPE  212=π  29=CURSOR RIGHT
                defb 8,13,189,162,1,2,4,24              ; 8=DEL(backspace)  13=RETURN  252=£  1=F1  2=F2  4=F3  24=HELP