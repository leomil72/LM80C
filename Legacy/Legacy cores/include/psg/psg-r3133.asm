; ------------------------------------------------------------------------------
; LM80C - PSG ROUTINES - R3.13.3
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
;
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; configure the PSG
initPSG:        ld      HL,CHASNDDTN    ; starting address of sound & keyboard RAM registers
                ld      B,SERIALS_EN-CHASNDDTN; # of PSG sound & keyboard registers
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

SNDREGCFG:      defb $00,$00,$00,$00,$00,$00,$00,%10111111
                defb $00,$00,$00,$00,$00,$00,$ff,$ff
                ; reg. 7: set I/O ch.A to OUTPUT, I/O ch.B to INPUT; set noise to OFF; set audio to OFF


; routine to play a welcome beep on channel C (tone 4010) and to shut it off
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

WLCBPDAT:       defb    $07,%01111011,$04,$56,$05,$00,$0A,$0F
NOBPDAT:        defb    $04,$00,$05,$00,$0A,$00,$07,%01111111


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
                ld      H,$01           ; mixer channels: A=>bit 1, B=>bit 2, C=>bit 3
CHKSNDCH:       ld      E,(IX+0)        ; load LSB into E
                ld      D,(IX+1)        ; load MSB into D
                ld      A,E             ; load E into A
                or      D               ; check that DE=0
                jr      Z,CNTCHKSND     ; yes, jump over
                dec     DE              ; no, so decrement DE
                ld      A,E             ; reload E into A...
                ld      (IX+0),E        ; store new...
                ld      (IX+1),D        ; ...duration and...
                or      D               ; ...do another check to see if DE=0
                jr      NZ,CNTCHKSND    ; no, so jump over
                                        ; if yes, let's shut down the corresponding channel
                                        ; to shut down a tone we disable it into the mixer
                                        ; then set 0 into its tone registers
                ld      D,$07           ; mixer register
                ld      C,PSG_REG       ; PSG register selector port
                out     (C),D           ; set mixer register
                in      A,(C)           ; load current value
                or      H               ; set off the channel into the mixer (remember that 1=OFF)
                out     (C),D           ; select mixer register
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; send new value for the mixer
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
CNTCHKSND:      inc     IX              ; set for...
                inc     IX              ; ...next channel...
                sla     H               ; shift left H 1 bit
                djnz    CHKSNDCH        ; repeat for 3 channels
                pop     IX              ; restore IX
                ret                     ; return to caller

; read a specific row of the keyboard matrix, set by A
; return read into A
READKBLN:       push    BC
                ld      B,$0F           ; reg #15
                ld      C,PSG_REG       ; PSG register port
                out     (C),B           ; select reg #15
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; activate the row
                ld      B,$0E           ; register #14 (port B)
                ld      C,PSG_REG       ; PSG register port
                out     (C),B           ; select reg. 14 (port B)
                in      A,(C)           ; read register #14
                pop     BC
                ret

; read the keyboard matrix to look for a key pressure
KEYBOARD:       ld      C,PSG_REG       ; PSG register port
                ld      B,$07           ; set register #7...
                out     (C),B           ; ...to work with
                in      A,(C)           ; read register #7
                set     7,A             ; port A set to output
                res     6,A             ; port B set to input
                out     (C),B           ; set register #7
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; set I/O ports w/o altering the rest of the mixer
                ; check special keys (SHIFT/ALT/CTRL)
                ld      A,%11111101     ; select SHIFT row
                call    READKBLN        ; read row
                bit     3,A             ; test if SHIFT key is pressed (4th bit is reset)
                jr      NZ,CHECKALT     ; no, so go on
                ld      HL,CONTROLKEYS  ; control key flags
                ld      (HL),%00000001  ; set SHIFT flag, reset CTRL & ALT flags (currently multiply control keys are NOT supported)
CHECKALT:       ld      A,%11111110     ; select ALT row
                call    READKBLN        ; read ALT row
                bit     5,A             ; test if ALT key is pressed (5th bit is reset)
                jr      NZ,CHECKCTRL    ; no, so go on
                ld      HL,CONTROLKEYS  ; control key flags
                ld      (HL),%00000100  ; set ALT flag, reset SHIFT & CTRL flag (currently multiply control keys are NOT supported)
CHECKCTRL:      ld      A,%11111110     ; select CTRL row
                call    READKBLN        ; read CTRL row
                bit     2,A             ; test if CTRL key is pressed (3rd bit is reset)
                jr      NZ,CHECKKBD     ; no, so make a normal reading
                ld      HL,CONTROLKEYS  ; control key flags
                ld      (HL),%00000010  ; set CTRL flag, reset SHIFT & ALT flags (currently multiply control keys are NOT supported)
CHECKKBD:       ld      B,$08           ; 8 lines
                ld      A,%01111111     ; start from the last line of the matrix
RPTKBDRD:       ld      D,$0F           ; register #15 (port B)
                ld      C,PSG_REG       ; PSG register port
                out     (C),D           ; select reg. #15
                ld      C,PSG_DAT       ; PSG data port
                out     (C),A           ; activate 1 line (active line is grounded, i.e. with a LOW signal)
                ld      E,A             ; save current line into E
                ld      D,$0E           ; register #14 (port A)
                ld      C,PSG_REG       ; PSG register port
                out     (C),D           ; select reg. 14 (port A)
                nop
                in      A,(C)           ; read register #14
                cp      $FF             ; is there any line set to 0?
                jr      Z,NOKEYPRSD     ; no, go to the next row
                ; check control keys
                ld      (KBTMP),A       ; yes, check if a control key was pressed. First, store current row
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
                ld      (CONTROLKEYS),A ; ...reset contro key flags...
                ld      (KBDNPT),A      ; ...no input from keyboard...
                ret                     ; ...and leave
FINDKEY:        ld      E,$FF           ; counter
CHKLN:          inc     E               ; E goes from 0 to 7
                srl     A               ; is the first bit reset? (we're looking for a "0", meaning grounded line)
                jr      C,CHKLN         ; no, check next bit
                ld      A,(CONTROLKEYS) ; load control key flags
                ld      HL,KBMAP        ; normal keymap
                cp      $01             ; SHIFT flag?
                jr      NZ,CHKCTRL      ; no, jump over
                ld      HL,KBMAP_SFT    ; SHIFT keymap
                jr      LOADMAP         ; and load it
CHKCTRL:        cp      $02             ; CTRL flag?
                jr      NZ,CHKALT       ; no, jump over
                ld      HL,KBMAP_CTRL   ; CTRL map
                jr      LOADMAP         ; and load it
CHKALT:         cp      $04             ; ALT flag?
                jr      NZ,LOADMAP      ; no, check over
                ld      HL,KBMAP_ALT    ; ALT map
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
                ld      (TMPKEYBFR),A   ; ...insert it into the INKEY buffer...
                ld      (CHR4VID),A     ; ...and store char for video
                cp      CTRLC           ; is it RUN/STOP?
                jr      NZ,CNTKBCK      ; no, jump over
                call    CHARINTOBFR     ; yes, send directly to buffer and...
                jr      LVKBRDCHK2      ; ...leave
CNTKBCK:        ld      BC,$0800        ; 8 FN keys (B), FN key number (C)
                ld      HL,FNKEYSORD    ; FN keys codes
CHKFNK:         cp      (HL)            ; is it an FN key?
                jp      Z,PRNTFNKEY     ; yes, jump over
                inc     C               ; next FN key
                inc     HL              ; next FN key code
                djnz    CHKFNK          ; continue for 8 FN keys
SNDKEYTOBFR:    ld      A,$01           ; normal key - set input flag
                ld      (KBDNPT),A      ; to keyboard
                ld      A,(PRNTVIDEO)   ; load status of print-on-video
                or      A               ; is the print-on-video disabled?
                jp      Z,PUTCHRBUF     ; yes, so send char to input buffer
                ld      A,(CRSR_STATE)  ; check cursor state
                or      A               ; is it 0 (cursor OFF)?
                jr      NZ,PNT2VD       ; no, print on screen
PUTCHRBUF:      xor     A
                ld      (KBDNPT),A      ; if send to input buffer, set RETURN as from BASIC
                ld      A,(TMPKEYBFR)   ; retrieve char
                call    CHARINTOBFR     ; cursor off, so send char to buffer...
                jp      LVKBRDCHK2      ; ...and leave
PNT2VD:         call    CHAR2VID        ; send char to video
LVKBRDCHK2:     xor     A
                ld      (CONTROLKEYS),A ; reset control key flags
LVKBRDCHK:      ret                     ; return to caller: the current key code is into TMPKEYBFR    
                ; manage FN keys          
PRNTFNKEY:      ld      D,A             ; copy A into D
                ld      HL,(LINEAT)     ; Get current line number
                inc     HL              ; -1 means direct statement
                ld      A,H
                or      L
                ld      A,D             ; retrieve char
                jr      NZ,SNDKEYTOBFR  ; indirect mode - just send FN key code to buffer
                ld      A,C             ; direct mode, so print text - first, get FN key number
                add     A,A
                add     A,A
                add     A,A
                add     A,A             ; FN key number * 16
                ld      C,A             ; move it into C
                ld      B,$00           ; reset B, to get offset
                ld      HL,FNKEYS       ; load address of FN keys texts
                add     HL,BC           ; get correct text address
                ld      B,$10           ; 16 chars
LDFNKEYCHR:     ld      A,(HL)          ; load char
                and     A               ; null char?
                jp      Z,LVKBRDCHK2    ; yes, so leave
                ld      D,A             ; pass char into D
                ld      A,(PRNTVIDEO)   ; load status of print-on-video
                or      A               ; is the print-on-video disabled?
                jp      Z,PUTCHRBUF1    ; yes, so send char to input buffer
                ld      A,(CRSR_STATE)  ; check cursor state
                or      A               ; is it 0 (cursor OFF)?
                call    NZ,PRNTFNK      ; no, print on screen
CNTFNK:         inc     HL              ; next char
                djnz    LDFNKEYCHR      ; repeat for max. 16 chars
                jp      LVKBRDCHK2      ; leave
PUTCHRBUF1:     xor     A               ; if send to input buffer,... 
                ld      (KBDNPT),A      ; ...set input as from BASIC
                ld      A,D             ; retrieve char
                push    HL              ; store HL
                call    CHARINTOBFR     ; cursor off, so send char to buffer...
                pop     HL              ; retrieve HL
                jp      CNTFNK          ; jump over
PRNTFNK:        ld      A,D             ; recover char
                ld      (CHR4VID),A     ; store char for printing
                ld      A,$01           ; normal key - set input flag
                ld      (KBDNPT),A      ; to keyboard
                call    CHAR2VID        ; print on screen
                ret                     ; return to caller


;-----------------------------------------------------------------------
FNKEYSORD:      defb 1,2,4,5,6,22,23,24                 ; order of FN Keys
;-----------------------------------------------------------------------
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
