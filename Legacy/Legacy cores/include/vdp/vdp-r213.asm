; ------------------------------------------------------------------------------
; LM80C - VDP ROUTINES - R2.13
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
; R1.0   - 20190511 - First version
; R1.0   - 20190512
; R1.2   - 20190515
; R1.3   - 20190521 - Video cursor management - preliminary
; R1.4   - 20190524 - Added scrolling capabilities
; R1.5   - 20190524 - Added backspace functionality
; R1.6   - 20190601 - Fixed scrolling bugs
; R1.7   - 20190606 - Show the computer name only at powerup
; R1.8   - 20190615 - Better cursor integration; added VPOKE & VPEEK statements; 6x8 & 8x8 fonts
; R1.9   - 20190620 - Added functions to read/write VDP registers
; R2.0   - 20190714 - Added SREG & SSTAT to write to/read from PSG
; R2.0   - 20190818 - Added SOUND command to play simple tones and VOLUME command
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
; R2.8.0 - 20191208 - Introduced support for SHIFT key for uppercase letters & alternate chars
; R2.8.2 - 20191215 - Fixed a bug introduced with 2.8.0 that lead to wrong functioning of
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
;
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; VDP INITIALISATION
; initialize VDP for a specific graphics mode
; INPUT:
; E contains the graphics mode:
; 0=text; 0=graphics 0; 2=graphics 2; 3=multicolor; 4=extended graphics 2
initVDP:        push    DE              ; store E
                call    EMPTY_VRAM      ; reset VRAM
                call    SET_GFX_MODE    ; load register settings
                call    EMPTY_RAM       ; reset RAM registers
                pop     DE              ; restore reg. E
                xor     A               ; reset A
                ld      B,A             ; reset B (will be used later)
                ld      A,E             ; move E into A
                ld      (SCR_MODE),A    ; storescreen mode
                cp      $01             ; is it graphics 0?
                jp      Z,G1MD          ; yes, jump over
                cp      $02             ; is it graphics 2?
                jp      Z,G2MD          ; yes, jump over
                cp      $03             ; is it multicolor?
                jp      Z,MCMD          ; yes, jump over
                cp      $04             ; is it extended graphics 2?
                jp      Z,EXG2MD        ; yes, jump over; otherwise, it must be $00 so we assume that it's text mode

                ; LOAD VDP SETTINGS FOR SELECTED VIDEO MODE:
                ; TEXT MODE (G0)
TXTMD:          ; load charset
                ld      H,B
                ld      L,B             ; HL=first pattern cell $0000
                call    LOADCHARSET     ; load patterns into VRAM
                ; set cursor & video overlay
                xor     A               ; reset A
                ld      (SCR_CURS_X),A  ; set cursor position at X=0
                ld      (SCR_CURS_Y),A  ; and Y=0
                ld      A,$28
                ld      (SCR_SIZE_W),A  ; screen width = 40 cols
                ld      A,$18
                ld      (SCR_SIZE_H),A  ; screen height = 24 rows
                ld      DE,$0800
                ld      (SCR_NAM_TB),DE ; set name table address
                ld      DE,$03C0
                ld      (SCR_SIZE_B),DE ; set screen size in chars (40x24=960)
                jp      ENDVDPSET       ; execute the rest of the video setting

                ; GRAPHICS 0 MODE (G1)
G1MD:           ; load pattern table
                ld      L,B
                ld      H,B             ; HL=first pattern cell $0000
                call    LOADCHARSET     ; load patterns into VRAM
                ; set cursor & video overlay
                xor     A               ; position cursor
                ld      (SCR_CURS_X),A  ; at X=0
                ld      (SCR_CURS_Y),A  ; and Y=0
                ld      A,$20
                ld      (SCR_SIZE_W),A  ; screen width = 32 cols
                ld      A,$18
                ld      (SCR_SIZE_H),A  ; screen height = 24 rows
                ld      DE,$1800
                ld      (SCR_NAM_TB),DE ; set name table address
                ld      DE,$0300
                ld      (SCR_SIZE_B),DE ; set screen size in chars (32x24=768)
                ; load color table
                ld      HL,$2000        ; color table start: $2000
                call    SETVDPADRS
                ld      A,$0F           ; foreground color is...
                ld      (FRGNDCLR),A    ; ...set to white
                ld      A,$F5           ; reg.A loaded with colors for chars: white pixels on light blue background
                ld      B,$20           ; 32 bytes of colors
                ld      C,VDP_DAT       ; VDP data mode
LDCLRTBMD1:     out     (C),A           ; after first byte, the VDP autoincrements VRAM pointer
                nop
                djnz    LDCLRTBMD1      ; repeat for 32 bytes
                jp      ENDVDPSET       ; execute the rest of the video setting

                ; GRAPHICS 2 MODE (G2)
G2MD:           xor     A               ; position cursor
                ld      (SCR_CURS_X),A  ; at X=0
                ld      (SCR_CURS_Y),A  ; and Y=0
                ld      (SCR_SIZE_W),A  ; screen width = 256 pixels (0=256)
                ld      A,$C0
                ld      (SCR_SIZE_H),A  ; screen height = 192 pixels
                ld      DE,$1800
                ld      (SCR_NAM_TB),DE ; set name table address
                ld      D,A
                ld      (SCR_SIZE_B),DE ; set screen size in pixels ($C000=49,152=256x192)
                jp      ENDVDPSET       ; execute the rest of the video setting

                ; MULTICOLOR MODE (G3)
MCMD:           xor     A               ; position cursor
                ld      (SCR_CURS_X),A  ; at X=0
                ld      (SCR_CURS_Y),A  ; and Y=0
                ld      A,$40
                ld      (SCR_SIZE_W),A  ; screen width = 64 blocks
                ld      A,$30
                ld      (SCR_SIZE_H),A  ; screen height = 48 blocks
                ld      DE,$0800
                ld      (SCR_NAM_TB),DE ; set name table address
                ld      D,$0C
                ld      (SCR_SIZE_B),DE ; set screen size in blocks ($0C00=3,072=64x48)
                jp      ENDVDPSET       ; execute the rest of the video setting

                ; EXTENDED GRAPHICS 2 (G4)
EXG2MD:         ; load pattern table
                ld      H,B
                ld      L,B             ; HL=first pattern cell $0000
                call    LOADCHARSET     ; load patterns into VRAM
                ; set cursor & video overlay
                xor     A               ; position cursor
                ld      (SCR_CURS_X),A  ; at X=0
                ld      (SCR_CURS_Y),A  ; and Y=0
                ld      A,$20
                ld      (SCR_SIZE_W),A  ; screen width = 32 cols
                ld      A,$18
                ld      (SCR_SIZE_H),A  ; screen height = 24 rows
                ld      DE,$3800
                ld      (SCR_NAM_TB),DE ; set name table address
                ld      D,$03
                ld      (SCR_SIZE_B),DE ; set screen size in chars ($300=768=32x24)
                 ; load color table
                ld      HL,$2000        ; color table start: $2000
                call    SETVDPADRS
                ld      A,$0F           ; foreground color is...
                ld      (FRGNDCLR),A    ; ...set to white
                ld      A,$F5           ; reg.A loaded with colors for chars: white pixels on light blue background
                ld      D,$08           ; 8 pages of 
                ld      B,$00           ; 256 bytes of colors (total of 2,048 cells)
                ld      C,VDP_DAT       ; VDP data mode
LDCLRTBEX2:     out     (C),A           ; after first byte, the VDP autoincrements VRAM pointer
                nop
                nop
                djnz    LDCLRTBEX2      ; repeat for 256 bytes
                dec     D               ; did we fill up all the cells?
                jr      NZ,LDCLRTBEX2   ; no, repeat
                ; LAST VDP SETTINGS
ENDVDPSET:      call    CURSOR_OFF      ; disable cursor
                call    EMPTYVIDBUF     ; empty video buffer
                ld      (SCR_ORG_CHR),A ; A containts the byte used to empty the video buffer
EXITVDWLCM:     ret                     ; return to caller


; show logo/message, play a beep and wait a while
DELAY_VDP:      ld      B,$02
                xor     A
                ld      D,A             ; 256 times
                ld      E,A             ; x 256 times
                ld      (TMPBFR1),A     ; sound flag set to 0
DEC_D:          nop                     ; does nothing...
                nop
                nop
                nop
                nop
                nop
                dec     E               ; decrement E
                jr      NZ,DEC_D        ; repeat until $00
                ld      A,D
                cp      $40             ; ...equal to 64
                call    Z,SETBEEP       ; if yes, start sound
                dec     D
                jr      NZ,DEC_D        ; repeat
                ld      A,(TMPBFR1)
                cp      $02
                call    NZ,BEEPOFF
                djnz    DEC_D
                jp      EMPTYVIDBUF     ; empty video buffer & exit

SETBEEP:        ld      A,(TMPBFR1)
                or      A               ; is it 0?
                ret     NZ              ; no, exit
                inc     A
                ld      (TMPBFR1),A     ; sound flag set to 0
                call    WLCMBEEP
                ret

BEEPOFF:        ld      A,$02
                ld      (TMPBFR1),A
                call    NOBEEP
                ret


; show initial logo
SHOW_LOGO:      call    EMPTY_VRAM      ; reset VRAM
                ; set VDP for G2 mode
                ld      B,$07           ; set only the first 7 registers
                ld      DE,$0010        ; load settings for G2 mode
                call    SET_GFX_MODE2   ; load register settings
                ld      D,$01           ; backdrop color set to black
                out     (C),D           ; send data to VDP
                out     (C),A           ; indicate the register to send data to
                ; set name table 
                call    SETG2NAMTBL
                ; set color table
                ld      A,$11           ; foreground and background set to black
                ld      D,$18           ; 24 pages
                ld      B,$00           ; 256 color cells
                ld      HL,$2000        ; first cell of color table
                call    SETVDPADRS      ; send address
RPT100:         out     (C),A
                nop
                nop
                djnz    RPT100
                dec     D
                jr      NZ,RPT100       ; for $1800 cells
                ; set colors for logo
                ld      HL,$2800        ; 2nd page of color table
                call    SETVDPADRS
                ld      B,5             ; 5 bands
                ld      HL,CLRTABLE
                ld      C,VDP_DAT
                ld      E,$08           ; 8 pixels each pattern        
RPT101:         ld      D,$40           ; 64 chars each band
                ld      A,(HL)
RPT102:         out     (C),A
                dec     E
                jr      NZ,RPT102
                ld      E,$08
                dec     D
                jr      NZ,RPT102
                inc     HL
                djnz    RPT101
                ; set pattern table
                ld      HL,$0800        ; address of first cell of 2nd area of pattern table
                ld      DE,LM80CLOGO    ; pointer to logo pattern
                ld      B,$00           ; 256 bytes, 8 rows
RPT103:         call    LOADLOGOCHRS
                inc     DE              ; next logo pattern
                djnz    RPT103
                ld      B,$20           ; repeat for another 2 rows
RPT104:         call    LOADLOGOCHRS
                inc     DE              ; next logo pattern
                djnz    RPT104
                jp      DELAY_VDP       ; continue

LOADLOGOCHRS:   ld      A,(DE)          ; load a pattern char of the logo
                add     A,A
                add     A,A
                add     A,A             ; multiply times 8 to get the offset
                push    BC
                push    DE              ; store BC and DE
                push    HL              ; store VRAM address to write to
                ld      HL,LOGOFONT     ; start of logo font data
                ld      E,A
                ld      D,$00           ; put offset (A) into DE
                add     HL,DE           ; get address of pattern data
                ex      DE,HL           ; move address into DE
                pop     HL              ; retrieve VRAM address
                ld      B,$08           ; 8 bytes per pattern
                call    SETVDPADRS      ; set VDP address
                ld      C,VDP_DAT
SNDLOGPT:       ld      A,(DE)          ; load data from RAM
                out     (C),A           ; and send to VRAM
                inc     DE              ; next byte into RAM
                inc     HL              ; next byte into VRAM (used in future iterations)
                djnz    SNDLOGPT        ; repeat 8 times
                pop     DE
                pop     BC              ; retrieve BC & DE
                ret                     ; return to caller

CLRTABLE:       equ $
                defb    $18,$1B,$13,$14,$1D     ; colors of background bands of the logo
                
; empty video buffer
EMPTYVIDBUF:    ld      C,$20           ; we will fill up the screen with spaces
                ld      A,(SCR_SIZE_H)  ; load the height of screen
                cp      $30             ; is it in graphics 2 or 3 modes (height >=48)?
                jr      C,SETGRPVOID    ; no (height <48, we are in text or G1/EXG2 modes), so jump over
                ld      C,$00           ; yes (height>=48, we are in G2 or MC modes), we change the filling byte: it will be a space
SETGRPVOID:     ld      B,A             ; move rows into B
                ld      A,C             ; move filling char into A
                ld      HL,(SCR_NAM_TB) ; load the name table address
                call    SETVDPADRS      ; send address to VDP
                ld      C,VDP_DAT       ; VDP address for passing data
LDCOLSTOEMPTY:  ld      E,A             ; store filling char into E
                ld      A,(SCR_SIZE_W)  ; load # of cols to empty into A
                ld      D,A             ; move A into D
                ld      A,E             ; recover filling char
RPTEMPTYBUF:    out     (C),A           ; write empty byte into VRAM
                dec     D               ; decr. D
                jr      NZ,RPTEMPTYBUF  ; repeat for the # of cols
                djnz    LDCOLSTOEMPTY   ; repeat for the # of rows
                ld      A,(SCR_MODE)    ; check mode
                cp      $02             ; is it graphics 2 mode?
                ret     NZ              ; no, return to caller
                ; GRAPHICS 2 needs additional work: sets the graphics page for an empty white screen
EMPTYG2:        call    SETG2NAMTBL     ; set name table
                ld      A,$01           ; set foreground color
                ld      (FRGNDCLR),A    ; to BLACK
                ld      HL,$2000        ; load the color table address
                call    SETVDPADRS      ; send address to VDP
                ld      C,VDP_DAT       ; VDP address for passing data
                ld      A,$1F           ; set pixel OFF to Black and pixel ON to White
                ld      D,$18           ; 6144 ($1800) cells to fill, so 24 pages ($18) of...
                ld      B,$00           ; ...256 bytes each ($00=256)
SNDCLRSET:      out     (C),A           ; send color setting
                nop                     ; wait a while
                djnz    SNDCLRSET       ; repeat for 0 page
                dec     D               ; have we filled all the pages?
                jr      NZ,SNDCLRSET    ; no, repeat
                ret                     ; return to caller

; set name table for G2 mode (patterns from $00 to $FF for each of the 3 areas of the screen)
SETG2NAMTBL:    ld      HL,$1800        ; name table
                call    SETVDPADRS      ; send address to VDP
                ld      C,VDP_DAT       ; VDP address for passing data
                ld      D,$03           ; 3 pages to fill into VRAM (768 cells)
                xor     A               ; starting char name #0 (chars go from 0 to 255)
                ld      B,A             ; reset B
RPTFLL1:        out     (C),A           ; send name to VRAM
                inc     A               ; increment # of name
                djnz    RPTFLL1         ; repeat for 256 cells (0 page)
                dec     D               ; did we fill all the pages?
                jr      NZ,RPTFLL1      ; no, continue
                ret                     ; return to caller

SETVDPADRS:     ld      C,VDP_SET       ; VDP address mode
                set     $06,H           ; set bit #6 of address, to write to VRAM
                out     (C),L           ; send low and...
                out     (C),H           ; ...high byte of the first cell
                ret                     ; return to caller

; clear the video buffer and position the cursor at 0,0
CLEARVIDBUF:    call    EMPTYVIDBUF     ; clear video buffer
                xor     A               ; reset A
                ld      (SCR_CURS_X),A  ; cursor X to 0
                ld      (SCR_CURS_Y),A  ; cursor Y to 0
                call    POS_CURSOR      ; position cursor
                ret                     ; return to caller

; load the char or byte at the VRAM position set by HL
; value is returned into A
READ_VIDEO_LOC: push    BC              ; store BC
                ld      C,VDP_SET       ; VDP setting mode
                out     (C),L           ; low byte then...
                out     (C),H           ; high byte
                ld      C,VDP_DAT       ; VDP data mode
                nop                     ; wait...
                nop                     ; ...a while
                in      A,(C)           ; read byte at current VRAM location
                pop     BC              ; restore BC
                ret                     ; return to caller

; write a byte at the VRAM pointed by HL
; value is in A
WRITE_VIDEO_LOC:push    BC              ; store BC
                push    HL              ; store HL (because this function alters it)
                ld      C,VDP_SET       ; VDP setting mode
                set     $06,H           ; write to VRAM
                out     (C),L           ; low byte then...
                out     (C),H           ; high byte of VRAM address
                ld      C,VDP_DAT       ; VDP data mode
                out     (C),A           ; write byte into VRAM
                pop     HL              ; retrieve HL
                pop     BC              ; restore BC
                ret                     ; return to caller

; write a value into a specific VDP register
; value is in E, register is in A
WRITE_VREG:     push    BC              ; store BC
                add     A,$80           ; set VDP to write to registers
                ld      C,VDP_SET       ; VDP setting mode
                out     (C),E           ; send data to VDP
                out     (C),A           ; select the destination register
                pop     BC              ; restore BC
                ret                     ; return to caller

; read VDP status register and return value into A
READ_VSTAT:     push    BC              ; store BC
                ld      C,VDP_SET       ; VDP register access
                in      A,(C)           ; read status register
                pop     BC              ; restore BC
                ret                     ; return to caller

; position the cursor at the current coordinates, preserving underlying char
POS_CURSOR:     call    LOAD_CRSR_POS   ; load the VRAM address of cursor into HL
                call    READ_VIDEO_LOC  ; load the current char at the cursor position (return in A)
                ld      (SCR_ORG_CHR),A ; store the current char
                ret

; move cursor to new X,Y coordinates
MOVCRS:         call    RSTCHRCRS       ; restore the char in the current cursor position and print it in its old location
                ld      A,(SCR_CUR_NX)  ; load new X
                ld      (SCR_CURS_X),A  ; write new X
                ld      A,(SCR_CUR_NY)  ; load new Y
                ld      (SCR_CURS_Y),A  ; write new Y
                ld      A,$ff           ; delete new values
                ld      (SCR_CUR_NX),A  ; of X
                ld      (SCR_CUR_NY),A  ; and Y
                call    POS_CURSOR      ; position cursor into new location
                ret                     ; return to caller

; recover char under the cursor and prints it onto the screen
RSTCHRCRS:      call    LOAD_CRSR_POS   ; recover old cursor position
                ld      A,(SCR_ORG_CHR) ; recover old char
                set     $06,H           ; write-to-VRAM mode
                ld      C,VDP_SET       ; VDP setting mode
                out     (C),L           ; pass low and
                out     (C),H           ; byte byte of address
                ld      C,VDP_DAT       ; VDP data mode
                out     (C),A           ; write old char into current cursor position
                ret

; retrieve cursor position from either current coordinates or next place
; return address position into HL
LOAD_CRSR_POS:  ld      A,(SCR_CURS_Y)  ; load cursor Y
                ld      L,A             ; move it into reg.L
                xor     A               ; reset A
                ld      H,A             ; reset H
                ld      B,A             ; reset B
                add     HL,HL           ; create offset (each address is 2-bytes long so we need to double HL)
                ld      DE,POS_TB_CRS_40; load position table address of cursor for 40 cols
                ld      A,(SCR_SIZE_W)  ; load screen width
                cp      $28             ; is it 40 cols?
                jr      Z,CONT_POS_CURS ; yes, jump over
                ld      DE,POS_TB_CRS_32; no, load position table address of cursor for 32 cols
CONT_POS_CURS:  add     HL,DE           ; the correct starting address of the required row is now into HL
                ld      DE,(HL)         ; load starting address of the required row into DE
                ld      HL,(SCR_NAM_TB) ; load starting address of name table
                add     HL,DE           ; starting address of the current row into name table
                ld      A,(SCR_CURS_X)  ; load cursor X
                ld      C,A             ; transfer A into C
                add     HL,BC           ; add X offset: now HL contains the address of the current cursor position
                ret

;-------------------------------------------------------------------------------
; send current char to video buffer
CHAR2VID:       push    AF              ; store AF
                push    BC              ; store BC
                push    DE              ; store DE
                push    HL              ; store HL
                ld      A,(CRSR_STATE)  ; store cursor state...
                push    AF              ; into stack
                call    CURSOR_OFF      ; cursor off
                ld      A,(CHR4VID)     ; recover char
                and     A               ; is it char 0? (null char)?
                jr      Z,EXITCHAR2VID  ; yes, exit now
                cp      CS              ; is it the CLEAR char ($0C)?
                jr      NZ,CHKCR        ; no, check over
                call    CLEARVIDBUF     ; yes, clear video buffer and position cursor at 0,0
                jr      EXITCHAR2VID    ; exit
CHKCR:          cp      CR              ; is it a carriage return ($0D)?
                jr      NZ,CHKBKSP      ; no, jump over
                call    CRGRETURN       ; yes, go to the beginning of the next line
                jr      EXITCHAR2VID    ; exit
CHKBKSP:        cp      BKSP            ; is it the backspace ($08)?
                jr      NZ,CHKCRSLFT    ; no, jump over
                call    BACKSPACE       ; move cursor left 0 position
                jr      EXITCHAR2VID    ; exit
CHKCRSLFT:      cp      CRSLFT          ; is it cursor left?
                jr      NZ,CHKCRSUP     ; no, jump over
                call    CURSORLEFT      ; move cursor left...
                jr      EXITCHAR2VID    ; ...and exit
CHKCRSUP:       cp      CRSUP           ; is it cursor up?
                jr      NZ,CHKCRSRGT    ; no, jump over
                call    CURSORUP        ; move cursor up...
                jr      EXITCHAR2VID    ; ...and exit
CHKCRSRGT:      cp      CRSRGT          ; is it cursor right?
                jr      NZ,CHKCRSDWN    ; no, jump over
                call    CURSORRIGHT     ; move cursor right...
                jr      EXITCHAR2VID    ; ...and exit
CHKCRSDWN:      cp      CRSDN           ; is it cursor down?
                jr      NZ,CHKLF        ; no, jump over
                call    CURSORDOWN      ; move cursor up...
                jr      EXITCHAR2VID    ; ...and exit
CHKLF:          cp      LF              ; is it a line feed ($0A)?
                jr      Z,EXITCHAR2VID  ; CURRENTLY WE DON'T PRINT LF, WE JUST USE CR TO SUBSTITUTE CR+LF COMBINATION
PRNTCHAR:       call    LOAD_CRSR_POS   ; recover position of cursor
                ld      A,(CHR4VID)     ; recover char to print
                call    WRITE_VIDEO_LOC ; write A into VRAM at (HL)
                ld      A,(SCR_CURS_Y)  ; load cursor Y into A
                ld      E,A             ; store cursor Y into E
                ld      A,(SCR_CURS_X)  ; load cursor X
                inc     A               ; move 0 step to right
                ld      HL,SCR_SIZE_W   ; cell that keeps the width of screen
                cp      (HL)            ; have we reached the most right position?
                jr      NZ,SETCSRCOORDS ; no, go over
                inc     E               ; yes, increment cursor Y (go to next line)
                ld      A,E             ; move cursor Y into A
                ld      HL,SCR_SIZE_H   ; cell that keeps the height of screen
                cp      (HL)            ; have we reached the bottom of the screen?
                jr      NZ,SETCRSRY     ; no, jump over
                push    DE
                call    SCROLLUP        ; scroll screen up
                pop     DE
                dec     E               ; decrement 0 row, to set cursor Y on the last line
SETCRSRY:       xor     A               ; then set cursor X to 0 (go to beginning of line)
SETCSRCOORDS:   ld      (SCR_CURS_X),A  ; store current cursor X
                ld      A,E             ; recover Y
                ld      (SCR_CURS_Y),A  ; store current cursor Y
                call    POS_CURSOR      ; position cursor to new location
EXITCHAR2VID:   xor     A               ; reset char
                ld      (CHR4VID),A     ; to be sent to screen
                pop     AF              ; recover cursor state
                and     A               ; was it off (A=0)?
                jr      Z,FINISHCHR2VD  ; yes, so jump over
                call    CURSOR_ON       ; no, reset cursor on
FINISHCHR2VD:   pop     HL              ; restore HL
                pop     DE              ; restore DE
                pop     BC              ; restore BC
                pop     AF              ; restore AF
                ret                     ; return to caller

; flash the cursor at the current position
; (this sub-routine is called by CH3 timer ISR)
FLASHCURSOR:    ld      A,(TMRCNT)      ; load the first byte of the 100ths of A second's counter
                and     $20             ; check if it's time to flash the cursor (check bit #6)
                ld      HL,LSTCSRSTA    ; load address of cell that stores the last cursor state
                ld      B,(HL)          ; load last state
                cp      B               ; compare current state with last state
                ret     Z               ; same state, no change required - exit
                ld      (HL),A          ; save new state
                push    AF              ; store A (keep state for later use)
                call    LOAD_CRSR_POS   ; load current cursor position into HL
                pop     AF              ; recover current state
                ld      B,$ff           ; cursor char
                cp      $20             ; is the cursor on video (A == $40)?
                jr      Z,PUTCRSCHR     ; yes, jump over
                ld      A,(SCR_ORG_CHR) ; no, load the original char
                ld      B,A             ; move char into B
PUTCRSCHR:      ld      A,B             ; recover char from B
                set     $06,H           ; set bit #6 for VRAM writing (this is similar to adding $40)
                ld      C,VDP_SET       ; VDP setting mode
                out     (C),L           ; pass the low byte
                out     (C),H           ; then the high byte of the address
                ld      C,VDP_DAT       ; VDP data mode
                out     (C),A           ; print cursor/char at the current position
                ret                     ; return to caller


; delete char immediately to the left of the cursor's position
BACKSPACE:      push    AF              ; store A
                ld      A,(SCR_CURS_X)  ; load cursor X into A
                and     A               ; is it at the most left of the screen (X=0)?
                jr      Z,EXITBACKSPACE ; yes, exit
                call    RSTCHRCRS       ; restore char under the cursor and print it
                ld      A,(SCR_CURS_X)  ; (re)load cursor X into A
                dec     A               ; decrement X
                ld      (SCR_CUR_NX),A  ; store new X
                ld      (SCR_CURS_X),A  ; prepare to delete the prev. char
                call    LOAD_CRSR_POS   ; recover position of cursor
                ld      A,$20           ; blank space
                call    WRITE_VIDEO_LOC ; write a blank space(ASCII 32) into the current cursor position
                ld      A,(SCR_CURS_Y)  ; load current cursor Y
                ld      (SCR_CUR_NY),A  ; no move over Y axis
                call    MOVCRS          ; move cursor into new position
EXITBACKSPACE:  pop     AF              ; restore A
                ret                     ; return to caller


; move cursor left
CURSORLEFT:     push    AF              ; store A
                ld      A,(SCR_CURS_X)  ; load cursor X into A
                and     A               ; is it at the most left of the screen (X=0)?
                jr      Z,CHCKYPOS      ; yes, check Y position
                dec     A               ; no, decrement X
                ld      (SCR_CUR_NX),A  ; store new X
                ld      A,(SCR_CURS_Y)  ; load current cursor Y
                ld      (SCR_CUR_NY),A  ; no move over Y axis
                jr      CONTCRSLFT      ; go on moving cursor
CHCKYPOS:       ld      A,(SCR_CURS_Y)  ; load cursor Y
                and     A               ; is it at the most top of the screen (Y=0)?
                jr      Z,EXITCURSORLEFT; yes, exit doing nothing
                dec     A               ; no, decrement Y
                ld      (SCR_CUR_NY),A  ; store new Y
                ld      A,(SCR_SIZE_W)  ; load current screen width
                dec     A               ; cursor to the most right position (width-0)
                ld      (SCR_CUR_NX),A  ; set new cursor X
CONTCRSLFT:     call    RSTCHRCRS       ; restore char under the cursor and print it
                call    MOVCRS          ; move cursor into new position
EXITCURSORLEFT: pop     AF              ; restore A
                ret                     ; return to caller


; move cursor up
CURSORUP:       push    AF              ; store A
                ld      A,(SCR_CURS_Y)  ; load cursor Y into A
                and     A               ; is it at the most top of the screen (Y=0)?
                jr      Z,EXITCURSORUP  ; yes, exit doing nothing
                dec     A               ; no, decrement Y
                ld      (SCR_CUR_NY),A  ; store new Y
                ld      A,(SCR_CURS_X)  ; load current cursor X
                ld      (SCR_CUR_NX),A  ; set new cursor X
                call    RSTCHRCRS       ; restore char under the cursor and print it
                call    MOVCRS          ; move cursor into new position
EXITCURSORUP:   pop     AF              ; restore A
                ret                     ; return to caller


; move cursor right
CURSORRIGHT:    push    AF              ; store A
                push    BC              ; store B
                ld      A,(SCR_SIZE_W)  ; load current screen width (in text modes it's 32 or 40)
                dec     A               ; decrement it (most right can only be 31 or 39)
                ld      B,A             ; move A into B
                ld      A,(SCR_CURS_X)  ; load cursor X into A    
                cp      B               ; is cursor at the most right position on the screen?
                jr      NC,CHCKYPOS2    ; yes, so jump to check Y position
                inc     A               ; no, so increment X
                ld      (SCR_CUR_NX),A  ; store new X
                ld      A,(SCR_CURS_Y)  ; load current cursor Y
                ld      (SCR_CUR_NY),A  ; no move over Y axis
                jr      CONTCRSRGT      ; go on moving cursor
CHCKYPOS2:      ld      A,(SCR_SIZE_H)  ; load screen height
                dec     A               ; decrement it (last row can only be 23)
                ld      B,A             ; move bottom into B
                ld      A,(SCR_CURS_Y)  ; load cursor Y into A
                cp      B               ; is the cursor at the bottom of the screen?
                jr      NC,EXITCURSORRGHT; yes, exit doing nothing
                inc     A               ; no, increment Y
                ld      (SCR_CUR_NY),A  ; store new Y
                xor     A               ; move cursor to top left
                ld      (SCR_CUR_NX),A  ; store new X
CONTCRSRGT:     call    RSTCHRCRS       ; restore char under the cursor and print it
                call    MOVCRS          ; move cursor into new position
EXITCURSORRGHT: pop     BC              ; retrieve BC
                pop     AF              ; restore A
                ret                     ; return to caller


; move cursor down
CURSORDOWN:     push    AF              ; store A
                push    BC              ; store B
                ld      A,(SCR_SIZE_H)  ; load current screen height (in text modes it's 24)
                dec     A               ; decrement it (positions can only vary between 0 and 23)
                ld      B,A             ; move X into B
                ld      A,(SCR_CURS_Y)  ; load cursor Y into A             
                cp      B               ; is current cursor position < 23?
                jr      NC,EXITCURSORDOWN; no, exit doing nothing
                inc     A               ; yes, increment Y
                ld      (SCR_CUR_NY),A  ; store new Y
                ld      A,(SCR_CURS_X)  ; load current cursor X
                ld      (SCR_CUR_NX),A  ; set new cursor X
                call    RSTCHRCRS       ; restore char under the cursor and print it
                call    MOVCRS          ; move cursor into new position
EXITCURSORDOWN: pop     BC              ; retrieve BC
                pop     AF              ; retrieve A
                ret                     ; return to caller


; set cursor on (visible on screen)
CURSOR_ON:      push    AF              ; store AF
                ld      A,(CRSR_STATE)  ; load cursor state
                cp      $01             ; is it on?
                jr      Z,EXITCURSOR_ON ; yes, so nothing to do
                ld      A,(SCR_SIZE_H)  ; check the video mode
                cp      $30             ; graphics 2 or 3 (if value>=48)?
                jr      nc,EXITCURSOR_ON; yes, so exit (no cursor in graphics 2 or 3)
                ld      A,$01           ; cursor state ON
                ld      (CRSR_STATE),A  ; set state
EXITCURSOR_ON:  pop     AF              ; restore AF
                ret                     ; return to caller

; set cursor off (invisible on screen)
CURSOR_OFF:     push    AF              ; store AF
                xor     A               ; cursor state OFF
                ld      (CRSR_STATE),A  ; set state
                pop     AF              ; restore AF
                ret

; scroll the screen 1 row up
SCROLLUP:       xor     A
                ld      (PRNTVIDEO),A
                ld      HL,(SCR_NAM_TB) ; start address of the name table
                ld      (VIDTMP1),HL    ; store address of the destination row (1st row of the screen)
                ld      A,(SCR_SIZE_W)  ; load the screen width
                ld      E,A             ; move width into E
                ld      D,$00           ; reset D
                add     HL,DE           ; HL now contains the address of the source row (2nd row of the screen)
                ld      (VIDTMP2),HL    ; store address of source row
                ld      A,(SCR_SIZE_H)  ; load the screen height
                dec     A               ; subtract 0 from the # of rows: now, A contains the # of rows to be moved
                ld      B,A             ; move # of rows into B
SCROLLNXTRW:    ld      A,(SCR_SIZE_W)  ; (re)load the screen width
                ld      E,A             ; move width into E
                ld      HL,(VIDTMP2)    ; load source address
                ld      C,VDP_SET       ; VDP setting mode
                out     (C),L           ; low byte of source
                out     (C),H           ; high byte of source
                ld      HL,VIDEOBUFF    ; load address of the first cell of the video buffer
                ld      C,VDP_DAT       ; VDP data mode
LOADNEXTCOL:    in      A,(C)           ; load char
                ld      (HL),A          ; store char
                inc     HL              ; next cell of the buffer
                dec     E               ; count the chars to be read
                jr      NZ,LOADNEXTCOL  ; repeat until we read the entire row
                ld      A,(SCR_SIZE_W)  ; reload the screen width
                ld      E,A             ; move # of rows into E
                ld      D,$00           ; reset D
                ld      HL,(VIDTMP1)    ; load address of destination row
                push    HL              ; store HL
                ld      HL,(VIDTMP2)    ; current source will be..
                ld      (VIDTMP1),HL    ; ..new destination
                add     HL,DE           ; address of new
                ld      (VIDTMP2),HL    ; source row
                pop     HL              ; restore address of current destination row
                set     $06,H           ; writing mode
                ld      C,VDP_SET       ; VDP setting mode
                out     (C),L           ; low byte
                out     (C),H           ; high byte of address
                ld      HL,VIDEOBUFF    ; video buffer address
                ld      C,VDP_DAT       ; VDP data mode
WRITEBUF:       ld      A,(HL)          ; load char
                out     (C),A           ; send char
                inc     HL              ; increment buffer index
                dec     E               ; next row
                jr      NZ,WRITEBUF     ; repeat until 0
                djnz    SCROLLNXTRW     ; repeat for the entire screen
                ld      A,(SCR_SIZE_W)  ; reload screen width
                ld      B,A             ; cells to empty into B
                ld      A,$20           ; empty char (space)
                ld      C,VDP_SET       ; VDP set mode
                ld      HL,(VIDTMP1)    ; load address of the last row
                set     $06,H           ; writing mode
                out     (C),L           ; low byte then..
                out     (C),H           ; high byte of address
                ld      C,VDP_DAT       ; VDP data mode
RPTEMPTYROW:    out     (C),A           ; empty cell
                nop                     ; delay
                djnz    RPTEMPTYROW     ; repeat until the last row has been cleaned
                ld      A,$01
                ld      (PRNTVIDEO),A   ; set print-on-video on
                ret                     ; return to caller

; new line - NOT USED
LINEFEED:       ld      A,(SCR_CURS_Y)  ; load cursor Y into A
                inc     A               ; new row
                ld      HL,SCR_SIZE_H   ; load screen height
                cp      (HL)            ; is the cursor over the bottom of the screen?
                ret     Z               ; yes, exit
                ld      (SCR_CUR_NY),A  ; no, move cursor to the new line
                ld      A,(SCR_CURS_X)  ; load cursor X
                ld      (SCR_CUR_NX),A  ; new cursor X
                call    MOVCRS          ; move cursor
                ret                     ; return to caller

; carriage return - move to the next line and position the cursor at the beginning of the row (CR+LF)
CRGRETURN:      call    RSTCHRCRS       ; recover char under the cursor
                xor     A               ; move to col 0
                ld      (SCR_CURS_X),A  ; store new X
                ld      A,(SCR_CURS_Y)  ; load cursor Y into A
                inc     A               ; new row
                ld      HL,SCR_SIZE_H   ; load screen height
                cp      (HL)            ; is the cursor over the bottom of the screen?
                jr      NZ,ADDNEWLINE   ; no, store new Y
                dec     A               ; yes, so come back 0 row, then
                push    AF
                call    SCROLLUP        ; scroll screen
                pop     AF
ADDNEWLINE:     ld      (SCR_CURS_Y),A  ; store new Y
                call    POS_CURSOR      ; position cursor to new location
                ret                     ; return to caller

; ------------------------------------------------------------------------------
                ; this table contains the values of the offsets to be added to
                ; the starting address of the name table to find the correct
                ; value of the first cell of the corresponding row
                ; (by doing so, it's faster than doing a multipication)
                ; table for graphics 0 text mode: 32 cols
POS_TB_CRS_32   DEFW    $0000,$0020,$0040,$0060,$0080,$00A0,$00C0,$00E0
                DEFW    $0100,$0120,$0140,$0160,$0180,$01A0,$01C0,$01E0
                DEFW    $0200,$0220,$0240,$0260,$0280,$02A0,$02C0,$02E0
                ; table for pure text mode: 40 cols
POS_TB_CRS_40   DEFW    $0000,$0028,$0050,$0078,$00A0,$00C8,$00F0,$0118
                DEFW    $0140,$0168,$0190,$01B8,$01E0,$0208,$0230,$0258
                DEFW    $0280,$02A8,$02D0,$02F8,$0320,$0348,$0370,$0398

; ------------------------------------------------------------------------------
; reset VRAM
EMPTY_VRAM:     xor     A               ; reg.A cleared: we fill up VRAM with $00
                ld      H,A
                ld      L,A             ; reset HL
                call    SETVDPADRS      ; set address of first VRAM cell to $0000
                ld      B,$40           ; $40 pages of RAM...
                ld      D,A             ; ...each one with $100 cells (tot. $4000 bytes)
                ld      C,VDP_DAT       ; VDP data mode
EMPTVRM:        out     (C),A           ; after first byte, the VDP autoincrements VRAM pointer
                inc     D               ; next cell
                jr      NZ,EMPTVRM      ; repeat until page is fully cleared
                djnz    EMPTVRM         ; repeat for $40 pages
                ret                     ; return to caller

; empty video registers in SRAM
EMPTY_RAM:      ld      HL,SCR_SIZE_W   ; address of first register
                xor     A               ; $00 to clean the registers
                ld      B,CHASNDDTN-SCR_SIZE_W; how many bytes to clean (this is calculated dinamically
                                        ; since we can add/remove some registers)
RSTVDPRAMREG:   ld      (HL),A          ; reset register
                inc     HL              ; next register
                djnz    RSTVDPRAMREG    ; repeat
                ret                     ; return to caller

; ------------------------------------------------------------------------------
; set a specific graphics mode, passed into reg. E
SET_GFX_MODE:   ld      B,$08           ; 8 registers means 8 bytes..
                sla     E               ; multiply E by 8..
                sla     E               ; so that reg. E can point..
                sla     E               ; to the correct settings
SET_GFX_MODE2:  ld      HL,VDPMODESET   ; pointer to register settings
                add     HL,DE           ; add offset to get the correct set of values for the required mode
                ld      A,$80           ; start with REG0 ($80+register number)
                ld      C,VDP_SET       ; VDP set
LDREGVLS:       ld      D,(HL)          ; load register's value
                out     (C),D           ; send data to VDP
                out     (C),A           ; indicate the register to send data to
                inc     A               ; next register
                inc     HL              ; next value
                djnz    LDREGVLS        ; repeat for 8 registers
                ret

; ------------------------------------------------------------------------------
LOADCHARSET:    ; reg. A contains the video mode
                ; reg. HL contains address of pattern table into VRAM
                ld      B,$00           ; 0=256 char to load (complete charset)
                set     $06,H           ; add $4000 to address to indicate that we want to write into VRAM
                ld      C,VDP_SET       ; load VDP address into C
                out     (C),L           ; send low byte of address
                out     (C),H           ; send high byte
                ld      HL,CHRST68      ; starting address of 6x8 charset into ROM
                and     A               ; is it text mode (A=0)?
                jr      Z,NXTCHAR       ; yes, so jump to load chars into VRAM
                ld      HL,CHRST88      ; no, so we change and load the 8x8 charset
NXTCHAR:        ld      D,$08           ; 8 bytes per pattern char
                ld      C,VDP_DAT       ; VDP data mode
SENDCHRPTRNS:   ld      A,(HL)          ; load byte to send to VDP
                out     (C),A           ; write byte into VRAM
                nop                     ; little delay (useless? - to verify)
                inc     HL              ; inc byte pointer
                dec     D               ; 8 bytes sents (0 char)?
                jr      NZ,SENDCHRPTRNS ; no, continue
                djnz    NXTCHAR         ; yes, decrement chars counter and continue for all the chars
                ret                     ; return to caller

;------------------------------------------------------------------------------
; NAME TABLE:       buffer video - contains the chars to be shown on video
; PATTERN TABLE:    charset - contains the chars/tiles to be loaded into the name table
; COLOR TABLE:      color settings for chars/tiles

                ; VDP register settings for a text display
VDPMODESET      defb    00000000b       ; reg.0: external video off
                defb    11010000b       ; reg.0: 16K VRAM, video on, int. off, text mode (40x24)
                defb    $02             ; reg.2: name table set to $0800 ($02x$400)
                defb    $00             ; reg.3: not used in text mode
                defb    $00             ; reg.4: pattern table set to $0000
                defb    $00             ; reg.5: not used in text mode
                defb    $00             ; reg.6: not used in text mode
                defb    $f5             ; reg.7: white text on light blue background

VDPMODESET1     ; VDP register settings for a graphics 0 mode
                defb    00000000b       ; reg.0: ext. video off
                defb    11000000b       ; reg.0: 16K Vram; video on, int off, graphics mode 0, sprite size 8x8, sprite magn. 0
                defb    $06             ; reg.2: name table address: $1800
                defb    $80             ; reg.3: color table address: $2000
                defb    $00             ; reg.4: pattern table address: $0000
                defb    $36             ; reg.5: sprite attr. table address: $1B00
                defb    $07             ; reg.6: sprite pattern table addr.: $3800
                defb    $05             ; reg.7: backdrop color (light blue)

VDPMODESET2     ; VDP register settings for a graphics 2 mode
                defb    00000010b       ; reg.0: graphics 2 mode, ext. video dis.
                defb    11000000b       ; reg.0: 16K VRAM, video on, INT off, sprite size 8x8, sprite magn. 0
                defb    $06             ; reg.2: name table addr.: $1800
                defb    $FF             ; reg.3: color table addr.: $2000
                defb    $03             ; reg.4: pattern table addr.: $0000
                defb    $36             ; reg.5: sprite attr. table addr.: $1B00
                defb    $07             ; reg.6: sprite pattern table addr.: $3800
                defb    $05             ; reg.7: backdrop color: light blue

VDPMODESETMC    ; VDP register settings for a multicolor mode
                defb    00000000b       ; reg.0: ext. video dis.
                defb    11001011b       ; reg.0: 16K VRAM, video on, INT off, multicolor mode, sprite size 8x8, sprite magn. 0
                defb    $02             ; reg.2: name table addr.: $0800
                defb    $00             ; reg.3: don't care
                defb    $00             ; reg.4: pattern table addr.: $0000
                defb    $36             ; reg.5: sprite attr. table addr.: $1B00
                defb    $07             ; reg.6: sprite pattern table addr.: $3800
                defb    $0F             ; reg.7: backdrop color (white)

VDPMODESETEX2   ; VDP register settings for a extended graphics 2 mode
                defb    00000010b       ; reg.0: graphics 2 mode, ext. video dis.
                defb    11000000b       ; reg.0: 16K VRAM, video on, INT off, sprite size 8x8, sprite magn. 0
                defb    $0E             ; reg.2: name table addr.: $3800
                defb    $9F             ; reg.3: color table addr.: $2000
                defb    $00             ; reg.4: pattern table addr.: $0000
                defb    $76             ; reg.5: sprite attr. table addr.: $3B00
                defb    $03             ; reg.6: sprite pattern table addr.: $1800
                defb    $05             ; reg.7: backdrop color: light blue

LM80CLOGO       ; patterns to compose the splash screen logo
                defb    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                defb    0,0,15,10,11,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                defb    0,0,13,23,0,12,0,0,0,1,0,0,0,20,0,0,6,5,6,21,22,7,6,21,22,7,6,1,1,7,0,0
                defb    0,0,13,0,0,12,0,0,0,1,0,0,0,1,20,6,1,5,3,5,3,5,3,5,3,5,3,5,3,5,0,0
                defb    0,0,13,0,0,12,0,0,0,1,0,0,0,1,1,1,1,5,9,20,19,8,3,5,19,5,3,5,0,0,0,0
                defb    0,0,13,0,0,12,0,0,0,1,0,0,0,1,0,8,3,5,6,21,22,7,3,1,22,5,3,5,0,0,0,0
                defb    0,0,13,0,0,12,0,0,0,1,0,0,0,1,0,0,3,5,3,5,3,5,3,5,3,5,3,5,3,5,0,0
                defb    0,0,13,0,0,12,0,0,0,1,4,4,0,1,0,0,3,5,9,20,19,8,9,20,19,8,9,1,1,8,0,0
                defb    0,0,14,18,18,17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                defb    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0