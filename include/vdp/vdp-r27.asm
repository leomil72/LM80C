; ------------------------------------------------------------------------------
; LM80C - VDP ROUTINES - R2.7
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Code Revision:
; R1.0  - 20190511 - First version
; R1.1  - 20190512
; R1.2  - 20190515
; R1.3  - 20190521 - Video cursor management - preliminary
; R1.4  - 20190524 - Added scrolling capabilities
; R1.5  - 20190524 - Added backspace functionality
; R1.6  - 20190601 - Fixed scrolling bugs
; R1.7  - 20190606 - Show the computer name only at powerup
; R1.8  - 20190615 - Better cursor integration; added VPOKE & VPEEK statements; 6x8 & 8x8 fonts
; R1.9  - 20190620 - Added functions to read/write VDP registers
; R2.0  - 20190714 - Added SREG & SSTAT to write to/read from PSG
; R2.1  - 20190818 - Added SOUND command to play simple tones and VOLUME command
; R2.1a - 20190908 - Cursor management improvements
; R2.2  - 20190920 - Fixed cursor bug within SCREEN statement; new command PAUSE
; R2.3  - 20190930 - Fixed bugs in SOUND command
; R2.4  - 20191013 - Added new graphic chars and reorganized previous ones
; R2.4a - 20191015 - More graphic chars
; R2.5  - 20191026 - Revision of init PSG code; revision of serial buffer exp. code;
;                    fixed a bug into the video buffer manager
; R2.6  - 20191102 - New function INKEY to read a key without a prompt;
;                    source code cleaning
; R2.7  - 20191116 - Fixed a bug into the INKEY code
;
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; VDP INITIALISATION
; initialize VDP for a specific graphics mode
; INPUT:
; E: graphics mode: 0=text; 1=graphics 1; 2=graphics 2; 3=multicolor
; D: show computer name: 0=no; 1=on
initVDP:        ; initialize VDP
                push    de             ; store D & E
                call    EMPTY_VRAM     ; reset VRAM
                call    SET_GFX_MODE   ; load register settings
                call    EMPTY_RAM      ; reset RAM registers
                pop     de              ; restore reg. E
                push    de             ; store D
                ld      a,e              ; move E into A
                cp      $01              ; is it graphics 1?
                jp      z,G1MD           ; yes, jump over
                cp      $02              ; is it graphics 2?
                jp      z,G2MD           ; yes, jump over
                cp      $03              ; is it multicolor?
                jp      z,MCMD           ; yes, jump over
                ; if $00 or no valid case, we assume that it's text mode

                ; LOAD VDP SETTINGS FOR SELECTED VIDEO MODE:
                ; TEXT MODE
TXTMD:          ; load charset
                ld      b,$00           ; chars to be loaded (0=256)
                ld      hl,$0000        ; first pattern cell $0000
                call    LOADCHARSET     ; load patterns into VRAM
                ; set cursor & video overlay
                xor     a               ; position cursor
                ld      (SCR_CURS_X),a  ; at X=0
                ld      (SCR_CURS_Y),a  ; and Y=0
                ld      a,$28
                ld      (SCR_SIZE_W),a  ; screen width = 40 cols
                ld      a,$18
                ld      (SCR_SIZE_H),a  ; screen height = 24 rows
                ld      de,$0800
                ld      (SCR_NAM_TB),de ; set name table address
                ld      de,$03C0
                ld      (SCR_SIZE_B),de ; set screen size in chars (40x24=960)
                jp      VIDWELCOME      ; execute the rest of the video setting

                ; GRAPHICS 1 MODE
G1MD:           ; load pattern table
                ld      b,$00           ; patterns to be loaded (0=256)
                ld      hl,$0000        ; first pattern cell $0000
                call    LOADCHARSET     ; load patterns into VRAM
                ; set cursor & video overlay
                xor     a               ; position cursor
                ld      (SCR_CURS_X),a  ; at X=0
                ld      (SCR_CURS_Y),a  ; and Y=0
                ld      a,$20
                ld      (SCR_SIZE_W),a  ; screen width = 32 cols
                ld      a,$18
                ld      (SCR_SIZE_H),a  ; screen height = 24 rows
                ld      de,$1800
                ld      (SCR_NAM_TB),de ; set name table address
                ld      de,$0300
                ld      (SCR_SIZE_B),de ; set screen size in chars (32x24=768)
                ; load color table
                ld      c,VDP_SET       ; load VPD port value
                ld      hl,$6000        ; color table start: $2000 (+$4000 bcs MSBs must be 0 & 1, resp.)
                ld      a,$f5           ; reg.A loaded with colors for chars: white pixels on light blue background
                out     (c),l           ; low byte of address to VDP
                out     (c),h           ; high byte address to VDP
                ld      b,$20           ; 32 bytes of colors
                ld      c,VDP_DAT       ; VDP data mode
LDCLRTBMD1:     out     (c),a           ; after first byte, the VDP autoincrements VRAM pointer
                nop
                djnz    LDCLRTBMD1      ; repeat for 32 bytes
                jp      VIDWELCOME      ; execute the rest of the video setting

                ; GRAPHICS 2 MODE
G2MD:           xor     a               ; position cursor
                ld      (SCR_CURS_X),a  ; at X=0
                ld      (SCR_CURS_Y),a  ; and Y=0
                ld      (SCR_SIZE_W),a  ; screen width = 256 pixels (0=256)
                ld      a,$c0
                ld      (SCR_SIZE_H),a  ; screen height = 192 pixels
                ld      de,$1800
                ld      (SCR_NAM_TB),de ; set name table address
                ld      de,$C000
                ld      (SCR_SIZE_B),de ; set screen size in pixels ($C000=49,152=256x192)
                jp      VIDWELCOME      ; execute the rest of the video setting

                ; MULTICOLOR MODE
MCMD:           xor     a               ; position cursor
                ld      (SCR_CURS_X),a  ; at X=0
                ld      (SCR_CURS_Y),a  ; and Y=0
                ld      a,$40
                ld      (SCR_SIZE_W),a  ; screen width = 64 blocks
                ld      a,$30
                ld      (SCR_SIZE_H),a  ; screen height = 48 blocks
                ld      de,$0800
                ld      (SCR_NAM_TB),de ; set name table address
                ld      de,$0C00
                ld      (SCR_SIZE_B),de ; set screen size in blocks ($0C00=3,072=64x48)
                jp      VIDWELCOME      ; execute the rest of the video setting

; LAST VDP SETTINGS
VIDWELCOME:     call    CURSOR_OFF      ; disable cursor
                call    EMPTYVIDBUF     ; empty video buffer
                ld      (SCR_ORG_CHR),a ; A containts the byte used to empty the video buffer
                pop     de              ; restore D
                ld      a,d             ; move D into A
                cp      $01             ; show the computer name?
                call    z,SHOWWLCMSG    ; yes, show welcome messagge with computer name
EXITVDWLCM:     ret                     ; return to caller

; show welcome message onto screen
SHOWWLCMSG:     ld      hl,(SCR_NAM_TB) ; starting address of name table
                ld      de,$0186        ; position at 12,6 (Y,X)
                add     hl,de           ; start printing at HL+DE
                ld      de,WLCMSG       ; load start address of welcome message
                call    WELCOMEMSG      ; print welcome message
                ld      d,$00           ; 256 times
                ld      b,$00           ; x times
DEC_D:          nop                     ; does nothing...
                nop
                nop
                nop
                nop
                nop
                dec     d               ; decrement D
                jr      nz,DEC_D        ; repeat until $00
                ld      a,b             ; check if A is...
                cp      $40             ; ...equal to 64
                call    Z,WLCMBEEP      ; if yes, start sound
                djnz    DEC_D           ; repeat
                call    NOBEEP          ; stop beep
                call    EMPTYVIDBUF     ; empty video buffer
                ret                     ; return to caller
; ------------------------------------------------------------------------------
; empty video buffer
EMPTYVIDBUF:    ld      c,$20           ; we will fill up the screen with spaces
                ld      a,(SCR_SIZE_H)  ; load the height of screen
                cp      $30             ; is it in graphics 2 or 3 modes (height >=48)?
                jr      nc,SETGRPVOID   ; no (<48), so jump over
                ld      c,$00           ; yes, we change the filling byte: it will be a $00
SETGRPVOID:     ld      b,a             ; move rows into B
                ld      a,c             ; move filling char into A
                ld      c,VDP_SET       ; VDP address for setting
                ld      hl,(SCR_NAM_TB) ; load the name table address
                set     $06,h           ; set bit #6 of address, to write to VRAM
                out     (c),l           ; low and
                out     (c),h           ; high byte of the first cell
                ld      c,VDP_DAT       ; VDP address for passing data
LDCOLSTOEMPTY:  ld      e,a             ; store filling char into E
                ld      a,(SCR_SIZE_W)  ; load # of cols to empty into A
                ld      d,a             ; move A into D
                ld      a,e             ; recover filling char
RPTEMPTYBUF:    out     (c),a           ; write empty byte into VRAM
                dec     d               ; decr. D
                jr      nz,RPTEMPTYBUF  ; repeat for the # of cols
                djnz    LDCOLSTOEMPTY   ; repeat for the # of rows
                ret                     ; return to caller

; clear the video buffer and position the cursor at 0,0
CLEARVIDBUF:    call    EMPTYVIDBUF     ; clear video buffer
                xor     a               ; reset A
                ld      (SCR_CURS_X),a  ; cursor X to 0
                ld      (SCR_CURS_Y),a  ; cursor Y to 0
                call    POS_CURSOR      ; position cursor
                ret                     ; return to caller

; load the char or byte at the VRAM position set by HL
; value is returned into A
READ_VIDEO_LOC: push    bc              ; store BC
                ld      c,VDP_SET       ; VDP setting mode
                out     (c),l           ; low byte then...
                out     (c),h           ; high byte
                ld      c,VDP_DAT       ; VDP data mode
                nop                     ; wait...
                nop                     ; ...a while
                in      a,(c)           ; read byte at current VRAM location
                pop     bc              ; restore BC
                ret                     ; return to caller

; write byte at the VRAM position set by HL
; value is in A
WRITE_VIDEO_LOC:push    bc              ; store BC
                ld      c,VDP_SET       ; VDP setting mode
                set     $06,h           ; write to VRAM
                out     (c),l           ; low byte then...
                out     (c),h           ; high byte of VRAM address
                ld      c,VDP_DAT       ; VDP data mode
                out     (c),a           ; write byte into VRAM
                pop     bc              ; restore BC
                ret                     ; return to caller

; write a value into a specific VDP register
; value is in E, register is in A
WRITE_VREG:     push    bc              ; store BC
                add     a,$80           ; set VDP to write to registers
                ld      c,VDP_SET       ; VDP setting mode
                out     (c),e           ; send data to VDP
                out     (c),a           ; select the destination register
                pop     bc              ; restore BC
                ret                     ; return to caller

; read VDP status register and return value into A
READ_VSTAT:     push    bc              ; store BC
                ld      c,VDP_SET       ; VDP register access
                in      a,(c)           ; read status register
                pop     bc              ; restore BC
                ret                     ; return to caller

; position the cursor at the current coordinates, preserving underlying char
POS_CURSOR:     call    LOAD_CRSR_POS   ; load the VRAM address of cursor into HL
                call    READ_VIDEO_LOC  ; load the current char at the cursor position (return in A)
                ld      (SCR_ORG_CHR),a ; store the current char
                ret

; move cursor to new X,Y coordinates
MOVCRS:         call    RSTCHRCRS       ; restore the char in the current cursor position and print it in its old location
                ld      a,(SCR_CUR_NX)  ; load new X
                ld      (SCR_CURS_X),a  ; write new X
                ld      a,(SCR_CUR_NY)  ; load new Y
                ld      (SCR_CURS_Y),a  ; write new Y
                ld      a,$ff           ; delete new values
                ld      (SCR_CUR_NX),a  ; of X
                ld      (SCR_CUR_NY),a  ; and Y
                call    POS_CURSOR      ; position cursor into new location
                ret                     ; return to caller

; recover char under the cursor and prints it onto the screen
RSTCHRCRS:      call    LOAD_CRSR_POS   ; recover old cursor position
                ld      a,(SCR_ORG_CHR) ; recover old char
                set     $06,h           ; write-to-VRAM mode
                ld      c,VDP_SET       ; VDP setting mode
                out     (c),l           ; pass low and
                out     (c),h           ; byte byte of address
                ld      c,VDP_DAT       ; VDP data mode
                out     (c),a           ; write old char into current cursor position
                ret

; retrieve cursor position from either current coordinates or next place
; return address position into HL
LOAD_CRSR_POS:  push    ix              ; store IX
                ld      ix,SCR_CURS_X   ; load address cell of current cursor position
                ld      l,(ix+1)        ; load cursor Y into reg.L
                ld      h,$00           ; reset H
                add     hl,hl           ; create offset (each address is 2-bytes long so we need to double HL)
                ld      a,(SCR_SIZE_W)  ; load screen width
                cp      $28             ; is it 40 cols?
                jr      z,SET40COLS     ; yes, jump over
                ld      de,POS_TB_CRS_32; load position table address of cursor for 32 cols
                jr      CONT_POS_CURS   ; jump over
SET40COLS:      ld      de,POS_TB_CRS_40; load position table address of cursor for 40 cols
CONT_POS_CURS:  add     hl,de           ; the correct starting address of the required row is now into HL
                ld      de,(hl)         ; load starting address of the required row into DE
                ld      hl,(SCR_NAM_TB) ; load starting address of name table
                add     hl,de           ; starting address of the current row into name table
                ld      a,(ix)          ; load cursor X
                ld      b,$00           ; reset B
                ld      c,a             ; transfer A into C
                add     hl,bc           ; add X offset: now HL contains the address of the current cursor position
                pop     ix              ; restore IX
                ret

;-------------------------------------------------------------------------------
; send current char to video buffer
CHAR2VID:       push    af              ; store AF
                push    bc              ; store BC
                push    de              ; store DE
                push    hl              ; store HL
                ld      a,(CRSR_STATE)  ; store cursor state...
                push    af              ; into stack
                call    CURSOR_OFF      ; cursor off
                ld      a,(CHR4VID)     ; recover char
                cp      $00             ; is it char 0? (null char)?
                jr      z,EXITCHAR2VID  ; yes, exit now
                cp      CS              ; is it the CLEAR char ($0C)?
                jr      nz,CHKCR        ; no, check over
                call    CLEARVIDBUF     ; yes, clear video buffer and position cursor at 0,0
                jr      EXITCHAR2VID    ; exit
CHKCR:          cp      CR              ; is it a carriage return ($0D)?
                jr      nz,CHKBKSP      ; no, jump over
                call    CRGRETURN       ; yes, go to the beginning of the next line
                jr      EXITCHAR2VID    ; exit
CHKBKSP:        cp      BKSP            ; is it the backspace ($08)?
                jr      nz,CHKLF        ; no, jump over
                call    CURSORLEFT      ; move cursor left 1 position
                jr      EXITCHAR2VID    ; exit
CHKLF:          cp      LF              ; is it a line feed ($0A)?
                jr      z,EXITCHAR2VID  ; CURRENTLY WE DON'T PRINT LF, WE JUST USE CR TO SUBSTITUTE CR+LF COMBINATION
PRNTCHAR:       call    LOAD_CRSR_POS   ; recover position of cursor
                ld      c,VDP_SET       ; VDP set mode
                set     $06,h           ; add $40 to high byte for writing into VRAM
                out     (c),l           ; send low byte then
                out     (c),h           ; high byte of current position
                ld      a,(CHR4VID)     ; recover char to print
                ld      c,VDP_DAT       ; VDP data mode
                out     (c),a           ; write char to the current cursor position
                ld      a,(SCR_CURS_Y)  ; load cursor Y into A
                ld      e,a             ; store cursor Y into E
                ld      a,(SCR_CURS_X)  ; load cursor X
                inc     a               ; move 1 step to right
                ld      hl,SCR_SIZE_W   ; cell that keeps the width of screen
                cp      (hl)            ; have we reached the most right position?
                jr      nz,SETCSRCOORDS ; no, go over
                inc     e               ; yes, increment cursor Y (go to next line)
                ld      a,e             ; move cursor Y into A
                ld      hl,SCR_SIZE_H   ; cell that keeps the height of screen
                cp      (hl)            ; have we reached the bottom of the screen?
                jr      nz,SETCRSRY     ; no, jump over
                push    de
                call    SCROLLUP        ; scroll screen up 1 row
                pop     de
                dec     e               ; decrement 1 row, to set cursor Y on the last line
SETCRSRY:       xor     a               ; then set cursor X to 0 (go to beginning of line)
SETCSRCOORDS:   ld      (SCR_CURS_X),a  ; store current cursor X
                ld      a,e             ; recover Y
                ld      (SCR_CURS_Y),a  ; store current cursor Y
                call    POS_CURSOR      ; position cursor to new location
EXITCHAR2VID:   xor     a               ; reset char
                ld      (CHR4VID),a     ; to be sent to screen
                pop     af              ; recover cursor state
                cp      $00             ; was it off?
                jr      z,FINISHCHR2VD  ; yes, so jump over
                call    CURSOR_ON       ; no, reset cursor on
FINISHCHR2VD:   pop     hl              ; restore HL
                pop     de              ; restore DE
                pop     bc              ; restore BC
                pop     af              ; restore AF
                ret                     ; return to caller

; flash the cursor at the current position
; (this sub-routine is called by CH3 timer ISR)
FLASHCURSOR:    ld      a,(TMRCNT)      ; load the first byte of the 100ths of a second's counter
                and     $40             ; check if it's time to flash the cursor (check bit #6)
                ld      hl,LSTCSRSTA    ; load address of cell that stores the last cursor state
                ld      b,(hl)          ; load last state
                cp      b               ; compare current state with last state
                ret     z               ; same state, no change required - exit
                ld      (hl),a          ; save new state
                push    af              ; store A (keep state for later use)
                call    LOAD_CRSR_POS   ; load current cursor position into HL
                pop     af              ; recover current state
                ld      b,$ff           ; cursor char
                cp      $40             ; is the cursor on video (A == $40)?
                jr      z,PUTCRSCHR     ; yes, jump over
                ld      a,(SCR_ORG_CHR) ; no, load the original char
                ld      b,a             ; move char into B
PUTCRSCHR:      ld      a,b             ; recover char from B
                set     $06,h           ; set bit #6 for VRAM writing (this is similar to adding $40)
                ld      c,VDP_SET       ; VDP setting mode
                out     (c),l           ; pass the low byte
                out     (c),h           ; then the high byte of the address
                ld      c,VDP_DAT       ; VDP data mode
                out     (c),a           ; print cursor/char at the current position
                ret                     ; return to caller


; move cursor left
CURSORLEFT:     push    af              ; store A
                ld      a,(SCR_CURS_X)  ; load cursor X into A
                cp      $00             ; is it at the most left of the screen?
                jr      z,EXITCURSORLEFT; yes, exit
                call    RSTCHRCRS       ; restore char under the cursor and print it
                ld      a,(SCR_CURS_X)  ; (re)load cursor X into A
                dec     a               ; decrement X
                ld      (SCR_CUR_NX),a  ; store new X
                ld      (SCR_CURS_X),a  ; prepare to delete the prev. char
                call    LOAD_CRSR_POS   ; recover position of cursor
                ld      c,VDP_SET       ; VDP set mode
                set     $06,h           ; add $40 to high byte for writing into VRAM
                out     (c),l           ; send low byte then
                out     (c),h           ; high byte of current position
                ld      a,$20           ; blank space
                ld      c,VDP_DAT       ; VDP data mode
                out     (c),a           ; write char to the current cursor position
                ld      a,(SCR_CURS_Y)  ; load current cursor Y
                ld      (SCR_CUR_NY),a  ; no move over Y axis
                call    MOVCRS          ; move cursor into new position
EXITCURSORLEFT: pop     af              ; restore A
                ret                     ; return to caller


; set cursor on (visible on screen)
CURSOR_ON:      push    af              ; store AF
                ld      a,(CRSR_STATE)  ; load cursor state
                cp      $01             ; is it on?
                jr      z,EXITCURSOR_ON ; yes, so nothing to do
                ld      a,(SCR_SIZE_H)  ; check the video mode
                cp      $30             ; graphics 2 or 3 (if value>=48)?
                jr      nc,EXITCURSOR_ON; yes, so exit (no cursor in graphics 2 or 3)
                ld      a,$01           ; cursor state ON
                ld      (CRSR_STATE),a  ; set state
EXITCURSOR_ON:  pop     af              ; restore AF
                ret                     ; return to caller

; set cursor off (invisible on screen)
CURSOR_OFF:     push    af              ; store AF
                ld      a,0             ; cursor state OFF (w/o flags change)
                ld      (CRSR_STATE),a  ; set state
                pop     af              ; restore AF
                ret

; scroll the screen 1 line up
SCROLLUP:       xor     a
                ld      (PRNTVIDEO),a
                ld      hl,(SCR_NAM_TB) ; start address of the name table
                ld      (VIDTMP1),hl    ; store address of the destination row (1st row of the screen)
                ld      a,(SCR_SIZE_W)  ; load the screen width
                ld      e,a             ; move width into E
                ld      d,$00           ; reset D
                add     hl,de           ; HL now contains the address of the source row (2nd row of the screen)
                ld      (VIDTMP2),hl    ; store address of source row
                ld      a,(SCR_SIZE_H)  ; load the screen height
                dec     a               ; subtract 1 from the # of rows: now, A contains the # of rows to be moved
                ld      b,a             ; move # of rows into B
SCROLLNXTRW:    ld      a,(SCR_SIZE_W)  ; (re)load the screen width
                ld      e,a             ; move width into E
                ld      hl,(VIDTMP2)    ; load source address
                ld      c,VDP_SET       ; VDP setting mode
                out     (c),l           ; low byte of source
                out     (c),h           ; high byte of source
                ld      hl,VIDEOBUFF    ; load address of the first cell of the video buffer
                ld      c,VDP_DAT       ; VDP data mode
LOADNEXTCOL:    in      a,(c)           ; load char
                ld      (hl),a          ; store char
                inc     hl              ; next cell of the buffer
                dec     e               ; count the chars to be read
                jr      nz,LOADNEXTCOL  ; repeat until we read the entire row
                ld      a,(SCR_SIZE_W)  ; reload the screen width
                ld      e,a             ; move # of rows into E
                ld      d,$00           ; reset D
                ld      hl,(VIDTMP1)    ; load address of destination row
                push    hl              ; store HL
                ld      hl,(VIDTMP2)    ; current source will be..
                ld      (VIDTMP1),hl    ; ..new destination
                add     hl,de           ; address of new
                ld      (VIDTMP2),hl    ; source row
                pop     hl              ; restore address of current destination row
                set     $06,h           ; writing mode
                ld      c,VDP_SET       ; VDP setting mode
                out     (c),l           ; low byte
                out     (c),h           ; high byte of address
                ld      hl,VIDEOBUFF    ; video buffer address
                ld      c,VDP_DAT       ; VDP data mode
WRITEBUF:       ld      a,(hl)          ; load char
                out     (c),a           ; send char
                inc     hl              ; increment buffer index
                dec     e               ; next row
                jr      nz,WRITEBUF     ; repeat until 0
                djnz    SCROLLNXTRW     ; repeat for the entire screen
                ld      a,(SCR_SIZE_W)  ; reload screen width
                ld      b,a             ; cells to empty into B
                ld      a,$20           ; empty char (space)
                ld      c,VDP_SET       ; VDP set mode
                ld      hl,(VIDTMP1)    ; load address of the last row
                set     $06,h           ; writing mode
                out     (c),l           ; low byte then..
                out     (c),h           ; high byte of address
                ld      c,VDP_DAT       ; VDP data mode
RPTEMPTYROW:    out     (c),a           ; empty cell
                nop                     ; delay
                djnz    RPTEMPTYROW     ; repeat until the last row has been cleaned
                ld      a,$01
                ld      (PRNTVIDEO),a
                ret                     ; return to caller

; new line - NOT USED
LINEFEED:       ld      a,(SCR_CURS_Y)  ; load cursor Y into A
                inc     a               ; new row
                ld      hl,SCR_SIZE_H   ; load screen height
                cp      (hl)            ; is the cursor over the bottom of the screen?
                ret     z               ; yes, exit
                ld      (SCR_CUR_NY),a  ; no, move cursor to the new line
                ld      a,(SCR_CURS_X)  ; load cursor X
                ld      (SCR_CUR_NX),a  ; new cursor X
                call    MOVCRS          ; move cursor
                ret                     ; return to caller

; carriage return - move to the next line and position the cursor at the beginning of the row (CR+LF)
CRGRETURN:      call    RSTCHRCRS       ; recover char under the cursor
                xor     a               ; move to col 0
                ld      (SCR_CURS_X),a  ; store new X
                ld      a,(SCR_CURS_Y)  ; load cursor Y into A
                inc     a               ; new row
                ld      hl,SCR_SIZE_H   ; load screen height
                cp      (hl)            ; is the cursor over the bottom of the screen?
                jr      nz,ADDNEWLINE   ; no, store new Y
                dec     a               ; yes, so come back 1 row, then
                push    af
                call    SCROLLUP        ; scroll screen
                pop     af
ADDNEWLINE:     ld      (SCR_CURS_Y),a  ; store new Y
                call    POS_CURSOR      ; position cursor to new location
                ret                     ; return to caller

; ------------------------------------------------------------------------------
                ; this table contains the values of the offsets to be added to
                ; the starting address of the name table to find the correct
                ; value of the first cell of the corresponding row
                ; (by doing so, it's faster than doing a multipication)
                ; table for graphics 1 text mode: 32 cols
POS_TB_CRS_32   DEFW    $0000,$0020,$0040,$0060,$0080,$00A0,$00C0,$00E0
                DEFW    $0100,$0120,$0140,$0160,$0180,$01A0,$01C0,$01E0
                DEFW    $0200,$0220,$0240,$0260,$0280,$02A0,$02C0,$02E0
                ; table for pure text mode: 40 cols
POS_TB_CRS_40   DEFW    $0000,$0028,$0050,$0078,$00A0,$00C8,$00F0,$0118
                DEFW    $0140,$0168,$0190,$01B8,$01E0,$0208,$0230,$0258
                DEFW    $0280,$02A8,$02D0,$02F8,$0320,$0348,$0370,$0398

; ------------------------------------------------------------------------------
; reset VRAM
EMPTY_VRAM:     ld      c,VDP_SET       ; load VPD port value
                ld      hl,$4000        ; first RAM cell $0000 (+$4000 bcs. MSBs must be 0 & 1, resp.)
                xor     a               ; reg.A cleared
                out     (c),l           ; low byte of address to VDP
                out     (c),h           ; high byte address to VDP
                ld      b,$40           ; $40 pages of RAM...
                ld      d,a             ; ...each one with $100 cells (tot. $4000 bytes)
                ld      c,VDP_DAT       ; VDP data mode
EMPTVRM:        out     (c),a           ; after first byte, the VDP autoincrements VRAM pointer
                nop                     ; useless?
                inc     d               ; next cell
                jr      nz,EMPTVRM      ; repeat until page is fully cleared
                djnz    EMPTVRM         ; repeat for $40 pages
                ret                     ; return to caller

; empty video registers in SRAM
EMPTY_RAM:      ld      hl,SCR_SIZE_W   ; address of first register
                xor     a               ; $00 to clean the registers
                ld      b,PBUFF-SCR_SIZE_W; how many bytes to clean (this is calculated dinamically since we can add/remove some registers)
RSTVDPRAMREG:   ld      (hl),a          ; reset register
                inc     hl              ; next register
                djnz    RSTVDPRAMREG    ; repeat
                ret                     ; return to caller

; ------------------------------------------------------------------------------
; set a specific graphics mode, passed into reg. E
SET_GFX_MODE:   ld      b,$08           ; 8 registers means 8 bytes..
                sla     e               ; multiply E by 8..
                sla     e               ; so that reg. E can point..
                sla     e               ; to the correct settings
                ld      hl,VDPMODESET   ; pointer to register settings
                add     hl,de           ; add offset to get the correct set of values for the required mode
                ld      a,$80           ; start with REG0 ($80+register number)
                ld      c,VDP_SET       ; VDP set
LDREGVLS:       ld      d,(HL)          ; load register's value
                out     (c),d           ; send data to VDP
                out     (c),a           ; indicate the register to send data to
                inc     a               ; next register
                inc     hl              ; next value
                djnz    LDREGVLS        ; repeat for 8 registers
                ret

; ------------------------------------------------------------------------------
LOADCHARSET:    ; reg. A contains the video mode
                ; reg. B containts the # of patterns to be loaded ($00 for 256 chars)
                ; reg. HL contains address of pattern table into VRAM
                set     $06,h           ; add $4000 to address to indicate that we want to write into VRAM
                ld      c,VDP_SET       ; load VDP address into C
                out     (c),l           ; send low byte of address
                out     (c),h           ; send high byte
                ld      hl,CHRST68      ; starting address of 6x8 charset into ROM
                cp      $00             ; is it text mode (A=0)?
                jr      z,NXTCHAR       ; yes, so jump to load chars into VRAM
                ld      hl,CHRST88      ; no, so we change and load the 8x8 charset
NXTCHAR:        ld      d,$08           ; 8 bytes per pattern char
                ld      c,VDP_DAT       ; VDP data mode
SENDCHRPTRNS:   ld      a,(hl)          ; load byte to send to VDP
                out     (c),a           ; write byte into VRAM
                nop                     ; little delay (useless? - to verify)
                inc     hl              ; inc byte pointer
                dec     d               ; 8 bytes sents (1 char)?
                jr      nz,SENDCHRPTRNS ; no, continue
                djnz    NXTCHAR         ; yes, decrement chars counter and continue for all the chars
                ret                     ; return to caller

; ------------------------------------------------------------------------------

WELCOMEMSG:     ; reg. DE contains address of message
                ; print at current cursor position (set into HL)
                set     $06,h           ; set bit #6 of address, to write to VRAM (this is like adding $4000 to address)
                ld      c,VDP_SET       ; load VPD port value
                out     (c),l           ; low byte of address to VDP
                out     (c),h           ; high byte address to VDP
                ld      hl,de           ; load start address of welcome message
LDWLCMMSG       ld      a,(hl)          ; load char
                cp      $00             ; is it the end of message?
                ret     z               ; yes, exit
                ld      c,VDP_DAT       ; VDP data mode
                out     (c),a           ; print char onto screen
                inc     hl              ; increment pointer
                jr      LDWLCMMSG       ; go to next char

;------------------------------------------------------------------------------
; NAME TABLE:       buffer video - contains the chars to be shown on video
; PATTERN TABLE:    charset - contains the chars/tiles to be loaded into the name table
; COLOR TABLE:      color settings for chars/tiles

                ; VDP register settings for a text display
VDPMODESET      defb    00000000b       ; reg.0: external video off
                defb    11010000b       ; reg.1: 16K VRAM, video on, int. off, text mode (40x24)
                defb    $02             ; reg.2: name table set to $0800 ($02x$400)
                defb    $00             ; reg.3: not used in text mode
                defb    $00             ; reg.4: pattern table set to $0000
                defb    $00             ; reg.5: not used in text mode
                defb    $00             ; reg.6: not used in text mode
                defb    $f5             ; reg.7: white text on light blue background

VDPMODESET1     ; VDP register settings for a graphics 1 mode
                defb    00000000b       ; reg.0: ext. video off
                defb    11000000b       ; reg.1: 16K Vram; video on, int off, graphics mode 1, sprite size 8x8, sprite magn. 0
                defb    $06             ; reg.2: name table address: $1800
                defb    $80             ; reg.3: color table address: $2000
                defb    $00             ; reg.4: pattern table address: $0000
                defb    $36             ; reg.5: sprite attr. table address: $1B00
                defb    $07             ; reg.6: sprite pattern table addr.: $3800
                defb    $05             ; reg.7: backdrop color (light blue)

VDPMODESET2     ; VDP register settings for a graphics 2 mode
                defb    00000010b       ; reg.0: graphics 2 mode, ext. video dis.
                defb    11000000b       ; reg.1: 16K VRAM, video on, INT off, sprite size 8x8, sprite magn. 0
                defb    $06             ; reg.2: name table addr.: $1800
                defb    $FF             ; reg.3: color table addr.: $2000
                defb    $03             ; reg.4: pattern table addr.: $0000
                defb    $36             ; reg.5: sprite attr. table addr.: $1B00
                defb    $07             ; reg.6: sprite pattern table addr.: $3800
                defb    $0F             ; reg.7: backdrop color: white

VDPMODESETMC    ; VDP register settings for a multicolor mode
                defb    00000000b       ; reg.0: ext. video dis.
                defb    11001011b       ; reg.1: 16K VRAM, video on, INT off, multicolor mode, sprite size 8x8, sprite magn. 0
                defb    $02             ; reg.2: name table addr.: $0800
                defb    $00             ; reg.3: don't care
                defb    $00             ; reg.4: pattern table addr.: $0000
                defb    $36             ; reg.5: sprite attr. table addr.: $1B00
                defb    $07             ; reg.6: sprite pattern table addr.: $3800
                defb    $0F             ; reg.7: backdrop color (white)

WLCMSG          defm    "LM80C Color Computer",0   ; system message
