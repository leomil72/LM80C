; ------------------------------------------------------------------------------
; LM80C - VDP ROUTINES - R1.3
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Code Revision:
; R1.0 - 20190511 - First version
; R1.1 - 20190512
; R1.2 - 20190515
; R1.3 - 20190521 - Video cursor management - preliminary
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
initVDP:        ; initialize VDP for a specific graphics mode
                ; mode is passed through reg. E

                ; initialize VDP
                push de             ; store reg. E
                call EMPTY_VRAM     ; reset VRAM
                call SET_GFX_MODE   ; load register settings
                call EMPTY_RAM      ; reset RAM registers
                pop de              ; recover reg. E
                ld a,e              ; move E into A
                cp $01              ; is it graphics 1?
                jp z,G1MD           ; yes, jump over
                cp $02              ; is it graphics 2?
                jp z,G2MD           ; yes, jump over
                cp $03              ; is it multicolor?
                jp z,MCMD           ; yes, jump over
                ; if $00 or no valid case, we assume that it's text mode

; LOAD VDP SETTINGS FOR SELECTED VIDEO MODE
;------------------------------------------
; TEXT MODE
TXTMD:          ; load charset
                ld b,$00            ; chars to be loaded (0=256)
                ld hl,$0000         ; first pattern cell $0000
                call LOADCHARSET    ; load patterns into VRAM
                ; set cursor & video overlay
                xor a               ; position cursor
                ld (SCR_CURS_X),a   ; at X=0
                ld a,$01
                ld (SCR_CURS_Y),a   ; and Y=1
                ld a,$28
                ld (SCR_SIZE_W),a   ; screen width = 40 cols
                ld a,$18
                ld (SCR_SIZE_H),a   ; screen height = 24 rows
                ld de,$0800
                ld (SCR_NAM_TB),de  ; set name table address
                ld de,$03C0
                ld (SCR_SIZE_B),de  ; set screen size in chars (40x24=960)
                jp VIDWELCOME       ; execute the rest of the video setting

; GRAPHICS 1 MODE
G1MD:           ; load pattern table
                ld b,$00            ; patterns to be loaded (0=256)
                ld hl,$0000         ; first pattern cell $0000
                call LOADCHARSET    ; load patterns into VRAM
                ; set cursor & video overlay
                xor a               ; position cursor
                ld (SCR_CURS_X),a   ; at X=0
                ld a,$01            ;
                ld (SCR_CURS_Y),a   ; and Y=1
                ld a,$20
                ld (SCR_SIZE_W),a   ; screen width = 32 cols
                ld a,$18
                ld (SCR_SIZE_H),a   ; screen height = 24 rows
                ld de,$1800
                ld (SCR_NAM_TB),de  ; set name table address
                ld de,$0300
                ld (SCR_SIZE_B),de  ; set screen size in chars (32x24=768)
                ; load color table
                ld c,VDP_SET        ; load VPD port value
                ld hl,$6000         ; color table start: $2000 (+$4000 bcs MSBs must be 0 & 1, resp.)
                ld a,$f5            ; reg.A loaded with colors for chars: white pixels on light blue background
                out (c),l           ; low byte of address to VDP
                out (c),h           ; high byte address to VDP
                ld b,$20            ; 32 bytes of colors
LDCLRTBMD1:     out (VDP_DAT),a     ; after first byte, the VDP autoincrements VRAM pointer
                nop
                djnz LDCLRTBMD1     ; repeat for 32 bytes
                jp VIDWELCOME       ; execute the rest of the video setting

; GRAPHICS 2 MODE
G2MD:           nop                 ; just a place-holder
                jp VIDWELCOME       ; execute the rest of the video setting

; MULTICOLOR MODE
MCMD:           nop                 ; just a place-holder
                jp VIDWELCOME       ; execute the rest of the video setting

; LAST VDP SETTINGS
VIDWELCOME:     call EMPTYVIDBUF    ; empty video buffer
                ld (SCR_ORG_CHR),a  ; A containts the byte used to empty the video buffer
                ; load welcome message into name table
                ld hl,(SCR_NAM_TB)  ; position at X,Y=0,0 in name table
                ld de,WLCMSG        ; load start address of welcome message
                call WELCOMEMSG     ; print welcome message
                ld a,(SCR_CURS_Y)   ; check the video mode
                cp $c0              ; graphics 2?
                jr z,EXITVDWLCM     ; yes, so exit
                ld a,$01            ; no, activate the
                ld (CURSOR_ON),a    ; cursor
EXITVDWLCM:     ret                 ; return to caller

; ------------------------------------------------------------------------------
; empty video buffer
EMPTYVIDBUF:    ld c,$20            ; we will fill up the screen with spaces
                ld a,(SCR_SIZE_H)   ; load the height of screen
                cp $c0              ; is it the graphics 2 mode (192 bytes instead of 24)?
                jr nz,SETGRPVOID    ; no, jump over
                ld c,$00            ; yes, we change the filling byte: it will be a $00
SETGRPVOID:     ld b,a              ; move rows into B
                ld a,c              ; move filling char into A
                ld c,VDP_SET        ; VDP address for setting
                ld hl,(SCR_NAM_TB)  ; load the name table address
                set $06,h           ; set bit #6 of address, to write to VRAM
                out (c),l           ; low and
                out (c),h           ; high byte of the first cell
                ld c,VDP_DAT        ; VDP address for passing data
LDCOLSTOEMPTY:  ld e,a              ; store filling char into E
                ld a,(SCR_SIZE_W)   ; load # of cols to empty into A
                ld d,a              ; move A into D
                ld a,e              ; recover filling char
RPTEMPTYBUF:    out (c),a           ; write empty byte into VRAM
                dec d               ; decr. D
                jr nz,RPTEMPTYBUF   ; repeat for the # of cols
                djnz LDCOLSTOEMPTY  ; repeat for the # of rows
                ret                 ; return to caller

; clear the video buffer and position the cursor at 0,0
CLEARVIDBUF:    call EMPTYVIDBUF    ; clear video buffer
                xor a               ; reset A
                ld (SCR_CURS_X),a   ; cursor X to 0
                ld (SCR_CURS_Y),a   ; cursor Y to 0
                call POS_CURSOR     ; position cursor
                ret                 ; return to caller

; position the cursor at the current coordinates, preserving underlying char
POS_CURSOR:     call LOAD_CRSR_POS  ; load the VRAM address of cursor into HL
                ld c,VDP_SET        ; VDP setting mode
                out (c),l           ; pass the low byte
                out (c),h           ; then the high byte of the address
                in a,(VDP_DAT)      ; load the current char at the cursor position
                ld (SCR_ORG_CHR),a  ; store the current char
                ld a,$ff            ; load cursor char
                set $06,h           ; set bit #6 for VRAM writing (this is similar to adding $40)
                ld c,VDP_SET        ; VDP setting mode
                out (c),l           ; pass the low byte
                out (c),h           ; then the high byte of the address
                ld c,VDP_DAT        ; VDP data mode
                out (c),a           ; send cursor char
                ret

; move cursor to new X,Y coordinates
MOVCRS:         call LOAD_CRSR_POS  ; recover old cursor position
                ld a,(SCR_ORG_CHR)  ; recover old char
                set $06,h           ; write-to-VRAM mode
                ld c,VDP_SET        ; VDP setting mode
                out (c),l           ; pass low and
                out (c),h           ; byte byte of address
                ld c,VDP_DAT        ; VDP data mode
                out (c),a           ; write old char into current cursor position
                ld a,(SCR_CUR_NX)   ; load new X
                ld (SCR_CURS_X),a   ; write new X
                ld a,(SCR_CUR_NY)   ; load new Y
                ld (SCR_CURS_Y),a   ; write new Y
                ld a,$ff            ; delete new values
                ld (SCR_CUR_NX),a   ; of X
                ld (SCR_CUR_NY),a   ; and Y
                call POS_CURSOR     ; position cursor into new location
                ret                 ; return to caller

; retrieve cursor position from either current coordinates or next place
; return address position into HL
LOAD_CRSR_POS:  ld ix,SCR_CURS_X    ; load address of current cursor position
                ld l,(ix+1)         ; load cursor Y into reg.L
                ld h,$00            ; reset H
                add hl,hl           ; create offset (each address is 2-bytes long so we need to double HL)
                ld a,(SCR_SIZE_W)   ; load screen width
                cp $28              ; is it 40 cols?
                jr z,SET40COLS      ; yes, jump over
                ld de,POS_TB_CRS_32 ; load position table address of cursor for 32 cols
                jr CONT_POS_CURS    ; jump over
SET40COLS:      ld de,POS_TB_CRS_40 ; load position table address of cursor for 40 cols
CONT_POS_CURS:  add hl,de           ; the correct starting address of the required row is now into HL
                ld de,(hl)          ; load starting address of the required row into DE
                ld hl,(SCR_NAM_TB)  ; load starting address of name table
                add hl,de           ; starting address of the current row into name table
                ld a,(ix)           ; load cursor X
                ld b,$00            ; reset B
                ld c,a              ; transfer A into C
                add hl,bc           ; add X offset: now HL contains the address of the current cursor position
                ret

;-------------------------------------------------------------------------------
; send current char to video buffer
CHAR2VID:       push bc             ; store BC
                push de             ; store DE
                push hl             ; store HL
                ld a,(CHR4VID)      ; recover char
                cp $00              ; is it char 0? (null char)?
                jr z,EXITCHAR2VID   ; yes, exit now
                cp $0c              ; is it the CLEAR char?
                jr nz,CHKCR         ; no, check over
                call CLEARVIDBUF    ; yes, clear video buffer and position cursor at 0,0
                jr EXITCHAR2VID     ; exit
CHKCR:          cp CR               ; is it a carriage return ($0D)?
                jr nz,CHKLF         ; no, jump over
                call CRGRETURN      ; yes, go the beginning of the next line
                jr EXITCHAR2VID     ; finished
CHKLF:          cp LF               ; is it a line feed ($0A)?
                jr z,EXITCHAR2VID   ; CURRENTLY WE DON'T PRINT LF, WE ONLY USE CR TO SUBSTITUTE CR+LF COMBINATION
                ; jr nz,PRNTCHAR      ; no, print the char
                ; call LINEFEED       ; print a LF
                ; jr EXITCHAR2VID     ; finished
PRNTCHAR:       call LOAD_CRSR_POS  ; recover position of cursor
                ld c,VDP_SET        ; VDP set mode
                set $06,h           ; add $40 to high byte for writing into VRAM
                out (c),l           ; send low byte then
                out (c),h           ; high byte of current position
                ld a,(CHR4VID)      ; recover char
                ld c,VDP_DAT        ; VDP data mode
                out (c),a           ; write char to the current cursor position
                ld a,(SCR_CURS_Y)   ; load cursor Y into A
                ld e,a              ; store cursor Y into E
                ld a,(SCR_CURS_X)   ; load cursor X
                inc a               ; move 1 step to right
                ld hl,SCR_SIZE_W    ; cell that keeps the width of screen
                cp (hl)             ; have we reached the most right position?
                jr nz,SETCRSRX      ; no, go over
                inc e               ; yes, increment cursor Y (go to next line)
                xor a               ; then move to cursor X (go to beginning of line)
SETCRSRX:       ld (SCR_CURS_X),a   ; store current cursor X
                ld a,e              ; recover Y
                ld (SCR_CURS_Y),a   ; store current cursor Y
EXITCHAR2VID:   xor a               ; reset A
                ld (CHR4VID),a      ; reset char
                pop hl              ; restore HL
                pop de              ; restore DE
                pop bc              ; restore BC
                ret                 ; return to caller

; move cursor up
CURSORUP:       push af             ; store A
                ld a,(SCR_CURS_Y)   ; load cursor Y into A
                cp $00              ; is it at the top of the screen?
                jr z,EXITCURSORUP   ; yes, exit
                dec a               ; decrement Y
                ld (SCR_CUR_NY),a   ; store new Y
                call MOVCRS         ; move cursor into new position
EXITCURSORUP:   pop af              ; restore A
                ret                 ; return to caller

; move cursor left
CURSORLEFT:     push af             ; store A
                ld a,(SCR_CURS_X)   ; load cursor X into A
                cp $00              ; is it at the most left of the screen?
                jr z,EXITCURSORLEFT ; yes, exit
                dec a               ; decrement X
                ld (SCR_CUR_NX),a   ; store new X
                call MOVCRS         ; move cursor into new position
EXITCURSORLEFT: pop af              ; restore A
                ret                 ; return to caller

; move cursor right
CURSORRIGHT:    push af             ; store A
                push hl             ; store HL
                ld a,(SCR_CURS_X)   ; load cursor X into A
                ld hl,SCR_SIZE_W    ; load screen width
                cp (hl)             ; is the cursor at the most right of the screen?
                jr z,EXITCURSORRIGHT; yes,exit
                inc a               ; move cursor to the next col.
                ld (SCR_CUR_NX),a   ; store new X (it's the same of the prev.one)
EXITCURSORRIGHT pop hl              ; restore HL
                pop af              ; restore A
                ret                 ; return to caller

; move cursor down
CURSORDOWN:     push af             ; store A
                push hl             ; store HL
                ld a,(SCR_CURS_Y)   ; load cursor Y into A
                ld hl,SCR_SIZE_H    ; load screen height
                cp (hl)             ; is the cursor at the bottom of the screen?
                jr z,EXITCURSORDOWN ; yes, exit
                inc a               ; move cursor to the next row
                ld (SCR_CUR_NY),a   ; store new Y
EXITCURSORDOWN: pop hl              ; restore HL
                pop af              ; restore A
                ret

; new line - NOT USED
LINEFEED:       ld a,(SCR_CURS_Y)   ; load cursor Y into A
                inc a               ; new row
                ld hl,SCR_SIZE_H    ; load screen height
                cp (hl)             ; is the cursor over the bottom of the screen?
                ret z               ; yes, exit
                ld (SCR_CUR_NY),a   ; no, move cursor to the new line
                ld a,(SCR_CURS_X)   ; load cursor X
                ld (SCR_CUR_NX),a   ; new cursor X
                call MOVCRS         ; move cursor
                ret                 ; return to caller

; carriage return - move to the next line and position the cursor at the beginning of the row (CR+LF)
CRGRETURN:      xor a               ; move to col 0
                ld (SCR_CUR_NX),a   ; store new X
                ld a,(SCR_CURS_Y)   ; load cursor Y into A
                inc a               ; new row
                ld hl,SCR_SIZE_H    ; load screen height
                cp (hl)             ; is the cursor over the bottom of the screen?
                jr nz,ADDNEWLINE    ; no, store new Y
                dec a               ; cursor stays on the current line - TODO: SCROLL SCREEN
ADDNEWLINE:     ld (SCR_CUR_NY),a   ; store new Y
                call MOVCRS         ; position cursor to new location
                ret                 ; return to caller

SCROLLUP:       ;at the moment it does nothing
                ret                 ; return to caller
; ------------------------------------------------------------------------------
                ; this table contains the values of the offsets to be added
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


CSR_ORG_CHR
; ------------------------------------------------------------------------------
; reset VRAM
EMPTY_VRAM:     ld c,VDP_SET        ; load VPD port value
                ld hl,$4000         ; first RAM cell $0000 (+$4000 bcs. MSBs must be 0 & 1, resp.)
                xor a               ; reg.A cleared
                out (c),l           ; low byte of address to VDP
                out (c),h           ; high byte address to VDP
                ld b,$40            ; $40 pages of RAM...
                ld d,a              ; ...each one with $100 cells (tot. $4000 bytes)
EMPTVRM:        out (VDP_DAT),a     ; after first byte, the VDP autoincrements VRAM pointer
                nop
                inc d               ; next cell
                jr nz,EMPTVRM       ; repeat until page is fully cleared
                djnz EMPTVRM        ; repeat for $40 pages
                ret

; empty RAM registers
EMPTY_RAM:      ld hl,SCR_SIZE_W    ; address of first register
                xor a               ; $00 to clean the registers
                ld b,BUFVIDEO-SCR_SIZE_W   ; how many bytes to clean (this is calculated dinamically since we can add/remove some registers)
RSTVDPRAMREG:   ld (hl),a           ; reset register
                inc hl              ; next register
                djnz RSTVDPRAMREG   ; repeat
                ret                 ; return to caller

; ------------------------------------------------------------------------------
; set a specific graphics mode, passed into reg. E
SET_GFX_MODE:   ld b,$08            ; 8 registers means 8 bytes
                sla e               ; multiply E by 8
                sla e               ; so that reg. E can point
                sla e               ; to the correct settings
                ld hl,VDPMODESET    ; pointer to register settings
                add hl,de           ; get the correct set of values for the required mode
                ld a,$80            ; start with REG0 ($80+register number)
                ld c,VDP_SET        ; VDP set
LDREGVLS:       ld d,(HL)           ; load register's value
                out (c),d           ; send data to VDP
                out (c),a           ; indicate the register to send data to
                inc a               ; next register
                inc hl              ; next value
                djnz LDREGVLS       ; repeat for 8 registers
                ret

; ------------------------------------------------------------------------------
LOADCHARSET:    ; reg. B containts the # of patterns to be loaded
                ; reg. HL contains address of pattern table into VRAM
                set 6,h             ; add $4000 to address to indicate that we want to write into VRAM
                ld c,VDP_SET        ; load VDP address into C
                out (c),l           ; send low byte of address
                out (c),h           ; send high byte
                ld hl,CHARSET       ; address of first byte of first pattern into ROM
NXTCHAR:        ld d,$08            ; 8 bytes per pattern char
SENDCHRPTRNS:   ld a,(hl)           ; load byte to send to VDP
                out (VDP_DAT),a     ; write byte into VRAM
                nop                 ; little delay (useless? - to verify)
                inc hl              ; inc byte pointer
                dec d               ; 8 bytes sents (1 char)?
                jr nz,SENDCHRPTRNS  ; no, continue
                djnz NXTCHAR        ; yes, decrement chars counter and continue for all the chars
                ret                 ; return to caller

; ------------------------------------------------------------------------------

WELCOMEMSG:     ; reg. DE contains address of message
                ; print at current cursor position (set into HL)
                set $06,h           ; set bit #6 of address, to write to VRAM (this is like adding $4000 to address)
                ld c,VDP_SET        ; load VPD port value
                out (c),l           ; low byte of address to VDP
                out (c),h           ; high byte address to VDP
                ld hl,de            ; load start address of welcome message
LDWLCMMSG       ld a,(hl)           ; load char
                cp $00              ; is it the end of message?
                ret z               ; yes, exit
                out (VDP_DAT),a     ; no, print char onto screen
                nop
                inc hl              ; increment pointer
                jr LDWLCMMSG        ; go to next char

;------------------------------------------------------------------------------
; NAME TABLE:       buffer video - contains the chars to be shown on video
; PATTERN TABLE:    charset - contains the chars/tiles to be loaded into the name table
; COLOR TABLE:      color settings for chars/tiles

                ; VDP register settings for a text display
VDPMODESET      defb 00000000b  ; reg.0: external video off
                defb 11010000b  ; reg.1: 16K VRAM, video on, int. off, text mode (40x24)
                defb $02        ; reg.2: name table set to $0800 ($02x$400)
                defb $00        ; reg.3: not used in text mode
                defb $00        ; reg.4: pattern table set to $0000
                defb $00        ; reg.5: not used in text mode
                defb $00        ; reg.6: not used in text mode
                defb $5f        ; reg.7: light blue text on white background

VDPMODESET1     ; VDP register settings for a graphics 1 mode
                defb 00000000b  ; reg.0: ext. video off
                defb 11000000b  ; reg.1: 16K Vram; video on, int off, graphics mode 1, sprite size 8x8, sprite magn. 0
                defb $06        ; reg.2: name table address: $1800
                defb $80        ; reg.3: color table address: $2000
                defb $00        ; reg.4: pattern table address: $0000
                defb $36        ; reg.5: sprite attr. table address: $1B00
                defb $07        ; reg.6: sprite pattern table addr.: $3800
                defb $05        ; reg.7: backdrop color (light blue)

VDPMODESET2     ; VDP register settings for a graphics 2 mode
                defb 00000010b  ; reg.0: graphics 2 mode, ext. video dis.
                defb 11000000b  ; reg.1: 16K VRAM, video on, INT off, sprite size 8x8, sprite magn. 0
                defb $06        ; reg.2: name table addr.: $1800
                defb $FF        ; reg.3: color table addr.: $2000
                defb $03        ; reg.4: pattern table addr.: $0000
                defb $36        ; reg.5: sprite attr. table addr.: $1B00
                defb $07        ; reg.6: sprite pattern table addr.: $3800
                defb $0F        ; reg.7: backdrop color: white

VDPMODESETMC    ; VDP register settings for a multicolor mode
                defb 00000000b  ; reg.0: ext. video dis.
                defb 11001011b  ; reg.1: 16K VRAM, video on, INT off, multicolor mode, sprite size 8x8, sprite magn. 0
                defb $02        ; reg.2: name table addr.: $0800
                defb $00        ; reg.3: don't care
                defb $00        ; reg.4: pattern table addr.: $0000
                defb $36        ; reg.5: sprite attr. table addr.: $1B00
                defb $07        ; reg.6: sprite pattern table addr.: $3800
                defb $0F        ; reg.7: backdrop color (white)

WLCMSG  defm "LM80C Color Computer",0                ; system message
