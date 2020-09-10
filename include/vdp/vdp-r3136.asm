; ------------------------------------------------------------------------------
; LM80C - VDP ROUTINES - R3.13.6
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. Code and computer schematics are released under
; the therms of the GNU GPL License 3.0 and in the form of "as is", without no
; kind of warranty: you can use them at your own risk.
; You are free to use them for any non-commercial use: you are only asked to
; maintain the copyright notices, include thiszzzzzzzzzzzzzzzzzz advice and the note to the 
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
;------------------------------------------------------------------------------
; VDP INITIALISATION
; initialize VDP for a specific graphics mode
; INPUT: E -> contains the graphics mode:
; 0=text; 1=graphics 1; 2=graphics 2; 3=multicolor; 4=extended graphics 2
initVDP:        push    DE              ; store E
                call    EMPTY_VRAM      ; reset VRAM
                call    SET_GFX_MODE    ; load register settings
                call    CLR_RAM_REG     ; reset RAM registers
                pop     DE              ; restore reg. E
                xor     A               ; reset A
                ld      B,A             ; reset B (will be used later)
                ld      A,E             ; move E into A
                ld      (SCR_MODE),A    ; store screen mode
                cp      $01             ; is it graphics 1 (A=1)?
                jp      Z,G1MD          ; yes, jump over
                cp      $02             ; is it graphics 2 (A=2)?
                jp      Z,G2MD          ; yes, jump over
                cp      $03             ; is it multicolor (A=3)?
                jp      Z,MCMD          ; yes, jump over
                cp      $04             ; is it extended graphics 2 (A=4)?
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
                ld      A,$05           ; light blue
                ld      (BKGNDCLR),A    ; set background/border color
                ld      A,$28
                ld      (SCR_SIZE_W),A  ; screen width = 40 cols
                ld      A,$18
                ld      (SCR_SIZE_H),A  ; screen height = 24 rows
                ld      DE,$0800
                ld      (SCR_NAM_TB),DE ; set name table address
                jp      ENDVDPSET       ; execute the rest of the video setting

                ; GRAPHICS 1 MODE (G1)
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
                ; load color table
                ld      HL,$2000        ; color table start: $2000
                call    SETVDPADRS
                ld      A,$01           ; foreground color...
                ld      (FRGNDCLR),A    ; ...set to black
                ld      A,$0F           ; background color...
                ld      (BKGNDCLR),A    ; ...set to white
                ld      A,$1F           ; reg.A loaded with colors for chars: bloack pixels on white background
                ld      B,$20           ; 32 bytes of colors
                ld      C,VDP_DAT       ; VDP data mode
LDCLRTBMD1:     out     (C),A           ; after first byte, the VDP autoincrements VRAM pointer
                nop
                nop
                djnz    LDCLRTBMD1      ; repeat for 32 bytes
                jp      ENDVDPSET       ; execute the rest of the video setting

                ; GRAPHICS 2 MODE (G2)
G2MD:           xor     A               ; position cursor
                ld      (SCR_CURS_X),A  ; at X=0
                ld      (SCR_CURS_Y),A  ; and Y=0
                ld      (SCR_SIZE_W),A  ; screen width = 256 pixels (0=256)
                inc     A               ; black on...
                ld      (FRGNDCLR),A    ; ...foreground
                ld      A,$0F           ; white on...
                ld      (BKGNDCLR),A    ; ...background
                ld      A,$C0
                ld      (SCR_SIZE_H),A  ; screen height = 192 pixels
                ld      DE,$1800
                ld      (SCR_NAM_TB),DE ; set name table address
                jp      ENDVDPSET       ; execute the rest of the video setting

                ; MULTICOLOR MODE (G3)
MCMD:           xor     A               ; position cursor
                ld      (SCR_CURS_X),A  ; at X=0
                ld      (SCR_CURS_Y),A  ; and Y=0
                ld      A,$0F           ; white color for...
                ld      (BKGNDCLR),A    ; ...background and...
                ld      (FRGNDCLR),A    ; ...foreground (even this is not used in MC)
                ld      A,$40
                ld      (SCR_SIZE_W),A  ; screen width = 64 blocks
                ld      A,$30
                ld      (SCR_SIZE_H),A  ; screen height = 48 blocks
                ld      DE,$0800
                ld      (SCR_NAM_TB),DE ; set name table address
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
                 ; load color table
                ld      HL,$2000        ; color table start: $2000
                call    SETVDPADRS
                ld      A,$01           ; foreground color is...
                ld      (FRGNDCLR),A    ; ...set to black
                ld      A,$0F           ; whitefor...
                ld      (BKGNDCLR),A    ; ...background
                ld      A,$1F           ; reg.A loaded with colors for chars: bloack pixels on white background
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
                xor     A
                ld      (SCR_ORG_CHR),A ; store byte used tochar used to empty the video buffer
                ret                     ; return to caller


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
                ld      HL,$1800        ; name table address
                call    SETNAMETABLE    ; set name table (load names into table)
                call    ERASECLRTBL     ; erase color table (set foreground & background to black)
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
                nop
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
                ; show logo/message, play a beep and wait a while
                ld      B,$02
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
ERASECLRTBL:    ; erase color table
                ld      A,$11           ; foreground and background set to black
                ld      D,$0A           ; 10 pages
                ld      B,$00           ; 256 color cells per page
                ld      HL,$2800        ; first cell of 2nd color table
                call    SETVDPADRS      ; send address
                ld      C,VDP_DAT       ; VDP address for passing data
RPT100:         out     (C),A           ; send data
                nop
                nop                     ; little delay
                djnz    RPT100          ; repeat for entire page
                dec     D
                jr      NZ,RPT100       ; repeat for all the pages ($0A00 cells)
                ret                     ; return to caller

; play a beep
SETBEEP:        ld      A,(TMPBFR1)     ; check the already-beeped flag?
                or      A               ; is it 0?
                ret     NZ              ; no, exit
                inc     A               ; flag to 1
                ld      (TMPBFR1),A     ; set sound
                jp      WLCMBEEP        ; play a beep & return

; beep off
BEEPOFF:        ld      A,$02           ; flag for sound off
                ld      (TMPBFR1),A     ; set flag
                jp      NOBEEP          ; stop beep and return


; used to load the chars that will compose the logo of the splash screen
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
                defb    $18,$1B,$13,$14,$1D ; colors of background bands of the logo
                
; empty video buffer
EMPTYVIDBUF:    ld      A,(SCR_MODE)    ; check screen mode
                cp      $02             ; is it G2 mode?
                jp      Z,EMPTYG2       ; yes, jump over
                cp      $03             ; is it MC mode?
                jp      Z,EMPTYMC       ; yes, jump over
                ld      A,(SCR_SIZE_H)  ; load height of screen
                ld      B,A             ; move rows into B
                xor     A               ; filling char is $00
                ld      HL,(SCR_NAM_TB) ; load the name table address
                call    SETVDPADRS      ; send address to VDP
                ld      C,VDP_DAT       ; VDP address for passing data
LDCOLSTOEMPTY:  ld      E,A             ; store filling char into E
                ld      A,(SCR_SIZE_W)  ; load # of cols to empty into A
                ld      D,A             ; move A into D
                ld      A,E             ; recover filling char
RPTEMPTYBUF:    out     (C),A           ; write empty byte into VRAM
                nop
                dec     D               ; decr. D
                jr      NZ,RPTEMPTYBUF  ; repeat for the # of cols
                djnz    LDCOLSTOEMPTY   ; repeat for the # of rows
                ret                     ; return to caller
EMPTYG2:        ld      HL,(SCR_NAM_TB) ; yes, additional setup for G2 - load G2 name table address (usually $1800)
                call    SETNAMETABLE    ; set name table
                ld      HL,$0000        ; set pattern table
                call    SETVDPADRS      ; send address to VDP
                xor     A               ; empty pattern
                ld      D,$18           ; 6144 ($1800) cell to clean, 24 pages ($18)
                ld      B,A             ; 256 bytes for page
                ld      C,VDP_DAT       ; VDP data mode
CLRG2PTNTBL:    out     (C),A           ; clear pattern
                nop                     ; little delay
                nop
                djnz    CLRG2PTNTBL     ; repeat for 1 page
                dec     D               ; next page
                jr      NZ,CLRG2PTNTBL  ; repeat
                ld      HL,$2000        ; load the color table address
                call    SETVDPADRS      ; send address to VDP
                ld      A,(FRGNDCLR)    ; load foreground
                add     A,A
                add     A,A
                add     A,A
                add     A,A             ; move to high nibble
                ld      D,A             ; store into D
                ld      A,(BKGNDCLR)    ; load background color
                or      D               ; combine with background color
                ld      D,$18           ; 6144 ($1800) cells to fill, so 24 pages ($18)
                jr      STARTEMPTY
EMPTYMC:        ld      HL,$0800        ; MC name table
                call    SETNAMETABLE    ; set name table
                ld      HL,$0000        ; color table address
                call    SETVDPADRS      ; send address to VDP
                ld      A,(BKGNDCLR)    ; load background
                ld      D,A             ; store into D
                add     A,A
                add     A,A
                add     A,A
                add     A,A             ; move to high nibble
                or      D               ; set background color for high and low nibble
                ld      D,$08           ; 2048 ($0800) cells to fill, so 8 pages ($08)
STARTEMPTY:     ld      C,VDP_DAT       ; VDP address for passing data
                ld      B,$00           ; 256 bytes each page ($00=256)
SNDCLRSET:      out     (C),A           ; send color setting
                nop                     ; wait a while
                nop
                djnz    SNDCLRSET       ; repeat for 1 page
                dec     D               ; have we filled all the pages?
                jr      NZ,SNDCLRSET    ; no, repeat
                ret                     ; return to caller

; set name table for G2 mode (patterns from $00 to $FF for each of the 3 areas of the screen)
SETNAMETABLE:   call    SETVDPADRS      ; send address to VDP
                ld      C,VDP_DAT       ; VDP address for passing data
                ld      D,$03           ; 3 pages to fill into VRAM (768 cells)
                xor     A               ; starting char name #0 (chars go from 0 to 255)
                ld      B,A             ; reset B
RPTFLL1:        out     (C),A           ; send name to VRAM
                nop
                inc     A               ; increment # of name
                djnz    RPTFLL1         ; repeat for 256 cells (1 page)
                dec     D               ; did we fill all the pages?
                jr      NZ,RPTFLL1      ; no, continue
                ret                     ; return to caller

; set an address into VRAM: address is in HL
SETVDPADRS:     ld      C,VDP_SET       ; VDP address mode
                set     6,H             ; set bit #6 of address, to write to VRAM
                out     (C),L           ; send low and...
                out     (C),H           ; ...high byte of the first cell
                ret                     ; return to caller

; clear the video buffer and position the cursor at 0,0
CLEARVIDBUF:    call    EMPTYVIDBUF     ; clear video buffer
                xor     A               ; reset A
                ld      (SCR_CURS_X),A  ; cursor X to 0
                ld      (SCR_CURS_Y),A  ; cursor Y to 0
                jp      POS_CURSOR      ; position cursor & return to caller

; HOME: position the cursor at coords. 0,0 
ATHOME:         xor     A               ; position cursor at 0,0 by storing...
                ld      (SCR_CUR_NY),A  ; ...new Y...
                ld      (SCR_CUR_NX),A  ; ...and new X
                jp      MOVCRS          ; move cursor to new location & return to caller

; load the char or byte at the VRAM position set by HL
; value is returned into A
READ_VIDEO_LOC: push    BC              ; store BC
                ld      C,VDP_SET       ; VDP setting mode
                ld      B,H
                res     7,B
                res     6,B
                out     (C),L           ; low byte then...
                out     (C),B           ; high byte
                ld      C,VDP_DAT       ; VDP data mode
                nop                     ; wait...
                nop                     ; ...a while
                nop
                in      A,(C)           ; read byte at current VRAM location
                pop     BC              ; restore BC
                ret                     ; return to caller

; write a byte at the VRAM position pointed by HL
; value is in A
WRITE_VIDEO_LOC:push    BC              ; store BC
                ld      C,VDP_SET       ; VDP setting mode
                ld      B,H             ; copy H into B
                res     7,B
                set     6,B             ; write to VRAM
                out     (C),L           ; low byte then...
                out     (C),B           ; high byte of VRAM address
                ld      C,VDP_DAT       ; VDP data mode
                nop                     ; wait...
                nop                     ; ...a while
                nop
                out     (C),A           ; write byte into VRAM
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
MOVCRS:         call    RSTCHRCRS       ; restore the char in the current cursor position
                call    NEWCRSRCOORD    ; set new cursor's coordinates
MOVSHOWCRS:     call    POS_CURSOR      ; position cursor into new location
                ld      A,(TMPBFR1)     ; load status of cursor flashing
                and     $20             ; check cursor state
                ld      (LSTCSRSTA),A   ; store the last cursor state
                ld      A,$FF           ; set cursor visible after moved it
                jp      WRITE_VIDEO_LOC ; write into video cell


; set new cursor's coordinates:
NEWCRSRCOORD:   ld      A,(SCR_CUR_NX)  ; load new X
                ld      (SCR_CURS_X),A  ; write new X
                ld      A,(SCR_CUR_NY)  ; load new Y
                ld      (SCR_CURS_Y),A  ; write new Y
                ld      A,$FF           ; delete new values
                ld      (SCR_CUR_NX),A  ; of X
                ld      (SCR_CUR_NY),A  ; and Y
                ret

; recover char under the cursor and prints it onto the screen
RSTCHRCRS:      call    LOAD_CRSR_POS   ; recover old cursor position
                ld      A,(SCR_ORG_CHR) ; recover old char
                jp      WRITE_VIDEO_LOC ; write char into VRAM & return

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

; find X,Y coordinates of a screen address pointed in VRAM by HL
; return them into L,A for X,Y
HL2XY:          push    DE              ; store DE
                ld      DE,(SCR_NAM_TB) ; load starting address of name table into DE
                xor     A               ; clear Carry
                sbc     HL,DE           ; find position relative to screen (from 0,0)
                pop     DE
                ld      A,(SCR_SIZE_W)  ; load screen width
                ld      C,A             ; move it into C
                call    DIV_16_8        ; divide position by C: return Y into L and X into A
                ret                     ; return to caller

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
                cp      HOME            ; is it HOME char ($19)?
                jr      NZ,CHKCS        ; no, check over
                call    ATHOME          ; yes, move the cursor to 0,0
                jp      EXITCHAR2VID    ; exit
CHKCS:          cp      CS              ; is it the CLEAR char ($0C)?
                jr      NZ,CHKCR        ; no, check over
                call    CLEARVIDBUF     ; yes, clear video buffer and position cursor at 0,0
                jr      EXITCHAR2VID    ; exit
CHKCR:          cp      CR              ; is it a carriage return ($0D)?
                jr      NZ,CHKBKSP      ; no, jump over
                call    CRGRETURN       ; yes, go to the beginning of the next line
                jr      EXITCHAR2VID    ; exit
CHKBKSP:        cp      BKSP            ; is it the backspace ($08)?
                jr      NZ,CHKCRSLFT    ; no, jump over
                call    BACKSPACE       ; move cursor left 1 position
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
                call    LOAD_CRSR_POS   ; recover position of cursor
                ld      A,(CHR4VID)     ; recover char to print
                call    WRITE_VIDEO_LOC ; write A into VRAM at (HL)
                ld      A,(SCR_CURS_Y)  ; load cursor Y into A
                ld      E,A             ; store cursor Y into E
                ld      A,(SCR_CURS_X)  ; load cursor X
                inc     A               ; move 1 step to right
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
                dec     E               ; decrement 1 row, to set cursor Y on the last line
SETCRSRY:       xor     A               ; then set cursor X to 0 (go to beginning of line)
SETCSRCOORDS:   ld      (SCR_CURS_X),A  ; store current cursor X
                ld      A,E             ; recover Y
                ld      (SCR_CURS_Y),A  ; store current cursor Y
                call    MOVSHOWCRS      ; move cursor to the new location and set it on
EXITCHAR2VID:   xor     A               ; reset char
                ld      (CHR4VID),A     ; to be sent to screen
                pop     AF              ; recover cursor state
                and     A               ; was it off (A=0)?
                call    NZ,CURSOR_ON    ; no, set cursor on
                pop     HL              ; restore HL
                pop     DE              ; restore DE
                pop     BC              ; restore BC
                pop     AF              ; restore AF
                ret                     ; return to caller

; flash the cursor at the current position
; (this sub-routine is called by CH3 timer ISR)
FLASHCURSOR:    ld      A,(CRSR_STATE)  ; now, check the cursor
                and     A               ; cursor off (A=0)?
                ret     Z               ; yes, return
                ld      A,(TMRCNT)      ; no, load the first byte of the 100ths of A second's counter
                and     $20             ; check if it's time to flash the cursor (check bit #6)
                ld      HL,LSTCSRSTA    ; load address of cell that stores the last cursor state
                ld      B,(HL)          ; load last state
                cp      B               ; compare current state with last state
                ret     Z               ; same state, no change required - exit
                ld      (HL),A          ; save new state
                push    AF              ; store A (keep state for later use)
                call    LOAD_CRSR_POS   ; load current cursor position into HL
                pop     AF              ; recover current state
                ld      B,$FF           ; cursor char
                cp      $20             ; is the cursor on video (A == $20)?
                jr      Z,PUTCRSCHR     ; yes, jump over
                ld      A,(SCR_ORG_CHR) ; no, load the original char
                ld      B,A             ; move char into B
PUTCRSCHR:      ld      A,B             ; recover char from B
                jp      WRITE_VIDEO_LOC ; print cursor/char at the current position & return


; delete the char at the left of the cursor
BACKSPACE:      call    MVCRS2LFT       ; prepare to move cursor to left
                call    RSTCHRCRS       ; restore char under the cursor
                call    NEWCRSRCOORD    ; set new cursor's coordinates
                call    LOAD_CRSR_POS   ; find address of new video cell
                xor     A               ; null char
                call    WRITE_VIDEO_LOC ; write into video cell
                jp      MOVSHOWCRS      ; move cursor to the new location and set it ON


; move cursor to left
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
CONTCRSLFT:     call    MOVCRS          ; move cursor into new position
EXITCURSORLEFT: pop     AF              ; restore A
                ret                     ; return to caller

; move cursor 1 position to the left
MVCRS2LFT:      ld      A,(SCR_CURS_X)  ; load cursor X into A
                and     A               ; is it at the most left of the screen (X=0)?
                jr      Z,CHKYPOS       ; yes, check Y position
                dec     A               ; no, decrement X
                ld      (SCR_CUR_NX),A  ; store new X
                ld      A,(SCR_CURS_Y)  ; load current cursor Y
                ld      (SCR_CUR_NY),A  ; no move over Y axis
                ret                     ; go on moving cursor
CHKYPOS:        ld      A,(SCR_CURS_Y)  ; load cursor Y
                and     A               ; is it at the most top of the screen (Y=0)?
                jr      Z,EXITCURSORLEFT; yes, exit doing nothing
                dec     A               ; no, decrement Y
                ld      (SCR_CUR_NY),A  ; store new Y
                ld      A,(SCR_SIZE_W)  ; load current screen width
                dec     A               ; cursor to the most right position (width-1)
                ld      (SCR_CUR_NX),A  ; set new cursor X
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


; move cursor to right
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
                jr      NC,EXITCURSOR_ON; yes, so exit (no cursor in graphics 2 or 3)
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
                dec     A               ; decrement the # of rows: now, A contains the # of rows to be moved
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
                set     6,H             ; writing mode
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
                xor     A               ; null char
                ld      C,VDP_SET       ; VDP set mode
                ld      HL,(VIDTMP1)    ; load address of the last row
                set     6,H             ; writing mode
                out     (C),L           ; low byte then..
                out     (C),H           ; high byte of address
                ld      C,VDP_DAT       ; VDP data mode
RPTEMPTYROW:    out     (C),A           ; empty cell
                nop                     ; delay
                nop
                djnz    RPTEMPTYROW     ; repeat until the last row has been cleaned
                ld      A,$01
                ld      (PRNTVIDEO),A   ; set print-on-video on
                ret                     ; return to caller

; carriage return: first, it looks for char $00 at the beginning of the line (look for the first null char),
; then it starts sendind every char it finds on the screen to the terminal buffer of the BASIC interpreter
; until another null char is found. Finally, move to the next line and position the cursor at the beginning
; of the row (equivalent to CR+LF), then gets back control to the screen editor to let it interpret the line
CUR_POS         equ     TMPBFR1         ; cursor position
SRTTXT          equ     TMPBFR2         ; start of text line
ENDTXT          equ     TMPBFR3         ; end of text line
CRGRETURN:      ; preliminary: disable cursor if on, and retrieve char under it
                ld      A,(CRSR_STATE)  ; recover cursor state
                ld      (TMPBFR4),A     ; store status
                and     A               ; is cursor on?
                call    NZ,CURSOR_OFF   ; yes, so set cursor off
                call    RSTCHRCRS       ; restore char under it
                ; first, check if cursor if off, so that we just interpret return as a new line command
                ld      A,(KBDNPT)      ; check if input from keyboad
                and     A               ; if 0, input is not from keyboard...
                jp      Z,PRNTRETURN    ; ...so just print a carriage return; otherwise, interpret the return
                ; first part: look for the beginning of the text line on screen
                call    LOAD_CRSR_POS   ; load cursor position into HL
                ld      (CUR_POS),HL    ; store it
                ld      DE,(SCR_NAM_TB) ; load VRAM address of top-left cell of screen ("home")
RPTNLLSRC:      push    HL
                call    CMP16           ; check if at "home"
                pop     HL
                jp      Z,CNTNULL       ; yes, exit because there is nothing before
                dec     HL              ; go 1 step back
                call    READ_VIDEO_LOC  ; read char of current position
                and     A               ; is it $00 (null char)?
                jr      NZ,RPTNLLSRC    ; no, continue searching
                inc     HL              ; move 1 step forward to go back to the last cell with something in
                ; second part: look for the ending of the text on screen
CNTNULL:        ld      (SRTTXT),HL     ; store beginning of text
                ld      A,(SCR_SIZE_H)
                ld      E,A             ; load screen height into DE
                ld      A,(SCR_SIZE_W)
                ld      L,A             ; load screen width into HL
                xor     A
                ld      H,A
                ld      D,A
                call    MUL16           ; multiply HL times DE to get the screen size
                ld      DE,(SCR_NAM_TB) ; load screen name table start address into DE
                add     HL,DE           ; get the address...
                dec     HL              ; ...of the "last" video cell
                ex      DE,HL           ; store address into DE
                ld      HL,(CUR_POS)    ; retrieve original cursor position
RPTNLLSRC2:     push    HL
                call    CMP16           ; check if at last position on screen (bottom right corner)
                pop     HL
                jp      Z,CNTNULL2      ; if yes, exit because these is nothing after
                inc     HL              ; 1 more step forward
                call    READ_VIDEO_LOC  ; read char of current position
                and     A               ; is it $00 (null char)?
                jr      NZ,RPTNLLSRC2   ; no, continue searching
CNTNULL2:       ld      (ENDTXT),HL     ; store ending of text line
                ld      DE,(SRTTXT)     ; load beginning of text line
                and     A               ; clear Carry
                sbc     HL,DE           ; how many chars?
                jr      Z,PRNTRETURN    ; no chars found (HL-DE=0), so just print return & leave
                ;---    central part: send the text on the screen to the interpreter
                ld      HL,(SRTTXT)     ; load beginning of text line
                ld      DE,(ENDTXT)     ; load ending of text line
SNDCHRTOBFR:    call    READ_VIDEO_LOC  ; read char
                push    HL
                call    CHARINTOBFR     ; send char to buffer
                pop     HL
                inc     HL              ; go to next char
                push    HL              ; store HL
                call    CMP16           ; check if DE=HL (finish chars)
                pop     HL
                jr      NZ,SNDCHRTOBFR  ; no, repeat
                ld      A,CR            ; yes, so now send carriage return
                call    CHARINTOBFR     ; send to buffer
                ld      HL,(ENDTXT)     ; recover address of last char of input text
                call    HL2XY           ; retrieve X,Y from address
                ld      A,L             ; move Y into A (we don't need X anymore)
                ld      (SCR_CURS_Y),A  ; store new Y
                ;---    final part: go at the beginning of a new line on the screen
PRNTRETURN:     xor     A               ; move to col 0
                ld      (SCR_CURS_X),A  ; store new X
                ld      A,(SCR_CURS_Y)  ; load cursor Y into A
                inc     A               ; new row
                ld      HL,SCR_SIZE_H   ; load address of cell that keeps screen height
                cp      (HL)            ; is the cursor over the bottom of the screen?
                jr      C,ADDNEWLINE    ; no, jump over
                dec     A               ; yes, so come back 1 row, then...
                push    AF              ; (store A)
                call    SCROLLUP        ; ...scroll the screen before to...
                pop     AF              ; (retrieve A)
ADDNEWLINE:     ld      (SCR_CURS_Y),A  ; ...store new Y
                ld      A,(TMPBFR4)     ; retrieve cursor state
                and     A               ; was it off (A=0)?
                call    NZ,CURSOR_ON    ; no, set cursor on
                jp      POS_CURSOR      ; position cursor to new location & return to caller

; ------------------------------------------------------------------------------
                ; this table contains the values of the offsets to be added to
                ; the starting address of the name table to find the correct
                ; value of the first cell of the corresponding row
                ; (by doing so, it's faster than doing a multipication)
                ; table for graphics 1 text mode: 32 cols
POS_TB_CRS_32   defw    $0000,$0020,$0040,$0060,$0080,$00A0,$00C0,$00E0
                defw    $0100,$0120,$0140,$0160,$0180,$01A0,$01C0,$01E0
                defw    $0200,$0220,$0240,$0260,$0280,$02A0,$02C0,$02E0
                ; table for pure text mode: 40 cols
POS_TB_CRS_40   defw    $0000,$0028,$0050,$0078,$00A0,$00C8,$00F0,$0118
                defw    $0140,$0168,$0190,$01B8,$01E0,$0208,$0230,$0258
                defw    $0280,$02A8,$02D0,$02F8,$0320,$0348,$0370,$0398

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
                nop
                jr      NZ,EMPTVRM      ; repeat until page is fully cleared
                djnz    EMPTVRM         ; repeat for $40 pages
                ret                     ; return to caller

; clear video registers in SRAM
CLR_RAM_REG:    ld      HL,SCR_SIZE_W   ; address of first register
                xor     A               ; $00 to clean the registers
                ld      B,CHASNDDTN-SCR_SIZE_W; how many bytes (registers) to clean (dinamically calculated)
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
SET_GFX_MODE2:  ld      D,$00           ; reset D
                ld      HL,VDPMODESET   ; pointer to register settings | <= here points the SHOW_LOGO sub-routine
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
                ld      B,$00           ; 0=256 chars to load (complete charset)
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
VDPMODESET      defb    %00000000       ; reg.0: external video off
                defb    %11010000       ; reg.1: 16K VRAM, video on, int. off, text mode (40x24)
                defb    $02             ; reg.2: name table set to $0800 ($02x$400)
                defb    $00             ; reg.3: not used in text mode
                defb    $00             ; reg.4: pattern table set to $0000
                defb    $00             ; reg.5: not used in text mode
                defb    $00             ; reg.6: not used in text mode
                defb    $f5             ; reg.7: white text on light blue background

VDPMODESET1     ; VDP register settings for a graphics 1 mode
                defb    %00000000       ; reg.0: ext. video off
                defb    %11000000       ; reg.1: 16K Vram; video on, int off, graphics mode 1, sprite size 8x8, sprite magn. 0
                defb    $06             ; reg.2: name table address: $1800
                defb    $80             ; reg.3: color table address: $2000
                defb    $00             ; reg.4: pattern table address: $0000
                defb    $36             ; reg.5: sprite attr. table address: $1B00
                defb    $07             ; reg.6: sprite pattern table addr.: $3800
                defb    $05             ; reg.7: backdrop color (light blue)

VDPMODESET2     ; VDP register settings for a graphics 2 mode
                defb    %00000010       ; reg.0: graphics 2 mode, ext. video dis.
                defb    %11000000       ; reg.1: 16K VRAM, video on, INT off, sprite size 8x8, sprite magn. 0
                defb    $06             ; reg.2: name table addr.: $1800
                defb    $FF             ; reg.3: color table addr.: $2000
                defb    $03             ; reg.4: pattern table addr.: $0000
                defb    $36             ; reg.5: sprite attr. table addr.: $1B00
                defb    $07             ; reg.6: sprite pattern table addr.: $3800
                defb    $05             ; reg.7: backdrop color: light blue

VDPMODESETMC    ; VDP register settings for a multicolor mode
                defb    %00000000       ; reg.0: ext. video dis.
                defb    %11001011       ; reg.1: 16K VRAM, video on, INT off, multicolor mode, sprite size 8x8, sprite magn. 0
                defb    $02             ; reg.2: name table addr.: $0800
                defb    $00             ; reg.3: don't care
                defb    $00             ; reg.4: pattern table addr.: $0000
                defb    $36             ; reg.5: sprite attr. table addr.: $1B00
                defb    $07             ; reg.6: sprite pattern table addr.: $3800
                defb    $0F             ; reg.7: backdrop color (white)

VDPMODESETEX2   ; VDP register settings for an extended graphics 2 mode
                defb    %00000010       ; reg.0: graphics 2 mode, ext. video dis.
                defb    %11000000       ; reg.1: 16K VRAM, video on, INT off, sprite size 8x8, sprite magn. 0
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