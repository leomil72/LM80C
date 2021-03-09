; ------------------------------------------------------------------------------
; LM80C 64K - DOS ROUTINES - R1.01
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
; Code Revision:
; R1.0  - 20210306 - first release
; R1.01 - 2021xxxx - 
;
;------------------------------------------------------------------------------


DFSCT0      defb    "LM80C DOS",$00,DOS_VER,$00     ; disk header
DSKDIRADR:  defb    $00,$01                         ; 1st sector of directory ($0001)

DIR_STRT:   equ     TMPDBF          ; (2) start of directory
DAT_STRT:   equ     DIR_STRT+$02    ; (2) start of DATA area
DIR_SCT:    equ     DAT_STRT+$02    ; (2) sector of first free entry in the directory
NTR_NBR:    equ     DIR_SCT+$02     ; (2) number of free entry
BYT_SIZ:    equ     NTR_NBR+$02     ; (2) file size in bytes
SCT_SIZ:    equ     BYT_SIZ+$02     ; (1) file size in sectors
MSB_SCT:    equ     SCT_SIZ+$01     ; (2) MSB sector of file
LSB_SCT:    equ     MSB_SCT+$02     ; (2) LSB sector of file
RAM_PTR:    equ     LSB_SCT+$02     ; (2) pointer to RAM
TPBF1:      equ     RAM_PTR+$02     ; (2) temp. word
TPBF2:      equ     TPBF1+$02       ; (2) temp. word
TPBF3:      equ     TPBF2+$02       ; (2) temp. word
TPBF4:      equ     TPBF3+$02       ; (2) temp. word
SRTMEM:     equ     TPBF4+$02       ; (2) temp. word
ENDMEM:     equ     SRTMEM+$02      ; (2) temp. word

; *****************************************************************************
; D I S K    I N I T
; Functions: format a disk creating a fresh new file system on disk or
;            rewrite only the Master Sector
; *****************************************************************************
DSK_INIT:   ld      A,(TPBF4)       ; load type of formatting
            ld      (TMPBFR1),A     ; save onto another location for later use
            call    CLRIOBF         ; clear I/O buffer
            call    CLRDOSBF        ; clear DOS buffer
            ld 	    A,$E0   		; select CF as master, driver 0, LBA mode (bits #5-7=111) 
            out 	(CF_LBA3),A     ; send configuration
            ld      A,$EC           ; select "drive ID" command
            out     (CF_CMD),A      ; send command
            call	CF_DAT_RDY      ; wait until data is ready to be read
            call	CF_RD_CMD       ; read data and store into I/O buffer
            ld      DE,(DOSBFR)     ; address of default conf. buffer
            ld 	    HL,(IOBUFF)     ; get starting address of I/O buffer
            ld      BC,$000E        ; position of current disk size in sectors
            add     HL,BC           ; set into HL
            ld      C,$04           ; 4 bytes to copy
            ldir                    ; copy (DE is auto-incremented)
            ld 	    HL,(IOBUFF)     ; get starting address of I/O buffer
            ld      BC,$0002        ; 2 bytes to copy and also address of number of cylinders
            add     HL,BC           ; get position of data
            ldir                    ; copy (DE is auto-incremented)
            ld 	    HL,(IOBUFF)     ; get starting address of I/O buffer
            ld      BC,$000C        ; address of number of sectors per cylinder
            add     HL,BC           ; get position of data
            ld      C,$02           ; 2 bytes to copy
            ldir                    ; copy (DE is auto-incremented)
            ld 	    HL,(IOBUFF)     ; get starting address of I/O buffer
            ld      BC,$0006        ; address of number of heads
            add     HL,BC           ; get position of data
            ld      C,$02           ; 2 bytes to copy
            ldir                    ; copy (DE is auto-incremented)
            ; now we calculate the # of files allowed (1 file = 1 block = 64K)
            ld      (TPBF4),DE      ; store current pointer to temp. def. conf. buffer
            ld      HL,(DOSBFR)     ; load first 2 BYTES
            ld      C,(HL)          ; load into AC
            inc     HL
            ld      A,(HL)
            inc     HL
            ld      E,(HL)          ; load into DE
            inc     HL
            ld      D,(HL)
            push    DE              ; move DE into IX
            pop     IX
            ld      DE,$0080        ; 128 sectors per block
            call    DIV_32_16       ; execute ACIX/DE; result is into ACIX, remainder into HL
            ld      B,A             ; now result is into BCIX
            or      C               ; BC=$0000?
            jr      Z,DOS_FTC       ; yes, but.....
            ld      DE,$FFFF        ; ... no more than $FFFF files, so set limit
            jr      DOS_FT1         ; jump over
DOS_FTC:    push    IX              ; move IX into DE
            pop     DE              ; now result is into BCDE
            ld      A,H             ; remainder = zero?
            or      L
            jp      Z,DOS_FT1       ; yes, jump over
            inc     DE              ; no, increment DE
            ld      A,D             ; check if DE is zero
            or      E
            jr      NZ,DOS_FT1      ; no, jump over
DOS_FT0:    dec     DE              ; yes, so set files to limit of $FFFF
DOS_FT1:    ld      HL,(TPBF4)      ; retrieve current pointer to temp. def. conf. buffer
            ld      (HL),E          ; store # of entries
            inc     HL
            ld      (HL),D
            inc     HL
            push    DE              ; store entries
            ex      DE,HL           ; copy current pointer into DE
            ld      HL,DSKDIRADR    ; address of directory start
            ld      BC,$0002        ; 2 bytes to copy
            ldir                    ; copy into buffer
            ; now we calculate the starting sector of data area
            pop     HL              ; entries into HL
            ld      C,$10           ; 16 entries per sector
            call    DIV_16_8        ; calculare how many sectors for dir (HL/C)
            or      A               ; remainder = 0?
            jr      Z,DOS_FT2       ; yes, jump over
            inc     HL              ; increment sectors
DOS_FT2:    push    HL              ; store size of directory
            pop     IY              ; into IY
            inc     HL              ; data area is 1 sector bigger than directory's size
            ex      DE,HL           ; restore pointer into HL, and move sectors into DE
            ld      (HL),E          ; store starting of 
            inc     HL              ; data area
            ld      (HL),D          ; into buffer
            ; now clean again the I/O buffer and copy the configuration into I/O buffer
            call    CLRIOBF         ; clear I/O buffer
            ld      HL,DFSCT0       ; address of disk header string
            ld      DE,(IOBUFF)     ; address of I/O buffer
            ld      BC,$000F        ; 15 chars
            ldir                    ; copy header into buffer
            ld      HL,(DOSBFR)     ; first part of configuration
            ld      BC,$0010        ; composed by 16 chars
            ldir                    ; copy (DE is auto-incremented)
            xor     A               ; A=$0
            ld      (DE),A          ; marker at $1F
            inc     DE
            call    CHKNMVAL        ; copy disk name into buffer
            jp      C,D1ERR         ; disk name error
DOS_FT5:    call    RND_ID          ; generate a semi-random disk ID
            ld      HL,(IOBUFF)     ; get starting address of I/O buffer
            ld      D,H             ; copy into DE...
            ld      E,L             ; ...for later use
            ld      BC,$01FE        ; get address of last 2 bytes...
            add     HL,BC           ; ...of the I/O buffer
            ld      A,'8'           ; write marker "80"
            ld      (HL),A          ; ...
            inc     HL              ; ...
            ld      A,'0'           ; ...
            ld      (HL),A          ; ...into last 2 locations
            ex      DE,HL           ; copy beginning of I/O buffer from DE into HL
            xor     A               ; reset A - set sector # to $00000000
            ld      B,A             ; LBA0=0
            ld      C,A             ; LBA1=0
            ld      D,A             ; LBA2=0
            ld      E,A             ; LBA3=0
            call    CF_WR_SEC       ; write sector to CF
            jp      C,D2ERR
            ; check if only re-writing of Master Sector was need
            ld      A,(TMPBFR1)     ; retrieve type of formatting
            dec     A               ; A=1?
            ret     Z               ; yes, finished job
            ; erase directory -----------------------------
            ; create a progress bar
            ld      A,CR            ; new line
            call    OUTC
            push    IY              ; copy directory's size
            pop     HL              ; into HL
            ld      BC,$0018        ; 24 steps
            call    DIV_16_8        ; calculate HL/24 (remainder is ignored, here)
            push    HL              ; store result
            pop     IX              ; into IX
            ex      DE,HL           ; and into DE
            ld      A,'-'           ; print a progress bar
            call    DOS_FT7         ; print it
            ld      A,CRSLFT        ; CURSOR left
            call    DOS_FT7         ; come back to beginning of line
            push    IY              ; copy directory's size
            pop     HL              ; into HL
            ld      BC,$0001        ; first sector of directory
            call    CLRIOBF         ; clear I/O buffer
DOS_FTA:    push    DE              ; store counter
            ld      DE,$0000        ; reset MSB of sector pointer
            call    CF_WR_SEC       ; write sector to CF
            jp      C,DOS_FT10      ; error occured
            inc     BC              ; next sector
            dec     HL              ; decrement HL
            pop     DE              ; retrieve counter
            dec     DE              ; decrement counter
            ld      A,E
            or      D               ; check if counter is zero
            jr      NZ,DOS_FT9      ; no, jump over
            ld      A,'*'           ; yes, print char
            call    OUTC
            push    IX              ; re-set counter
            pop     DE
DOS_FT9:    ld      A,H             ; check if...
            or      L               ; ...HL = 0
            jr      NZ,DOS_FTA      ; if not, repeat
            ld      A,CR            ; return
            call    OUTC            ; next line
            call    CF_STANDBY      ; set CF into stand-by mode
            ret
DOS_FT10:   pop     DE
            jp      D2ERR           ; disk geometry error

; print 24 chars
DOS_FT7:    ld      B,$18           ; 24 chars
DOS_FT8:    call    OUTC            ; move cursor
            djnz    DOS_FT8         ; repeat
            ret                     ; return to caller


; *****************************************************************************
; D I S K    R E N A M E
;******************************************************************************
DSK_RNM:    call    CLRIOBF         ; clear I/O buffer
            call    CLRDOSBF        ; clear DOS buf
            ld      DE,$0000        ; MSB sector
            ld      B,D             ; LSB sector
            ld      C,D
            call    CF_SETSTR       ; set sector
            call    CF_RD_SEC       ; read sector
            ld      HL,(IOBUFF)     ; point to start of I/O buffer
            ld      BC,$0020        ; offset for disk name
            add     HL,BC           ; get pointer
            ex      DE,HL           ; copy pointer into DE
            call    CHKNMVAL        ; copy disk name into buffer
            jp      C,D1ERR         ; disk name error
            ld      DE,$0000        ; reset MSB sector
            ld      B,D             ; reset LSB sector
            ld      C,D
            call    CF_WR_SEC       ; write sector
            jp      C,WRT_ERR       ; error?
            ret                     ; no, return to caller


; *****************************************************************************
; DOS ERRORS
; *****************************************************************************
D2ERR:      ld      A,D2            ; disk geometry error
            jr      RET_ERR
DSKFULL:    ld      A,D5            ; disk full error
            jr      RET_ERR
DUPLERR:    ld      A,D6            ; duplicate file name
            jr      RET_ERR
NAMERR:     ld      A,D1            ; file name error
            jr      RET_ERR
FLNTFND:    ld      A,D8            ; file not found
            jr      RET_ERR
LODERR:     ld      A,D4            ; generic load error
            jr      RET_ERR
DOSVERSERR: ld      A,D7            ; DOS version error
            jp      RET_ERR
D1ERR:      ld      A,D1            ; name string error
RET_ERR:    ld      (DOSER),A       ; store DOS error
            call    CF_STANDBY      ; set CF into stand-by
            scf                     ; set Carry for error
            ret                     ; return to caller

; *****************************************************************************
; L I S T    F I L E S
; Function: print details of disk and list files
; Input: D: $00=only disk details; $01=file list, too
; *****************************************************************************
DSKNMTX:    defb    "Disk name: ",0
TLSCTTX:    defb    CR,"Sectors: ",0
ALFLSTXT:   defb    CR,"Allowed files: ",0
TLFLSTX:    defb    " file(s)",CR,0
LST_FILES:  call    CHKDSKVAL       ; check DOS version
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            push    DE              ; store D
            ld      BC,$0000        ; point to sector #0
            ld      DE,$0000
            call    CF_SETSTR       ; set sector
            call    CF_RD_SEC       ; read sector
            ld      HL,DSKNMTX      ; pointer to "Disk name" message
            call    PRS             ; print it
            ld 	    HL,(IOBUFF)     ; get starting address of I/O buffer
            ld      DE,$0020        ; position of disk name
            add     HL,DE           ; get address
            ld      B,$10           ; 16 chars
INPR1:      ld      A,(HL)          ; load char
            call    OUTC            ; print it
            inc     HL        
            djnz    INPR1           ; repeat
            ld      A,CR
            call    OUTC            ; carriage return
            pop     DE              ; retrieve D
            ld      IX,$0000        ; reset file counter
            ld      A,D
            or      A               ; is D=0?
            jp      Z,PNTSTATS      ; yes, jump over
            ; print list of files
            ld      HL,(IOBUFF)     ; start of I/O buffer
            ld      DE,$000F        ; point to details of disk
            add     HL,DE           ; find address
            ld      DE,(DOSBFR)     ; store into DOS buffer
            ld      BC,$0020        ; 32 bytes
            ldir                    ; copy
            ld      DE,$0000        ; beginning...
            ld      BC,$0001        ; ...of directory
LSTFILES1:  call    CF_SETSTR       ; set sector to read
            call    CF_RD_SEC       ; read sector
LSTFILES2:  ld      L,$10           ; 16 entries to check
            ld      IY,(IOBUFF)     ; beginning of I/O buffer
LSTFILES4:  ld      A,(IY)          ; load 1st char of entry name
            or      A               ; is it an empty entry ($00)?
            jr      Z,LSTFILES6     ; yes, ignore it
            cp      $7F             ; is it a deleted entry ($7F)?
            jr      Z,LSTFILES6     ; yes, ignore it
            push    BC              ; this is a valid entry - so, store BC
            push    IY              ; store IY
            push    HL              ; store HL
            push    DE              ; store DE
            push    IX              ; store IX
            ld      B,$10           ; 16 chars to read and print
LSTFILES3:  ld      A,(IY)          ; load char from name
            call    OUTC            ; print char
            inc     IY              ; next char
            djnz    LSTFILES3       ; repeat
            ld      A,SPC
            call    OUTC            ; print space
            ld      A,(IY)          ; file type
            sub     $80             ; types start from $80
            or      A               ; BAS type ($00)?
            jr      NZ,LSTFILES20   ; no, jump over
            ld      HL,FILETP       ; print "BAS"
            jr      LSTFILESPR
LSTFILES20: dec     A               ; BIN type ($01)?
            jr      NZ,LSTFILES21   ; no, jump over
            ld      HL,FILETP+5     ; print "BIN "
            jr      LSTFILESPR
LSTFILES21: ld      HL,FILETP+10    ; print "??? "
LSTFILESPR: call    PRS
            ld      DE,$0000        ; MSB = $0000
            ld      BC,$0008
            add     IY,BC           ; point to file size in bytes
            ld      C,(IY)          ; load size in BC, first LSB
            inc     IY
            ld      B,(IY)          ; then MSB
            push    BC              ; copy...
            pop     IX              ; ...into IX
            call    PRN16ASCIX      ; print size in bytes (DEIX)
            ld      A,CR
            call    OUTC            ; print carriage return
LSTFILES8:  pop     IX
            pop     DE
            pop     HL
            pop     IY              ; retrieve IY
            pop     BC              ; retrieve BC
            inc     IX              ; increment file counter
LSTFILES6:  call    TSTBRK          ; Test for break key
            call    TSTSPC          ; test for space
            push    BC              ; store sector counter
            ld      BC,$0020        ; dir entry size
            add     IY,BC           ; next entry in current sector
            pop     BC              ; retrieve sector counter
            dec     L               ; decrement entry counter
            jr      NZ,LSTFILES4    ; all entries for this sector done? no, repeat
LSTFILES5:  inc     BC              ; increment sector counter
            ld      A,B             ; check if BC=$0000
            or      C               ; (means more than $FFFF entries, so counter overflowed)
            jr      Z,LSTFILES7     ; yes, exit
            push    DE              ; store MSB of sector pointer
            ld      HL,(DOSBFR)     ; load data area sector from DOS buffer
            ld      DE,$000E        ; point to data area
            add     HL,DE           ; find address in memory
            ld      E,(HL)          ; load data area sector...
            inc     HL
            ld      D,(HL)          ; ...into DE
            ld      H,B             ; copy current sector
            ld      L,C             ; into HL
            call    CMP16           ; is current sector < data area sector?
            pop     DE              ; retrieve MSB of sector pointer
            jp      C,LSTFILES1     ; repeat if there are still directory sectors to check
LSTFILES7:  ld      DE,$0000
            push    IX
            call    PRN16ASCIX      ; print number of files from DEIX
            ld      HL,TLFLSTX
            call    PRS             ; print "file(s)"
            pop     IX
PNTSTATS:   ld      HL,TLSCTTX      ; Point to message "Tot. sectors"
            call    PRS             ; print message
            ld      HL,(DOSBFR)     ; reload address of I/O buffer and point to disk size
            push    IX              ; store number of entries
            call    PRN32ASCII      ; print size
            ld      A,'/'
            call    OUTC            ; print a "/""
            pop     DE              ; copy number of entries into DE
            ld      BC,$0080        ; 128 sectors per entry block
            call    MUL_U32         ; multiply BC times DE: returns DEHL
            ld      (TPBF1),DE      ; store results
            ld      (TPBF2),HL
            ld      HL,TPBF1        ; print results
            call    PRN32ASCII
            ld      HL,ALFLSTXT     ; Point to message "Tot. blocks"
            call    PRS             ; print message
            ld      HL,(DOSBFR)     ; reload address of I/O buffer
            ld      BC,$000A        ; address of allowed files
            add     HL,BC           ; find pointer
            ld      DE,$0000        ; MSB set to $0000
            call    PRN16ASCII      ; print max files
            ld      A,CR
            call    OUTC            ; print a carriage return
EXITFLS:    call    CF_STANDBY      ; put CF into standby
            xor     A               ; clear Carry flag
            ret                     ; return to caller
FILETP:     defb    "BAS ",0        ; BASIC type
            defb    "BIN ",0        ; BINARY type
            defb    "??? ",0        ; unkown


; *****************************************************************************
; S A V E    F I L E
; save current BASIC program onto a file
; *****************************************************************************
SAVFILE:    call    CLRIOBF         ; clear I/O
            call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            ld      HL,(IOBUFF)     ; start of I/O buffer
            ld      BC,$0019        ; point to max. allowed entries
            add     HL,BC           ; get address
            call    HL2DE           ; get entries into DE
            ld      (NTR_NBR),DE    ; store
            call    HL2DE           ; get start of directory into DE
            ld      (DIR_STRT),DE   ; store
            call    HL2DE           ; get start of data area into DE
            ld      (DAT_STRT),DE   ; store
            call    CLRIOBF         ; clear IO buffer
            call    CLRDOSBF        ; clear DOS buffer
            ld      DE,(DOSBFR)     ; DE set to beginning of DOS buffer
            call    CHKNMVAL        ; check and copy file name
            jp      C,NAMERR        ; if Carry, file name error
            call    FILE_EXIST      ; check for name already present in dir
            jp      C,DUPLERR       ; name is present - error
            call    FNDFREDIR       ; find a free entry in the directory
            jp      C,DSKFULL       ; no entry, disk full error
            ld      A,(TPBF4)       ; check what to save
            cp      $80             ; BASIC program?
            jp      NZ,SAVFL3       ; no, jump over
            ; BASIC area to save starts at PROGND and ends at (PROGND)
            ld      HL,(PROGND)     ; load end of BASIC program <- WARNING: pay attention to (PROGND) and PROGND
            ld      DE,PROGND       ; start of RAM to be saved  <- WARNING: pay attention to (PROGND) and PROGND
            ld      (SRTMEM),DE     ; copy DE
            jr      SAVFL1
SAVFL3:     ld      HL,(ENDMEM)     ; save a portion of memory: HL <= end of portion
            inc     HL              ; we always need 1 more byte to store the portion of memory
            ld      DE,(SRTMEM)     ; DE <= start of portion
            ; compute how many sectors/blocks are needed to save file
SAVFL1:     xor     A               ; reset Carry
            sbc     HL,DE           ; get how many bytes to save
            ld      (BYT_SIZ),HL    ; store into memory
            ld      DE,$0200        ; 512 bytes per sector
            ld      A,H             ; move lenght...
            ld      C,L             ; ...into AC
            call    DIV_16_16       ; lenght/512 = nbr. of sectors: quotient in AC but A will be discarded
            ld      A,H             ; check if...
            or      L               ; ...remainder (HL) is 0
            jr      Z,SAVFL6        ; yes, jump over
            inc     BC              ; no, so we need another sector
SAVFL6:     ld      A,C
            ld      (SCT_SIZ),A     ; store into memory
SAVFL7:     ; start saving on disk
            ld      DE,$0000        ; directory is always from sector 0000-0001
            ld      BC,(DIR_SCT)    ; load sector of dir where to save file entry
            call    CF_SETSTR       ; set sector to read
            call    CF_RD_SEC       ; read sector
            ld      DE,(NTR_NBR)    ; load entry number into E (D is $00)
            ld      A,E             ; load entry LSB into A
            and     %00001111       ; be sure to get only low nibble
            add     A,A             ; multiply times 32
            add     A,A
            add     A,A
            add     A,A
            add     A,A
            ld      C,A             ; copy into C
            ld      B,$00           ; reset B
            ; set name
            ld 	    HL,(IOBUFF)     ; get starting address of I/O buffer
            add     HL,BC           ; add offset to get address of entry
            ld      DE,(DOSBFR)     ; pointer to file name
            ld      BC,$0010        ; 16 chars
            ex      DE,HL           ; move source into HL and destination into DE
            ldir                    ; copy file name
            ex      DE,HL           ; move current buffer pointer into HL
            ld      A,(TPBF4)       ; load the type of file
            ld      (HL),A          ; store it
            inc     HL
            xor     A               ; file attributes - AT THE MOMENT, NO ATTRIBUTES
            ld      (HL),A
            inc     HL
            ld      DE,(NTR_NBR)    ; reload entry number
            call    DE2HL           ; store entry into buffer
            push    HL              ; store pointer
            ld      BC,$0080        ; 128 sectors per entry block
            call    MUL_U32         ; multiply BC times DE: returns DEHL
            ld      BC,(DAT_STRT)   ; load start of data
            add     HL,BC           ; add sector of data area
            jr      NC,SAVFL9       ; overflow?
            inc     DE              ; yes, increment DE
SAVFL9:     ld      (MSB_SCT),DE    ; store MSB of sector
            ld      (LSB_SCT),HL    ; store LSB of sector
            pop     HL              ; retrieve pointer
            call    DE2HL           ; also copy MSB of sector into entry
            ld      DE,(LSB_SCT)    ; retrieve LSB
            call    DE2HL           ; also copy LSB of sector into entry
            ld      DE,(BYT_SIZ)    ; retrieve file size in bytes
            call    DE2HL           ; copy file size into buffer
            ld      A,(SCT_SIZ)     ; retrieve file size in sectors
            ld      (HL),A          ; copy into buffer
            inc     HL              ; next location
            ld      DE,(SRTMEM)     ; start of address of file in RAM
            call    DE2HL           ; copy into buffer
            ld      B,$03           ; remaining chars...
            xor     A               ; ...set to $00
SAVFL2:     ld      (HL),A          ; reset byte
            inc     HL              ; next cell
            djnz    SAVFL2          ; repeat
            ; save entry into directory
            ld      DE,$0000        ; block 0
            ld      BC,(DIR_SCT)    ; load sector of dir where to save file entry
            call    CF_WR_SEC       ; write new entry into directory
            jp      C,D2ERR         ; error occured
            call    CLRIOBF         ; clear I/O buffer
            ; start saving RAM
            ld      DE,(SRTMEM)     ; load start of RAM
            ld      (RAM_PTR),DE    ; store
SAVFL10:    call    CLRIOBF         ; clear I/O buffer
            ld      HL,(BYT_SIZ)    ; load bytes left to be copied
            ld      DE,$0200        ; are they < 512?
            call    CMP16
            jr      NC,SAVFL4       ; no, jump over
            ld      DE,(BYT_SIZ)    ; yes, so load remaining bytes
SAVFL4:     ld      C,E             ; bytes to copy
            ld      B,D             ; into BC
            ld      HL,(RAM_PTR)    ; load pointer to RAM to be saved
            ld      DE,(IOBUFF)     ; load start of I/O buffer
            ldir                    ; copy data
            ld      DE,(MSB_SCT)    ; load MSB of sector
            ld      BC,(LSB_SCT)    ; load LSB of sector   
            ;call    CF_SETSTR       ; set sector
            call    CF_WR_SEC       ; write sector
            jp      C,WRT_ERR
            ld      HL,SCT_SIZ      ; sector counter
            dec     (HL)            ; copied all the sectors?
            jp      Z,SAVFLEXT      ; yes, exit
SAVFL11:    ld      HL,(RAM_PTR)    ; pointer to RAM
            ld      DE,$0200        ; block of 512 bytes copied
            add     HL,DE           ; calculate next starting address
            ld      (RAM_PTR),HL    ; store next block
            xor     A               ; reset Carry
            ld      HL,(BYT_SIZ)    ; load left bytes
            sbc     HL,DE           ; subtract copied bytes
            ld      (BYT_SIZ),HL    ; store left bytes
            ld      BC,(LSB_SCT)    ; load LSB of sector
            inc     BC              ; next sector
            ld      A,B             ; BC=$0000?
            or      C
            jr      NZ,SAVFL12      ; no, jump over
            ld      DE,(MSB_SCT)    ; load MSB of sector
            inc     DE              ; increment DE
            ld      (MSB_SCT),DE    ; store new MSB of sector
SAVFL12:    ld      (LSB_SCT),BC    ; store new LSB of sector
            jp      SAVFL10         ; repeat
SAVFLEXT:   call    CF_STANDBY      ; set CF into stand-by mode
            xor     A               ; clear Carry flag
            ret                     ; return to caller


; *****************************************************************************
; F I L E    L O A D
; load a file from the disk into the memory
; *****************************************************************************
LODFILE:    call    CLRIOBF         ; clear I/O
            call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            ld      HL,(IOBUFF)     ; start of I/O buffer
            ld      BC,$0019        ; point to max. allowed entries
            add     HL,BC           ; get address
            call    HL2DE           ; get entries into DE
            ld      (NTR_NBR),DE    ; store
            call    CLRIOBF         ; clear IO buffer
            call    CLRDOSBF        ; clear DOS buffer
            ld      DE,(DOSBFR)     ; DE set to beginning of DOS buffer
            call    CHKNMVAL        ; check and copy file name
            jp      C,NAMERR        ; if Carry, file name error
            call    FILE_EXIST      ; check if file exists
            jp      NC,FLNTFND      ; file not found - error
            push    IX              ; copy pointer to file...
            pop     HL              ; ...into HL
            ld      BC,$0010        ; skip file name (16 chars)...
            add     HL,BC           ; ...by getting pointer to file details
            ld      A,(HL)          ; load file type
            cp      $80             ; 'BAS' type?
            jr      Z,LDFL4         ; yes, jump over
            cp      $81             ; 'BIN' type?
            jp      NZ,LODERR       ; no, raise error
            ld      A,(TPBF4)       ; if BIN file, file must be loaded into its original position
            or      A               ; did the user set this?
            jp      Z,LODERR        ; no, raise an error
LDFL4:      ld      C,$04           ; 4 steps forward and also 4 bytes to copy
            add     HL,BC           ; point to starting sector
            ld      DE,MSB_SCT      ; point to store sector address of file
            ldir                    ; copy MSB/LSB from entry into buffer
            ld      DE,BYT_SIZ      ; point to store size of file
            ld      BC,$0003        ; 3 bytes to copy
            ldir                    ; copy size in bytes and sectors from entry into buffer
            ld      DE,PROGND       ; load start of BASIC RAM
            ld      A,(TPBF4)       ; check where to save the data from
            or      A               ; is it 0? (meaning from the current BASIC pointers)
            jr      Z,LDFL1         ; yes, don't re-load the file from the address stored into the file
            ld      E,(HL)          ; no, load RAM address from disk - LSB...
            inc     HL
            ld      D,(HL)          ; ...then MSB
LDFL1:      ld      (RAM_PTR),DE    ; store starting pointer
            ; start loading from disk
LDFL2:      call    CLRIOBF         ; clear I/O buffer
            ld      DE,(MSB_SCT)    ; load MSB of sector
            ld      BC,(LSB_SCT)    ; load LSB of sector   
            call    CF_SETSTR       ; set sector
            call    CF_RD_SEC       ; read sector
            ld      HL,(BYT_SIZ)    ; load bytes left to be copied
            ld      DE,$0200        ; are they < 512?
            call    CMP16
            jr      NC,LDFL3        ; no, jump over
            ld      DE,(BYT_SIZ)    ; yes, so load only remaining bytes
LDFL3:      ld      C,E             ; move bytes to copy...
            ld      B,D             ; ...into BC
            ld      HL,(IOBUFF)     ; point to I/O buffer
            ld      DE,(RAM_PTR)    ; point to RAM where to save
            ldir                    ; copy data
            ld      HL,SCT_SIZ      ; sector counter
            dec     (HL)            ; copied all the sectors?
            jp      Z,LDFLEX        ; yes, exit
            ld      HL,(RAM_PTR)    ; pointer to RAM
            ld      DE,$0200        ; block of 512 bytes copied
            add     HL,DE           ; calculate next starting address
            ld      (RAM_PTR),HL    ; store next block
            xor     A               ; reset Carry
            ld      HL,(BYT_SIZ)    ; load left bytes
            sbc     HL,DE           ; subtract copied bytes
            ld      (BYT_SIZ),HL    ; store left bytes
            ld      BC,(LSB_SCT)    ; load LSB of sector
            inc     BC              ; next sector
            ld      A,B             ; BC=$0000?
            or      C
            jr      NZ,LDFL5        ; no, jump over
            ld      DE,(MSB_SCT)    ; load MSB of sector
            inc     DE              ; increment DE
            ld      (MSB_SCT),DE    ; store new MSB of sector
LDFL5:      ld      (LSB_SCT),BC    ; store new LSB of sector
            jp      LDFL2           ; repeat
LDFLEX:     call    CF_STANDBY      ; set CF into stand-by mode
            xor     A               ; clear Carry flag
            ret                     ; return to caller


; *****************************************************************************
; F I L E    E R A S E
; erase a file from disk, freeing its block
; *****************************************************************************
FIL_ERASE:  call    CLRIOBF         ; clear I/O
            call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            ld      HL,(IOBUFF)     ; start of I/O buffer
            ld      BC,$0019        ; point to max. allowed entries
            add     HL,BC           ; get address
            call    HL2DE           ; get entries into DE
            ld      (NTR_NBR),DE    ; store
            call    CLRIOBF         ; clear IO buffer
            call    CLRDOSBF        ; clear DOS buffer
            ld      DE,(DOSBFR)     ; DE set to beginning of DOS buffer
            call    CHKNMVAL        ; check and copy file name
            jp      C,NAMERR        ; if Carry, file name error
            call    FILE_EXIST      ; check if file exists
            jp      NC,FLNTFND      ; file not found - error
            ld      E,(IX+$14)      ; load MSB into DE
            ld      D,(IX+$15)
            ld      C,(IX+$16)      ; load LSB into BC
            ld      B,(IX+$17)
            ld      (MSB_SCT),DE    ; store DE
            ld      (LSB_SCT),BC    ; store BC
            ld      A,(IX+$1A)      ; load size in sectors
            ld      (SCT_SIZ),A     ; store it
            ld      A,$7F           ; marker for file deleted
            ld      (IX),A          ; set file as deleted (quick erase)
            ld      A,(TPBF4)       ; check for quick or full delete
            or      A               ; A=0 quick erase
            jr      Z,FIL_ERA5      ; jump if quick erase
            xor     A               ; clear A
            ld      B,$20           ; full erase - clean entry (32 cells)
FIL_ERA6:   ld      (IX),A          ; reset cell
            inc     IX              ; next cell
            djnz    FIL_ERA6        ; repeat
FIL_ERA5:   ld      DE,(TPBF2)      ; retrieve MSB of current directory sector
            ld      BC,(TPBF3)      ; retrieve LSB of current directory sector
            call    CF_WR_SEC       ; write sector
            jr      C,WRT_ERR
            ld      A,(TPBF4)       ; check for quick or full delete
            or      A               ; if A=0 then quick erase
            jr      Z,FIL_ERA3      ; yes, leave
            call    CLRIOBF         ; no, full delete - clear I/O buffer
            ld      DE,(MSB_SCT)    ; load MSB of 1st sector of file block
            ld      BC,(LSB_SCT)    ; load LSB of 1st sector of file block
            ld      HL,SCT_SIZ      ; pointer to size in sectors
FIL_ERA1:   call    CF_WR_SEC       ; erase sector
            jr      C,WRT_ERR
            inc     BC              ; next sector
            ld      A,B             ; is LSB rolled back to 0?
            or      C
            jr      NZ,FIL_ERA2     ; no, jump over
            inc     DE              ; yes, increment MSB
FIL_ERA2:   dec     (HL)            ; decrement sector counter
            jr      NZ,FIL_ERA1     ; repeat if other sectors
FIL_ERA3:   call    CF_STANDBY      ; set CF into stand-by mode
            xor     A               ; clear Carry
            ret
WRT_ERR:    call    CF_STANDBY
            scf
            ret


; *****************************************************************************
; F I L E    C H E C K
; check if file name already exists in directory
; file name must be stored from (DOSBFR) for 16 chars
; *****************************************************************************
FILE_EXIST: ld      DE,$0000        ; beginning...
            ld      BC,$0001        ; ...of directory
            ld      IY,(NTR_NBR)    ; load max entries
CHKSTNM1:   call    CF_SETSTR       ; set sector to read
            call    CF_RD_SEC       ; read sector
CHKSTNM2:   ld      IX,(IOBUFF)     ; beginning of I/O buffer
            ld      L,$10           ; reset entry counter
CHKSTNM8:   ld      A,(IX)          ; load 1st char of entry name
            or      A               ; is it $00 (empty entry)?
            jp      Z,CHKSTNM10     ; yes, jump over
            cp      $7F             ; is it $7F (deleted, re-usable, entry)?
            jr      Z,CHKSTNM10     ; yes, jump over
            ld      (TPBF1),HL      ; store HL
            ld      (TPBF2),DE      ; store DE
            ld      (TPBF3),BC      ; store BC
            push    IX              ; copy IX...
            pop     HL              ; ...into HL
            ld      DE,(DOSBFR)     ; beginning of name file
            ld      B,$10           ; 16 chars to check
CHKSTNM3:   ld      A,(DE)          ; load a char from name on disk
            cpi                     ; compare with name from user
            jr      NZ,CHKSTNM6     ; chars are different, leave
            inc     DE              ; inc DE (HL is incremented by "cpi")
            djnz    CHKSTNM3        ; repeat for 16 chars
            jr      FLEXST          ; file exists in dir
CHKSTNM6:   ld      HL,(TPBF1)      ; retrieve HL
            ld      DE,(TPBF2)      ; retrieve DE
            ld      BC,(TPBF3)      ; retrieve sector counter
CHKSTNM10:  dec     IY              ; decrement entries left
            push    HL              ; preserve current entry
            push    IY              ; copy max allowed files...
            pop     HL              ; ...into HL
            ld      A,H             ; chech if entries...
            or      L               ; ...are finished
            pop     HL              ; (retrieve current entry)
            jr      Z,FLNTEXS       ; entries finished - exit
            push    BC              ; store bc
            ld      BC,$0020        ; dir entry size
            add     IX,BC           ; next entry in current sector
            pop     BC              ; retrieve BC
            dec     L               ; decrement entry counter
            jr      NZ,CHKSTNM8     ; more entries in this sector, repeat
CHKSTNM5:   inc     BC              ; entries finished, goto next sector
            ld      A,C             ; check if...
            or      B               ; BC=$0000? 
            jr      NZ,CHKSTNM7     ; no, jump over
            inc     DE              ; yes, increment DE (MSB)
CHKSTNM7:   jp      CHKSTNM1        ; repeat
FLNTEXS:    xor     A               ; file not found, reset Carry
            ret                     ; return
FLEXST:     scf                     ; file is present - set Carry flag for error
            ret


; *****************************************************************************
; FIND A FREE ENTRY
; find a free entry in the directory to store a new file
; *****************************************************************************
FNDFREDIR:  ld      BC,$0000        ; point to sector #0
            ld      DE,$0000
            call    CF_SETSTR       ; set sector
            call    CF_RD_SEC       ; read sector
            ld      HL,(IOBUFF)     ; address of I/O buffer
            ld      DE,$0019        ; pointer to number of allowed files (same of allowed blocks)
            add     HL,DE           ; find address
            ld      E,(HL)          ; load max entries into DE
            inc     HL
            ld      D,(HL)
            push    DE              ; copy number of entries...
            pop     IY              ; ...into IY
            ld      BC,$0001        ; BC=$0001 (starting sector of dir) (LSB)
            ld      D,B             ; DE=$0000 (starting sector of dir) (MSB)
            ld      E,B
            ld      H,B             ; HL=$0000 (entry counter)
            ld      L,B
FNDFREDIR1: call    CF_SETSTR       ; set sector to read (BC-DE)
            call    CF_RD_SEC       ; read sector
            ld      IX,(IOBUFF)     ; beginning of I/O buffer
FNDFREDIR2: ld      A,(IX)          ; load 1st char of entry name
            dec     IY              ; decrement number of entries
            or      A               ; is it $00 (empty entry)?
            jr      Z,FNDFREDIR3    ; yes, found an entry
            cp      $7F             ; is it $7F (deleted entry)?
            jr      Z,FNDFREDIR3    ; yes, found an entry
            push    BC              ; no, store BC
            ld      BC,$0020        ; load BC with directory entry size (32 bytes)
            add     IX,BC           ; next entry in current sector
            pop     BC              ; retrieve sector pointer
            inc     HL              ; increment entry counter
            ld      A,L
            and     %00001111       ; just done 16 entries?
            jr      NZ,FNDFREDIR2   ; no, repeat
            inc     BC              ; entries in this sector finished .. goto next sector
            ld      A,B             ; check if...
            or      C               ; ...BC=$000
            jr      NZ,FNDFREDIR5   ; no, jump over
            inc     DE              ; yes, increment DE (MSB)
FNDFREDIR5: push    HL              ; preserve current entry
            push    IY              ; copy max allowed files...
            pop     DE              ; ...into HL
            call    CMP16           ; check if reached max allowed entries
            pop     HL              ; (retrieve current entry)
            jr      NC,FNDFREDIR1   ; no, there are other entries
            scf                     ; entries are finished, raise an error
            ret                     ; return
FNDFREDIR3: ld      (DIR_SCT),BC    ; store sector of dir
            ld      (NTR_NBR),HL    ; store entry number
            xor     A               ; reset Carry
            ret


; *****************************************************************************
;   U T I L I T I E S
; *****************************************************************************

; generate random disk ID
RND_ID:     ld      HL,(TPBF4)      ; retrieve pointer to beginning of name
            ld      A,(HL)          ; get first char
            ld      (DE),A          ; copy it to disk ID
            inc     DE              ; inc pointer
            call    RND8            ; get a random char
            call    CH0TO9          ; get a number from 0 to 9
            ld      (DE),A          ; store it
            inc     DE              ; inc pointer
            inc     HL              ; next disk name char
            ld      A,(HL)          ; get second char from disk name
            ld      (DE),A          ; store it
            inc     DE              ; inc pointer
            call    RND8            ; get a random char
            call    CH0TO9          ; get a number from 0 to 9
            ld      (DE),A          ; store it
            inc     DE              ; inc pointer
            ret                     ; return to caller


; generate a pseudo-random number from TMR and R register
RND8:       ld      A,(TMRCNT)      ; load LSB of sys-timer
            ld      B,A             ; copy into B
            ld      A,R             ; load refresh register
            xor     B               ; A xor B
            ld      B,A             ; copy into B
            rrca                    ; multiply by 32
            rrca
            rrca
            xor     $1F
            add     A,B
            sbc     A,255           ; carry
            ret                     ; return to caller


; char ported into 0-9 interval
CH0TO9:     and     %00001111       ; get only low nibble
            cp      $0A             ; is it < 10?
            jr      C,CH0TO9E       ; yes, jump over
            sub     $0A             ; subract 10
CH0TO9E:    add     $30             ; get a number from 0 to 9
            ret


; first check DOS validity then load disk details
CHKDSKVAL:  call    CF_INIT         ; open CF card comm.
            ret     C               ; if errors, leave
            push    BC              ; store BC
            push    DE              ; store DE
            push    HL              ; store HL
            ld      BC,$0000        ; reset LSB of sector
            ld      D,B             ; reset MSB of sector
            ld      E,B
            call    CF_SETSTR       ; set sector #0
            call    CF_RD_SEC       ; read sector
            ld      HL,(IOBUFF)     ; address of default conf. buffer
            ld      DE,$000A        ; point to disk DOS version
            add     HL,DE
            ld      DE,DFSCT0+10    ; get starting address of I/O buffer
            ld      B,$04           ; 4 chars
CHKDSKVAL1: ld      A,(DE)          ; load char from DOS version into memory
            cp      (HL)            ; compare with disk DOS version
            jr      NZ,CHKDSKVALE   ; no match - so error
            inc     DE              ; next source
            inc     HL              ; next comparison
            djnz    CHKDSKVAL1      ; repeat
            ld      HL,(DOSBFR)     ; address of default conf. buffer
            ld      DE,$001D        ; point to address of data area
            add     HL,DE           ; set pointer to beginning of identifies
            ld      E,(HL)          ; load LSB
            inc     HL              ; increment pointer
            ld      D,(HL)          ; load MSB: DE now has the first sector of data area
            xor     A               ; no error - clear Carry flag
            jr      CHKDSKVAL2      ; jump over
CHKDSKVALE: scf                     ; error - set carry flag
CHKDSKVAL2: pop     HL
            pop     DE
            pop     BC
            ret                     ; return to caller


; check name validity (only allowed chars) and copy it from string pool into a temp buff
; Ipnputs: DE: pointer to dest. buffer
; operation: copy (HL)->(DE) and pads to get a 16-chars name
; destroys: A, BC, DE, HL
CHKNMVAL:   ld      BC,(DKLNPT)     ; load lenght of file name
            ld      A,C             ; lenght is max. 16 char, so we only check C
            cp      $11             ; is it <=16?
            jr      C,CHKNMVAL1     ; yes, so jump over
            ld      C,$10           ; no, get only 16 chars
CHKNMVAL1:  ld      B,C             ; copy lenght into B
            ld      C,$10           ; char counter
            ld      HL,(DKNMPT)     ; pointer to disk name
CHKNMVAL2:  ld      A,(HL)          ; get a char from string name
            call    CHK_NAM         ; check if valid
            ret     C               ; no, file name error
            ld      (DE),A          ; yes, store char
            inc     DE              ; next I/O location
            inc     HL              ; next disk name char
            dec     C               ; decrement number of chars copied
            djnz    CHKNMVAL2       ; repeat until name ends
            ld      A,C             ; check if there are no empty chars in file
            or      A
            ret     Z               ; yes, job finished - return
            ld      A,SPC           ; no, padding required
CHKNMVAL3:  ld      (DE),A          ; store char
            inc     DE              ; next location
            dec     C               ; check if padding is over
            jr      NZ,CHKNMVAL3    ; no, continue
            xor     A               ; clear Carry flag
            ret                     ; return to caller


; store DE into (HL) and (HL+1)
DE2HL:      ld      (HL),E          ; LSB of size
            inc     HL
            ld      (HL),D          ; MSB of size
            inc     HL
            ret                     ; return to caller


; get DE from (HL) and (HL+1)
HL2DE:      ld      E,(HL)          ; get LSB into E
            inc     HL              ; next location
            ld      D,(HL)          ; get MSB into D
            inc     HL              ; next location
            ret                     ; return to caller


; convert a 16/32-bit number into an ASCII string and print it
; inputs: HL pointer to 32-bit number
PRN32ASCII: ld      E,(HL)          ; load MSB into DE  <-- entry for 32-bit
            inc     HL
            ld      D,(HL)          
            inc     HL
PRN16ASCII: ld      C,(HL)          ; load LSB into BC   <-- entry for 16-bit (set DE to $0000 before to call)
            inc     HL
            ld      B,(HL)
            push    BC              ; copy BC...
            pop     IX              ; ...into IX
PRN16ASCIX: ld      IY,TPBF1        ; number is into DEIX - now, load pointer to destination buffer
            call    CLCN32          ; convert number in DEIX into ASCII number
            ld      HL,TPBF1        ; address of ASCII number
PRNTSIZ:    ld      A,(HL)          ; get a char
            or      A               ; is it $00 (end of string)?
            ret     Z               ; yes, leave
            call    OUTC            ; no, print char
            inc     HL              ; next char
            jr      PRNTSIZ         ; repeat


; convert one or more bytes into memory in hex format
; HL: pointer to number in memory - B: number of bytes to convert
; destroys: A, D, HL
PRN_HEX:    ld      D,B             ; move bytes into D
PRN_HEX1:   ld      A,(HL)          ; load value
            call    BYTEHEX         ; print hex number
            inc     HL              ; next location
            dec     D               ; decrement bytes to convert
            jr      NZ,PRN_HEX1     ; repeat
            ret


; convert one or more words into memory in hex format
; IX: pointer to number in memory - B: number of words to convert
; destroys: AF, IX
PRN_WHEX:   push    DE
            ld      D,B
PRN_WHEX1:  ld      A,(IX+1)        ; load MSB into A
            call    BYTEHEX         ; print hex number
            ld      A,(IX)          ; load LSB into A
            call    BYTEHEX         ; print hex number
            inc     IX
            inc     IX              ; next word
            dec     D               ; decrement words to convert
            jr      NZ,PRN_WHEX1    ; repeat
            pop     DE
            ret


; print A in hex format
BYTEHEX:    push    BC
            push    DE
            call    BYT2ASC         ; convert to HEX and get back into BC
            ld      A,B
            call    OUTC            ; print left nibble
            ld      A,C
            call    OUTC            ; print right nibble
            pop     DE
            pop     BC
            ret                     ; return to caller


; check for non-allowed chars in disk/file names - allowed chars: '0'-'9' and 'A'-'Z'
; input: A -> char to check
; return: C is set if ERROR, reset otherwise
CHK_NAM:    cp      SPC             ; is it a space?
            ret     Z               ; return if equal (C is reset)
            cp      '-'             ; is it a minus?
            ret     Z               ; return if equal
            cp      '0'             ; char < '0' ?
            ret     C               ; yes, disk name error
            cp      ':'             ; is char <= '9' ?
            jr      C,CHK_C_CF      ; yes, leave
            and     %01011111       ; for letters, only UPPER CASE 
            cp      'A'             ; is char >= 'A' ?
            ret     C               ; no, error
            cp      '['             ; is char <= 'Z' ? (if yes, C=1, then C=0; otherwise, C=0 then C=1)
CHK_C_CF:   ccf                     ; Carry complement (invert Carry)
CHK_NAM_LV: ret                     ; return to caller



; clear I/O buffer
CLRIOBF:    push    AF              ; store AF
            push    BC              ; store BC
            push    HL              ; store HL
            ld 	    HL,(IOBUFF)     ; load address of last BASIC location
            ld      BC,$0002        ; B=256 iterations ($00); C=repeat 2 times
CLRBUFF:    xor     A               ; reset A
RSTIOBF:    ld      (HL),A          ; reset cell
            inc     HL              ; next cell
            djnz    RSTIOBF         ; repeat for 256 times
            dec     C               ; decrement C
            jr      NZ,RSTIOBF      ; repeat if not zero
            pop     HL              ; retrieve HL
            pop     BC              ; retrieve BC
            pop     AF              ; retrieve AF            
            ret                     ; return to caller


; clear DOS buffer
CLRDOSBF:   push    AF
            push    BC
            push    HL
            ld 	    HL,(DOSBFR)     ; load address of last BASIC location
            ld      BC,$2001        ; B=32 iterations; C=repeat 1 time
            jp      CLRBUFF         ; continue