; ------------------------------------------------------------------------------
; LM80C 64K - DOS ROUTINES - R1.05
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
; R1.01 - 20210309 - code cleaning & optimization - new behaviour for ERASE (full erase everything)
; R1.02 - 20210310 - code optimization - new UNDELETE feature for DISK statement
; R1.03 - 20210316 - code size enhancements
; R1.04 - 20210319 - code re-organization and new positioning into RAM
; R1.05 - 20210327 - added support for sequential files
;
;------------------------------------------------------------------------------


            org     $EE68

DOSSTART:   equ     $
DSKHDR      defb    "LM80C DOS",$00,DOS_VER,$00     ; disk header
DSKDIRADR:  defb    $00,$01                         ; 1st sector of directory ($0001)

DIR_STRT:   equ     TMPDBF          ; (2) start of directory
DAT_STRT:   equ     DIR_STRT+$02    ; (2) start of DATA area
DIR_SCT:    equ     DAT_STRT+$02    ; (2) sector of first free entry in the directory
NTR_NBR:    equ     DIR_SCT+$02     ; (2) number of free entry
BYT_SIZ:    equ     NTR_NBR+$02     ; (2) file size in bytes
SCT_SIZ:    equ     BYT_SIZ+$02     ; (1) file size in sectors
MSW_SCT:    equ     SCT_SIZ+$01     ; (2) MSW sector of file
LSW_SCT:    equ     MSW_SCT+$02     ; (2) LSW sector of file
RAM_PTR:    equ     LSW_SCT+$02     ; (2) pointer to RAM
TPBF1:      equ     RAM_PTR+$02     ; (2) temp. word
TPBF2:      equ     TPBF1+$02       ; (2) temp. word
TPBF3:      equ     TPBF2+$02       ; (2) temp. word
TPBF4:      equ     TPBF3+$02       ; (2) temp. word
SRTMEM:     equ     TPBF4+$02       ; (2) temp. word
ENDMEM:     equ     SRTMEM+$02      ; (2) temp. word


; **************************************************************************************
;                            B A S I C    I N T E R F A C E
;***************************************************************************************

; OPEN a sequential file
; OPEN "filename",flnm,mod = open file "filename" with #flnm for READ (mod=0) or WRITE (mod=1)
OPEN:   call    CHKFLNM         ; check if a string follows
        call    CHKSYN          ; Make sure ',' follows
        defb    ','
        call    GETINT          ; get file number
        ld      (TMPDBF),A      ; store it into a temp buffer
        call    CHKSYN          ; Make sure ',' follows
        defb    ','
        call    GETINT          ; get mode
        cp      $02             ; 0 or 1?
        jp      NC,FCERR        ; no, illegal function call
        ld      (TMPDBF+1),A    ; store it into a temp buffer
        dec     HL              ; dec 'cos GETCHR INCs
        call    GETCHR          ; check if something follows
        jp      NZ,SNERR        ; if something more, raise a syntax error
        push    HL              ; store code string pointer
        call    OPNFIL          ; call open file
        pop     HL              ; retrieve code string pointer
        jp      C,DOS_ERR       ; any error?
        ret                     ; return to caller


; CLOSE a file
CLOSE:  call    GETINT          ; look for a number
        ld      (TMPDBF),A      ; store file number
        push    HL              ; store code string pointer
        call    CLOSFIL         ; close an open file
        pop     HL              ; retrieve code string pointer
        jp      C,DOS_ERR
        ret                     ; return to caller


; LOAD "filename"[,1]  <- load a BASIC program from disk
; LOAD x,y,w,z         <- load a sector from disc, LBA address equals to ((((z*256)+w)*256)+y)*256)+x
LOAD:   call    LDSVPT          ; check what's following
        jp      Z,LOADST        ; number - load a single sector
        call    DIRMOD          ; a file can be loaded ONLY in direct mode
        jp      NZ,IMERR        ; raise error if in indirect mode
        call    CHKFN1          ; string - evaluate file name
        call    CHK1AR          ; check for ",1" argument
        ld      (TMPBFR1),HL    ; store HL
        ld      HL,TXTLDG       ; loading message
        call    PRS             ; print
        call    LODFILE         ; load file into memory
        jp      LDEND           ; retrieve registers and leave
LOADST: ld      HL,(TMPBFR1)    ; retrieve HL
        call    GET4AR          ; get 4 arguments
        ld      (TMPBFR1),HL    ; store HL
        call    CF_SETSTR       ; set sector
        call    CF_RD_SEC       ; read sector
        jr      C,LDERR         ; load error?
        call    CF_STANDBY      ; put CF in standby mode
        xor     A               ; clear Carry
LDEND:  ld      HL,(TMPBFR1)    ; retrieve HL
        jp      C,DOS_ERR       ; jump if DOS error
        ret                     ; return to caller
LDERR:  ld      E,D4            ; load error
        jp      ERROR
TXTLDG: defb    "Loading file... ",0


; common code for LOAD/SAVE to check the type of operation required
; (load/save a file or a single sector)
; returns Z if argument is a number, NZ if it's a string
LDSVPT: dec     HL              ; dec 'cos GETCHR INCs
        call    GETCHR          ; check if something follows
        jp      Z,SNERR         ; if anything else, raise a syntax error
        ld      (TMPBFR1),HL    ; save current code string pointer
        call    EVAL            ; Evaluate expression
        ld      A,(TYPE)        ; Get variable type
        or      A               ; Is it a string variable?
        ret                     ; return to caller


; shared code between LOAD and SAVE
; get 4 arguments after the command LOAD or SAVE
; and load them into appropriate registers
GET4AR: ld      HL,(TMPBFR1)    ; retrieve string code pointer
        call    GETINT          ; get 1st pointer
        ld      (TMPDBF),A      ; store it into a temp buffer
        call    CHKSYN          ; Make sure ',' follows
        defb    ','
        call    GETINT          ; get 2nd pointer
        ld      (TMPDBF+1),A    ; store it into a temp buffer
        call    CHKSYN          ; Make sure ',' follows
        defb    ','
        call    GETINT          ; get 3rd pointer
        ld      (TMPDBF+2),A    ; store it into a temp buffer
        call    CHKSYN          ; Make sure ',' follows
        defb    ','
        call    GETINT          ; get 4th pointer
        ld      D,A             ; store it
        ld      A,(TMPDBF)      ; retrieve 1st pointer
        ld      C,A
        ld      A,(TMPDBF+1)    ; retrieve 2nd pointer
        ld      B,A
        ld      A,(TMPDBF+2)    ; retrieve 3rd pointer
        ld      E,A
        call    CF_INIT         ; init CF card
        jp      C,NRERR         ; error if device not available/ready
        ret                     ; return to caller


; save a file onto the disk
; syntax: save "filename" to save a BASIC program
; save x,y,z to save IO buffer into x-y-z sector
; save "filename",start,end to save a portion of memory
SAVE:   dec     HL              ; dec 'cos GETCHR INCs
        call    GETCHR          ; check if something follows
        jp      Z,SNERR         ; if nothing else, raise a syntax error
        ld      (TMPBFR1),HL    ; save current code string pointer
        call    EVAL            ; Evaluate expression
        ld      A,(TYPE)        ; Get variable type
        or      A               ; Is it a string variable?
        jp      Z,SAVESCT       ; no - save a single sector
        call    CHKFN1          ; yes, load string name
        ld      (TMPBFR4),DE    ; store lenght into another buffer
        ld      (TPBF4+2),BC    ; store address into another buffer
        ld      A,$80           ; set BAS as file type
        ld      (TPBF4),A
        dec     HL              ; dec 'cos GETCHR INCs
        call    GETCHR          ; check if something follows
        jr      Z,SAVE1         ; no, jump over
        call    CHKSYN          ; yes - Make sure ',' follows
        defb    ','
        ld      (TMPDBF),HL     ; store code string pointer
        call    EVAL            ; check expression
        ld      HL,(TMPDBF)     ; point back to the beginning of epression
        ld      A,(TYPE)        ; check result type
        or      A               ; is it another string?
        jr      NZ,RENFIL       ; yes, jump to rename file
        call    GETNUM          ; no, this is the memory address for a BIN file
        call    DEINT           ; Get integer -32768 to 32767
        ld      (SRTMEM),DE     ; Store DE into a temp. buffer
        call    CHKSYN          ; Make sure ',' follows
        defb    ','
        call    GETNUM          ; Get memory address
        call    DEINT           ; Get integer -32768 to 32767
        ld      (ENDMEM),DE     ; Store DE into a temp. buffer
        ld      A,$81           ; set BIN for file type
        ld      (TPBF4),A
SAVE1:  call    DIRMOD          ; can be executed ONLY in direct mode
        jp      NZ,IMERR        ; raise error if in indirect mode
        ld      (TMPBFR1),HL    ; store code string pointer
        ld      HL,TXTSVG       ; loading message
        call    PRS             ; print
        call    SAVFILE         ; load file into memory
        jp      SVEND           ; retrieve registers and leave
        ;save a single sector
SAVESCT:ld      HL,(TMPDBF)     ; retrieve string code pointer
        call    GET4AR          ; get 4 arguments
        ld      (TMPBFR1),HL    ; store code string pointer
        call    CF_WR_SEC       ; read sector
        jr      C,SVERR
        call    CF_STANDBY      ; put CF in standby mode
        xor     A               ; clear Carry
SVEND:  ld      HL,(TMPBFR1)    ; retrieve code string pointer
        jp      C,DOS_ERR       ; jump if DOS error
        ret                     ; return to caller
RENFIL: call    GSTRCU          ; check that a string follows
        call    LOADFP          ; Move string block to BCDE (BC=pointer, E=length)
        ld      (BYT_SIZ),DE    ; store values into
        ld      (RAM_PTR),BC    ; temp buffers
        push    HL              ; store code string pointer
        ld      HL,TXTRNM       ; point to "renaming file"
        call    PRS             ; print string
        call    CHNGNAM         ; rename file
        pop     HL              ; retrieve code string pointer
        jp      C,DOS_ERR       ; any error?
        ret                     ; return to caller
TXTRNM: defb    "Renaming file...",0
SVERR:  ld      E,D3            ; save error
        jp      ERROR
TXTSVG: defb    "Saving file... ",0


; FILES
; list files on disk
FILES:  call    CLRIOBF         ; clear I/O buffer
        call    CF_INIT         ; init CF card
        jp      C,DOS_ERR       ; error if device not available/ready
        push    HL              ; store code string pointer
        ld      D,$01           ; print disk details and file list
        call    LST_FILES       ; list files
        pop     HL              ; retrieve code string pointer
        jp      C,DOS_ERR       ; any error?
        ret                     ; return to caller


; raise a DOS error
DOS_ERR:ld      A,(DOSER)       ; load error code
        ld      E,A             ; copy into E
        jp      ERROR           ; goto error routine


; check for ",1" argument
; check if 1 is passed as argument after LOAD and ERASE
CHK1AR: xor     A               ; reset A
        ld      (TPBF4),A       ; default setting 
                                ; (load=store file in RAM using current BASIC pointers
                                ; (erase=quick delete)
        dec     HL              ; dec 'cause GETCHR increments
        call    GETCHR          ; check if something follows
        ret     Z               ; if nothing follows, return
        call    CHKSYN          ; Make sure ',' follows
        defb    ','
        call    GETINT          ; get a value
        dec     A               ; decrement it
        jp      NZ,SNERR        ; not '1' - raise an error
        inc     A               ; A=1
        ld      (TPBF4),A       ; load=store file in RAM using address in file
                                ; erase=full delete
        ret                     ; return to caller


; ERASE "filename"
; erase a file from disk
ERASE:  call    CHKFLNM         ; check for a disk name
        call    CHK1AR          ; check for ",1" argument
        push    HL              ; store code string pointer
        ld      HL,ERSTX        ; Point to message
        call    PRS             ; print message for init confirmation
        pop     HL              ; retrieve HL
        call    CNFREQ          ; ask for confirmation
        jr      C,ABRTDS        ; if Carry set then abort
        call    CF_INIT         ; init CF card
        jp      C,DOS_ERR       ; error if device not available/ready
        push    HL              ; store code string pointer
        call    FIL_ERASE       ; deleted file
        pop     HL              ; retrieve code string pointer
        jp      C,DOS_ERR       ; leave if error
        push    HL              ; store HL
        ld      HL,ERASED       ; point to message of file deleted
        call    PRS             ; print it
        pop     HL              ; retrieve HL
        ret                     ; return to caller
ERSTX:  defb    "Delete file? ",0
ERASED: defb    "File deleted",CR,0


; disk operation aborted
ABRTDS: ld      A,$01           ; re-enable...
        ld      (PRNTVIDEO),A   ; ...print-on-video
        push    HL              ; store code string pointer
        ld      HL,ABRTXT       ; Point to message
        call    PRS             ; print message for leaving init procedure
        pop     HL              ; retrieve code string pointer
        ret                     ; return to caller
ABRTXT: defb    CR,"Aborted",0
 

; **************************************************************************************
;                               D O S    R O U T I N E S
;***************************************************************************************

; DISK command -------
; execute several operations on a disk:
; "F": format/rewrite Master Sector
; "R": rename disk
; "W": rewrite Master Sector
; "U": undelete deleted files
; syntax: DISK "arg1"[,"arg2"]
TPHL:   equ     TMPDBF          ; temp. buffer for code string pointer
DISK:   call    DIRMOD          ; can be executed ONLY in direct mode
        jp      NZ,IMERR        ; raise error if in indirect mode
        ; check first argument
        dec     HL              ; dec 'cause GETCHR increments
        call    GETCHR          ; check if something follows
        jp      Z,SNERR         ; if nothing else, raise a syntax error
        call    EVAL            ; Evaluate expression
        call    TSTSTR          ; Make sure it's a string
        ld      (TPHL),HL       ; store code string pointer into a temp buffer
        call    GSTRCU          ; check that a string follows
        call    LOADFP          ; Move string block to BCDE (BC=pointer, E=length)
        ld      A,E
        dec     A               ; lenght must be = 1
        ld      HL,(TPHL)       ; retrieve code string pointer
        jp      NZ,SNERR
        xor     A
        ld      (TPBF4),A       ; set default to full format 
        ld      A,(BC)          ; load command
        and     %01011111       ; Force upper case
        cp      'F'             ; format command?
        jp      Z,DSKFRM        ; yes
        cp      'W'             ; rewrite master sector?
        jp      Z,RWMSSC        ; yes
        cp      'R'             ; rename command?
        jp      Z,DSKRNM        ; yes
        cp      'U'             ; undelete command?
        jp      Z,DSKUND        ; yes
        jp      SNERR           ; nothing more - raise a syntax error


; Format disk or rewrite Master sector
; syntax: DISK "F/W","disk name"
; "F" -> format disk, "W" -> rewrite master sector
; "disk name" is the name -> max 16 chars, allowed chars: "A" to "Z", "0" to "9","-", SPACE
; Format: set up a fresh new file system, creating the Master Sector and
; initializing the directory
; Master Sector rewriting: re-initialize the Master Sector, writing a new disk name and
; re-calculating disk geometry
DKNMPT: equ     TMPBFR2         ; store the pointer to the disk name string
DKLNPT: equ     TMPBFR3         ; store the pointer to the lenght of disk name string
RWMSSC  ld      A,$01           ; set rewriting of master sector
        ld      (TPBF4),A       
DSKFRM: call    CHKSYN          ; Make sure ',' follows
        defb    ','
        call    CHKFLNM         ; check for file name
        ld      DE,MSTTXT       ; format message
        ld      A,(TPBF4)       ; check for type of operation
        or      A               ; A=0 (full format)?
        jr      NZ,CFINIT       ; no, jump over
        ld      DE,FRMTXT       ; rewrite Master Sector
CFINIT: push    HL              ; store code string pointer
        ld      H,D             ; Point to message
        ld      L,E
        call    PRS             ; print message for confirmation
        pop     HL              ; retrieve HL
        call    CNFREQ          ; ask for confirmation
        jp      C,ABRTDS        ; if Carry set then abort
        call    CF_INIT         ; init CF card
        jp      C,DOS_ERR       ; error if device not available/ready
        push    HL              ; store code string pointer
        call    DSK_INIT        ; init disk
        pop     HL              ; retrieve code string pointer
        jp      C,DOS_ERR       ; if Carry set, raise error
        push    HL              ; store code string pointer
        ld      A,(TMPBFR1)
        or      A               ; full formatting?
        jr      NZ,MSPTOK       ; no
        ld      HL,OPRCMP       ; yes, Point to message
        call    PRS             ; print message for init confirmation
        ld      D,$00           ; print only disk details, no file list
        call    LST_FILES       ; print details of disk and list files
        pop     HL              ; retrieve code string pointer
        ret                     ; return to caller
MSPTOK: ld      HL,OPRCMP       ; format OK message
        call    PRS             ; print message for init confirmation
        pop     HL              ; retrieve code string pointer
        ret                     ; return to caller
MSTTXT: defb    "Rewrite Master Sector? ",0
FRMTXT: defb    "WARNING!! Format disk? ",0
OPRCMP: defb    "Operation completed",CR,0


; undelete files
; look for files marked as "deleted" and undelete them
DSKUND: dec     HL              ; dec 'cause GETCHR increments
        call    GETCHR          ; check if something follows
        jp      NZ,SNERR        ; if something else, raise a syntax error
        push    HL              ; store code string pointer
        call    DSKUNDFL        ; undelete files
        pop     HL              ; retrieve code string pointer
        jp      C,DOS_ERR       ; DOS error
        ret                     ; return to caller


; disk rename
; change the name of the disk w/o altering anything else
DSKRNM: call    CHKSYN          ; Make sure ',' follows
        defb    ','
        call    CHKFLNM         ; check for file name
        push    HL              ; store code string pointer
        ld      HL,RNDKTX       ; Point to message
        call    PRS             ; print message for confirmation
        pop     HL              ; retrieve HL
        call    CNFREQ          ; ask for confirmation
        jp      C,ABRTDS        ; if Carry set then abort
        call    CF_INIT         ; init CF card
        jp      C,DOS_ERR       ; error if device not available/ready
        push    HL              ; store code string pointer
        call    DSK_RNM         ; init disk
        pop     HL              ; retrieve code string pointer
        jp      C,DOS_ERR       ; DOS error
        ret
RNDKTX: defb    "Rename disk? ",0


; confirmation required by the user ('y' or 'Y' DO confirm, otherwise DON'T)
CNFREQ: call    CURSOR_ON       ; enable cursor
        xor     A
        ld      (PRNTVIDEO),A   ; disable print-on-video
        call    RXA             ; look for a pressed key
        cp      CTRLC           ; is it RUN STOP?
        jr      Z,CNFRQN        ; yes, abort operation
        and     %11011111       ; only UPPERCASE char
        cp      'Y'             ; 'Y'?
        jr      NZ,CNFRQN       ; no, abort operation
        ex      AF,AF'          ; store char into A'
        ld      A,$01           ; re-enable...
        ld      (PRNTVIDEO),A   ; ...print-on-video
        ex      AF,AF'          ; retrieve char from A'
        call    ECHO_CHAR       ; yes, echoes the char
        call    CURSOR_OFF      ; disable cursor
        xor     A               ; reset Carry flag
        ret                     ; return to caller
CNFRQN: scf                     ; set Carry flag
        ret                     ; return to caller


; check that a disk/file name follows
CHKFLNM:dec     HL              ; dec 'cause GETCHR increments
        call    GETCHR          ; check if something follows
        jp      Z,SNERR         ; if nothing else, raise a syntax error
        call    EVAL            ; Evaluate expression
CHKFN1: call    TSTSTR          ; Make sure it's a string
        ld      (TMPDBF+2),HL   ; store code string pointer into a temp buffer
        call    GSTRCU          ; get current string into pool
        call    LOADFP          ; Move string block to BCDE (BC=pointer, E=length)
        ld      A,E             ; check if lenght = 0
        and     A               ; null string?
        jp      Z,SNERR         ; yes, syntax error
        ld      (DKLNPT),DE     ; no, store lenght
        ld      (DKNMPT),BC     ; store address of temp string
        ld      HL,(TMPDBF+2)   ; retrieve code string pointer
        ret                     ; return to caller


; *****************************************************************************
; D I S K    I N I T
; Functions: format a disk creating a fresh new file system on disk or
;            rewrite only the Master Sector
; *****************************************************************************
DSK_INIT:   call    CHKSQFL         ; check if a seq. file is open
            jp      NZ,FILOPCLER    ; jump if open
            ld      A,(TPBF4)       ; load type of formatting
            ld      (TMPBFR1),A     ; save onto another location for later use
            call    CLRIOBF         ; clear I/O buffer
            call    CLRDOSBF        ; clear DOS buffer
            ld 	    A,$E0           ; select CF as master, driver 0, LBA mode (bits #5-7=111) 
            out     (CF_LBA3),A     ; send configuration
            ld      A,$EC           ; select "drive ID" command
            out     (CF_CMD),A      ; send command
            call    CF_DAT_RDY      ; wait until data is ready to be read
            call    CF_RD_CMD       ; read data and store into I/O buffer
            ld      DE,DOSBFR       ; address of default conf. buffer
            ld 	    HL,IOBUFF       ; get starting address of I/O buffer
            ld      BC,$000E        ; position of current disk size in sectors
            add     HL,BC           ; set into HL
            ld      C,$04           ; 4 bytes to copy
            ldir                    ; copy (DE is auto-incremented)
            ld 	    HL,IOBUFF       ; get starting address of I/O buffer
            ld      BC,$0002        ; 2 bytes to copy and also address of number of cylinders
            add     HL,BC           ; get position of data
            ldir                    ; copy (DE is auto-incremented)
            ld 	    HL,IOBUFF       ; get starting address of I/O buffer
            ld      BC,$000C        ; address of number of sectors per cylinder
            add     HL,BC           ; get position of data
            ld      C,$02           ; 2 bytes to copy
            ldir                    ; copy (DE is auto-incremented)
            ld 	    HL,IOBUFF       ; get starting address of I/O buffer
            ld      BC,$0006        ; address of number of heads
            add     HL,BC           ; get position of data
            ld      C,$02           ; 2 bytes to copy
            ldir                    ; copy (DE is auto-incremented)
            ; now we calculate the # of files allowed (1 file = 1 block = 64K)
            ld      (TPBF4),DE      ; store current pointer to temp. default conf. buffer
            ld      HL,DOSBFR       ; load number of sectors
            ld      C,(HL)          ; MSW into AC
            inc     HL
            ld      A,(HL)
            inc     HL
            ld      E,(HL)          ; LSW into DE
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
            dec     DE              ; yes, so set files to limit of $FFFF
DOS_FT1:    ld      HL,(TPBF4)      ; retrieve current pointer to temp. def. conf. buffer
            call    DE2HL           ; store # of entries
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
            ld      HL,DSKHDR       ; address of disk header string
            ld      DE,IOBUFF       ; address of I/O buffer
            ld      BC,$000F        ; 15 chars
            ldir                    ; copy header into buffer
            ld      HL,DOSBFR       ; first part of configuration
            ld      BC,$0010        ; composed by 16 chars
            ldir                    ; copy (DE is auto-incremented)
            xor     A               ; A=$0
            ld      (DE),A          ; marker at $1F
            inc     DE
            call    CHKNMVAL        ; copy disk name into buffer
            jp      C,NAMERR        ; disk name error
            call    RND_ID          ; generate a semi-random disk ID
            ld      HL,IOBUFF       ; get starting address of I/O buffer
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
            ld      A,CR            ; no, full format - go to new line
            call    OUTC
            push    IY              ; copy directory's size...
            pop     HL              ; ...into HL
            ld      BC,$0018        ; 24 steps
            call    DIV_16_8        ; calculate HL/24 (remainder is ignored, here)
            push    HL              ; store result...
            pop     IX              ; ...into IX...
            ex      DE,HL           ; ...and into DE
            ld      A,'-'           ; print a progress bar
            call    DOS_FT7         ; print it
            ld      A,CRSLFT        ; CURSOR left
            call    DOS_FT7         ; come back to beginning of line
            push    IY              ; copy directory's size
            pop     HL              ; into HL
            ld      BC,$0001        ; first sector of directory
            call    CLRIOBF         ; clear I/O buffer
DOS_FTA:    push    DE              ; store counter
            ld      DE,$0000        ; reset MSW of sector pointer
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

; print a char 24 times
DOS_FT7:    ld      B,$18           ; 24 times
DOS_FT8:    call    OUTC            ; print char
            djnz    DOS_FT8         ; repeat
            ret                     ; return to caller


; *****************************************************************************
; D I S K    R E N A M E
;******************************************************************************
DSK_RNM:    call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            call    CLRIOBF         ; clear I/O buffer
            call    CLRDOSBF        ; clear DOS buff.
            call    LDMSCT          ; load Master Sector
            ld      HL,IOBUFF       ; point to start of I/O buffer
            ld      BC,$0020        ; offset for disk name
            add     HL,BC           ; get pointer
            ex      DE,HL           ; copy pointer into DE
            call    CHKNMVAL        ; copy disk name into buffer
            jp      C,NAMERR        ; disk name error
            ld      DE,$0000        ; reset MSW sector
            ld      B,D             ; reset LSW sector
            ld      C,D
            call    CF_WR_SEC       ; write sector
            jp      C,WRT_ERR       ; error?
            ret                     ; no, return to caller


; *****************************************************************************
; PUT A CHAR INTO A SEQUENTIAL FILE
; when the buffer is full, it writes it on the disk,
; then move to another sector
; *****************************************************************************
PUTFIL:     call    CHKSQFL         ; is there a seq. file open?
            jp      Z,FILOPCLER     ; no, raise an error
            ld      B,A             ; copy file number into B
            ld      A,(TMPDBF)      ; load argument
            cp      B               ; are they equal?
            jp      NZ,FLNTFND      ; file not found
            ld      A,(SEQFLS)      ; check access mode
            or      A               ; only reading?
            ld      A,D3            ; prepare a save error
            jp      Z,RET_ERR       ; yes, error
            ; store byte
            ld      DE,(SEQBYSZ)    ; load size in bytes
            inc     DE              ; increment file size
            ld      A,E             ; check if...
            or      D               ; ...rolled back to $0000
            jp      Z,DSKFULL       ; yes, block is full
            ld      (SEQBYSZ),DE    ; no, store new size
            ld      HL,(SEQPNT)     ; pointer to byte
            ld      DE,IOBUFF       ; load starting address of I/O buffer
            ld      B,H             ; copy byte pointer...
            ld      C,L             ; ...to BC
            add     HL,DE           ; point to new location
            ld      A,(TMPDBF+1)    ; retrieve byte
            ld      (HL),A          ; store it
            inc     BC              ; increment byte pointer
            ld      (SEQPNT),BC     ; store it
            call    PUTNXSC         ; check if I/O buffer is full
            xor     A               ; clear Carry
            ret                     ; return to caller
            ; check if I/O buffer is full
            ; if yes, store buffer and goto a new sector
PUTNXSC:    ld      DE,$0200        ; max buffer size
            ld      HL,(SEQPNT)     ; pointer to byte
            call    CMP16           ; check if pointer < $200
            ret     C               ; yes, return
            ld      BC,(SEQSCTL)    ; load LSW of sector address
            ld      DE,(SEQSCTM)    ; load MSW of sector address
            call    CF_WR_SEC       ; write buffer into sector
            ld      A,(SEQSCSZ)     ; load size in sectors
            inc     A               ; we need another sector
            ld      H,A             ; store A into H
            cp      $81             ; A>128?
            ld      A,D5            ; prepare a disk full error
            jp      NC,GETER        ; yes, raise an error
            ld      A,H             ; no, retrieve sector counter
            ld      (SEQSCSZ),A     ; store new size in sectors
            ld      HL,$0000        ; reset pointer
            ld      (SEQPNT),HL
            inc     BC              ; next sector
            ld      A,C             ; check if BC is rolled back to zero (overflow)
            or      B
            jr      NZ,PUTFIL2      ; no, jump over
            inc     DE              ; yes, increment MSW of sector address
            ld      (SEQSCTM),DE    ; save new MSW of sector address
PUTFIL2:    ld      (SEQSCTL),BC    ; save new LSW of sector address
            jp      OPNFRD1         ; load sector and return


; *****************************************************************************
; OPEN A SEQUENTIAL FILE
; open a sequential file for read/write operations
; if file already exists, it appends new data at the end of the file
;******************************************************************************
OPNFIL:     call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            call    CHKSQFL         ; check if there is a file already opened
            jp      NZ,FILALROP     ; yes, jump over
            call    CLRSEQBF        ; clear seq. file buffer
            ld      A,(TMPDBF)      ; recover file number and...
            ld      (SEQFL),A       ; ...store it
            ld      A,(TMPDBF+1)    ; load access mode
            ld      (SEQFLS),A      ; store access mode
            ; open file
            call    CLRIOBF         ; clear I/O
            call    CHKFLEXT        ; check if file already exists
            ld      E,$00           ; reset E
            rl      E               ; store Carry into E (0=file not exists, 1=file exists)
            ld      A,(SEQFLS)      ; load access mode (0=read, 1=create/append)
            or      E               ; if read mode and file not exist...
            jp      Z,FLNTFND       ; ...raise a file not found error
            ld      A,(SEQFLS)      ; reload access mode (0=read, 1=create/append)
            or      A               ; read mode?
            jr      Z,OPFLRS        ; yes, open file for read
            ; open file for writing
            ld      A,E             ; no, open file for save
            or      A               ; check if file exists
            jr      NZ,OPNFRD       ; file exists, open for append
            ; open file for creating (new file)
            call    CLRIOBF         ; clear I/O buffer
            call    FNDFRENTR       ; file doesn't exist, find a free entry in the directory
            jp      C,DSKFULL       ; no entry, disk full error
            ld      A,$82           ; set seq. file type
            ld      (TPBF4),A
            ld      A,$01           ; set initial size to 1 sector
            ld      (SEQSCSZ),A     ; store it for seq. file manager
            ld      (SCT_SIZ),A     ; store it for DOS, too
            ld      HL,$0000        ; set initial file size to 0 bytes
            ld      (BYT_SIZ),HL    ; store it for DOS
            ld      (SRTMEM),HL     ; RAM starting address set to 0 (seq. file is not saved/loaded from/into RAM)
            call    SVENTRY         ; save entry into directory
            ld      DE,(MSW_SCT)    ; store MSW of sector address for DOS
            ld      (SEQSCTM),DE    ; also for seq. file manager
            ld      BC,(LSW_SCT)    ; store LSW of sector address for DOS
            ld      (SEQSCTL),BC    ; also for seq. file manager
            call    SVSQFLNM        ; store name
            call    CLRIOBF         ; clear I/O buffer
            ld      DE,(MSW_SCT)    ; load MSW of sector
            ld      BC,(LSW_SCT)    ; load LSW of sector   
            call    CF_WR_SEC       ; write sector
            jp      OPNFILPT        ; go setting pointer                     
            ; open file for append (add data after the end)
OPNFRD:     call    OPNRDDET        ; load file details - size in bytes in DE - size in sectors in A
            ld      HL,$0200        ; sector wide
            ex      DE,HL           ; move file size into HL and sector size into DE
OPNFRD3:    dec     A               ; decrement sector counter
            jr      Z,OPNFRD2       ; reached 0? jump over
            or      A               ; reset Carry
            sbc     HL,DE           ; decrement sector size
            jr      OPNFRD3         ; repeat
OPNFRD2:    ld      (SEQPNT),HL     ; save pointer
            ld      A,(SEQSCSZ)     ; load size in sectors
            dec     A               ; decrement by 1
            ld      C,A             ; store into...
            ld      B,$00           ; ...HL
            ld      HL,(SEQSCTL)    ; load LSW of sector address
            ld      DE,(SEQSCTM)    ; load MSW of sector address
            add     HL,BC           ; point to latest sector
            ld      C,L             ; copy HL into BC
            ld      B,H
            jr      NC,OPNFRD1      ; if LSW didn't overflow, jump over
            inc     DE              ; overflow, so increment MSW
            ld      (SEQSCTM),DE    ; store new MSW of sector address
OPNFRD1:    ld      (SEQSCTL),BC    ; store new LSW of sector address
            call    CF_SETSTR       ; set sector to load
            call    CF_RD_SEC       ; read sector
            ret                     ; return to caller
            ; open file for read & point to beginning of it
OPFLRS:     call    OPNRDDET        ; load file details
            ld      DE,(SEQSCTM)    ; load MSW of sector address
            ld      BC,(SEQSCTL)    ; load LSW of sector addres
            call    CF_SETSTR       ; point to 1st sector 
            call    CF_RD_SEC       ; load sector
OPNFILPT:   xor     A               ; A=0
            ld      (SEQPNT),A      ; file pointer...
            ld      (SEQPNT+1),A    ; ...set to 0
            ret                     ; return to caller


; open an existing file and load details
OPNRDDET:   ld      E,(IX+$14)      ; load MSW into DE
            ld      D,(IX+$15)
            ld      C,(IX+$16)      ; load LSW into BC
            ld      B,(IX+$17)
            ld      (MSW_SCT),DE    ; store DE for DOS
            ld      (SEQSCTM),DE    ; also for seq. file manager
            ld      (LSW_SCT),BC    ; store BC dor DOS
            ld      (SEQSCTL),BC    ; also for seq. file manager
            ld      E,(IX+$18)      ; load size in bytes
            ld      D,(IX+$19)
            ld      (BYT_SIZ),DE    ; store it for DOS
            ld      (SEQBYSZ),DE    ; store it for seq. file manager
            ld      A,(IX+$1A)      ; load size in sectors
            ld      (SCT_SIZ),A     ; store it for DOS
            ld      (SEQSCSZ),A     ; and also for seq. file manager
            ret                     ; return to caller


; save file name into seq. buffer
SVSQFLNM:   ld      DE,TMPNAM       ; temp. buff.
            ld      BC,$0010        ; 10 chars
            ld      HL,DOSBFR       ; pointer to file name
            ldir                    ; store filename
            ret                     ; return to caller

; *****************************************************************************
; CLOSE A SEQUENTIAL FILE
; *****************************************************************************
CLOSFIL:    call    CHKSQFL         ; is there a seq. file open?
            jp      Z,FILOPCLER     ; no, raise an error
            ld      B,A             ; copy file number into B
            ld      A,(TMPDBF)      ; load argument
            cp      B               ; are they equal?
            jp      NZ,FLNTFND      ; file not found
            ld      A,(SEQFLS)      ; check access mode
            or      A               ; only reading?
            jr      Z,CLOSFIL1      ; yes, just mark file closed
            call    SVCRNTSE        ; save current buffer
            call    UPCRNENT        ; update entry into dir
            jp      C,FLNTFND       ; file not found?
CLOSFIL1:   ;call    CLRSEQBF        ; clear seq. buffer
            xor     A               ; mark file as closed
            ld      (SEQFL),A
            ret                     ; return to caller


; save current buffer into a sequential file
SVCRNTSE:   ld      DE,(SEQSCTM)    ; MSW of sector address
            ld      BC,(SEQSCTL)    ; LSW of sector address
            call    CF_WR_SEC       ; save current open sector
            ret


; update current entry (for a seq. file)
UPCRNENT:   ld      DE,$0010
            ld      (DKLNPT),DE     ; lenght of name
            ld      DE,TMPNAM
            ld      (DKNMPT),DE     ; pointer to name
            call    CHKFLEXT        ; check file existance
            jp      NC,UPCRNENT1    ; file not found
            ld      HL,(SEQBYSZ)
            ld      (IX+$18),L      ; point to file size
            ld      (IX+$19),H
            ld      A,(SEQSCSZ)     ; load size in sectors
            ld      (IX+$1A),A
            ld      DE,(TPBF2)      ; retrieve MSW of current directory sector
            ld      BC,(TPBF3)      ; retrieve LSW of current directory secto
            call    CF_WR_SEC       ; store entry
            scf                     ; set Carry
UPCRNENT1:  ccf                     ; invert Carry
            ret


; *****************************************************************************
; E O F 
; check EOF/size
; input: 0 => return size of current file
; input: file number => 1 if EOF, 0 otherwise
; *****************************************************************************
EOF:        call    CHKSQFL         ; is there a seq. file open?
            ld      E,D0            ; prepare a file open error
            jp      Z,ERROR         ; no, raise an error
            call    DEINT           ; get any number
            ld      A,E             ; is it 0?
            or      A
            jr      Z,EOF1          ; ok, jump over
            ld      A,(SEQFL)       ; get current file number
            cp      E               ; same?
            jp      NZ,FCERR        ; no, error
EOF1:       push    DE              ; store value
            push    HL              ; store code string pointer
            call    CHKEOF          ; check EOF/size
            pop     HL              ; retrieve code string pointer
            pop     BC              ; retrieve argument
            ld      A,$00           ; reset A (w/o altering Carry)
            rla                     ; copy Carry into bit #0
            ld      B,A             ; copy A into B
            ld      A,C             ; retrieve argument
            or      A               ; is it zero?
            jr      NZ,RETEOF       ; return EOF
            ld      A,D             ; copy size into AB
            ld      B,E
            jp      ABPASS          ; return size into AB
RETEOF:     ld      A,B             ; retrieve EOF
            jp      PASSA           ; return value


; *****************************************************************************
; P U T
; *****************************************************************************
PUT:        call    GETINT          ; get file number
            ld      (TMPDBF),A      ; store it
            call    CHKSYN          ; Make sure ',' follows
            defb    ','
            call    GETINT          ; get mode
            ld      (TMPDBF+1),A    ; store it
            push    HL              ; store code string pointer
            call    PUTFIL          ; insert data into file
            jp      C,DOS_ERR       ; any error?
            pop     HL              ; retrieve code string pointer
            ret                     ; return to caller


; *****************************************************************************
; G E T
; return a byte read from a sequential file
; *****************************************************************************
GET:        call    DEINT           ; get file number
            ld      A,E             ; check if...
            or      A               ; ...it's zero
            jp      Z,FCERR         ; yes, illegal function call
            call    CHKSQFL         ; is there a seq. file opened?
            ld      A,D0            ; prepare a file open error
            jr      Z,GETER         ; no, exit with error
            ld      A,(SEQFLS)      ; check mode
            or      A               ; is it write?
            jp      NZ,FCERR        ; yes, raise error
            ld      A,(SEQFL)       ; file number
            cp      E               ; same?
            jp      NZ,FCERR        ; no, error
            ; load byte pointed by seq. file pointer
            push    HL              ; store code string pointer
            call    CHKEOF          ; check if EOF
            ld      E,DA
            jp      C,ERROR         ; return EOF error
            ld      HL,(SEQPNT)     ; reload pointer in bytes
            ld      A,H             ; copy into AC
            ld      C,L
            ld      DE,$0200        ; size of buffer
            call    DIV_16_16       ; calculate (pointer MOD $200) => HL=current pointer into buffer
            ld      DE,IOBUFF       ; address of I/O buffer
            add     HL,DE           ; find position of byte to load
            ld      A,(HL)          ; load byte
            ld      (TMPBFR1),A     ; store
            ld      HL,(SEQPNT)     ; load pointer
            inc     HL              ; increment pointer
            ld      (SEQPNT),HL
            ld      DE,$0200        ; sector size
            ld      A,H             ; copy HL into AC
            ld      C,L
            call    DIV_16_16       ; calculate <pointer/$200>
            ld      A,L
            cp      H               ; check if remainder is 0 meaning that we read all the buffer ($200 chars)
            jr      NZ,GET1         ; no, jump over
            ld      BC,(SEQSCTL)    ; load LSW of sector address
            inc     BC              ; next sector
            ld      A,C             ; check if BC is rolled back to zero (overflow)
            or      B
            jr      NZ,GET2         ; no, jump over
            ld      DE,(SEQSCTM)    ; load MSW of sector address
            inc     DE              ; yes, increment MSW of sector address
            ld      (SEQSCTM),DE    ; save MSW of sector address
GET2:       ld      (SEQSCTL),BC    ; save LSW of sector address
            ld      DE,(SEQSCTM)    ; load MSW of sector address
            call    CF_SETSTR       ; set sector to read
            call    CF_RD_SEC       ; read next sector
            call    CF_STANDBY      ; set CF to standby
GET1:       pop     HL              ; retrieve code string pointer
            ld      A,(TMPBFR1)
            jp      PASSA           ; return A and then return to caller
GETER:      pop     HL              ; retrieve code string pointer
            ld      E,A             ; load error code
            call    CF_STANDBY      ; set CF into stand-by
            jp      ERROR           ; raise error


; *****************************************************************************
; DOS ERRORS
; *****************************************************************************
FILOPCLER:  ld      A,D0            ; file open/close error
            defb    $01             ; Skip next statement
FILALROP:   ld      A,D9            ; file already open
            defb    $01             ; Skip next statement
D2ERR:      ld      A,D2            ; disk geometry error
            defb    $01             ; Skip next statement
DSKFULL:    ld      A,D5            ; disk full error
            defb    $01             ; Skip next statement
DUPLERR:    ld      A,D6            ; duplicate file name
            defb    $01             ; Skip next statement
NAMERR:     ld      A,D1            ; file name error
            defb    $01             ; Skip next statement
FLNTFND:    ld      A,D8            ; file not found
            defb    $01             ; Skip next statement
LODERR:     ld      A,D4            ; generic load error
            defb    $01             ; Skip next statement
DOSVERSERR: ld      A,D7            ; DOS version error
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
LST_FILES:  call    CHKSQFL         ; check if a seq. file is open
            jp      NZ,FILOPCLER    ; jump if open
            call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            push    DE              ; store D
            call    LDMSCT          ; load Master Sector
            ld      HL,DSKNMTX      ; pointer to "Disk name" message
            call    PRS             ; print it
            ld 	    HL,IOBUFF       ; get starting address of I/O buffer
            ld      DE,$0020        ; position of disk name
            add     HL,DE           ; get address
            ld      B,$10           ; 16 chars
INPR1:      ld      A,(HL)          ; load char
            call    OUTC            ; print it
            inc     HL        
            djnz    INPR1           ; repeat
            ld      A,CR
            call    OUTC            ; carriage return
            ld      IX,$0000        ; reset file counter (for use in formatting)
            pop     DE              ; retrieve D
            ld      A,D
            or      A               ; is D=0?
            jp      Z,PNTSTATS      ; yes, jump over
            ; print list of files
            ld      HL,IOBUFF       ; start of I/O buffer
            ld      DE,$000F        ; point to details of disk
            add     HL,DE           ; find address
            ld      DE,DOSBFR       ; store into DOS buffer
            ld      BC,$0020        ; 32 bytes
            ldir                    ; copy
            call    LDENTRIES       ; load entries
            ld      IY,(NTR_NBR)    ; load max entries
            exx
            ld      BC,$0000        ; reset file counter
            exx
            call    SETPTEN         ; point to 1st sector of dir
LSTFILES1:  call    PT2FSEN         ; point to 1st entry of a dir's sect
LSTFILES2:  call    CKCREN          ; check current entry
            jr      Z,LSTFILES6     ; if empty or deleted, ignore it
            push    BC              ; this is a valid entry - so, store BC (LSW of sect)
            push    IY              ; store IY (entries counter)
            push    HL              ; store HL (sect entry counter)
            push    DE              ; store DE (MSW of sector)
            push    IX              ; store IX (pointer to first byte of entry)
            ld      B,$10           ; 16 chars to read and print
LSTFILES3:  ld      A,(IX)          ; load char from name
            call    OUTC            ; print char
            inc     IX              ; next char
            djnz    LSTFILES3       ; repeat
            ld      A,SPC
            call    OUTC            ; print space
            ld      A,(IX)          ; file type
            sub     $80             ; types start from $80
            or      A               ; BAS type ($00)?
            jr      NZ,LSTFILES20   ; no, jump over
            ld      HL,FILETP       ; print "BAS"
            jr      LSTFILESPR
LSTFILES20: dec     A               ; BIN type ($01)?
            jr      NZ,LSTFILES21   ; no, jump over
            ld      HL,FILETP+5     ; print "BIN"
            jr      LSTFILESPR
LSTFILES21: dec     A
            jr      NZ,LSTFILES22
            ld      HL,FILETP+10    ; print "SEQ"
            jr      LSTFILESPR
LSTFILES22: ld      HL,FILETP+10    ; print "???"
LSTFILESPR: call    PRS
            ld      BC,$0008
            add     IX,BC           ; point to file size in bytes
            ld      C,(IX)          ; load size in BC, first LSW
            inc     IX
            ld      B,(IX)          ; then MSW
            push    BC              ; copy...
            pop     IX              ; ...into IX
            ld      DE,$0000        ; reset DE
            push    IY
            call    PRN16ASCIX      ; print size in bytes (DEIX)
            pop     IY
            ld      A,CR
            call    OUTC            ; print carriage return
            exx                     ; set shadow registers
            inc     BC              ; increment file counter
            exx                     ; restore main registers
            pop     IX              ; retrieve IX
            pop     DE              ; retrieve DE
            pop     HL              ; retrieve HL
            pop     IY              ; retrieve IY
            pop     BC              ; retrieve BC
LSTFILES6:  call    TSTBRK          ; Test for break key
            call    TSTSPC          ; test for space
            call    GTNXTEN         ; other entries in this sector?
            jr      NZ,LSTFILES2    ; yes, continue check
            call    CKLSTEN         ; go to next sector
            jp      NC,LSTFILES1    ; more entries? repeat
            exx                     ; set shadow registers
            push    BC              ; store file counter
            exx                     ; restore main registers
            pop     IX              ; retrieve file counter
            push    IX              ; store it again
            ld      DE,$0000        ; reset DE
            call    PRN16ASCIX      ; print number of files from DEIX
            ld      HL,TLFLSTX
            call    PRS             ; print "file(s)"
            pop     IX
PNTSTATS:   ld      HL,TLSCTTX      ; Point to message "Tot. sectors"
            call    PRS             ; print message
            ld      HL,DOSBFR       ; reload address of I/O buffer and point to disk size
            push    IX
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
            ld      HL,ALFLSTXT     ; Point to message "Allowed files"
            call    PRS             ; print message
            ld      HL,DOSBFR       ; reload address of I/O buffer
            ld      BC,$000A        ; address of allowed files
            add     HL,BC           ; find pointer
            ld      DE,$0000        ; MSW set to $0000
            call    PRN16ASCII      ; print max files
            ld      A,CR
            call    OUTC            ; print a carriage return
            call    CF_STANDBY      ; put CF into standby
            xor     A               ; clear Carry flag
            ret                     ; return to caller
FILETP:     defb    "BAS ",0        ; BASIC type
            defb    "BIN ",0        ; BINARY type
            defb    "SEQ ",0        ; SEQUENTIAL type
            defb    "??? ",0        ; unkown


; *****************************************************************************
; S A V E    F I L E
; save current BASIC program onto a file
; *****************************************************************************
SAVFILE:    call    CHKSQFL         ; check if a seq. file is open
            jp      NZ,FILOPCLER    ; jump if open
            call    CHKFLEXT        ; file already exists?
            jp      C,DUPLERR       ; name is present - error
            call    FNDFRENTR       ; find a free entry in the directory
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
            call    SVENTRY         ; save new entry into dir
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
            ld      DE,IOBUFF       ; load start of I/O buffer
            ldir                    ; copy data
            ld      DE,(MSW_SCT)    ; load MSW of sector
            ld      BC,(LSW_SCT)    ; load LSW of sector   
            call    CF_WR_SEC       ; write sector
            jp      C,WRT_ERR       ; error?
            ld      HL,SCT_SIZ      ; sector counter
            dec     (HL)            ; copied all the sectors?
            jp      Z,SAVFLEXT      ; yes, exit
            ld      HL,(RAM_PTR)    ; pointer to RAM
            ld      DE,$0200        ; block of 512 bytes copied
            add     HL,DE           ; calculate next starting address
            ld      (RAM_PTR),HL    ; store next block
            xor     A               ; reset Carry
            ld      HL,(BYT_SIZ)    ; load left bytes
            sbc     HL,DE           ; subtract copied bytes
            ld      (BYT_SIZ),HL    ; store left bytes
            ld      BC,(LSW_SCT)    ; load LSW of sector
            inc     BC              ; next sector
            ld      A,B             ; BC=$0000?
            or      C
            jr      NZ,SAVFL12      ; no, jump over
            ld      DE,(MSW_SCT)    ; load MSW of sector
            inc     DE              ; increment DE
            ld      (MSW_SCT),DE    ; store new MSW of sector
SAVFL12:    ld      (LSW_SCT),BC    ; store new LSW of sector
            jp      SAVFL10         ; repeat
SAVFLEXT:   call    CF_STANDBY      ; set CF into stand-by mode
            xor     A               ; clear Carry flag
            ret                     ; return to caller


; save entry on disk
SVENTRY:    ld      DE,$0000        ; directory is always from sector 0000-0001
            ld      BC,(DIR_SCT)    ; load sector of dir where to save file entry
            call    CF_SETSTR       ; set sector to read
            call    CF_RD_SEC       ; read sector
            ld      A,(NTR_NBR)     ; load entry number (ignoring MSB)
            and     %00001111       ; be sure to get only low nibble
            add     A,A             ; multiply...
            add     A,A             ; ...times 16 by...
            add     A,A             ; ...adding...
            add     A,A             ; ...4 times
            sla     A               ; left shift to multiply times 32 - Carry is set if results is > 255
            ld      E,A             ; copy into C
            rl      D               ; if Carry, then increment D (D was 0, see above)
            ; set name
            ld 	    HL,IOBUFF       ; get starting address of I/O buffer
            add     HL,DE           ; add offset to get address of entry
            ld      DE,DOSBFR       ; pointer to file name
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
SAVFL9:     ld      (MSW_SCT),DE    ; store MSW of sector
            ld      (LSW_SCT),HL    ; store LSW of sector
            pop     HL              ; retrieve pointer
            call    DE2HL           ; also copy MSW of sector into entry
            ld      DE,(LSW_SCT)    ; retrieve LSW
            call    DE2HL           ; also copy LSW of sector into entry
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
            ret


; *****************************************************************************
; C H A N G E    F I L E N A M E
; change name to a file
; *****************************************************************************
CHNGNAM:    ld      DE,(BYT_SIZ)    ; load lenght
            ld      (DKLNPT),DE     ; store it
            ld      DE,(RAM_PTR)    ; load address
            ld      (DKNMPT),DE     ; store it
            call    CHKFLEXT        ; destination file already exists?
            jp      C,DUPLERR       ; file name already exists
            call    CLRIOBF         ; clear I/O buffer
            ld      DE,(TMPBFR4)    ; load lenght
            ld      (DKLNPT),DE     ; store it
            ld      DE,(TPBF4+2)    ; load address
            ld      (DKNMPT),DE     ; store it
            call    LK4FILE         ; look for file
            jp      NC,FLNTFND      ; file not found error
            ld      DE,(BYT_SIZ)    ; load lenght
            ld      (DKLNPT),DE     ; store it
            ld      DE,(RAM_PTR)    ; load address
            ld      (DKNMPT),DE     ; store it
            call    CLRDOSBF        ; clear DOS buffer
            ld      DE,DOSBFR       ; DE set to beginning of DOS buffer
            call    CHKNMVAL        ; check and copy file name
            ld      HL,DOSBFR       ; retrieve new name pointer
            push    IX              ; copy dest. address...
            pop     DE              ; ...into DE
            ld      BC,$0010        ; 16 chars to copy
            ldir                    ; copy
            ld      DE,(TPBF2)      ; retrieve MSW of dir. sector
            ld      BC,(TPBF3)      ; retrieve LSW of dir. sector
            call    CF_WR_SEC       ; write sector
            call    CF_STANDBY      ; put CF in standby
            ret                     ; return to caller


; *****************************************************************************
; C H E C K    F I L E    E X I S T A N C E
; check if a file with the given name exists into dir
; Carry set if exist
; *****************************************************************************
CHKFLEXT:   call    CLRIOBF         ; clear I/O
            call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            call    LDENTRIES       ; load entries into RAM register NTR_NBR
            call    HL2DE           ; get start of directory into DE
            ld      (DIR_STRT),DE   ; store
            call    HL2DE           ; get start of data area into DE
            ld      (DAT_STRT),DE   ; store
            jp      LK4FILE1        ; check for name already present in dir and return to caller


; *****************************************************************************
; L O O K    F O R    A    F I L E
; look for the given file into the dir
; Carry is reset if not found, set otherwise
; *****************************************************************************
LK4FILE:    call    LDENTRIES       ; load entries into RAM register NTR_NBR
LK4FILE1:   call    CLRIOBF         ; clear IO buffer
            call    CLRDOSBF        ; clear DOS buffer
            ld      DE,DOSBFR       ; DE set to beginning of DOS buffer
            call    CHKNMVAL        ; check and copy file name
            jp      C,NAMERR        ; if Carry, file name error
            jp      FILE_EXIST      ; check if file exists and return to caller


; *****************************************************************************
; F I L E    L O A D
; load a file from the disk into the memory
; *****************************************************************************
LODFILE:    call    CHKSQFL         ; check if a seq. file is open
            jp      NZ,FILOPCLER    ; jump if open
            call    CLRIOBF         ; clear I/O
            call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            call    LK4FILE         ; look for given file
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
            ld      DE,MSW_SCT      ; point to store sector address of file
            ldir                    ; copy MSW/LSW from entry into buffer
            ld      DE,BYT_SIZ      ; point to store size of file
            ld      BC,$0003        ; 3 bytes to copy
            ldir                    ; copy size in bytes and sectors from entry into buffer
            ld      DE,PROGND       ; load start of BASIC RAM
            ld      A,(TPBF4)       ; check where to save the data from
            or      A               ; is it 0? (meaning from the current BASIC pointers)
            jr      Z,LDFL1         ; yes, don't re-load the file from the address stored into the file
            call    HL2DE           ; no, load RAM address from disk
LDFL1:      ld      (RAM_PTR),DE    ; store starting pointer
            ; start loading from disk
LDFL2:      call    CLRIOBF         ; clear I/O buffer
            ld      DE,(MSW_SCT)    ; load MSW of sector
            ld      BC,(LSW_SCT)    ; load LSW of sector   
            call    CF_SETSTR       ; set sector
            call    CF_RD_SEC       ; read sector
            ld      HL,(BYT_SIZ)    ; load bytes left to be copied
            ld      DE,$0200        ; are they < 512?
            call    CMP16
            jr      NC,LDFL3        ; no, jump over
            ld      DE,(BYT_SIZ)    ; yes, so load only remaining bytes
LDFL3:      ld      C,E             ; move bytes to copy...
            ld      B,D             ; ...into BC
            ld      HL,IOBUFF       ; point to I/O buffer
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
            ld      BC,(LSW_SCT)    ; load LSW of sector
            inc     BC              ; next sector
            ld      A,B             ; BC=$0000?
            or      C
            jr      NZ,LDFL5        ; no, jump over
            ld      DE,(MSW_SCT)    ; load MSW of sector
            inc     DE              ; increment DE
            ld      (MSW_SCT),DE    ; store new MSW of sector
LDFL5:      ld      (LSW_SCT),BC    ; store new LSW of sector
            jp      LDFL2           ; repeat
LDFLEX:     call    CF_STANDBY      ; set CF into stand-by mode
            xor     A               ; clear Carry flag
            ret                     ; return to caller


; *****************************************************************************
; F I L E    E R A S E
; erase a file from disk, freeing its block
; *****************************************************************************
FIL_ERASE:  call    CHKSQFL         ; check if a seq. file is open
            jp      NZ,FILOPCLER    ; jump if open
            call    CLRIOBF         ; clear I/O
            call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            call    LK4FILE         ; look for given file
            jp      NC,FLNTFND      ; file not found - error
            ld      E,(IX+$14)      ; load MSW into DE
            ld      D,(IX+$15)
            ld      C,(IX+$16)      ; load LSW into BC
            ld      B,(IX+$17)
            ld      (MSW_SCT),DE    ; store DE
            ld      (LSW_SCT),BC    ; store BC
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
FIL_ERA5:   ld      DE,(TPBF2)      ; retrieve MSW of current directory sector
            ld      BC,(TPBF3)      ; retrieve LSW of current directory sector
            call    CF_WR_SEC       ; write sector
            jr      C,WRT_ERR       ; error?
            ld      A,(TPBF4)       ; check for quick or full delete
            or      A               ; if A=0 then quick erase
            jr      Z,FIL_ERA3      ; yes, leave
            call    CLRIOBF         ; no, full delete - clear I/O buffer
            ld      DE,(MSW_SCT)    ; load MSW of 1st sector of file block
            ld      BC,(LSW_SCT)    ; load LSW of 1st sector of file block
            ld      HL,SCT_SIZ      ; pointer to size in sectors
FIL_ERA1:   call    CF_WR_SEC       ; erase sector
            jr      C,WRT_ERR       ; error?
            inc     BC              ; next sector
            ld      A,B             ; is LSW rolled back to 0?
            or      C
            jr      NZ,FIL_ERA2     ; no, jump over
            inc     DE              ; yes, increment MSW
FIL_ERA2:   dec     (HL)            ; decrement sector counter
            jr      NZ,FIL_ERA1     ; repeat if other sectors
FIL_ERA3:   call    CF_STANDBY      ; set CF into stand-by mode
            xor     A               ; clear Carry
            ret
WRT_ERR:    call    CF_STANDBY      ; put CF into stand-by
            scf                     ; set error
            ret                     ; return to caller


; *****************************************************************************
; F I L E    C H E C K
; check if file name already exists in directory
; file name must be stored from DOSBFR   for 16 chars
; *****************************************************************************
FILE_EXIST: ld      IY,(NTR_NBR)    ; load max entries
            call    SETPTEN         ; point to 1st sector of dir
CHKSTNM1:   call    PT2FSEN         ; point to 1st entry of a dir's sect
CHKSTNM2:   call    CKCREN          ; check current entry
            jr      Z,CHKSTNM10     ; if empty or deleted, jump over
            ld      (TPBF1),HL      ; store HL
            ld      (TPBF2),DE      ; store DE
            ld      (TPBF3),BC      ; store BC
            push    IX              ; copy IX...
            pop     HL              ; ...into HL
            ld      DE,DOSBFR       ; beginning of name file
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
CHKSTNM10:  call    GTNXTEN         ; other entries in this sector?
            jr      NZ,CHKSTNM2     ; yes, continue check
            call    CKLSTEN         ; go to next sector
            jr      NC,CHKSTNM1     ; more entries? repeat
            xor     A               ; file not found, reset Carry
            ret                     ; return
FLEXST:     scf                     ; file is present - set Carry flag for error
            ret


; *****************************************************************************
; UNDELETE DELETED FILES
; look for deleted files and undelete them
; *****************************************************************************
DSKUNDFL:   call    CLRIOBF         ; clear I/O
            call    CHKDSKVAL       ; check DOS version & load disk details
            jp      C,DOSVERSERR    ; if Carry is set, raise DOS version error
            call    FNDFRENTR       ; find a free entry
DSKUNDFL1:  ret     C               ; return if entries are finished
            ld      A,(IX)          ; reload first char of entry
            cp      $7F             ; is it a deleted entry?
            jr      NZ,DSKUNDFL2    ; no, jump over
            call    RND8            ; get a random char
            call    CHATOZ          ; transform it into a letter
            ld      (IX),A          ; store it as the first letter of filename
            call    CF_WR_SEC       ; write sector (address is into DEBC)
            push    HL              ; store HL
            push    BC              ; store BC
            push    DE              ; store DE
            push    IX              ; copy IX...
            pop     HL              ; ...into HL
            ld      B,$10           ; 16 chars
DSKUNDPR:   ld      A,(HL)          ; retrieve char from filename
            call    OUTC            ; print char
            inc     HL              ; next char
            djnz    DSKUNDPR        ; repeat
            ld      A,SPC           ; print a space
            call    OUTC
            ld      HL,DSKUNDTXT    ; print undeleted message
            call    PRS
            pop     DE              ; retrieve DE
            pop     BC              ; retrieve BC
            pop     HL              ; retrieve HL
DSKUNDFL2:  call    FNDFRENTR4      ; goto next entry
            jr      DSKUNDFL1       ; repeat
DSKUNDTXT:  defb    "undeleted",CR,0


; *****************************************************************************
; FIND A FREE ENTRY
; find a free entry in the directory to store a new file
; *****************************************************************************
FNDFRENTR:  call    LDMSCT          ; read Master Sector
            call    LDENTRIES       ; load entries into RAM register NTR_NBR and DE
            push    DE              ; copy number of entries...
            pop     IY              ; ...into IY
            call    SETPTEN         ; point to first entry
FNDFRENTR1: call    PT2FSEN         ; point to 1st entry of sector
FNDFRENTR2: call    CKCREN          ; check current entry
            jr      Z,FNDFRENTR3    ; found a free entry
FNDFRENTR4: call    GTNXTEN         ; other entries in this sector?
            jr      NZ,FNDFRENTR2   ; yes, continue check
            call    CKLSTEN         ; go to next sector
            jr      NC,FNDFRENTR1   ; more entries? repeat
            ret                     ; entries finished - leave
FNDFRENTR3: ld      (DIR_SCT),BC    ; store sector of dir
            ld      (NTR_NBR),HL    ; store entry number
            xor     A               ; reset Carry
            ret


; *****************************************************************************
;   U T I L I T I E S
; *****************************************************************************


; check if EOF for seq. file
; C is set if EOF, NC otherwise
; destroys DE & HL
; return: DE=size of seq. file; C=1 if EOF, C=0 otherwise
CHKEOF:     ld      DE,(SEQBYSZ)    ; size of seq. file
            ld      A,E             ; check if file size...
            or      D               ; ...is 0
            jr      Z,CHKEOFC       ; yes, EOF
            ld      HL,(SEQPNT)     ; pointer to last byte of seq. file
            call    CMP16           ; over the EOF?
CHKEOFC:    ccf                     ; invert Carry
            ret


; check if a sequential file is open
CHKSQFL:    ld      A,(SEQFL)       ; seq. file status
            or      A               ; if A<>0 then a file is open
            ret                     ; return to caller


; set up registers to point to first sector of directory
SETPTEN:    ld      BC,$0001        ; BC=$0001 (starting sector of dir) (LSW)
            ld      D,B             ; DE=$0000 (starting sector of dir) (MSW)
            ld      E,B
            ld      H,B             ; HL=$0000 (entry counter)
            ld      L,B
            ret                     ; return to caller


; load a sector and point to first entry
PT2FSEN:    call    CF_SETSTR       ; set sector to read (BC-DE)
            call    CF_RD_SEC       ; read sector
            ld      IX,IOBUFF       ; beginning of I/O buffer
            ret


; check current entry
CKCREN:     ld      A,(IX)          ; load 1st char of entry name
            ;dec     IY              ; decrement number of entries
            or      A               ; is it $00 (empty entry)?
            ret     Z               ; yes, found an entry
            cp      $7F             ; is it $7F (deleted entry)?
            ret


; goto next entry
GTNXTEN:    push    BC              ; store BC
            ld      BC,$0020        ; load BC with directory entry size (32 bytes)
            add     IX,BC           ; next entry in current sector
            pop     BC              ; retrieve sector pointer
            inc     HL              ; increment entry counter
            ld      A,L
            and     %00001111       ; just done 16 entries?
            ret


; check if reached last entry
; Carry is set if entries finished
CKLSTEN:    inc     BC              ; entries in this sector finished .. goto next sector
            ld      A,B             ; check if...
            or      C               ; ...BC=$000
            jr      NZ,CKLSTEN1     ; no, jump over
            inc     DE              ; yes, increment DE (MSW)
CKLSTEN1:   push    HL              ; preserve current entry
            push    DE
            push    IY              ; copy max allowed files...
            pop     DE              ; ...into HL
            call    CMP16           ; check if reached max allowed entries
            pop     DE
            pop     HL              ; (retrieve current entry)
            ccf
            ret                     ; return


; load entries intro RAM register
LDENTRIES:  ld      HL,IOBUFF       ; start of I/O buffer
            ld      BC,$0019        ; point to max. allowed entries
            add     HL,BC           ; get address
            call    HL2DE           ; get entries into DE
            ld      (NTR_NBR),DE    ; store
            ret                     ; return to caller


; load Master Sector (sector #0)
LDMSCT:     ld      BC,$0000        ; LSW of sector
            ld      D,B             ; MSW of sector
            ld      E,B
            call    CF_SETSTR       ; set sector
            call    CF_RD_SEC       ; read sector
            ret


; generate random disk ID
RND_ID:     push    BC
            ld      B,$02           ; repeat 2 times
RND_ID1:    call    RND8            ; get a random value
            call    CHATOZ          ; transform it into a letter
            ld      (DE),A          ; store it
            inc     DE              ; inc pointer
            call    RND8            ; get a random value   
            call    CH0TO9          ; transform it into a number from 0 to 9
            ld      (DE),A          ; store it
            inc     DE              ; inc pointer
            djnz    RND_ID1         ; repeat
            pop     BC
            ret                     ; return to caller


; generate a pseudo-random number using TMR and R registers
RND8:       push    BC              ; store B
            ld      A,(TMRCNT)      ; load LSW of sys-timer
            ld      B,A             ; copy into B
            ld      A,R             ; load refresh register
            xor     B               ; A xor B
            ld      B,A             ; copy into B
            rrca                    ; multiply by 32
            rrca
            rrca
            xor     $1F             ; a XOR to mix bits
            add     A,B             ; add B
            sbc     A,255           ; carry
            pop     BC              ; retrieve B
            ret                     ; return to caller


; char ported into 0-9 interval
CH0TO9:     and     %00001111       ; get only low nibble
            cp      $0A             ; is it < 10?
            jr      C,CH0TO9E       ; yes, jump over
            sub     $0A             ; subract 10
CH0TO9E:    add     $30             ; get a number from 0 to 9
            ret


; char ported into A-Z interval
CHATOZ:     and     %00011111       ; get only first 5 bits
            cp      $1A             ; is it < 26?
            jr      C,CHATOZE       ; yes, jump over
            sub     $1A             ; no, subtract 26
CHATOZE:    add     $41             ; get a letter from 'A' to 'Z'
            ret                     ; return to caller


; first check DOS validity then load disk details
CHKDSKVAL:  call    CF_INIT         ; open CF card comm.
            ret     C               ; if errors, leave
            push    BC              ; store BC
            push    DE              ; store DE
            push    HL              ; store HL
            ld      BC,$0000        ; reset LSW of sector
            ld      D,B             ; reset MSW of sector
            ld      E,B
            call    CF_SETSTR       ; set sector #0
            call    CF_RD_SEC       ; read sector
            ld      HL,IOBUFF       ; address of default conf. buffer
            ld      DE,$000A        ; point to disk DOS version
            add     HL,DE
            ld      DE,DSKHDR+10    ; get starting address of I/O buffer
            ld      B,$04           ; 4 chars
CHKDSKVAL1: ld      A,(DE)          ; load char from DOS version into memory
            cp      (HL)            ; compare with disk DOS version
            jr      NZ,CHKDSKVALE   ; no match - so error
            inc     DE              ; next source
            inc     HL              ; next comparison
            djnz    CHKDSKVAL1      ; repeat
            ld      HL,DOSBFR       ; address of default conf. buffer
            ld      DE,$001D        ; point to address of data area
            add     HL,DE           ; set pointer to beginning of identifies
            call    HL2DE           ; first sector of data area into DE
            xor     A               ; no error - clear Carry flag
            jr      CHKDSKVAL2      ; jump over
CHKDSKVALE: scf                     ; error - set carry flag
CHKDSKVAL2: pop     HL
            pop     DE
            pop     BC
            ret                     ; return to caller


; check name validity (only allowed chars) and copy it from string pool into a temp buff
; Inputs: DE: pointer to dest. buffer
; operation: copy (HL)->(DE) and pads to get a 16-chars name
; destroys: A, BC, DE, HL
CHKNMVAL:   ld      BC,(DKLNPT)     ; load lenght of name
            ld      A,C             ; lenght is max. 16 char, so we only check C
            cp      $11             ; is it <=16?
            jr      C,CHKNMVAL1     ; yes, so jump over
            ld      C,$10           ; no, get only 16 chars
CHKNMVAL1:  ld      B,C             ; copy lenght into B
            ld      C,$10           ; char counter
            ld      HL,(DKNMPT)     ; pointer to name
CHKNMVAL2:  ld      A,(HL)          ; get a char from string name
            call    CHK_NAM         ; check if valid
            ret     C               ; no, name error
            ld      (DE),A          ; yes, store char
            inc     DE              ; next I/O location
            inc     HL              ; next name char
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
DE2HL:      ld      (HL),E          ; LSW of size
            inc     HL
            ld      (HL),D          ; MSW of size
            inc     HL
            ret                     ; return to caller


; get DE from (HL) and (HL+1)
HL2DE:      ld      E,(HL)          ; get LSW into E
            inc     HL              ; next location
            ld      D,(HL)          ; get MSW into D
            inc     HL              ; next location
            ret                     ; return to caller


; convert a 16/32-bit number into an ASCII string and print it
; inputs: HL pointer to 32-bit number
PRN32ASCII: call    HL2DE           ; load MSW into DE  <-- entry for 32-bit
PRN16ASCII: ld      C,(HL)          ; load LSW into BC   <-- entry for 16-bit (set DE to $0000 before to call)
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


; check for non-allowed chars in disk/file names - allowed chars: '0'-'9', 'A'-'Z', '-', SPACE
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
            ret                     ; return to caller


; clear I/O buffer
CLRIOBF:    push    AF              ; store AF
            push    BC              ; store BC
            push    HL              ; store HL
            ld 	    HL,IOBUFF       ; load address of I/O buffer
            ld      BC,$0002        ; B=256 iterations ($00); C=repeat 2 times
CLRBUFF:    xor     A               ; reset A   ----- common part -----
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
CLRDOSBF:   push    AF              ; store AF
            push    BC              ; store BC
            push    HL              ; store HL
            ld 	    HL,DOSBFR       ; load address of DOS buffer
            ld      BC,$2001        ; B=32 iterations; C=repeat 1 time
            jp      CLRBUFF         ; continue to common part


; clear seq. file buffer
CLRSEQBF:   push    AF              ; store AF
            push    BC              ; store BC
            push    HL              ; store HL
            ld 	    HL,SEQFL        ; load address of DOS buffer
            ld      BC,$0B01        ; B=11 iterations; C=repeat 1 time
            jp      CLRBUFF         ; continue to common part
