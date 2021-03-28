; ------------------------------------------------------------------------------
; LM80C 64K - BUFFERS - R1.00
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
; R1.00 - 20210319 - first release
; R1.01 - 20210327 - added support for sequential files
;
;------------------------------------------------------------------------------


            DEFINE  DOSJPTBLS   $FDA0       ; beginning of DOS jump table & buffers
            BLOCK   DOSJPTBLS-BIOSEND,$FF   ; filler

;DOS jump table & buffers
            org     DOSJPTBLS       
SRTBFS:     equ     $

            ; buffers
DOSBFR:     BLOCK   $20,$FF         ; 32 bytes buffer
IOBUFF:     BLOCK   $200,$FF        ; 512 bytes buffer

            BLOCK   $06,$FF
            ; pointers for sequential file
TMPNAM:     equ     $
            BLOCK   $10,$00         ; 16 butes wide
SEQFL:      defb    $00             ; (1) 0=no seq. file open / >1=seq. file number
SEQFLS:     defb    $00             ; (1) seq. file mode: 0 read / 1 write
SEQSCTM:    defb    $00,$00         ; (2) MSW of sector address
SEQSCTL:    defb    $00,$00         ; (2) LSW of sector address
SEQSCSZ:    defb    $00             ; (1) size in sectors
SEQBYSZ:    defb    $00,$00         ; (2) size in bytes
SEQPNT:     defb    $00,$00         ; (2) pointer to byte

            defb    $FF
            ; DOS jump table
DOSJPTB     equ     $               ; address of 1st entry
JPEOF:      jp      EOF             ; jump to EOF statement
JPPUT:      jp      PUT             ; jump to PUT statement
JPGET:      jp      GET             ; jump to GET statement
JPCLOSE:    jp      CLOSE           ; jump to CLOSE statement
JPOPEN:     jp      OPEN            ; jump to OPEN statement
JPDISK:     jp      DISK            ; jump to DISK statement
JPERAS:     jp      ERASE           ; jump to ERASE statement
JPLOAD:     jp      LOAD            ; jump to LOAD statement
JPSAVE:     jp      SAVE            ; jump to SAVE statement
JPFILS:     jp      FILES           ; jump to FILES statement

