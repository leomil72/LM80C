; ------------------------------------------------------------------------------
; LM80C 64K - WORKSPACE EQUATES - R1.02
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

; set starting of RAM based on computer model
SERBUF_START    equ     END_OF_FW       ; RAM starts here

;-------------------------------------------------------------------------------
serInPtr        equ     SERBUF_START + SER_BUFSIZE
serRdPtr        equ     serInPtr+2
serBufUsed      equ     serRdPtr+2
basicStarted    equ     serBufUsed+1
bufWrap         equ     (SERBUF_START + SER_BUFSIZE) & $FF
TEMPSTACK       equ     CURPOS - 3      ; top of BASIC line input buffer so is "free ram" when BASIC resets

; BASIC WORK SPACE LOCATIONS
; THE INTERPRETER ALLOCATES THE FOLLOWING RAM CELLS
; TO STORE IMPORTANT VALUES USED FOR SOME SPECIFIC FUNCTIONS:
; THEY CAN BE VECTOR (ADDRESSES) FUNCTIONS, SYSTEM DATAS (I.E. VARIABLES)
; AND SO ON. THE FIRST CELLS ARE FILLED WITH VALUES STORED INTO ROM AT $(INITAB) ADDRESS
WRKSPC          equ     basicStarted+$01; (3) BASIC Work space
NMIUSR          equ     WRKSPC+$03      ; (3) NMI exit point routine
USR             equ     NMIUSR+$03      ; (3) "USR (x)" jump  <-- in (USR+$01)/(USR+$02) the user can store the address of a specific machine language routine
OUTSUB          equ     USR+$03         ; (1) "out p,n"
OTPORT          equ     OUTSUB+$01      ; (2) Port (p)
DIVSUP          equ     OTPORT+$02      ; (1) Division support routine
DIV1            equ     DIVSUP+$01      ; (4) <- Values
DIV2            equ     DIV1+$04        ; (4) <-   to
DIV3            equ     DIV2+$04        ; (3) <-   be
DIV4            equ     DIV3+$03        ; (2) <-inserted
SEED            equ     DIV4+$02        ; (35) Random number seed  <-- starting address of a seed table
LSTRND          equ     SEED+$23        ; (4) Last random number
INPSUB          equ     LSTRND+$04      ; (1) INP A,(x) Routine
INPORT          equ     INPSUB+$01      ; (2) PORT (x)
LWIDTH          equ     INPORT+$02      ; (1) Terminal width
COMMAN          equ     LWIDTH+$01      ; (1) Width for commas
NULFLG          equ     COMMAN+$01      ; (1) Null after input byte flag
CTLOFG          equ     NULFLG+$01      ; (1) Control "O" flag
CHKSUM          equ     CTLOFG+$01      ; (2) Array load/save check sum
NMIFLG          equ     CHKSUM+$02      ; (1) Flag for NMI break routine
BRKFLG          equ     NMIFLG+$01      ; (1) Break flag
RINPUT          equ     BRKFLG+$01      ; (3) Input reflection
STRSPC          equ     RINPUT+$03      ; (2) Pointer to bottom (start) of string space - default is 100 bytes below the top of memory
LINEAT          equ     STRSPC+$02      ; (2) Current line number. -1 means "direct mode", while -2 means cold start.
HLPLN           equ     LINEAT+$02      ; (2) Current line with errors
KEYDEL          equ     HLPLN+$02       ; (1) delay before key auto-repeat starts
AUTOKE          equ     KEYDEL+$01      ; (1) delay for key auto-repeat
FNKEYS          equ     AUTOKE+$01      ; (128) default text of FN keys
BASTXT          equ     FNKEYS+$80      ; (3) Pointer to start of BASIC program in memory
; - - - - - - - - - - - - - - - - - - -   the above are locations pre-filled by the firmware at startup
BUFFER          equ     BASTXT+$03      ; (5) Input buffer
STACK           equ     BUFFER+$05      ; (85) Initial stack
CURPOS          equ     STACK+$55       ; (1) Character position on line
LCRFLG          equ     CURPOS+$01      ; (1) Locate/Create flag for DIM statement
TYPE            equ     LCRFLG+$01      ; (1) Data type flag: 0=numeric, non-zero=string
DATFLG          equ     TYPE+$01        ; (1) Literal statement flag
LSTRAM          equ     DATFLG+$01      ; (2) Last available RAM location for BASIC
DOSER           equ     LSTRAM+$02      ; (1) Error from DOS
TMPDBF          equ     DOSER+$01       ; (36) Secondary buffer for DOS 
TMSTPT          equ     TMPDBF+$24      ; (2) Temporary string pointer
TMSTPL          equ     TMSTPT+$02      ; (12) Temporary string pool
TMPSTR          equ     TMSTPL+$0C      ; (4) Temporary string
STRBOT          equ     TMPSTR+$04      ; (2) Bottom of string space
CUROPR          equ     STRBOT+$02      ; (2) Current operator in EVAL
LOOPST          equ     CUROPR+$02      ; (2) First statement of loop
DATLIN          equ     LOOPST+$02      ; (2) Line of current DATA item
FORFLG          equ     DATLIN+$02      ; (1) "FOR" loop flag
LSTBIN          equ     FORFLG+$01      ; (1) Last byte entered
READFG          equ     LSTBIN+$01      ; (1) Read/Input flag
BRKLIN          equ     READFG+$01      ; (2) Line of break
NXTOPR          equ     BRKLIN+$02      ; (2) Next operator in EVAL
ERRLIN          equ     NXTOPR+$02      ; (2) Line of error
CONTAD          equ     ERRLIN+$02      ; (2) Where to CONTinue
TMRCNT          equ     CONTAD+$02      ; (4) TMR counter for 1/100 seconds
CTC0IV          equ     TMRCNT+$04      ; (3) CTC0 interrupt vector
CTC1IV          equ     CTC0IV+$03      ; (3) CTC1 interrupt vector
CTC2IV          equ     CTC1IV+$03      ; (3) CTC2 interrupt vector
CTC3IV          equ     CTC2IV+$03      ; (3) CTC3 interrupt vector
; - - - - - - - - - - - - - - - - - - -   VIDEO REGISTERS - FROM HERE...
SCR_SIZE_W      equ     CTC3IV+$03      ; (1) screen width (it can be either 40 chars or 32 chars/bytes)
SCR_SIZE_H      equ     SCR_SIZE_W+$01  ; (1) screen height (it can be 24/48/192: 24 for text, 48 for MC, 192 for graphics)
SCR_MODE        equ     SCR_SIZE_H+$01  ; (1) screen mode (0=text, 1=G1, 2=G2, 3=MC, 4=ExG2)
SCR_NAM_TB      equ     SCR_MODE+$01    ; (2) video name table address
SCR_CURS_X      equ     SCR_NAM_TB+$02  ; (1) cursor X
SCR_CURS_Y      equ     SCR_CURS_X+$01  ; (1) cursor Y
SCR_CUR_NX      equ     SCR_CURS_Y+$01  ; (1) new cursor X position
SCR_CUR_NY      equ     SCR_CUR_NX+$01  ; (1) new cursor Y position
SCR_ORG_CHR     equ     SCR_CUR_NY+$01  ; (1) original char positioned under the cursor
CRSR_STATE      equ     SCR_ORG_CHR+$01 ; (1) state of cursor (1=on, 0=off)
LSTCSRSTA       equ     CRSR_STATE+$01  ; (1) last cursor state
PRNTVIDEO       equ     LSTCSRSTA+$01   ; (1) print on video buffer (1=on / 0=off) set to off on graphic only modes
CHR4VID         equ     PRNTVIDEO+$01   ; (1) char for video buffer
FRGNDCLR        equ     CHR4VID+$01     ; (1) foreground color as set by SCREEN or COLOR commands
BKGNDCLR        equ     FRGNDCLR+$01    ; (1) background color as set by SCREEN or COLOR commands
TMPBFR1         equ     BKGNDCLR+$01    ; (2) word for general purposes use (temp. buffer for 1 or 2 bytes)
TMPBFR2         equ     TMPBFR1+$02     ; (2) word for general purposes use (temp. buffer for 1 or 2 bytes)
TMPBFR3         equ     TMPBFR2+$02     ; (2) word for general purposes use (temp. buffer for 1 or 2 bytes)
TMPBFR4         equ     TMPBFR3+$02     ; (2) word for general purposes use (temp. buffer for 1 or 2 bytes)
VIDEOBUFF       equ     TMPBFR4+$02     ; (40) buffer used for video scrolling and other purposes
VIDTMP1         equ     VIDEOBUFF+$28   ; (2) temporary video word
VIDTMP2         equ     VIDTMP1+$02     ; (2) temporary video word
; - - - - - - - - - - - - - - - - - - -   ...TO HERE. DO NOT ADD ANYTHING RELATED TO VPD OUT OF THIS RANGE,
                                        ; OTHERWISE YOU WILL HAVE TO CHECK THE POINTER IN "CLR_RAM_REG" FUNCTION
; - - - - - - - - - - - - - - - - - - -   SOUND & KEYBOARD REGISTERS - FROM HERE...
CHASNDDTN       equ     VIDTMP2+$02     ; (2) sound Ch.A duration (in 1/100s)
CHBSNDDTN       equ     CHASNDDTN+$02   ; (2) sound Ch.B duration (in 1/100s)
CHCSNDDTN       equ     CHBSNDDTN+$02   ; (2) sound Ch.C duration (in 1/100s)
KBDNPT          equ     CHCSNDDTN+$02   ; (1) temp cell used to flag if input comes from keyboard
KBTMP           equ     KBDNPT+$01      ; (1) temp cell used by keyboard scanner
TMPKEYBFR       equ     KBTMP+$01       ; (1) temp buffer for last key pressed
LASTKEYPRSD     equ     TMPKEYBFR+$01   ; (1) last key code pressed
STATUSKEY       equ     LASTKEYPRSD+$01 ; (1) status key, used for auto-repeat
KEYTMR          equ     STATUSKEY+$01   ; (2) timer used for auto-repeat key
CONTROLKEYS     equ     KEYTMR+$02      ; (1) flags for control keys (bit#0=SHIFT; bit#1=CTRL; bit#2=C=)
; - - - - - - - - - - - - - - - - - - -   ...TO HERE. DO NOT ADD ANYTHING RELATED TO PSG OUT OF THIS RANGE,
                                        ; OTHERWISE YOU WILL HAVE TO CHANGE THE POINTER IN "initPSG" FUNCTION
SERIALS_EN      equ     CONTROLKEYS+$01 ; (1) serial ports status: bit 0 for Port1 (A), bit 1 for Port2 (B): 0=OFF, 1=ON
SERABITS        equ     SERIALS_EN+$01  ; (1) serial port A data bits
SERBBITS        equ     SERABITS+$01    ; (1) serial port B data bits
DOS_EN          equ     SERBBITS+$01    ; (1) DOS enable/disable (1/0)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                        ; from here there are the RAM locations that
                                        ; are saved during SAVE
PROGND          equ     DOS_EN+$01      ; (2) End of program
VAREND          equ     PROGND+$02      ; (2) End of variables
ARREND          equ     VAREND+$02      ; (2) End of arrays
NXTDAT          equ     ARREND+$02      ; (2) Next data item
FNRGNM          equ     NXTDAT+$02      ; (2) Name of FN argument
FNARG           equ     FNRGNM+$02      ; (4) FN argument value
FPREG           equ     FNARG+$04       ; (3) Floating point register
FPEXP           equ     FPREG+$03       ; (1) Floating point exponent
SGNRES          equ     FPEXP+$01       ; (1) Sign of result
PBUFF           equ     SGNRES+$01      ; (13) Number print buffer
MULVAL          equ     PBUFF+$0D       ; (3) Multiplier
PROGST          equ     MULVAL+$03      ; (100) Start of program text area
STLOOK          equ     PROGST+$64      ; Start of memory test
