; ------------------------------------------------------------------------------
; LM80C 64K - BIOS ROUTINES - R1.03
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
; R1.0  - 20210307 - first release
; R1.01 - 20210310 - Code optimizing & better error handling
; R1.02 - 20210316 - disk speed improvements with INIR and OTIR
; R1.03 - 20210319 - code re-organization and new positioning into RAM
;
;------------------------------------------------------------------------------

; equates for CF at port $50
CF_DATA:        equ %01010000   ; ($50) Data register (R/W)
CF_ERR:         equ %01010001   ; ($51) Error (R)
CF_FTR:         equ %01010001   ; ($51) Features (W)
CF_SECCNT:      equ %01010010   ; ($52) Sector count register (R/W)
CF_LBA0:        equ %01010011   ; ($53) LBA register 0 (bits 0-7) (R/W)
CF_LBA1:        equ %01010100   ; ($54) LBA register 1 (bits 8-15) (R/W)
CF_LBA2:        equ %01010101   ; ($55) LBA register 2 (bits 16-23) (R/W)
CF_LBA3:        equ %01010110   ; ($56) LBA register 3 (bits 24-27) (R/W) - bits 28-31 must be set to 111 in LBA mode
CF_STAT:        equ %01010111   ; ($57) Status (R)
CF_CMD:         equ %01010111   ; ($57) Command register (W)


;------------------------------------------------------------------------------
; R O U T I N E S
;------------------------------------------------------------------------------

BIOSSTART:  equ     $
; initilialize CF to work with, wakeing it up from standby and setting it to work in 8-bit mode
CF_INIT:    call    CF_NOP          ; execute a NOP to wake up the CF
            call    CR_DEV_RDY      ; wait for CF available and ready
            ret     C               ; no card or I/O error, leave
            ld      A,$01           ; 8-bit mode
            out     (CF_FTR),A      ; set mode
            call    CF_BUSY         ; wait for CF being ready
            ld      A,$EF           ; command to set mode
            out     (CF_CMD),A      ; execute command
            call    CF_BUSY         ; wait for CF being ready
            ret                     ; return to caller


; a NOP command, just used to wake up the CF card 
CF_NOP:     ld      A,$69           ; NOP command
            out     (CF_FTR),A      ; send it
            ld      A,$EF           ; set mode command
            out     (CF_CMD),A      ; execute NOP
            ret                     ; return to caller


; wait until BUSY bit is 0 (means CF has executed the requested job)  
CF_BUSY:    in      A,(CF_STAT)     ; read status register
            rlca                    ; copy bit #7 into the Carry
            jp      C,CF_BUSY       ; loop while bit #7 is 1
            ret                     ; bit #7 cleared - return to caller


; check that CF is ready to get commands
CF_CMDRDY:  in      A,(CF_STAT)     ; read status register
            bit     0,A             ; any error?
            jr      NZ,RETERR       ; yes, return error
            and     %11000000       ; check only bits #6 & #7
            xor     %01000000       ; bit #7 (BUSY) must be 0 and bit #6 (DRVRDY) must be 1
            jr      NZ,CF_CMDRDY    ; wait
            ret                     ; return to caller
RETERR:     scf                     ; set carry flag
            ret                     ; return


; wait until data is ready to be read
CF_DAT_RDY: in      A,(CF_STAT)     ; read status register
            bit     0,A             ; any error?
            jr      NZ,RETERR       ; yes, return error
            and     %10001000       ; check only bits #7 & #3
            xor     %00001000       ; bit #7 (BUSY) must be 0 and bit #3 (DRQ) must be 1
            jr      NZ,CF_DAT_RDY   ; wait until data is ready
            ret                     ; return to caller


; set sector to read from/write to - sector number is into DEBC (C=LSB, D=MSB)
CF_SETSTR:  call    CF_CMDRDY       ; Make sure drive is ready for command
            ld      A,$01           ; 1 sector at a time
            out     (CF_SECCNT),A   ; set number of sectors
            call    CF_CMDRDY       ; Make sure drive is ready for command
            ld      A,C             ; load LBA0 byte
            out     (CF_LBA0),A     ; send it
            call    CF_CMDRDY       ; Make sure drive is ready for command
            ld      A,B             ; load LBA1 byte
            out     (CF_LBA1),A     ; send it
            call    CF_CMDRDY       ; Make sure drive is ready for command
            ld      A,E             ; load LBA2 byte
            out     (CF_LBA2),A     ; send it
            call    CF_CMDRDY       ; Make sure drive is ready for command
            ld      A,$E0           ; load LBA3 byte+master+LBA addressing
            or      D               ; add LBA sector
            out     (CF_LBA3),A     ; send it
            ret                     ; return to caller


; check if device is available & ready - try a bit of times, then exit with
; error if no response, otherwise wait until device is ready
; return Carry = 0 if device is available and ready, Carry = 1 if errors
CR_DEV_RDY: push    BC              ; store HL
            ld      B,$00           ; 256 tries
            ld      C,CF_STAT       ; address of status register
CR_DV_RD_1: in      A,(C)           ; load status register (curiously, with no CF attached, in(CF_STAT) returns %01111000)
            cp      %01000000       ; busy=0, rdy=1
            jr      Z,CR_DV_RD_E    ; got a response, so leave
            cp      %01010000       ; busy=0, rdy=1, dsc=1
            jr      Z,CR_DV_RD_E    ; got a response, so leave
            djnz    CR_DV_RD_1      ; repeat until timeout (Carry=1 while HL<DE)
            scf                     ; exit with Carry = 1 (device NOT ready)
CR_DV_RD_E: pop     BC              ; retrieve HL
            ret                     ; return to caller


; put the CF into stand-by mode
CF_STANDBY: ld 	    A,$E0   		; select CF as master, driver 0, LBA mode (bits #5-7=111) 
            out 	(CF_LBA3),A     ; send configuration
            ld      A,$92           ; standby mode
            out     (CF_CMD),A      ; send command
            call    CF_BUSY         ; wait for CF being ready
            ret                     ; return to caller


;***************************************************************************
; CF_RD_SEC
; Function: load a sector (512 bytes) into RAM buffer.
;***************************************************************************			
CF_RD_SEC:  call    CF_CMDRDY       ; Make sure drive is ready for command
            ret     C               ; return if error
            ld      A,$20           ; Prepare read command
            out     (CF_CMD),A      ; Send read command
            call    CF_DAT_RDY      ; Wait until data is ready to be read
            ret     C               ; return if error          
            in      A,(CF_STAT)     ; Read status
            and     %00000001       ; mask off error bit
            jp      NZ,CF_RD_SEC    ; Try again if error
; read CF buffer after it's been filled up by a previous command
; and store data into the I/O buffer
CF_RD_CMD:  push    BC              ; store BC
            push    HL              ; store HL
            call    CF_DAT_RDY	    ; wait for data from CF to be ready
            jr      C,CF_RD_EXIT    ; if error, leave
            ld      BC,CF_DATA      ; set 256 bytes per loop (B=$00) and CF port (C=CF_DATA)
            ld      HL,IOBUFF       ; get starting address of I/O buffer
            inir                    ; get 256 bytes
            inir                    ; get 256 bytes
CF_RD_EXIT: pop     HL              ; retrieve HL
            pop     BC              ; retrieve BC
            ret                     ; return to caller


;***************************************************************************
; CF_WR_SEC
; Function: write a sector to Compact Flash - sector address is into BCDE - source address is into HL
;***************************************************************************
CF_WR_SEC:  push    BC              ; store BC
            push    HL              ; store HL
            call    CF_SETSTR       ; set sector
            call    CF_CMDRDY       ; Make sure drive is ready for command
            jr      C,CF_WR_EXIT    ; return if error
            ld      A,$30           ; set write command
            out     (CF_CMD),A      ; send command
            call    CF_DAT_RDY      ; Make sure drive is ready to get data
            jr      C,CF_WR_EXIT    ; return if error
            ld 	    HL,IOBUFF       ; get starting address of I/O buffer
            ld      BC,CF_DATA      ; set 256 bytes per loop (B=$00) and CF port (C=CF_DATA)
            otir                    ; output 256 bytes
            otir                    ; output 256 bytes
            call    CF_BUSY         ; wait for CF to complete the writing
            xor     A               ; clear Carry
CF_WR_EXIT: pop     HL              ; retrieve HL
            pop     BC              ; retrieve BC
            ret                     ; return to caller

BIOSEND:    equ     $               ; end of BIOS