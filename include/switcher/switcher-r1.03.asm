; ------------------------------------------------------------------------------
; LM80C - ROM/RAM SWITCHER - R1.03
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
; R1.0   - 20200718 - First version
; R1.1   - 20200827 - PIO settings now are part of the file
; R1.02  - 20210319 - support for LM80C DOS and its repositioning into high-RAM
; R1.03  - 20210408 - code revision
;-------------------------------------------------------------------------------

TMP_FW_LOC:     equ     $8000           ; address from where to make a temporary copy of the FW
;-------------------------------------------------------------------------------
; THIS CODE WILL BE EXECUTED FROM RAM
RAMRUN:         ld      A,%11111100     ; set ROM off and RAM on..
                out     (PIO_DB),A      ; ...by setting bit #0 to 0 (and leave VRAM bank #0)
                ld      BC,END_OF_FW    ; let's copy back FW into low RAM - bytes to copy
                ld      HL,TMP_FW_LOC   ; source address
                ld      DE,$0000        ; dest. address
                ldir                    ; copy!
                xor     A
                out     (PIO_DB),A      ; all LEDs off - finished copying
                jp      INIT_HW         ; return control to old init (now into RAM)

;-------------------------------------------------------------------------------
; copy the whole contents of ROM into RAM then disable the first memory
ROM2RAM:        ld      A,(basicStarted); check if BASIC is already started
                cp      'Y'             ; to see if this is a power-up
                jr      NZ,CNTCP2RAM    ; no, continue copy to RAM
                ; WARNING: Do **NOT** change the following "out()" sequence, ABSOLUTELY!
                ld      A,%11001111     ; set mode 3 ("control mode")
                out     (PIO_CB),A      ; for PIO port B
                xor     A               ; set LEDs off, RAM on, VRAM on bank #0
                out     (PIO_DB),A      ; send settings to PIO register
                out     (PIO_CB),A      ; set pins following register's status
                jp      INIT_HW         ; jump to re-init HW

                ; WARNING: Do **NOT** change the following "out()" sequence, ABSOLUTELY!
CNTCP2RAM:      ld      A,%11001111     ; set mode 3 (mode control)
                out     (PIO_CB),A      ; for PIO port B
                ld      A,%11111101     ; set pin #0 as HIGH to enable ROM
                out     (PIO_DB),A      ; store the value into the internal register
                xor     A               ; set pins to OUTPUT
                out     (PIO_CB),A      ; for port B, activating the RAM
                ; copy DOS
                ld      BC,$FFFF-DOSSTART+1 ; bytes to copy
                ld      HL,END_OF_FW    ; load DOS from its original location and...
                ld      DE,DOSSTART     ; ...store it into its portion of memory
                ldir                    ; copy!
                ; copy BASIC
                ld      BC,END_OF_FW    ; copy FW from ROM to high RAM 
                ld      HL,$0000        ; source address
                ld      DE,TMP_FW_LOC   ; dest. address
                ldir                    ; copy!
                jp      RAMRUN+TMP_FW_LOC; jump to execute code into RAM

;-------------------------------------------------------------------------------
END_OF_FW:      equ     $   ; this is the last cell of the firmware