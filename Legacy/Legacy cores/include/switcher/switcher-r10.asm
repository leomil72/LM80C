; ------------------------------------------------------------------------------
; LM80C - ROM/RAM SWITCHER - R1.0
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
;-------------------------------------------------------------------------------

TMP_FW_LOC:     equ     $8040           ; address where to make a temporary copy of the FW
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
                ld      A,%11001111     ; set mode 3 ("control mode")
                out     (PIO_CB),A      ; for PIO port B
                xor     A               ; set LEDs off, RAM on, VRAM on bank #0
                out     (PIO_DB),A      ; send settings to PIO register
                out     (PIO_CB),A      ; set pins following register's status
                ld      HL,STACK        ; temporary stack
                jp      INIT_HW2        ; jump to re-init HW
CNTCP2RAM:      ld      HL,TMP_FW_LOC-2 ; load temp stack pointer
                ld      SP,HL           ; set stack to temp stack pointer
                call    INITPIO
                ld      BC,END_OF_FW    ; copy FW from ROM to high RAM 
                ld      HL,$0000        ; source address
                ld      DE,TMP_FW_LOC   ; dest. address
                ldir                    ; copy!
                jp      RAMRUN+TMP_FW_LOC; jump to execute code into RAM


;-------------------------------------------------------------------------------
END_OF_FW       equ     $   ; this is the last cell of the firmware