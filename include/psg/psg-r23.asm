; ------------------------------------------------------------------------------
; LM80C - PSG ROUTINES - R2.3
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Code Revision:
; R2.1  - 20190818 - Added SOUND command to play simple tones and VOLUME command
; R2.1a - 20190908 - Cursor management improvements
; R2.2  - 20190920 - Fixed cursor bug within SCREEN statement; new command PAUSE
; R2.3  - 20190930 - Fixed bugs in SOUND command
;
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; configure the PSG
initPSG:        push    AF          ; store AF
                push    BC          ; store BC
                push    DE          ; store DE
                ld      B,$07       ; first 6 registers
                xor     A           ; load 0 into A
                ld      D,A         ; load 0 into D
RSTPSG1:        ld      C,PSG_REG   ; PSG register port
                out     (C),D       ; set register
                ld      C,PSG_DAT   ; PSG data port
                out     (C),A       ; set register to 0
                inc     D           ; next register
                djnz    RSTPSG1     ; repeat for each register
                ld      A,00111111b ; set I/O channels to INPUT, set noise to OFF, set audio to OFF
                ld      C,PSG_REG   ; PSG register port
                out     (C),D       ; D=7 and 7 is mixer register
                ld      C,PSG_DAT   ; PSG data register
                out     (C),A       ; set mixer
                ld      B,$07       ; last 6 registers (regs. 14&15 are not to be changed, they are the I/O ports)
                xor     A           ; load 0 into A
                ld      D,$08       ; load 8 into D
RSTPSG2:        ld      C,PSG_REG   ; PSG register port
                out     (C),D       ; set register
                ld      C,PSG_DAT   ; PSG data port
                out     (C),A       ; set register to 0
                inc     D           ; next register
                djnz    RSTPSG2     ; repeat for each register
                pop     DE          ; retrieve DE
                pop     BC          ; retrieve BC
                pop     AF          ; retrieve AF
                ret                 ; return to caller

                
; manage the sounds' duration: each time this subroutine is called, it
; decrements the single sound durations (measured in ms) and eventually
; shut off the audio channel whose counter has reached 0.
; (this sub-routine is called by CH3 timer ISR)
MNGSNDS:        push af             ; store A
                push bc             ; store BC
                push de             ; store DE
                push hl             ; store HL
                push ix             ; store IX
                ld ix,CHASNDDTN     ; starting address of tones duration
                ld b,03H            ; 3 channels to check
                ld h,01h            ; channel A=>bit 1 into mixer, B=>bit 2, C=>bit 3
CHKSNDCH:       ld e,(ix+0)         ; load LSB into E
                ld d,(ix+1)         ; load MSB into D
                ld a,e              ; load E into A
                or d                ; do E OR D, to check that DE=0 (if just a bit is 1, the result will be <> 0)
                jr z,CNTCHKSND      ; yes, jump over
                dec de              ; no, so decrement DE
                ld a,e              ; reload E into A...
                ld (ix+0),e         ; store new...
                ld (ix+1),d         ; ...duration and...
                or d                ; ...do another check to see if DE=0
                jr nz,CNTCHKSND     ; no, so jump over
                                    ; yes, let's shut down the corresponding channel
                                    ; to shut down a tone we set 0 into tone register
                                    ; and disable the channel into mixer
                ld a,03h            ; three channels
                sub b               ; find current channel (0->A, 1->B, 2->C)
                add a,a             ; and find first register (A=>0, B=>2, C=>4)
                ld c,PSG_REG        ; PSG register selector port
                out (c),a           ; select first tone register of channel
                ld l,00h            ; value 0 into L
                ld c,PSG_DAT        ; PSG data selector port
                out (c),l           ; write 0 into register
                ld c,PSG_REG        ; PSG register selector port
                inc a               ; next tone register
                out (c),a           ; select second tone register of channel
                ld c,PSG_DAT        ; PSG data selector port
                out (c),l           ; write 0 into register
                ld a,$07            ; mixer register
                ld c,PSG_REG        ; PSG register selector port
                out (c),a           ; set mixer register
                ld c,PSG_DAT        ; PSG data port
                in a,(c)            ; load current value
                or h                ; set off the channel into the mixer (remember that 1=OFF)
                out (c),a           ; send new value for the mixer
CNTCHKSND:      inc ix              ; set for...
                inc ix              ; ...next channel...
                sla h               ; shift H 1 bit to left
                djnz CHKSNDCH       ; repeat for 3 channels
                pop ix              ; restore IX
                pop hl              ; restore HL
                pop de              ; restore DE
                pop bc              ; restore BC
                pop af              ; restore AF
                ret                 ; return to caller
