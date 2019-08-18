; ------------------------------------------------------------------------------
; LM80C - PSG ROUTINES - R2.1
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Code Revision:
; R2.1 - 20190818 - Added SOUND command to play simple tones and VOLUME command
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
                push ix             ; store IX
                ld ix,CHASNDDTN     ; starting address of tones duration
                ld b,03H            ; 3 channels to check
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
                ld c,PSG_REG        ; yes, let's shut down the corresponding channel
                ld a,0bh            ; first volume register (8) + 3 = 11
                sub b               ; find current channel's volume register (8->A, 9->B, 10->C)
                out (c),a           ; set PSG register
                xor a               ; volume level set to 0
                ld c,PSG_DAT        ; output port
                out (c),a           ; send value to PSG
CNTCHKSND:      inc ix              ; set for...
                inc ix              ; ...next channel...
                djnz CHKSNDCH       ; repeat for 3 channels
                pop ix              ; restore IX
                pop de              ; restore DE
                pop bc              ; restore BC
                pop af              ; restore AF
                ret                 ; return to caller
;
;                ld hl,CHASNDDTN+5   ; load address of third channel's duration
;                ld b,03H            ; 3 channels to check
;CHKSNDCH:       ld d,(hl)           ; load LSB into D
;                dec hl              ; decrement HL
;                ld e,(hl)           ; load MSB into E
;                ld a,e              ; move E into A
;                cp 00h              ; is it zero?
;                jr z,CNTCHKSND      ; yes, jump over
;                dec a               ; no, decrement A
;                ld (hl),a           ; store new value of A
;                ld e,a              ; also in E
;                cp 00h              ; is it zero?
;                jr nz,CNTCHKSND     ; no, jump over
;                ld a,d              ; yes, let's decrement D
;                cp 00h              ; is it already zero?
;                jr z,CNTCHKSND      ; yes, jump over
;                dec a               ; decrement A
;                ld d,a              ; store value into D
;                cp 00h              ; is it zero?
;                jr nz,CNTCHKSND     ; no, let's continue
;                ld c,PSG_REG        ; yes, the sound has been entirely played, so let's shut down its volume
;                ld a,b              ; retrive channel number
;
;                ret                 ; return to caller
