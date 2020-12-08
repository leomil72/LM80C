; ------------------------------------------------------------------------------
; LM80C - FIRMWARE - R2.4
; ------------------------------------------------------------------------------
; The following code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Coding/Editing/Compiling:
; Original init code for MC68B05 by Grant Searle
; Original SIO/CTC/PIO init code by Mario Blunk
; NASCOM BASIC originally modified by Gran Searle
; Code modified and adapted for LM80C by Leonardo Miliani
;
; Edited with Atom Editor
;
; Compiled with ZASM assembler 4.2.4
; https://k1.spdns.de/Develop/Projects/zasm-4.0/Distributions/
; ------------------------------------------------------------------------------
; Copyright notes:
; Parts of the code (c) Grant Searle - free for non commercial use
; Please include this advice and the note to the attribution of the original
; version to Grant Searle if you intend to redistribuite it
; http://searle.hostei.com/grant/index.html
; eMail: home.micros01@btinternet.com
;
; Parts of the code (c) Mario Blunk
; http://www.train­z.de
;
; NASCOM ROM BASIC Ver 4.7, (C) 1978 Microsoft
; Scanned from source published in 80-BUS NEWS from Vol 2, Issue 3
; (May-June 1983) to Vol 3, Issue 3 (May-June 1984)
; Adapted for the freeware Zilog Macro Assembler 2.10 to produce
; the original ROM code (checksum A934H). PA
;
; Parts of the code by Leonardo Miliani
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Code Revision:
; R1.3  - 20190521 - Added preliminary support for video cursor management
; R1.4  - 20190524 - Added scrolling capabilities
; R1.5  - 20190525 - Added backspace functionality
; R1.6  - 20190601 - Fixed scrolling bugs
; R1.7  - 20190606 - Added "screen" command; code revision
; R1.8  - 20190615 - Better cursor integration; added VPOKE & VPEEK statements; 6x8 & 8x8 fonts
; R1.9  - 20190620 - New VREG, VSTAT, & LOCATE statement; 8x8 pixels font completed
; R2.0  - 20190714 - Added SREG & SSTAT to write to/read from PSG
; R2.1  - 20190818 - Added SOUND command to play simple tones and VOLUME command
; R2.1a - 20190908 - Cursor management improvements
; R2.2  - 20190920 - Fixed cursor bug within SCREEN statement; new command PAUSE
; R2.3  - 20190930 - Fixed bugs in SOUND command
; R2.4  - 20191013 - Added new graphic chars and reorganized previous ones
;
; -----------------------------------------------------------------------------

; this line instructs the assembler to prepare a file for a ROM target
; meaning that blank cells will be filled up with 0xff
#target rom

; this line instructs the assembler to compile taking account that code
; starts at $0000 (the address reached by Z80 upon reset)
#code BOOT, 0000h

; ------------------------------------------------------------------------------
; include the latest version of the bootloader: this sets up the address aliases
; configure the hardware, checks if warm or cold startup and loads the BASIC interpreter
#include "../include/bootloader/bootloader-r24.asm"

; incude the latest version of the VDP module
#include "../include/vdp/vdp-r24.asm"

; incude the latest version of the PSG module
#include "../include/psg/psg-r24.asm"

; include the latest version of the NASCOM BASIC interpreter
#include "../include/basic/basic32k-r24.asm"

; include the latest version of the font sets
#include "../include/vdp/6x8fonts.asm"
#include "../include/vdp/8x8fonts-r12.asm"

; END OF ASSEMBLY SOURCE
#end

;-------------------------------------------------------------------------------
