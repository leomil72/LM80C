; ------------------------------------------------------------------------------
; LM80C - FIRMWARE - R2.10
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
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Parts of the code (c) Mario Blunk
; http://www.train­z.de
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; NASCOM ROM BASIC Ver 4.7, (C) 1978 Microsoft
; Scanned from source published in 80-BUS NEWS from Vol 2, Issue 3
; (May-June 1983) to Vol 3, Issue 3 (May-June 1984)
; Adapted for the freeware Zilog Macro Assembler 2.10 to produce
; the original ROM code (checksum A934H). PA
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
; R1.3   - 20190521 - Added preliminary support for video cursor management
; R1.4   - 20190524 - Added scrolling capabilities
; R1.5   - 20190525 - Added backspace functionality
; R1.6   - 20190601 - Fixed scrolling bugs
; R1.7   - 20190606 - Added "screen" command; code revision
; R1.8   - 20190615 - Better cursor integration; added VPOKE & VPEEK statements; 6x8 & 8x8 fonts
; R1.9   - 20190620 - New VREG, VSTAT, & LOCATE statement; 8x8 pixels font completed
; R2.0   - 20190714 - Added SREG & SSTAT to write to/read from PSG
; R2.1   - 20190818 - Added SOUND command to play simple tones and VOLUME command
; R2.1a  - 20190908 - Cursor management improvements
; R2.2   - 20190920 - Fixed cursor bug within SCREEN statement; new command PAUSE
; R2.3   - 20190930 - Fixed bugs in SOUND command
; R2.4   - 20191013 - Added new graphic chars and reorganized previous ones
; R2.4a  - 20191015 - More graphic chars
; R2.5   - 20191026 - Revision of init PSG code; revision of serial buffer exp. code;
;                     fixed a bug into the video buffer manager
; R2.6   - 20191102 - New function INKEY to read a key without a prompt;
;                     source code cleaning
; R2.7   - 20191116 - Fixed a bug into the INKEY code
; R2.8   - 20191207 - Minor bug fixes; added support for built-in keyboard;
;                     revision of some char codes;
; R2.8.1 - 20191208 - Introduced support for SHIFT key for uppercase letters & alternate chars
; R2.8.2 - 20191215 - Fixed a bug introduced with 2.8.1 that lead to wrong functioning of
;                     several BASIC statements (system tick counter, Locate, etc..)
; R2.9   - 20192212 - Code cleaning; improved SOUND statement; revision of PSG code;
;                     revision of release notes; add support for cursor keys & cursor movements
; R2.9   - 20191222 - Code cleaning; improved SOUND statement; revision of PSG code;
;                     revision of release notes; add support for cursor keys & cursor movements
; R2.10  - 20191226 - SIO init code cleaning & improved support for serial RX; added extended
;                     char codes (128-255) for 6x8 fonts; removed double chars in 8x8 fonts
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
#include "../include/bootloader/bootloader-r210.asm"

; incude the latest version of the VDP module
#include "../include/vdp/vdp-r210.asm"

; incude the latest version of the PSG module
#include "../include/psg/psg-r210.asm"

; include the latest version of the NASCOM BASIC interpreter
#include "../include/basic/basic32k-r210.asm"

; include the latest version of the font sets
#include "../include/vdp/6x8fonts-r13.asm"
#include "../include/vdp/8x8fonts-r16.asm"

; END OF ASSEMBLY SOURCE
#end

;-------------------------------------------------------------------------------
