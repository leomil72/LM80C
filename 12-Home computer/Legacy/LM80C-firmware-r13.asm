; ------------------------------------------------------------------------------
; LM80C - FIRMWARE - R1.3
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
; R1.3 - 20190521 - Added preliminary support for video cursor management (to act like an home computer)
; ------------------------------------------------------------------------------
; NASCOM BASIC versions:
; 4.7  - original version by NASCOM
; 4.7b - modified version by Grant Searle (additional commands & functions)
; 4.7c - modified by Leonardo Miliani (1/100 secs timer)
; -----------------------------------------------------------------------------

; this line instructs the assembler to prepare a file for a ROM target
; meaning that blank cells will be filled up with 0xff
#target rom

; ------------------------------------------------------------------------------
; include the latest version of the bootloader: this sets up the address aliases
; configure the hardware, checks if warm or cold startup and loads the BASIC interpreter
#include "../include/bootloader/bootloader-r12.asm"

; configure the VDP
#include "../include/vdp/vdp-r13.asm"

; inclue the latest version of the NASCOM BASIC interpreter
#include "../include/basic/basic32k-r15.asm"

; include the latest version of the 6x8 fontset
#include "../include/vdp/6x8fonts.asm"

; END OF ASSEMBLY SOURCE
#end

;-------------------------------------------------------------------------------
