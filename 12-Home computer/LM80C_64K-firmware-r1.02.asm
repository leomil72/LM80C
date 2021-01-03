; ------------------------------------------------------------------------------
; LM80C 64K - FIRMWARE - R1.02
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
; Edited with Visual Studio Code
;
; Compiled with SjASMPlus assembler 1.18.0
; https://github.com/z00m128/sjasmplus
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
; output binary file
    OUTPUT "LM80C-64K-firmware-r1.02.bin"

; set computer model
    DEFINE  LM80C64K

; ------------------------------------------------------------------------------
; include the latest version of the bootloader: this sets up the address aliases
; configure the hardware, checks if warm or cold startup and loads the BASIC interpreter
    INCLUDE "../include/bootloader/bootloader64k-r1.02.asm"

; incude the latest version of the VDP module
    INCLUDE "../include/vdp/vdp64k-r1.0.asm"

; incude the latest version of the PSG module
    INCLUDE "../include/psg/psg64k-r1.0.asm"

; include the latest version of the LM80C 64K BASIC interpreter
    INCLUDE "../include/basic/basic64k-r1.01.asm"

; include utils
    INCLUDE "../include/utils/utils-r11.asm"

; include the latest version of the font sets
    INCLUDE "../include/vdp/6x8fonts-r16.asm"
    INCLUDE "../include/vdp/8x8fonts-r18.asm"
    INCLUDE "../include/vdp/logo-fonts.asm"

; include ROM/RAM switcher
    INCLUDE "../include/switcher/switcher-r11.asm"

; include workspace equates
    INCLUDE "../include/workspace/workspace-r1.0.asm"

; END OF ASSEMBLY SOURCE
;-------------------------------------------------------------------------------
