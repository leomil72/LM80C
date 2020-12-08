<a href="https://www.leonardomiliani.com/en/lm80c/"><img src="https://raw.githubusercontent.com/leomil72/LM80C/master/lm80c_inside.jpg" title="LM80C, a Z80-based computer" alt="LM80C, a Z80 -based computer"></a>


# LM80C Color Computer - A Z80-based homebrew computer

> This is the official repo of the **LM80C Color Computer**, an 8-bit home computer based on the Z80 CPU entirely developed in 2019 by **Leonardo Miliani**. Here you can find schematics, code and other stuff that you can use to replicate this project and build your own homebrew computer as a sort of "back to the old, good, days of 8-bit systems".
>
> Project main page: [LM80C](http://www.leonardomiliani.com/en/lm80c/)
>
> Tags: z80, zilog, homebrew, 8-bits, computer, lm80c, ctc, pio, sio, tms9918a, ay-3-8910, ym2149f

---

## Getting started

- [Overview](#overview)
- [What you need](#what%20you%20need)
- [Files](#files)
- [Compilation](#compilation)
- [Emulation](#emulation)
- [FAQ](#faq)
- [Support](#support)
- [Donations](#donations)
- [License](#license)

---

## Overview

The **LM80C** is an 8-bit home-computer built around the Zilog Z80 CPU with video and audio capabilities. It's a stand-alone system thanks to its integrated keyboard that permits to use it without the necessity of host computers, usually used as input/output devices in other similar projects. With the LM80C BASIC you can write your own games, programs, and much more.

### Main features

- CPU: Zilog Z80B@3.68 MHz
- Memory:
  - ROM: 32KB (with built-in firmware & BASIC)
  - RAM: 32KB SRAM
- Video:
  - TMS9918A with 16KB of dedicated VRAM
  - composite output
  - 256x192 pixels, 15 colors, 32 sprites
- Audio:
  - Yamaha YM2149F (or GI AY-3-8910)
  - 3 analog channels with tone&noise generation
  - envelope control
  - 2x8-bit I/O ports
- I/O capabilities:
  - Z80 PIO:
    - parallel input/output periphery
    - 2x8-bit ports
    - 4 operatin modes
  - Z80 SIO:
    - serial input/output periphery
    - 2 serial ports
    - higly configurable
    - software-adjustable baude rates
  - Z80 CTC:
    - timer/counter with 4 channels
   - generate system ticks and baud rates
- Keyboard:
  - external keyboard with 64 keys (I used a Commodore 16 keyboard, that has 66 keys but only 64 unique keys due to double connections for SHIFT and SHIFT/LOCK)
- More to come:
  - support for SD memory cards

<img src="https://raw.githubusercontent.com/leomil72/LM80C/master/lm80c_key_n_inside.jpg" title="LM80C motherboard inside its case" alt="LM80C motherboard inside its case">

### BASIC

The **LM80C BASIC** is a dialect of the BASIC, a versatile and user-friendly programming language, the de-facto standard of the '80s since it was integrated into almost every 8-bit home computer. It derives from Nascom BASIC, that in turn derives from Microsoft Z80 BASIC 4.7. The LM80C BASIC is more than just a derivative language: in fact, it has its own set of commands and functions to take advantage of the specific LM80C hardware, like statements to draw point, lines, and circles, change foreground and background colors, move the cursor, play a tone or a noise, open/close the serial port, etc. It lets you to write good BASIC games, like "Lunar Lander", see below:
<img src="https://raw.githubusercontent.com/leomil72/LM80C/master/lunarlander.jpg" title="Lunar Lander on LM80C" alt="Lunar Lander on LM80C">

---

## What you need

Before to start building your own LM80C computer you need several things:
- of course, you need some hardware (electronic components, solder, breadboards, chips, etc.);
- some software (code editor & assembler);
- a CAD (to open schematics files and to generate Gerber files to make the PCB).
- some skills (you may be able to assemble a prototype, program it, burn a ROM file, etc.).

### Software
Personally I'm using [Visual Studio Code](https://code.visualstudio.com/) to write the ASM files: it's free, based on an open source project, and multi-platform (do not confuse with Visual Studio IDE). The extension to highlight the assembly code is [Z80 ASM](https://github.com/Imanolea/z80asm-vscode). Visual Code is a very complete editor and you can use for every language you use. Another good editor is [ATOM editor](https://atom.io) to edit the code, with [Language-assembler-SJASMPLUS](https://atom.io/packages/language-assembler-sjasmplus) package to highlight the Z80 assembly code. I switched from ATOM to Visual Studio Code because the latter is lighter. 

To compile binaries I use [ZASM Assembler](http://k1.spdns.de/Develop/Projects/zasm/Distributions/), a multi-platform Z80 assembler that has a lot of features, like macros, C files inclusion and much more. It runs on Linux and MacOS, but you can use it on a Windows-based system just by running a Linux instance inside a virtual machine: almost everyone can compile the code with it, and obtain the same binary files. However, for your commodity, the latest compiled binary file, ready to be burned into an EEPROM, is always available. 

To open/modify schematics you need a CAD: I used [KiCad](http://www.kicad-pcb.org/), that (IMHO) is the best choice, since it's free and doesn't have the limitations of the freeware version of [EagleCAD](https://www.autodesk.com/products/eagle/overview), that can't let you create a board bigger than 80 square cms., and the LM80C PCB is much bigger. However, I've also added PDF files if you just want to look at the schematics but don't want/need any additional software to open the schematics.

### Hardware
To burn the binaries on the (E)EPROM chips I used a modified version of [TommyPROM](https://github.com/leomil72/TommyPROM) so that I can use 74HCT595 shift registers, but you can use any (E)EPROM programmer you have. If you want to replicate my programmer, you need an [Arduino/Genuino Uno](https://www.arduino.cc) board. Please buy a *genuine board*, not a counterfit one, to help the open-source project. 

### Prototyping
I strongly reccomend you to assemble a prototype on breadboards to be sure that everything is working right. The pictures below represent the LM80C during its development:
<img src="https://raw.githubusercontent.com/leomil72/LM80C/master/12-Home%20computer/LM80C.jpg" title="LM80C prototype, built upon breadboards" alt="LM80C prototype, built upon breadboards">

<img src="https://raw.githubusercontent.com/leomil72/LM80C/master/keyboard.jpg" title="LM80C keyboard over the breaboards" alt="LM80C keyboard over the breaboards">

---

## Files
The files in this repo are organized in folders:
- "12-Home Computer": this folder contains the main ASM file to compile the current firmware of the LM80C Color Computer.
- "BASIC examples": this folder contains BASIC programs that can be loaded and executed with the integrated LM80C BASIC interpreter. If the name contains a release version (i.e. R20), it means it's the minimum firmware release required to run such program, because it makes use of some statements not present in previous releases.
-  "Legacy": old firmware are stored into this folder. Folders whose names start with "01-" through "09-" contain the first tests I made with the computer without the video section, that you can replicate to work solely through the serial line. Folders whose names start with "10-" and "11-" contain the releases that had preliminary support for video output. "Legacy cores" contains the old firmwares of the latest hardware version of the computer. Every release adds some functionality to the original BASIC. The greater the release number is, the newer the firmware is.
- "Rom": this folder contains pre-compiled binary files, ready to be burned into the EEPROM chip.
- "Schematics": this folder contains the KiCad files of the current hardware revision of the computer, plus some PDF files including the complete schematics of the computer.
- "include": this folder contains the sub-folders with the files needed to compile the firmware. They are: "basic", "bootloader","psg", and "vdp". Each sub-folder contains the file of the corresponding module.
- "manuals": this folder contains useful manuals for the CPU, the peripheral chips, the video and audio chips, and a couple of  files that illustrate the main features of the LM80C and the reference manual of the LM80C BASIC language.

---

## Compilation
To compile the firmware, first download or clone the repo on your computer, than go into the "12-Home computer" folder and open a terminal in it, then give this command:
`zasm -uwy --z80 ./LM80C-firmware-rXX.asm`
where "XX" is the release you want to compile. After the compilation has finished, you'll find a file with extension `.rom` that you can burn into the EEPROM.


---

## Emulation
There is an [LM80C online emulator](https://github.com/nippur72/lm80c-emu), written in Javascript by Antonino Porcino, that can be used to see how the real machine acts and works: you can test directly in your browser by opening [this link](https://nippur72.github.io/lm80c-emu/).

---

## FAQ

- **How can I clone this repo?**
  - On your desktop computer, by using a GUI frontend like SmartGit, or via a terminal by typing the following command ```git clone https://github.com/leomil72/LM80C```
  - Online, by clicking on the "Fork" button
- **How can I download the files?**
  - By clicking the green top-right "Clone or Download" and then by clicking on "Download ZIP"
- **How do I write and compile an ASM source file?**
  - You need an editor to write the source code and then an 'assembler' to compile the binary file. Please look at the "Software" section above.
- **How do I burn the binary file into the EEPROM?**
  - You need an (E)EPROM programmer, a device that can be connected to your computer and that can write the data into the memory chip. You can buy a programmer from internet or build your own device like me.
- **Where can I find the chips you're using in the LM80C computer?**
  - Zilog still manufactures the Z80 family chips, so you can get them from almost any good reseller, or, alternatively, new/used parts can be catched from online auctions sites. The 74HCTxx integrated are very common, so you shouldn't have any problem to grab them. The VDP and PSG chips are only available as used parts from several eastern resellers. Pay attention that you can easily buy a counterfit chip so if it won't work, before to think that your computer is badly assembled, try another part.
- **The software seems not to be working/the hardware I built following your schematics doesn't work: where can I find help? Can I write to you?**
  - The project is released "as is", without any warranty/responsability: despite I've tested it and I'm currently using it, I can not guarantee that it is completely error-free, nor that it can run with no issues for a relitively long time, nor that there are no errors in the code/schematic files that are online. So, please do NOT write me to complain about something's not working. Open an "issue" or a "pull request". Thank you.

---

## Support

You can find more about me on my website at <a href="https://www.leonardomiliani.com/en/" target="_blank">www.leonardomiliani.com</a>.

---

## Donations

- If you want to help me maintain my site, my projects, and keep me rolling on, you may consider to make a small donation with <a href="https://www.paypal.me/LeonardoMiliani" target="_blank">Paypal</a>.

---

## License

- Some portions of code are copyright <a href="http://searle.hostei.com/grant/" target="_blank">Grant Searle</a>
- Some portions of code are copyright <a href="http://www.train­z.de" target="_blank">Mario Blunk</a>
- BASIC code is copyright <a href="http://www.microsoft.com" target="_blank">Microsoft</a>
- Some commercial names (like KiCAD, EagleCAD, Visual Studio Code) lead to the corresponding owners.
- The rest of the stuff is my own work and is distribuited under the [Gnu GPL Licence 3.0](https://opensource.org/licenses/GPL-3.0).

The names "LM80C", "LM80C Color Computer", and "LM80C BASIC", the "rainbow LM80C" logo, the LM80C schematics, the LM80C sources, and this work belong to Leonardo Miliani. 

The "rainbow LM80C" logo and the "LM80C/LM80C Color Computer" names can not be used in any work without my explicit permission. You are allowed to use the "LM80C" name only in reference to or to describe the LM80C Color Computer.

The LM80C schematics and source codes are released under the GNU GPL License 3.0 and in the form of "as is", without any kind of warranty: you can use them at your own risk. You are free to use them for any non-commercial use: you are only asked to maintain the copyright notices, to include this advice and, if you intend to re-distribute them, the note to the attribution of the original works to Leonardo Miliani. For any other use, please contact me by opening an issue.
