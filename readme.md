<a href="https://www.leonardomiliani.com/en/lm80c/"><img src="https://raw.githubusercontent.com/leomil72/LM80C/master/lm80c_inside.jpg" title="LM80C, a Z80-based computer" alt="LM80C, a Z80 -based computer"></a>


# LM80C - A Z80-based homebrew computer

> This repo contains schematics, code and other stuff that you can use to replicate this project and build your own Z80-based homebrew computer as a sort of "back to the old, good, days of 8-bit systems".
>
> Project main page: [LM80C](http://www.leonardomiliani.com/en/lm80c/)
>
> z80, zilog, homebrew, 8-bits, computer, lm80c, ctc, pio, sio

---

## Getting started

- [Overview](#overview)
- [What you need](#what%20you%20need)
- [Files](#files)
- [Compilation](#compilation)
- [Clone](#clone)
- [FAQ](#faq)
- [Support](#support)
- [Donations](#donations)
- [License](#license)

---

## Overview

The **LM80C** is an 8-bit home-computer built around the Zilog Z80 CPU with video and audio capabilities. It's a stand-alone system thanks to its integrated keyboard that permits to use it without the necessity of host computers, usually used as input/output devices in other similar projects. With the LM80C BASIC you can write your own games, programs and other.

### Main features

- CPU: Zilog Z80@3.68 MHz
- Memory:
  - ROM: 32KB with built-in firmware
  - RAM: 32KB SRAM
- Video:
  - TMS9918A with and 16KB of dedicated VRAM
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
  - external keyboard with 66 keys (a Commodore 16 keyboard is used)
- More to come:
  - support for SD memory cards

<img src="https://raw.githubusercontent.com/leomil72/LM80C/master/lm80c_key_n_inside.jpg" title="LM80C motherboard inside its case" alt="LM80C motherboard inside its case">

### BASIC

The **LM80C BASIC** is a versatile and user-friendly programming language, the de-facto standard of the '80s since it was integrated into almost every 8-bit home computer. It derives from Nascom BASIC, that in turn derives from Microsoft Z80 BASIC 4.7. The LM80C is more than just a derivative language: in fact, it has its own set of commands and functions to take advantage of the specific LM80C hardware, like statements to draw point, lines, and circles, change foreground and background colors, move the cursor, play a tone or a noise, open/close the serial port, etc. It lets to write good BASIC games, like "Lunar Lander", see below:
<img src="https://raw.githubusercontent.com/leomil72/LM80C/master/lunarlander.jpg" title="Lunar Lander on LM80C" alt="Lunar Lander on LM80C">

---

## What you need

Before to start  building your own LM80C computer you need several things:
- of course, you need some hardware;
- Editor & assembler
- A CAD to open schematics files

### Software
Personally I'm using [Visual Studio Code](https://code.visualstudio.com/) to write the ASM files: it's free, based on an open source project, and multi-platform (do not confuse with Visual Studio IDE). The extension to highlight the assembly code is [Z80 ASM](https://github.com/Imanolea/z80asm-vscode). Visual Code is a very complete editor and you can use for every language you use. Another good editor is [ATOM editor](https://atom.io) to edit the code, with [Language-assembler-SJASMPLUS](https://atom.io/packages/language-assembler-sjasmplus) package to highlight the Z80 assembly code. I switched from ATOM to Visual Studio Code because the latter is lighter. 

To compile binaries I use [ZASM Assembler](http://k1.spdns.de/Develop/Projects/zasm/Distributions/), a multi-platform Z80 assembler that has a lot of features, like macros, C files inclusion and more. It runs on Linux and MacOS (but you can use it on a Windows-based system just by running a Linux instance in a virtual machine) so that almost everyone can compile the code with it and obtain the same binary files. However, the latest compiled binary file, ready to be burned into an EEPROM, is always available. 

To open/modify schematics you need a CAD: [KiCad](http://www.kicad-pcb.org/) is the best choice, since it's free and doesn't have the limitations of the freeware version of [EagleCAD](https://www.autodesk.com/products/eagle/overview) (it can not permit to create a board bigger than 80 square cm., and the LM80C PCB is much bigger). Moreover, I've also added PDF files if you just want to look at the schematics but don't want to install any additional software.

### Hardware
To burn the binaries on the (E)EPROM chips I used a modified version of [TommyPROM](https://github.com/leomil72/TommyPROM) to use 74HCT595 shift registers, but you can use any (E)EPROM programmer you have. If you want to replicate my programmer, you need an [Arduino/Genuino Uno](https://www.arduino.cc) board. Please buy a *genuine board*, not a counterfit one, to help the open-source project. 

I reccomend you to assemble a prototype on breadboards to be sure that everything is working right. The pictures below represent the LM80C during its development:
<img src="https://raw.githubusercontent.com/leomil72/LM80C/master/12-Home%20computer/LM80C.jpg" title="LM80C prototype, built upon breadboards" alt="LM80C prototype, built upon breadboards">

<img src="https://raw.githubusercontent.com/leomil72/LM80C/master/keyboard.jpg" title="LM80C keyboard over the breaboards" alt="LM80C keyboard over the breaboards">

---

## Files
The files in this repo are organized in folders:
- Folders numbered from "01" to "09": the contain the first tests I made and the computer without the video section, that you can replicate to work solely through the serial line.
- Folders numbered "10" and "11" contain the releases that had preliminary support for video output.
- Folder named "12-Home computer" contains the actual schematic of the LM80C computer and the main file of the firmware. Every release adds some functionality to the original Nascom BASIC. The greater the release number is the newer the firmware is.
- Folder named "Include" contains the sub-folders with the files needed to compile the firmware. They are: "basic", "bootloader","psg", and "vdp". Each sub-folder contains the releases of the corresponding module of the firmware.
- Folder named "Roms" contains pre-compiled binary files ready to be burned into the EEPROM chip.
- Folder named "Manuals" contains useful manuals for the CPU, the peripheral chips, the video and audio chips, and a couple of text files that illustrate the main features of the LM80C and the reference manual of its BASIC language.

---

## Compilation
To compile the firmware, first download or clone the repo on your computer, than go into the "12-Home computer" folder and open a terminal in it, then give this command:
`zasm -uwy --z80 ./LM80C-firmware-rXX.asm`
where "XX" is the release you want to compile. After the compilation you'll find a file with extension `.rom` that you can burn into the EEPROM.
Obviously, you should have downloaded and installed the compiler previously. The compiler I used is ZASM, that you can get from [this site](http://k1.spdns.de/Develop/Projects/zasm/Distributions/). It's available both for Linux and MacOS 8and other BSD related OSs). At the moment there isn't a Windows version, so to compile the source you need a Linux box, a Mac or a Windows computer with a virtual machine running Linux on it.

---

## Clone
If you need, you can clone this repo to your local machine using `https://github.com/leomil72/LM80C`

---

## FAQ

- **How can I clone this repo?**
  - On your desktop computer, by using a GUI frontend like SmartGit, or by terminal by typing the following command ```git clone https://github.com/leomil72/LM80C```
  - Online, by clicking on the "Fork" button
- **How can I download the files?**
  - By clicking the green top-right "Clone or Download" and then by clicking on "Download ZIP"
- **How do I write and compile an ASM source file?**
  - You need an editor to write the source code and then an 'assembler' to compile the binary file
- **How do I burn the binary file into the EEPROM?**
  - You need an (E)EPROM programmer, a device that can be connected to your computer and that can write the data into the memory chip. You can buy a programmer from internet or build your own device like me.
- **Where can I find the chips you're using in the LM80C computer?**
  - Unfortunately, Zilog seems that it has stopped manufacturing the Z80 family chips, so currently you can find some remains of the warehouse from big online resellers or new/used parts over online auctions sites. The 74HCTxx integrated are very common, so you shouldn't have any problem to grab them. The VDp and PSG chips are only available as used parts from several chinese reseller.
- **The software seems not to be working/the hardware I built following your schematics doesn't work: where can I find help? Can I write to you?**
  - The project is released "as is", without any warranty/responsability: despite I've tested it and I'm currently using it, I can not guarantee that it is completely error-free, so please don't write to complain about something not working. Open an "issue" or a "pull request". Thank you.

---

## Support

You can find more about me on my website at <a href="https://www.leonardomiliani.com/en/" target="_blank">www.leonardomiliani.com</a>.

---

## Donations

- If you want to help me maintain my site, my projects and keep me rolling on, you can make a small donation with <a href="https://www.paypal.me/LeonardoMiliani" target="_blank">Paypal</a>.

---

## License

- Some portions of code are copyright <a href="http://searle.hostei.com/grant/" target="_blank">Grant Searle</a>
- Some portions of code are copyright <a href="http://www.train­z.de" target="_blank">Mario Blunk</a>
- BASIC code is copyright <a href="http://www.microsoft.com" target="_blank">Microsoft</a>
- The rest of the stuff is my own work and is distribuited under the [Gnu GPL Licence 3.0](https://opensource.org/licenses/GPL-3.0).
You can copy and use everything you need but you should maintain the copyright notices and, please, cite me for my work.
