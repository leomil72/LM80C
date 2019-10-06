<a href="https://www.leonardomiliani.com/en/lm80c/"><img src="https://raw.githubusercontent.com/leomil72/LM80C/master/12-Home%20computer/LM80C.jpg" title="LM80C, a Z80-based computer" alt="LM80C, a Z80 -based computer"></a>


# LM80C - A Z80-based homebrew computer

> This repo contains schematics, code and other stuff that you can use to realize my project of building a Z80-based homebrew computer as a sort of "back to the old, good, days of 8-bits systems".

> Project main page: [LM80C](http://www.leonardomiliani.com/en/lm80c/)

> z80, zilog, homebrew, 8-bits, computer, lm80c, ctc, pio, sio

---

## Getting started

- [What you need](#what%20you%20need)
- [Clone](#clone)
- [FAQ](#faq)
- [Support](#support)
- [License](#license)

---

## What you need

Before to start building your own LM80C computer you need several things:
- of course, you need some hardware;
- Editor & assembler
- A CAD to open schematics files

### Software
I suggest to use [ATOM editor](https://atom.io) to write your ASM files, with [Language-assembler-SJASMPLUS](https://atom.io/packages/language-assembler-sjasmplus) package to highlight the Z80 assembly code. ATOM is a very complete editor and you can use for every language you use (personally, I use it for Lua coding, too). Another good editor that I've started using recently is [Visual Code Editor](https://code.visualstudio.com/): it's free and based on open source and multi-platform. The extension to highlight the assembly code is [Z80 ASM](https://github.com/Imanolea/z80asm-vscode).

To compile binaries I used [ZASM Assembler](http://k1.spdns.de/Develop/Projects/zasm/Distributions/), a multi-platform Z80 assembler that has a lot of features, like macros, C files inclusion and more. It also runs on Windows, Linux and Mac so that almost every user can compile the code with it and obtain the same binary files.

To open/modify schematics you need a CAD: [EagleCAD](https://www.autodesk.com/products/eagle/overview), a professional electronics CAD that is multi-platform and free to use for non commercial purposes. I used it for the first schematics I realized but recently I started using [KiCad](http://www.kicad-pcb.org/). Moreover, I've also added PDF files if you just want to look at the schematics but don't want to install any additional software.

### Hardware
To burn the binaries on the (E)EPROM chips I used a modified version of [TommyPROM](https://github.com/leomil72/TommyPROM) to use 74HCT595 shift registers, but you can use any (E)EPROM programmer you have. If you want to replicate my programmer, you need an [Arduino/Genuino Uno](https://www.arduino.cc) board. Please buy a genuine board, not a counterfit one, to help the opensource project. 

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

---

## Clone
If you need, you can clone this repo to your local machine using `https://github.com/leomil72/LM80C`

---

## FAQ

- **How can I clone this repo?**
    - On your desktop computer, by using a GUI frontend like SmartGit, or by terminal by typing the following command ```git clone https://github.com/leomil72/LM80C```
    - Online, by clicking on the "Fork" button
- **How do I write and compile an ASM source file?**
    - You need an editor to write the source code and then an 'assembler' to compile the binary file.
- **How do I burn the binary file into the EEPROM?**
    - You need an (E)EPROM programmer, a device that can be connected to your computer and that can write the data into the memory chip. You can buy a programmer from internet or build your own device like me.
- **Where can I find the chips you're using in your computer?**
    - Fortunately, Zilog still sells the Z80 family chips, so you can find them anywhere. Personally I've bought mine from Farnell, but you can find them in other resellers' catalogue. The 74HCTxx integrated are very common, so you shouldn't have any problem to grab them.

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
