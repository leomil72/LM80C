# LM80C - A Z80-based homebrew computer

## BASIC
> This folder contains the stuff that you need to replicate my tests over the BASIC interpreter.

> Project main page: [LM80C](http://www.leonardomiliani.com/en/lm80c/)

---

## What you need

Before to start building your own LM80C computer you need several things:
- of course, you need some hardware;
- Editor & assembler
- A CAD to open schematics files

### Software
I suggest to use [ATOM editor](https://atom.io) to write your ASM files, with [Language-assembler-SJASMPLUS](https://atom.io/packages/language-assembler-sjasmplus) package to highlight the Z80 assembly code. ATOM is a very complete editor and you can use for every language you use (personally, I use it for Lua coding, too).

To compile binaries I used [ZASM Assembler](http://k1.spdns.de/Develop/Projects/zasm/Distributions/), a multi-platform Z80 assembler that has a lot of features, like macros, C files inclusion and more. It also runs on Windows, Linux and Mac so that almost every user can compile the code with it and obtain the same binary files.

To open/modify schematics you need [EagleCAD](https://www.autodesk.com/products/eagle/overview), a professional electronics CAD that is multi-platform and free to use for non commercial purposes. Moreover, I've also added PDF files if you just want to look at the schematics but don't want to install any additional software.

### Hardware
To burn the binaries on the (E)EPROM chips I used a modified version of [TommyPROM](https://github.com/leomil72/TommyPROM) to use 74HCT595 shift registers, but you can use any (E)EPROM programmer you have. If you want to replicate my programmer, you need an [Arduino/Genuino Uno](https://www.arduino.cc) board. Please buy a genuine board, not a counterfit one, to help the opensource project.

---

## Clone
- If you need, you can clone this repo to your local machine using `https://github.com/leomil72/LM80C`

---

## FAQ

- **How can I clone this repo?**
    - On your desktop computer, by using a GUI frontend like SmartGit, or by terminal by typing the following command ```git clone https://github.com/leomil72/LM80C```
    - Online, by clicking on the "Fork" button
- **How do I write and compile an ASM source file?**
    - You need an editor to write it, like Atom, and then an 'assembler' to compile the binary file.
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
