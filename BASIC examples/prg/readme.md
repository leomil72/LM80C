# LM80C 64K BASIC - program images

This folder contains some program sources in .prg format rady to be loaded by the [LM80C online emulator](https://github.com/nippur72/lm80c-emu), written in Javascript by Antonino Porcino (direct link: [LM80C 64K (with integrated DOS)](https://nippur72.github.io/lm80c-emu/?rom=64K119))

## How to use them
When the emulator is up and running, then simply drag-and-drop the .prg file in the emulator's window and the program will be automatically loaded and run. If you want to save a BASIC list as a .prg file, press the "F12" key on your computer keyboard: the Javascript console will open up. Click on the "Console" tab in the side window, then in the console itself write `SAVE("program_name.prg")` (please change the program name to something useful for you) and the BASIC program will be saved as a auto-loadable file on your computer, from which you will be able to load it again by using the command `LOAD("program_name.prg")`.
