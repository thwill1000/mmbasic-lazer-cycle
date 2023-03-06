# Lazer Cycle for MMBasic

A 1-4 player implementation of the classic ["Light Cycle"](https://en.wikipedia.org/wiki/Light_Cycle) game for Colour Maximite 2, PicoMite (VGA and LCD) and MMBasic for Windows.

*Depending on the platform the cycles can be controlled with Wii Classic Controller, Wii Nunchuck, NES gamepad, Atari joystick or keyboard.*

**Screenshots from the [PicoGAME VGA](https://github.com/thwill1000/pico-game-vga):**

<img src="/images/pmvga-title.png" width="320"> <img src="/images/pmvga-instructions.png" width="320"> <img src="/images/pmvga-high-score.png" width="320"> <img src="/images/pmvga-settings.png" width="320"> <img src="/images/pmvga-gameplay.png" width="320">

**Screenshot from the Colour Maximite 2:**

*Practically identical to the PicoMite VGA except that the arena wall is grey rather than lilac.*

<img src="/images/cmm2-gameplay.png" width="320">

**Screenshot from the PicoMite with 320x240 2.8" ILI9341 SPI LCD display:**

<img src="/images/pglcd-gameplay.jpg" width="320">

**YouTube video of Lazer Cycle running on my [PicoGAME LCD](https://github.com/thwill1000/pico-game-lcd) protoype:**

&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://www.youtube.com/watch?v=jB5hF2ZWHrA"><img src="https://www.gstatic.com/youtube/img/branding/youtubelogo/svg/youtubelogo.svg" width="10%" title="https://www.youtube.com/watch?v=jB5hF2ZWHrA"></a>

## How do I install it ?

 - Download the .bas file for the desired platform:
     - Colour Maximite 2 or MMBasic for Windows: [lazer-cycle-cmm2-100.bas](lazer-cycle-cmm2-100.bas)
     - PicoMiteVGA / PicoGAME VGA: [lazer-cycle-pmvga-100.bas](lazer-cycle-pmvga-100.bas)
     - PicoMite with LCD display / PicoGAME LCD: [lazer-cycle-pglcd-100.bas](lazer-cycle-pglcd-100.bas)
 - Copy it to a directory of your choice, perhaps renaming it to ```lazer-cycle.bas```.
 - That's it!

## How do I run it ?
 - Navigate to the directory the file is in and type: ```*lazer-cycle```
 - **IMPORTANT**: Be aware of what hardware you have attached to the GPIO when running Lazer Cycle. To detect attached NES controllers the game sends pulses to pins 38 & 40 on the Colour Maximite 2 and GP2, GP3, GP5 and GP22 on the PicoMite. The PicoMite LCD version also transmits audio via PWM on pins GP4 and GP6.

## How do I play it ?

 1. Watch the attract mode to see the in-game instructions.
 2. When you are ready to play press START (on your gamepad), FIRE (on your joystick) or SPACE (on your keyboard).

**Keyboard controls - all platforms:**

- Cursor keys
 - Up: `A`,              Down: `Z`,              Left: comma `,`,        Right: period `.`
 - Up: `A`,              Down: `Z`,              Left: `X`,              Right: `C`
 - Up: apostrophe `'`,   Down: slash `/`,        Left: comma `,`,        Right: period `.`

**Colour Maximite 2 controls:**

 - Wii Nunchuck controller
     - I2C1, I2C2 or I2C3
 - Wii Classic controller
     - I2C1, I2C2 or I2C3
 - Atari joystick
     - Compatible with the DB9 port on the [Colour Maximite 2 Deluxe Edition GEN2](https://github.com/pslabs-ps/CMM2-DeluxeEdition-GEN2).
     - Pin 35: Up, Pin 36: Down, Pin 38: Left, Pin 40: Right, Pin 32: Fire A, Pin 33: Fire B (not required)
 - NES gamepad
     - Requires an adapter when used with the DB9 port on the [Colour Maximite 2 Deluxe Edition GEN2](https://github.com/pslabs-ps/CMM2-DeluxeEdition-GEN2).
         - Swap the Male DB9 (CMM2) +5V supply on Pin 7 to Pin 6 on the Female DB9 (Gamepad).
     - Pin 38: Latch, Pin 40: Clock, Pin 36: Data

**Additional PicoMiteVGA / PicoGAME VGA controls:**

Compatible with the game ports on a [PicoGAME VGA](https://github.com/thwill1000/pico-game-vga).

 - Atari joystick A
     - GP0: Up, GP1 36: Down, GP2: Left, GP3: Right, GP14: Fire
 - Atari joystick B
     - GP 28: Up, GP 4: Down, GP5: Left, GP22: Right, GP15: Fire
 - NES gamepad A
     - GP2: Latch, GP3: Clock, GP1: Data
 - NES gamepad B
     - GP5: Latch, GP22: Clock, GP4: Data

**PicoMite with LCD display / PicoGAME LCD controls:**

- NES controller
     - GP2: Latch, GP3: Clock, GP1: Data

## FAQ

**1. What is the Colour Maximite 2 ?**

The Colour Maximite 2 is a small self contained computer inspired by the home computers of the early 80's such as the Tandy TRS-80, Commodore 64 and Apple II. 

It uses a USB keyboard and outputs to a standard VGA monitor.  Programs are saved on a full size SD card and it boots straight into its own sophisticated BASIC interpreter with a full screen program editor.

The Colour Maximite 2 is designed to be simple to use, easy to program and provide endless hours of fun.

While the concept of the Colour Maximite 2 is borrowed from the computers of the 80's the technology used is very much up to date.  Its CPU is an ARM Cortex-M7 32-bit RISC processor running at 480MHz and it generates a VGA output at resolutions up to 1920x1080 pixels with up to 16 million colours.

The assembly instructions, PCB design and the firmware are completely free.  It can be easily built by a beginner with no special skills in an hour or two or you can purchased it partially or fully assembled.

With a monitor and keyboard it makes a complete stand alone BASIC computer. It is perfect for rediscovering the computers of the 80's, learning to program, entertaining and inspiring children, creating and playing simple computer games and generally having fun discovering what you can do with this powerful low cost computer.

More information can be found on the official Colour Maximite 2 website at
http://geoffg.net/maximite.html

**2. What is a PicoMite ?**

The PicoMite is a Raspberry Pi Pico running the free MMBasic interpreter.

MMBasic is a Microsoft BASIC compatible implementation of the BASIC language with floating point, integer and string variables, arrays, long variable names, a built in program editor and many other features.

Using MMBasic you can use communications protocols such as I2C or SPI to get data from a variety of sensors. You can save data to an SD card, display information on colour LCD displays, measure voltages, detect digital inputs and drive output pins to turn on lights, relays, etc. All from inside this low cost microcontroller.

The PicoMite firmware is totally free to download and use.

More information can be found on the official PicoMite website at https://geoffg.net/picomite.html

**3. How do I contact the author ?**

The author can be contacted via:
 - https://github.com as user "thwill1000"
 - https://www.thebackshed.com/forum/ViewForum.php?FID=16 as user "thwill"
 
##

Lazer Cycle for MMBasic is distributed for free but if you enjoy it then
perhaps you would like to buy me a coffee?

<a href="https://www.buymeacoffee.com/thwill"><img src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" alt="Buy Me A Coffee" style="width:217px;"></a>

