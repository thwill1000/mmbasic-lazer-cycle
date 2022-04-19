# Light Cycle for MMBasic
**A version of the classic "Snake" game using polar instead of cartesian-coordinates.**

Unlike standard ["Snake"](https://en.wikipedia.org/wiki/Snake_(video_game_genre)) the game is played in a circular rather than rectangular playing field and instead of moving the Snake north, south east and west the player moves it outwards, inwards, clockwise and anti-clockwise.

**Screenshots from the Colour Maximite 2:**

<img src="/resources/cmm2-screenshot-1.png" width="320"> <img src="/resources/cmm2-screenshot-2.png" width="320"> <img src="/resources/cmm2-screenshot-3.png" width="320">

See [video here.](https://youtu.be/quosIboidbM)

**Screenshots from the PicoMite with 320x240 2.8" ILI9341 SPI LED display:**

<img src="/resources/pico-screenshot-1.png" width="320"> <img src="/resources/pico-screenshot-2.png" width="320"> <img src="/resources/pico-screenshot-3.png" width="320">

See (poor quality) [video here.](https://youtu.be/LbC8pWRzu-4)

Please read the [LICENSE](LICENSE) file for further details about modifying and distributing
this program.

Polar-coordinate "Snake" for MMBasic is distributed for free but if you enjoy it then
perhaps you would like to buy me a coffee?

[![paypal](https://www.paypalobjects.com/en_GB/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=T5F7BZ5NZFF66&source=url)

## How do I install it ?

 - Download the [latest release](https://github.com/thwill1000/mmbasic-polar-snake/releases/latest)
 - Extract to a directory of your choice, e.g. ```/polar-snake/```

## How do I run it ?
 - Type ```chdir "/polar-snake"```
 - Type ```run "polar-snake"```
     - *Note that on the CMM2 you can just type:* ```*polar-snake```

## How do I play it ?

Use the keyboard to navigate the snake to eat the apples without hitting the perimeter or biting its own tail. Every time you eat an apple the snake gets longer.

### Controls for menu screen
```
[A]     - Up
[Z]     - Down
[<]     - Decrease value
[>]     - Increase value
[Space] - Select / Toggle
```

### Controls for Game Type 1
```
[<] - Turn left
[>] - Turn right
```

### Controls for Game Type 2
```
[A] - Move inwards
[Z] - Move outwards
[<] - Move anticlockwise
[>] - Move clockwise
```

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
