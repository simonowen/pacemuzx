# Pac-Man Emulator for Sinclair Spectrum v1.6

## Introduction

This program emulates the Pac-Man hardware environment, allowing the arcade
Pac-Man ROMs to run (almost) unmodified on the Sinclair Spectrum. The Z80 code
is executed natively, but the sprite and tile display hardware is emulated. The
sound and input are mapped to the closest Spectrum equivalents.

## Requirements

- Spectrum +2A/+2B/+3
- Arcade Pac-Man ROM images

The emulator requires the special paging mode of the Spectrum +2A/+2B/+3, and
will not work on earlier 48/128/+2 models. Only the combined use of both normal
and special paging modes provides the flexibility needed to access everything
needed at runtime. Using a peripheral to provide RAM at address 0 is not enough
to satisfy this requirement. The current DivIDE support is purely to disable its
hardware traps from triggering when executing code in the lower 16K.

The Pac-Man ROMs cannot be supplied with this program, so you must supply your
own copies of the following files (from the Midway ROM set):

  pacman.6e pacman.6f pacman.6h pacman.6j

## Usage

The `pacemuzx.tap` tape image is created by combining the supplied tape file
fragments with the Pac-Man ROM images:

 1) Copy the Pac-Man ROM images (detailed above) to the pacemuzx directory.
 2) Windows users: run `make.bat`. Linux/Mac/Unix users run `make`.
 3) Load the .tap image into a Spectrum emulator that supports +3 emulation.

## Controls

|                 Key | Action                                 |
|--------------------:|:---------------------------------------|
|                   1 | 1 Player Start                         |
|                   2 | 2 Player Start                         |
|                   3 | Insert Coin                            |
|                   C | Colour Sprites                         |
|                   M | Mono Sprites                           |
|                   H | Hard difficulty (hold during ROM boot) |
|         Cursor Keys | Joystick Control                       |
|             Q/A/O/P | Joystick Control                       |
|   Kempston Joystick | Joystick Control                       |
| Sinclair 2 Joystick | Joystick Control                       |
|       Symbol-1 to 7 | Maze Colour (Sym-1 for blue)           |

To check the difficulty setting, watch the cyan ghost at the start of the game.
On Hard it exits the ghost box immediately, on Normal it waits around 6 seconds.

## Build From Source

You may also build the emulator from source code, which requires a few extra
tools.

### Prerequisites

- The pacemuzx [source code](https://github.com/simonowen/pacemuzx).
- The arcade Pac-Man ROM images detailed above.
- [tile2sam.py](https://github.com/simonowen/tile2sam) to create sprite data.
- [Pasmo assembler](https://pasmo.speccy.org/) to build the Z80 code.

Ensure `tile2sam.py` and `pasmo` are in your path, then run `make.bat` (Windows)
or `make` (Linux/Mac/Unix) to generate `pacemuzx.tap`.

---

## Changelog

### Version 1.6 (2020/03/30)
- Fixed error message corruption when required hardware not found
- Updated to use `tile2sam.py` instead of `png2bin.pl`
- Converted remove_rom.pl to Python

### Version 1.5 (2016/11/19)
- Fixed code overflow crash on mono/colour toggle (thanks Matthew Logue)
- Changed maze colours to be bright by default, with Shift to dim
- Changed green ghost to non-bright yellow, to be closer to orange original

### Version 1.4 (2014/03/01)
- Added support for diagonal control inputs from keyboard and 8-way joysticks

### Version 1.3 (2012/01/08)
- Added run-time colour/mono sprite selection
- Added boot-time selection of Hard difficulty
- Restored traditional blue/white maze flash when using blue/white colours
- Restored sprite trimming at maze edges in colour version
- Added loading message to show version number

### Version 1.2 (2011/12/06)
- Added colour sprites
- Added Spectranet support, to prompt for trap disable
- Added key combos to change maze colour
- Maze flash now toggles current bright instead of blue
- Various minor fixes and speed-ups

### Version 1.1 (2011/11/09)
- Added DivIDE support, using pages 3+0 for ROM if present
- Improved control handling, favouring newest direction pressed
- Holding a key after loading delays the emulation starting until released
- Moved Blinky's downward-facing mouth down a pixel

### Version 1.0 (2011/11/08)
- Initial release

---

Simon Owen  
https://simonowen.com/spectrum/pacemuzx/
