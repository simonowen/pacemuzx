Pac-Man Emulator for Sinclair Spectrum (v1.2)
---------------------------------------------

This program requires a Spectrum +2A/+3, and will not work on earlier models.

The Pac-Man ROMs cannot be supplied with this program, so you must supply your
own copies of the following files (from the Midway ROM set):

  pacman.6e pacman.6f pacman.6h pacman.6j

Copy them to the same directory as this file, then run make.bat (Windows).
Under Mac/Linux/Un*x, use make to build the final pacemuzx.tap image file,
or combine manually using:

  cat start.part pacman.6[efhj] end.part > pacemuzx.tap

Many thanks to #zx and WOS for feedback and testing :)

Enjoy!

---

Version 1.2 (2011/12/06)
- Added colour sprites
- Added Spectranet support, to prompt for trap disable
- Added key combos to change maze colour
- Maze flash now toggles current bright instead of blue
- Various minor fixes and speed-ups

Version 1.1 (2011/11/09)
- Added DivIDE support, using pages 3+0 for ROM if present
- Improved control handling, favouring newest direction pressed
- Holding a key after loading delays the emulation starting until released
- Moved Blinky's downward-facing mouth down a pixel

Version 1.0 (2011/11/08)
- Initial release

---

Simon Owen
http://simonowen.com/spectrum/pacemuzx/
