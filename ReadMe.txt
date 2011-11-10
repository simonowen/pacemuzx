Pac-Man Emulator for Sinclair Spectrum (v1.1)
---------------------------------------------

This program requires a Spectrum +2A/+3, and will not work on earlier models.

The Pac-Man ROMs cannot be supplied with this program, so you must supply your
own copies of the following files (from the Midway ROM set):

  pacman.6e pacman.6f pacman.6h pacman.6j

Copy them to the same directory as this file, then run make.bat (Windows) or
make (Mac/Linux/Un*x) to build the final pacemuzx.tap image file.

Thanks to Tom-Cat and Pete for feedback suggestions :)

Enjoy!

---

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
