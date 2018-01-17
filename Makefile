NAME=pacemuzx
ROMS=pacman.6e pacman.6f pacman.6h pacman.6j
UNAME := $(shell uname -s)

.PHONY: dist clean

$(NAME).tap: loader.tap $(NAME).o
	cat loader.tap $(NAME).o > $@

sprites.bin: sprites.png
	tile2sam.py -q --mode 1 --tiles 76 sprites.png 12x12

tiles.bin: tiles.png
	tile2sam.py -q --mode 1 tiles.png 6x6

$(NAME).o: $(NAME).asm tiles.bin sprites.bin $(ROMS)
	pasmo --tap $(NAME).asm $(NAME).o $(NAME).sym

run: $(NAME).tap
ifeq ($(UNAME),Darwin)
	open $(NAME).tap
else
	xdg-open $(NAME).tap
endif

dist: $(NAME).tap
	mkdir -p dist
	cp ReadMe.txt dist/
	cp Makefile-dist dist/Makefile
	cp make.bat-dist dist/make.bat
	./remove_rom.py $(NAME).tap pacman.6e 16384 dist/start.part dist/end.part

clean:
	rm -f $(NAME).tap $(NAME).sym $(NAME).o
	rm -f tiles.bin sprites.bin start.part end.part
	rm -rf dist
