NAME=pacemuzx
ROMS=pacman.6e pacman.6f pacman.6h pacman.6j
UNAME := $(shell uname -s)

.PHONY: dist clean

$(NAME).tap: loader.tap $(NAME).o
	cat loader.tap $(NAME).o > $@

tiles.bin: tiles.png
	./png2bin.pl $< 6

sprites.bin: sprites.png
	./png2bin.pl $< 12

$(NAME).o: $(NAME).asm tiles.bin sprites.bin $(ROMS)
	pasmo --tap $(NAME).asm $(NAME).o $(NAME).sym

run: $(NAME).tap
ifeq ($(UNAME),Darwin)
	open $(NAME).tap
else
	xdg-open $(NAME).tap
endif

dist: $(NAME).tap
	rm -rf dist
	mkdir dist
	cp ReadMe.txt dist/
	cp Makefile-dist dist/Makefile
	cp make.bat-dist dist/make.bat
	./remove_rom.pl $(NAME).tap
	mv start.part end.part dist/

clean:
	rm -f $(NAME).tap $(NAME).sym $(NAME).o
	rm -f tiles.bin sprites.bin start.part end.part
	rm -rf dist
