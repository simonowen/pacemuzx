TAPE=pacemuzx.tap
ROMS=pacman.6e pacman.6f pacman.6h pacman.6j

.PHONY: dist clean

$(TAPE): loader.tap pacemuzx.o
	cat loader.tap pacemuzx.o > $@

tiles.bin: tiles.png
	./png2bin.pl $< 6

sprites.bin: sprites.png
	./png2bin.pl $< 12

pacemuzx.o: pacemuzx.asm tiles.bin sprites.bin $(ROMS)
	pasmo --tap pacemuzx.asm pacemuzx.o pacemuzx.sym

dist: $(TAPE)
	rm -rf dist
	mkdir dist
	cp ReadMe.txt dist/
	cp Makefile-dist dist/Makefile
	cp make.bat-dist dist/make.bat
	./remove_rom.pl $(TAPE)
	mv start.part end.part dist/

clean:
	rm -f $(TAPE) pacemuzx.sym pacemuzx.o
	rm -f tiles.bin sprites.bin start.part end.part
	rm -rf dist
