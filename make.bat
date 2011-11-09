@echo off

if "%1"=="clean" goto clean

png2bin.pl tiles.png 6
png2bin.pl sprites.png 12

pasmo --tap pacemuzx.asm pacemuzx.o pacemuzx.sym

copy /b loader.tap+pacemuzx.o pacemuzx.tap

if not "%1"=="dist" goto end

if not exist dist mkdir dist
copy ReadMe.txt dist\
copy Makefile-dist dist\Makefile
copy make.bat-dist dist\make.bat
remove_rom.pl
move start.part dist\
move end.part dist\
goto end

:clean
if exist pacemuzx.tap del pacemuzx.tap pacemuzx.sym pacemuzx.o
if exist tiles.bin del tiles.bin sprites.bin
if exist dist\ del dist\Makefile dist\make.bat dist\start.part dist\end.part dist\pacemuzx.tap
if exist dist\ rmdir dist

:end
