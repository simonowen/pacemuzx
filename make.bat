@echo off
setlocal
set NAME=pacemuzx

if "%1"=="clean" goto clean

tile2sam.py -q --mode 1 --tiles 76 sprites.png 12x12
tile2sam.py -q --mode 1 tiles.png 6x6

pasmo --tap %NAME%.asm %NAME%.o %NAME%.sym
if errorlevel 1 goto end

copy /b loader.tap+%NAME%.o %NAME%.tap

if "%1"=="dist" goto dist
if "%1"=="run" start %NAME%.tap
goto end

:dist
if not exist dist mkdir dist
copy ReadMe.txt dist\
copy Makefile-dist dist\Makefile
copy make.bat-dist dist\make.bat
remove_rom.py %NAME%.tap pacman.6e 16384 dist\start.part dist\end.part
goto end

:clean
del /q %NAME%.tap %NAME%.sym %NAME%.o 2>nul
del /q tiles.bin sprites.bin 2>nul
del /q dist\ReadMe.txt dist\Makefile dist\make.bat dist\start.part dist\end.part 2>nul
del /q dist\%NAME%.tap 2>nul
rmdir dist 2>nul

:end
endlocal
