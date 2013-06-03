@echo off
setlocal
set NAME=pacemuzx

if "%1"=="clean" goto clean

png2bin.pl tiles.png 6
png2bin.pl sprites.png 12

pasmo --tap %NAME%.asm %NAME%.o %NAME%.sym
if errorlevel 1 goto end

copy /b loader.tap+%NAME%.o %NAME%.tap
if "%1"=="run" start %NAME%.tap

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
if exist %NAME%.tap del %NAME%.tap %NAME%.sym %NAME%.o
if exist tiles.bin del tiles.bin sprites.bin
if exist dist\ del dist\ReadMe.txt dist\Makefile dist\make.bat dist\start.part dist\end.part
if exist dist\%NAME%.tap del dist\%NAME%.tap
if exist dist\ rmdir dist

:end
endlocal
