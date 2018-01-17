#!/usr/bin/env python
import sys
import argparse

parser = argparse.ArgumentParser("Remove ROM placeholder from pacemu master disk image")
parser.add_argument("tapfile", help="input TAP file containing ROM")
parser.add_argument("romfile", help="start of ROM image to remove")
parser.add_argument("romsize", type=int, help="size of ROM to remove")
parser.add_argument("startfile", help="output file for start segment")
parser.add_argument("endfile", help="output file for end segment")
args = parser.parse_args()

with open(args.tapfile, 'rb') as tapfile:
    tap = tapfile.read()

with open(args.romfile, 'rb') as romfile:
    rom = romfile.read()

rom_offset = tap.find(rom)
if rom_offset == -1:
    sys.exit("error: ROM image not found in TAP file!")

start_part = tap[:rom_offset]
end_part = tap[rom_offset + args.romsize:]

with open(args.startfile, 'wb') as startfile:
    startfile.write(start_part)

with open(args.endfile, 'wb') as endfile:
    endfile.write(end_part)
