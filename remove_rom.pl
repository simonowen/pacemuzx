#!/usr/bin/perl -w

# First 4K of Pac-Man ROM
$file = 'pacman.6e';
open FILE, "<$file" and binmode FILE or die "$file: $!\n";
read FILE, $rom='', -s $file;
close FILE;

# Full TAP image for emulator, including ROM
$file = 'pacemuzx.tap';
open FILE, "<$file" and binmode FILE or die "$file: $!\n";
read FILE, $data='', -s $file;
close FILE;

$index = index($data, $rom);
die "ROM image not found in TAP file!\n" if $index < 0;

$file = 'start.part';
open FILE, ">$file" and binmode FILE or die "$file: $!\n";
print FILE substr $data, 0, $index;
close FILE;

$file = 'end.part';
open FILE, ">$file" and binmode FILE or die "$file: $!\n";
print FILE substr $data, $index+16384;
close FILE;
