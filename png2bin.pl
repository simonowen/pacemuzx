#!/usr/bin/perl -w
#
# Convert PNG image to Spectrum format sprite image data
#
# Input image should be palettised 1-bit with no transparency
#
# Simon Owen <simon@simonowen.com>

use Compress::Zlib;
use Getopt::Std;

# Allow -v option for verbose output
getopts('v');

# Strip path from input filename, and check 
$0 =~ s/.*\///;
die "Usage: $0 <image> <sprite-width> [<sprite-height>]\n" unless @ARGV == 2 or @ARGV == 3;

# Input image and output data file
($file,$w,$h) = (@ARGV,$ARGV[1]); # height defaults to width if not provided
die "Input image must be in PNG format\n" unless $file =~ /.png$/i;
die "Sprite width should be 2-16 pixels\n" unless $w >= 2 && $w <= 16;
die "Sprite height should be at least 1 pixel\n" unless $h > 0;

# Slurp the entire PNG image
open INPUT, "<$file" and binmode INPUT or die "$file: $!\n";
read INPUT, $data='', -s $file;
close INPUT;

# Extract and check the image dimensions
$data =~ /IHDR(.{8})/s;
($iw,$ih) = unpack "N2", $1;
die "Input image (${iw}x${ih}) must be an exact multiple of sprite size (${w}x${h})\n" if ($iw%$w) or ($ih%$h);

# Extract and expand the compressed image data
($data) = $data =~ /IDAT(.*).{8}IEND/s;
$data = Compress::Zlib::uncompress($data);

# Remove the type byte from the start of each line, leaving just 0+1 pixel values
if (length($data) == (($iw+1)*$ih)) { # 8bpp?
  $data =~ s/.(.{$iw})/$1/sg;
  @data = map { $_&1 } unpack("C*", $data);
} elsif (length($data) == (($iw/2+1)*$ih)) { # 4bpp?
  my $iw_2 = $iw/2;
  $data =~ s/.(.{$iw_2})/$1/sg;
  foreach (unpack("C*", $data)) {
    push @data, ($_>>4)&1, $_&1;
  }
} else {
  die "Image data format is unsupported (size = ".@data." bytes)\n";
}

# Calculate rows+columns and the expected size of the output data
($c,$r) = ($iw/$w,$ih/$h);
$size = int(($w+7)/8)*$h*$r*$c;
print "Input image ($file) = ${iw}x${ih}, sprite = ${w}x${h}\n" if $opt_v;

foreach $y (0..$r-1)
{
  foreach $x (0..$c-1)
  {
    foreach $l (0..$h-1) # line
    {
      # Calculate the offset of the required bit sequence
      my $o = ((($y*$h+$l)*$c) + $x) * $w;

      # Convert the individual bits to a binary string then a decimal value
      my $n = unpack("N", pack("B32", substr("0"x32 . join('', @data[$o..$o+$w-1]), -32)));

      # Pack into 1 or 2 bytes, with the bits left-aligned (MSB)
      $output .= pack(($w<=8)?"C":"n", $n << (8-($w & 7)));
    }
  }
}

# Sanity check the output data size
die "Output data ", length($output), " is the wrong size! (should be $size)\n" unless length($output) == $size;

# The output file is the input filename with a .bin extension instead of .png
$file =~ s/png$/bin/i;
open OUTPUT, ">$file" and binmode OUTPUT or die "$file: $!\n";

# Determine the size of a single sprite, and strip blank sprites from the end of the data
$ss = length($output) / ($r*$c);
$output =~ s/(\0{$ss})*$//;
print "Output data ($file) = ", length($output), " bytes = ", length($output)/$ss, " sprites\n"  if $opt_v;

print OUTPUT $output;
close OUTPUT;
