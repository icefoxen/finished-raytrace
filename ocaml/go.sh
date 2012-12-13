#!/bin/sh
rm -f raytrace
rm -f bar.png
echo "Compiling..."
make
echo "Running..."
./raytrace.opt
echo "Converting..."
convert -depth 8 -size 800x800 rgb:fop.rgb png:bar.png
#echo "Sending..."
#scp bar.png alopex.li:/home/icefox/webtemp/
