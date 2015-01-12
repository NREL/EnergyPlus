#!/bin/sh
#
# USAGE: sum2csv.sh <sum file> <mf index>
#
fil=$1
imf=$2
tfil=tmptmp.out
ofil=`echo ${fil} | sed -e "s/out/csv/"`
make xr1sum2csv
rm -f $ofil $tfil
n=`fgrep -n 'CUT HERE' $fil | sed 's/:.*//'`
head -n $n $fil > $tfil
./xr1sum2csv -i $tfil -m $imf -D 2 YU XU -p 1 -o $ofil
rm -f $tfil
