#!/bin/sh
# USAGE: ./l2peak.sh <L2 FLUSH> <in-L2size>
# First arg is size of L2 to pass to total flush, second is for L2 timings
#
# Discover the BLDdir
#
SRCdir=`fgrep "SRCdir =" Make.inc | sed -e "s/ *SRCdir = *//"`
BINdir=$SRCdir/bin
#
# First time out-of-cache operations
#
./xr1ksearch -T 2 -i R1CASES/mvtch.idx -2 $1 -o mvtch.out
./xr1ksearch -T 2 -i R1CASES/r1tch1.idx -2 $1 -o r1tch1.out
./xr1ksearch -T 2 -i R1CASES/r1tch2.idx -2 $1 -o r1tch2.out
#
# Run in-L2 timings
#
./xr1ksearch -T 32 -i R1CASES/mvtch_L2.idx -2 $2 -o mvtch_L2.out
./xr1ksearch -T 32 -i R1CASES/r1tch1_L2.idx -2 $2 -o r1tch1_L2.out
./xr1ksearch -T 32 -i R1CASES/r1tch2_L2.idx -2 $2 -o r1tch2_L2.out
#
# Translate generated files to csv format
#
$BINdir/sum2csv.sh mvtch.out 2
$BINdir/sum2csv.sh r1tch1.out 2
$BINdir/sum2csv.sh r1tch2.out 2
#
$BINdir/sum2csv.sh mvtch_L2.out 3
$BINdir/sum2csv.sh r1tch1_L2.out 3
$BINdir/sum2csv.sh r1tch2_L2.out 3
