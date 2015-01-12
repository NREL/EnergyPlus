#!/bin/sh
atldir=$1
shift
$atldir/$*
#mach=MyMach
#rdir=/tmp
#atldir=$1
#shift
#exe=$1
#shift
#scp $atldir/$exe $mach:$(rdir)/$exe
#ssh $mach "cd $rdir ; ./$exe $*"
