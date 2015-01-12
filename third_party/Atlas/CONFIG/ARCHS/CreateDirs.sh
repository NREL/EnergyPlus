#!/bin/sh
# Creates dirs from .tar.bz2
for arch in $*
do
bunzip2 -c $arch | tar xvf -
done
