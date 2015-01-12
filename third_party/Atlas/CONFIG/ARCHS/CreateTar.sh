#!/bin/sh
# Creates the appropriate .tgz from all subdirs
for arch in `ls -F | fgrep "/" | sed -e 's/\///'`
do
tar cvf $arch.tar $arch/*
bzip2 --best $arch.tar
done
