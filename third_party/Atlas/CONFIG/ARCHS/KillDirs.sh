#!/bin/bash
# Kills all dirs for which we have a .tgz
archs=`ls *.tgz`
for arch in $archs
do
   echo rm -rf ${arch%\.tgz}
   rm -rf ${arch%\.tgz}
done
