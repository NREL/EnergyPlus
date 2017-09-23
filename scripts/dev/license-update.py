#!/usr/bin/env python

import os
import glob
import json
import licensetext

TOOL_NAME = 'license-update'
dryrun = False

#
# Directories to check
#
dirs = ["./src/EnergyPlus/",
        "./tst/EnergyPlus/unit/"]

# Get the current text
current = licensetext.current()
previous = licensetext.previous()

# Create LICENSE.txt
licensetxt = licensetext.mergeParagraphs(current)

if not dryrun:
    print('Writing out LICENSE.txt')
    filename = "LICENSE.txt"
    fp = open(filename,'w')
    fp.write(licensetxt)
    fp.close
else:
    print('Skipping writing out LICENSE.txt')

# Create Replacer object
replacer = licensetext.Replacer(previous, current, dryrun=dryrun)

# Check files
for base in dirs:
    subdirs = glob.glob(base + '*/')
    replacer.check(base)
    for sub in subdirs:
        replacer.check(sub)

print(replacer.summary())
