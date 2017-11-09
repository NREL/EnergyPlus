#!/usr/bin/env python

import os
import glob
import licensetext

TOOL_NAME = 'license-check'

#
# Directories to check
#
dirs = ["./src/EnergyPlus/",
        "./tst/EnergyPlus/unit/"]

current = licensetext.current()

# Check LICENSE.txt
# Create the text as it should be
licensetxt = licensetext.mergeParagraphs(current)
# Load the text file
filename = "LICENSE.txt"
fp = open(filename)
filetxt = fp.read()
fp.close()
# Compare the two strings
licensetext.checkLicense('LICENSE.txt',filetxt,licensetxt,toolname=TOOL_NAME)

# Create Checker object
checker = licensetext.Checker(current, toolname=TOOL_NAME)

# Check files
for base in dirs:
    subdirs = glob.glob(base + '*/')
    checker.check(base)
    for sub in subdirs:
        checker.check(sub)
