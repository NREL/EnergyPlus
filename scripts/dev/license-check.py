#!/usr/bin/env python3

import licensetext
import sys

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
base_license_text_success = licensetext.checkLicense('LICENSE.txt',filetxt,licensetxt,toolname=TOOL_NAME)

# Create Checker object
checker = licensetext.Checker(current, toolname=TOOL_NAME)

# Check files
file_license_success = True
for base in dirs:
    file_success = checker.visit(base)
    if not file_success:
        file_license_success = False

if base_license_text_success and file_license_success:
    sys.exit(0)
else:
    sys.exit(1)
