#!/usr/bin/env python
#
# Use this script to update the license text
#
# Directions: First, check that the "dryrun" parameter below is set to True.
# This will run the script and check the files but will not actually make any
# changes. There are large number of files, so it's best to make sure that
# everything makes sense before actually making the change. Next, execute the
# script from the root of the EnergyPlus source tree:
#
#   $> python ./scripts/dev/license-update.py
#   Skipping writing out LICENSE.txt
#   Checked 809 files
#   Would have replaced text in 0 files
#
# The number of files will likely change, this is as of 1/2/2019. Note that it
# says that 0 files would have been modified. This is because it isn't looking
# for the right text.
#
# In the file "licensetext.py", find the variable named "_previous_year" and
# change it to the year that is to be replaced. In 2019, the variable was set
# to '2018'. Note that it should be a string. Run the script again:
#
#   $> python ./scripts/dev/license-update.py
#   Skipping writing out LICENSE.txt
#   Checked 809 files
#   Would have replaced text in 809 files
#
# We're ready to go. Change the "dryrun" parameter below to "False" and run
# the script again:
#
#   $> python ./scripts/dev/license-update.py
#   Writing out LICENSE.txt
#   Checked 809 files
#   Replaced text in 809 files
#
# Finally, change the "dryrun" parameter back to "True".
#

import os
import glob
import json
import licensetext

TOOL_NAME = 'license-update'
dryrun = True

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
    fp.close()
else:
    print('Skipping writing out LICENSE.txt')

# Create Replacer object
replacer = licensetext.Replacer(previous, current, dryrun=dryrun)

# Check files
for base in dirs:
    #subdirs = glob.glob(base + '*/')
    replacer.check(base)
    #for sub in subdirs:
    #    replacer.check(sub)

print(replacer.summary())
