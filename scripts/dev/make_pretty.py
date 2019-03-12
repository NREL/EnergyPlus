#!/usr/bin/env python

# Attempt to automatically change the version number of all idf and imf files in a repo
# Two arguments: old version number and new version number

import sys
import fnmatch
import os
import subprocess

# provide a nice usage function
def usage():
    print("""Call this script with three command line arguments:
 $ make_pretty.py <path to repo> <path to transition dir> <transition exe>
 $ make_pretty.py /repos/eplus /build/products Trans-V-X-to-V-Y.exe

 To run, 'MakingPretty' needs to be TRUE in DataVCompareGlobals.f90
 and VCompareGlobals.f90. The exe's need to be compiled with this in place.
 Both .idd versions refereced by the transition exe need to match the current transition verison.
 Example: To run on V8.7, both V8-6-0-Energy+.idd and V8-7-0-Energy+.idd need to match the V8.7 idd.""")

# check the command line argument status
if not len(sys.argv) == 4:
    print("Invalid command line arguments")
    usage()
    sys.exit(1)

# store the command line arguments since they appear valid so far
repo = sys.argv[1]
transition_dir = sys.argv[2]
transition_exe_name = sys.argv[3]

transition_exe_path = os.path.join(transition_dir, transition_exe_name)

# walk across
for extension in ['*.idf', '*.imf']:
    for folder in ['testfiles', 'performance_tests', 'datasets']:
        thisdir = os.path.join(repo, folder)
        for root, dirnames, filenames in os.walk(thisdir):
            for filename in fnmatch.filter(filenames, extension):
                test_file_path = os.path.abspath(os.path.join(thisdir, filename))
                print(filename)
                transition_run = subprocess.Popen(["" + transition_exe_name, test_file_path], shell=True, cwd=transition_dir, stderr=subprocess.STDOUT)
                transition_run.communicate()
                thisfilepath = test_file_path.split(".")
                for ext in ['.idfnew', '.idfold', '.rvinew', '.rviold', '.VCpErr', 'imfnew', 'imfold']:
                    try:
                        os.remove(thisfilepath[0] + ext)
                    except:
                        continue
