#!/usr/bin/env python

# Attempt to automatically change the version number of all idf and imf files in a repo
# Two arguments: old version number and new version number

import sys
import fnmatch
import os

# provide a nice usage function
def usage():
    print("""Call this script with four command line arguments:
    $ change_version.py <path to input files> <path to transition dir> <path to run dir> <path to output dir>
    $ transition_run_check.py /transition_check/idfs /transition_check/trans /transition_check/eplus /transition_check/output""")

# check the command line argument status
if not len(sys.argv) == 5:
    print("Invalid command line arguments")
    usage()
    sys.exit(1)

# store the command line arguments since they appear valid so far
input_files = sys.argv[1]
tran_dir = sys.argv[2]
run_dir = sys.argv[3]
output_dir = sys.argv[4]

def getInputFiles():
    # walk across
    for extension in ['*.idf', '*.imf']:
        for folder in ['testfiles', 'performance_tests', 'datasets']:
            thisdir = os.path.join(repo, folder)
            for root, dirnames, filenames in os.walk(thisdir):
                for filename in fnmatch.filter(filenames, extension):
                    filedata = None
                    print(filename)
                    # with open(os.path.join(root, filename)) as fil:
                        # filedata = fil.read()
                    # filedata = filedata.replace('Version,'+v_old, 'Version,'+v_new)
                    # with open(os.path.join(root, filename), 'w') as fil:
                        # fil.write(filedata)
