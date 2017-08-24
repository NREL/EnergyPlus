#!/usr/bin/env python

# Attempt to automatically change the version number of all idf and imf files in a repo
# Two arguments: old version number and new version number

import fnmatch
import os
import sys


# provide a nice usage function
def usage():
    print("""Call this script with three command line arguments:
 $ change_version.py <path to repo> <old version number> <new version number>
 $ change_version.py /repos/eplus 8.7 8.8""")


# check the command line argument status
if not len(sys.argv) == 4:
    print("Invalid command line arguments")
    usage()
    sys.exit(1)

# store the command line arguments since they appear valid so far
repo = sys.argv[1]
v_old = sys.argv[2]
v_new = sys.argv[3]

# walk across the idf and imf files
for extension in ['*.idf', '*.imf']:
    for folder in ['testfiles', 'performance_tests', 'datasets', os.path.join('testfiles', 'BasicsFiles')]:
        this_dir = os.path.join(repo, folder)
        for root, dir_names, file_names in os.walk(this_dir):
            for filename in fnmatch.filter(file_names, extension):
                with open(os.path.join(root, filename)) as input_file:
                    file_data = input_file.read()
                    file_data = file_data.replace('Version,' + v_old, 'Version,' + v_new)
                    file_data = file_data.replace('VERSION,' + v_old, 'Version,' + v_new)
                with open(os.path.join(root, filename), 'w') as output_file:
                    output_file.write(file_data)

# then walk across all the unit test files too
for folder in [os.path.join('tst', 'EnergyPlus', 'unit')]:
    this_dir = os.path.join(repo, folder)
    for root, dir_names, file_names in os.walk(this_dir):
        for filename in fnmatch.filter(file_names, 'cc'):
            with open(os.path.join(root, filename)) as input_file:
                file_data = input_file.read()
                file_data = file_data.replace('Version,' + v_old, 'Version,' + v_new)
                file_data = file_data.replace('VERSION,' + v_old, 'Version,' + v_new)
            with open(os.path.join(root, filename), 'w') as output_file:
                output_file.write(file_data)