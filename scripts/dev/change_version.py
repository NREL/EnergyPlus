#!/usr/bin/env python

# Attempt to automatically change the version number of all idf and imf files in a repo
# Two arguments: old version number and new version number

import sys
import fnmatch
import os

# provide a nice usage function
def usage():
	print("""Call this script with three command line arguments:
 $ change_version.py <path to repo> <old version number> <new version number>
 $ change_version.py /repos/eplus 8.4 8.5""")

# check the command line argument status
if not len(sys.argv) == 4:
	print("Invalid command line arguments")
	usage()
	sys.exit(1)

# store the command line arguments since they appear valid so far
repo  = sys.argv[1]
v_old = sys.argv[2]
v_new = sys.argv[3]

# walk across
for extension in ['*.idf', '*.imf']:
	for folder in ['testfiles', 'performance_tests', 'datasets']:
		thisdir = os.path.join(repo, folder)
		for root, dirnames, filenames in os.walk(thisdir):
			for filename in fnmatch.filter(filenames, extension):
				filedata = None
				with open(os.path.join(root, filename)) as fil:
					filedata = fil.read()
				filedata = filedata.replace('Version,'+v_old, 'Version,'+v_new)
				with open(os.path.join(root, filename), 'w') as fil:
					fil.write(filedata)
