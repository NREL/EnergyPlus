#!/usr/bin/env python

"""
This script is used to clear away all the gcda coverage files that get dropped into the build directory structure.
It takes one required argument: the path to a build directory.  The script will delete all .gcda files in that tree.
It takes a second argument, which is a text string that will be used as an exclusion.  If this is included, the script
  will try to find the pattern in the filename (case-insensitive) and will allow that gcda file to stay.
  This is useful when trying to quickly investigate just the coverage of a single source file.
"""

import fnmatch
import os
import sys

if len(sys.argv) < 1 or len(sys.argv) > 3:
    print("Need either 1 or 2 arguments, no more, no less.")
    sys.exit(2)
else:
    build_dir = sys.argv[1]
    excluded_pattern = None
    if len(sys.argv) == 3:
        excluded_pattern = sys.argv[2]

if not os.path.exists(build_dir):
    print("Build dir at %s did not exist, aborting!" % build_dir)
    sys.exit(1)

gcda_files_to_delete = []
all_gcda_files = []
for root, _, filenames in os.walk(build_dir):
    for filename in fnmatch.filter(filenames, '*.gcda'):
        all_gcda_files.append(os.path.join(root, filename))
        if excluded_pattern and excluded_pattern.lower() in filename.lower():
            continue
        gcda_files_to_delete.append(os.path.join(root, filename))

for gcda_file in gcda_files_to_delete:
    os.remove(gcda_file)
