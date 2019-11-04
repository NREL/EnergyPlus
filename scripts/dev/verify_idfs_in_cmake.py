#!/usr/bin/env python

import json
import os


def usage():
    print("""This script verifies that the idf list in CMakeLists.txt matches the idf files in the testfiles directory.
Call with one argument: the directory which includes idfs and the CMakeLists.txt file""")


current_script_dir = os.path.dirname(os.path.realpath(__file__))
test_files_dir = os.path.join(current_script_dir, '..', '..', 'testfiles')

cmake_lists_file = os.path.join(test_files_dir, "CMakeLists.txt")
cmake_list_idf_files = set()
with open(cmake_lists_file) as f:
    for line in f.readlines():
        line_stripped = line.strip()
        if "ADD_SIMULATION_TEST" in line_stripped:
            idf_file_index = line_stripped.index("IDF_FILE")
            epw_file_index = line_stripped.index("EPW_FILE")
            filename = line_stripped[idf_file_index + 9:epw_file_index - 1]
            cmake_list_idf_files.add(filename)

found_idf_files = set()
for root, dirs, files in os.walk(test_files_dir):
    for sfile in files:
        if sfile.endswith('.idf') or sfile.endswith('.imf'):
            if root == test_files_dir:
                found_idf_files.add(sfile)
            else:
                folder = os.path.basename(os.path.normpath(root))
                found_idf_files.add(os.path.join(folder, sfile))

# there are a few files we purposely skip
files_to_skip = {"_1a-Long0.0.idf", "_ExternalInterface-actuator.idf", "_ExternalInterface-schedule.idf",
                 "_ExternalInterface-variable.idf", "HVAC3Zone-IntGains-Def.imf", "HVAC3ZoneChillerSpec.imf",
                 "HVAC3ZoneGeometry.imf", "HVAC3ZoneMat-Const.imf"}
found_idf_files_trimmed = found_idf_files - files_to_skip

# the CMakeLists file will always have forward slashes
# on Linux and Mac, the found_idfs will also have forward slashes
# but on Windows, the path delimiter will be a backslash
# so replace all backslashes here before comparing anything.
found_idf_files_refined = set()
for fil in found_idf_files_trimmed:
    found_idf_files_refined.add(fil.replace("\\", "/"))

# check if any are missing in cmake
need_to_add_to_cmake = found_idf_files_refined.difference(cmake_list_idf_files)
if len(need_to_add_to_cmake) > 0:
    for this_file in sorted(need_to_add_to_cmake):
        print(json.dumps({
            'tool': 'verify_idfs_in_cmake',
            'filename': this_file,
            'file': this_file,
            'line': 0,
            'messagetype': 'error',
            'message': 'File missing from testfiles/CMakeLists.txt'
        }))
