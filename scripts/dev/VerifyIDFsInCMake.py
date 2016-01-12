import sys
import os
import glob
from sets import Set

def usage():
    print("""This script verifies that the idf listing in CMakeLists.txt matches the actual idf files in the testfiles directory.
Call with one argument: the directory which includes idfs and the CMakeLists.txt file""")

testfiles_dir = ""
if len(sys.argv) != 2:
    print("Error: Bad arguments")
    usage()
    sys.exit(1)
else:
    testfiles_dir = sys.argv[1]

testfiles_cmakelists = os.path.join(testfiles_dir, "CMakeLists.txt")
cmake_idfs = Set()
with open(testfiles_cmakelists) as f:
    for line in f.readlines():
	line_stripped = line.strip()
	if "ADD_SIMULATION_TEST" in line_stripped:
	    idf_file_index = line_stripped.index("IDF_FILE")
	    epw_file_index = line_stripped.index("EPW_FILE")
	    filename = line_stripped[idf_file_index+9:epw_file_index-1]
	    cmake_idfs.add(filename)

found_idfs = Set()
for root, dirs, files in os.walk(testfiles_dir):
    for sfile in files:
        if sfile.endswith('.idf') or sfile.endswith('.imf'):
            if root == testfiles_dir:
		found_idfs.add(sfile)
	    else:
		folder = os.path.basename(os.path.normpath(root))
		found_idfs.add(os.path.join(folder,sfile))

# there are a few files we purposely skip
files_to_skip = Set(["_1a-Long0.0.idf", "_ExternalInterface-actuator.idf", "_ExternalInterface-schedule.idf", "_ExternalInterface-variable.idf", "HVAC3Zone-IntGains-Def.imf", "HVAC3ZoneChillerSpec.imf", "HVAC3ZoneGeometry.imf", "HVAC3ZoneMat-Const.imf"])
found_idfs_trimmed = found_idfs - files_to_skip

# the CMakeLists file will always have forward slashes
# on Linux and Mac, the found_idfs will also have forward slashes
# but on Windows, the path delimiter will be a backslash
# so replace all backslashes here before comparing anything.
found_idfs_refined = Set()
for fil in found_idfs_trimmed:
    found_idfs_refined.add(fil.replace("\\","/"))

# check if any are missing in cmake
need_to_add_to_cmake = found_idfs_refined.difference(cmake_idfs)
if len(need_to_add_to_cmake) > 0:
    print("[decent_ci:test_result:warn]: Files in idf folder missing from CMakeLists.txt.  Files:")
    for ffile in sorted(need_to_add_to_cmake):
	print("  " + ffile)
