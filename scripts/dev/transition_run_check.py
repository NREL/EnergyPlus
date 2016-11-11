#!/usr/bin/env python

# Attempt to automatically change the version number of all idf and imf files in a repo
# Two arguments: old version number and new version number

import sys
import shutil
import fnmatch
import os
import subprocess
import time
import re

# provide a nice usage function
def usage():
    print("""Call this script with three command line arguments:
 $ transition_run_check.py <path to idfs> <path to eplus build> <path to output dir>
 $ transition_run_check.py /repos/eplus /repos/eplus/build /tests/transitions""")

# check the command line argument status
if not len(sys.argv) == 4:
    print(len(sys.argv))
    print("Invalid command line arguments")
    usage()
    sys.exit(1)

transition_exe_name = "Transition-V8-5-0-to-V8-6-0.exe"
eplus_exe_name = "energyplus.exe"

err_file = open("error.txt", 'w')

# store the command line arguments since they appear valid so far
path_to_test_files = os.path.abspath(sys.argv[1])
path_to_build = os.path.abspath(sys.argv[2])
path_to_output = os.path.abspath(sys.argv[3])

path_to_build_exe = os.path.join(path_to_build, transition_exe_name)

# walk across
for ext in ['*.idf']:
    for root, dirnames, filenames in os.walk(path_to_test_files):
        for file in fnmatch.filter(filenames, ext):
            try:
                # copy to transition
                src_file_path = os.path.abspath(os.path.join(root,file))
                shutil.copy(src_file_path, path_to_build)

                # run transition
                test_file_path = os.path.abspath(os.path.join(path_to_build, file))
                transition_run = subprocess.Popen(["" + transition_exe_name, os.path.basename(test_file_path)], shell=True, cwd=path_to_build, stderr=subprocess.STDOUT)
                transition_run.communicate()

                time.sleep(.200)

                # copy files to test file dir
                base_test_file_name = file.split(".")[0]
                test_file_dir = os.path.join(path_to_output, base_test_file_name)
                try:
                    if os.path.exists(test_file_dir):
                        shutil.rmtree(test_file_dir)

                    os.makedirs(test_file_dir)
                except:
                    pass

                for _,_, copyfiles in os.walk(path_to_build):
                    for copyfile in fnmatch.filter(copyfiles, base_test_file_name + ".*"):
                        shutil.move(os.path.join(path_to_build, copyfile), test_file_dir)

                time.sleep(.200)

                # run e+
                eplus_run = subprocess.Popen(["" + eplus_exe_name, "-D", "-x", "-d", test_file_dir, os.path.join(test_file_dir, file)], shell=True, cwd=path_to_build, stderr=subprocess.STDOUT)
                eplus_run.communicate()

            except:
                print('Failed on file: %s' %(file))
                err_file.write('Failed on file: %s\n' %(file))

err_file.close()
