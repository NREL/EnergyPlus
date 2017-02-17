#!/usr/bin/env python

# Attempt to automatically change the version number of all idf and imf files in a repo
# Two arguments: old version number and new version number

import sys
import shutil
import fnmatch
import os
import subprocess
import time

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

transition_exe_name = "Transition-V8-6-0-to-V8-7-0.exe"
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

                # copy files to test file dir
                base_test_file_name = file.split(".")[0]
                test_file_dir = os.path.join(path_to_output, base_test_file_name)
                test_file_path = os.path.abspath(os.path.join(test_file_dir, file))

                if os.path.exists(test_file_dir):
                    shutil.rmtree(test_file_dir)

                os.makedirs(test_file_dir)

                shutil.copy(src_file_path, test_file_dir)

                # run transition
                transition_run = subprocess.Popen(["" + transition_exe_name, test_file_path], shell=True, cwd=path_to_build, stderr=subprocess.STDOUT)
                transition_run.communicate()

                time.sleep(.200)

                run_file_path = os.path.join(path_to_build, 'in.idf')
                shutil.copy(test_file_path, run_file_path)

                # run e+
                eplus_run = subprocess.Popen(["" + eplus_exe_name, "-D", "-x", "-d", test_file_dir], shell=True, cwd=path_to_build, stderr=subprocess.STDOUT)
                eplus_run.communicate()

            except:
                print('Failed on file: %s' %(file))
                err_file.write('Failed on file: %s\n' %(file))

err_file.close()
