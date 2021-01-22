# Copies the Python standard library to the target dir
# If the target_dir already exists, it doesn't do anything
# an easy way is to find the __init__ file for a known standard lib package and go two parents up
# should result in something like `/usr/lib/python3.7`

# this script must be called with two args:
# 1 - the path to the EnergyPlus executable in the install tree, which is be used to determine where to copy the library
#     since this is in the install tree, you'll need to use a cmake generator expression
# 2 - name of the folder to create to store the copied in python standard library, usually python_standard_library
import ctypes
from distutils.dir_util import copy_tree
import os
import platform
import shutil
import sys
import glob as gb

print("PYTHON: Copying standard library files")

if len(sys.argv) == 3:
    exe_path = sys.argv[1]
    folder_name = sys.argv[2]
else:
    print("Must call " + sys.argv[0] + "with two command line arguments: the path to the energyplus exe and the name "
                                       "of the new library directory")
    sys.exit(1)
exe_dir = os.path.dirname(exe_path)
target_dir = os.path.join(exe_dir, folder_name)

ctypes_import_file = os.path.abspath(ctypes.__file__)
ctypes_package_dir = os.path.dirname(ctypes_import_file)
standard_lib_dir = os.path.dirname(ctypes_package_dir)

if os.path.exists(target_dir):
    # Let's check the library files to see if the ABI matches
    # Otherwise if you build with say python 3.8 initially, and then switch to
    # python 3.9, your lib-dynload will still have the 38 .so files
    std_lib_ctypes_sos = gb.glob(os.path.join(standard_lib_dir, "**/_ctypes.*"), recursive=True)
    this_lib_ctypes_sos = gb.glob(os.path.join(target_dir, "**/_ctypes.*"), recursive=True)

    def find_libs(dirPath):
        sos = []
        for ext in ['a', 'so', 'lib']:
            sos += gb.glob(os.path.join(dirPath, "**/*.{}*".format(ext)),
                           recursive=True)
        return [os.path.basename(f) for f in sos]

    std_lib_ctypes_sos = find_libs(standard_lib_dir)
    this_lib_ctypes_sos = find_libs(target_dir)

    if ((set(std_lib_ctypes_sos) - set(this_lib_ctypes_sos)) or
        (set(this_lib_ctypes_sos) - set(std_lib_ctypes_sos))):
        print("Detected changes in the python libs, wiping and recopying")
        shutil.rmtree(target_dir)
    else:
        # File names match
        sys.exit(0)


shutil.copytree(standard_lib_dir, target_dir)

# On Windows, we also need to grab the DLLs folder, which is one folder up
if platform.system() == 'Windows':
    python_root_dir = os.path.dirname(standard_lib_dir)
    dll_dir = os.path.join(python_root_dir, 'DLLs')
    copy_tree(dll_dir, target_dir)

# then I'm going to try to clean up any __pycache__ folders in the target dir to reduce installer size
for root, dirs, _ in os.walk(target_dir):
    for this_dir in dirs:
        if this_dir == "__pycache__":
            shutil.rmtree(os.path.join(root, this_dir))

# on Windows the site_packages folder is inside the standard lib folder, so we need to delete that too
if os.path.exists(os.path.join(target_dir, 'site-packages')):
    shutil.rmtree(os.path.join(target_dir, 'site-packages'))
