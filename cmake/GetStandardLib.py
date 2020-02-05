import os
import ctypes
ctypes_import_file = os.path.abspath(ctypes.__file__)
ctypes_package_dir = os.path.dirname(ctypes_import_file)
standard_lib_dir = os.path.dirname(ctypes_package_dir)
print(standard_lib_dir)
