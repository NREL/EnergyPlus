# Linking up to Python is not necessarily complex, but it is tricky to get right.
# Our goal here is to provide a fully packaged embedded Python interpreter and Python 3.5+ standard library
# This file's job is to copy over the required Python DLL and fixup the EnergyPlus binary on Mac to find on it locally
# To accomplish this, we need to know which EnergyPlus binary to fixup, and some info about the Python library
# The steps we follow are:
#  Use finder to get Python library and include paths
#  Add Python include path to include_directories
#  Link E+ against Python library
#  At install time find the Python library and copy it into the install tree
#  At install time find the Python site-packages folder and copy it into the install tree
#  In E+ need to setPath before calling PyInitialize so we can get all the site_packages
# Now we should also consider whether we want to try to build *without* Python
#  I can imagine doing this on the 32 bit Windows, for example.
#  And this would *not* exclude calling E+ as a library from C or Python -- it would just disable Python Plugins
#  If we don't do this then we'll need to install both 32 and 64 bit Python on Windows and get the right one

# We need to connect up to python for a couple reasons.
# 1. We use Python in our testing scripts
# 2. We link EnergyPlus up against the Python lib for Python Plugin work
# 3. We use Python for our Python API runs
# We are going to create a local virtual environment that is portable so we can package it and install it
# Users will not need to have Python installed, and it will come with a Python.exe and a Pip.exe for installing libraries

# set RESOLVED_PYTHON_LIB, and EXECUTABLE_PATH when calling

message("Collecting Python dynamic library")

# get some path info things
get_filename_component(BASE_PATH ${EXECUTABLE_PATH} DIRECTORY)
get_filename_component(PYTHON_LIB_FILENAME ${RESOLVED_PYTHON_LIB} NAME)

# on Windows, RESOLVED_PYTHON_LIB is pointing to the likes of `C:\Python38\libs\python38.lib`,
# when we actually want the DLL at `C:\Python38\python38.dll`
# need to fix that up here
if(WIN32)
  get_filename_component(LIB_SUB_DIR "${RESOLVED_PYTHON_LIB}" DIRECTORY)
  get_filename_component(PYTHON_ROOT_DIR "${LIB_SUB_DIR}" DIRECTORY)
  string(REPLACE ".lib" ".dll" DLL_FILE_NAME "${PYTHON_LIB_FILENAME}")
  set(RESOLVED_PYTHON_LIB "${PYTHON_ROOT_DIR}/${DLL_FILE_NAME}")
endif()

# then copy the DLL in
execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${RESOLVED_PYTHON_LIB}" "${BASE_PATH}")
