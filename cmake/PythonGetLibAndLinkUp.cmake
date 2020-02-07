# Steps we need to follow for now:
#  Use finder to get Python executable
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

# set RESOLVED_PYTHON_LIB, PYTHON_LIB_FILENAME, and EXECUTABLE_PATH when calling

message("Setting up Python:\n RESOLVED_PYTHON_LIB: ${RESOLVED_PYTHON_LIB}\n PYTHON_LIB_FILENAME: ${PYTHON_LIB_FILENAME}\n EXECUTABLE_PATH: ${EXECUTABLE_PATH}")

include(GetPrerequisites)
get_filename_component(PYTHON_LIB_FILENAME "${RESOLVED_PYTHON_LIB}" NAME)
get_filename_component(BASE_PATH "${EXECUTABLE_PATH}" DIRECTORY)
execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${RESOLVED_PYTHON_LIB}" "${BASE_PATH}")
if(APPLE)
  get_prerequisites("${EXECUTABLE_PATH}" PREREQUISITES 1 1 "" "")
  foreach(PREREQ IN LISTS PREREQUISITES)
    string(TOLOWER "${PREREQ}" PREREQ_LOWERCASE )
    string(FIND "${PREREQ_LOWERCASE}" "python" PYTHON_IN_PREREQ)
    if (NOT PYTHON_IN_PREREQ EQUAL -1)
      gp_resolve_item("" "${PREREQ}" "" "${LIBRARY_SEARCH_DIRECTORY}" resolved_item_var)
      execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PYTHON_LIB_FILENAME}" "${EXECUTABLE_PATH}")
    endif()
  endforeach()
endif()
