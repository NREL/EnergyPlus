# Only call this for APPLE
# set RESOLVED_PYTHON_LIB, and EXECUTABLE_PATH when calling

# when the build first completes, prior to fixing anything up, the executables have dependencies that need to be fixed up
# energyplus, the core binary
#  it has a dependency on the libenergyplusapi dynamic library, and it will be looking for it at: @rpath/libenergyplusapi.X.Y.Z.dylib
#  it will need to be changed to look for the api lib at @executable_path/libenergyplusapi.X.Y.Z.dylib
#  this is handled separately
# libenergyplusapi, the dynamic library
#  now that we have detached the python dynamic lib into a lazy load, this won't have anything to change, w00t!
# libpythonwrapper, the python wrapper/interface
#  this library was added to detach energyplus from a hard dependency on the python dylib, and e+ now instead lazy loads this interface
#  this wrapper depends on the core python dll at /some/path/to/Python.framework/Versions/3.7/Python
#  we are packing up the python lib with E+, so it just needs to look for it at @executable_path/Python
#  we also change the -id from @rpath/libpythonwrapper to @executable_path/libpythonwrapper
# Python, the actual python library
#  this is the main python dynamic library that we distribute with e+
#  we just need to change the -id from /some/path/to/Python.framework/Versions/3.7/Python to @executable_path/Python

# the paths are dynamic, so we need to build out all these change commands somewhat dynamically
# the only thing we have to go on is that we know:
#   EXECUTABLE_PATH points to the current energyplus executable, and
#   RESOLVED_PYTHON_LIB points to the Python dynamic library

message("Fixing up Python Dependencies on Mac")

# hard wire a couple things for now, could be changed to pass in other stuff
set(PYTHON_LIB_FILENAME "Python")
set(WRAPPER_FILE_NAME "libpythonwrapper.dylib")

# derive a few paths from the args passed in
get_filename_component(BASE_PATH ${EXECUTABLE_PATH} DIRECTORY)
set(LOCAL_PYTHON_LIBRARY "${BASE_PATH}/${PYTHON_LIB_FILENAME}")
set(PYTHON_WRAPPER_PATH "${BASE_PATH}/${WRAPPER_FILE_NAME}")

# now just fix up the pieces we need to fix up
execute_process(COMMAND "install_name_tool" -id "@executable_path/${PYTHON_LIB_FILENAME}" "${LOCAL_PYTHON_LIBRARY}")
execute_process(COMMAND "install_name_tool" -id "@executable_path/${WRAPPER_FILE_NAME}" "${PYTHON_WRAPPER_PATH}")

# changing the libpythonwrapper Python prereq is a bit funny - we should search to get the exact string original
include(GetPrerequisites)
get_prerequisites("${PYTHON_WRAPPER_PATH}" PREREQUISITES 1 1 "" "")
foreach(PREREQ IN LISTS PREREQUISITES)
    string(FIND "${PREREQ}" "Python" PYTHON_IN_PREREQ)
    if (NOT PYTHON_IN_PREREQ EQUAL -1)
        execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PYTHON_LIB_FILENAME}" "${PYTHON_WRAPPER_PATH}")
    endif()
endforeach()
