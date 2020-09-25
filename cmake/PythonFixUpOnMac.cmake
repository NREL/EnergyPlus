# Only call this for APPLE
# set RESOLVED_PYTHON_LIB, and EXECUTABLE_PATH when calling

# when the build first completes, prior to fixing anything up, the executables have dependencies that need to be fixed up
# energyplus, the core binary
#  it has a dependency on the libenergyplusapi dynamic library, and it will be looking for it at: @rpath/libenergyplusapi.X.Y.Z.dylib
#  it will need to be changed to look for the api lib at @executable_path/libenergyplusapi.X.Y.Z.dylib
#  this is handled separately
#  however it also has a dependency on the core python dll at /some/path/to/Python.framework/Versions/3.7/{SomePythonLibName}
#  we are packing up the python lib with E+, so ti just needs to look for it at @executable_path/{SomePythonLibName}
# libenergyplusapi, the dynamic library
#  like the exe, this depends on the core python dll at /some/path/to/Python.framework/Versions/3.7/{SomePythonLibName}
#  we are packing up the python lib with E+, so it just needs to look for it at @executable_path/{SomePythonLibName}
# Python, or libpython3.7m.dylib, or whatever - the actual python library
#  this is the main python dynamic library that we distribute with e+
#  we just need to change the -id from /some/path/to/Python.framework/Versions/3.7/{SomePythonLibName} to @executable_path/{SomePythonLibName}

# the paths are dynamic, so we need to build out all these change commands somewhat dynamically
# the only thing we have to go on is that we know:
#   EXECUTABLE_PATH points to the current energyplus executable, and
#   EPLUS_DYNAMIC_LIB_NAME is the file _name_ of the E+ API dylib (libenergyplusapi.X.Y.Z.dylib)
#   RESOLVED_PYTHON_LIB points to the Python dynamic library

message("PYTHON: Fixing up Python Dependencies on Mac")

include(GetPrerequisites)

# message("ENERGYPLUS API DYNAMIC LIB NAME: ${EPLUS_DYNAMIC_LIB_NAME}")
# message("RESOLVED PYTHON LIB: ${RESOLVED_PYTHON_LIB}")

# get the python lib filename
get_filename_component(PYTHON_LIB_FILENAME ${RESOLVED_PYTHON_LIB} NAME)

# derive a few paths from the args passed in
get_filename_component(BASE_PATH ${EXECUTABLE_PATH} DIRECTORY)
set(LOCAL_PYTHON_LIBRARY "${BASE_PATH}/${PYTHON_LIB_FILENAME}")
set(ENERGYPLUS_API_PATH "${BASE_PATH}/${EPLUS_DYNAMIC_LIB_NAME}")

# now just fix up the pieces we need to fix up
execute_process(COMMAND "chmod" "+w" "${LOCAL_PYTHON_LIBRARY}")
execute_process(COMMAND "install_name_tool" -id "@executable_path/${PYTHON_LIB_FILENAME}" "${LOCAL_PYTHON_LIBRARY}")

# changing the libpythonwrapper Python prereq is a bit funny - we should search to get the exact string original
get_prerequisites("${ENERGYPLUS_API_PATH}" PREREQUISITES 1 1 "" "")
foreach(PREREQ IN LISTS PREREQUISITES)
    string(FIND "${PREREQ}" "${PYTHON_LIB_FILENAME}" PYTHON_IN_PREREQ)
    if (NOT PYTHON_IN_PREREQ EQUAL -1)
        execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@loader_path/${PYTHON_LIB_FILENAME}" "${ENERGYPLUS_API_PATH}")
    endif()
endforeach()

# and the exact same thing with the energyplus binary itself
get_prerequisites("${EXECUTABLE_PATH}" PREREQUISITES 1 1 "" "")
foreach(PREREQ IN LISTS PREREQUISITES)
    string(FIND "${PREREQ}" "${PYTHON_LIB_FILENAME}" PYTHON_IN_PREREQ)
    if (NOT PYTHON_IN_PREREQ EQUAL -1)
        execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PYTHON_LIB_FILENAME}" "${EXECUTABLE_PATH}")
    endif()
endforeach()

# and the python library itself may depend on a gettext lib on github action apparently
get_prerequisites("${LOCAL_PYTHON_LIBRARY}" PREREQUISITES 1 1 "" "")
foreach(PREREQ IN LISTS PREREQUISITES)
    string(FIND "${PREREQ}" "libint" LIBINT_IN_PREREQ)
    if (NOT LIBINT_IN_PREREQ EQUAL -1)
        get_filename_component(LIB_INT_FILENAME ${PREREQ} NAME)
        execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@loader_path/${LIB_INT_FILENAME}" "${ENERGYPLUS_API_PATH}")
    endif()
endforeach()
