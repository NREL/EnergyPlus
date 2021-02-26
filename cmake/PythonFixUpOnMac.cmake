# Only call this for APPLE
# set RESOLVED_PYTHON_LIB, EPLUS_DYNAMIC_LIB_NAME and EXECUTABLE_PATH when calling

# when the build first completes, during packaging, the executables have dependencies that need to be fixed up
# energyplus, the core binary
#  it has a dependency on the libenergyplusapi dynamic library, and it will be looking for it at: @rpath/libenergyplusapi.X.Y.Z.dylib
#  it will need to be changed to look for the api lib at @executable_path/libenergyplusapi.X.Y.Z.dylib
#  this is handled separately
#  however it also has a dependency on the core python dll at /some/path/to/Python.framework/Versions/3.7/{SomePythonLibName}
#  we are packing up the python lib with E+, so it just needs to look for it at @executable_path/{SomePythonLibName}
#  using @executable_path here works well because when you are running energyplus(.exe), it should definitely just be from the run directory
# libenergyplusapi, the dynamic library
#  like the exe, this depends on the core python dll at /some/path/to/Python.framework/Versions/3.7/{SomePythonLibName}
#  we are packing up the python lib with E+, so it will technically live at @executable_path/{SomePythonLibName}, HOWEVER
#  with the API work, the client can now call this dynamic library from other locations, where the calling executable is not in the E+ dir
#  because of this, we are not using @executable_path/, but instead @loader_path, which will allow the nested python dylib
#  to be found relative to the file loading it, which is this libenergyplusapi dynamic library
# Python, or libpython3.7m.dylib, or whatever - the actual python library
#  this is the main python dynamic library that we distribute with e+
#  we change the -id from /some/path/to/Python.framework/Versions/3.7/{SomePythonLibName} to @executable_path/{SomePythonLibName}
#  there is also a dependency on libintl.8.dylib which we fix up using the @loader_path approach

# TODO: Look at whether we could do @loader_path across the board to make these things much easier, maybe even just put @loader_path inside fixup_executable...
# TODO: Look at whether we actually need to set any of these blob's IDs; I'm not sure

# the paths are dynamic, so we need to build out all these change commands somewhat dynamically
# the only thing we have to go on is that we know:
#   EXECUTABLE_PATH points to the current energyplus executable, and
#   EPLUS_DYNAMIC_LIB_NAME is the file _name_ of the E+ API dylib (libenergyplusapi.X.Y.Z.dylib)
#   RESOLVED_PYTHON_LIB points to the Python dynamic library

message("PYTHON: Fixing up Python Dependencies on Mac")

include(GetPrerequisites)

# derive a few terms from the args passed in
get_filename_component(PYTHON_LIB_FILENAME ${RESOLVED_PYTHON_LIB} NAME) # Python dylib file name
get_filename_component(BASE_PATH ${EXECUTABLE_PATH} DIRECTORY) # Path to the staged install tree in the build directory
set(LOCAL_PYTHON_LIBRARY "${BASE_PATH}/${PYTHON_LIB_FILENAME}") # Path to the Python dylib once copied into the install tree
set(ENERGYPLUS_API_PATH "${BASE_PATH}/${EPLUS_DYNAMIC_LIB_NAME}") # Path to the EnergyPlus dylib once copied into the install tree

# the Python dylib apparently needed chmod +x at one point; # TODO: Try without this...
execute_process(COMMAND "chmod" "+w" "${LOCAL_PYTHON_LIBRARY}")

# then we set the ID on the Python dylib; # TODO: Try without this...
execute_process(COMMAND "install_name_tool" -id "@executable_path/${PYTHON_LIB_FILENAME}" "${LOCAL_PYTHON_LIBRARY}")

# for the energyplus executable, just find the python dynamic library right next to it for sure
get_prerequisites("${EXECUTABLE_PATH}" PREREQUISITES 1 1 "" "")
foreach(PREREQ IN LISTS PREREQUISITES)
  string(FIND "${PREREQ}" "${PYTHON_LIB_FILENAME}" PYTHON_IN_PREREQ)
  if(NOT PYTHON_IN_PREREQ EQUAL -1)
    execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PYTHON_LIB_FILENAME}" "${EXECUTABLE_PATH}")
  endif()
endforeach()

# for the energyplus dylib, search for the python dylib prereq and change it to use @loader_path
get_prerequisites("${ENERGYPLUS_API_PATH}" PREREQUISITES 1 1 "" "")
foreach(PREREQ IN LISTS PREREQUISITES)
  string(FIND "${PREREQ}" "${PYTHON_LIB_FILENAME}" PYTHON_IN_PREREQ)
  if(NOT PYTHON_IN_PREREQ EQUAL -1)
    execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@loader_path/${PYTHON_LIB_FILENAME}" "${ENERGYPLUS_API_PATH}")
  endif()
endforeach()

# and the python library itself depends on a gettext lib (on our github actions builds anyway)
get_prerequisites("${LOCAL_PYTHON_LIBRARY}" PREREQUISITES 1 1 "" "")
foreach(PREREQ IN LISTS PREREQUISITES)
  string(FIND "${PREREQ}" "libint" LIBINT_IN_PREREQ)
  if(NOT LIBINT_IN_PREREQ EQUAL -1)
    get_filename_component(LIB_INT_FILENAME ${PREREQ} NAME)
    execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${PREREQ}" "${BASE_PATH}")
    execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@loader_path/${LIB_INT_FILENAME}" "${LOCAL_PYTHON_LIBRARY}")
  endif()
endforeach()
