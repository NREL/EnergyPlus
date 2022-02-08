# Only call this for APPLE
# set EPLUS_DYNAMIC_LIB_NAME and EXECUTABLE_PATH when calling

# the built binaries should be all set up and ready in the build tree
# however, some additional fixes need to be completed in the install tree
# energyplus, the core binary
#  it has a dependency on the libenergyplusapi dynamic library, and it will be looking for it at: @executable_path/libenergyplusapi.X.Y.Z.dylib
#  using @executable_path here works well because when you are running energyplus(.exe), it should definitely just be from the run directory
#  nothing we need to do
# libenergyplusapi, the dynamic library
#  like the exe, this depends on the core python dll at /usr/local/lib/libpython3.10.dylib
#  we are packing up the python lib with E+, so it will technically live at @executable_path/{SomePythonLibName}, HOWEVER
#  with the API work, the client can now call this dynamic library from other locations, where the calling executable is not in the E+ dir
#  because of this, we are not using @executable_path/, but instead @loader_path, which will allow the nested python dylib
#  to be found relative to the file loading it, which is this libenergyplusapi dynamic library
# libpython3.10.dylib, the actual python library
#  this is the main python dynamic library that we distribute with e+
#  I don't think we actually need to do anything with the -id
#  there is also a dependency on libintl.8.dylib which we fix up using the @loader_path approach

# the paths are dynamic, so we need to build out all these change commands somewhat dynamically
# the only thing we have to go on is that we know:
#   EXECUTABLE_PATH points to the current energyplus executable, and
#   EPLUS_DYNAMIC_LIB_NAME is the file _name_ of the E+ API dylib (libenergyplusapi.X.Y.Z.dylib)

message("PYTHON: Fixing up Python Dependencies on Mac")

include(GetPrerequisites)

# derive a few terms from the args passed in
# set(PYTHON_LIB_FILENAME "libpython3.10.dylib") # Python dylib file name
get_filename_component(BASE_PATH ${EXECUTABLE_PATH} DIRECTORY) # Path to the staged install tree in the build directory
# set(LOCAL_PYTHON_LIBRARY "${BASE_PATH}/${PYTHON_LIB_FILENAME}") # Path to the Python dylib once copied into the install tree
set(ENERGYPLUS_API_PATH "${BASE_PATH}/${EPLUS_DYNAMIC_LIB_NAME}") # Path to the EnergyPlus dylib once copied into the install tree

# for the energyplus dylib, search for the python dylib prereq and change it to use @loader_path
message("COMMAND install_name_tool -change @executable_path/libpython3.10.dylib @loader_path/libpython3.10.dylib ${ENERGYPLUS_API_PATH}")
execute_process(COMMAND install_name_tool -change @executable_path/libpython3.10.dylib @loader_path/libpython3.10.dylib "${ENERGYPLUS_API_PATH}")

# and the python library itself depends on a gettext lib (on our github actions builds anyway)
#execute_process(COMMAND "install_name_tool" -change "/usr/local/lib/${PYTHON_LIB_FILENAME}" "@loader_path/${PYTHON_LIB_FILENAME}" "${ENERGYPLUS_API_PATH}")
#
# TODO: Trying without this libintl
#get_prerequisites("${LOCAL_PYTHON_LIBRARY}" PREREQUISITES 1 1 "" "")
#foreach(PREREQ IN LISTS PREREQUISITES)
#  string(FIND "${PREREQ}" "libint" LIBINT_IN_PREREQ)
#  if(NOT LIBINT_IN_PREREQ EQUAL -1)
#    get_filename_component(LIB_INT_FILENAME ${PREREQ} NAME)
#    execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${PREREQ}" "${BASE_PATH}")
#    execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@loader_path/${LIB_INT_FILENAME}" "${LOCAL_PYTHON_LIBRARY}")
#  endif()
#endforeach()