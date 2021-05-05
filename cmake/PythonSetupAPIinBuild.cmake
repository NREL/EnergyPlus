# Sets up the pyenergyplus Python package, which is the EnergyPlus Python API wrapper, in the build tree, next to E+
# set REPO_ROOT, EXECUTABLE_PATH (path to energyplus.exe), and E+ and API major/minor/etc version variables when calling

message("Full executable path is: ${EXECUTABLE_PATH}")

# get the parent path of the current EXE and drop the pyenergyplus stuff in there
get_filename_component(EPLUS_EXE_DIR ${EXECUTABLE_PATH} DIRECTORY)

# informative messaging
message("Setting up Python API, creating pyenergyplus package at ${EPLUS_EXE_DIR}/pyenergyplus")

# now do it
if(NOT EXISTS "${EPLUS_EXE_DIR}/pyenergyplus")
  file(MAKE_DIRECTORY "${EPLUS_EXE_DIR}/pyenergyplus")
endif()
set(API_SOURCE_DIR "${REPO_ROOT}/src/EnergyPlus/api")
set(API_TARGET_DIR "${EPLUS_EXE_DIR}/pyenergyplus")
configure_file("${API_SOURCE_DIR}/common.py" "${API_TARGET_DIR}/common.py")
configure_file("${API_SOURCE_DIR}/datatransfer.py" "${API_TARGET_DIR}/datatransfer.py")
configure_file("${API_SOURCE_DIR}/api.py" "${API_TARGET_DIR}/api.py")
#configure_file( "${API_SOURCE_DIR}/autosizing.py" "${API_TARGET_DIR}/autosizing.py" )
configure_file("${API_SOURCE_DIR}/func.py" "${API_TARGET_DIR}/func.py")
configure_file("${API_SOURCE_DIR}/runtime.py" "${API_TARGET_DIR}/runtime.py")
configure_file("${API_SOURCE_DIR}/plugin.py" "${API_TARGET_DIR}/plugin.py")
configure_file("${API_SOURCE_DIR}/state.py" "${API_TARGET_DIR}/state.py")
configure_file("${API_SOURCE_DIR}/__init__.py" "${API_TARGET_DIR}/__init__.py")
