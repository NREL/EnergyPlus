
# set REPO_ROOT, BUILD_DIR, and E+ and API major/minor/etc version variables when calling

if (NOT EXISTS "${CMAKE_BINARY_DIR}/Produces/pyenergyplus")
    file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/Products/pyenergyplus")
endif()

set(API_SOURCE_DIR "${REPO_ROOT}/src/EnergyPlus/api")
set(API_TARGET_DIR "${BUILD_DIR}/Products/pyenergyplus")
configure_file( "${API_SOURCE_DIR}/common.py" "${API_TARGET_DIR}/common.py" )
configure_file( "${API_SOURCE_DIR}/datatransfer.py" "${API_TARGET_DIR}/datatransfer.py" )
configure_file( "${API_SOURCE_DIR}/api.py" "${API_TARGET_DIR}/api.py" )
configure_file( "${API_SOURCE_DIR}/func.py" "${API_TARGET_DIR}/func.py" )
configure_file( "${API_SOURCE_DIR}/datatransfer.py" "${API_TARGET_DIR}/datatransfer.py" )
configure_file( "${API_SOURCE_DIR}/runtime.py" "${API_TARGET_DIR}/runtime.py" )
configure_file( "${API_SOURCE_DIR}/plugin.py" "${API_TARGET_DIR}/plugin.py" )
configure_file( "${API_SOURCE_DIR}/__init__.py" "${API_TARGET_DIR}/__init__.py" )
