
# set REPO_ROOT and BUILD_DIR when calling

if (NOT EXISTS "${CMAKE_BINARY_DIR}/Produces/api")
    file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/Products/api")
endif()

set(API_SOURCE_DIR "${REPO_ROOT}/src/EnergyPlus/api")
set(API_TARGET_DIR "${BUILD_DIR}/Products/api")
configure_file( "${API_SOURCE_DIR}/common.py" "${API_TARGET_DIR}/common.py" )
configure_file( "${API_SOURCE_DIR}/datatransfer.py" "${API_TARGET_DIR}/datatransfer.py" )
configure_file( "${API_SOURCE_DIR}/energyplus_api.py" "${API_TARGET_DIR}/energyplus_api.py" )
configure_file( "${API_SOURCE_DIR}/func.py" "${API_TARGET_DIR}/func.py" )
configure_file( "${API_SOURCE_DIR}/datatransfer.py" "${API_TARGET_DIR}/datatransfer.py" )
configure_file( "${API_SOURCE_DIR}/runtime.py" "${API_TARGET_DIR}/runtime.py" )
