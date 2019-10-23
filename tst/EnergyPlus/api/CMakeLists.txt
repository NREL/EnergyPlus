
add_executable( TestAPI_Functional_C TestFunctional.c )
target_link_libraries( TestAPI_Functional_C energyplusapi )
add_test(NAME "API.TestFunctionalC" COMMAND TestAPI_Functional_C)

add_executable( TestAPI_Runtime_C TestRuntime.c )
target_link_libraries( TestAPI_Runtime_C energyplusapi )
add_test(NAME "API.TestRuntimeC" COMMAND TestAPI_Runtime_C)

add_executable( TestAPI_DataTransfer_C TestDataTransfer.c )
target_link_libraries( TestAPI_DataTransfer_C energyplusapi )
add_test(NAME "API.TestDataTransferC" COMMAND TestAPI_DataTransfer_C)

set(API_TEST_SRC_DIR "${CMAKE_SOURCE_DIR}/tst/EnergyPlus/api")
set(API_TEST_TARGET_DIR "${CMAKE_BINARY_DIR}/Products")

# This is tricky, the test scripts should be copied into the Products/<config> directory, but add_test has to be
# called so early, it's hard to figure that out ahead of time.  I dunno.  Just skipping these on MSVC builds for now.
if (NOT MSVC)
configure_file( "${API_TEST_SRC_DIR}/TestRuntime.py" "${API_TEST_TARGET_DIR}/TestRuntime.py" )
add_test(NAME "API.TestFunctionalPy"
        COMMAND "${PYTHON_EXECUTABLE}" "${API_TEST_TARGET_DIR}/TestFunctional.py"
        WORKING_DIRECTORY "${API_TEST_TARGET_DIR}")

configure_file( "${API_TEST_SRC_DIR}/TestFunctional.py" "${API_TEST_TARGET_DIR}/TestFunctional.py" )
add_test(NAME "API.TestRuntimePy"
        COMMAND "${PYTHON_EXECUTABLE}" "${API_TEST_TARGET_DIR}/TestRuntime.py"
        WORKING_DIRECTORY "${API_TEST_TARGET_DIR}")

configure_file( "${API_TEST_SRC_DIR}/TestDataTransfer.py" "${API_TEST_TARGET_DIR}/TestDataTransfer.py" )
add_test(NAME "API.TestDataTransferPy"
        COMMAND "${PYTHON_EXECUTABLE}" "${API_TEST_TARGET_DIR}/TestDataTransfer.py"
        WORKING_DIRECTORY "${API_TEST_TARGET_DIR}")
endif()
