
add_executable( TestAPI_Functional_Cpp TestFunctional.cc )
target_link_libraries( TestAPI_Functional_Cpp energyplusapi )
add_executable( TestAPI_Functional_C TestFunctional.c )
target_link_libraries( TestAPI_Functional_C energyplusapi )
#add_executable( TestAPI_Runtime_Cpp TestRuntime.cpp )
#target_link_libraries( TestAPI_Runtime_Cpp energyplusapi )
add_executable( TestAPI_Runtime_C TestRuntime.c )
target_link_libraries( TestAPI_Runtime_C energyplusapi )
add_executable( TestAPI_DataTransfer_C TestDataTransfer.c )
target_link_libraries( TestAPI_DataTransfer_C energyplusapi )
add_executable( TestAPI_DataTransfer2_C TestDataTransfer2.c )
target_link_libraries( TestAPI_DataTransfer2_C energyplusapi )

add_test(NAME "API.TestFunctionalCpp" COMMAND TestAPI_Functional_Cpp)
add_test(NAME "API.TestFunctionalC" COMMAND TestAPI_Functional_C)
add_test(NAME "API.TestRuntimeC" COMMAND TestAPI_Runtime_C)
add_test(NAME "API.TestDataTransferC" COMMAND TestAPI_DataTransfer_C)
add_test(NAME "API.TestDataTransfer2C" COMMAND TestAPI_DataTransfer2_C)

set(API_TEST_SRC_DIR "${CMAKE_SOURCE_DIR}/tst/EnergyPlus/api")
set(API_TEST_TARGET_DIR "${CMAKE_BINARY_DIR}/Products")
configure_file( "${API_TEST_SRC_DIR}/TestRuntime.py" "${API_TEST_TARGET_DIR}/TestRuntime.py" )
configure_file( "${API_TEST_SRC_DIR}/TestFunctional.py" "${API_TEST_TARGET_DIR}/TestFunctional.py" )
configure_file( "${API_TEST_SRC_DIR}/TestDataTransfer.py" "${API_TEST_TARGET_DIR}/TestDataTransfer.py" )
configure_file( "${API_TEST_SRC_DIR}/TestDataTransfer2.py" "${API_TEST_TARGET_DIR}/TestDataTransfer2.py" )
add_test(NAME "API.TestFunctionalPy"
        COMMAND "${PYTHON_EXECUTABLE}" "${API_TEST_TARGET_DIR}/TestFunctional.py"
        WORKING_DIRECTORY "${API_TEST_TARGET_DIR}")
add_test(NAME "API.TestRuntimePy"
        COMMAND "${PYTHON_EXECUTABLE}" "${API_TEST_TARGET_DIR}/TestRuntime.py"
        WORKING_DIRECTORY "${API_TEST_TARGET_DIR}")
add_test(NAME "API.TestDataTransferPy"
        COMMAND "${PYTHON_EXECUTABLE}" "${API_TEST_TARGET_DIR}/TestDataTransfer.py"
        WORKING_DIRECTORY "${API_TEST_TARGET_DIR}")
add_test(NAME "API.TestDataTransferPy2"
        COMMAND "${PYTHON_EXECUTABLE}" "${API_TEST_TARGET_DIR}/TestDataTransfer2.py"
        WORKING_DIRECTORY "${API_TEST_TARGET_DIR}")
