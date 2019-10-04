
add_executable( TestAPI_Functional_Cpp TestFunctional.cc )
target_link_libraries( TestAPI_Functional_Cpp energyplusapi )
add_executable( TestAPI_Functional_C TestFunctional.c )
target_link_libraries( TestAPI_Functional_C energyplusapi )
configure_file( TestFunctional.py "${CMAKE_BINARY_DIR}/Products/TestFunctional.py" )

configure_file( TestRuntime.py "${CMAKE_BINARY_DIR}/Products/TestRuntime.py" )

add_test(NAME "API.TestFunctionalCpp" COMMAND TestAPI_Functional_Cpp)
add_test(NAME "API.TestFunctionalC" COMMAND TestAPI_Functional_C)
add_test(NAME "API.TestFunctionalPy" COMMAND "${PYTHON_EXECUTABLE}" "${CMAKE_BINARY_DIR}/Products/TestFunctional.py")
add_test(NAME "API.TestRuntimePy" COMMAND "${PYTHON_EXECUTABLE}" "${CMAKE_BINARY_DIR}/Products/TestRuntime.py")