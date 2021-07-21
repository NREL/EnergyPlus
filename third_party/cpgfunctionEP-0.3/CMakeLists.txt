cmake_minimum_required(VERSION 3.6)
project(cpgfunctionEP)

set(CMAKE_CXX_STANDARD 17)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --std=c++17")
else()
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
    add_compile_definitions(_USE_MATH_DEFINES)
endif()

# Everything will need to access this
include_directories(include)
# Third party includes
include_directories(third_party/eigen-3.3.9)  # Eigen for LU decomposition
include_directories(third_party/qdt-master)

# Create a DLL with an API
add_library(cpgfunctionEP SHARED
        src/boreholes.cpp
        src/gfunction.cpp
        src/heat_transfer.cpp
        src/interpolation.cpp
        src/utilities.cpp
        src/coordinates.cpp
        src/statistics.cpp
        src/segments.cpp
        src/blas.cpp)

# It depends on threads
find_package(Threads REQUIRED)
target_link_libraries(cpgfunctionEP ${CMAKE_THREAD_LIBS_INIT})

OPTION (USE_OpenMP "Use OpenMP" ON)
IF(USE_OpenMP)
    FIND_PACKAGE(OpenMP)
    IF(OPENMP_FOUND)
        SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    ENDIF()
ENDIF()

# if we are only building cpgfnctionEP
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    include_directories(third_party/nlohmann-3.9.1)
    # Turn on testing
    enable_testing()

    # Create a test binary that depends on the DLL
    add_executable(gFunction_minimal test/gFunction_minimal.cpp)
    add_executable(interpolation test/interpolation.cpp)
    # add_executable(test3 test/configurations.cpp)
    add_executable(borefield_definition test/borefield_definition.cpp)
    add_executable(time_definition test/time_definition.cpp)
    add_executable(gFunction_tests test/gFunction_tests.cpp)
    add_executable(packed test/packed.cpp)
    add_executable(gFunction_adaptive test/gFunction_adaptive.cpp)

    target_link_libraries(gFunction_minimal cpgfunctionEP)
    target_link_libraries(interpolation cpgfunctionEP)
    # target_link_libraries(test3 cpgfunctionEP)
    target_link_libraries(borefield_definition cpgfunctionEP)
    target_link_libraries(time_definition cpgfunctionEP)
    target_link_libraries(gFunction_tests cpgfunctionEP)
    target_link_libraries(packed cpgfunctionEP)
    target_link_libraries(gFunction_adaptive cpgfunctionEP)

    # Copy validation files to build directory so tests can open
    file(GLOB JsonValidation test/validation/*.json)
    file(GLOB CustomLayout test/custom_configurations/*.json)

    file(COPY ${JsonValidation} DESTINATION ${CMAKE_BINARY_DIR})
    file(COPY ${CustomLayout} DESTINATION ${CMAKE_BINARY_DIR})
    message(STATUS "The binary directory: ${CMAKE_BINARY_DIR}")

    # Test the test binary itself
    add_test(NAME RunTest1 COMMAND "${CMAKE_BINARY_DIR}/gFunction_minimal")
    add_test(NAME RunTest2 COMMAND "${CMAKE_BINARY_DIR}/interpolation")
    # add_test(NAME RunTest3 COMMAND "${CMAKE_BINARY_DIR}/test3")
    add_test(NAME RunTest4 COMMAND "${CMAKE_BINARY_DIR}/borefield_definition")
    add_test(NAME RunTest5 COMMAND "${CMAKE_BINARY_DIR}/time_definition")
    # Pass variable path into test 6 for json files
    add_test(NAME RunTest6 COMMAND ${CMAKE_BINARY_DIR}/gFunction_tests)
    add_test(NAME RunTest7 COMMAND ${CMAKE_BINARY_DIR}/packed)
    add_test(NAME RunTest8 COMMAND ${CMAKE_BINARY_DIR}/gFunction_adaptive)
endif()
