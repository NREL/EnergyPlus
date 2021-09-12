cmake_minimum_required(VERSION 3.6)
project(LU-Decomposition)

include_directories(include)

add_library(LU-Decomposition
        src/lu.cpp)

# Only run tests if the main project is LU-Decomposition
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)

    enable_testing()

    add_executable(solve tests/solve.cpp)

    target_link_libraries(solve PRIVATE LU-Decomposition)

    add_test(NAME RunTest1 COMMAND "${CMAKE_BINARY_DIR}/solve")

endif()