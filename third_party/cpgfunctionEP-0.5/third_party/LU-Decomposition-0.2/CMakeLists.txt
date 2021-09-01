cmake_minimum_required(VERSION 3.6)
project(LU-Decomposition)

set(CMAKE_CXX_STANDARD 17)
include_directories(include)

add_library(LU-Decomposition
        src/lu.cpp)

option (USE_OpenMP "Use OpenMP" ON)
if(USE_OpenMP)
    find_package(OpenMP)
    if(OpenMP_CXX_FOUND)
        target_link_libraries(LU-Decomposition PRIVATE OpenMP::OpenMP_CXX)
    else()
        message(AUTHOR_WARNING "OpenMP_CXX not found")
    endif()
endif()

# Only run tests if the main project is LU-Decomposition
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)

    enable_testing()

    add_executable(solve tests/solve.cpp)

    target_link_libraries(solve PRIVATE LU-Decomposition)

    add_test(NAME RunTest1 COMMAND "${CMAKE_BINARY_DIR}/solve")

endif()