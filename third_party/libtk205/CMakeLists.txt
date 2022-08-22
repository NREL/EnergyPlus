cmake_minimum_required (VERSION 3.10)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
project(libtk205)

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

include(CTest)
set(JSON_BuildTests OFF CACHE INTERNAL "")
option( LIBTK205_BUILD_TESTING "Build libtk205 testing targets." ON )

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
add_subdirectory(src)
add_subdirectory(vendor)

if (LIBTK205_BUILD_TESTING)
  add_compile_definitions(TEST205_INPUT_EXAMPLES_DIR="${PROJECT_SOURCE_DIR}")
  enable_testing()
  add_subdirectory(test)
endif()

# set(CPACK_PROJECT_NAME ${PROJECT_NAME})
# set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
# include(CPack)
