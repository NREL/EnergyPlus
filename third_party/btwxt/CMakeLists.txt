cmake_minimum_required(VERSION 3.10)

project(btwxt)

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

find_package(Git QUIET)

set (CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

include(compiler-flags)

option( BUILD_BTWXT_TESTING "Build Btwxt testing targets" OFF )
option( BTWXT_COVERAGE "Add coverage reports" OFF)

# Set up testing/coverage
if (BUILD_BTWXT_TESTING)
  enable_testing()
  if (BTWXT_COVERAGE)
    set(ENABLE_COVERAGE ON CACHE BOOL "" FORCE)
    find_package(codecov)
  endif()
endif()


add_subdirectory(src)
add_subdirectory(vendor)

if (BUILD_BTWXT_TESTING)
  add_subdirectory(test)
  if (BTWXT_COVERAGE)
    coverage_evaluate()
  endif()
endif()
