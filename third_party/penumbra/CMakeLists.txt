cmake_minimum_required(VERSION 3.7)
project(penumbra)

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

set (CMAKE_MODULE_PATH "${penumbra_SOURCE_DIR}/cmake")

include(compiler-flags)

option( BUILD_PENUMBRA_TESTING "Build Penumbra testing targets" OFF )
option( PENUMBRA_COVERAGE "Add coverage reports" OFF)

# Set up testing/coverage
if (BUILD_PENUMBRA_TESTING)
  enable_testing()
  if (PENUMBRA_COVERAGE)
    set(ENABLE_COVERAGE ON CACHE BOOL "" FORCE)
    find_package(codecov)
  endif()
endif()


# Dependencies
add_subdirectory(vendor)

add_subdirectory(src)
add_subdirectory(examples)

if (BUILD_PENUMBRA_TESTING)
  add_subdirectory(test)
  if (PENUMBRA_COVERAGE)
    coverage_evaluate()
  endif()
endif()
