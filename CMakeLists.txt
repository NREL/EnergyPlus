project(EnergyPlus)

cmake_minimum_required(VERSION 2.8)

set( CMAKE_VERSION_MAJOR 8 )
set( CMAKE_VERSION_MINOR 2 )
set( CMAKE_VERSION_PATCH 0 ) 

option( BUILD_PACKAGE "Build package" OFF )
option( BUILD_TESTING "Build testing targets" OFF )

# Not yet released by CMake, but is being developed
# This will help us set the proper options for c++11 across various configurations
# FIND_PACKAGE( CXXFeatures ) 

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 ")

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )

include(cmake/ProjectMacros.cmake)

include_directories( ${CMAKE_SOURCE_DIR}/src/ )
include_directories( ${CMAKE_SOURCE_DIR}/src/gtest/include/ SYSTEM )

if( BUILD_PACKAGE )
  include(CPack.cmake)
endif()

if( BUILD_TESTING )
  enable_testing()
  include(CTest) 
endif()

add_subdirectory(src/gtest)
add_subdirectory(src/ObjexxFCL)
add_subdirectory(src/EnergyPlus)

