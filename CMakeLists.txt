project(EnergyPlus)

cmake_minimum_required(VERSION 2.8)

set( CMAKE_VERSION_MAJOR 8 )
set( CMAKE_VERSION_MINOR 2 )
set( CMAKE_VERSION_PATCH 0 ) 

option( BUILD_PACKAGE "Build package" OFF )

# Not yet released by CMake, but is being developed
# This will help us set the proper options for c++11 across various configurations
# FIND_PACKAGE( CXXFeatures ) 

# Not robust to configurations
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 ")

include_directories( ${CMAKE_SOURCE_DIR}/src/ )

add_subdirectory(src/EnergyPlus)
add_subdirectory(src/ObjexxFCL)

if( BUILD_PACKAGE )
  include(CPack.cmake)
endif()

