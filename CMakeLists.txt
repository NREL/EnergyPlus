Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

option( BUILD_TESTING "Build testing targets" OFF )

#FIND_PACKAGE( CXXFeatures ) 

if(MSVC)
  add_definitions(/W3)
else()
  add_definitions(-Wall -Wnon-virtual-dtor -pedantic -std=c++11)
endif()


set( gtest_force_shared_crt ON )
set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )

include(cmake/ProjectMacros.cmake)

INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src/ )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src/gtest/include/ SYSTEM )

SET( CPACK_PACKAGE_VERSION_MAJOR 8 CACHE STRING "Major Version" )
SET( CPACK_PACKAGE_VERSION_MINOR 2 CACHE STRING "Minor Version" )
SET( CPACK_PACKAGE_VERSION_PATCH 0 CACHE STRING "Patch Version" )
# Build number incremented manually starting from where the official builds left off.
SET( BUILD_NUMBER "---" CACHE STRING "Build Number" )

if( BUILD_TESTING )
  enable_testing()
  include(CTest) 
endif()

ADD_SUBDIRECTORY(src/gtest)
ADD_SUBDIRECTORY(src/ObjexxFCL)
ADD_SUBDIRECTORY(src/EnergyPlus)

