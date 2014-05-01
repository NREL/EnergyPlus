Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

set( CMAKE_VERSION_MAJOR 8 )
set( CMAKE_VERSION_MINOR 2 )
set( CMAKE_VERSION_PATCH 0 ) 

option( BUILD_PACKAGE "Build package" OFF )
option( BUILD_TESTING "Build testing targets" OFF )

# Not yet released by CMake, but is being developed
# This will help us set the proper options for c++11 across various configurations
#FIND_PACKAGE( CXXFeatures ) 

if(MSVC)
  add_definitions(/W3 /MP)
else()
  add_definitions(-Wall -Wnon-virtual-dtor -pedantic -std=c++11)
endif()

set(gtest_force_shared_crt ON)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )

include(cmake/ProjectMacros.cmake)

INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/gtest/include/ SYSTEM )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/ObjexxFCL/src/ )

if( BUILD_PACKAGE )
  include(CPack.cmake)
endif()

if( BUILD_TESTING )
  enable_testing()
  include(CTest) 
endif()

ADD_SUBDIRECTORY(third_party/gtest)
ADD_SUBDIRECTORY(third_party/ObjexxFCL)
ADD_SUBDIRECTORY(src/EnergyPlus)
ADD_SUBDIRECTORY(testfiles)


