Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

set( CMAKE_VERSION_MAJOR 8 )
set( CMAKE_VERSION_MINOR 2 )
set( CMAKE_VERSION_PATCH 0 ) 

option( BUILD_PACKAGE "Build package" OFF )
option( BUILD_TESTING "Build testing targets" OFF )

# Not yet released by CMake, but is being developed
# This will help us set the proper options for c++11 across various configurations
# FIND_PACKAGE( CXXFeatures ) 

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 ")

if (UNIX AND NOT APPLE AND NOT WIN32)
  if (CMAKE_COMPILER_IS_GNUCXX)
  #if ( CMAKE_CXX_COMPILER MATCHES "g++" )
    SET( CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS} -pedantic -Wall -Wextra -Wno-unused-parameter -Wno-unused-but-set-parameter -Wno-unused-variable -Wno-unused-but-set-variable -Wno-unused-label -ffor-scope -fmessage-length=0 -m64 -march=native -O3 -DNDEBUG -s")
  endif()
endif()

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

