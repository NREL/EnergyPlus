Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

option( BUILD_TESTING "Build testing targets" OFF )

include(cmake/ProjectMacros.cmake)

#FIND_PACKAGE( CXXFeatures ) 
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 ")

INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src/ )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src/gtest/include/ SYSTEM )

SET( CPACK_PACKAGE_VERSION_MAJOR 8 CACHE STRING "Major Version" )
SET( CPACK_PACKAGE_VERSION_MINOR 2 CACHE STRING "Minor Version" )
SET( CPACK_PACKAGE_VERSION_PATCH 0 CACHE STRING "Patch Version" )
# Build number incremented manually starting from where the official builds left off.
SET( BUILD_NUMBER "---" CACHE STRING "Build Number" )

ADD_SUBDIRECTORY(src/gtest)
ADD_SUBDIRECTORY(src/ObjexxFCL)
ADD_SUBDIRECTORY(src/EnergyPlus)

