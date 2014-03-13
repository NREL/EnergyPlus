Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

#FIND_PACKAGE( CXXFeatures ) 
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 ")

INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src/ )

SET( CPACK_PACKAGE_VERSION_MAJOR 8 CACHE STRING "Major Version" )
SET( CPACK_PACKAGE_VERSION_MINOR 2 CACHE STRING "Minor Version" )
SET( CPACK_PACKAGE_VERSION_PATCH 0 CACHE STRING "Patch Version" )
# Build number incremented manually starting from where the official builds left off.
SET( BUILD_NUMBER "---" CACHE STRING "Build Number" )

ADD_SUBDIRECTORY(src/EnergyPlus)
ADD_SUBDIRECTORY(src/ObjexxFCL)

