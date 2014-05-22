Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

set( CMAKE_VERSION_MAJOR 8 )
set( CMAKE_VERSION_MINOR 2 )
set( CMAKE_VERSION_PATCH 0 ) 

option( BUILD_PACKAGE "Build package" OFF )
option( BUILD_TESTING "Build testing targets" OFF )
option( BUILD_FORTRAN "Build Fortran stuff" OFF )

# Not yet released by CMake, but is being developed
# This will help us set the proper options for c++11 across various configurations
#FIND_PACKAGE( CXXFeatures ) 
#added by Geof
SET(CMAKE_CXX_FLAGS_DEBUG "-ggdb -pg -O0 -DNDEBUG") #changed from Og
SET(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-ggdb -pg -O3 -DNDEBUG")
SET(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
SET(CMAKE_CXX_COMPILER "g++-4.8")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )

include(cmake/ProjectMacros.cmake)
include(cmake/CompilerFlags.cmake)

INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/gtest/include/ SYSTEM )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/ObjexxFCL/src/ )

if( BUILD_TESTING )
  option( TEST_ANNUAL_SIMULATION "Use annual simulations for tests instead of only design days" OFF )
  enable_testing()
  include(CTest) 
  ADD_SUBDIRECTORY(third_party/gtest)
endif()

ADD_SUBDIRECTORY(third_party/ObjexxFCL)
ADD_SUBDIRECTORY(src/EnergyPlus)
ADD_SUBDIRECTORY(testfiles)

if( BUILD_FORTRAN )
  include(CMakeAddFortranSubdirectory)
  cmake_add_fortran_subdirectory(src/ExpandObjects PROJECT ExpandObjects NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/ReadVars PROJECT ReadVars NO_EXTERNAL_INSTALL )
endif()

if( BUILD_PACKAGE )
  set(CPACK_INSTALL_CMAKE_PROJECTS
    "${CMAKE_BINARY_DIR};EnergyPLus;ALL;/"
  )

  if( BUILD_FORTRAN )
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/ExpandObjects/;ExpandObjects;ALL;/")
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/ReadVars/;ReadVars;ALL;/")
  endif()

  include(CPack.cmake)
endif()

# MESSAGE("CMake build type is: ${CMAKE_BUILD_TYPE}")
# MESSAGE("CMake CXX flags: ${FLAGS}")
IF(CMAKE_BUILD_TYPE STREQUAL "Debug" OR CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo")
	SET(CMAKE_LDD_FLAGS "-pg")
ENDIF()
