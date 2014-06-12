Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

set( CMAKE_VERSION_MAJOR 8 )
set( CMAKE_VERSION_MINOR 2 )
set( CMAKE_VERSION_PATCH 0 ) 

option( BUILD_PACKAGE "Build package" OFF )
option( BUILD_TESTING "Build testing targets" OFF )
option( BUILD_FORTRAN "Build Fortran stuff" OFF )

if( BUILD_TESTING )
  option( ENABLE_REGRESSION_TESTING "Enable Regression Tests" OFF )
  if( ENABLE_REGRESSION_TESTING )
    find_package(PythonInterp REQUIRED)
    set( REGRESSION_BASELINE_SHA "" CACHE STRING "SHA of baseline comparison" )
    set( COMMIT_SHA "" CACHE STRING "Commit sha of this build" )
    set( REGRESSION_SCRIPT_PATH "" CACHE PATH "Path to regression scripts" )
    set( REGRESSION_BASELINE_PATH "" CACHE PATH "Path to energyplus baseline folder to compare against" )
  endif()
endif()

# Not yet released by CMake, but is being developed
# This will help us set the proper options for c++11 across various configurations
#FIND_PACKAGE( CXXFeatures ) 

if (MSVC)
  set(gtest_force_shared_crt ON)
endif()


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

install( FILES idd/Energy+.idd DESTINATION ./ )

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

