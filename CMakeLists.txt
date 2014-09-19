Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

set( CMAKE_VERSION_MAJOR 8 )
set( CMAKE_VERSION_MINOR 2 )
set( CMAKE_VERSION_PATCH 0 ) 

option( BUILD_PACKAGE "Build package" OFF )
option( BUILD_TESTING "Build testing targets" OFF )
option( BUILD_FORTRAN "Build Fortran stuff" OFF )

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
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/SQLite/ SYSTEM )

if( BUILD_TESTING )
  option( TEST_ANNUAL_SIMULATION "Use annual simulations for tests instead of only design days" OFF )
  enable_testing()
  include(CTest) 
  ADD_SUBDIRECTORY(third_party/gtest)
  ADD_SUBDIRECTORY(testfiles)
endif()

ADD_SUBDIRECTORY(third_party/SQLite)
ADD_SUBDIRECTORY(third_party/ObjexxFCL)
ADD_SUBDIRECTORY(src/EnergyPlus)

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

  set(EPLUS_BUILDSUPPORT_REPO 
    "RootOfEnergyPlusBuildSupportRepo" CACHE PATH "Enter the root of the EnergyPlusBuildSupport repo"
  )
  
  if( BUILD_FORTRAN )
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/ExpandObjects/;ExpandObjects;ALL;/")
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/ReadVars/;ReadVars;ALL;/")
  endif()

  include(CPack.cmake)
endif()

