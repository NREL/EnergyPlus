Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

set( CMAKE_VERSION_MAJOR 8 )
set( CMAKE_VERSION_MINOR 2 )
set( CMAKE_VERSION_PATCH 3 ) 

set( CMAKE_VERSION_BUILD "Unknown" CACHE STRING "Build number" )
find_package(Git)

if(NOT GIT_FOUND)
  find_program(GIT_EXECUTABLE git HINTS "$ENV{LOCALAPPDATA}/Programs/git/bin")
  if (NOT GIT_EXECUTABLE_NOTFOUND)
    set(GIT_FOUND TRUE)
  endif()
endif()

if(GIT_FOUND)
  execute_process(COMMAND "${GIT_EXECUTABLE}" "rev-parse" "--short=10" "HEAD"
                  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
                  TIMEOUT 10
                  RESULT_VARIABLE RESULT
                  OUTPUT_VARIABLE GIT_VERSION
                  ERROR_QUIET
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(${RESULT} EQUAL 0 AND NOT "${GIT_VERSION}" EQUAL "${CMAKE_VERSION_BUILD}")
    set(CMAKE_VERSION_BUILD ${GIT_VERSION} CACHE STRING "Build number" FORCE) # git sha
  endif()

  get_filename_component(GIT_DIR "${GIT_EXECUTABLE}" PATH)
else()
  set(GIT_DIR "")
endif()


set(CPACK_PACKAGE_CONTACT "Kyle Benne <Kyle.Benne@nrel.gov>")

option( BUILD_PACKAGE "Build package" OFF )
option( BUILD_TESTING "Build testing targets" OFF )
option( BUILD_FORTRAN "Build Fortran stuff" OFF )

if( BUILD_TESTING )
  option( ENABLE_REGRESSION_TESTING "Enable Regression Tests" OFF )
  if( ENABLE_REGRESSION_TESTING )
    if ( CMAKE_CL_64 )
      set( ARCH_FLAG "-64bit" )
    else()
      set( ARCH_FLAG "" )
    endif()

    find_package(PythonInterp REQUIRED)
    set( REGRESSION_BASELINE_SHA "" CACHE STRING "SHA of baseline comparison" )
    set( COMMIT_SHA "" CACHE STRING "Commit sha of this build" )
    set( REGRESSION_SCRIPT_PATH "" CACHE PATH "Path to regression scripts" )
    set( REGRESSION_BASELINE_PATH "" CACHE PATH "Path to energyplus baseline folder to compare against" )
    set( DEVICE_ID "${CMAKE_SYSTEM_NAME}-${CMAKE_CXX_COMPILER_ID}${ARCH_FLAG}" CACHE STRING "Identifier for this device configuration" )
  endif()
endif()

if( ENABLE_REGRESSION_TESTING )
  string(STRIP "${REGRESSION_SCRIPT_PATH}" REG_PATH_STRIPPED)
  string(STRIP "${REGRESSION_BASELINE_PATH}" REG_BASELINE_STRIPPED)
  if (REG_PATH_STRIPPED STREQUAL "" OR REG_BASELINE_STRIPPED STREQUAL "")
    message( "Regression testing is enabled, but no script or baseline has been provided" )
    set( DO_REGRESSION_TESTING OFF )
  else()
    set( DO_REGRESSION_TESTING ON )
  endif()
endif()

if( UNIX AND NOT APPLE )
  set(CMAKE_INSTALL_RPATH "$ORIGIN")
endif()

if (MSVC)
  if (CMAKE_C_COMPILER_VERSION VERSION_LESS "18.00.30723")
    message(FATAL_ERROR "Visual Studio 2013 Update 3 or Greater Required")
  endif()
  set(gtest_force_shared_crt ON)
endif()

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )

include(cmake/ProjectMacros.cmake)
include(cmake/CompilerFlags.cmake)

INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/zlib )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/gtest/include/ SYSTEM )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/ObjexxFCL/src/ )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/SQLite/ SYSTEM )
INCLUDE_DIRECTORIES("${CMAKE_SOURCE_DIR}/third_party/Expat" "${CMAKE_SOURCE_DIR}/third_party/Expat/lib" SYSTEM)

# E+ required libraries
ADD_SUBDIRECTORY(third_party/SQLite)
ADD_SUBDIRECTORY(third_party/ObjexxFCL)
ADD_SUBDIRECTORY(third_party/BCVTB)
ADD_SUBDIRECTORY(third_party/Expat)
ADD_SUBDIRECTORY(third_party/FMI)
ADD_SUBDIRECTORY(third_party/zlib)
ADD_SUBDIRECTORY(third_party/DElight)
IF(NOT APPLE )
  ADD_SUBDIRECTORY(third_party/FMUParser)
ENDIF()

# of course E+ itself
ADD_SUBDIRECTORY(src/EnergyPlus)

if( BUILD_TESTING )
  option( TEST_ANNUAL_SIMULATION "Use annual simulations for tests instead of only design days" OFF )
  enable_testing()
  include(CTest)
  ADD_SUBDIRECTORY(third_party/gtest)
  ADD_SUBDIRECTORY(testfiles)
  ADD_SUBDIRECTORY(tst/EnergyPlus/unit)
endif()

if( BUILD_FORTRAN )
  include(CMakeAddFortranSubdirectory)
  cmake_add_fortran_subdirectory(src/ExpandObjects PROJECT ExpandObjects NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/ReadVars PROJECT ReadVars NO_EXTERNAL_INSTALL )
  set(TRANSITION_EXTRA_FLAGS "-DCMAKE_VERSION_MAJOR:STRING=${CMAKE_VERSION_MAJOR}" "-DCMAKE_VERSION_MINOR:STRING=${CMAKE_VERSION_MINOR}" "-DCMAKE_VERSION_PATCH:STRING=${CMAKE_VERSION_PATCH}")
  cmake_add_fortran_subdirectory(src/Transition PROJECT Transition CMAKE_COMMAND_LINE ${TRANSITION_EXTRA_FLAGS} NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/Basement PROJECT Basement NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/HVAC-Diagram PROJECT HVAC-Diagram NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/ParametricPreprocessor PROJECT ParametricPreprocessor NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/Slab PROJECT Slab NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/ConvertESOMTR PROJECT ConvertESOMTR NO_EXTERNAL_INSTALL )
endif()

configure_file( idd/Energy+.idd.in "${CMAKE_BINARY_DIR}/Energy+.idd" )

if( BUILD_PACKAGE )
  set(CPACK_INSTALL_CMAKE_PROJECTS
    "${CMAKE_BINARY_DIR};EnergyPlus;ALL;/"
  )

  set(EPLUS_BUILDSUPPORT_REPO 
    "RootOfEnergyPlusBuildSupportRepo" CACHE PATH "Enter the root of the EnergyPlusBuildSupport repo"
  )
  
  if( BUILD_FORTRAN )
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/ExpandObjects/;ExpandObjects;ALL;/")
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/ReadVars/;ReadVars;ALL;/")
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/Transition/;Transition;ALL;/")
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/Basement/;Basement;ALL;/")
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/HVAC-Diagram/;HVAC-Diagram;ALL;/")
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/ParametricPreprocessor/;ParametricPreprocessor;ALL;/")
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/Slab/;Slab;ALL;/")
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/ConvertESOMTR/;ConvertESOMTR;ALL;/")
  endif()

  include(cmake/Install.cmake)
endif()

