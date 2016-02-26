Project(EnergyPlus)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

if(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} VERSION_GREATER "3.0")
  CMAKE_POLICY(SET CMP0054 NEW) # CMake 3.1 added this policy
endif()

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

set( CMAKE_VERSION_MAJOR 8 )
set( CMAKE_VERSION_MINOR 5 )
set( CMAKE_VERSION_PATCH 0 )

set( PREV_RELEASE_SHA "832e4bb" )

set( ENERGYPLUS_VERSION "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}" )

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

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

set(CPACK_PACKAGE_CONTACT "Kyle Benne <Kyle.Benne@nrel.gov>")

option( BUILD_PACKAGE "Build package" OFF )
option( BUILD_TESTING "Build testing targets" OFF )
option( BUILD_FORTRAN "Build Fortran stuff" OFF )
option( BUILD_VALIDATION_REPORTS "Build validation reports" OFF )
option( BUILD_DOCS "Build markdown based documentation" OFF )

if (BUILD_DOCS)
  option( BUILD_MONOLITHIC_ENGINEERINGREFERENCE  "Build all-in-one EngineeringReference (has caveats)" ON )

  if (BUILD_PACKAGE)
    option( PACKAGE_BUILT_DOCS "Add built docs to package" OFF )
  endif()
endif()

# Turning ENABLE_GTEST_DEBUG_MODE ON will cause assertions and exceptions to halt the test case and unwind.
# Turn this option OFF for automated testing.
option( ENABLE_GTEST_DEBUG_MODE "Enable options to help debug test failures" ON )
option( ENABLE_GTEST_SHUFFLE "Enable shuffle to eliminate order dependency" ON )
option( ENABLE_INSTALL_REMOTE "Enable install_remote and install_remote_plist commands to install files from remote resources on the internet" ON )
mark_as_advanced( ENABLE_INSTALL_REMOTE )

# we are making python a required dependency, so find it here...starting off by requiring at least 2.7
find_package(PythonInterp 2.7 REQUIRED)

# then we can add python-based tests for it
if ( BUILD_TESTING )
  add_test( NAME VerifyIDFsInCMakeFolder COMMAND ${PYTHON_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/scripts/dev/VerifyIDFsInCMake.py" "${CMAKE_CURRENT_SOURCE_DIR}/testfiles" )
endif()

if( BUILD_TESTING )
  option( ENABLE_REGRESSION_TESTING "Enable Regression Tests" OFF )
  if( ENABLE_REGRESSION_TESTING )
    if ( CMAKE_CL_64 )
      set( ARCH_FLAG "-64bit" )
    else()
      set( ARCH_FLAG "" )
    endif()

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

if ( MSVC AND NOT ( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel" ) )
  if (CMAKE_C_COMPILER_VERSION VERSION_LESS "18.00.30723")
    message(FATAL_ERROR "Visual Studio 2013 Update 3 or Greater Required")
  endif()
  set(gtest_force_shared_crt ON)
endif()

if (MINGW)
  set(CMAKE_SHARED_LIBRARY_PREFIX "")
endif ()

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/Products" )

# Advanced, time consuming compiler optimization options

option(ENABLE_LTO "Enable link-time-optimization" FALSE)
option(PROFILE_GENERATE "Generate profile data" FALSE)
option(PROFILE_USE "Use profile data" FALSE)


if (PROFILE_USE AND PROFILE_GENERATE)
  message(SEND_ERROR "Cannot enable PROFILE_USE and PROFILE_GENERATE simultaneously")
endif()


include(cmake/ProjectMacros.cmake)
include(cmake/CompilerFlags.cmake)

INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/src )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/zlib )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/gtest/include/ SYSTEM )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/ObjexxFCL/src/ )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/SQLite/ SYSTEM )
INCLUDE_DIRECTORIES( "${CMAKE_SOURCE_DIR}/third_party/Expat" "${CMAKE_SOURCE_DIR}/third_party/Expat/lib" SYSTEM)
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/CLI/ )
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/jsoncpp)
INCLUDE_DIRECTORIES( ${CMAKE_BINARY_DIR}/src/EnergyPlus)  # so that any source can #include <ConfiguredFunctions.hh>

if( BUILD_TESTING )
  option( TEST_ANNUAL_SIMULATION "Use annual simulations for tests instead of only design days" OFF )
  enable_testing()
  include(CTest)
endif()

# E+ required libraries
ADD_SUBDIRECTORY(idd)
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
ADD_SUBDIRECTORY(third_party/jsoncpp)

# of course E+ itself
ADD_SUBDIRECTORY(src/EnergyPlus)

if( BUILD_TESTING )
  ADD_SUBDIRECTORY(third_party/gtest)
  ADD_SUBDIRECTORY(testfiles)
  ADD_SUBDIRECTORY(tst/EnergyPlus/unit)
  ADD_SUBDIRECTORY(tst/jsoncpp/unit)

  option( BUILD_PERFORMANCE_TESTS "Build performance testing targets" OFF )

  if (BUILD_PERFORMANCE_TESTS)

    if (UNIX AND NOT APPLE)
      option( VALGRIND_ANALYZE_PERFORMANCE_TESTS "Analyze performance tests via valgrind / callgrind" OFF )

      if (VALGRIND_ANALYZE_PERFORMANCE_TESTS)
        find_program(VALGRIND NAMES valgrind PATH /usr/bin /usr/local/bin)
      endif()
    endif()
    ADD_SUBDIRECTORY(performance_tests)
  endif()
endif()

configure_file( idd/Energy+.idd.in "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.idd" )
configure_file( idd/BasementGHT.idd "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/BasementGHT.idd" )
configure_file( idd/SlabGHT.idd "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/SlabGHT.idd" )

if( BUILD_FORTRAN )
  include(CMakeAddFortranSubdirectory)
  cmake_add_fortran_subdirectory(src/ExpandObjects PROJECT ExpandObjects NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/ReadVars PROJECT ReadVars NO_EXTERNAL_INSTALL )
  set(TRANSITION_EXTRA_FLAGS "-DCMAKE_VERSION_MAJOR:STRING=${CMAKE_VERSION_MAJOR}" "-DCMAKE_VERSION_MINOR:STRING=${CMAKE_VERSION_MINOR}" "-DCMAKE_VERSION_PATCH:STRING=${CMAKE_VERSION_PATCH}")
  cmake_add_fortran_subdirectory(src/Transition PROJECT Transition CMAKE_COMMAND_LINE ${TRANSITION_EXTRA_FLAGS} NO_EXTERNAL_INSTALL )
  file( COPY "idd/V8-4-0-Energy+.idd" DESTINATION "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}" )
  configure_file( idd/Energy+.idd.in "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/V8-5-0-Energy+.idd" )
  file( COPY "src/Transition/SupportFiles/Report Variables 8-4-0 to 8-5-0.csv" DESTINATION "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}" )
  cmake_add_fortran_subdirectory(src/Basement PROJECT Basement NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/HVAC-Diagram PROJECT HVAC-Diagram NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/ParametricPreprocessor PROJECT ParametricPreprocessor NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/Slab PROJECT Slab NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/ConvertESOMTR PROJECT ConvertESOMTR NO_EXTERNAL_INSTALL )
  cmake_add_fortran_subdirectory(src/CalcSoilSurfTemp PROJECT CalcSoilSurfTemp NO_EXTERNAL_INSTALL )
endif()

if( BUILD_PACKAGE )
  set(CPACK_INSTALL_CMAKE_PROJECTS
    "${CMAKE_BINARY_DIR};EnergyPlus;ALL;/"
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
    list(APPEND CPACK_INSTALL_CMAKE_PROJECTS "${CMAKE_BINARY_DIR}/src/CalcSoilSurfTemp/;CalcSoilSurfTemp;ALL;/")
  endif()

  include(cmake/Install.cmake)
endif()


set(OPTIMIZABLE_TARGETS
  sqlite objexx bcvtb epexpat epfmiimport miniziplib DElight energyplus energyplusapi energypluslib
    )
if(NOT APPLE AND NOT MSVC)
  list(APPEND OPTIMIZABLE_TARGETS parser)
endif()

if(NOT MSVC)
  list(APPEND OPTIMIZABLE_TARGETS objexx_tests energyplus_tests)
endif()

if(MSVC AND (PROFILE_GENERATE OR PROFILE_USE OR ENABLE_LTO) )
  get_filename_component(LINKER_DIR "${CMAKE_LINKER}" PATH)
  get_filename_component(LINKER_DIR_NAME "${LINKER_DIR}" NAME)

  if (MSVC12)
    set(PGORTVER "120")
  elseif(MSVC14)
    set(PGORTVER "140")
  endif()

  message("Examining ${LINKER_DIR_NAME} for PGO RT DLL")

  if ("${LINKER_DIR_NAME}" MATCHES ".*amd64.*")
    get_filename_component(LINKER_PARENT_DIR "${LINKER_DIR}" PATH)
    set(PGORTDIR "${LINKER_PARENT_DIR}/amd64")
    message("Retrieving amd64 PGO dll from: ${PGORTDIR}")
  else()
    set(PGORTDIR "${LINKER_DIR}")
  endif()

  file(COPY "${PGORTDIR}/pgort${PGORTVER}.dll" DESTINATION "${CMAKE_BINARY_DIR}/Products/Release" )

  foreach(target ${OPTIMIZABLE_TARGETS})
    get_target_property(targ_link_flags ${target} LINK_FLAGS)
    get_target_property(targ_static_link_flags ${target} STATIC_LIBRARY_FLAGS)
    get_target_property(targ_compile_flags ${target} COMPILE_FLAGS)

    # message("${target}: ${targ_link_flags}; ${targ_static_link_flags}; ${targ_compile_flags}")
    if(targ_link_flags STREQUAL "targ_link_flags-NOTFOUND")
      set(targ_link_flags "")
    endif()

    if(targ_static_link_flags STREQUAL "targ_static_link_flags-NOTFOUND")
      set(targ_static_link_flags "")
    endif()

    if(targ_compile_flags STREQUAL "targ_compile_flags-NOTFOUND")
      set(targ_compile_flags "")
    endif()

    if (ENABLE_LTO OR PROFILE_GENERATE OR PROFILE_USE)
      set(targ_compile_flags "${targ_compile_flags} /GL")
    endif()

    if(ENABLE_LTO AND NOT PROFILE_GENERATE AND NOT PROFILE_USE)
      set(targ_link_flags "${targ_link_flags} /LTCG")
      set(targ_static_link_flags "${targ_static_link_flags} /LTCG")
    endif()

    if (PROFILE_GENERATE)
      set(targ_link_flags "${targ_link_flags} /LTCG:PGI")
      set(targ_static_link_flags "${targ_static_link_flags} /LTCG")
    endif()

    if (PROFILE_USE)
      set(targ_link_flags "${targ_link_flags} /LTCG:PGO")
      set(targ_static_link_flags "${targ_static_link_flags} /LTCG")
    endif()

    set_target_properties(${target} PROPERTIES LINK_FLAGS "${targ_link_flags}" STATIC_LIBRARY_FLAGS "${targ_static_link_flags}"
       COMPILE_FLAGS "${targ_compile_flags}")

  endforeach()

elseif (CMAKE_COMPILER_IS_GNUCXX)
  if(ENABLE_LTO)
    set_target_properties(${OPTIMIZABLE_TARGETS}
      PROPERTIES LINK_FLAGS "-flto=7 -fno-use-linker-plugin" COMPILE_FLAGS "-flto=7 -fno-use-linker-plugin")
    set_target_properties(energyplus
      PROPERTIES LINK_FLAGS "-flto=7 -fuse-linker-plugin -fwhole-program" COMPILE_FLAGS "-flto=7 -fuse-linker-plugin -fwhole-program")
  endif()

  if (PROFILE_GENERATE)
    set_target_properties(${OPTIMIZABLE_TARGETS} energyplus_tests TestEnergyPlusCallbacks
      PROPERTIES LINK_FLAGS "-fprofile-generate"  COMPILE_FLAGS "-fprofile-generate")
  endif()

  if (PROFILE_USE)
    set_target_properties(${OPTIMIZABLE_TARGETS}
      PROPERTIES LINK_FLAGS "-fprofile-use"  COMPILE_FLAGS "-fprofile-use")
  endif()

  if (PROFILE_USE AND ENABLE_LTO)
    set_target_properties(${OPTIMIZABLE_TARGETS}
      PROPERTIES LINK_FLAGS "-flto=7 -fno-use-linker-plugin -fprofile-use" COMPILE_FLAGS "-flto=7 -fno-use-linker-plugin -fprofile-use")
    set_target_properties(energyplus
      PROPERTIES LINK_FLAGS "-flto=7 -fuse-linker-plugin -fwhole-program -fprofile-use" COMPILE_FLAGS "-flto=7 -fuse-linker-plugin -fwhole-program -fprofile-use")
  endif()
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" ) # Clang
  if(ENABLE_LTO)
    set_target_properties(energyplus
      PROPERTIES LINK_FLAGS "-flto" COMPILE_FLAGS "-flto")
  endif()

  if (PROFILE_GENERATE)
    set_target_properties(${OPTIMIZABLE_TARGETS}
      PROPERTIES LINK_FLAGS "-fprofile-instr-generate" COMPILE_FLAGS "-fprofile-instr-generate")
  endif()

  if (PROFILE_USE)
    set_target_properties(${OPTIMIZABLE_TARGETS}
      PROPERTIES LINK_FLAGS "-fprofile-instr-use=${CMAKE_BINARY_DIR}/profdata" COMPILE_FLAGS "-fprofile-instr-use=${CMAKE_BINARY_DIR}/profdata")
  endif()

  if (PROFILE_USE AND ENABLE_LTO)
    set_target_properties(${OPTIMIZABLE_TARGETS}
      PROPERTIES LINK_FLAGS "-flto -fprofile-instr-use=${CMAKE_BINARY_DIR}/profdata" COMPILE_FLAGS "-flto -fprofile-instr-use=${CMAKE_BINARY_DIR}/profdata")
  endif()


endif()



if (BUILD_VALIDATION_REPORTS)
  include(ExternalProject)
  ExternalProject_Add(ValidationReports
    DEPENDS energyplus ReadVarsESO Basement Slab
    GIT_REPOSITORY git://github.com/NREL/EnergyPlusValidationReports
    GIT_TAG master
    CMAKE_CACHE_ARGS
      -DENERGYPLUS_PATH:PATH=${CMAKE_BINARY_DIR}
      -DENERGYPLUS_VERSION:STRING=${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}
    INSTALL_COMMAND ""
    )
endif()


if (BUILD_DOCS)
  add_subdirectory(doc/src)
endif()
