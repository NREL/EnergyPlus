project(libkiva)

if(${KIVA_3D})
  add_definitions(-D KIVA_3D )
endif()

set(PROCESSOR_ARCHITECTURE "x86_64" CACHE STRING "Set architecture used for version string.")

# Set a default build type if none was specified
if(NOT PROCESSOR_ARCHITECTURE)
  message(STATUS "Setting processor architecture to 'x86_64' as none was specified.")
  set(PROCESSOR_ARCHITECTURE x86_64 CACHE STRING "Choose the processor architecture." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE PROCESSOR_ARCHITECTURE PROPERTY STRINGS "x86_64" "x86")
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(build_architecture macosx-${CMAKE_CXX_COMPILER_ID}-${PROCESSOR_ARCHITECTURE})
else()
  set(build_architecture ${CMAKE_SYSTEM_NAME}-${CMAKE_CXX_COMPILER_ID}-${PROCESSOR_ARCHITECTURE})
endif()

string(TOLOWER ${build_architecture} build_architecture)

if(KIVA_EXE_BUILD)
  execute_process(COMMAND git describe --tags --long
                  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
                  RESULT_VARIABLE git_exit_status
                  OUTPUT_VARIABLE kiva_version
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
else()
  set( CMAKE_VERSION_MAJOR 0 )
  set( CMAKE_VERSION_MINOR 4 )
  set( CMAKE_VERSION_PATCH 0 )
  set(kiva_version "v${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}")
  set(git_exit_status "0")
endif()

if(${CMAKE_BUILD_TYPE} MATCHES "Debug")
  set (kiva_version "${kiva_version}-Debug")
endif()

if(${git_exit_status} MATCHES "0")
  set(VERSION_NUMBER "${kiva_version} (${build_architecture})")
  message("-- Building Kiva version: ${VERSION_NUMBER}")
else()
  set(VERSION_NUMBER "v?.?.?")
  message("Cannot determine version number.")
endif()

string(TIMESTAMP YEAR %Y)

configure_file (
    "${PROJECT_SOURCE_DIR}/Version.hpp.in"
    "${PROJECT_BINARY_DIR}/Version.hpp"
)

set(kiva_src Algorithms.cpp
             Algorithms.hpp
             BoundaryConditions.hpp
             Domain.cpp
             Domain.hpp
             Foundation.cpp
             Foundation.hpp
             Functions.cpp
             Functions.hpp
             Geometry.cpp
             Geometry.hpp
             Ground.cpp
             Ground.hpp
             GroundOutput.hpp
             Mesher.cpp
             Mesher.hpp
             Version.hpp )

include_directories(${CMAKE_CURRENT_BINARY_DIR})

option( KIVA_STATIC_LIB "Make libkiva a static library" ON )

if (KIVA_STATIC_LIB)
  add_library(libkiva STATIC ${kiva_src})
  if(UNIX)
    set_target_properties(libkiva PROPERTIES COMPILE_FLAGS "-DLIBKIVA_STATIC_DEFINE -fPIC")
  else()
    set_target_properties(libkiva PROPERTIES COMPILE_FLAGS "-DLIBKIVA_STATIC_DEFINE")
  endif()
else()
  add_library(libkiva SHARED ${kiva_src})
endif()

include(GenerateExportHeader)
generate_export_header(libkiva)

set_target_properties(libkiva PROPERTIES OUTPUT_NAME kiva)
set_target_properties(libkiva PROPERTIES PDB_NAME libkiva)


set(kiva_link_flags "")

if(${ENABLE_OPENMP})
  set(kiva_link_flags "${kiva_link_flags} -fopenmp")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")
endif()

set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${kiva_link_flags}")

if (KIVA_COVERAGE)
  add_coverage(libkiva)
endif()
