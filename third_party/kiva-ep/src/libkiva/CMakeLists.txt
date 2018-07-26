project(libkiva)

if(${KIVA_3D})
  add_definitions(-D KIVA_3D )
endif()

include(TargetArch)
target_architecture(TARGET_ARCH)

if(${TARGET_ARCH} MATCHES "x86_64" OR ${TARGET_ARCH} MATCHES "ia64" OR ${TARGET_ARCH} MATCHES "ppc64")
  set(BIT "64bit")
else()
  set(BIT "32bit")
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(TARGET_OS mac)
else()
  set(TARGET_OS ${CMAKE_SYSTEM_NAME})
endif()

set(KIVA_BUILD_CONFIG ${TARGET_OS}-${CMAKE_CXX_COMPILER_ID}-${BIT})
string(TOLOWER ${KIVA_BUILD_CONFIG} KIVA_BUILD_CONFIG)
set(KIVA_BUILD_CONFIG ${KIVA_BUILD_CONFIG} PARENT_SCOPE)

if(KIVA_EXE_BUILD)
  file(WRITE ${CMAKE_BINARY_DIR}/arch.txt ${KIVA_BUILD_CONFIG})

  string(TOLOWER "${TARGET_OS}-${BIT}" KIVA_PACKAGE_CONFIG)
  set(KIVA_PACKAGE_CONFIG ${KIVA_PACKAGE_CONFIG} PARENT_SCOPE)

  execute_process(COMMAND git describe --tags --long
                  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
                  RESULT_VARIABLE git_exit_status
                  OUTPUT_VARIABLE git_version
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
else()
  set(git_exit_status "1")
endif()

if(${git_exit_status} MATCHES "0")

  # TODO: Should be "v([0-9]+)\\.([0-9]+)\\.([0-9]+)-?([a-z]*[0-9]*)-[0-9]+-([a-z,0-9])+"
  #       for v<major>.<minor>.<patch>-<build>-n-<sha>
  string( REGEX REPLACE "v([0-9]+)\\.[0-9]+\\.[0-9]+-[0-9]+-[a-z,0-9]+" "\\1" libkiva_VERSION_MAJOR "${git_version}" )
  string( REGEX REPLACE "v[0-9]+\\.([0-9]+)\\.[0-9]+-[0-9]+-[a-z,0-9]+" "\\1" libkiva_VERSION_MINOR "${git_version}" )
  string( REGEX REPLACE "v[0-9]+\\.[0-9]+\\.([0-9]+)-[0-9]+-[a-z,0-9]+" "\\1" libkiva_VERSION_PATCH "${git_version}" )
  string( REGEX REPLACE "v[0-9]+\\.[0-9]+\\.[0-9]+-[0-9]+-([a-z,0-9]+)" "\\1" libkiva_VERSION_BUILD "${git_version}" )

  set(CPACK_PACKAGE_VERSION_MAJOR "${libkiva_VERSION_MAJOR}" PARENT_SCOPE)
  set(CPACK_PACKAGE_VERSION_MINOR "${libkiva_VERSION_MINOR}" PARENT_SCOPE)
  set(CPACK_PACKAGE_VERSION_PATCH "${libkiva_VERSION_PATCH}" PARENT_SCOPE)
  set(CPACK_PACKAGE_VERSION_BUILD "${libkiva_VERSION_BUILD}" PARENT_SCOPE)
else()
  set( libkiva_VERSION_MAJOR 0 )
  set( libkiva_VERSION_MINOR 4 )
  set( libkiva_VERSION_PATCH 0 )
  set( libkiva_VERSION_BUILD "unknown-commit" )
  if(KIVA_EXE_BUILD)
    message("Cannot determine version number.")
  endif()
endif()

set(kiva_version "v${libkiva_VERSION_MAJOR}.${libkiva_VERSION_MINOR}.${libkiva_VERSION_PATCH}-${libkiva_VERSION_BUILD}")

if ( "${CMAKE_BUILD_TYPE}" STREQUAL "" OR "${CMAKE_BUILD_TYPE}" STREQUAL "Release" )
  set (kiva_version "${kiva_version}")
else()
  set (kiva_version "${kiva_version}-${CMAKE_BUILD_TYPE}")
endif()

set(VERSION_NUMBER "${kiva_version} (${KIVA_BUILD_CONFIG})")

if(KIVA_EXE_BUILD)
  message("-- Building Kiva version: ${VERSION_NUMBER}")
endif()

string(TIMESTAMP YEAR %Y)

configure_file (
    "${PROJECT_SOURCE_DIR}/Version.hpp.in"
    "${PROJECT_BINARY_DIR}/Version.hpp"
)

set(kiva_src Algorithms.cpp
             Algorithms.hpp
             BoundaryConditions.hpp
             Cell.cpp
             Cell.hpp
             Domain.cpp
             Domain.hpp
             Errors.cpp
             Errors.hpp
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

include_directories("${kiva_BINARY_DIR}/src/libkiva")

option( KIVA_STATIC_LIB "Make libkiva a static library" ON )

if (KIVA_STATIC_LIB)
  add_library(libkiva STATIC ${kiva_src})
  if(UNIX)
    set_target_properties(libkiva PROPERTIES COMPILE_FLAGS "-DLIBKIVA_STATIC_DEFINE -fPIC")
  else()
    set_target_properties(libkiva PROPERTIES COMPILE_FLAGS "-DLIBKIVA_STATIC_DEFINE")
  endif()
else()
  set(CMAKE_MACOSX_RPATH 1)
  add_library(libkiva SHARED ${kiva_src})
  if(KIVA_EXE_BUILD)
    install(TARGETS libkiva DESTINATION bin/)
  endif()
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
