cmake_minimum_required (VERSION 2.8)
project (kiva)

option(KIVA_COVERAGE "Add coverage reports" OFF)
option(KIVA_3D "Enable 3D Calculations" ON)
option(KIVA_EXE_BUILD "Create Kiva program executable" ON)
option(ENABLE_OPENMP "Use OpenMP" OFF)
option( KIVA_GROUND_PLOT "Build ground plotting library" ON )
mark_as_advanced(FORCE BUILD_GROUND_PLOT)
option( KIVA_TESTING "Build tests" ON )


set(CMAKE_MODULE_PATH "${kiva_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

if (KIVA_TESTING)
  enable_testing()
  if (KIVA_COVERAGE)
    set(ENABLE_COVERAGE ON CACHE BOOL "" FORCE)
    find_package(codecov)
  endif()
endif()

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${kiva_BINARY_DIR}" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${kiva_BINARY_DIR}" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${kiva_BINARY_DIR}" )

if (KIVA_EXE_BUILD)
  set(CompilerFlags
          CMAKE_CXX_FLAGS
          CMAKE_CXX_FLAGS_DEBUG
          CMAKE_CXX_FLAGS_RELEASE
          CMAKE_C_FLAGS
          CMAKE_C_FLAGS_DEBUG
          CMAKE_C_FLAGS_RELEASE
          )
  foreach(CompilerFlag ${CompilerFlags})
    string(REPLACE "/MD" "/MT" ${CompilerFlag} "${${CompilerFlag}}")
  endforeach()
endif()

if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  if (UNIX)
    add_definitions("-fPIC")
  endif()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -Wextra")
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-deprecated-register -Wno-deprecated-declarations")
endif()

if( MSVC AND NOT ( CMAKE_CXX_COMPILER_ID STREQUAL "Intel" ) )
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4251")
endif()

include_directories( ${kiva_SOURCE_DIR}/src/libkiva/)
include_directories( SYSTEM ${kiva_SOURCE_DIR}/vendor/boost-1.61.0/)
include_directories( SYSTEM ${kiva_SOURCE_DIR}/vendor/eigen-da9b4e14c255/)

if (KIVA_EXE_BUILD OR KIVA_GROUND_PLOT)
  add_definitions("-DBOOST_ALL_NO_LIB")
  include_directories( ${kiva_SOURCE_DIR}/src/libgroundplot/)
  include_directories( SYSTEM ${kiva_SOURCE_DIR}/vendor/mathgl-2.3.5.1/include/)
  include_directories( SYSTEM ${kiva_BINARY_DIR}/vendor/mathgl-2.3.5.1/include/)
  if (KIVA_EXE_BUILD)
    include_directories( SYSTEM ${kiva_SOURCE_DIR}/vendor/yaml-cpp-0.5.3/include/)
  endif()
endif()

if (KIVA_TESTING)
  include_directories( SYSTEM ${kiva_SOURCE_DIR}/vendor/gtest/include/)
endif()

add_subdirectory(vendor)
add_subdirectory(src/libkiva)

if (KIVA_EXE_BUILD OR KIVA_GROUND_PLOT)
  add_subdirectory(src/libgroundplot)
  if (KIVA_EXE_BUILD)
    add_subdirectory(src/kiva)
  endif()
endif()

# Testing
if (KIVA_TESTING)
  add_subdirectory(test)
  if (KIVA_COVERAGE)
    coverage_evaluate()
  endif()
endif()

if (KIVA_EXE_BUILD)
  include(Install)
endif()