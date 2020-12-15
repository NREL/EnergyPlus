project(ReadVars Fortran)

cmake_minimum_required(VERSION 3.5.1)

cmake_policy(SET CMP0012 NEW) # if() recognizes boolean constants

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")

set(SRC ReadVarsESO.f90)

set(STATIC_EXE FALSE)

add_library(ReadVarsOptions INTERFACE)


if(APPLE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(ReadVarsOptions INTERFACE -static-intel)
  endif()
elseif(UNIX)
  set(CMAKE_SKIP_RPATH TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(ReadVarsOptions INTERFACE -static-intel)
  else()
    target_compile_options(ReadVarsOptions INTERFACE -static)
    set(STATIC_EXE TRUE)
  endif()
else()
  set(STATIC_EXE TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    target_compile_options(ReadVarsOptions INTERFACE /libs:static)
  else()
    target_compile_options(ReadVarsOptions INTERFACE -static)
  endif()
endif()

set(SKIP_BUILD_RPATH true)
add_executable(ReadVarsESO ${SRC})
set_target_properties(ReadVarsESO PROPERTIES FOLDER Auxiliary)
target_link_libraries(ReadVarsESO PRIVATE ReadVarsOptions)

install(TARGETS ReadVarsESO DESTINATION PostProcess)
if(NOT ${STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs(ReadVarsESO PostProcess)
endif()
