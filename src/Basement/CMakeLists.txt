project(Basement Fortran)

cmake_minimum_required(VERSION 3.5.1)

cmake_policy(SET CMP0012 NEW) # if() recognizes boolean constants

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")

set(SRC
    3DBasementHT.f90
    DataGlobals.f90
    DataStringGlobals.f90
    EPWRead.f90
    UtilityRoutines.f90
    DataPrecisionGlobals.f90
    EPWPrecisionGlobals.f90
    InputProcessor.f90
    BasementSimData.f90)

set(STATIC_EXE FALSE)

add_library(BasementOptions INTERFACE)
if(APPLE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(BasementOptions INTERFACE -static-intel)
  else()
    target_compile_options(BasementOptions INTERFACE -ffloat-store)
  endif()
elseif(UNIX)
  set(CMAKE_SKIP_RPATH TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(BasementOptions INTERFACE -static-intel)
  else()
    target_compile_options(BasementOptions INTERFACE -ffloat-store)
    target_compile_options(BasementOptions INTERFACE -static)
    set(STATIC_EXE TRUE)
  endif()
else() # Windows
  set(STATIC_EXE TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    target_compile_options(
      BasementOptions
      INTERFACE /nologo
                /debug:minimal
                /O2
                /Oy-
                /assume:buffered_io
                /noaltparam
                /fpscomp:nolibs
                /fpe:0
                /fp:fast=2
                /traceback
                /check:uninit
                /libs:static
                /threads
                /c
                /assume:protect-parens
                /Qprec-div
                /Qprec-sqrt
                /Qimf-precision:high
                /Qimf-arch-consistency:true
                /Qvec-)
  else()
    target_compile_options(BasementOptions INTERFACE -static)
    target_compile_options(BasementOptions INTERFACE -ffloat-store)
  endif()
endif()

set(SKIP_BUILD_RPATH TRUE)
add_executable(Basement ${SRC})

set_target_properties(
  Basement PROPERTIES COMPILE_FLAGS
                      "-O1 -ffree-line-length-200")
set_target_properties(Basement PROPERTIES FOLDER Auxiliary)
target_link_libraries(Basement PRIVATE BasementOptions)

install(TARGETS Basement DESTINATION PreProcess/GrndTempCalc)
if(NOT ${STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs(Basement PreProcess/GrndTempCalc)
endif()
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/../../idd/BasementGHT.idd"
        DESTINATION "PreProcess/GrndTempCalc/")
