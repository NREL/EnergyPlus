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

add_library(BasementOptions INTERFACE)
if(NOT CMAKE_Fortran_COMPILER MATCHES "ifort")
  target_compile_options(BasementOptions INTERFACE -ffloat-store)
endif()

if(NOT APPLE AND NOT UNIX) # Windows
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
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
  endif()
endif()

set(CMAKE_SKIP_RPATH ${FORTRAN_SKIP_RPATH})
set(SKIP_BUILD_RPATH TRUE)

add_executable(Basement ${SRC})
target_compile_options(Basement PRIVATE -O1)
set_target_properties(Basement PROPERTIES FOLDER Auxiliary)

if(NOT UNIX)  # Need to reinclude it on Windows
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/Fortran.cmake")
endif()
get_target_property(F_INTERFACE_COMPILE_FLAGS fortran_project_options INTERFACE_COMPILE_OPTIONS)
message(DEBUG "Basement: F_INTERFACE_COMPILE_FLAGS=${F_INTERFACE_COMPILE_FLAGS}")
target_link_libraries(Basement PRIVATE BasementOptions fortran_project_options)

install(TARGETS Basement DESTINATION PreProcess/GrndTempCalc)
if(NOT ${FORTRAN_STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs(Basement PreProcess/GrndTempCalc)
endif()
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/../../idd/BasementGHT.idd" DESTINATION "PreProcess/GrndTempCalc/")
