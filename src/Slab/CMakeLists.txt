project(Slab Fortran)

cmake_minimum_required(VERSION 3.5.1)

cmake_policy(SET CMP0012 NEW) # if() recognizes boolean constants

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")

set(SRC
    3DSlabHT.f90
    DataGlobals.f90
    DataStringGlobals.f90
    EPWRead.f90
    General.f90
    MainProgram.f90
    UtilityRoutines.f90
    DataPrecisionGlobals.f90
    EPWPrecisionGlobals.f90
    EPWWeather.f90
    InputProcessor.f90
    SimData.f90)

set(STATIC_EXE FALSE)

add_library(SlabOptions INTERFACE)

if(APPLE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(SlabOptions INTERFACE -static-intel)
  endif()
elseif(UNIX)
  set(CMAKE_SKIP_RPATH TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(SlabOptions INTERFACE -static-intel)
  else()
    target_compile_options(SlabOptions INTERFACE -static)
    set(STATIC_EXE TRUE)
  endif()
else()
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    target_compile_options(SlabOptions INTERFACE /libs:static)
  else()
    target_compile_options(SlabOptions INTERFACE -static)
    set(STATIC_EXE TRUE)
  endif()
endif()

set(SKIP_BUILD_RPATH TRUE)
add_executable(Slab ${SRC})
target_link_libraries(Slab PRIVATE SlabOptions)
set_target_properties(Slab PROPERTIES FOLDER Auxiliary)

set_target_properties(Slab PROPERTIES COMPILE_FLAGS "-ffree-line-length-200")

install(TARGETS Slab DESTINATION PreProcess/GrndTempCalc)
if(NOT ${STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs(Slab PreProcess/GrndTempCalc)
endif()
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/../../idd/SlabGHT.idd" DESTINATION "PreProcess/GrndTempCalc/")
