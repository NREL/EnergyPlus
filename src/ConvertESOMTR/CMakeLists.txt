project(ConvertESOMTR Fortran)

cmake_minimum_required(VERSION 3.5.1)

cmake_policy(SET CMP0012 NEW) # if() recognizes boolean constants

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")

set(SRC mainModule.f90)

set(STATIC_EXE FALSE)

set(SKIP_BUILD_RPATH TRUE)
add_executable(convertESOMTR ${SRC})
set_target_properties(convertESOMTR PROPERTIES FOLDER Auxiliary)
add_library(convertESOMTROptions INTERFACE)

if(APPLE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(convertESOMTROptions INTERFACE -static-intel)
  endif()
elseif(UNIX)
  set(CMAKE_SKIP_RPATH TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    target_compile_options(convertESOMTROptions INTERFACE -static-intel)
  else()
    target_compile_options(convertESOMTROptions INTERFACE -static)
    set(STATIC_EXE TRUE)
  endif()
else()
  set(STATIC_EXE TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    target_compile_options(convertESOMTROptions INTERFACE /libs:static)
  else()
    target_compile_options(convertESOMTROptions INTERFACE -static)
  endif()
endif()
target_link_libraries(convertESOMTR PRIVATE convertESOMTROptions)


install(TARGETS convertESOMTR DESTINATION "PostProcess/convertESOMTRpgm/")
if(NOT ${STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs(convertESOMTR "PostProcess/convertESOMTRpgm/")
endif()
install(FILES "convert.txt" DESTINATION "PostProcess/convertESOMTRpgm/")
