project(CalcSoilSurfTemp Fortran)

cmake_minimum_required(VERSION 3.5.1)

cmake_policy(SET CMP0012 NEW) # if() recognizes boolean constants

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")

set(SRC EPWRead.f90 SoilSurfTemp.f90)

set(CMAKE_SKIP_RPATH ${FORTRAN_SKIP_RPATH})
set(SKIP_BUILD_RPATH TRUE)

add_executable(CalcSoilSurfTemp ${SRC})
target_link_libraries(CalcSoilSurfTemp PRIVATE fortran_project_options)
set_target_properties(CalcSoilSurfTemp PROPERTIES FOLDER Auxiliary)

install(TARGETS CalcSoilSurfTemp DESTINATION PreProcess/CalcSoilSurfTemp)
if(NOT ${FORTRAN_STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs(CalcSoilSurfTemp PreProcess/CalcSoilSurfTemp)
endif()
