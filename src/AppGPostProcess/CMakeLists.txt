project(AppGPostProcess Fortran)

cmake_minimum_required(VERSION 3.5.1)

cmake_policy(SET CMP0012 NEW) # if() recognizes boolean constants

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")

set(SRC AppGPostProcess.f90)

set(CMAKE_SKIP_RPATH ${FORTRAN_SKIP_RPATH})
set(SKIP_BUILD_RPATH TRUE)

add_executable(AppGPostProcess ${SRC})
set_target_properties(AppGPostProcess PROPERTIES FOLDER Auxiliary)

include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/Fortran.cmake")
target_link_libraries(AppGPostProcess PRIVATE fortran_project_options)

install(TARGETS AppGPostProcess DESTINATION PostProcess)
if(NOT ${FORTRAN_STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs(AppGPostProcess PostProcess)
endif()
