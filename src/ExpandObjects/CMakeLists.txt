project(ExpandObjects Fortran)

cmake_minimum_required(VERSION 3.5.1)

cmake_policy(SET CMP0012 NEW) # if() recognizes boolean constants

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products")

if(WIN32)
  configure_file(ExpandObjects.rc.in "${CMAKE_CURRENT_BINARY_DIR}/ExpandObjects.rc")
endif()

set(SRC epfilter.f90)

set(SKIP_BUILD_RPATH TRUE)
set(CMAKE_SKIP_RPATH ${FORTRAN_SKIP_RPATH})

if(APPLE OR UNIX)
  add_executable(ExpandObjects ${SRC})
else() # windows
  add_executable(ExpandObjects ${SRC} "${CMAKE_CURRENT_BINARY_DIR}/ExpandObjects.rc")
endif()

target_link_libraries(ExpandObjects PRIVATE fortran_project_options)
set_target_properties(ExpandObjects PROPERTIES FOLDER Auxiliary)

install(TARGETS ExpandObjects DESTINATION "./")
if(NOT ${FORTRAN_STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs(ExpandObjects "./")
endif()
