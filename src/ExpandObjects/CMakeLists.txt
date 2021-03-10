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

set(STATIC_EXE FALSE)

if(APPLE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel")
  else()
    if(NOT "Ninja" STREQUAL ${CMAKE_GENERATOR})
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
    endif()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-200")
  endif()
elseif(UNIX)
  set(CMAKE_SKIP_RPATH TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel")
  else()
    if(NOT "Ninja" STREQUAL ${CMAKE_GENERATOR})
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
    endif()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-200")
    set(STATIC_EXE TRUE)
  endif()
else() # Windows
  set(STATIC_EXE TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /libs:static")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /fpp")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
    if(NOT "Ninja" STREQUAL ${CMAKE_GENERATOR})
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
    endif()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-200")
  endif()
endif()

set(SKIP_BUILD_RPATH TRUE)
if(APPLE OR UNIX)
  add_executable(ExpandObjects ${SRC})
else() # windows
  add_executable(ExpandObjects ${SRC} "${CMAKE_CURRENT_BINARY_DIR}/ExpandObjects.rc")
endif()

set_target_properties(ExpandObjects PROPERTIES FOLDER Auxiliary)

install(TARGETS ExpandObjects DESTINATION "./")
if(NOT ${STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs(ExpandObjects "./")
endif()
