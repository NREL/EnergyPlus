project(ParametricPreprocessor Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

set(SRC ParametricPreprocessor.f90)

set(STATIC_EXE FALSE)

if(APPLE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel" )
  endif()
elseif(UNIX)
  set(CMAKE_SKIP_RPATH TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-intel" )
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static" )
    set(STATIC_EXE TRUE)
  endif()
else()
  set(STATIC_EXE TRUE)
  if(CMAKE_Fortran_COMPILER MATCHES "ifort")
    # Set release flags to be empty
    set(CMAKE_Fortran_FLAGS_RELEASE "")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /libs:static")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
  endif()
endif()

set(SKIP_BUILD_RPATH true)
add_executable( ParametricPreprocessor ${SRC} )

set_target_properties(ParametricPreprocessor PROPERTIES
  COMPILE_FLAGS "-ffree-line-length-200 ${SHARED_FLAGS}"
)

install( TARGETS ParametricPreprocessor DESTINATION PreProcess )
if(NOT ${STATIC_EXE})
  include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
  install_target_prereqs( ParametricPreprocessor PreProcess )
endif()

