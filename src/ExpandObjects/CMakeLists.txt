project(ExpandObjects Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

SET(SRC epfilter.f90) 

# the Fortran apps require a DEF for proper path handling on Windows
if( WIN32 ) 
  add_definitions(-D_WIN32) 
  if( CMAKE_Fortran_COMPILER MATCHES "ifort" )
    add_definitions( "/fpp" )
    set(CMAKE_Fortran_FLAGS /lib:static)
  else() # assume mingw gfortran if not intel fortran
    add_definitions( "-x f95-cpp-input" )
    set(CMAKE_Fortran_FLAGS "-static")
  endif()  
else()
  if( CMAKE_Fortran_COMPILER MATCHES "ifort" )
    add_definitions( "-fpp" )
  elseif( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" ) # could use this above in WIN32 block to be more specific
    add_definitions( "-cpp" )
  endif()
endif()

ADD_EXECUTABLE( ExpandObjects ${SRC} )

SET_TARGET_PROPERTIES(ExpandObjects PROPERTIES
  COMPILE_FLAGS "-ffree-line-length-200  ${SHARED_FLAGS}"
)
  
include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
install_and_fixup_exe_target( ExpandObjects "./" )

