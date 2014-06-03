project(ExpandObjects Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

SET(SRC epfilter.f90) 

ADD_EXECUTABLE( ExpandObjects ${SRC} )

SET_TARGET_PROPERTIES(ExpandObjects PROPERTIES
  COMPILE_FLAGS "-std=legacy -ffree-line-length-200 ${SHARED_FLAGS}"
)

# the Fortran apps require a DEF for proper path handling on Windows
if( WIN32 ) 
  add_definition(-D_WIN32) 
  if( CMAKE_Fortran_COMPILER MATCHES "ifort" )
    add_definition( "/fpp" )
  else() # assume mingw gfortran if not intel fortran
    add_definition( "-x f95-cpp-input" )
  endif()  
endif()
  
install(TARGETS ExpandObjects DESTINATION ./)

