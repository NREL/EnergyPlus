project(ExpandObjects Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

SET(SRC epfilter.f90) 

ADD_EXECUTABLE( ExpandObjects ${SRC} )

SET_TARGET_PROPERTIES(ExpandObjects PROPERTIES
  COMPILE_FLAGS "-std=legacy -ffree-line-length-200 ${SHARED_FLAGS}"
)

install(TARGETS ExpandObjects DESTINATION ./)

