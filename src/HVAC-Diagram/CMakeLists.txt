project(HVAC-Diagram Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

SET(SRC HVAC-Diagram-Main.f90) 

if( WIN32 )
  set(CMAKE_Fortran_FLAGS "-static")
endif()

ADD_EXECUTABLE( HVAC-Diagram ${SRC} )

SET_TARGET_PROPERTIES(HVAC-Diagram PROPERTIES
  COMPILE_FLAGS "-std=legacy -ffree-line-length-200 ${SHARED_FLAGS}"
)

include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
install_and_fixup_exe_target( HVAC-Diagram PostProcess )

