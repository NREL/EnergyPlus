project(ReadVars Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

SET(SRC ReadVarsESO.f90) 

ADD_EXECUTABLE( ReadVarsESO ${SRC} )

install(TARGETS ReadVarsESO DESTINATION PostProcess)
install(CODE "
  include(\"${CMAKE_SOURCE_DIR}/cmake/ProjectMacros.cmake\")
  fixup_executable(\"\${CMAKE_INSTALL_PREFIX}/PostProcess/ReadVarsESO${CMAKE_EXECUTABLE_SUFFIX}\")
")

