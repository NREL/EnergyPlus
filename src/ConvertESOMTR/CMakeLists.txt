project(ConvertESOMTR Fortran)

set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/../../Products" )

set(SRC 
  mainModule.f90  
) 

add_executable( convertESOMTR ${SRC} )

if( WIN32 )
  set(CMAKE_Fortran_FLAGS "-static")
endif()

include("${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake")
install_and_fixup_exe_target(convertESOMTR "PostProcess/convertESOMTRpgm/")
install(FILES "convert.txt" DESTINATION "PostProcess/convertESOMTRpgm/")

