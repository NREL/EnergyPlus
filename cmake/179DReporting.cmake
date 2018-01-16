option( 179D_COMPLIANCE "Generate 179D Compliance Report" OFF )

if( 179D_COMPLIANCE )
  find_program(DPKG_CMD dpkg)
  if( NOT DPKG_CMD )
    message(FATAL_ERROR "Unable to find 'dpkg' which is required for extracting OpenStudio for running 179D compliance report")
  endif()

  file( DOWNLOAD https://github.com/NREL/OpenStudio/releases/download/v2.4.0/OpenStudio-2.4.0.f58a3e1808-Linux.deb ${CMAKE_BINARY_DIR}/OpenStudio.deb )
  execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/openstudio)
  execute_process(COMMAND ${DPKG_CMD} -x ${CMAKE_BINARY_DIR}/OpenStudio.deb ${CMAKE_BINARY_DIR}/openstudio)
 
  # todo, place output file in ${CMAKE_BINARY_DIR}/doc-build

endif()
