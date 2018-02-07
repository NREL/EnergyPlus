option( 179D_COMPLIANCE "Generate 179D Compliance Report" OFF )

if( 179D_COMPLIANCE )
  include(ExternalProject)
  find_program(DPKG_CMD dpkg)
  if( NOT DPKG_CMD )
    message(FATAL_ERROR "Unable to find 'dpkg' which is required for extracting OpenStudio for running 179D compliance report")
  endif()

  file( DOWNLOAD https://github.com/NREL/OpenStudio/releases/download/v2.4.0/OpenStudio-2.4.0.f58a3e1808-Linux.deb ${CMAKE_BINARY_DIR}/OpenStudio.deb )
  execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/openstudio)
  execute_process(COMMAND ${DPKG_CMD} -x ${CMAKE_BINARY_DIR}/OpenStudio.deb ${CMAKE_BINARY_DIR}/openstudio)

  ExternalProject_Add(OS_EP_Bestest
    GIT_REPOSITORY https://github.com/lefticus/OS_EP_Bestest_Public
    GIT_TAG master
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    UPDATE_COMMAND ""
    INSTALL_COMMAND ""
  )

  configure_file("${CMAKE_SOURCE_DIR}/cmake/179D_Generate_Reports.sh.in" "${CMAKE_BINARY_DIR}/179D_Generate_Reports.sh")

  add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/179D_Reports/RESULTS5-2A.xlsx
		  COMMAND sh ${CMAKE_BINARY_DIR}/179D_Generate_Reports.sh
                  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/179D_Reports
                  COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_BINARY_DIR}/OS_EP_Bestest-prefix/src/OS_EP_Bestest/results ${CMAKE_BINARY_DIR}/179D_Reports
                  COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_BINARY_DIR}/OS_EP_Bestest-prefix/src/OS_EP_Bestest/results/bestest_zips ${CMAKE_BINARY_DIR}/179D_Reports
                  COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}/179D_Reports/bestest_zips
                  COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}/179D_Reports/resources
		  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/OS_EP_Bestest-prefix/src/OS_EP_Bestest
		  DEPENDS OS_EP_Bestest energyplus
		  )

  add_custom_target( OS_EP_Bestest_Files ALL
		     DEPENDS ${CMAKE_BINARY_DIR}/179D_Reports/RESULTS5-2A.xlsx
		  )  

  # todo, place output file in ${CMAKE_BINARY_DIR}/doc-build

endif()
