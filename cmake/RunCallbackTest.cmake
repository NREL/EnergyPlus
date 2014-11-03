  # Currently the TestEnergyPlusCallbacks runner does not exit. Give it an idf and supporting files so that it will run.
  execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${SOURCE_DIR}/testfiles/1ZoneUncontrolled.idf" "${BINARY_DIR}/tst/api_callback/in.idf" )
  execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${SOURCE_DIR}/weather/USA_CO_Golden-NREL.724666_TMY3.epw" "${BINARY_DIR}/tst/api_callback/in.epw" )
  execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${BINARY_DIR}/Energy+.idd" "${BINARY_DIR}/tst/api_callback/Energy+.idd" )
  
  # Find and execute the test executable, passing the argument of the directory to run in
  if( WIN32 )
    set(ECHO_CMD cmd /C echo.)
  else()
    set(ECHO_CMD "echo")
  endif()
  find_program(TEST_EXE TestEnergyPlusCallbacks PATHS "${BINARY_DIR}/Products/" NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
  execute_process(COMMAND ${ECHO_CMD} COMMAND "${TEST_EXE}" "${BINARY_DIR}/tst/api_callback")
  
  # Clean up
  execute_process(COMMAND "${CMAKE_COMMAND}" -E remove "${BINARY_DIR}/tst/api_callback/Energy+.idd" "${BINARY_DIR}/tst/api_callback/in.epw" )
  
  # Check the outputs
  file(READ "${BINARY_DIR}/tst/api_callback/eplusout.end" FILE_CONTENT)
  string(FIND "${FILE_CONTENT}" "EnergyPlus Completed Successfully" RESULT)

  if( RESULT EQUAL 0 )
    message("Test Passed")
  else()
    message("Test Failed")
  endif()
