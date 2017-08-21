  # Set up for convenience
  set(TEST_DIR "tst/api_callback")
  set( ENV{DDONLY} y)

  # Currently the TestEnergyPlusCallbacks runner does not exit. Give it an idf and supporting files so that it will run.
  execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${SOURCE_DIR}/testfiles/${IDF_FILE}" "${BINARY_DIR}/${TEST_DIR}/in.idf" )
  execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${SOURCE_DIR}/weather/${EPW_FILE}" "${BINARY_DIR}/${TEST_DIR}/in.epw" )
  execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${BINARY_DIR}/Products/Energy+.idd" "${BINARY_DIR}/${TEST_DIR}/Energy+.idd" )
  execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${BINARY_DIR}/Products/Energy+.schema.epJSON" "${BINARY_DIR}/${TEST_DIR}/Energy+.schema.epJSON" )

  # Find and execute the test executable, passing the argument of the directory to run in
  if( WIN32 )
    set(ECHO_CMD cmd /C echo.)
  else()
    set(ECHO_CMD "echo")
  endif()
  find_program(TEST_EXE TestEnergyPlusCallbacks PATHS "${BINARY_DIR}/Products/" "${BINARY_DIR}/Products/Release/" "${BINARY_DIR}/Products/Debug/" )
  message( "Executing TestEnergyPlusCallbacks from ${TEST_EXE}" )
  message( "Passing run directory as ${TEST_DIR}" )
  execute_process(COMMAND ${ECHO_CMD} COMMAND "${TEST_EXE}" "${BINARY_DIR}/${TEST_DIR}")

  # Clean up
  execute_process(COMMAND "${CMAKE_COMMAND}" -E remove "${BINARY_DIR}/${TEST_DIR}/Energy+.idd" "${BINARY_DIR}/${TEST_DIR}/in.epw")

  # Check the outputs and return appropriately
  file(READ "${BINARY_DIR}/${TEST_DIR}/eplusout.end" FILE_CONTENT)
  string(FIND "${FILE_CONTENT}" "EnergyPlus Completed Successfully" RESULT)
  if( RESULT EQUAL 0 )
    message("Test Passed")
  else()
    message("Test Failed")
  endif()
