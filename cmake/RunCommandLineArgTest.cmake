  # Set up for convenience
  set(TEST_DIR "tst/cli_args")

  # Find and execute the test executable, passing the argument of the directory to run in
  if( WIN32 )
    set(ECHO_CMD cmd /C echo.)
  else()
    set(ECHO_CMD "echo")
  endif()
  find_program(TEST_EXE "energyplus" PATHS "${BINARY_DIR}/Products/" NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
  execute_process(COMMAND ${ECHO_CMD} COMMAND "${TEST_EXE}" -d "${BINARY_DIR}/${TEST_DIR}" -D "${SOURCE_DIR}/testfiles/${IDF_FILE}" RESULT_VARIABLE RESULT)

  # Check the outputs and return appropriately
  file(READ "${BINARY_DIR}/${TEST_DIR}/eplusout.end" FILE_CONTENT)
  string(FIND "${FILE_CONTENT}" "EnergyPlus Completed Successfully" RESULT)
  if( RESULT EQUAL 0 )
    message("Test Passed")
  else()
    message("Test Failed")
  endif()
