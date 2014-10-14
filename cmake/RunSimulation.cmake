
# These need to be defined by the caller
# SOURCE_DIR
# BINARY_DIR
# ENERGYPLUS_EXE
# IDF_FILE
# EPW_FILE
# ANNUAL_SIMULATION
# BUILD_FORTRAN

if(ANNUAL_SIMULATION)
 set( ENV{FULLANNUALRUN} y )
else()
 set( ENV{DDONLY} y)
endif()

get_filename_component(IDF_NAME "${IDF_FILE}" NAME_WE)

execute_process(COMMAND "${CMAKE_COMMAND}" -E remove_directory "${BINARY_DIR}/testfiles/${IDF_NAME}" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/testfiles/${IDF_FILE}" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/weather/${EPW_FILE}" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/in.epw" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${BINARY_DIR}/Energy+.idd" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/Energy+.idd" )

if(BUILD_FORTRAN)
  # ExpandObjects (and other preprocessors) as necessary
  find_program(EXPANDOBJECTS_EXE ExpandObjects PATHS "${BINARY_DIR}/Products/")
  message("Executing ExpandObjects from ${EXPANDOBJECTS_EXE}")
  execute_process(COMMAND "${EXPANDOBJECTS_EXE}" WORKING_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}")
  if (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/expanded.idf")
    if (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")
        file(REMOVE "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")
    endif()
    file(RENAME "${BINARY_DIR}/testfiles/${IDF_NAME}/expanded.idf" "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")
  endif()
endif()


if( WIN32 )
  set(ECHO_CMD cmd /C echo.)
else()
  set(ECHO_CMD "echo")
endif()


execute_process(COMMAND ${ECHO_CMD}
                COMMAND "${ENERGYPLUS_EXE}" WORKING_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}")


execute_process(COMMAND "${CMAKE_COMMAND}" -E remove 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/Energy+.idd"
                "${BINARY_DIR}/testfiles/${IDF_NAME}/in.epw" )

file(READ "${BINARY_DIR}/testfiles/${IDF_NAME}/eplusout.end" FILE_CONTENT)

string(FIND "${FILE_CONTENT}" "EnergyPlus Completed Successfully" RESULT)

if( RESULT EQUAL 0 )
  message("Test Passed")
else()
  message("Test Failed")
endif()

