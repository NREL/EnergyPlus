
# These need to be defined by the caller
# SOURCE_DIR
# BINARY_DIR
# ENERGYPLUS_EXE
# IDF_FILE

get_filename_component(IDF_NAME "${IDF_FILE}" NAME_WE)

execute_process(COMMAND "${CMAKE_COMMAND}" -E remove_directory "${BINARY_DIR}/testfiles/${IDF_NAME}" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/testfiles/${IDF_FILE}" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/weather/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/in.epw" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/idd/Energy+.idd" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/Energy+.idd" )

execute_process(COMMAND "${ENERGYPLUS_EXE}" WORKING_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}")

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

