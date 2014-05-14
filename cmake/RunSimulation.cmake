
# These need to be defined by the caller
# SOURCE_DIR
# BINARY_DIR
# ENERGYPLUS_EXE
# EXPANDOBJECTS_EXE
# IDF_FILE
# EPW_FILE

get_filename_component(IDF_NAME "${IDF_FILE}" NAME_WE)

execute_process(COMMAND "${CMAKE_COMMAND}" -E remove_directory "${BINARY_DIR}/testfiles/${IDF_NAME}" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/testfiles/${IDF_FILE}" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/weather/${EPW_FILE}" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/in.epw" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/idd/Energy+.idd" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/Energy+.idd" )

if (BUILD_FORTRAN)
    # ExpandObjects (and other preprocessors) as necessary
    execute_process(COMMAND "${EXPANDOBJECTS_EXE}" WORKING_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}")
    if (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/expanded.idf")
	if (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")
	    file(REMOVE "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")
	endif ()
	file(RENAME "${BINARY_DIR}/testfiles/${IDF_NAME}/expanded.idf" "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")
    endif ()
endif ()

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

