
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

# Read the file contents to check for special cases
file(READ "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" IDF_CONTENT)

# Handle setting up datasets files first
string( FIND "${IDF_CONTENT}" "Window5DataFile.dat" WINDOW_RESULT )
if ( "${WINDOW_RESULT}" GREATER -1 )
  file( MAKE_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}/DataSets" )
  file( COPY "${SOURCE_DIR}/datasets/Window5DataFile.dat" DESTINATION "${BINARY_DIR}/testfiles/${IDF_NAME}/DataSets" )
endif ()

# Handle setting up TDV dataset files next
string(FIND "${IDF_CONTENT}" "TDV" TDV_RESULT)
if ( "${TDV_RESULT}" GREATER -1 )
  file( MAKE_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}/DataSets" )
  file( MAKE_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}/DataSets/TDV" )
  file( COPY "${SOURCE_DIR}/datasets/TDV/" DESTINATION "${BINARY_DIR}/testfiles/${IDF_NAME}/DataSets/TDV/" )
endif ()

# Handle setting up External Interface FMU-import files next
# Note we only have FMUs for Win32 in the repo, this should be improved
string(FIND "${IDF_CONTENT}" "ExternalInterface:" EXTINT_RESULT)
if ( "${EXTINT_RESULT}" GREATER -1 )
  file( MAKE_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}/DataSets" )
  file( MAKE_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}/DataSets/FMUs" )
  file( COPY "${SOURCE_DIR}/datasets/FMUs/" DESTINATION "${BINARY_DIR}/testfiles/${IDF_NAME}/DataSets/FMUs/" )
endif ()

if(BUILD_FORTRAN)
  
  # EPMacro should run here first
  
  # Ground heat transfer objects
  
  # Parametric preprocessor next
  string(FIND "${IDF_CONTENT}" "Parametric:" PAR_RESULT)
  if ( "${PAR_RESULT}" GREATER -1 )
    find_program(PARAMETRIC_EXE parametricpreprocessor PATHS "${BINARY_DIR}/Products/" 
      NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
    message("Executing ParametricPreprocessor from ${PARAMETRIC_EXE}")
    execute_process(COMMAND "${PARAMETRIC_EXE}" "in.idf" WORKING_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}")
    
    # this handles the LBuildingAppGRotPar parametric file
    if (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/in-G000.idf")
      message("Parametric: Trying to run: in-G000.idf")
      FILE( RENAME "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" "${BINARY_DIR}/testfiles/${IDF_NAME}/in-original.idf" )
      FILE( RENAME "${BINARY_DIR}/testfiles/${IDF_NAME}/in-G000.idf" "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" )
    
    # this handles the LBuildingAppGRotPar and ParametricInsulation-5ZoneAirCooled parametric files
    elseif (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/in-000001.idf")
      message("Parametric: Trying to run: in-000001.idf")
      FILE( RENAME "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" "${BINARY_DIR}/testfiles/${IDF_NAME}/in-original.idf" )
      FILE( RENAME "${BINARY_DIR}/testfiles/${IDF_NAME}/in-000001.idf" "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" )
    
    # this shouldn't happen unless a new parametric file is added with a different processed filename
    else ()
      message("Couldn't find parametric preprocessor output file, attempting to continue with original in.idf")
    
    endif ()

  endif ()
  
  # Need to copy in Slab/Basement IDDs before ExpandObjects if relevant for this file
 # string(FIND "${IDF_CONTENT}" "GroundHeatTransfer:Control" GHT_RESULT)
 # if ( "${GHT_RESULT}" GREATER -1 )
 #   file ( COPY "${SOURCE_DIR}/idd/SlabGHT.idd" DESTINATION "${BINARY_DIR}/testfiles/${IDF_NAME}/" )
 #   file ( COPY "${SOURCE_DIR}/idd/BasementGHT.idd" DESTINATION "${BINARY_DIR}/testfiles/${IDF_NAME}/" )
 #   file ( COPY "${BINARY_DIR}/Products/Slab" DESTINATION "${BINARY_DIR}/testfiles/${IDF_NAME}/" )
 # endif ()
      
  # ExpandObjects (and other preprocessors) as necessary
  find_program(EXPANDOBJECTS_EXE ExpandObjects PATHS "${BINARY_DIR}/Products/" 
    NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
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

