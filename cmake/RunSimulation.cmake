
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
get_filename_component(IDF_EXT "${IDF_FILE}" EXT)

execute_process(COMMAND "${CMAKE_COMMAND}" -E remove_directory "${BINARY_DIR}/testfiles/${IDF_NAME}" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/testfiles/${IDF_FILE}" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/in${IDF_EXT}" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${SOURCE_DIR}/weather/${EPW_FILE}" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/in.epw" )

execute_process(COMMAND "${CMAKE_COMMAND}" -E copy 
                "${BINARY_DIR}/Energy+.idd" 
                "${BINARY_DIR}/testfiles/${IDF_NAME}/Energy+.idd" )

# Read the file contents to check for special cases
file(READ "${BINARY_DIR}/testfiles/${IDF_NAME}/in${IDF_EXT}" IDF_CONTENT)

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

# EPMacro should run here first
if( "${IDF_EXT}" STREQUAL ".imf" )
  # first bring in all imf files into the run folder
  file( GLOB SRC_IMF_FILES "${SOURCE_DIR}/testfiles/*.imf" )
  foreach( IMF_FILE ${SRC_IMF_FILES} )
    file( COPY "${IMF_FILE}" DESTINATION "${BINARY_DIR}/testfiles/${IDF_NAME}/" )
  endforeach()
  # find the appropriate macro file
  if( UNIX AND NOT APPLE )
    find_program(EPMACRO_EXE EPMacro PATHS "${SOURCE_DIR}/bin/EPMacro/Linux" 
      NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
  elseif( APPLE )
    find_program(EPMACRO_EXE EPMacro PATHS "${SOURCE_DIR}/bin/EPMacro/Mac" 
      NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
  else() # windows
    find_program(EPMACRO_EXE EPMacro PATHS "${SOURCE_DIR}/bin/EPMacro/Windows" 
      NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
  endif()
  # execute EPMacro and rename the idf file
  message("Executing EPMacro from ${EPMACRO_EXE}")
  execute_process(COMMAND "${EPMACRO_EXE}" WORKING_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}")
  file(RENAME "${BINARY_DIR}/testfiles/${IDF_NAME}/out.idf" "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")
endif()
    

if(BUILD_FORTRAN)

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

  endif () # parametric preprocessor definitions detected
        
  # ExpandObjects (and other preprocessors) as necessary
  find_program(EXPANDOBJECTS_EXE ExpandObjects PATHS "${BINARY_DIR}/Products/" 
    NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
  message("Executing ExpandObjects from ${EXPANDOBJECTS_EXE}")
  execute_process(COMMAND "${EXPANDOBJECTS_EXE}" WORKING_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}")
  
  if (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/expanded.idf")
  
    # Copy the expanded idf into in.idf, which could be updated with ground stuff as well
    if (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")
        file(REMOVE "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")
    endif()
    file(RENAME "${BINARY_DIR}/testfiles/${IDF_NAME}/expanded.idf" "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf")

    # i f we have an expanded.idf, then ExpandObjects must have found something
    # it is possible this includes ground heat transfer objects, so run them here if needed
    # Need to copy in Slab/Basement IDDs before ExpandObjects if relevant for this file
    string(FIND "${IDF_CONTENT}" "GroundHeatTransfer:Slab" SLAB_RESULT)
    if ( "${SLAB_RESULT}" GREATER -1 )
      file ( COPY "${SOURCE_DIR}/idd/SlabGHT.idd" DESTINATION "${BINARY_DIR}/testfiles/${IDF_NAME}/" )
      # Find and run slab
      find_program(SLAB_EXE Slab PATHS "${BINARY_DIR}/Products/" 
        NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
      message("Executing Slab from ${SLAB_EXE}")
      execute_process(COMMAND "${SLAB_EXE}" WORKING_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}")
      # Then copy slab results into the expanded file
      file(READ "${BINARY_DIR}/testfiles/${IDF_NAME}/SLABSurfaceTemps.TXT" SLAB_CONTENTS)
      file(APPEND "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" "${SLAB_CONTENTS}")
    endif ()
    
    string(FIND "${IDF_CONTENT}" "GroundHeatTransfer:Basement" BASEMENT_RESULT)
    if ( "${BASEMENT_RESULT}" GREATER -1 )
      file ( COPY "${SOURCE_DIR}/idd/BasementGHT.idd" DESTINATION "${BINARY_DIR}/testfiles/${IDF_NAME}/" )
      # Find and run basement
      find_program(BASEMENT_EXE Basement PATHS "${BINARY_DIR}/Products/" 
        NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
      message("Executing Basement from ${BASEMENT_EXE}")
      execute_process(COMMAND "${BASEMENT_EXE}" WORKING_DIRECTORY "${BINARY_DIR}/testfiles/${IDF_NAME}")
      # Then copy basement results into the expanded file
      file(READ "${BINARY_DIR}/testfiles/${IDF_NAME}/EPObjects.TXT" BASEMENT_CONTENTS)
      file(APPEND "${BINARY_DIR}/testfiles/${IDF_NAME}/in.idf" "${BASEMENT_CONTENTS}")
    endif ()

  endif() # expand objects found something and created expanded.idf
  
endif() # build fortran


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

