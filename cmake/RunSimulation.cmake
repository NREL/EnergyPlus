
# These need to be defined by the caller
# SOURCE_DIR
# BINARY_DIR
# ENERGYPLUS_EXE
# IDF_FILE
# EPW_FILE
# BUILD_FORTRAN
# ENERGYPLUS_FLAGS

get_filename_component(IDF_NAME "${IDF_FILE}" NAME_WE)
get_filename_component(IDF_EXT "${IDF_FILE}" EXT)
get_filename_component(EXE_PATH "${ENERGYPLUS_EXE}" PATH)

# Clean up old test directory
execute_process(COMMAND "${CMAKE_COMMAND}" -E remove_directory "${BINARY_DIR}/testfiles/${IDF_NAME}" )
execute_process(COMMAND "${CMAKE_COMMAND}" -E make_directory "${BINARY_DIR}/testfiles/${IDF_NAME}" )

# Create path variables
set (OUTPUT_DIR_PATH "${BINARY_DIR}/testfiles/${IDF_NAME}/")
set (IDF_PATH "${SOURCE_DIR}/testfiles/${IDF_FILE}")
set (PRODUCT_PATH "${BINARY_DIR}/Products/")
set (EXE_PATH "${EXE_PATH}/")
set (EPW_PATH "${SOURCE_DIR}/weather/${EPW_FILE}")

# Read the file contents to check for special cases
file(READ "${IDF_PATH}" IDF_CONTENT)

# Convert flags back to CMake list
string(STRIP ${ENERGYPLUS_FLAGS} ENERGYPLUS_FLAGS)
string(REPLACE " " ";" ENERGYPLUS_FLAGS_LIST ${ENERGYPLUS_FLAGS})

# Use EPMacro if necessary
list(FIND ENERGYPLUS_FLAGS_LIST -m EPMACRO_RESULT)

if("${EPMACRO_RESULT}" GREATER -1)
  # first bring in all imf files into the run folder
  file( GLOB SRC_IMF_FILES "${SOURCE_DIR}/testfiles/*.imf" )
  foreach( IMF_FILE ${SRC_IMF_FILES} )
    file( COPY "${IMF_FILE}" DESTINATION "${OUTPUT_DIR_PATH}" )
  endforeach()
  # find the appropriate executable file
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
  # Move EPMacro to executable directory
  execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different "${EPMACRO_EXE}" "${EXE_PATH}")
endif()

if(BUILD_FORTRAN)

  # Parametric preprocessor next
  string(FIND "${IDF_CONTENT}" "Parametric:" PAR_RESULT)
  if ( "${PAR_RESULT}" GREATER -1 )
    find_program(PARAMETRIC_EXE parametricpreprocessor PATHS "${PRODUCT_PATH}" 
      NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy "${IDF_PATH}" "${OUTPUT_DIR_PATH}")
    execute_process(COMMAND "${PARAMETRIC_EXE}" "${IDF_FILE}" WORKING_DIRECTORY "${OUTPUT_DIR_PATH}")
    
    # this handles the LBuildingAppGRotPar parametric file
    if (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/${IDF_NAME}-G000.idf")
      set (IDF_PATH "${BINARY_DIR}/testfiles/${IDF_NAME}/${IDF_NAME}-G000.idf")
    
    # this handles the LBuildingAppGRotPar and ParametricInsulation-5ZoneAirCooled parametric files
    elseif (EXISTS "${BINARY_DIR}/testfiles/${IDF_NAME}/${IDF_NAME}-000001.idf")
      set (IDF_PATH "${BINARY_DIR}/testfiles/${IDF_NAME}/${IDF_NAME}-000001.idf")
    
    # this shouldn't happen unless a new parametric file is added with a different processed filename
    else ()
      message("Couldn't find parametric preprocessor output file for ${IDF_NAME}, attempting to continue with original input file.")
    
    endif ()

  endif () # parametric preprocessor definitions detected
  
  list(FIND ENERGYPLUS_FLAGS_LIST -x EXPAND_RESULT)

  if("${EXPAND_RESULT}" GREATER -1)
    find_program(EXPANDOBJECTS_EXE ExpandObjects PATHS "${PRODUCT_PATH}" 
      NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
    # Move to executable directory
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different "${EXPANDOBJECTS_EXE}" "${EXE_PATH}")
    
    find_program(BASEMENT_EXE Basement PATHS "${PRODUCT_PATH}" 
      NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
    # Move Basement to executable directory
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different "${BASEMENT_EXE}" "${EXE_PATH}")
    
    find_program(SLAB_EXE Slab PATHS "${PRODUCT_PATH}" 
      NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
    # Move Slab to executable directory
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different "${SLAB_EXE}" "${EXE_PATH}")
  endif()
  
  list(FIND ENERGYPLUS_FLAGS_LIST -r READVARS_RESULT)

  if("${READVARS_RESULT}" GREATER -1)
    find_program(READVARS_EXE ReadVarsESO PATHS "${PRODUCT_PATH}" 
      NO_DEFAULT_PATH NO_CMAKE_ENVIRONMENT_PATH NO_CMAKE_PATH NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_FIND_ROOT_PATH)
    # Move to executable directory
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different "${READVARS_EXE}" "${EXE_PATH}")
  endif()
  
endif() # build fortran


if( WIN32 )
  set(ECHO_CMD cmd /C echo.)
else()
  set(ECHO_CMD "echo")
endif()

execute_process(COMMAND ${ECHO_CMD}
                COMMAND "${ENERGYPLUS_EXE}" -w "${EPW_PATH}" -d "${OUTPUT_DIR_PATH}" ${ENERGYPLUS_FLAGS_LIST} "${IDF_PATH}" 
                WORKING_DIRECTORY "${OUTPUT_DIR_PATH}"
                RESULT_VARIABLE RESULT)

if( RESULT EQUAL 0 )
  message("Test Passed")
else()
  message("Test Failed")
endif()

