include(CMakeParseArguments)

# Install files from a remote url
# TYPE can be either "FILES" or "PROGRAMS"
# Use the appropriate TYPE to get the proper permissions on the installed file
# SOURCE should be a url where the function will attempt to download from
# DESTINATION is the absolute or relative destination which will be passed to 
# the built in install command after the SOURCE is downloaded to a temporary location
# If a fourth argument is provided it will be used for the file's install name.
# If a fifth argument is provided and "TRUE" the file will be saved to the temporary
# location at ${CMAKE_BINARY_DIR}/install_temp.
function( install_remote TYPE SOURCE DESTINATION )
  if( DEFINED ARGV3 )
    set(FILENAME "${ARGV3}")
  else()
    get_filename_component(FILENAME ${SOURCE} NAME)
  endif()
  set(OUTPUT_DIR "${CMAKE_BINARY_DIR}/install_temp")
  install(CODE "
  file(DOWNLOAD ${SOURCE} \"${OUTPUT_DIR}/${FILENAME}\" STATUS status_var TIMEOUT 120 INACTIVITY_TIMEOUT 120)
  list(GET status_var 0 status)
  list(GET status_var 1 status_msg)
  if( NOT (status EQUAL 0) )
    message(\"install_remote failed, trying again: ${SOURCE} error ${status_msg}\")
    file(DOWNLOAD ${SOURCE} \"${OUTPUT_DIR}/${FILENAME}\" STATUS status_var LOG log_var TIMEOUT 240 INACTIVITY_TIMEOUT 240)
    list(GET status_var 0 status)
    list(GET status_var 1 status_msg)
    if( NOT (status EQUAL 0) )
      message(SEND_ERROR \"install_remote failed after 2 attempts: ${SOURCE} error ${status_msg}\")
    endif() 
  endif()
  ")
  install(${TYPE} "${OUTPUT_DIR}/${FILENAME}" DESTINATION ${DESTINATION})
  if(NOT ARGV4)
    install(CODE "
      file(REMOVE \"${OUTPUT_DIR}/${FILENAME}\")
    ")
  endif()
endfunction()

# Similar to install_remote but explicitly for MacOSX plist files.
# This function will configure a unique bundle id based on build number
# so that packages will not try to relocate the .app to an older version location.
function( install_remote_plist SOURCE DESTINATION APP_NAME )
  install(CODE "
    file(DOWNLOAD \"${SOURCE}\" 
      \"${CMAKE_BINARY_DIR}/install_temp/Info.in.plist\" 
    )
    set(MACOSX_BUNDLE_GUI_IDENTIFIER \"gov.nrel.energyplus.${CMAKE_VERSION_BUILD}.${APP_NAME}\")
    configure_file(\"${CMAKE_BINARY_DIR}/install_temp/Info.in.plist\" \"${CMAKE_BINARY_DIR}/install_temp/Info.plist\")
  ")
  install(FILES "${CMAKE_BINARY_DIR}/install_temp/Info.plist" DESTINATION "${DESTINATION}")
  install(CODE "
    file(REMOVE_RECURSE \"${CMAKE_BINARY_DIR}/install_temp/Info.plist\")
  ")
endfunction()

# Add google tests macro
macro(ADD_GOOGLE_TESTS executable)
  foreach ( source ${ARGN} )
    string(REGEX MATCH .*cpp|.*cc source "${source}")
    if(source)
      file(READ "${source}" contents)
      string(REGEX MATCHALL "TEST_?F?\\(([A-Za-z_0-9 ,]+)\\)" found_tests ${contents})
      foreach(hit ${found_tests})
        string(REGEX REPLACE ".*\\(( )*([A-Za-z_0-9]+)( )*,( )*([A-Za-z_0-9]+)( )*\\).*" "\\2.\\5" test_name ${hit})
        add_test(NAME ${test_name} 
                 COMMAND "${executable}" "--gtest_filter=${test_name}")
      endforeach(hit)
    endif()
  endforeach()
endmacro()

# Create source groups automatically based on file path
macro( CREATE_SRC_GROUPS SRC )
  foreach( F ${SRC} )
    string( REGEX MATCH "(^.*)([/\\].*$)" M ${F} )
    if(CMAKE_MATCH_1)
      string( REGEX REPLACE "[/\\]" "\\\\" DIR ${CMAKE_MATCH_1} )
      source_group( ${DIR} FILES ${F} )
    else()
      source_group( \\ FILES ${F} )
    endif()
  endforeach()
endmacro()

# Create test targets
macro( CREATE_TEST_TARGETS BASE_NAME SRC DEPENDENCIES )
  if( BUILD_TESTING )
    add_executable( ${BASE_NAME}_tests ${SRC} )

    CREATE_SRC_GROUPS( "${SRC}" )
    
    get_target_property(BASE_NAME_TYPE ${BASE_NAME} TYPE)
    if ("${BASE_NAME_TYPE}" STREQUAL "EXECUTABLE")
      # don't link base name
      set(ALL_DEPENDENCIES ${DEPENDENCIES} )
    else()
      # also link base name
      set(ALL_DEPENDENCIES ${BASE_NAME} ${DEPENDENCIES} )
    endif()
      
    target_link_libraries( ${BASE_NAME}_tests 
      ${ALL_DEPENDENCIES} 
      gtest 
      gtest_main
    )

    ADD_GOOGLE_TESTS( ${BASE_NAME}_tests ${SRC} )
  endif()
endmacro()

function( ADD_SIMULATION_TEST )
  set(options ANNUAL_SIMULATION DESIGN_DAY_ONLY EXPECT_FATAL)
  set(oneValueArgs IDF_FILE EPW_FILE)
  set(multiValueArgs "")
  cmake_parse_arguments(ADD_SIM_TEST "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  if( DESIGN_DAY_ONLY )
    set(ANNUAL_SIMULATION false)
  elseif( ADD_SIM_TEST_ANNUAL_SIMULATION OR TEST_ANNUAL_SIMULATION  )
    set(ANNUAL_SIMULATION true)
  else()
    set(ANNUAL_SIMULATION false)
  endif()


  get_filename_component(IDF_NAME "${ADD_SIM_TEST_IDF_FILE}" NAME_WE)

  if ( PROFILE_GENERATE AND IDF_NAME MATCHES "^(ChilledWaterStorage-Mixed|AirflowNetwork3zVent|AirflowNetwork3zVentAutoWPC|DElightCFSWindow|PipeHeatTransfer_Outair|RadHiTempElecTermReheat|RadLoTempCFloTermReheat|RadLoTempHydrMulti10|RefBldgSmallOfficeNew2004_Chicago|WindowTestsSimple|.*CentralChillerHeaterSystem.*|EMSCustomOutputVariable|EMSTestMathAndKill)$")
    message("Setting ANNUAL_SIMULATION to true for ${IDF_NAME} for the purpose of PGO training")
    set(ANNUAL_SIMULATION true)
  endif()

  add_test(NAME "integration.${IDF_NAME}" COMMAND ${CMAKE_COMMAND}
    -DSOURCE_DIR=${CMAKE_SOURCE_DIR}
    -DBINARY_DIR=${CMAKE_BINARY_DIR}
    -DENERGYPLUS_EXE=$<TARGET_FILE:EnergyPlus>
    -DIDF_FILE=${ADD_SIM_TEST_IDF_FILE}
    -DEPW_FILE=${ADD_SIM_TEST_EPW_FILE}
    -DANNUAL_SIMULATION=${ANNUAL_SIMULATION}
    -DBUILD_FORTRAN=${BUILD_FORTRAN}
    -P ${CMAKE_SOURCE_DIR}/cmake/RunSimulation.cmake
  )  

  # MSVC's profile generator does not work with parallel runs
  #if( MSVC AND PROFILE_GENERATE )
    #set_tests_properties("integration.${IDF_NAME}" PROPERTIES RUN_SERIAL true)
  #endif()

  # Added the expect_fatal here to detect files that are expected to fatal error properly
  if( ADD_SIM_TEST_EXPECT_FATAL )
    set_tests_properties("integration.${IDF_NAME}" PROPERTIES PASS_REGULAR_EXPRESSION "Test Failed")
    set_tests_properties("integration.${IDF_NAME}" PROPERTIES FAIL_REGULAR_EXPRESSION "ERROR;FAIL;Test Passed")
  else()
    set_tests_properties("integration.${IDF_NAME}" PROPERTIES PASS_REGULAR_EXPRESSION "Test Passed")
    set_tests_properties("integration.${IDF_NAME}" PROPERTIES FAIL_REGULAR_EXPRESSION "ERROR;FAIL;Test Failed")
  endif()

  if ( PROFILE_GENERATE AND ANNUAL_SIMULATION )
    set_tests_properties("integration.${IDF_NAME}" PROPERTIES TIMEOUT 4500)
  endif()


  if( DO_REGRESSION_TESTING AND (NOT ADD_SIM_TEST_EXPECT_FATAL) )
    add_test(NAME "regression.${IDF_NAME}" COMMAND ${CMAKE_COMMAND}
      -DBINARY_DIR=${CMAKE_BINARY_DIR}
      -DPYTHON_EXECUTABLE=${PYTHON_EXECUTABLE}
      -DIDF_FILE=${ADD_SIM_TEST_IDF_FILE}
      -DREGRESSION_SCRIPT_PATH=${REGRESSION_SCRIPT_PATH}
      -DREGRESSION_BASELINE_PATH=${REGRESSION_BASELINE_PATH}
      -DREGRESSION_BASELINE_SHA=${REGRESSION_BASELINE_SHA}
      -DCOMMIT_SHA=${COMMIT_SHA}
      -DDEVICE_ID=${DEVICE_ID}
      -P ${CMAKE_SOURCE_DIR}/cmake/RunRegression.cmake
      )
    # Note, CMake / CTest doesn't seem to validate if this dependent name actually exists,
    # but it does seem to honor the requirement
    set_tests_properties("regression.${IDF_NAME}" PROPERTIES DEPENDS "integration.${IDF_NAME}")
    set_tests_properties("regression.${IDF_NAME}" PROPERTIES PASS_REGULAR_EXPRESSION "Success")
    set_tests_properties("regression.${IDF_NAME}" PROPERTIES FAIL_REGULAR_EXPRESSION "ERROR;FAIL;Test Failed")
  endif()

endfunction()

macro( ADD_CXX_DEFINITIONS NEWFLAGS )
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${NEWFLAGS}")
endmacro()

macro( ADD_CXX_DEBUG_DEFINITIONS NEWFLAGS )
  SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${NEWFLAGS}")
endmacro()

macro( ADD_CXX_RELEASE_DEFINITIONS NEWFLAGS )
  SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${NEWFLAGS}")
endmacro()

function(fixup_executable EXECUTABLE_PATH )
  include(GetPrerequisites)
  get_prerequisites("${EXECUTABLE_PATH}" PREREQUISITES 1 1 "" "")

  foreach(PREREQ IN LISTS PREREQUISITES)
      gp_resolve_item("" "${PREREQ}" "" "${LIBRARY_SEARCH_DIRECTORY}" resolved_item_var)
      get_filename_component(BASE_PATH "${EXECUTABLE_PATH}" DIRECTORY)
      execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${resolved_item_var}" "${BASE_PATH}")
      if(APPLE)
        get_filename_component(PREREQNAME "${resolved_item_var}" NAME)
        execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PREREQNAME}" "${EXECUTABLE_PATH}")
        foreach(PR IN LISTS PREREQUISITES)
          gp_resolve_item("" ${PR} "" "" PRPATH)
          get_filename_component( PRNAME ${PRPATH} NAME)
          execute_process(COMMAND "install_name_tool" -change "${PR}" "@loader_path/${PRNAME}" "${BASE_PATH}/${PREREQNAME}")
        endforeach()
      endif()
  endforeach()
endfunction()

# On Mac and linux this function copies in dependencies of target
# On windows it just installs the target
function(install_and_fixup_exe_target TARGET_NAME INSTALL_PATH)
  install( TARGETS ${TARGET_NAME} DESTINATION ${INSTALL_PATH} )
  #Warning this is only ok because we are counting on static linked executables on windows.
  if(NOT WIN32)
    install(CODE "
      include(\"${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake\")
      fixup_executable(\"\${CMAKE_INSTALL_PREFIX}/${INSTALL_PATH}/${TARGET_NAME}${CMAKE_EXECUTABLE_SUFFIX}\")
    ")
  endif()
endfunction()

