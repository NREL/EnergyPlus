include(CMakeParseArguments)

# Add google tests macro
macro(ADD_GOOGLE_TESTS executable)
  foreach(source ${ARGN})
    string(REGEX MATCH .*cpp|.*cc source "${source}")
    if(source)
      file(READ "${source}" contents)
      string(REGEX MATCHALL "TEST_?F?\\(([A-Za-z_0-9 ,]+)\\)" found_tests ${contents})
      foreach(hit ${found_tests})
        string(REGEX REPLACE ".*\\(( )*([A-Za-z_0-9]+)( )*,( )*([A-Za-z_0-9]+)( )*\\).*" "\\2.\\5" test_name ${hit})
        add_test(NAME ${test_name} COMMAND "${executable}" "--gtest_filter=${test_name}")
      endforeach(hit)
    endif()
  endforeach()
endmacro()

# Create source groups automatically based on file path
macro(CREATE_SRC_GROUPS SRC)
  foreach(F ${SRC})
    string(REGEX MATCH "(^.*)([/\\].*$)" M ${F})
    if(CMAKE_MATCH_1)
      string(REGEX REPLACE "[/\\]" "\\\\" DIR ${CMAKE_MATCH_1})
      source_group(${DIR} FILES ${F})
    else()
      source_group(\\ FILES ${F})
    endif()
  endforeach()
endmacro()

# Create test targets
macro(CREATE_TEST_TARGETS BASE_NAME SRC DEPENDENCIES USE_PCH)
  if(BUILD_TESTING)

    add_executable(${BASE_NAME}_tests ${SRC})
    target_link_libraries(${BASE_NAME}_tests PRIVATE project_options project_warnings)

    if(USE_PCH)
      target_link_libraries(${BASE_NAME}_tests PRIVATE cpp_pch_files)
    endif()

    if(ENABLE_GTEST_DEBUG_MODE)
      target_compile_definitions(${BASE_NAME}_tests PRIVATE ENABLE_GTEST_DEBUG_MODE)
    endif()

    if(UNIX AND "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")
      # Disabled Warnings:
      # 1684 conversion from pointer to same-sized integral type (potential portability problem) - Due to gtest...
      target_compile_options(${BASE_NAME}_tests PRIVATE -diag-disable:1684)
    endif()

    create_src_groups("${SRC}")

    get_target_property(BASE_NAME_TYPE ${BASE_NAME} TYPE)
    if("${BASE_NAME_TYPE}" STREQUAL "EXECUTABLE")
      # don't link base name
      set(ALL_DEPENDENCIES ${DEPENDENCIES})
    else()
      # also link base name
      set(ALL_DEPENDENCIES ${BASE_NAME} ${DEPENDENCIES})
    endif()

    target_link_libraries(${BASE_NAME}_tests PRIVATE ${ALL_DEPENDENCIES} gtest)

    add_google_tests(${BASE_NAME}_tests ${SRC})
  endif()
endmacro()

# Named arguments
# IDF_FILE <filename> IDF input file
# EPW_FILE <filename> EPW weather file
#
# Optional Arguments
# DESIGN_DAY_ONLY force design day simulation
# ANNUAL_SIMULATION force annual simulation
# EXPECT_FATAL Expect simulation to fail
# PERFORMANCE Tag test as performance analysis
# COST <integer> Cost of this simulation relative to other simulations.
#                Higher cost simulations run earlier in an attempt to enhance
#                test parallelization and reduce overall test run time.

function(ADD_SIMULATION_TEST)
  set(options ANNUAL_SIMULATION DESIGN_DAY_ONLY EXPECT_FATAL PERFORMANCE)
  set(oneValueArgs IDF_FILE EPW_FILE COST)
  set(multiValueArgs ENERGYPLUS_FLAGS)
  # CMake Parse Arguments: will set the value of variables starting with 'ADD_SIM_TEST_', eg: 'ADD_SIM_TEST_IDF_FILE'
  cmake_parse_arguments(ADD_SIM_TEST "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  if(ADD_SIM_TEST_DESIGN_DAY_ONLY)
    set(ANNUAL_SIMULATION false)
    # If passed argument "ANNUAL_SIMULATION" or global cache variables
  elseif(ADD_SIM_TEST_ANNUAL_SIMULATION OR TEST_ANNUAL_SIMULATION)
    set(ANNUAL_SIMULATION true)
  else()
    set(ANNUAL_SIMULATION false)
  endif()

  # Note JM 2018-11-23: -r means "Call ReadVarEso", which unless you actually have BUILD_FORTRAN=TRUE shouldn't exist
  if(ANNUAL_SIMULATION)
    set(ENERGYPLUS_FLAGS "${ADD_SIM_TEST_ENERGYPLUS_FLAGS} -a")
  else()
    set(ENERGYPLUS_FLAGS "${ADD_SIM_TEST_ENERGYPLUS_FLAGS} -D")
  endif()

  # Add -r flag if BUILD_FORTRAN is on, regardless of whether we run regression/performance tests
  # So that it'll produce the CSV output automatically for convenience
  if(BUILD_FORTRAN)
    set(ENERGYPLUS_FLAGS "${ENERGYPLUS_FLAGS} -r")
  else()
    # Now, if you don't have BUILD_FORTRAN, but you actually need that because of regression/performance testing, we issue messages

    if(ADD_SIM_TEST_PERFORMANCE)
      # For performance testing, it's more problematic, because that'll cut on the ReadVarEso time
      message(WARNING "Will not be able to call ReadVarEso unless BUILD_FORTRAN=TRUE, skipping flag -r.")
    elseif(DO_REGRESSION_TESTING)
      # DO_REGRESSION_TESTING shouldn't really occur here since EnergyPlus/CMakeLists.txt will throw an error if BUILD_FORTRAN isn't enabled
      # Not that bad, just a dev warning
      message(AUTHOR_WARNING "Will not be able to call ReadVarEso unless BUILD_FORTRAN=TRUE, skipping flag -r.")
    endif()
  endif()

  get_filename_component(IDF_NAME "${ADD_SIM_TEST_IDF_FILE}" NAME_WE)

  if(ADD_SIM_TEST_PERFORMANCE)
    set(TEST_CATEGORY "performance")
    set(TEST_FILE_FOLDER "performance_tests")
  else()
    set(TEST_CATEGORY "integration")
    set(TEST_FILE_FOLDER "testfiles")
  endif()

  if(ADD_SIM_TEST_PERFORMANCE AND VALGRIND_ANALYZE_PERFORMANCE_TESTS)
    set(RUN_CALLGRIND TRUE)
  else()
    set(RUN_CALLGRIND FALSE)
  endif()

  add_test(
    NAME "${TEST_CATEGORY}.${IDF_NAME}"
    COMMAND
      ${CMAKE_COMMAND} -DSOURCE_DIR=${PROJECT_SOURCE_DIR} -DBINARY_DIR=${PROJECT_BINARY_DIR} -DENERGYPLUS_EXE=$<TARGET_FILE:energyplus>
      -DIDF_FILE=${ADD_SIM_TEST_IDF_FILE} -DEPW_FILE=${ADD_SIM_TEST_EPW_FILE} -DENERGYPLUS_FLAGS=${ENERGYPLUS_FLAGS} -DBUILD_FORTRAN=${BUILD_FORTRAN}
      -DTEST_FILE_FOLDER=${TEST_FILE_FOLDER} -DRUN_CALLGRIND:BOOL=${RUN_CALLGRIND} -DVALGRIND=${VALGRIND} -P
      ${PROJECT_SOURCE_DIR}/cmake/RunSimulation.cmake)

  if(ADD_SIM_TEST_COST AND NOT ADD_SIM_TEST_COST STREQUAL "")
    set_tests_properties("${TEST_CATEGORY}.${IDF_NAME}" PROPERTIES COST ${ADD_SIM_TEST_COST})
  endif()

  # Added the expect_fatal here to detect files that are expected to fatal error properly
  if(ADD_SIM_TEST_EXPECT_FATAL)
    set_tests_properties("${TEST_CATEGORY}.${IDF_NAME}" PROPERTIES PASS_REGULAR_EXPRESSION "Test Failed")
    set_tests_properties("${TEST_CATEGORY}.${IDF_NAME}" PROPERTIES FAIL_REGULAR_EXPRESSION "ERROR;FAIL;Test Passed")
  else()
    set_tests_properties("${TEST_CATEGORY}.${IDF_NAME}" PROPERTIES PASS_REGULAR_EXPRESSION "Test Passed")
    set_tests_properties("${TEST_CATEGORY}.${IDF_NAME}" PROPERTIES FAIL_REGULAR_EXPRESSION "ERROR;FAIL;Test Failed")
  endif()

  if(DO_REGRESSION_TESTING AND (NOT ADD_SIM_TEST_EXPECT_FATAL))
    add_test(
      NAME "regression.${IDF_NAME}"
      COMMAND
        ${CMAKE_COMMAND} -DBINARY_DIR=${PROJECT_BINARY_DIR} -DPYTHON_EXECUTABLE=${Python_EXECUTABLE} -DIDF_FILE=${ADD_SIM_TEST_IDF_FILE}
        -DREGRESSION_SCRIPT_PATH=${REGRESSION_SCRIPT_PATH} -DREGRESSION_BASELINE_PATH=${REGRESSION_BASELINE_PATH}
        -DREGRESSION_BASELINE_SHA=${REGRESSION_BASELINE_SHA} -DCOMMIT_SHA=${COMMIT_SHA} -DDEVICE_ID=${DEVICE_ID} -P
        ${PROJECT_SOURCE_DIR}/cmake/RunRegression.cmake)
    # Note, CMake / CTest doesn't seem to validate if this dependent name actually exists,
    # but it does seem to honor the requirement
    set_tests_properties("regression.${IDF_NAME}" PROPERTIES DEPENDS "${TEST_CATEGORY}.${IDF_NAME}")
    set_tests_properties("regression.${IDF_NAME}" PROPERTIES PASS_REGULAR_EXPRESSION "Success")
    set_tests_properties("regression.${IDF_NAME}" PROPERTIES FAIL_REGULAR_EXPRESSION "ERROR;FAIL;Test Failed")
  endif()

endfunction()

function(fixup_executable EXECUTABLE_PATH)
  include(GetPrerequisites)
  get_prerequisites("${EXECUTABLE_PATH}" PREREQUISITES 1 1 "" "")

  foreach(PREREQ IN LISTS PREREQUISITES)
    gp_resolve_item("" "${PREREQ}" "" "${LIBRARY_SEARCH_DIRECTORY}" resolved_item_var)
    get_filename_component(BASE_PATH "${EXECUTABLE_PATH}" DIRECTORY)
    execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${resolved_item_var}" "${BASE_PATH}")
    if(APPLE)
      get_filename_component(PREREQNAME "${resolved_item_var}" NAME)
      execute_process(COMMAND "chmod" "+w" "${BASE_PATH}/${PREREQNAME}")
      execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PREREQNAME}" "${EXECUTABLE_PATH}")
      foreach(PR IN LISTS PREREQUISITES)
        gp_resolve_item("" ${PR} "" "" PRPATH)
        get_filename_component(PRNAME ${PRPATH} NAME)
        execute_process(COMMAND "install_name_tool" -change "${PR}" "@loader_path/${PRNAME}" "${BASE_PATH}/${PREREQNAME}")
      endforeach()
    endif()
  endforeach()
endfunction()

# On dynamic exes, this function copies in dependencies of the target
function(install_target_prereqs TARGET_NAME INSTALL_PATH)
  install(
    CODE "
    include(\"${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/ProjectMacros.cmake\")
    fixup_executable(\"\${CMAKE_INSTALL_PREFIX}/${INSTALL_PATH}/${TARGET_NAME}${CMAKE_EXECUTABLE_SUFFIX}\")
  ")
endfunction()
