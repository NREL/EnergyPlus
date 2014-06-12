
# These need to be defined by the caller
# BINARY_DIR
# PYTHON_EXECUTABLE
# REGRESSION_SCRIPT_PATH
# REGRESSION_BASELINE_PATH

get_filename_component(IDF_NAME "${IDF_FILE}" NAME_WE)

execute_process(COMMAND ${PYTHON_EXECUTABLE} "${REGRESSION_SCRIPT_PATH}/Testing/RegressionSuite/Scripts/RunComparison.py" "${BINARY_DIR}/testfiles/${IDF_NAME}" "${REGRESSION_BASELINE_PATH}/testfiles/${IDF_NAME}" )



