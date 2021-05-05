# These need to be defined by the caller
# IDF_FILE
# BINARY_DIR
# PYTHON_EXECUTABLE
# REGRESSION_SCRIPT_PATH
# REGRESSION_BASELINE_PATH
# REGRESSION_BASELINE_SHA
# COMMIT_SHA
# DEVICE_ID

get_filename_component(IDF_NAME "${IDF_FILE}" NAME_WE)

execute_process(
  COMMAND
    ${PYTHON_EXECUTABLE} "${REGRESSION_SCRIPT_PATH}/epregressions/diffs/ci_compare_script.py" "${IDF_NAME}"
    "${REGRESSION_BASELINE_PATH}/testfiles/${IDF_NAME}" "${BINARY_DIR}/testfiles/${IDF_NAME}" ${REGRESSION_BASELINE_SHA} ${COMMIT_SHA} true
    "${DEVICE_ID}")
