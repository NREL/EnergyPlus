# Only call this for APPLE
# set RESOLVED_PYTHON_LIB, and EXECUTABLE_PATH when calling

message("Fixing up Python Dependencies on Mac")

# get some path info things
get_filename_component(BASE_PATH ${EXECUTABLE_PATH} DIRECTORY)
get_filename_component(PYTHON_LIB_FILENAME ${RESOLVED_PYTHON_LIB} NAME)

# initialize this
set(EPLUS_LIB_NAME "")

# on Mac, we then need to fixup the binary
include(GetPrerequisites)
get_prerequisites("${EXECUTABLE_PATH}" PREREQUISITES 1 1 "" "")
foreach(PREREQ IN LISTS PREREQUISITES)
  string(TOLOWER "${PREREQ}" PREREQ_LOWERCASE )
  string(FIND "${PREREQ_LOWERCASE}" "python" PYTHON_IN_PREREQ)
  string(FIND "${PREREQ_LOWERCASE}" "libenergyplusapi" EPLUS_IN_PREREQ)
  if (NOT EPLUS_IN_PREREQ EQUAL -1)
    get_filename_component(EPLUS_LIB_NAME ${PREREQ} NAME)
  endif()
  if (NOT PYTHON_IN_PREREQ EQUAL -1)
    gp_resolve_item("" "${PREREQ}" "" "${LIBRARY_SEARCH_DIRECTORY}" resolved_item_var)
    execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PYTHON_LIB_FILENAME}" "${EXECUTABLE_PATH}")
  endif()
endforeach()

# Now, I know there are better ways, but for now I'm just going to repeat it for the eplus api lib as well
set(EPLUS_LIB_PATH "${BASE_PATH}/${EPLUS_LIB_NAME}")
get_prerequisites("${EPLUS_LIB_PATH}" PREREQUISITES 1 1 "" "")
foreach(PREREQ IN LISTS PREREQUISITES)
  string(TOLOWER "${PREREQ}" PREREQ_LOWERCASE )
  string(FIND "${PREREQ_LOWERCASE}" "python" PYTHON_IN_PREREQ)
  if (NOT PYTHON_IN_PREREQ EQUAL -1)
    gp_resolve_item("" "${PREREQ}" "" "${LIBRARY_SEARCH_DIRECTORY}" resolved_item_var)
    execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PYTHON_LIB_FILENAME}" "${EPLUS_LIB_PATH}")
  endif()
endforeach()
