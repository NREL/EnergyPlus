# Steps we need to follow for now:
#  Use finder to get Python executable
#  Use finder to get Python library and include paths
#  Add Python include path to include_directories
#  Link E+ against Python library
#  At install time find the Python library and copy it into the install tree
#  At install time find the Python site-packages folder and copy it into the install tree
#  In E+ need to setPath before calling PyInitialize so we can get all the site_packages
# Now we should also consider whether we want to try to build *without* Python
#  I can imagine doing this on the 32 bit Windows, for example.
#  And this would *not* exclude calling E+ as a library from C or Python -- it would just disable Python Plugins
#  If we don't do this then we'll need to install both 32 and 64 bit Python on Windows and get the right one

# We need to connect up to python for a couple reasons.
# 1. We use Python in our testing scripts
# 2. We link EnergyPlus up against the Python lib for Python Plugin work
# 3. We use Python for our Python API runs
# We are going to create a local virtual environment that is portable so we can package it and install it
# Users will not need to have Python installed, and it will come with a Python.exe and a Pip.exe for installing libraries

function(setup_python_dependencies EXECUTABLE_PATH ORIGINAL_PYTHON_LIB_PATH RESOLVED_PYTHON_LIB)
  message("**Inside setup_python_dependencies**")
  include(GetPrerequisites)
  get_filename_component(PYTHON_LIB_FILENAME "${RESOLVED_PYTHON_LIB}" NAME)
  get_filename_component(BASE_PATH "${EXECUTABLE_PATH}" DIRECTORY)
  execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${RESOLVED_PYTHON_LIB}" "${BASE_PATH}")
  get_prerequisites("${EXECUTABLE_PATH}" PREREQUISITES 1 1 "" "")
  foreach(PREREQ IN LISTS PREREQUISITES)
    string(TOLOWER "${PREREQ}" PREREQ_LOWERCASE )
    string(FIND "${PREREQ_LOWERCASE}" "python" PYTHON_IN_PREREQ)
    if (NOT PYTHON_IN_PREREQ EQUAL -1)
      gp_resolve_item("" "${PREREQ}" "" "${LIBRARY_SEARCH_DIRECTORY}" resolved_item_var)
      if(APPLE)
        execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PYTHON_LIB_FILENAME}" "${EXECUTABLE_PATH}")
      endif()
    endif()
  endforeach()
endfunction()

function(install_python_dependencies INITIAL_FOUND_LIBRARY RESOLVED_LIBRARY)
  message("**Inside install_python_dependencies**")
  install(CODE "
    include(\"${CMAKE_CURRENT_SOURCE_DIR}/cmake/SetupPython.cmake\")
    setup_python_dependencies(
      \"/eplus/repos/myoldmopar/cmake-build-debug/_CPack_Packages/Darwin/IFW/EnergyPlus-9.3.0-66943131ca-Darwin-x86_64-Debug/packages/Unspecified/data/energyplus-9.3.0\"
      \"${INITIAL_FOUND_LIBRARY}\"
      \"${RESOLVED_LIBRARY}\"
    )
  ")
  install(DIRECTORY "/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/lib/python3.7"
          DESTINATION "./pypackages")
  install(DIRECTORY "/eplus/repos/myoldmopar/cmake-build-debug/Products/pyenergyplus/"
          DESTINATION "./pyenergyplus")
endfunction()



# message("Python configuration (1): Got Python Executable: \n  Executable: ${PYTHON_EXECUTABLE} \n  Include Dirs: ${PYTHON_INCLUDE_DIRS}")

## At this point, we should have *a* Python executable, so we can make sure we also have a virtual environment in the build tree
#if(EXISTS ${CMAKE_BINARY_DIR}/Products/pythonenv)
#  message("Found Python Virtual Environment at ${CMAKE_BINARY_DIR}/Products/pythonenv")
#else()
#  message("Creating virtual environment")
#  execute_process(
#          COMMAND "${PYTHON_EXECUTABLE}" "-m" "venv" "${CMAKE_BINARY_DIR}/Products/pythonenv"
#          TIMEOUT 30
#          RESULT_VARIABLE ENV_EXIT_CODE
#          OUTPUT_VARIABLE CREATION_OUTPUT
#  )
#  message("${CREATION_OUTPUT}")
#  set(EXE_EXTENSION "")
#  if(WIN32)
#    set(EXE_EXTENSION ".EXE")
#  endif()
#  message("Cleaning up virtual environment")
#  file(REMOVE ${CMAKE_BINARY_DIR}/Products/pythonenv/bin/activate)
#  file(REMOVE ${CMAKE_BINARY_DIR}/Products/pythonenv/bin/activate.csh)
#  file(REMOVE ${CMAKE_BINARY_DIR}/Products/pythonenv/bin/activate.fish)
#  file(REMOVE ${CMAKE_BINARY_DIR}/Products/pythonenv/bin/easy_install)
#  file(REMOVE ${CMAKE_BINARY_DIR}/Products/pythonenv/bin/easy_install-3.7)
#  file(REMOVE ${CMAKE_BINARY_DIR}/Products/pythonenv/bin/pip)
#  file(REMOVE ${CMAKE_BINARY_DIR}/Products/pythonenv/bin/pip3.7)
#  file(REMOVE ${CMAKE_BINARY_DIR}/Products/pythonenv/bin/python)
#  file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/Products/pythonenv/include)
#  file(REMOVE ${CMAKE_BINARY_DIR}/Products/pythonenv/pyvenv.cfg)
#endif()
#
#message("Python configuration (2): Got Python Executable: \n  Executable: ${PYTHON_EXECUTABLE} \n  Version String: ${PYTHONLIBS_VERSION_STRING}")
#
## Now we need to try to make sure the Python executable is the right one, it should point to the one in the build env
#get_filename_component(ACTUALEXELOCATION "${PYTHON_EXECUTABLE}" ABSOLUTE)
#set(EXE_EXTENSION "")
#if(WIN32)
#  set(EXE_EXTENSION ".EXE")
#  get_filename_component(IDEALEXELOCATION "${CMAKE_BINARY_DIR}/PRODUCTS/PYTHONENV/BIN/PYTHON3.EXE" ABSOLUTE)
#  string(TOUPPER ACTUALEXELOCATION "${ACTUALEXELOCATION}")  # TODO: Will this work?
#else()
#  get_filename_component(IDEALEXELOCATION "${CMAKE_BINARY_DIR}/Products/pythonenv/bin/python3" ABSOLUTE)
#endif()
#
#message("Python configuration (3): \n  Actual Python EXE: ${ACTUALEXELOCATION}\n  Ideal Python EXE: ${IDEALEXELOCATION}")
#
#if("${ACTUALEXELOCATION}" STREQUAL "${IDEALEXELOCATION}")
#  # great, we have one in the build env, carry on
#else()
#  # we have an external Python executable -- need to point to the one in the build env
#  set(PYTHON_EXECUTABLE "${IDEALEXELOCATION}")
#  find_package(PythonInterp 3 REQUIRED)
#  # we should now have the ideal python, in the build tree, but we need to be *very* sure
#  if ("${PYTHON_EXECUTABLE}" STREQUAL "${IDEALEXELOCATION}")
#    # I am using IDEAL location in the fixup call because I never want to accidentally "fixup" a system python executable
#    message("Fixing up Python executable")
#    #fixup_executable(${IDEALEXELOCATION})
#
#    get_prerequisites("${IDEALEXELOCATION}" PREREQUISITES 1 1 "" "")
#
#    foreach(PREREQ IN LISTS PREREQUISITES)
#      gp_resolve_item("" "${PREREQ}" "" "${LIBRARY_SEARCH_DIRECTORY}" resolved_item_var)
#      get_filename_component(BASE_PATH "${IDEALEXELOCATION}" DIRECTORY)
#      message("${CMAKE_COMMAND} -E copy ${resolved_item_var} ${BASE_PATH}")
#      # execute_process(COMMAND "${CMAKE_COMMAND}" -E copy "${resolved_item_var}" "${BASE_PATH}")
#      if(APPLE)
#        get_filename_component(PREREQNAME "${resolved_item_var}" NAME)
#        message("install_name_tool -change ${PREREQ} @executable_path/${PREREQNAME} ${IDEALEXELOCATION}")
#        # execute_process(COMMAND "install_name_tool" -change "${PREREQ}" "@executable_path/${PREREQNAME}" "${IDEALEXELOCATION}")
##        foreach(PR IN LISTS PREREQUISITES)
##          gp_resolve_item("" ${PR} "" "" PRPATH)
##          get_filename_component( PRNAME ${PRPATH} NAME)
##          message("install_name_tool -change ${PR} @loader_path/${PRNAME} ${BASE_PATH}/${PREREQNAME}")
##          #execute_process(COMMAND "install_name_tool" -change "${PR}" "@loader_path/${PRNAME}" "${BASE_PATH}/${PREREQNAME}")
##        endforeach()
#      endif()
#    endforeach()
#
#    # this is currently resulting in an invalid Python executable
#  else()
#    error("Did not find Python executable in ideal location -- problem!")
#  endif()
#
#  # TODO: then hint the library finder to the build env
#  execute_process(
#          COMMAND "${PYTHON_EXECUTABLE}" "-c" "\"import distutils.sysconfig as sysconfig; print(sysconfig.get_config_var('LIBDIR'))\""
#          OUTPUT_VARIABLE THIS_OUTPUT
#  )
#  set(PYTHON_LIBRARY ${THIS_OUTPUT})
#  find_package(PythonLibs 3 REQUIRED)
#  include_directories(${PYTHON_INCLUDE_DIRS})
#endif()

# Now we should have a PYTHON_EXECUTABLE that points to the desired locale in the build tree, fixed up, and with libs as well
# message("Python configuration (4): \n  Executable: ${PYTHON_EXECUTABLE} \n  Include Dirs: ${PYTHON_INCLUDE_DIRS} \n  Version String: ${PYTHONLIBS_VERSION_STRING} \n  Library: ${PYTHON_LIBRARIES}")
