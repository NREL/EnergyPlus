message("Codesigning inner executables and library from ${CMAKE_CURRENT_LIST_FILE}")

message("CMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}")
message("CPACK_CODESIGNING_DEVELOPPER_ID_APPLICATION=${CPACK_CODESIGNING_DEVELOPPER_ID_APPLICATION}")
message("BUILD_FORTRAN=${BUILD_FORTRAN}")

if(NOT CPACK_CODESIGNING_DEVELOPPER_ID_APPLICATION)
  message(FATAL_ERROR "CPACK_CODESIGNING_DEVELOPPER_ID_APPLICATION is required")
endif()

if(NOT FILES_TO_SIGN)
  message(FATAL_ERROR "FILES_TO_SIGN is required")
endif()

if(NOT BUILD_FORTRAN)
  message(FATAL_ERROR "BUILD_FORTRAN is required")
endif()

function(print_relative_paths)
  set(prefix "")
  set(valueLessKeywords NAME_ONLY NEWLINE)
  set(singleValueKeywords PREFIX BASE_DIRECTORY)
  set(multiValueKeywords ABSOLUTE_PATHS)
  cmake_parse_arguments(
    PARSE_ARGV 0 # Start at one with NAME is the first param
      "${prefix}"
      "${valueLessKeywords}"
      "${singleValueKeywords}"
      "${multiValueKeywords}"
  )

  if (_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Extra unknown arguments were passed: ${_UNPARSED_ARGUMENTS}")
  endif()
  if (_KEYWORDS_MISSING_VALUES)
    message(FATAL_ERROR "Keywords missing values: ${_KEYWORDS_MISSING_VALUES}")
  endif()

  if(NOT _BASE_DIRECTORY)
    set(_BASE_DIRECTORY ${CMAKE_INSTALL_PREFIX})
  endif()

  foreach(path ${_ABSOLUTE_PATHS})
    if(_NAME_ONLY)
      cmake_path(GET path FILENAME rel_path)
    else()
      cmake_path(RELATIVE_PATH path BASE_DIRECTORY ${_BASE_DIRECTORY} OUTPUT_VARIABLE rel_path)
    endif()
    list(APPEND rel_paths ${rel_path})
  endforeach()

  if(_NEWLINE)
    message("${_PREFIX}")
    foreach(path ${rel_paths})
      message("   - ${path}")
    endforeach()
  else()
    message("${_PREFIX}${rel_paths}")
  endif()
endfunction()


foreach(path ${FILES_TO_SIGN})
  list(APPEND FULL_PATHS "${CMAKE_INSTALL_PREFIX}/${path}")
endforeach()

file(GLOB _all_root_dylibs "${CMAKE_INSTALL_PREFIX}/lib*.dylib")
foreach(path ${_all_root_dylibs})
  message("${path}")
  if(NOT IS_SYMLINK ${path})
    list(FIND FULL_PATHS ${path} _found)
    if(_found EQUAL -1)
      list(APPEND ROOT_DYLIBS ${path})
    endif()
  endif()
endforeach()

if(BUILD_FORTRAN)
  set(_fortran_utilities_relpaths
    "ExpandObjects"
    "PostProcess/ReadVarsESO"
    "PostProcess/AppGPostProcess"
    "PostProcess/HVAC-Diagram"
    "PostProcess/convertESOMTRpgm/convertESOMTR"
    "PreProcess/CalcSoilSurfTemp/CalcSoilSurfTemp"
    "PreProcess/GrndTempCalc/Basement"
    "PreProcess/GrndTempCalc/Slab"
    "PreProcess/ParametricPreprocessor/ParametricPreprocessor"
  )

  file(GLOB FORTRAN_UTILITIES "${CMAKE_INSTALL_PREFIX}/PreProcess/IDFVersionUpdater/Transition-V*")
  foreach(path ${_fortran_utilities_relpaths})
    list(APPEND FORTRAN_UTILITIES "${CMAKE_INSTALL_PREFIX}/${path}")
  endforeach()
endif()

file(GLOB PYTHON_SOS "${CMAKE_INSTALL_PREFIX}/python_standard_lib/lib-dynload/*.so")


print_relative_paths(PREFIX "FULL_PATHS=" ABSOLUTE_PATHS ${FULL_PATHS})
print_relative_paths(PREFIX "ROOT_DYLIBS=" ABSOLUTE_PATHS ${ROOT_DYLIBS})
print_relative_paths(PREFIX "FORTRAN_UTILITIES=" ABSOLUTE_PATHS ${FORTRAN_UTILITIES} NEWLINE)
print_relative_paths(PREFIX "PYTHON_SOS, in ${CMAKE_INSTALL_PREFIX}/python_standard_lib/lib-dynload/=" ABSOLUTE_PATHS ${PYTHON_SOS} NAME_ONLY)

include(${CMAKE_CURRENT_LIST_DIR}/CodeSigning.cmake)
codesign_files_macos(
  FILES ${FULL_PATHS} ${ROOT_DYLIBS} ${FORTRAN_UTILITIES} ${PYTHON_SOS}
  SIGNING_IDENTITY ${CPACK_CODESIGNING_DEVELOPPER_ID_APPLICATION}
  PREFIX "org.nrel.EnergyPlus."
  FORCE VERBOSE
)

message("Finished Codesigning inner executables and library")
