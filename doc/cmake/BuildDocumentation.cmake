# Caller needs to set:
  # XELATEX, the path to the xelatex compiler
  # INNAME, the name of the input tex file
  # OUTNAME, the pretty output name for the pdf
  # ORIGINAL_CMAKE_SOURCE_DIR, the root of the source repo
  # ORIGINAL_CMAKE_BINARY_DIR, the root of the build tree
# this will get more complicated as we add things like bibtex and makeindex, for now just execute xelatex twice and rename the output
if ("${TEX_INTERACTION}" STREQUAL "")
  set(THIS_TEX_INTERACTION "batchmode")
else()
  set(THIS_TEX_INTERACTION "${TEX_INTERACTION}")
endif()

set(COMMAND_ECHO_MODE NONE)
if (XELATEX_MEM_FLAGS)
  set(XELATEX_MEM_FLAGS "--extra-mem-top=2000000" "--extra-mem-bot=4000000")
endif()

if(DOCS_TESTING)

  # TODO: You can change this to ON for active debugging if you find a problem
  set(_DEBUG_DOCS OFF)
  if(_DEBUG_DOCS)
    set(COMMAND_ECHO_MODE STDOUT)
    set(CMAKE_VERBOSE_MAKEFILE ON)
  endif()

  if (XELATEX_MEM_FLAGS)
    message("XELATEX_MEM_FLAGS=${XELATEX_MEM_FLAGS}")
  endif()

  get_filename_component(XELATEX_BIN_DIR ${XELATEX} DIRECTORY)
  find_program(PDFTOTEXT NAME pdftotext HINTS ${XELATEX_BIN_DIR})
  if(NOT PDFTOTEXT)
    message(AUTHOR_WARNING "pdftotext should be in your path to test whether the Table of Contents worked. On Windows it should be installed via miktex already, on ubuntu it's apt install poppler-utils, on mac brew install poppler")
  endif()

  function(test_toc PASS_NUM DEBUG_DOCS)
    if(PDFTOTEXT)
      execute_process(
            COMMAND ${PDFTOTEXT} -f 2 -l 2 ${INNAME}.pdf -
            OUTPUT_VARIABLE _TOC_PAGE1_CONTENT
      )
      string(REPLACE "\n" ";" _TOC_PAGE_LIST "${_TOC_PAGE1_CONTENT}")

      # Clean it out
      set(_CLEANED_TOC "")
      set(_CLEANED_TOC_LIST "")
      foreach(LINE IN LISTS _TOC_PAGE_LIST)
        string(LENGTH "${LINE}" LINE_LEN)
        if (${LINE_LEN} GREATER 5)
          string(REPLACE " ." "" CLEANED_LINE "${LINE}")
          list(APPEND _CLEANED_TOC_LIST "${CLEANED_LINE}")
          set(_CLEANED_TOC "${_CLEANED_TOC}\n${CLEANED_LINE}")
        endif()
      endforeach()

      list(LENGTH _CLEANED_TOC_LIST _TOC_NUMENTRIES)
      if(_TOC_NUMENTRIES LESS 3)
        if (PASS_NUM LESS 2)
          message("${INNAME} Pass ${PASS_NUM}: TOC is missing (as expected)")
        else()
          if (DEBUG_DOCS)
            message(AUTHOR_WARNING "${INNAME} Pass ${PASS_NUM}: TOC is missing")
          else()
            message(FATAL_ERROR "${INNAME} Pass ${PASS_NUM}: TOC is missing")
          endif()
        endif()
      else()
        message("${INNAME} Pass ${PASS_NUM}: TOC OK, Number of entries in TOC = ${_TOC_NUMENTRIES}")
        if(DEBUG_DOCS)
          message("TOC Content:\n${_CLEANED_TOC}\n\n")
        endif()
      endif()
    endif()

    if(DEBUG_DOCS)
      file( COPY "${INNAME}.pdf" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass${PASS_NUM}.pdf" )
      file( COPY "${INNAME}.toc" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass${PASS_NUM}.toc" )
      file( COPY "${INNAME}.log" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass${PASS_NUM}.log" )
      file( COPY "${INNAME}.aux" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass${PASS_NUM}.aux" )
      file( COPY "${INNAME}.out" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass${PASS_NUM}.out" )
    endif()
  endfunction()

  message("================ RUNNING XELATEX THE FIRST TIME =====================")
endif()

execute_process(
  COMMAND "${XELATEX}" --interaction=${THIS_TEX_INTERACTION} ${XELATEX_MEM_FLAGS} ${INNAME}.tex
  TIMEOUT 600
  RESULT_VARIABLE ERRCODE
  COMMAND_ECHO ${COMMAND_ECHO_MODE}
)

if(DOCS_TESTING)
  test_toc(1 ${_DEBUG_DOCS})
endif()

if(DOCS_TESTING)
  message("================ RUNNING XELATEX THE SECOND TIME =====================")
endif()

execute_process(
  COMMAND "${XELATEX}" --interaction=${THIS_TEX_INTERACTION} ${XELATEX_MEM_FLAGS} ${INNAME}.tex
  TIMEOUT 600
  RESULT_VARIABLE ERRCODE
  COMMAND_ECHO ${COMMAND_ECHO_MODE}
)

if(DOCS_TESTING)
  test_toc(2 ${_DEBUG_DOCS})
endif()

if(DOCS_TESTING)
  message("================ RUNNING XELATEX THE THIRD TIME =====================")
endif()

execute_process(
  COMMAND "${XELATEX}" --interaction=${THIS_TEX_INTERACTION} ${XELATEX_MEM_FLAGS} ${INNAME}.tex
  TIMEOUT 600
  RESULT_VARIABLE ERRCODE
  COMMAND_ECHO ${COMMAND_ECHO_MODE}
)

if(DOCS_TESTING)
  test_toc(3 ${_DEBUG_DOCS})
endif()

file( COPY "${INNAME}.pdf" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/" )
file( RENAME "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}.pdf" "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${OUTNAME}.pdf")
