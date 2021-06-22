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

if(DOCS_TESTING)
  if (XELATEX_MEM_FLAGS)
    message("XELATEX_MEM_FLAGS=${XELATEX_MEM_FLAGS}")
  endif()

  get_filename_component(XELATEX_BIN_DIR ${XELATEX} DIRECTORY)
  find_program(PDFTOTEXT NAME pdftotext HINTS ${XELATEX_BIN_DIR})
  if(NOT PDFTOTEXT)
    message(AUTHOR_WARNING "pdftotext should be in your path to test whether the Table of Contents worked. On Windows it should be installed via miktex already, on ubuntu it's apt install poppler-utils, on mac brew install poppler")
  endif()

  message("================ RUNNING XELATEX THE FIRST TIME =====================")
endif()

execute_process(
  COMMAND ${XELATEX} -interaction=${THIS_TEX_INTERACTION} ${XELATEX_MEM_FLAGS} ${INNAME}.tex
  TIMEOUT 600
  RESULT_VARIABLE ERRCODE
)

if(DOCS_TESTING)
  if (PDFTOTEXT)
    message("PASS 1: ERRCODE=${ERRCODE}")
    execute_process(
      COMMAND ${PDFTOTEXT} -f 2 -l 2 ${INNAME}.pdf -
      OUTPUT_VARIABLE TOC_PAGE1_CONTENT
    )
    string(LENGTH ${TOC_PAGE1_CONTENT} TOC_PAGE1_CONTENT_LEN)
    message("PASS 1: TOC_PAGE1_CONTENT_LEN=${TOC_PAGE1_CONTENT_LEN}, TOC_PAGE1_CONTENT=${TOC_PAGE1_CONTENT}")
  endif()

  file( COPY "${INNAME}.pdf" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass1.pdf" )
  file( COPY "${INNAME}.toc" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass1.toc" )
  file( COPY "${INNAME}.log" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass1.log" )
  file( COPY "${INNAME}.aux" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass1.aux" )
  file( COPY "${INNAME}.out" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass1.out" )
endif()

if(DOCS_TESTING)
  message("================ RUNNING XELATEX THE SECOND TIME =====================")
endif()

execute_process(
  COMMAND ${XELATEX} -interaction=${THIS_TEX_INTERACTION} ${XELATEX_MEM_FLAGS} ${INNAME}.tex
  TIMEOUT 600
  RESULT_VARIABLE ERRCODE
)

if(DOCS_TESTING)
  if(PDFTOTEXT)
    message("PASS 2: ERRCODE=${ERRCODE}")
    execute_process(
      COMMAND ${PDFTOTEXT} -f 2 -l 2 ${INNAME}.pdf -
      OUTPUT_VARIABLE TOC_PAGE1_CONTENT
    )
    string(LENGTH ${TOC_PAGE1_CONTENT} TOC_PAGE1_CONTENT_LEN)
    message("PASS 2: TOC_PAGE1_CONTENT_LEN=${TOC_PAGE1_CONTENT_LEN}, TOC_PAGE1_CONTENT=${TOC_PAGE1_CONTENT}")
  endif()

  file( COPY "${INNAME}.pdf" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass2.pdf" )
  file( COPY "${INNAME}.toc" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass2.toc" )
  file( COPY "${INNAME}.log" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass2.log" )
  file( COPY "${INNAME}.aux" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass2.aux" )
  file( COPY "${INNAME}.out" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass2.out" )

endif()

if(DOCS_TESTING)
  message("================ RUNNING XELATEX THE THIRD TIME =====================")
endif()

execute_process(
  COMMAND ${XELATEX} -interaction=${THIS_TEX_INTERACTION} ${XELATEX_MEM_FLAGS} ${INNAME}.tex
  TIMEOUT 600
  RESULT_VARIABLE ERRCODE
)

if(DOCS_TESTING)
  if(PDFTOTEXT)
    message("PASS 3: ERRCODE=${ERRCODE}")
    execute_process(
      COMMAND ${PDFTOTEXT} -f 2 -l 2 ${INNAME}.pdf -
      OUTPUT_VARIABLE TOC_PAGE1_CONTENT
    )
    string(LENGTH ${TOC_PAGE1_CONTENT} TOC_PAGE1_CONTENT_LEN)
    message("PASS 3: TOC_PAGE1_CONTENT_LEN=${TOC_PAGE1_CONTENT_LEN}, TOC_PAGE1_CONTENT=${TOC_PAGE1_CONTENT}")

      # At this point I do expect the TOC to have worked
    if(TOC_PAGE1_CONTENT_LEN LESS 10)
      message(WARNING "The TOC for '${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${OUTNAME}.pdf' appears broken!")
    endif()

  endif()

  file( COPY "${INNAME}.pdf" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass3.pdf" )
  file( COPY "${INNAME}.toc" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass3.toc" )
  file( COPY "${INNAME}.log" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass3.log" )
  file( COPY "${INNAME}.aux" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass3.aux" )
  file( COPY "${INNAME}.out" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}_Pass3.out" )

endif()

file( COPY "${INNAME}.pdf" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/" )
file( RENAME "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}.pdf" "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${OUTNAME}.pdf")
