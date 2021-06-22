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
  string(REPLACE xelatex pdftotext PDFTOTEXT ${XELATEX})

  message("================ RUNNING XELATEX THE FIRST TIME =====================")
endif()

execute_process(
  COMMAND ${XELATEX} -interaction=${THIS_TEX_INTERACTION} ${INNAME}.tex
  TIMEOUT 600
  RESULT_VARIABLE ERRCODE
)

if(DOCS_TESTING)
  message("PASS 1: ERRCODE=${ERRCODE}")
  execute_process(
    COMMAND ${PDFTOTEXT} -f 2 -l 2 ${INNAME}.pdf -
    OUTPUT_VARIABLE TOC_PAGE1_CONTENT
  )
  string(LENGTH ${TOC_PAGE1_CONTENT} TOC_PAGE1_CONTENT_LEN)
  message("PASS 1: TOC_PAGE1_CONTENT_LEN=${TOC_PAGE1_CONTENT_LEN}, TOC_PAGE1_CONTENT=${TOC_PAGE1_CONTENT}")
endif()

if(DOCS_TESTING)
  message("================ RUNNING XELATEX THE SECOND TIME =====================")
endif()

execute_process(
  COMMAND ${XELATEX} -interaction=${THIS_TEX_INTERACTION} ${INNAME}.tex
  TIMEOUT 600
  RESULT_VARIABLE ERRCODE
)

if(DOCS_TESTING)
  message("PASS 2: ERRCODE=${ERRCODE}")
  execute_process(
    COMMAND ${PDFTOTEXT} -f 2 -l 2 ${INNAME}.pdf -
    OUTPUT_VARIABLE TOC_PAGE1_CONTENT
  )
  string(LENGTH ${TOC_PAGE1_CONTENT} TOC_PAGE1_CONTENT_LEN)
  message("PASS 2: TOC_PAGE1_CONTENT_LEN=${TOC_PAGE1_CONTENT_LEN}, TOC_PAGE1_CONTENT=${TOC_PAGE1_CONTENT}")
endif()

if(DOCS_TESTING)
  message("================ RUNNING XELATEX THE THIRD TIME =====================")
endif()

execute_process(
  COMMAND ${XELATEX} -interaction=${THIS_TEX_INTERACTION} ${INNAME}.tex
  TIMEOUT 600
  RESULT_VARIABLE ERRCODE
)

if(DOCS_TESTING)
  message("PASS 3: ERRCODE=${ERRCODE}")
  execute_process(
    COMMAND ${PDFTOTEXT} -f 2 -l 2 ${INNAME}.pdf -
    OUTPUT_VARIABLE TOC_PAGE1_CONTENT
  )
  string(LENGTH ${TOC_PAGE1_CONTENT} TOC_PAGE1_CONTENT_LEN)
  message("PASS 3: TOC_PAGE1_CONTENT_LEN=${TOC_PAGE1_CONTENT_LEN}, TOC_PAGE1_CONTENT=${TOC_PAGE1_CONTENT}")
endif()

file( COPY "${INNAME}.pdf" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/" )
file( RENAME "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${INNAME}.pdf" "${ORIGINAL_CMAKE_BINARY_DIR}/pdf/${OUTNAME}.pdf")
