# Caller needs to set:
  # XELATEX, the path to the xelatex compiler
  # INNAME, the name of the input tex file
  # OUTNAME, the pretty output name for the pdf
  # ORIGINAL_CMAKE_SOURCE_DIR, the root of the source repo
  # ORIGINAL_CMAKE_BINARY_DIR, the root fo the build tree
# this will get more complicated as we add things like bibtex and makeindex, for now just execute xelatex twice and rename the output
if ("${TEX_INTERACTION}" STREQUAL "")
  set(THIS_TEX_INTERACTION "batchmode")
else()
  set(THIS_TEX_INTERACTION "${TEX_INTERACTION}")
endif()
execute_process( COMMAND ${XELATEX} -interaction=${THIS_TEX_INTERACTION} ${INNAME}.tex TIMEOUT 600 )
execute_process( COMMAND ${XELATEX} -interaction=${THIS_TEX_INTERACTION} ${INNAME}.tex TIMEOUT 600 )
execute_process( COMMAND ${XELATEX} -interaction=${THIS_TEX_INTERACTION} ${INNAME}.tex TIMEOUT 600 )
file( COPY "${INNAME}.pdf" DESTINATION "${ORIGINAL_CMAKE_BINARY_DIR}/doc-pdf/" )
file( RENAME "${ORIGINAL_CMAKE_BINARY_DIR}/doc-pdf/${INNAME}.pdf" "${ORIGINAL_CMAKE_BINARY_DIR}/doc-pdf/${OUTNAME}.pdf")
