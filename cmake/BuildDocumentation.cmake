# Caller needs to set:
  # XELATEX, the path to the xelatex compiler
  # INNAME, the name of the input tex file
  # OUTNAME, the pretty output name for the pdf
  # ORIGINAL_CMAKE_SOURCE_DIR, the root of the source repo
  # ORIGINAL_CMAKE_BINARY_DIR, the root fo the build tree
# this will get more complicated as we add things like bibtex and makeindex, for now just copy the files into the build tree, execute xelatex twice and rename the output
#message("Hoping to copy from ${ORIGINAL_CMAKE_SOURCE_DIR}/doc/${INNAME} to ${ORIGINAL_CMAKE_BINARY_DIR}/doc-build/${INNAME}")
execute_process( COMMAND cmake -E copy_directory ${ORIGINAL_CMAKE_SOURCE_DIR}/doc/${INNAME} ${ORIGINAL_CMAKE_BINARY_DIR}/doc-build/${INNAME})
execute_process( COMMAND ${XELATEX} -interaction=batchmode ${INNAME}.tex )
execute_process( COMMAND ${XELATEX} -interaction=batchmode ${INNAME}.tex )
execute_process( COMMAND ${XELATEX} -interaction=batchmode ${INNAME}.tex )
file( RENAME "${INNAME}.pdf" "../${OUTNAME}.pdf" )
