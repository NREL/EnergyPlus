# Caller needs to set:
  # XELATEX, the path to the xelatex compiler
  # INNAME, the name of the input tex file
  # OUTNAME, the pretty output name for the pdf
# this will get more complicated as we add things like bibtex and makeindex, for now just execute xelatex twice and rename the output
execute_process( COMMAND ${XELATEX} -interaction=nonstopmode ${INNAME}.tex )
execute_process( COMMAND ${XELATEX} -interaction=nonstopmode ${INNAME}.tex )
file( RENAME "${INNAME}.pdf" "../${OUTNAME}.pdf" )
