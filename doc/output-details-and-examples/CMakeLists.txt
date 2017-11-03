
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${CMAKE_SOURCE_DIR}/doc/acknowledgments/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${CMAKE_SOURCE_DIR}/doc/header.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/output-details-and-examples.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-details-and-examples.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/introduction.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/output-file-list.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-audit.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-bnd.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-dbg.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-dxf.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-edd.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-eio.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusssz-lt-ext-gt.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/epluszsz-lt-ext-gt.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-lt-ext-gt.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusmtr-lt-ext-gt.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusmap-lt-ext-gt.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-dfs.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusscreen-csv.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplustbl-lt-ext-gt.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/readvars-rvaudit.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-svg.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-sci.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-wrl.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/delight-output-files.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/example-input-files.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/data-sets.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/data-sets/simple-list-data-sets.tex
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/src/data-sets/macro-data-sets.tex
  ${CMAKE_BINARY_DIR}/doc-build/title.tex
)

# to get the list of included images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image001.jpg
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image002.jpg
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image017.png
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image018.jpg
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image019.png
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image020.png
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image021.png
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image022.jpg
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image023.jpg
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image024.jpg
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image025.jpg
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image026.jpg
  ${CMAKE_SOURCE_DIR}/doc/output-details-and-examples/media/image027.jpg
)

SET( SOURCE_FILENAME "output-details-and-examples" )
SET( OUTPUT_FILENAME "OutputDetailsAndExamples" )

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME} ALL
                   DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
                 )
