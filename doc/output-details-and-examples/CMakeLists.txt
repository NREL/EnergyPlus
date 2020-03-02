
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${PROJECT_SOURCE_DIR}/doc/acknowledgments/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${PROJECT_SOURCE_DIR}/doc/header.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/output-details-and-examples.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-details-and-examples.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/introduction.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/output-file-list.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-audit.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-bnd.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-dbg.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-dxf.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-edd.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-eio.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusssz-lt-ext-gt.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/epluszsz-lt-ext-gt.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-lt-ext-gt.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusmtr-lt-ext-gt.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusmap-lt-ext-gt.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-dfs.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusscreen-csv.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplustbl-lt-ext-gt.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/readvars-rvaudit.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-svg.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-sci.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/eplusout-wrl.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/output-files/delight-output-files.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/example-input-files.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/data-sets.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/data-sets/simple-list-data-sets.tex
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/src/data-sets/macro-data-sets.tex
  ${PROJECT_SOURCE_DIR}/doc/title.tex
)

# to get the list of included images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image001.jpg
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image002.jpg
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image017.png
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image018.jpg
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image019.png
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image020.png
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image021.png
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image022.jpg
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image023.jpg
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image024.jpg
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image025.jpg
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image026.jpg
  ${PROJECT_SOURCE_DIR}/doc/output-details-and-examples/media/image027.jpg
)

SET( SOURCE_FILENAME "output-details-and-examples" )
SET( OUTPUT_FILENAME "OutputDetailsAndExamples" )

add_custom_command( OUTPUT ${PROJECT_BINARY_DIR}/doc/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${PROJECT_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${PROJECT_BINARY_DIR} -DTEX_INTERACTION=${TEX_INTERACTION} -P ${PROJECT_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/doc/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME}
                   DEPENDS ${PROJECT_BINARY_DIR}/doc/${OUTPUT_FILENAME}.pdf
                 )

# Make top-level 'documentation' target depend on this one
add_dependencies(documentation zPDF_${OUTPUT_FILENAME})
