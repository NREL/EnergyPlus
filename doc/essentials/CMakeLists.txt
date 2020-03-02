
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${PROJECT_SOURCE_DIR}/doc/acknowledgments/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${PROJECT_SOURCE_DIR}/doc/header.tex
  ${PROJECT_SOURCE_DIR}/doc/essentials/essentials.tex
  ${PROJECT_SOURCE_DIR}/doc/title.tex
)

# to get the list of included images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/coordinatesystem.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/energyplusfolder.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/ep.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/eplaunch2.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/eplaunch3.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/idfeditor.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/IDFVersionUpdater.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/kiva-2d-elements.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/NodeDiagram.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/tariff-charges.png
  ${PROJECT_SOURCE_DIR}/doc/essentials/media/window_frame_and_divider.png
)

SET( SOURCE_FILENAME "essentials" )
SET( OUTPUT_FILENAME "EnergyPlusEssentials" )

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
