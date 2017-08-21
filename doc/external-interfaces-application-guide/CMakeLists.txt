
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${CMAKE_SOURCE_DIR}/doc/acknowledgements/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${CMAKE_SOURCE_DIR}/doc/header.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/external-interfaces-application-guide.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interfaces-application-guide.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/introduction.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/coupling-energyplus-with-the-building.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/bcvtb-examples.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/coupling-energyplus-with-functional-mock-up.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/fmu-examples.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/exporting-energyplus-as-a-functional-mock-up.tex
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/src/references.tex
  ${CMAKE_BINARY_DIR}/doc-build/title.tex
)

# to get the list of inluded images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image004.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image005.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image006.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image007.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image008.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image016.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image033.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image034.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image035.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image036.png
  ${CMAKE_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image037.png
)

SET( SOURCE_FILENAME "external-interfaces-application-guide" )
SET( OUTPUT_FILENAME "ExternalInterfacesApplicationGuide" )

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME} ALL
                   DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
                 )
