
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${PROJECT_SOURCE_DIR}/doc/acknowledgments/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${PROJECT_SOURCE_DIR}/doc/header.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/external-interfaces-application-guide.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interfaces-application-guide.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/introduction.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/coupling-energyplus-with-the-building.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/bcvtb-examples.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/coupling-energyplus-with-functional-mock-up.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/fmu-examples.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/src/external-interface-s/exporting-energyplus-as-a-functional-mock-up.tex
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/src/references.tex
  ${PROJECT_SOURCE_DIR}/doc/title.tex
)

# to get the list of included images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image004.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image005.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image006.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image007.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image008.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image016.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image033.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image034.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image035.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image036.png
  ${PROJECT_SOURCE_DIR}/doc/external-interfaces-application-guide/media/image037.png
)

SET( SOURCE_FILENAME "external-interfaces-application-guide" )
SET( OUTPUT_FILENAME "ExternalInterfacesApplicationGuide" )

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
