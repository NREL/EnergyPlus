
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${CMAKE_SOURCE_DIR}/doc/acknowledgements/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${CMAKE_SOURCE_DIR}/doc/header.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/interface-developer.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/guide-for-interface-developers.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/introduction.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/introduction/interface-expectations.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/energyplus-install-contents.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/input-overview.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/input-overview/general-input-rules.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/input-overview/input-data-dictionary.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/input-overview/input-data-file.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/input-details.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/input-details/idd-conventions.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/input-details/standard-energyplus-units.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/input-details/energyplus-reports.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/output.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/weather-data.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/running-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/licensing.tex
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/src/appendix-a-simple-idf-file.tex
  ${CMAKE_BINARY_DIR}/doc-build/title.tex
)

# to get the list of inluded images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/media/image001.png
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/media/image002.jpg
  ${CMAKE_SOURCE_DIR}/doc/interface-developer/media/image003.png
)

SET( SOURCE_FILENAME "interface-developer" )
SET( OUTPUT_FILENAME "InterfaceDeveloper" )

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR} -DTEX_INTERACTION=${TEX_INTERACTION} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME} ALL
                   DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
                 )
