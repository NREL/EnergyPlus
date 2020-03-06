
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${PROJECT_SOURCE_DIR}/doc/acknowledgments/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${PROJECT_SOURCE_DIR}/doc/header.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/interface-developer.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/guide-for-interface-developers.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/introduction.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/introduction/interface-expectations.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/energyplus-install-contents.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/input-overview.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/input-overview/general-input-rules.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/input-overview/input-data-dictionary.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/input-overview/input-data-file.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/input-details.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/input-details/idd-conventions.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/input-details/standard-energyplus-units.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/input-details/energyplus-reports.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/output.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/weather-data.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/running-energyplus.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/licensing.tex
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/src/appendix-a-simple-idf-file.tex
  ${PROJECT_SOURCE_DIR}/doc/title.tex
)

# to get the list of included images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/media/image001.png
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/media/image002.jpg
  ${PROJECT_SOURCE_DIR}/doc/interface-developer/media/image003.png
)

SET( SOURCE_FILENAME "interface-developer" )
SET( OUTPUT_FILENAME "InterfaceDeveloper" )

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
