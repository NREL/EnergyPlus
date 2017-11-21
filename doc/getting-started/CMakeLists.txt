
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${CMAKE_SOURCE_DIR}/doc/acknowledgements/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${CMAKE_SOURCE_DIR}/doc/header.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/getting-started.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/title.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/energyplus-overview.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/energyplus-overview/what-is-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/energyplus-overview/why-does-energyplus-exist-and-what-were-its.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/energyplus-overview/energyplus-documentation-library.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/getting-started-with-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/running-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/running-energyplus/introduction.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/running-energyplus/ep-launch-program.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/running-energyplus/energyplus-file-extensions.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/tutorial-example-for-running-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/tutorial-example-for-running-energyplus/running-energyplus-building-envelope-internal.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/tutorial-example-for-running-energyplus/instructions.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/overall-scheme-methodology-for-running.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/overall-scheme-methodology-for-running/building-simulation.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/overall-scheme-methodology-for-running/a-methodology-for-using-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/tutorial-exercise-2.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/tutorial-exercise-2/unitary-system-and-vav-using-hvactemplate.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/tutorial-exercise-2/instructions-001.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/idf-editor-brief-introduction.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/hvactemplate-objects.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/data-sets.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/slab-and-basement-programs.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/coefficient-curve-generation.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/useful-programs.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/weatherdata.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/results-processing.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/hvac-diagram.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/csvproc.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/convertesomtr.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/datafiles.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/other-useful-programs-information/library-files.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/energy-meters.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/energy-meters/standard-energy-meters.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/energy-meters/custom-meters.tex
  ${CMAKE_SOURCE_DIR}/doc/getting-started/src/standard-energyplus-units.tex
  ${CMAKE_BINARY_DIR}/doc-build/title.tex
)

# to get the list of inluded images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image001.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image002.jpg
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image004.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image005.jpg
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image006.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image007.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image008.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image009.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image010.jpg
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image011.jpg
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image012.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image013.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image014.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image015.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image016.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image017.jpg
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image018.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image019.jpg
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image020.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image021.jpg
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image022.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image023.png
  ${CMAKE_SOURCE_DIR}/doc/getting-started/media/image024.jpg
)

SET( SOURCE_FILENAME "getting-started" )
SET( OUTPUT_FILENAME "GettingStarted" )

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR} -DTEX_INTERACTION=${TEX_INTERACTION} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME} ALL
                   DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
                 )
