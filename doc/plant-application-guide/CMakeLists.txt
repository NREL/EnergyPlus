
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${CMAKE_SOURCE_DIR}/doc/acknowledgements/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${CMAKE_SOURCE_DIR}/doc/header.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/plant-application-guide.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/title.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/introduction.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/introduction/organization.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/scope.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/energyplus-nomenclature.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/generating-an-energyplus-line-diagram.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/generating-an-energyplus-line-diagram/example-for-energyplus-line-diagram.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/inputting-the-system-into-the-idf-file.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-01-chiller-and-condenser-loops.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-01-chiller-and-condenser-loops/chilled-water-loop.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-01-chiller-and-condenser-loops/condenser-loop.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-02-thermal-energy-storage.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-02-thermal-energy-storage/primary-cooling-loop-coolsysprimary-chiller.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-02-thermal-energy-storage/condenser-loop-condenser-loop-cooling-tower.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-02-thermal-energy-storage/heating-loop-heatsys1-boiler.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-system-3-primary-secondary-pumping.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-system-3-primary-secondary-pumping/primary-chilled-water-loop-chiller-s.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-system-3-primary-secondary-pumping/secondary-chilled-water-loop-plate-heat.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-system-3-primary-secondary-pumping/primary-secondary-pumping.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/example-system-3-primary-secondary-pumping/condenser-loop-cooling-tower.tex
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/src/references.tex
  ${CMAKE_BINARY_DIR}/doc-build/title.tex
)

# to get the list of inluded images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image001.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image002.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image003.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image004.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image005.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image006.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image007.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image008.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image009.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image010.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image011.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image012.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image013.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image014.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image015.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image016.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image017.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image018.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image019.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image020.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image021.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image022.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image023.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image024.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image025.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image026.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image027.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image028.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image029.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image030.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image031.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image032.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image033.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image034.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image035.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image036.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image037.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image038.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image039.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image040.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image041.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image042.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image043.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image044.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image045.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image046.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image047.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image048.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image049.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image050.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image051.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image052.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image053.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image054.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image055.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image056.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image057.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image058.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image059.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image060.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image061.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image062.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image063.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image064.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image065.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image066.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image067.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image068.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image069.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image070.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image071.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image072.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image073.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image074.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image075.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image076.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image077.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image078.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image079.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image080.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image081.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image082.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image083.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image084.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image085.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image086.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image087.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image088.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image089.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image090.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image091.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image092.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image093.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image094.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image095.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image096.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image097.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image098.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image099.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image100.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image101.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image102.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image103.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image104.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image105.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image106.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image107.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image108.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image109.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image110.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image111.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image112.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image113.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image114.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image115.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image116.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image117.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image118.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image119.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image120.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image121.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image122.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image123.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image124.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image125.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image126.png
  ${CMAKE_SOURCE_DIR}/doc/plant-application-guide/media/image127.png
)

SET( SOURCE_FILENAME "plant-application-guide" )
SET( OUTPUT_FILENAME "PlantApplicationGuide" )

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME} ALL
                   DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
                 )
