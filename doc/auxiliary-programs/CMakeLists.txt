
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${CMAKE_SOURCE_DIR}/doc/acknowledgements/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/../header.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/auxiliary-programs.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/auxiliary-energyplus-programs.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/background.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/weather-format-for-simulation-programs.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/weather-data-availability.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/using-the-weather-converter.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/definitions-file-custom-file-processing.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/reports-files-produced-by-the-weather.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/k-ppen-climate-classification.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/ashrae-climate-classification.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/energyplus-weather-file-epw-data-dictionary.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/epw-csv-format-in-out.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/missing-weather-file-data.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/source-weather-data-formats.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/data-sources-uncertainty.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/advanced-use-accessing-weather-conversion.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/references.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/weather-converter-program/web-resources.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/use-of-the-ground-temperatures-with-slabs.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/slab-configuration-drawing.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/running-the-slab-program.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/technical-details-on-files-for-ground-heat.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/slab-program-output-files.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/description-of-the-objects-in-the-e-slabght.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/using-ground-temperatures-with-basements.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/running-the-basement-program.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/technical-details-on-files-for-ground-heat-001.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/basement-program-output-files.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/the-basement-idd.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/description-of-the-objects-in-the-basementght.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/using-the-interface-surface-temperature.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/ground-heat-transfer-in-energyplus/references-001.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/view-factor-calculation-program.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/view-factor-calculation-program/using-the-view-factor-interface-program.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/view-factor-calculation-program/additional-information.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/using-older-version-input-files-transition.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/using-older-version-input-files-transition/idf-version-updater.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/using-older-version-input-files-transition/transition-console-program-details.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/using-older-version-input-files-transition/idf-version-converter-transition-file-lists.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/using-older-version-input-files-transition/converting-imf-files.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/epdrawgui.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/epdrawgui/main-tab.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/epdrawgui/option-tab.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/input-macros.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/input-macros/introduction-001.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/input-macros/running-the-ep-macro-program.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/input-macros/incorporating-external-files.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/input-macros/selectively-accepting-or-skipping-lines-of.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/input-macros/defining-blocks-of-input.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/input-macros/macro-debugging-and-listing-control.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/input-macros/listing-format.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-diagram.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-diagram/introduction-002.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/coeffconv-coeffcheck.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/coeffconv-coeffcheck/coeffconv.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/coeffconv-coeffcheck/coeffcheck.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/coeffconv-coeffcheck/running-the-coeffconv-or-coeffcheck-programs.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/expandobjects.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/expandobjects/introduction-003.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/expandobjects/hvac-template-objects-processed.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/expandobjects/ground-heat-transfer-objects-processed.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/expandobjects/building-surface-objects-processed.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/csvproc.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/convertesomtr.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/calcsoilsurftemp-program.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/calcsoilsurftemp-program/running-the-program.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/calcsoilsurftemp-program/program-inputs.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/calcsoilsurftemp-program/program-outputs.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-performance-curve-fit-tool.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-performance-curve-fit-tool/introduction-004.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-performance-curve-fit-tool/inputs.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-performance-curve-fit-tool/rated-data.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-performance-curve-fit-tool/performance-data.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-performance-curve-fit-tool/outputs.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-performance-curve-fit-tool/error-message.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-performance-curve-fit-tool/notes.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/hvac-performance-curve-fit-tool/references-002.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/parametric-spreadsheets.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/parametric-spreadsheets/water-to-water-heat-pump-parameter.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/parametric-spreadsheets/water-to-air-heat-pump-parameter-coefficient.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/parametric-spreadsheets/water-to-air-heat-pump-parameter-coefficient-001.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/parametric-spreadsheets/g-function-spreadsheet.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/parametricpreprocessor.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/appgpostprocess-appendix-g-postprocessing.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/blasttranslator.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/doe2translator.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/running-console-applications.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/running-console-applications/modifying-the-batch-file.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/technical-details-of-running-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/technical-details-of-running-energyplus/ep-launch-program.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/technical-details-of-running-energyplus/ep-launch-lite.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/technical-details-of-running-energyplus/energyplus-file-extensions.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/technical-details-of-running-energyplus/runeplus-batch-file.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/technical-details-of-running-energyplus/running-energyplus-by-hand.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/technical-details-of-running-energyplus/runeplus-details.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/technical-details-of-running-energyplus/rundirmulti-batch-file.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/technical-details-of-running-energyplus/energyplus-files.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/creating-input-files.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/creating-input-files/idd-conventions.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/creating-input-files/idfeditor.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/manipulating-output-files.tex
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/src/manipulating-output-files/ep-compare.tex
  ${CMAKE_BINARY_DIR}/doc-build/title.tex
)

# to get the list of inluded images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image001.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image002.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image003.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image006.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image007.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image008.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image009.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image010.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image011.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image012.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image015.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image016.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image018.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image019.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image020.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image021.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image022.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image023.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image024.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image025.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image026.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image027.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image028.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image029.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image030.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image034.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image036.jpg
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image037.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image038.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image039.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image040.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image041.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image041.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image041.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image042.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image043.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image044.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image045.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image046.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image047.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image048.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image049.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image050.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image051.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image052.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image053.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image054.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image055.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image056.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image057.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image058.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image059.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image059.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image060.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image101.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image102.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image103.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image104.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image105.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image106.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image107.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image108.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image109.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image110.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image111.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image112.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image113.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image114.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image115.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image116.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image117.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image118.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image119.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image120.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image121.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image122.png
  ${CMAKE_SOURCE_DIR}/doc/auxiliary-programs/media/image123.png
)

SET( SOURCE_FILENAME "auxiliary-programs" )
SET( OUTPUT_FILENAME "AuxiliaryPrograms" )

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME} ALL
                   DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
                 )
