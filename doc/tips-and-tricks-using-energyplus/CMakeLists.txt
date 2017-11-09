
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${CMAKE_SOURCE_DIR}/doc/acknowledgments/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${CMAKE_BINARY_DIR}/doc-build/title.tex
  ${CMAKE_SOURCE_DIR}/doc/header.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/tips-and-tricks-using-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/tips-tricks-for-using-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/introduction-support.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/introduction-support/organization.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/introduction-support/energyplus-support.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/general.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/general/what-energyplus-is.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/general/what-energyplus-isn-t.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/general/getting-started.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/general/comparing-energyplus-to-other-programs.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/datasets.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/datasets/datasets-aka-libraries.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/datasets/locations-designdays.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/design-day-weather-data.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/design-day-weather-data/design-day-creation.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/design-day-weather-data/epw-weather-files.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/design-day-weather-data/meteonorm-weather-files.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/design-day-weather-data/weather-data-for-simulations.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/design-day-weather-data/weather-file-sources.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/design-day-weather-data/measuring-solar-data.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/input.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/input/creating-files-for-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/input/converting-older-version-energyplus-files.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/input/using-macros-and-editing-inputs-in-idf-editor.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/input/getting-data-from-window-program.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/building-surface-dimensions-inside-outside.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/describing-roof-overhangs.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/solar-reflection-from-shading-surfaces.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/air-wall-open-air-connection-between-zones.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/daylight-modeling.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/rain-flag.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/interzone-exterior-convection.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/modeling-reflective-radiant-barriers.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/cavity-algorithm-model.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/using-multipliers-zone-and-or-window.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/building-geometry-shading-zone-model/using-osc-other-side-coefficients-to-create.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/natural-and-mechanical-ventilation.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/natural-and-mechanical-ventilation/airflownetwork-and-earthtube.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/hvac-sizing-tips.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/variable-refrigerant-flow-air-conditioner.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/modeling-desiccant-dehumidifiers.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/boiler-control-schedule.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/difference-between-eir-and-reformulated-eir.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/using-well-water.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/plant-load-profile.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/hvac-system-turn-off.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/fan-types.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/use-of-set-point-managers.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/hvac-availability-schedules.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/hvac-system-types.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/separating-ventilation-loads-v-zone-loads.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/hvac-sizing-equipment-simulation-and-controls/system-not-cooling.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/schedule-and-availability-manager.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/schedule-and-availability-manager/schedule-and-availability-manager.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/output.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/output/output-does-not-match-epw-values.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/output/schedules-off-by-1-hour.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/output/reporting-options.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/output/output-variables-in-idf-editor.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/output/output-variable-definition.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/output/advanced-output-variable-reporting.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/output/use-of-comma-and-point-in-numeric-output.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/utilities.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/documentation-and-guides.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/errors-and-warnings.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/errors-and-warnings/max-iterations-exceeded.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details/standard-error-message-format.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details/example-error-messages-for-preprocessors.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details/example-error-messages-for-the-input.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details/example-error-messages-from-module-getinput.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details/example-error-messages-during-sizing.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details/recurring-errors.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details/summaries-at-end-of-simulation.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details/psychrometric-errors.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/error-messages-details/error-summary.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/validation-and-testing.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/platforms-and-run-time.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/platforms-and-run-time/reduce-energyplus-run-time.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/platforms-and-run-time/run-energyplus-in-parallel.tex
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/src/platforms-and-run-time/running-energyplus-on-windows-vista-and.tex
)

# to get the list of included images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/media/image001.jpg
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/media/image002.png
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/media/image003.png
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/media/image004.png
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/media/image005.png
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/media/image006.jpg
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/media/image007.png
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/media/image008.png
  ${CMAKE_SOURCE_DIR}/doc/tips-and-tricks-using-energyplus/media/image009.png
)

SET( SOURCE_FILENAME "tips-and-tricks-using-energyplus" )
SET( OUTPUT_FILENAME "TipsAndTricksUsingEnergyPlus" )

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME} ALL
                   DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
                 )
