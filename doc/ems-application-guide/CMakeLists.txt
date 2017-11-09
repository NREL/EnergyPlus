
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${CMAKE_SOURCE_DIR}/doc/acknowldgements/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${CMAKE_SOURCE_DIR}/doc/header.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/ems-application-guide.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/application-guide-for-ems.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/application-guide-for-ems/background.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/application-guide-for-ems/organization.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/energyplus-runtime-language.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/energyplus-runtime-language/statement-keywords.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/energyplus-runtime-language/variables.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/energyplus-runtime-language/expressions.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/energyplus-runtime-language/built-in-functions.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/internal-variables.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/internal-variables/zone-geometry.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/internal-variables/internal-gains.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/internal-variables/hvac-systems.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/internal-variables/on-site-electricity-production.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/internal-variables/sizing.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-sensors.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-sensors/variable-types.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-sensors/schedules.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-actuators.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-actuators/hvac-systems-001.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-actuators/thermal-envelope.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-actuators/air-movement.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-actuators/internal-gains-and-exterior-lights.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-actuators/on-site-electricity-production-001.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-actuators/refrigeration.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-actuators/general.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-actuators/sizing-001.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/begin-new-environment.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/after-new-environment-warmup-is-complete.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/begin-timestep-before-predictor.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/after-predictor-before-hvac-managers.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/after-predictor-after-hvac-managers.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/inside-hvac-system-iteration-loop.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/end-of-zone-timestep-before-reporting.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/end-of-zone-timestep-after-reporting.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/end-of-system-timestep-before-hvac-reporting.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/end-of-system-timestep-after-hvac-reporting.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/end-of-zone-sizing.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/end-of-system-sizing.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/after-component-model-input-has-been-read-in.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-calling-points/user-defined-component-model.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/user-defined-component-models.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/user-defined-component-models/common-characteristics.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/user-defined-component-models/zone-forced-air-unit.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/user-defined-component-models/air-terminal-unit.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/user-defined-component-models/air-coil.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/user-defined-component-models/plant-component.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-1-whole-building-average-zone-air.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-2-traditional-setpoint.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-3-hygro-thermal-window-opening.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-4-halt-program-based-on-constraint.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-5-computed-schedule.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-6-window-shade-control.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-7-constant-volume-purchased-air.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-8-system-sizing-with-discrete-package.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-9-demand-management.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-10-plant-loop-override-control.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-11-performance-curve-result-override.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-12-variable-refrigerant-flow-system.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/ems-examples/example-13-surface-construction-actuator-for.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/debugging-ems-programs.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/debugging-ems-programs/err-file.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/debugging-ems-programs/edd-file.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/debugging-ems-programs/line-trace.tex
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/src/debugging-ems-programs/debugging-strategies.tex
  ${CMAKE_BINARY_DIR}/doc-build/title.tex
)

# to get the list of inluded images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/media/image003.jpg
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/media/image004.jpg
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/media/image005.jpg
  ${CMAKE_SOURCE_DIR}/doc/ems-application-guide/media/image011.jpg
)

SET( SOURCE_FILENAME "ems-application-guide" )
SET( OUTPUT_FILENAME "EMSApplicationGuide" )

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME} ALL
                   DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
                 )
