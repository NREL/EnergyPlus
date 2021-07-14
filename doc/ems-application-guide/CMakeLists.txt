# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${PROJECT_SOURCE_DIR}/acknowldgements/|g' | sed 's/$/.tex/g'
set(INCLUDED_TEX
    ${PROJECT_SOURCE_DIR}/header.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/ems-application-guide.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/application-guide-for-ems.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/application-guide-for-ems/background.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/application-guide-for-ems/organization.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/energyplus-runtime-language.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/energyplus-runtime-language/statement-keywords.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/energyplus-runtime-language/variables.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/energyplus-runtime-language/expressions.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/energyplus-runtime-language/built-in-functions.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/internal-variables.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/internal-variables/zone-geometry.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/internal-variables/internal-gains.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/internal-variables/hvac-systems.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/internal-variables/on-site-electricity-production.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/internal-variables/sizing.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-sensors.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-sensors/variable-types.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-sensors/schedules.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-actuators.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-actuators/hvac-systems-001.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-actuators/thermal-envelope.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-actuators/air-movement.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-actuators/internal-gains-and-exterior-lights.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-actuators/on-site-electricity-production-001.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-actuators/refrigeration.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-actuators/general.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-actuators/sizing-001.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/begin-new-environment.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/after-new-environment-warmup-is-complete.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/begin-timestep-before-predictor.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/after-predictor-before-hvac-managers.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/after-predictor-after-hvac-managers.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/inside-hvac-system-iteration-loop.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/end-of-zone-timestep-before-reporting.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/end-of-zone-timestep-after-reporting.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/end-of-system-timestep-before-hvac-reporting.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/end-of-system-timestep-after-hvac-reporting.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/end-of-zone-sizing.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/end-of-system-sizing.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/after-component-model-input-has-been-read-in.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-calling-points/user-defined-component-model.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/user-defined-component-models.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/user-defined-component-models/common-characteristics.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/user-defined-component-models/zone-forced-air-unit.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/user-defined-component-models/air-terminal-unit.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/user-defined-component-models/air-coil.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/user-defined-component-models/plant-component.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-1-whole-building-average-zone-air.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-2-traditional-setpoint.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-3-hygro-thermal-window-opening.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-4-halt-program-based-on-constraint.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-5-computed-schedule.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-6-window-shade-control.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-7-constant-volume-purchased-air.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-8-system-sizing-with-discrete-package.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-9-demand-management.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-10-plant-loop-override-control.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-11-performance-curve-result-override.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-12-variable-refrigerant-flow-system.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/ems-examples/example-13-surface-construction-actuator-for.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/debugging-ems-programs.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/debugging-ems-programs/err-file.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/debugging-ems-programs/edd-file.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/debugging-ems-programs/line-trace.tex
    ${PROJECT_SOURCE_DIR}/ems-application-guide/src/debugging-ems-programs/debugging-strategies.tex
    ${PROJECT_SOURCE_DIR}/title.tex)

# to get the list of inluded images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
set(INCLUDED_IMAGES ${PROJECT_SOURCE_DIR}/ems-application-guide/media/image003.jpg ${PROJECT_SOURCE_DIR}/ems-application-guide/media/image004.jpg
                    ${PROJECT_SOURCE_DIR}/ems-application-guide/media/image005.jpg ${PROJECT_SOURCE_DIR}/ems-application-guide/media/image011.jpg)

create_doc_target("ems-application-guide" "EMSApplicationGuide")
