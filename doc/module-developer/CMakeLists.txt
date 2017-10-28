
# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${CMAKE_SOURCE_DIR}/doc/acknowledgments/|g' | sed 's/$/.tex/g'
SET( INCLUDED_TEX
  ${CMAKE_SOURCE_DIR}/doc/header.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/module-developer.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/guide-for-module-developers.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/introduction.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/modules-in-energyplus.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/modules-in-energyplus/what-is-a-module-anyway.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/modules-in-energyplus/what-is-a-module-developer.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/modules-in-energyplus/adding-new-features.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/input-concepts.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/input-concepts/input-data-dictionary.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/input-concepts/data-dictionary-naming-conventions.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/input-concepts/input-data-file.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/input-concepts/input-considerations.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/input-concepts/advanced-input-considerations.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/input-concepts/datasets.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/module-structure.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/module-structure/module-outline.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/module-structure/module-example.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/module-structure/changing-existing-code.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/module-structure/considerations-for-legacy-codes.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/module-structure/code-readability-vs-speed-of-execution.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/module-structure/how-it-fits-together.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-calling-structure.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-calling-structure/top-level-calling-tree.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-calling-structure/high-level-hvac-calling-tree-schematic-not.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-calling-structure/air-system-calling-tree-schematic-not-all.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-calling-structure/plant-calling-tree-schematic-not-all-routines.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-calling-structure/zone-equipment-calling-tree-schematic-not-all.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-calling-structure/inserting-the-new-module-into-the-program.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/hvac-network.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/hvac-network/branches-connectors-and-nodes.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/hvac-network/nodes-in-the-simulation.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/hvac-network/getting-nodes.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/hvac-network/data-flow-in-an-hvac-component-module.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/hvac-network/interfacing-with-plant.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/hvac-network/node-mass-flow-variables.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/utility-routines-functions.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/input-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/object-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/branch-node-checking-and-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/schedule-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/data-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/other-useful-utilities.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/error-messages.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/display-strings.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/performance-curve-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/fluid-property-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/weather-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/global-data-flags-and-parameters.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/psychrometric-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/tabular-output-utilities.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energyplus-services/internal-zone-gains-services.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/output.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/output/how-do-i-output-my-variables.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/output/output-variable-dos-and-don-ts.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energy-management-system.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/energy-management-system/ems-actuator-interface.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/running-testing-energyplus-for-developers.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/running-testing-energyplus-for-developers/environment-variables-to-assist-running.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/running-testing-energyplus-for-developers/air-loop-simulation-environment-variables.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/quick-procedure-outline-for-making-code.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/important-rules-for-module-developers.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/appendix-a-dataglobals-and-dataenvironments.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/appendix-a-dataglobals-and-dataenvironments/dataglobals.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/appendix-a-dataglobals-and-dataenvironments/dataenvironment.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/appendix-b-submissions-and-check-ins.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/appendix-c-documentation-specifics.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/appendix-d-module-subroutine-function.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/appendix-e-test-file-documentation.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/appendix-f-new-or-changed-proposal-feature.tex
  ${CMAKE_SOURCE_DIR}/doc/module-developer/src/appendix-g.tex
  ${CMAKE_BINARY_DIR}/doc-build/title.tex
)

# to get the list of included images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
SET( INCLUDED_IMAGES
  ${CMAKE_SOURCE_DIR}/doc/module-developer/media/image001.png
  ${CMAKE_SOURCE_DIR}/doc/module-developer/media/image002.png
  ${CMAKE_SOURCE_DIR}/doc/module-developer/media/image003.jpg
)

SET( SOURCE_FILENAME "module-developer" )
SET( OUTPUT_FILENAME "ModuleDeveloper" )

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
	COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DINNAME=${SOURCE_FILENAME} -DOUTNAME=${OUTPUT_FILENAME} -DORIGINAL_CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} -DORIGINAL_CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
		    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${SOURCE_FILENAME}
		    DEPENDS ${INCLUDED_TEX} ${INCLUDED_IMAGES}
                 )
add_custom_target( zPDF_${OUTPUT_FILENAME} ALL
                   DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${OUTPUT_FILENAME}.pdf
                 )
