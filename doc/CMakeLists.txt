# the only thing that really needs to be set outside of here is the XELATEX variable, the path to the xelatex compiler

execute_process( COMMAND cmake -E copy_directory ${CMAKE_SOURCE_DIR}/doc ${CMAKE_BINARY_DIR}/doc-build)

set( DOCS
     acknowledgements:Acknowledgements
     auxiliary-programs:AuxiliaryPrograms
     ems-application-guide:EMS_Application_Guide
     engineering-reference:EngineeringReference
     external-interfaces-application-guide:ExternalInterfaces_Application_Guide
     getting-started:GettingStarted
     input-output-reference:InputOutputReference
     interface-developer:InterfaceDeveloper
     module-developer:ModuleDeveloper
     output-details-and-examples:OutputDetailsAndExamples
     plant-application-guide:PlantApplicationGuide
     tips-and-tricks-using-energyplus:Tips_and_Tricks_Using_EnergyPlus
     using-energyplus-for-compliance:Using_EnergyPlus_for_Compliance
)

foreach( DOC ${DOCS} )
    string(REPLACE ":" ";" TOKENS ${DOC})
    list(GET TOKENS 0 FOLDER)
    list(GET TOKENS 1 FILENAME)
    add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/doc-build/${FILENAME}.pdf
	                COMMAND ${CMAKE_COMMAND} -DXELATEX=${XELATEX} -DOUTNAME=${FILENAME} -DINNAME=${FOLDER} -P ${CMAKE_SOURCE_DIR}/cmake/BuildDocumentation.cmake
			WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build/${FOLDER}
			DEPENDS ${CMAKE_SOURCE_DIR}/doc/${FOLDER}/${FOLDER}.tex
		      )
    add_custom_target( zPDF_${FILENAME} ALL
	               DEPENDS ${CMAKE_BINARY_DIR}/doc-build/${FILENAME}.pdf
		     )
endforeach()
