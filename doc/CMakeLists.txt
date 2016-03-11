# the only thing that really needs to be set outside of here is the XELATEX variable, the path to the xelatex compiler

execute_process( COMMAND cmake -E copy_directory ${CMAKE_SOURCE_DIR}/doc ${CMAKE_BINARY_DIR}/doc-build)

add_subdirectory(acknowledgements)
add_subdirectory(auxiliary-programs)
add_subdirectory(ems-application-guide)
add_subdirectory(engineering-reference)
add_subdirectory(external-interfaces-application-guide)
add_subdirectory(getting-started)
add_subdirectory(input-output-reference)
add_subdirectory(interface-developer)
add_subdirectory(module-developer)
add_subdirectory(output-details-and-examples)
add_subdirectory(plant-application-guide)
add_subdirectory(tips-and-tricks-using-energyplus)
add_subdirectory(using-energyplus-for-compliance)
