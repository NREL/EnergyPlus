
if(MSVC)
  find_program(WKHTMLTOPDF_EXECUTABLE 
    wkhtmltopdf
    PATHS "C:/Program Files/wkhtmltopdf/bin" "C:/Program Files (x86)/wkhtmltopdf/bin")
else()
  find_program(WKHTMLTOPDF_EXECUTABLE 
    wkhtmltopdf
    )
endif()

# wkhtmltopdf
execute_process(COMMAND ${WKHTMLTOPDF_EXECUTABLE} -V 
  OUTPUT_VARIABLE wkhtmltopdfversionout)

string(REGEX MATCH "([0-9]+\\.?)+" WKHTMLTOPDF_VER "${wkhtmltopdfversionout}")
string(FIND "${wkhtmltopdfversionout}" "with patched qt" WKHTMLTOPDF_PATCHEDQT )


if (WKHTMLTOPDF_VER VERSION_LESS "0.12.2.1")
  message(FATAL_ERROR "wkhtmltopdf version 0.12.2.1 or greater required")
endif()

if (WKHTMLTOPDF_PATCHEDQT LESS "0")
  message(FATAL_ERROR "wkhtmltopdf with special patchedqt build required")
endif()

# pandoc

find_program(PANDOC_EXECUTABLE pandoc)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Products)

execute_process(COMMAND ${PANDOC_EXECUTABLE} -v 
  OUTPUT_VARIABLE pandocversionout)

string(REGEX MATCH "([0-9]+\\.?)+" PANDOC_VER "${pandocversionout}")

if (PANDOC_VER VERSION_LESS "1.13.2")
  message(FATAL_ERROR "pandoc version 1.13.2 or greater required")
endif()



Function(AddMarkdownInput docName tag offset)
  set(output_name ${CMAKE_BINARY_DIR}/docs/${docName}/${docName}${tag}.html)

  set(input_names "")

  foreach(input_name ${ARGN})
    list(APPEND input_names ${CMAKE_CURRENT_SOURCE_DIR}/docs/${docName}/${input_name})
  endforeach()

  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/docs/${docName})

  add_custom_command(OUTPUT ${output_name}
    COMMAND ${PANDOC_EXECUTABLE} ${input_names} --number-offset ${offset}  -o ${output_name} -f markdown_github -s -t html -N -H ${CMAKE_CURRENT_SOURCE_DIR}/docs/header.html  --email-obfuscation=none
    DEPENDS ${input_names} ${CMAKE_CURRENT_SOURCE_DIR}/docs/header.html
    )
EndFunction()


Function(AddPDF docName tag)

  set(output_name ${CMAKE_BINARY_DIR}/built_pdfs/${docName}${tag}.pdf)
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/built_pdfs)

  add_custom_command(OUTPUT ${output_name}
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/docs/${docName}/media ${CMAKE_BINARY_DIR}/docs/${docName}/media
    COMMAND ${WKHTMLTOPDF_EXECUTABLE} -q --window-status "MATHJAXDONE" --outline --outline-depth 3  --enable-smart-shrinking --page-size Letter --load-error-handling abort --load-media-error-handling abort --minimum-font-size 3 --footer-center [page] --no-stop-slow-scripts  toc --xsl-style-sheet ${CMAKE_CURRENT_SOURCE_DIR}/docs/toc.xsl  ${CMAKE_BINARY_DIR}/docs/${docName}/${docName}${tag}.html ${output_name} 
    DEPENDS ${CMAKE_BINARY_DIR}/docs/${docName}/${docName}${tag}.html ${CMAKE_CURRENT_SOURCE_DIR}/docs/toc.xsl
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/docs/${docName}
    )

  add_custom_target(${docName}${tag}_pdf ALL
    DEPENDS ${output_name}
    )

  INSTALL(FILES "${output_name}" DESTINATION "./Documentation")

EndFunction()

AddMarkdownInput(Acknowledgements "" 0 Acknowledgements.md)
AddPDF(Acknowledgements "")

AddMarkdownInput(AuxiliaryPrograms "" 0 AuxiliaryPrograms.md)
AddPDF(AuxiliaryPrograms "")

AddMarkdownInput(EMS_Application_Guide "" 0 EMS_Application_Guide.md)
AddPDF(EMS_Application_Guide "")


# Note: put part2 first because it takes the longest to build, so we don't want things waiting on it
AddMarkdownInput(EngineeringReference "-part2" 6 07-Daylighting.md 08-AirHeatBalance.md 09-BuildingSystemSimulation.md 10-Sizing.md OperationalFaults.md 11-DemandLimiting.md 12-AlternativeModeling.md)
AddPDF(EngineeringReference "-part2")

AddMarkdownInput(EngineeringReference "-part1" 0 01-Overview.md 02-IntegratedSolution.md 03-SurfaceHeatBalance.md 04-AdvancedSurface.md 05-Climate.md 06-SolarRadiation.md )
AddPDF(EngineeringReference "-part1")

AddMarkdownInput(EngineeringReference "-part3" 13 13a-EncyclopaedicRefs.md 13b-EncyclopaedicRefs.md)
AddPDF(EngineeringReference "-part3")

AddMarkdownInput(EngineeringReference "-part4" 16 13c-EncyclopaedicRefs.md 13d-EncyclopaedicRefs.md)
AddPDF(EngineeringReference "-part4")

AddMarkdownInput(EngineeringReference "-part5" 17 13e-EncyclopaedicRefs.md 13f-EncyclopaedicRefs.md 14-OnSiteGeneration.md 15-PerformanceCurves.md 16-Economics.md 17-SpecialModulesReporting.md)
AddPDF(EngineeringReference "-part5")


AddMarkdownInput(ExternalInterfaces_Application_Guide "" 0 ExternalInterfaces_Application_Guide.md)
AddPDF(ExternalInterfaces_Application_Guide "")

AddMarkdownInput(GettingStarted "" 0 GettingStarted.md)
AddPDF(GettingStarted "")

AddMarkdownInput(InputOutputReference "" 0 01a-InputOutputReference.md 01b-InputOutputReference.md 01c-InputOutputReference.md 01d-InputOutputReference.md 02-HVACTemplates.md 03-Economics.md 04-Parametrics.md 05-InputForOutput.md 06-StandardOutputReports.md 07-WeatherData.md 08-RunningEnergyPlus.md 09-Appendix.md)
AddPDF(InputOutputReference "")

AddMarkdownInput(InterfaceDeveloper "" 0 InterfaceDeveloper.md)
AddPDF(InterfaceDeveloper "")

AddMarkdownInput(ModuleDeveloper "" 0 ModuleDeveloper.md)
AddPDF(ModuleDeveloper "")

AddMarkdownInput(OutputDetailsAndExamples "" 0 OutputDetailsAndExamples.md)
AddPDF(OutputDetailsAndExamples "")

AddMarkdownInput(PlantApplicationGuide "" 0 PlantApplicationGuide.md)
AddPDF(PlantApplicationGuide "")

AddMarkdownInput(Tips_and_Tricks_Using_EnergyPlus "" 0 Tips_and_Tricks_Using_EnergyPlus.md)
AddPDF(Tips_and_Tricks_Using_EnergyPlus "")

AddMarkdownInput(Using_EnergyPlus_for_Compliance "" 0 Using_EnergyPlus_for_Compliance.md)
AddPDF(Using_EnergyPlus_for_Compliance "")



