
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


Function(AddSplitMarkdownInput docName fileName offset)
  set(input_name ${CMAKE_CURRENT_SOURCE_DIR}/docs/${docName}/${fileName})
  set(output_name ${CMAKE_BINARY_DIR}/docs/${docName}/${fileName}.html)
  set(${docName}_Inputs ${${docName}_Inputs} ${output_name} PARENT_SCOPE)

  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/docs/${docName})

  add_custom_command(OUTPUT ${output_name}
    COMMAND ${PANDOC_EXECUTABLE} ${input_name} -o ${output_name} -f markdown_github -s -t html -N -H ${CMAKE_CURRENT_SOURCE_DIR}/docs/header.html --number-offset=${offset} --email-obfuscation=none
    DEPENDS ${input_name} ${CMAKE_CURRENT_SOURCE_DIR}/docs/header.html
    )
EndFunction()

Function(AddSplitPDF docName zoom )
  set(output_name ${CMAKE_BINARY_DIR}/built_pdfs/${docName}.pdf)
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/built_pdfs)

  set(shrinking_options --disable-smart-shrinking --zoom ${zoom})

  add_custom_command(OUTPUT ${output_name}
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/docs/${docName}/media ${CMAKE_BINARY_DIR}/docs/${docName}/media
    COMMAND ${WKHTMLTOPDF_EXECUTABLE}  --window-status "MATHJAXDONE" --outline --outline-depth 3  ${shrinking_options} --page-size Letter --load-error-handling abort --load-media-error-handling abort --minimum-font-size 3 --footer-center [page] --no-stop-slow-scripts   toc --xsl-style-sheet ${CMAKE_CURRENT_SOURCE_DIR}/docs/toc.xsl  ${${docName}_Inputs} ${output_name} 
    DEPENDS ${${docName}_Inputs} ${CMAKE_CURRENT_SOURCE_DIR}/docs/toc.xsl
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/docs/${docName}
    )

  add_custom_target(${docName}_pdf ALL
    DEPENDS ${output_name}
    )

  INSTALL(FILES "${output_name}" DESTINATION "./Documentation")

EndFunction()


Function(AddMarkdownInput docName tag)
  set(output_name ${CMAKE_BINARY_DIR}/docs/${docName}/${docName}${tag}.html)

  set(input_names "")

  foreach(input_name ${ARGN})
    list(APPEND input_names ${CMAKE_CURRENT_SOURCE_DIR}/docs/${docName}/${input_name})
  endforeach()

  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/docs/${docName})

  add_custom_command(OUTPUT ${output_name}
    COMMAND ${PANDOC_EXECUTABLE} ${input_names} -o ${output_name} -f markdown_github -s -t html -N -H ${CMAKE_CURRENT_SOURCE_DIR}/docs/header.html  --email-obfuscation=none
    DEPENDS ${input_names} ${CMAKE_CURRENT_SOURCE_DIR}/docs/header.html
    )
EndFunction()


Function(AddPDF docName tag)

  set(output_name ${CMAKE_BINARY_DIR}/built_pdfs/${docName}${tag}.pdf)
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/built_pdfs)

  add_custom_command(OUTPUT ${output_name}
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/docs/${docName}/media ${CMAKE_BINARY_DIR}/docs/${docName}/media
    COMMAND ${WKHTMLTOPDF_EXECUTABLE} --window-status "MATHJAXDONE" --outline --outline-depth 3  --enable-smart-shrinking --page-size Letter --load-error-handling abort --load-media-error-handling abort --minimum-font-size 3 --footer-center [page] --no-stop-slow-scripts  toc --xsl-style-sheet ${CMAKE_CURRENT_SOURCE_DIR}/docs/toc.xsl  ${CMAKE_BINARY_DIR}/docs/${docName}/${docName}${tag}.html ${output_name} 
    DEPENDS ${CMAKE_BINARY_DIR}/docs/${docName}/${docName}${tag}.html ${CMAKE_CURRENT_SOURCE_DIR}/docs/toc.xsl
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/docs/${docName}
    )

  add_custom_target(${docName}${tag}_pdf ALL
    DEPENDS ${output_name}
    )

  INSTALL(FILES "${output_name}" DESTINATION "./Documentation")

EndFunction()

AddMarkdownInput(Acknowledgements "" Acknowledgements.md)
AddPDF(Acknowledgements "")

AddMarkdownInput(AuxiliaryPrograms "" AuxiliaryPrograms.md)
AddPDF(AuxiliaryPrograms "")

AddMarkdownInput(EMS_Application_Guide "" EMS_Application_Guide.md)
AddPDF(EMS_Application_Guide "")


#AddSplitMarkdownInput(EngineeringReference 01-Overview.md 0)
#AddSplitMarkdownInput(EngineeringReference 02-IntegratedSolution.md 1)
#AddSplitMarkdownInput(EngineeringReference 03-SurfaceHeatBalance.md 2)
#AddSplitMarkdownInput(EngineeringReference 04-AdvancedSurface.md 3)
#AddSplitMarkdownInput(EngineeringReference 05-Climate.md 4)
#AddSplitMarkdownInput(EngineeringReference 06-SolarRadiation.md 5)
#AddSplitMarkdownInput(EngineeringReference 07-Daylighting.md 6)
#AddSplitMarkdownInput(EngineeringReference 08-AirHeatBalance.md 7)
#AddSplitMarkdownInput(EngineeringReference 09-BuildingSystemSimulation.md 8)
#AddSplitMarkdownInput(EngineeringReference 10-Sizing.md 9)
#AddSplitMarkdownInput(EngineeringReference OperationalFaults.md 10)
#AddSplitMarkdownInput(EngineeringReference 11-DemandLimiting.md 11)
#AddSplitMarkdownInput(EngineeringReference 12-AlternativeModeling.md 12)
#AddSplitMarkdownInput(EngineeringReference 13-EncyclopaedicRefs.md 13)
#AddSplitMarkdownInput(EngineeringReference 13a-EncyclopaedicRefs.md 14)
#AddSplitMarkdownInput(EngineeringReference 13b-EncyclopaedicRefs.md 15)
#AddSplitMarkdownInput(EngineeringReference 13c-EncyclopaedicRefs.md 16)
#AddSplitMarkdownInput(EngineeringReference 13d-EncyclopaedicRefs.md 17)
#AddSplitMarkdownInput(EngineeringReference 13e-EncyclopaedicRefs.md 18)
#AddSplitMarkdownInput(EngineeringReference 13f-EncyclopaedicRefs.md 19)
#AddSplitMarkdownInput(EngineeringReference 14-OnSiteGeneration.md 20)
#AddSplitMarkdownInput(EngineeringReference 15-PerformanceCurves.md 21)
#AddSplitMarkdownInput(EngineeringReference 16-Economics.md 22)
#AddSplitMarkdownInput(EngineeringReference 17-SpecialModulesReporting.md 23)
#AddSplitPDF(EngineeringReference .61)

#AddMarkdownInput(EngineeringReference "" 01-Overview.md 02-IntegratedSolution.md 03-SurfaceHeatBalance.md 04-AdvancedSurface.md 05-Climate.md 06-SolarRadiation.md 07-Daylighting.md 08-AirHeatBalance.md 09-BuildingSystemSimulation.md 10-Sizing.md OperationalFaults.md 11-DemandLimiting.md 12-AlternativeModeling.md 13a-EncyclopaedicRefs.md 13b-EncyclopaedicRefs.md 13c-EncyclopaedicRefs.md 13d-EncyclopaedicRefs.md 13e-EncyclopaedicRefs.md 13f-EncyclopaedicRefs.md 14-OnSiteGeneration.md 15-PerformanceCurves.md 16-Economics.md 17-SpecialModulesReporting.md)
#AddPDF(EngineeringReference "")

AddMarkdownInput(EngineeringReference "-part1" 01-Overview.md 02-IntegratedSolution.md 03-SurfaceHeatBalance.md 04-AdvancedSurface.md 05-Climate.md 06-SolarRadiation.md 07-Daylighting.md 08-AirHeatBalance.md 09-BuildingSystemSimulation.md 10-Sizing.md OperationalFaults.md 11-DemandLimiting.md 12-AlternativeModeling.md)
AddPDF(EngineeringReference "-part1")

AddMarkdownInput(EngineeringReference "-part2" 13a-EncyclopaedicRefs.md 13b-EncyclopaedicRefs.md)
AddPDF(EngineeringReference "-part2")

AddMarkdownInput(EngineeringReference "-part3" 13c-EncyclopaedicRefs.md 13d-EncyclopaedicRefs.md)
AddPDF(EngineeringReference "-part3")

AddMarkdownInput(EngineeringReference "-part4" 13e-EncyclopaedicRefs.md 13f-EncyclopaedicRefs.md 14-OnSiteGeneration.md 15-PerformanceCurves.md 16-Economics.md 17-SpecialModulesReporting.md)
AddPDF(EngineeringReference "-part4")


AddMarkdownInput(ExternalInterfaces_Application_Guide "" ExternalInterfaces_Application_Guide.md)
AddPDF(ExternalInterfaces_Application_Guide "")

AddMarkdownInput(GettingStarted GettingStarted.md "")
AddPDF(GettingStarted "")

AddMarkdownInput(InputOutputReference "" 01a-InputOutputReference.md 01b-InputOutputReference.md 01c-InputOutputReference.md 01d-InputOutputReference.md 02-HVACTemplates.md 03-Economics.md 04-Parametrics.md 05-InputForOutput.md 06-StandardOutputReports.md 07-WeatherData.md 08-RunningEnergyPlus.md 09-Appendix.md)
AddPDF(InputOutputReference "")

AddMarkdownInput(InterfaceDeveloper "" InterfaceDeveloper.md)
AddPDF(InterfaceDeveloper "")

AddMarkdownInput(ModuleDeveloper "" ModuleDeveloper.md)
AddPDF(ModuleDeveloper "")

AddMarkdownInput(OutputDetailsAndExamples "" OutputDetailsAndExamples.md)
AddPDF(OutputDetailsAndExamples "")

AddMarkdownInput(PlantApplicationGuide "" PlantApplicationGuide.md)
AddPDF(PlantApplicationGuide "")

AddMarkdownInput(Tips_and_Tricks_Using_EnergyPlus "" Tips_and_Tricks_Using_EnergyPlus.md)
AddPDF(Tips_and_Tricks_Using_EnergyPlus "")

AddMarkdownInput(Using_EnergyPlus_for_Compliance Using_EnergyPlus_for_Compliance.md)
AddPDF(Using_EnergyPlus_for_Compliance "")



