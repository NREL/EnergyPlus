find_program(WKHTMLTOPDF_EXECUTABLE wkhtmltopdf)


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

Function(AddMarkdownInput docName fileName offset)
  set(input_name ${CMAKE_CURRENT_SOURCE_DIR}/docs/${docName}/${fileName})
  set(output_name ${CMAKE_BINARY_DIR}/docs/${docName}/${fileName}.html)
  set(${docName}_Inputs ${${docName}_Inputs} ${output_name} PARENT_SCOPE)

  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/docs/${docName})

  add_custom_command(OUTPUT ${output_name}
    COMMAND ${PANDOC_EXECUTABLE} ${input_name} -o ${output_name} -f markdown_github -s -t html -N -H ${CMAKE_CURRENT_SOURCE_DIR}/docs/header.html --number-offset=${offset} --email-obfuscation=none --mathjax
    DEPENDS ${input_name} ${CMAKE_CURRENT_SOURCE_DIR}/docs/header.html
    )
EndFunction()

Function(AddPDF docName timeout smart_shrink )

  set(output_name ${CMAKE_BINARY_DIR}/built_pdfs/${docName}.pdf)
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/built_pdfs)

  if (${smart_shrink})
    set(shrinking_options --enable-smart-shrinking)
  else()
    set(shrinking_options --disable-smart-shrinking --zoom ${ARGV3})
  endif()


  add_custom_command(OUTPUT ${output_name}
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/docs/${docName}/media ${CMAKE_BINARY_DIR}/docs/${docName}/media
    COMMAND ${WKHTMLTOPDF_EXECUTABLE} ${shrinking_options} --page-size Letter --load-error-handling abort --load-media-error-handling abort --minimum-font-size 3 --footer-center [page] --no-stop-slow-scripts --javascript-delay ${timeout} --disable-external-links --outline --outline-depth 3 toc ${${docName}_Inputs} ${output_name} 
    DEPENDS ${${docName}_Inputs}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/docs/${docName}
    )

  add_custom_target(${docName}_pdf ALL
    DEPENDS ${output_name}
    )

  INSTALL(FILES "${output_name}" DESTINATION "./Documentation")

EndFunction()

AddMarkdownInput(Acknowledgements Acknowledgements.md 0)
AddPDF(Acknowledgements 10000 true)

AddMarkdownInput(AuxiliaryPrograms AuxiliaryPrograms.md 0)
AddPDF(AuxiliaryPrograms 10000 true)

AddMarkdownInput(EMS_Application_Guide EMS_Application_Guide.md 0)
AddPDF(EMS_Application_Guide 10000 true)

AddMarkdownInput(EngineeringReference 01-Overview.md 0)
AddMarkdownInput(EngineeringReference 02-IntegratedSolution.md 1)
AddMarkdownInput(EngineeringReference 03-SurfaceHeatBalance.md 2)
AddMarkdownInput(EngineeringReference 04-AdvancedSurface.md 3)
AddMarkdownInput(EngineeringReference 05-Climate.md 4)
AddMarkdownInput(EngineeringReference 06-SolarRadiation.md 5)
AddMarkdownInput(EngineeringReference 07-Daylighting.md 6)
AddMarkdownInput(EngineeringReference 08-AirHeatBalance.md 7)
AddMarkdownInput(EngineeringReference 09-BuildingSystemSimulation.md 8)
AddMarkdownInput(EngineeringReference 10-Sizing.md 9)
AddMarkdownInput(EngineeringReference OperationalFaults.md 10)
AddMarkdownInput(EngineeringReference 11-DemandLimiting.md 11)
AddMarkdownInput(EngineeringReference 12-AlternativeModeling.md 12)
AddMarkdownInput(EngineeringReference 13-EncyclopaedicRefs.md 13)
AddMarkdownInput(EngineeringReference 13a-EncyclopaedicRefs.md 14)
AddMarkdownInput(EngineeringReference 13b-EncyclopaedicRefs.md 15)
AddMarkdownInput(EngineeringReference 13c-EncyclopaedicRefs.md 16)
AddMarkdownInput(EngineeringReference 13d-EncyclopaedicRefs.md 17)
AddMarkdownInput(EngineeringReference 13e-EncyclopaedicRefs.md 18)
AddMarkdownInput(EngineeringReference 13f-EncyclopaedicRefs.md 19)
AddMarkdownInput(EngineeringReference 14-OnSiteGeneration.md 20)
AddMarkdownInput(EngineeringReference 15-PerformanceCurves.md 21)
AddMarkdownInput(EngineeringReference 16-Economics.md 22)
AddMarkdownInput(EngineeringReference 17-SpecialModulesReporting.md 23)
AddPDF(EngineeringReference 1800000 false .68)

AddMarkdownInput(ExternalInterfaces_Application_Guide ExternalInterfaces_Application_Guide.md 0)
AddPDF(ExternalInterfaces_Application_Guide 10000 true)

AddMarkdownInput(GettingStarted GettingStarted.md 0)
AddPDF(GettingStarted 10000 true)

AddMarkdownInput(InputOutputReference 01a-InputOutputReference.md 0)
AddMarkdownInput(InputOutputReference 01b-InputOutputReference.md 1)
AddMarkdownInput(InputOutputReference 01c-InputOutputReference.md 2)
AddMarkdownInput(InputOutputReference 01d-InputOutputReference.md 3)
AddMarkdownInput(InputOutputReference 02-HVACTemplates.md 4)
AddMarkdownInput(InputOutputReference 03-Economics.md 5)
AddMarkdownInput(InputOutputReference 04-Parametrics.md 6)
AddMarkdownInput(InputOutputReference 05-InputForOutput.md 7)
AddMarkdownInput(InputOutputReference 06-StandardOutputReports.md 8)
AddMarkdownInput(InputOutputReference 07-WeatherData.md 9)
AddMarkdownInput(InputOutputReference 08-RunningEnergyPlus.md 10)
AddMarkdownInput(InputOutputReference 09-Appendix.md 11)
AddPDF(InputOutputReference 300000 false .68)

AddMarkdownInput(InterfaceDeveloper InterfaceDeveloper.md 0)
AddPDF(InterfaceDeveloper 10000 true)

AddMarkdownInput(ModuleDeveloper ModuleDeveloper.md 0)
AddPDF(ModuleDeveloper 10000 true)

AddMarkdownInput(OutputDetailsAndExamples OutputDetailsAndExamples.md 0)
AddPDF(OutputDetailsAndExamples 10000 true)

AddMarkdownInput(PlantApplicationGuide PlantApplicationGuide.md 0)
AddPDF(PlantApplicationGuide 10000 true)

AddMarkdownInput(Tips_and_Tricks_Using_EnergyPlus Tips_and_Tricks_Using_EnergyPlus.md 0)
AddPDF(Tips_and_Tricks_Using_EnergyPlus 10000 true)

AddMarkdownInput(Using_EnergyPlus_for_Compliance Using_EnergyPlus_for_Compliance.md 0)
AddPDF(Using_EnergyPlus_for_Compliance 10000 true)



