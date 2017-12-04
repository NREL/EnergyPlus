# the only thing that really needs to be set outside of here is the XELATEX variable, the path to the xelatex compiler

# the root doc build directory will be created at configure time, but lets not copy anything over until build time.
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build)

# add a configure rule to the header file though
configure_file(${CMAKE_SOURCE_DIR}/doc/header.tex ${CMAKE_BINARY_DIR}/doc-build/header.tex COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/doc/title.tex.in ${CMAKE_BINARY_DIR}/doc-build/title.tex)
configure_file(${CMAKE_SOURCE_DIR}/doc/index.html ${CMAKE_BINARY_DIR}/doc-build/index.html COPYONLY)

# add each of the documents, they have their own CMakeLists.txt files
add_subdirectory(acknowledgments)
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

# we still need to find wkhtmltopdf and pandoc for building validation reports
if (BUILD_VALIDATION_REPORTS)

  # wkhtmltopdf
  if(MSVC)
    find_program(WKHTMLTOPDF_EXECUTABLE
      wkhtmltopdf
      PATHS "C:/Program Files/wkhtmltopdf/bin" "C:/Program Files (x86)/wkhtmltopdf/bin")
  else()
    find_program(WKHTMLTOPDF_EXECUTABLE
      wkhtmltopdf
      )
  endif()
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

endif()
