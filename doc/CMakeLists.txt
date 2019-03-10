# the only thing that really needs to be set outside of here is the XELATEX variable, the path to the xelatex compiler

# the root doc build directory will be created at configure time, but lets not copy anything over until build time.
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-build)

# in addition, let's create a folder to drop ONLY PDFs in once they are built, this will allow easy upload to s3
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/doc-pdf)
file(COPY ${CMAKE_SOURCE_DIR}/doc/index.html DESTINATION ${CMAKE_BINARY_DIR}/doc-pdf)

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

