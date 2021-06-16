
INCLUDE_DIRECTORIES( "${PROJECT_SOURCE_DIR}/third_party/Expat/lib")
INCLUDE_DIRECTORIES( "${PROJECT_SOURCE_DIR}/third_party/")
INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/third_party/zlib )

SET(SRC
  parser.h
  parser.c
  fmumini.c
  fmumini.h
  stack.c
  stack.h
  util.c
  util.h
  xml_parser_cosim.c
  xml_parser_cosim.h
  fmiFunctions.h
  fmiModelTypes.h
)

#ADD_LIBRARY( parser SHARED ${SRC} )

IF (UNIX)
  ADD_DEFINITIONS("-fPIC")
ENDIF()

ADD_EXECUTABLE( parser ${SRC} )

TARGET_LINK_LIBRARIES( parser epexpat miniziplib )

INSTALL( TARGETS parser
  RUNTIME DESTINATION PreProcess/FMUParser
  LIBRARY DESTINATION ./
  ARCHIVE DESTINATION ./
)

if( UNIX )
set_target_properties(
  parser
  PROPERTIES
  INSTALL_NAME_DIR "@executable_path"
)
endif()

if(BUILD_TESTING)
  # Avoid creating some artifacts (ShadingController/modelDescription.xml and ShadingController/binaries inside "${PROJECT_SOURCE_DIR}/datasets/FMUs/"
  configure_file("${PROJECT_SOURCE_DIR}/datasets/FMUs/ShadingController.fmu" "${CMAKE_CURRENT_BINARY_DIR}/ShadingController.fmu" COPYONLY)
  add_test(NAME "FMUParser"
    COMMAND parser "--printidf" "${CMAKE_CURRENT_BINARY_DIR}/ShadingController.fmu"
  )
endif()
