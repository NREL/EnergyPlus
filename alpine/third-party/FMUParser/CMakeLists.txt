add_subdirectory( "${CMAKE_CURRENT_SOURCE_DIR}/../Expat" ${CMAKE_CURRENT_BINARY_DIR}/Expat )
add_subdirectory( "${CMAKE_CURRENT_SOURCE_DIR}/../zlib" ${CMAKE_CURRENT_BINARY_DIR}/zlib )

INCLUDE_DIRECTORIES( "${CMAKE_CURRENT_SOURCE_DIR}/../Expat/lib")
INCLUDE_DIRECTORIES( "${CMAKE_CURRENT_SOURCE_DIR}/../")
INCLUDE_DIRECTORIES( "${CMAKE_CURRENT_SOURCE_DIR}/../zlib" )

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

ADD_LIBRARY( alpparser ${SRC} )

IF( MSVC )
  ADD_DEFINITIONS( "/w" )
ELSE()
  ADD_DEFINITIONS( "-w" )
ENDIF()

#ADD_EXECUTABLE( parser ${SRC} )

TARGET_LINK_LIBRARIES( alpparser alpexpat alpziplib )

#INSTALL( TARGETS parser
#  RUNTIME DESTINATION PreProcess/FMUParser
#  LIBRARY DESTINATION ./
#  ARCHIVE DESTINATION ./
#)

#if( UNIX )
#set_target_properties(
#  parser
#  PROPERTIES
#  INSTALL_NAME_DIR "@executable_path"
#)
#endif()
