
INCLUDE_DIRECTORIES( "${CMAKE_SOURCE_DIR}/third_party/Expat/lib")
INCLUDE_DIRECTORIES( "${CMAKE_SOURCE_DIR}/third_party/")
INCLUDE_DIRECTORIES( ${CMAKE_SOURCE_DIR}/third_party/zlib )

SET(SRC
  main.h
  main.c
  fmiFunctions.h
  fmiModelTypes.h
  fmiPlatformTypes.h
  fmumini.c
  fmumini.h
  stack.c
  stack.h
  util.c
  util.h
  xml_parser_cosim.c
  xml_parser_cosim.h
  eplusModelFunctions.h
  fmiModelFunctions.h
  fmiModelTypes.h
)

ADD_LIBRARY( epfmiimport STATIC ${SRC} )

IF( MSVC )
  ADD_DEFINITIONS( "/w" )
ELSE()
  ADD_DEFINITIONS( "-w -fPIC" )
ENDIF()

TARGET_LINK_LIBRARIES( epfmiimport epexpat miniziplib )

if( UNIX )
set_target_properties(
  epfmiimport
  PROPERTIES
  INSTALL_NAME_DIR "@executable_path"
)
endif()
