SET(SRC 
defines.h
utilSocket.c
utilSocket.h
utilXml.c
utilXml.h
) 

IF( WIN32 )
  LIST( APPEND SRC bcvtb.def )
ENDIF()

INCLUDE_DIRECTORIES( "${CMAKE_SOURCE_DIR}/third_party/Expat/lib")

ADD_LIBRARY( bcvtb SHARED ${SRC} )

TARGET_LINK_LIBRARIES( bcvtb epexpat )

IF( WIN32 )
  TARGET_LINK_LIBRARIES( bcvtb wsock32 )
ENDIF()

IF( MSVC )
  ADD_DEFINITIONS( "/w" )
ELSE()
  ADD_DEFINITIONS( "-w" )
ENDIF()

set_target_properties(
  bcvtb
  PROPERTIES
  INSTALL_NAME_DIR "@executable_path"
)

INSTALL( TARGETS bcvtb 
  RUNTIME DESTINATION ./ 
  LIBRARY DESTINATION ./ 
)

