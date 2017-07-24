include(ConfigureChecks.cmake)

INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}/lib")

if(MSVC)
  add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif(MSVC)

set(alpexpat_SRCS
lib/xmlparse.c
lib/xmlrole.c
lib/xmltok.c 
lib/xmltok_impl.c 
lib/xmltok_ns.c
)

if(MSVC)
  set(alpexpat_SRCS ${alpexpat_SRCS} lib/libexpat.def)
endif(MSVC)

IF( MSVC )
  ADD_DEFINITIONS( "/w" )
ELSE()
  ADD_DEFINITIONS( "-w -fPIC" )
ENDIF()

add_library(alpexpat STATIC ${alpexpat_SRCS})

set_target_properties(
  alpexpat
  PROPERTIES
  INSTALL_NAME_DIR "@executable_path"
)


