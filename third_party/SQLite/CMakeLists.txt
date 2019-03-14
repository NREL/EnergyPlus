
IF ( MSVC ) # visual c++ (VS 2013)
    ADD_DEFINITIONS("-W0")
    ADD_CXX_DEBUG_DEFINITIONS("/fp:fast") # override fp:strict in debug builds

ELSEIF ( CMAKE_COMPILER_IS_GNUCXX OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" ) # g++/Clang
    ADD_DEFINITIONS("-w")
ELSEIF ( WIN32 AND "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel" )
    ADD_DEFINITIONS("/W0")
ELSEIF ( UNIX AND "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel" )
    ADD_DEFINITIONS("-w")
ENDIF () # COMPILER TYPE

SET(SRC 
  sqlite3.h
  sqlite3.c
) 

if (UNIX)
  ADD_DEFINITIONS( " -fPIC " )
endif()

ADD_LIBRARY( sqlite ${SRC} )

