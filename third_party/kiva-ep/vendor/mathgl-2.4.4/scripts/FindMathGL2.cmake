# - FindMathGL2.cmake
# This module can be used to find MathGL v.2.* and several of its optional components.
#
# You can specify one or more component as you call this find module.
# Possible components are: FLTK, GLUT, Qt, WX.
#
# The following variables will be defined for your use:
#
#  MATHGL2_FOUND           = MathGL v.2 and all specified components found
#  MATHGL2_INCLUDE_DIRS    = The MathGL v.2 include directories
#  MATHGL2_LIBRARIES       = The libraries to link against to use MathGL v.2
#                           and all specified components
#  MATHGL2_VERSION_STRING  = A human-readable version of the MathGL v.2 (e.g. 2.1)
#  MATHGL2_XXX_FOUND       = Component XXX found (replace XXX with uppercased
#                           component name -- for example, QT or FLTK)
#
# The minimum required version and needed components can be specified using
# the standard find_package()-syntax, here are some examples:
#  find_package(MathGL2 REQUIRED)				- v.2.* (no interfaces), required
#  find_package(MathGL2 REQUIRED Qt)		- v.2.1 + Qt interface, required
#  find_package(MathGL2 2.1 REQUIRED)			- v.2.1 (no interfaces), required
#  find_package(MathGL2 COMPONENTS Qt WX)	- v.2.0 + Qt and WX interfaces, optional
#
# Note, some cmake builds require to write "COMPONENTS" always, like
#  find_package(MathGL2 REQUIRED COMPONENTS Qt)	- v.2.* + Qt interface, required
#
# Typical usage could be something like this:
#   find_package(MathGL REQUIRED FLTK)
#   include_directories(${MATHGL2_INCLUDE_DIRS})
#   add_executable(myexe main.cpp)
#   target_link_libraries(myexe ${MATHGL2_LIBRARIES} ${MATHGL2_FLTK_LIBRARIES})
#

#=============================================================================
# Copyright (c) 2011 Denis Pesotsky <denis@kde.ru>, 2014 Alexey Balakin <mathgl.abalakin@gmail.com>
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file COPYING-CMAKE-MODULES for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================

FIND_PATH(MATHGL2_INCLUDE_DIR
		NAMES mgl2/mgl.h
		DOC "The MathGL2 v.2.* include directory")
FIND_LIBRARY(MATHGL2_LIBRARY
		NAMES mgl
		PATHS ${MATHGL2_LIBRARY_DIR}
		DOC "The MathGL v.2.* include directory")

GET_FILENAME_COMPONENT(MATHGL2_LIBRARY_DIR ${MATHGL2_LIBRARY} PATH)

SET(MATHGL2_LIBRARIES ${MATHGL2_LIBRARY})
SET(MATHGL2_INCLUDE_DIRS ${MATHGL2_INCLUDE_DIR})

IF(MATHGL2_INCLUDE_DIR)
	SET(_CONFIG_FILE_PATH "${MATHGL2_INCLUDE_DIR}/mgl2/define.h")
	SET(_VERSION_ERR "Cannot determine MathGL v.2.* version")
	IF(EXISTS "${_CONFIG_FILE_PATH}")
		FILE(STRINGS "${_CONFIG_FILE_PATH}"
			MATHGL2_VERSION_STRING REGEX "^#define MGL_VER2.*$")
		IF(MATHGL2_VERSION_STRING)
			STRING(REGEX
				REPLACE "#define MGL_VER2" ""
				MATHGL2_VERSION_STRING ${MATHGL2_VERSION_STRING})
			STRING(REGEX
				REPLACE "//.*" ""
				MATHGL2_VERSION_STRING ${MATHGL2_VERSION_STRING})
			STRING(STRIP ${MATHGL2_VERSION_STRING} MATHGL2_VERSION_STRING)
			SET(MATHGL2_VERSION_STRING 2.${MATHGL2_VERSION_STRING})
#			MESSAGE(STATUS "Find MathGL version -- ${MATHGL2_VERSION_STRING}")
		ELSE()
			SET(_ERR_MESSAGE "${_VERSION_ERR}: ${_CONFIG_FILE_PATH} parse error")
		ENDIF()
	ELSE()
		SET(_ERR_MESSAGE "${_VERSION_ERR}: ${_CONFIG_FILE_PATH} not found")
	ENDIF()
	IF(_ERR_MESSAGE)
		UNSET(_ERR_MESSAGE)
		SET(_CONFIG_FILE_PATH "${MATHGL2_INCLUDE_DIR}/mgl2/config.h")
		SET(_VERSION_ERR "Cannot determine MathGL v.2.* version")
		IF(EXISTS "${_CONFIG_FILE_PATH}")
			FILE(STRINGS "${_CONFIG_FILE_PATH}"
				MATHGL2_VERSION_STRING REGEX "^#define MGL_VER2.*$")
			IF(MATHGL2_VERSION_STRING)
				STRING(REGEX
					REPLACE "#define MGL_VER2" ""
					MATHGL2_VERSION_STRING ${MATHGL2_VERSION_STRING})
				STRING(REGEX
					REPLACE "//.*" ""
					MATHGL2_VERSION_STRING ${MATHGL2_VERSION_STRING})
				STRING(STRIP ${MATHGL2_VERSION_STRING} MATHGL2_VERSION_STRING)
				SET(MATHGL2_VERSION_STRING 2.${MATHGL2_VERSION_STRING})
	#			MESSAGE(STATUS "Find MathGL version -- ${MATHGL2_VERSION_STRING}")
			ELSE()
				SET(_ERR_MESSAGE "${_VERSION_ERR}: ${_CONFIG_FILE_PATH} parse error")
			ENDIF()
		ELSE()
			SET(_ERR_MESSAGE "${_VERSION_ERR}: ${_CONFIG_FILE_PATH} not found")
		ENDIF()
	ENDIF(_ERR_MESSAGE)

	if(_ERR_MESSAGE)
		MESSAGE(FATAL_ERROR ${_ERR_MESSAGE})
	endif(_ERR_MESSAGE)
ENDIF()

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(MathGL2
		REQUIRED_VARS MATHGL2_LIBRARY MATHGL2_INCLUDE_DIR
		VERSION_VAR MATHGL2_VERSION_STRING)

FOREACH(_Component ${MathGL2_FIND_COMPONENTS})
	STRING(TOLOWER ${_Component} _component)
	STRING(TOUPPER ${_Component} _COMPONENT)

	SET(MATHGL2_${_Component}_FIND_REQUIRED ${MATHGL2_FIND_REQUIRED})
	SET(MATHGL2_${_Component}_FIND_QUIETLY true)
	if(${_component} STREQUAL "qt4" OR ${_component} STREQUAL "qt5")
		FIND_PATH(MATHGL2_${_COMPONENT}_INCLUDE_DIR
					NAMES mgl2/qt.h
					PATHS ${MATHGL2_INCLUDE_DIR} NO_DEFAULT_PATH)
	else(${_component} STREQUAL "qt4" OR ${_component} STREQUAL "qt5")
		FIND_PATH(MATHGL2_${_COMPONENT}_INCLUDE_DIR
				NAMES mgl2/${_component}.h
				PATHS ${MATHGL2_INCLUDE_DIR} NO_DEFAULT_PATH)
	endif(${_component} STREQUAL "qt4" OR ${_component} STREQUAL "qt5")
	FIND_LIBRARY(MATHGL2_${_COMPONENT}_LIBRARY
				NAMES mgl-${_component}
				PATHS ${MATHGL2_LIBRARY_DIR} NO_DEFAULT_PATH)

	FIND_PACKAGE_HANDLE_STANDARD_ARGS(MATHGL2_${_Component} DEFAULT_MSG
										MATHGL2_${_COMPONENT}_LIBRARY
										MATHGL2_${_COMPONENT}_INCLUDE_DIR)

	IF(MATHGL2_${_COMPONENT}_FOUND)
		SET(MATHGL2_LIBRARIES
			${MATHGL2_LIBRARIES} ${MATHGL2_${_COMPONENT}_LIBRARY})
		SET(MATHGL2_INCLUDE_DIRS
			${MATHGL2_INCLUDE_DIRS} ${MATHGL2_${_COMPONENT}_INCLUDE_DIR})
	ENDIF()

	MARK_AS_ADVANCED(MATHGL2_${_COMPONENT}_INCLUDE_DIR MATHGL2_${_COMPONENT}_LIBRARY)
ENDFOREACH()

MARK_AS_ADVANCED(MATHGL2_INCLUDE_DIR MATHGL2_LIBRARY MATHGL2_VERSION_STRING)
