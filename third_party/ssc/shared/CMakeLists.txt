#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
endif()


#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

Project(shared)

set(SHARED_SRC
		lib_irradproc.cpp
		lib_irradproc.h
		lib_pv_incidence_modifier.cpp
		lib_pv_incidence_modifier.h
		lib_pvshade.cpp
		lib_pvshade.h
		lib_pvwatts.cpp
		lib_pvwatts.h
		lib_util.cpp
		lib_util.h
		lib_weatherfile.cpp
		lib_weatherfile.h)

#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /bigobj")
	add_compile_options(/MP /W0)
    add_compile_definitions( __64BIT__ __WINDOWS__ _WINDOWS _CRT_SECURE_NO_WARNINGS LPWINAPP )
    foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
		set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
	endforeach()
else(MSVC)
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common -Wno-unused-private-field -Wunused-private-field)
  		add_definitions(-DWX_PRECOMP)
	else()
		SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Werror -Wno-misleading-indentation -Wno-int-in-bool-context" )
		add_definitions(-D__64BIT__)
	endif()
  	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11" )
	set_source_files_properties(${SHARED_SRC} PROPERTIES 
		COMPILE_FLAGS "-Wall -Wno-strict-aliasing -Wno-deprecated-declarations -Wno-unknown-pragmas -Wno-reorder")
	set_source_files_properties(${SHARED_SRC} PROPERTIES 
		COMPILE_FLAGS "-Wno-ignored-attributes -Wno-deprecated")
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		add_compile_definitions(_DEBUG)
	else()
		SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -DNDEBUG" )
	endif()
endif(MSVC)


#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

add_library(shared STATIC ${SHARED_SRC})
set_target_properties(shared
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (UNIX)
	set (CMAKE_SHARED_LINKER_FLAGS "-lm")
endif()
if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()
