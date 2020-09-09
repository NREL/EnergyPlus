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

Project(nlopt)

if (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/config.h)
	execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/configure)
	execute_process(COMMAND cp config.h ${CMAKE_CURRENT_SOURCE_DIR})
endif()

include_directories(.)

set(NLOPT_SRC
	auglag.c
	bobyqa.c
	ccsa_quadratic.c
	cdirect.c
	cobyla.c
	crs.c
	deprecated.c
	DIRect.c
	direct_wrap.c
	DIRserial.c
	DIRsubrout.c
	esch.c
	general.c
	hybrid.c
	isres.c
	mlsl.c
	mma.c
	mssubs.c
	mt19937ar.c
	newuoa.c
	nldrmd.c
	optimize.c
	options.c
	plip.c
	plis.c
	pnet.c
	praxis.c
	pssubs.c
	qsort_r.c
	redblack.c
	rescale.c
	sbplx.c
	slsqp.c
	sobolseq.c
	stop.c
	
)

if(MSVC)
	list( APPEND NLOPT_SRC sys/time.cpp timer.c)
else()
	list( APPEND NLOPT_SRC timer.c)
endif()


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
	add_compile_options(/W4 /MP)
    add_compile_definitions( __WXMSW__ __64BIT__ __WINDOWS__ _CRT_SECURE_NO_WARNINGS _MBCS )
    foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
		set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
	endforeach()
else(MSVC)
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common)
		add_definitions(-DWX_PRECOMP)
	else()
		add_definitions(-D__64BIT__)
	endif()
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-deprecated-declarations -std=c++11")
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		add_compile_definitions(_DEBUG)
	else()
		SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -DNDEBUG" )
	endif()
endif(MSVC)


####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

add_library(nlopt STATIC ${NLOPT_SRC})
set_target_properties(nlopt
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)
if (MSVC)
	set_target_properties(nlopt
		PROPERTIES
		LINK_FLAGS /SUBSYSTEM:CONSOLE)
endif()

#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()

