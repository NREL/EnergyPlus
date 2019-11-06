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


#include_directories(. ../splinter)

set(SHARED_SRC
#	lsqfit.cpp
#	lib_fuel_cell.cpp
#	lib_fuel_cell_dispatch.cpp
#	lib_battery.cpp
#	lib_battery_dispatch.cpp
#	lib_battery_powerflow.cpp
#	lib_cec6par.cpp
#	lib_financial.cpp
#	lib_geothermal.cpp
#	lib_iec61853.cpp
	lib_irradproc.cpp
#	lib_miniz.cpp
#	lib_physics.cpp
#	lib_powerblock.cpp
#	lib_power_electronics.cpp
#	lib_pvinv.cpp
	lib_pv_incidence_modifier.cpp
#	lib_pv_io_manager.cpp
#	lib_pvmodel.cpp
	lib_pvshade.cpp
#	lib_pv_shade_loss_mpp.cpp
	lib_pvwatts.cpp
#	lib_sandia.cpp
#	lib_shared_inverter.cpp
#	lib_snowmodel.cpp
#	lib_time.cpp
	lib_util.cpp
#	lib_utility_rate.cpp
	lib_weatherfile.cpp)
#	lib_windfile.cpp
#	lib_wind_obos.cpp
#	lib_wind_obos_cable_vessel.cpp
#	lib_windwakemodel.cpp
#	lib_windwatts.cpp
#	lib_mlmodel.cpp
#	lib_ondinv.cpp)


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
