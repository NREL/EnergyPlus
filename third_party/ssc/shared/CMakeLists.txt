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


include_directories(. ../splinter)

set(SHARED_SRC
		6par_gamma.h
		6par_jacobian.h
		6par_lu.h
		6par_newton.h
		6par_search.h
		6par_solve.h
		CMakeLists.txt
		DB8_vmpp_impp_uint8_bin.h
		lib_aoi.h
		lib_battery.cpp
		lib_battery.h
		lib_battery_dispatch.cpp
		lib_battery_dispatch.h
		lib_battery_powerflow.cpp
		lib_battery_powerflow.h
		lib_cec6par.cpp
		lib_cec6par.h
		lib_financial.cpp
		lib_financial.h
		lib_fuel_cell.cpp
		lib_fuel_cell.h
		lib_fuel_cell_dispatch.cpp
		lib_fuel_cell_dispatch.h
		lib_geothermal.cpp
		lib_geothermal.h
		lib_iec61853.cpp
		lib_iec61853.h
		lib_irradproc.cpp
		lib_irradproc.h
		lib_miniz.cpp
		lib_miniz.h
		lib_mlmodel.cpp
		lib_mlmodel.h
		lib_ondinv.cpp
		lib_ondinv.h
		lib_physics.cpp
		lib_physics.h
		lib_power_electronics.cpp
		lib_power_electronics.h
		lib_power_electronics_old.h
		lib_powerblock.cpp
		lib_powerblock.h
		lib_pv_incidence_modifier.cpp
		lib_pv_incidence_modifier.h
		lib_pv_io_manager.cpp
		lib_pv_io_manager.h
		lib_pv_shade_loss_mpp.cpp
		lib_pv_shade_loss_mpp.h
		lib_pvinv.cpp
		lib_pvinv.h
		lib_pvmodel.cpp
		lib_pvmodel.h
		lib_pvshade.cpp
		lib_pvshade.h
		lib_pvwatts.cpp
		lib_pvwatts.h
		lib_resilience.cpp
		lib_resilience.h
		lib_sandia.cpp
		lib_sandia.h
		lib_shared_inverter.cpp
		lib_shared_inverter.h
		lib_snowmodel.cpp
		lib_snowmodel.h
		lib_time.cpp
		lib_time.h
		lib_util.cpp
		lib_util.h
		lib_utility_rate.cpp
		lib_utility_rate.h
		lib_weatherfile.cpp
		lib_weatherfile.h
		lib_wind_obos.cpp
		lib_wind_obos.h
		lib_wind_obos_cable_vessel.cpp
		lib_wind_obos_cable_vessel.h
		lib_windfile.cpp
		lib_windfile.h
		lib_windwakemodel.cpp
		lib_windwakemodel.h
		lib_windwatts.cpp
		lib_windwatts.h
		lsqfit.cpp
		lsqfit.h
		wind_obos.lk
		)


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
