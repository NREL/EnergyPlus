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

Project(tcs)

include_directories(. ../nlopt ../shared ../splinter ../lpsolve ../solarpilot)

set(TCS_SRC
		atmospheric_aod.cpp
		co2_compressor_library.cpp
		CO2_properties.cpp
		csp_dispatch.cpp
		csp_radiator.cpp
		csp_solver_core.cpp
		csp_solver_gen_collector_receiver.cpp
		csp_solver_lf_dsg_collector_receiver.cpp
		csp_solver_mono_eq_methods.cpp
		csp_solver_mspt_collector_receiver.cpp
		csp_solver_mspt_receiver.cpp
		csp_solver_mspt_receiver_222.cpp
		csp_solver_pc_gen.cpp
		csp_solver_pc_heat_sink.cpp
		csp_solver_pc_Rankine_indirect_224.cpp
		csp_solver_pc_sco2.cpp
		csp_solver_pc_steam_heat_sink.cpp
		csp_solver_pt_heliostatfield.cpp
		csp_solver_pt_receiver.cpp
		csp_solver_pt_sf_perf_interp.cpp
		csp_solver_stratified_tes.cpp
		csp_solver_tou_block_schedules.cpp
		csp_solver_trough_collector_receiver.cpp
		csp_solver_two_tank_tes.cpp
		csp_solver_util.cpp
		csp_solver_weatherreader.cpp
		csp_system_costs.cpp
		direct_steam_receivers.cpp
		fmin.cpp
		fmin_callbacks.cpp
		heat_exchangers.cpp
		Heliostat_AzElAod.cpp
		interconnect.cpp
		nlopt_callbacks.cpp
		numeric_solvers.cpp
		sam_mw_pt_heliostatfield_csp_solver.cpp
		sam_sco2_recomp_type424.cpp
		sam_type250_input_generator.cpp
		sco2_cycle_components.cpp
		sco2_partialcooling_cycle.cpp
		sco2_pc_csp_int.cpp
		sco2_power_cycle.cpp
		sco2_rec_util.cpp
		sco2_recompression_cycle.cpp
		sco2_test_type401.cpp
		tc_test_type402.cpp
		tcskernel.cpp
		trnsys_weatherreader.cpp
		typelib.cpp
		ud_power_cycle.cpp
		water_properties.cpp
		waterprop.cpp
		weatherreader_csp_solver.cpp
		cavity_calcs.cpp
		datatest.cpp
		direct_steam_receivers.cpp
		htf_props.cpp
		interpolation_routines.cpp
		ngcc_powerblock.cpp
		powerblock.cpp
		pump.cpp
		sam_csp_util.cpp
		sam_dsg_controller_type265.cpp
		sam_iscc_parasitics.cpp
		sam_iscc_powerblock.cpp
		sam_lf_st_pt_type232.cpp
		sam_mw_csp_SumCalcs.cpp
		sam_mw_gen_Type260_csp_solver.cpp
		sam_mw_lf_type261_steam.cpp
		sam_mw_lf_type261_Wnet.cpp
		sam_mw_lf_type262_salt.cpp
		sam_mw_pt_type222_csp_solver.cpp
		sam_mw_pt_type224_csp_solver.cpp
		sam_mw_pt_type228.cpp
		sam_mw_trough_type250.cpp
		sam_mw_trough_type251.cpp
		sam_mw_type234.cpp
		sam_pf_dish_collector_type295.cpp
		sam_pf_dish_engine_type297.cpp
		sam_pf_dish_parasitic_type298.cpp
		sam_pf_dish_receiver_type296.cpp
		sam_trough_model_type805.cpp
		sam_trough_plant_type807.cpp
		sam_trough_storage_type806.cpp
		solarcollector.cpp
		storage_hx.cpp
		sumprod.cpp
		thermocline_tes.cpp
		tou_translator.cpp
		flat_plate_solar_collector.cpp
		atmospheric_aod.h
		cavity_calcs.h
		co2props_nn1.h
		co2_compressor_library.h
		CO2_properties.h
		co2_testing.h
		csp_dispatch.h
		csp_radiator.h
		csp_solver_core.h
		csp_solver_gen_collector_receiver.h
		csp_solver_lf_dsg_collector_receiver.h
		csp_solver_mspt_collector_receiver.h
		csp_solver_mspt_receiver.h
		csp_solver_mspt_receiver_222.h
		csp_solver_pc_gen.h
		csp_solver_pc_heat_sink.h
		csp_solver_pc_Rankine_indirect_224.h
		csp_solver_pc_sco2.h
		csp_solver_pc_steam_heat_sink.h
		csp_solver_pt_heliostatfield.h
		csp_solver_pt_receiver.h
		csp_solver_pt_sf_perf_interp.h
		csp_solver_stratified_tes.h
		csp_solver_tou_block_schedules.h
		csp_solver_trough_collector_receiver.h
		csp_solver_two_tank_tes.h
		csp_solver_util.h
		csp_system_costs.h
		direct_steam_receivers.h
		flat_plate_solar_collector.h
		fmin.h
		fmin_callbacks.h
		heat_exchangers.h
		htf_props.h
		interconnect.h
		interpolation_routines.h
		ngcc_powerblock.h
		nlopt_callbacks.h
		numeric_solvers.h
		powerblock.h
		sam_csp_util.h
		sco2_cycle_components.h
		sco2_cycle_templates.h
		sco2_partialcooling_cycle.h
		sco2_pc_csp_int.h
		sco2_power_cycle.h
		sco2_recompression_cycle.h
		sco2_rec_util.h
		storage_hx.h
		tcskernel.h
		tcstype.h
		tcs_debug.h
		thermocline_tes.h
		ud_power_cycle.h
		user_defined_power_cycle.h
		waterprop.h
		water_properties.h
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
		add_compile_options(-arch x86_64 -fno-common)
  		add_definitions(-DWX_PRECOMP)
	else()
		add_definitions(-D__64BIT__)
	endif()
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11" )
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-unknown-pragmas -Wno-sign-compare -Wno-unused-variable -Wno-uninitialized -Wno-int-to-pointer-cast ")
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

add_library(tcs STATIC ${TCS_SRC})
set_target_properties(tcs
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()
