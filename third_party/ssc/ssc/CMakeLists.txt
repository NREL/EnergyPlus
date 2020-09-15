#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
	set(CMAKE_MACOSX_RPATH 1)
endif()


#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

Project(ssc)

include_directories(. ../splinter ../shared ../nlopt ../lpsolve ../solarpilot ../tcs ../ssc)

set(SSC_SRC
		cmod_6parsolve.cpp
		cmod_annualoutput.cpp
		cmod_battery.cpp
		cmod_battery.h
		cmod_battwatts.cpp
		cmod_battwatts.h
		cmod_belpe.cpp
		cmod_biomass.cpp
		cmod_cashloan.cpp
		cmod_cb_construction_financing.cpp
		cmod_cb_empirical_hce_heat_loss.cpp
		cmod_cb_mspt_system_costs.cpp
		cmod_csp_common_eqns.cpp
		cmod_csp_common_eqns.h
		cmod_csp_tower_eqns.cpp
		cmod_csp_tower_eqns.h
		cmod_dsg_flux_preprocess.cpp
		cmod_equpartflip.cpp
		cmod_financial_eqns.cpp
		cmod_financial_eqns.h
		cmod_fossilgen.cpp
		cmod_fuelcell.cpp
		cmod_fuelcell.h
		cmod_generic_system-builder.cpp
		cmod_generic_system-builder.h
		cmod_generic_system.cpp
		cmod_geothermal.cpp
		cmod_geothermal_costs.cpp
		cmod_grid.cpp
		cmod_grid.h
		cmod_hcpv.cpp
		cmod_host_developer.cpp
		cmod_iec61853par.cpp
		cmod_inv_cec_cg.cpp
		cmod_iph_to_lcoefcr.cpp
		cmod_ippppa.cpp
		cmod_irradproc.cpp
		cmod_iscc_design_point.cpp
		cmod_layoutarea.cpp
		cmod_lcoefcr.cpp
		cmod_levpartflip.cpp
		cmod_linear_fresnel_dsg_iph.cpp
		cmod_merchantplant.cpp
		cmod_merchantplant_eqns.cpp
		cmod_merchantplant_eqns.h
		cmod_mhk_costs.cpp
		cmod_mhk_eqns.cpp
		cmod_mhk_eqns.h
		cmod_mhk_tidal.cpp
		cmod_mhk_wave.cpp
		cmod_poacalib.cpp
		cmod_pv6parmod.cpp
		cmod_pv_get_shade_loss_mpp.cpp
		cmod_pvsamv1.cpp
		cmod_pvsamv1.h
		cmod_pvsamv1_eqns.cpp
		cmod_pvsamv1_eqns.h
		cmod_pvsandiainv.cpp
		cmod_pvwattsv0.cpp
		cmod_pvwattsv1.cpp
		cmod_pvwattsv1_1ts.cpp
		cmod_pvwattsv1_poa.cpp
		cmod_pvwattsv5.cpp
		cmod_pvwattsv7.cpp
		cmod_saleleaseback.cpp
		cmod_sco2_air_cooler.cpp
		cmod_sco2_csp_system.cpp
		cmod_sco2_csp_ud_pc_tables.cpp
		cmod_sco2_design_cycle.cpp
		cmod_sco2_design_point.cpp
		cmod_singlediode.cpp
		cmod_singleowner.cpp
		cmod_snowmodel.cpp
		cmod_solarpilot.cpp
		cmod_swh.cpp
		cmod_tcsdirect_steam.cpp
		cmod_tcsdish.cpp
		cmod_tcsgeneric_solar.cpp
		cmod_tcsiscc.cpp
		cmod_tcslinear_fresnel.cpp
		cmod_tcsmolten_salt.cpp
		cmod_tcsmslf.cpp
		cmod_tcstrough_empirical.cpp
		cmod_tcstrough_physical.cpp
		cmod_tcstrough_physical_csp_solver.cpp
		cmod_test_ud_power_cycle.cpp
		cmod_thermalrate.cpp
		cmod_thirdpartyownership.cpp
		cmod_timeseq.cpp
		cmod_trough_physical.cpp
		cmod_trough_physical_iph.cpp
		cmod_ui_tes_calcs.cpp
		cmod_ui_udpc_checks.cpp
		cmod_user_htf_comparison.cpp
		cmod_utilityrate.cpp
		cmod_utilityrate2.cpp
		cmod_utilityrate3.cpp
		cmod_utilityrate4.cpp
		cmod_utilityrate5.cpp
		cmod_wavefile.cpp
		cmod_utilityrate5_eqns.cpp 
		cmod_utilityrate5_eqns.h
		cmod_wfcheck.cpp
		cmod_wfcsv.cpp
		cmod_wfreader.cpp
		cmod_wind_obos.cpp
		cmod_windbos.cpp
		cmod_windcsm.cpp
		cmod_windfile.cpp
		cmod_windpower.cpp
		cmod_windpower.h
		cmod_windpower_eqns.cpp
		cmod_windpower_eqns.h
		common.cpp
		common.h
		common_financial.cpp
		common_financial.h
		core.cpp
		core.h
		csp_common.cpp
		csp_common.h
		ssc_equations.h
		sscapi.cpp
		sscapi.h
		tckernel.cpp
		tckernel.h
		vartab.cpp
		vartab.h
		)

if(MSVC)	
	set	(SSC_SRC 
		${SSC_SRC}
		vbapi.cpp
		vbapi.h
		)
endif(MSVC)		


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD 11)

if(MSVC)
	add_compile_options(/MP /W4 /w44191 /w44242  /w44266 /w44302 /w44388 /w44826 /w44905 /w44906 /w44928)
    add_compile_definitions( __DLL__ __64BIT__ __WINDOWS__ _CRT_SECURE_NO_WARNINGS __VISUALC__ )
    foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
		set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
	endforeach()
else(MSVC)
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common)
		add_definitions(-D__MACOSX__ -DWX_PRECOMP)
	else ()
		add_definitions(-D__UNIX__)
	endif()
	add_definitions(-D__64BIT__)
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-unknown-pragmas -Wno-deprecated -std=c++11" )
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		add_compile_definitions(_DEBUG)
	else()
		SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3" )
	endif()
endif(MSVC)

#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

add_library(ssc SHARED ${SSC_SRC})
set_target_properties(ssc
    PROPERTIES
    PREFIX ""
    DEBUG_POSTFIX "d"
	)


# dltest executable for checking ssc library
if(UNIX AND NOT SAMAPI_EXPORT)
	add_executable(dltest ../build_resources/dltest.c)
	if (NOT APPLE)
		target_link_libraries(dltest -ldl -Wl,--no-as-needed)
	endif()
	add_custom_command(
		TARGET ssc
		POST_BUILD 
		COMMAND dltest $<TARGET_FILE:ssc>)
endif()

# libssc for PySAM
if(UNIX)
	if (SAMAPI_EXPORT)
		set(CMAKE_MACOSX_RPATH 1)
		set_property(TARGET ssc PROPERTY PREFIX "lib")
		set(CMAKE_SHARED_LIBRARY_SUFFIX ".so")
		if (APPLE)
			add_custom_command(
				TARGET ssc
				POST_BUILD
				COMMAND install_name_tool -id @rpath/libssc.so $<TARGET_FILE:ssc>
			)
		endif()
	endif()
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

set( DEPENDENCIES
	solarpilot
	tcs
	nlopt
	shared
	lpsolve
	splinter
	)
foreach( name ${DEPENDENCIES} )
	target_link_libraries(ssc ${name})
endforeach()

if (UNIX)
	target_link_libraries(ssc -lm -ldl -lstdc++)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
	set(SSC_LIB $<TARGET_FILE_DIR:ssc>/$<TARGET_FILE_BASE_NAME:ssc>.lib)
	set(SSC_H ${CMAKE_CURRENT_LIST_DIR}/sscapi.h)

	set(TESTDIR ${CMAKE_CURRENT_BINARY_DIR}/../test/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)

	if(DEFINED ENV{SAMNTDIR})
		add_custom_command(
			TARGET ssc
			POST_BUILD
			COMMAND ${CMAKE_COMMAND}
			ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${SSC_H} $ENV{SAMNTDIR}/deploy/x64
			COMMAND ${CMAKE_COMMAND}
			ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${TESTDIR}
			)
	endif()

	set (SDKTOOLDIR ${CMAKE_CURRENT_BINARY_DIR}/../sdktool/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)
	if (EXISTS ${SDKTOOLDIR})
		add_custom_command(
			TARGET ssc
			POST_BUILD
			COMMAND ${CMAKE_COMMAND}
			ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${SDKTOOLDIR}
			)
	endif()

	set (TCSCONSOLDEDIR ${CMAKE_CURRENT_BINARY_DIR}/../tcsconsole/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)
	if (EXISTS ${TCSCONSOLDEDIR})
		add_custom_command(
			TARGET ssc
			POST_BUILD
			COMMAND ${CMAKE_COMMAND}
			ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${TCSCONSOLDEDIR}>
			)
	endif()

	if (NOT skip_api)
		if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
			set(APIGENDIR ${CMAKE_CURRENT_BINARY_DIR}/../../sam/api/api_autogen/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)
			add_custom_command(
				TARGET ssc
				POST_BUILD
				COMMAND ${CMAKE_COMMAND}
				ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${APIGENDIR}
				)
		else()
			set(APIGENDIR $ENV{SAMNTDIR}/build_msvc/api/api_autogen/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)
			add_custom_command(
				TARGET ssc
				POST_BUILD
				COMMAND ${CMAKE_COMMAND}
				ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} ${APIGENDIR}
				)
		endif()
	endif()

	add_custom_target(build-time-make-directory ALL
		COMMAND ${CMAKE_COMMAND} -E make_directory ${TESTDIR} ${APIGENDIR})
	add_dependencies(ssc build-time-make-directory)

endif()

if(DEFINED ENV{PYSAMDIR})
	if (SAMAPI_EXPORT OR MSVC)
		if (MSVC)
			add_custom_command(
					TARGET ssc
					POST_BUILD
					COMMAND ${CMAKE_COMMAND} -E remove
					$ENV{PYSAMDIR}/files/$<TARGET_FILE_BASE_NAME:ssc>.lib
					$ENV{PYSAMDIR}/files/$<TARGET_FILE_BASE_NAME:ssc>.dll
			)
			set(SSC_LIB $<TARGET_FILE_DIR:ssc>/$<TARGET_FILE_BASE_NAME:ssc>.lib)
		endif()
		add_custom_command(
				TARGET ssc
				POST_BUILD
				COMMAND ${CMAKE_COMMAND}
				ARGS -E copy $<TARGET_FILE:ssc> ${SSC_LIB} $ENV{PYSAMDIR}/files
		)
	endif()
endif()

