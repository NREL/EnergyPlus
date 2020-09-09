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

Project(lpsolve)

include_directories(.)

set(LPSOLVE_SRC
	colamd.c
	commonlib.c
	fortify.c
	ini.c
	lp_crash.c
	lp_Hash.c
	lp_lib.c
	lp_LUSOL.c
	lp_matrix.c
	lp_MDO.c
	lp_mipbb.c
	lp_MPS.c
	lp_params.c
	lp_presolve.c
	lp_price.c
	lp_pricePSE.c
	lp_report.c
	lp_rlp.c
	lp_scale.c
	lp_simplex.c
	lp_solve.c
	lp_SOS.c
	lp_utils.c
	lp_wlp.c
	lusol.c
	mmio.c
	myblas.c
	yacc_read.c)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(MSVC)
	add_compile_options(/W4 /MP)
	foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
		set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
	endforeach()
    add_compile_definitions( LPWINAPP  _CONSOLE MSDOS CHECK_SOLUTION _SCL_SECURE_NO_DEPRECATE)
	add_compile_definitions( YY_NEVER_INTERACTIVE PARSER_LP INVERSE_ACTIVE=INVERSE_LUSOL )
	add_compile_definitions( RoleIsExternalInvEngine _CRT_SECURE_NO_DEPRECATE _CRT_NONSTDC_NO_DEPRECATE )
	foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
		set(${flag_var} "${${flag_var}} /D_DEBUG /DFPUexception /DNOFORTIFY /DLP_MAXLINELEN=0" CACHE STRING "compile flags" FORCE)
	endforeach()
else()
	if (APPLE)
		add_compile_options(-arch x86_64 -fno-common)
	else()
		add_definitions(-D__64BIT__)
	endif()
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11" )
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall WX_PRECOMP CHECK_SOLUTION YY_NEVER_INTERACTIVE PARSER_LP
		INVERSE_ACTIVE=INVERSE_LUSOL RoleIsExternalInvEngine")
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		add_compile_definitions(_DEBUG)
	else()
		SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -DNDEBUG" )
	endif()
endif()


####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

add_library(lpsolve STATIC ${LPSOLVE_SRC})
set_target_properties(lpsolve
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)
if (MSVC)
	set_target_properties(lpsolve
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

