#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

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

set_default_compile_options(lpsolve)
set_no_warnings(lpsolve)

set(ADD_CFLAGS "")
if(MSVC)
    set(ADD_CFLAGS "${ADD_CFLAGS} /D_CONSOLE /DMSDOS /DCHECK_SOLUTION /D_SCL_SECURE_NO_DEPRECATE")
    set(ADD_CFLAGS "${ADD_CFLAGS} /DYY_NEVER_INTERACTIVE /DPARSER_LP /DINVERSE_ACTIVE=INVERSE_LUSOL")
    set(ADD_CFLAGS "${ADD_CFLAGS} /DRoleIsExternalInvEngine /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE")
    set(ADD_CFLAGS "${ADD_CFLAGS} /DFPUexception /DNOFORTIFY /D LP_MAXLINELEN=0")
else()
    set(ADD_CFLAGS "${ADD_CFLAGS} -Wall -DWX_PRECOMP -DCHECK_SOLUTION -DYY_NEVER_INTERACTIVE -DPARSER_LP
		-DINVERSE_ACTIVE=INVERSE_LUSOL -DRoleIsExternalInvEngine")
endif()
set_additional_compile_options(lpsolve ${ADD_CFLAGS})


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()

