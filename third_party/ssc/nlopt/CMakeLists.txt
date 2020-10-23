#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

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

set_default_compile_options(nlopt)
set_no_warnings(nlopt)
if(MSVC)
    set_additional_compile_options(nlopt "/D__WXMSW__ /D_MBCS")
else(MSVC)
    set_additional_compile_options(nlopt "-Wall")
endif(MSVC)


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()

