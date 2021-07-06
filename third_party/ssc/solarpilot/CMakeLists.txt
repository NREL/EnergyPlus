#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

include_directories(. ../nlopt ..)

set(SOLARPILOT_SRC
	fluxsim.cpp
	API_structures.cpp
	Ambient.cpp
	AutoPilot_API.cpp
	Financial.cpp
	Flux.cpp
	Heliostat.cpp
	IOUtil.cpp
	Land.cpp
	LayoutSimulateThread.cpp
	OpticalMesh.cpp
	Receiver.cpp
	STObject.cpp
	STSimulateThread.cpp
	SolarField.cpp
	Toolbox.cpp
	definitions.cpp
	heliodata.cpp
	interop.cpp
	mod_base.cpp
	optimize.cpp
	solpos.cpp
	string_util.cpp)

#####################################################################################################################
#
# CMake Targets
#
#####################################################################################################################

add_library(solarpilot STATIC ${SOLARPILOT_SRC})
set_target_properties(solarpilot
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)

set_default_compile_options(solarpilot)
set_no_warnings(solarpilot)


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()
