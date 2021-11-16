#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

include_directories(. ../ssc)

set(SPLINTER_SRC
	bspline.cpp
	bsplinebasis.cpp
	bsplinebasis1d.cpp
	bsplinebuilder.cpp
	datapoint.cpp
	datatable.cpp
	function.cpp
	knots.cpp
	mykroneckerproduct.cpp
	serializer.cpp
	utilities.cpp)


#####################################################################################################################
#
# CMake Targets
#
#####################################################################################################################

add_library(splinter STATIC ${SPLINTER_SRC})
set_target_properties(splinter
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)

set_default_compile_options(splinter)
if(MSVC)
    set_additional_compile_options(splinter "/W2 /wd4267 /wd4244")
endif(MSVC)
set_no_warnings(splinter)


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (MSVC)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
endif()


