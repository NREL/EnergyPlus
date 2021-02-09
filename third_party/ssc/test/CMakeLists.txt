#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

if (DEFINED ENV{GTDIR})
    set(GTDIR $ENV{GTDIR})
elseif (DEFINED ENV{GTEST})
    set(GTDIR $ENV{GTEST})
endif()

include_directories(${GTDIR}/include ${GTDIR}/googletest/include . input_cases shared_test ssc_test tcs_test
						../ssc ../tcs ../solarpilot ../shared ../splinter ../jsoncpp/include )

file(GLOB SSC_TESTS ssc_test/*.cpp)
file(GLOB SSC_TESTS_HEADERS ssc_test/*.h)
file(GLOB SHARED_TESTS shared_test/*.cpp)
file(GLOB SHARED_TESTS_HEADERS shared_test/*.h)
file(GLOB TCS_TESTS tcs_test/*.cpp)
file(GLOB TCS_TESTS_HEADERS tcs_test/*.h)
file(GLOB INPUTS_SRC input_cases/*.cpp)
file(GLOB INPUTS_SRC_HEADERS input_cases/*.h)
set(SSC_SRC ../ssc/core.cpp ../ssc/common.cpp ../ssc/vartab.cpp)
# add files which need to be compiled with Test in order to be tested on Windows
set(SRC_TO_TEST
        ../ssc/cmod_windpower.cpp
		../ssc/cmod_utilityrate5.cpp
        ../ssc/cmod_battery.cpp
        ../ssc/cmod_battwatts.cpp
        ../ssc/cmod_fuelcell.cpp)


#####################################################################################################################
#
# CMake Targets
#
#####################################################################################################################

add_executable(Test
        ${SSC_TESTS}
		${SSC_TESTS_HEADERS}
        ${SHARED_TESTS}
		${SHARED_TESTS_HEADERS}
        ${TCS_TESTS}
		${TCS_TESTS_HEADERS}
        ${INPUTS_SRC}
		${INPUTS_SRC_HEADERS}
        ${SSC_SRC}
        ${SRC_TO_TEST}
        main.cpp)

set_target_properties(Test
        PROPERTIES
        DEBUG_POSTFIX "d"
        PREFIX ""
        )

set_default_compile_options(Test)
if(MSVC)
    set_additional_compile_options(Test "/MP /W3 /wd4244 /D_MBCS")
else()
    set_additional_compile_options(Test "-Wno-ignored-attributes -Wno-deprecated-declarations")
endif()

if (MSVC)
    set_target_properties(Test PROPERTIES
            LINK_FLAGS /SUBSYSTEM:CONSOLE)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (CMAKE_BUILD_TYPE STREQUAL "Debug" OR MSVC)
find_library( GTESTD_LIB
        NAMES libgtestd.a gtestd.lib
        PATHS $ENV{GTESTD_DIR} ${GTDIR}/build/lib ${GTDIR}/build/lib/Debug)
target_link_libraries(Test debug ${GTESTD_LIB})
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Release" OR MSVC)
    find_library( GTEST_LIB
            NAMES libgtest.a gtest.lib
            PATHS $ENV{GTEST_DIR} ${GTDIR}/build/lib ${GTDIR}/build/lib/Release)
    target_link_libraries(Test optimized ${GTEST_LIB})
endif()
target_link_libraries(Test ssc splinter)

if (UNIX)
    target_link_libraries(Test -ldl -lpthread)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

if(MSVC)
    set(TEST_DIR ${CMAKE_CURRENT_BINARY_DIR}/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)

	foreach( file_i ${LIBCURL_FILES})
		add_custom_command(
		TARGET Test
		POST_BUILD
		COMMAND ${CMAKE_COMMAND}
		ARGS -E copy $ENV{WEXDIR}/build_resources/libcurl_ssl_x64/bin/${file_i} ${TEST_DIR}
	)
	endforeach( file_i )
endif()
