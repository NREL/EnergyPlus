#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

cmake_minimum_required(VERSION 2.8)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
endif()


#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

Project(ssc_test)

if (DEFINED ENV{GTDIR})
    set(GTDIR $ENV{GTDIR})
elseif (DEFINED ENV{GTEST})
    set(GTDIR $ENV{GTEST})
endif()

include_directories(${GTDIR}/include ${GTDIR}/googletest/include . input_cases shared_test ssc_test tcs_test
        ../ssc ../tcs ../solarpilot ../shared ../splinter )

file(GLOB SSC_TESTS ssc_test/*.cpp)
file(GLOB SHARED_TESTS shared_test/*.cpp)
file(GLOB TCS_TESTS tcs_test/*.cpp)
file(GLOB INPUTS_SRC input_cases/*.cpp)
set(SSC_SRC ../ssc/core.cpp ../ssc/common.cpp ../ssc/vartab.cpp)
# add files which need to be compiled with Test in order to be tested on Windows
set(SRC_TO_TEST
        ../ssc/cmod_windpower.cpp
        ../ssc/cmod_battery.cpp
        ../ssc/cmod_battwatts.cpp
        ../ssc/cmod_fuelcell.cpp)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

set(CMAKE_CXX_STANDARD 11)

if(MSVC)
    add_compile_options(/MP /W3 /wd4244)
    add_compile_definitions( __64BIT__ __WINDOWS__ _CRT_SECURE_NO_WARNINGS _MBCS )
    foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
        set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
    endforeach()
else()
    if(APPLE)
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -arch x86_64 -Wno-deprecated" )
    else()
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++11 -Wno-strict-aliasing -Wno-deprecated-declarations -Wno-unknown-pragmas -Wno-reorder" )
    endif()
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_DEBUG")
    else()
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -D_NDEBUG" )
    endif()
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-function -Wno-unused-variable -Wno-unused-value")
endif(MSVC)


#####################################################################################################################
#
# CMake Targets
#
#####################################################################################################################

add_executable(Test
        ${SSC_TESTS}
        ${SHARED_TESTS}
        ${TCS_TESTS}
        ${INPUTS_SRC}
        ${SSC_SRC}
        ${SRC_TO_TEST}
        main.cpp)

set_target_properties(Test
        PROPERTIES
        DEBUG_POSTFIX "d"
        PREFIX ""
        )

if (MSVC)
    set_target_properties(Test PROPERTIES
            LINK_FLAGS /SUBSYSTEM:CONSOLE)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

unset(GTEST_LIB CACHE)
unset(GTESTD_LIB CACHE)

if (CMAKE_BUILD_TYPE STREQUAL "Debug" OR MSVC)
find_library( GTESTD_LIB
        NAMES libgtestd.a gtestd.lib
        PATHS ${GTDIR}/build/lib ${GTDIR}/build/lib/Debug)
target_link_libraries(Test debug ${GTESTD_LIB})
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Release" OR MSVC)
    find_library( GTEST_LIB
            NAMES libgtest.a gtest.lib
            PATHS ${GTDIR}/build/lib ${GTDIR}/build/lib/Release)
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
