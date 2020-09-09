#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

cmake_minimum_required(VERSION 2.8)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
endif()

if (UNIX AND NOT CMAKE_C_COMPILER)
	set(CMAKE_C_COMPILER gcc)
	set(CMAKE_CXX_COMPILER g++)
endif()
set(CMAKE_CXX_STANDARD 11)


#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

Project(sam_simulation_core)

add_subdirectory(splinter)
add_subdirectory(shared)
add_subdirectory(nlopt)
add_subdirectory(lpsolve)
add_subdirectory(solarpilot)
add_subdirectory(tcs)
add_subdirectory(ssc)

if (NOT skip_tools)
	add_subdirectory(sdktool)
	add_subdirectory(tcsconsole)
endif()

if (NOT skip_tests)
	add_subdirectory(test)
endif()