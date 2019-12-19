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

if (POLICY CMP0043) # CMAKE 2.8 compatibility
  cmake_policy(SET CMP0043 OLD)
endif (POLICY CMP0043)

#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

Project(sam_simulation_core)

add_subdirectory(ssc)
add_subdirectory(shared)
