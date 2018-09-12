# this is where there will be build instructions
# https://github.com/cse/btwxt

cmake_minimum_required(VERSION 2.8)
#cmake_minimum_required(VERSION 3.10)

project(btwxt)

option( BUILD_BTWXT_TESTING "Build Btwxt testing targets" OFF )

if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  if (UNIX)
    add_definitions("-fPIC")
  endif()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -Wextra")
endif()

set (CMAKE_MODULE_PATH "${btwxt_SOURCE_DIR}/cmake")


add_subdirectory(src)
add_subdirectory(vendor)


if (BUILD_BTWXT_TESTING)
  add_subdirectory(test)
endif()