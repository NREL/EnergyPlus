# this is where there will be build instructions
# https://github.com/cse/btwxt
cmake_minimum_required(VERSION 3.10)

project(btwxt)

option( BUILD_BTWXT_TESTING "Build Btwxt testing targets" OFF )
option( BTWXT_GET_EIGEN "Download Eigen (if not already included in top project)" ON )


set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -Wextra")

set (CMAKE_MODULE_PATH "${btwxt_SOURCE_DIR}/cmake")


add_subdirectory(src)
add_subdirectory(vendor)


if (BUILD_BTWXT_TESTING)
  include(ctest)
  add_subdirectory(test)
endif()