cmake_minimum_required(VERSION 3.10)

project(btwxt)

option( BUILD_BTWXT_TESTING "Build Btwxt testing targets" OFF )

find_package(Git QUIET)

if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  if (UNIX)
    add_definitions("-fPIC")
  endif()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -Wextra")
endif()

add_subdirectory(src)
add_subdirectory(vendor)


if (BUILD_BTWXT_TESTING)
  add_subdirectory(test)
endif()