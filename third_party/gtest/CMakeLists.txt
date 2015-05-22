# CMake build script for Google Test.

#option(gtest_disable_pthreads "Disable uses of pthreads in gtest." OFF)

include(cmake/internal_utils.cmake)
config_compiler_and_linker()  # Defined in internal_utils.cmake.

# Where Google Test's .h files can be found.
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  ${CMAKE_CURRENT_SOURCE_DIR})

# Where Google Test's libraries can be found.
link_directories(${CMAKE_BINARY_DIR})

# Defines the gtest & gtest_main libraries.  User tests should link
# with one of them.

# Google Test libraries.  We build them using more strict warnings than what
# are used for other targets, to ensure that gtest can be compiled by a user
# aggressive about warnings.
# However, there is a known issue where a stray warning is being issued in Clang, mute it
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  ADD_CXX_DEFINITIONS("-Wno-missing-field-initializers")
endif ()
cxx_library(gtest "${cxx_strict}" src/gtest-all.cc)
cxx_library(gtest_main "${cxx_strict}" src/gtest_main.cc)
target_link_libraries(gtest_main gtest)

