# Test Executable

include_directories("${btwxt_SOURCE_DIR}/src"
                    "${gtest_SOURCE_DIR}/include")

set(library_sources
  main_test.cpp
  fixtures.hpp
  btwxt_test.cpp
  griddeddata_test.cpp
  gridpoint_test.cpp
  math_test.cpp
  large_test.cpp
  hypercube_test.cpp
)

link_libraries(btwxt gtest gmock)

add_executable(btwxt_tests ${library_sources})

include(GoogleTest)

gtest_discover_tests(btwxt_tests TEST_PREFIX btwxt:)
