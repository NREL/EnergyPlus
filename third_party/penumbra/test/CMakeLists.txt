include_directories("${penumbra_SOURCE_DIR}/src"
  "${penumbra_SOURCE_DIR}/include"
  "${gtest_SOURCE_DIR}/include")

set(test_sources
  penumbra_test.cpp)

link_libraries(penumbra gtest)

add_executable(penumbra_tests ${test_sources})

include(GoogleTest)
gtest_discover_tests(penumbra_tests)