# Test Executable

include_directories("${PROJECT_SOURCE_DIR}/src"
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
  fixtures.cpp)

add_executable(${PROJECT_NAME}_tests ${library_sources})

target_compile_features(${PROJECT_NAME}_tests PRIVATE cxx_std_17)

target_link_libraries(${PROJECT_NAME}_tests ${PROJECT_NAME} gtest gmock)

include(GoogleTest)

gtest_discover_tests(${PROJECT_NAME}_tests TEST_PREFIX ${PROJECT_NAME}:)
