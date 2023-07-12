# Test Executable
set(library_sources
        test-main.cpp
        fixtures/public-fixtures.h
        fixtures/implementation-fixtures.h
        btwxt-tests.cpp
        implementation-tests.cpp
        grid-axis-tests.cpp)

add_executable(${PROJECT_NAME}_tests ${library_sources})

target_compile_features(${PROJECT_NAME}_tests PRIVATE cxx_std_17)

target_include_directories(${PROJECT_NAME}_tests PRIVATE "${PROJECT_SOURCE_DIR}/src")

target_link_libraries(${PROJECT_NAME}_tests ${PROJECT_NAME} gtest gmock fmt)

include(GoogleTest)

gtest_discover_tests(${PROJECT_NAME}_tests TEST_PREFIX ${PROJECT_NAME}:)
