set(kiva_test_src
  foundation.unit.cpp
)

include_directories(${CMAKE_BINARY_DIR}/src/libkiva/)

add_executable(kiva_tests ${kiva_test_src})

target_link_libraries(kiva_tests gtest libkiva)

add_test(NAME unit.BESTEST.GC10a COMMAND $<TARGET_FILE:kiva_tests> "--gtest_filter=BESTESTFixture.GC10a")
add_test(NAME unit.BESTEST.GC30a COMMAND $<TARGET_FILE:kiva_tests> "--gtest_filter=BESTESTFixture.GC30a")
add_test(NAME unit.BESTEST.GC30b COMMAND $<TARGET_FILE:kiva_tests> "--gtest_filter=BESTESTFixture.GC30b")
add_test(NAME unit.BESTEST.GC30c COMMAND $<TARGET_FILE:kiva_tests> "--gtest_filter=BESTESTFixture.GC30c")
add_test(NAME unit.BESTEST.GC60b COMMAND $<TARGET_FILE:kiva_tests> "--gtest_filter=BESTESTFixture.GC60b")
add_test(NAME unit.BESTEST.GC65b COMMAND $<TARGET_FILE:kiva_tests> "--gtest_filter=BESTESTFixture.GC65b")
add_test(NAME unit.BESTEST.1D COMMAND $<TARGET_FILE:kiva_tests> "--gtest_filter=BESTESTFixture.1D")
