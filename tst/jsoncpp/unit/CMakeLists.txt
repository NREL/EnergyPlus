
set( test_src
  json.unit.cc
)
set( test_dependencies
 )

CREATE_TEST_TARGETS( jsoncpp "${test_src}" "${test_dependencies}" )
target_link_libraries( jsoncpp_tests gtest_main )
