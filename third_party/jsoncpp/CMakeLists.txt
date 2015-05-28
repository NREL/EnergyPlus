
set(SRC
  json/json.h
  json/json-forwards.h
  jsoncpp.cpp
)

add_library( jsoncpp STATIC ${SRC} )

set( TEST_SRC tst/unit/json.unit.cc )
CREATE_TEST_TARGETS( jsoncpp "${TEST_SRC}" "" )
