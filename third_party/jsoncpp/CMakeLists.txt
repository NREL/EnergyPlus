
set(SRC
  json/json.h
  json/json-forwards.h
  jsoncpp.cpp
)

add_library( jsoncpp STATIC ${SRC} )

