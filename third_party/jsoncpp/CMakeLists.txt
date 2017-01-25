
set(SRC
  json/json.h
  json/json-forwards.h
  jsoncpp.cpp
)

if(CMAKE_COMPILER_IS_GNUCXX)
  # Suppress warnings about enum comparisons (and everything else)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
endif()

add_library( jsoncpp STATIC ${SRC} )

