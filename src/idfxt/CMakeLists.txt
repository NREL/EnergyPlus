
set(SRC 
  JSONDataInterface.cpp
  JSONDataInterface.h
  idd-full.h
  idd-minimal-ordered.h
)

add_library(JSONDataAPI ${SRC})
add_executable(idfxt main.cpp)
target_link_libraries(idfxt JSONDataAPI cJSON)

install( TARGETS idfxt DESTINATION ./ )

