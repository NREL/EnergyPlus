cmake_minimum_required(VERSION 2.8.12)

project(AirflowNetwork)

set(HDRS include/AirflowNetwork/Solver.hpp include/AirflowNetwork/Elements.hpp include/AirflowNetwork/Properties.hpp)

set(SRCS src/Solver.cpp src/Elements.cpp src/Properties.cpp)

# Create a static library
# this will be linked statically to create the DLL and also the unit tests
add_library(airflownetworklib STATIC ${SRCS} ${HDRS})

target_include_directories(airflownetworklib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(airflownetworklib PUBLIC fmt::fmt)
