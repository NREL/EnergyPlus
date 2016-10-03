include_directories("${CMAKE_SOURCE_DIR}/third_party/kiva/src")
include_directories("${CMAKE_BINARY_DIR}/third_party/kiva/src/libkiva")
include_directories( SYSTEM "${CMAKE_SOURCE_DIR}/third_party/kiva/vendor/boost-1.61.0/")
include_directories( SYSTEM "${CMAKE_SOURCE_DIR}/third_party/kiva/vendor/lis-1.5.66/include/")

configure_file("${CMAKE_SOURCE_DIR}/third_party/cmake/CMakeLists-kiva.txt" "${CMAKE_BINARY_DIR}/third_party/cmake/CMakeLists.txt")
execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" . WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/third_party/cmake)
execute_process(COMMAND ${CMAKE_COMMAND} --build . WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/third_party/cmake)

add_subdirectory("${CMAKE_SOURCE_DIR}/third_party/kiva/src/libkiva")
add_subdirectory("${CMAKE_SOURCE_DIR}/third_party/kiva/vendor/lis-1.5.66/")
