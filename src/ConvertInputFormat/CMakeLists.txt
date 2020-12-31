cmake_minimum_required(VERSION 3.5.1)
project(ConvertInputFormat)

set(CMAKE_CXX_STANDARD 11)

# Set the CFLAGS and CXXFLAGS depending on the options the user specified.
# Only GCC-like compilers support -Wextra, and other compilers give tons of
# output for -Wall, so only -Wall and -Wextra on GCC.
if (CMAKE_COMPILER_IS_GNUCC OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic")
endif (CMAKE_COMPILER_IS_GNUCC OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")

# Detect OpenMP support in a compiler. If the compiler supports OpenMP, the
# flags to compile with OpenMP are returned and added.
find_package(OpenMP)
if (OPENMP_FOUND)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif (OPENMP_FOUND)

if (UNIX)
  ADD_DEFINITIONS("-fPIC")
endif()

if (APPLE OR UNIX)
  add_executable( ConvertInputFormat main.cpp )
else()  # windows
  add_executable( ConvertInputFormat main.cpp ) #  "${CMAKE_CURRENT_BINARY_DIR}/energyplus.rc" )
endif()

target_link_libraries( ConvertInputFormat energyplusparser )

if(UNIX AND NOT APPLE)
  target_link_libraries( ConvertInputFormat dl )
endif()

if (WIN32)
  target_link_libraries( ConvertInputFormat Shlwapi )
endif()

set_target_properties(ConvertInputFormat PROPERTIES VERSION ${ENERGYPLUS_VERSION})
set_target_properties(ConvertInputFormat PROPERTIES FOLDER Auxiliary)

install( TARGETS ConvertInputFormat DESTINATION ./ )
