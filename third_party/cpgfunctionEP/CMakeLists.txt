cmake_minimum_required(VERSION 3.6)
project(cpgfunctionEP)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
if (MSVC)
    add_compile_definitions(_USE_MATH_DEFINES)
endif ()

# Everything will need to access this
include_directories(include)

# Create a DLL with an API
add_library(cpgfunctionEP
        src/blas.cpp
        src/boreholes.cpp
        src/coordinates.cpp
        src/gfunction.cpp
        src/heat_transfer.cpp
        src/interpolation.cpp
        src/lu.cpp
        src/segments.cpp
        src/statistics.cpp
        src/utilities.cpp
        )
