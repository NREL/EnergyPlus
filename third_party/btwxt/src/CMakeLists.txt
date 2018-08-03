# Main library

include_directories("${PROJECT_SOURCE_DIR}/vendor/eigen3")

set(library_sources
  btwxt.h
  btwxt.cpp
  griddeddata.h
  griddeddata.cpp
  gridpoint.h
  gridpoint.cpp
  hypercube.h
  hypercube.cpp
  error.h
  error.cpp
)

add_library(btwxt ${library_sources})
