# Main library

set(library_sources
  btwxt.h
  btwxt.cpp
  griddeddata.h
  griddeddata.cpp
  gridpoint.h
  gridpoint.cpp
  error.h
  error.cpp
)

add_library(btwxt ${library_sources})
