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

option( BTWXT_STATIC_LIB "Make btwxt a static library" ON )

if (BTWXT_STATIC_LIB)
  add_library(btwxt STATIC ${library_sources})
  set_target_properties(btwxt PROPERTIES COMPILE_FLAGS "-DBTWXT_STATIC_DEFINE")
else()
  set(CMAKE_MACOSX_RPATH 1)
  add_library(btwxt SHARED ${library_sources})
endif()

include(GenerateExportHeader)
generate_export_header(btwxt)

if (BTWXT_COVERAGE)
  add_coverage(btwxt)
endif()