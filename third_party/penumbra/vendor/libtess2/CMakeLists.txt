project(tess2)

file(GLOB sources "${tess2_SOURCE_DIR}/Source/[a-zA-Z]*.c")

set(library_sources
  ${sources}
  ${public_headers}
  ${private_headers}
)

include_directories(${tess2_SOURCE_DIR}/Include)
include_directories(${tess2_SOURCE_DIR}/Source)

if (UNIX)
    add_definitions("-fPIC")
endif()

add_library(tess2 ${library_sources})

#add_subdirectory(Example) # Doesn't work with OpenGL 3.2
