# Main library
file(GLOB_RECURSE sources "${penumbra_SOURCE_DIR}/src/[a-zA-Z]*.cpp")
file(GLOB_RECURSE public_headers "${penumbra_SOURCE_DIR}/include/penumbra/[a-zA-Z]*.h")
file(GLOB_RECURSE private_headers "${penumbra_SOURCE_DIR}/src/[a-zA-Z]*.h")

set(library_sources
  ${sources}
  ${public_headers}
  ${private_headers}
)

include_directories(${penumbra_SOURCE_DIR}/include)
include_directories(${penumbra_SOURCE_DIR}/src)
include_directories(SYSTEM ${penumbra_SOURCE_DIR}/vendor/glad/include)
include_directories(SYSTEM ${penumbra_SOURCE_DIR}/vendor/glfw-3.2.1/include)
include_directories(SYSTEM ${penumbra_SOURCE_DIR}/vendor/glfw-3.2.1/deps)
include_directories(SYSTEM ${penumbra_SOURCE_DIR}/vendor/libtess2/Include)

option( PENUMBRA_STATIC_LIB "Make penumbra a static library" ON )

if (PENUMBRA_STATIC_LIB)
  add_library(penumbra STATIC ${library_sources})
  if(UNIX)
    set_target_properties(penumbra PROPERTIES COMPILE_FLAGS "-DPENUMBRA_STATIC_DEFINE -fPIC")
  else()
    set_target_properties(penumbra PROPERTIES COMPILE_FLAGS "-DPENUMBRA_STATIC_DEFINE")
  endif()
else()
  set(CMAKE_MACOSX_RPATH 1)
  add_library(penumbra SHARED ${library_sources})
endif()

include(GenerateExportHeader)
generate_export_header(penumbra)

target_link_libraries(penumbra glad glfw tess2)

if (PENUMBRA_COVERAGE)
  add_coverage(penumbra)
endif()
