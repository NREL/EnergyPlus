
SET( generate_embeddedable_jdd_src
  generate_embeddable_jdd.cpp
)

if (UNIX)
  ADD_DEFINITIONS( " -fPIC " )
endif()

add_executable( generate_embeddedable_jdd ${generate_embeddedable_jdd_src} )

set_target_properties( generate_embeddedable_jdd
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/scripts"
)

