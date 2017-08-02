
SET( generate_embeddedable_schema_src
  generate_embeddable_schema.cpp
)

if (UNIX)
  ADD_DEFINITIONS( " -fPIC " )
endif()

add_executable( generate_embeddedable_schema ${generate_embeddedable_schema_src} )

set_target_properties( generate_embeddedable_schema
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/scripts"
)

