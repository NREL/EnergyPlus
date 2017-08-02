
SET( generate_embeddedable_epJSON_schema_src
  generate_embeddable_epJSON_schema.cpp
)

if (UNIX)
  ADD_DEFINITIONS( " -fPIC " )
endif()

add_executable( generate_embeddedable_epJSON_schema ${generate_embeddedable_epJSON_schema_src} )

set_target_properties( generate_embeddedable_epJSON_schema
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/scripts"
)

