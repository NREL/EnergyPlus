message(AUTHOR_WARNING "generating embedded epJSON schema")
execute_process(
  COMMAND "${EnergyPlus_embeddable_epJSON_schema}" "${EnergyPlus_EPJSON_SCHEMA}"
  TIMEOUT 90
  RESULT_VARIABLE generate_embedded_epJSON_schema
  OUTPUT_VARIABLE embedded_epJSON_schema
)
if(${generate_embedded_epJSON_schema} MATCHES ".*timeout.*")
  message(FATAL_ERROR "Generating embedded epJSON Schema from epJSON Schema failed: ${generate_embedded_epJSON_schema}")
endif()
get_filename_component(PARENT_DIR ${EnergyPlus_EMBEDDED_OUTPUT} DIRECTORY)
file(MAKE_DIRECTORY ${PARENT_DIR})
configure_file("${EnergyPlus_EMBEDDED_OUTPUT_TEMPLATE}" "${EnergyPlus_EMBEDDED_OUTPUT}")
