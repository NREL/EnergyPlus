configure_file("Energy+.idd.in" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.idd")


add_custom_command(
  OUTPUT "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.schema.epJSON"
  COMMAND ${Python_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/generate_epJSON_schema/generate_epJSON_schema.py" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
  MAIN_DEPENDENCY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.idd"
  DEPENDS
    generate_epJSON_schema/generate_epJSON_schema.py
    generate_epJSON_schema/idd_parser.py
    generate_epJSON_schema/modify_schema.py
  VERBATIM
)

# Since multiple targets depend on the JSON Schema, we need to make a custom target for it so the dependent targets don't try to run the custom command at the same time.
# (see: https://cmake.org/cmake/help/latest/command/add_custom_command.html?highlight=add_custom_command#example-generating-files-for-multiple-targets)
add_custom_target(epjson_schema ALL DEPENDS "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.schema.epJSON")
set_target_properties(epjson_schema PROPERTIES FOLDER "Internal")

add_executable(generate_embeddable_epJSON_schema generate_embeddable_epJSON_schema/generate_embeddable_epJSON_schema.cpp)
target_link_libraries(generate_embeddable_epJSON_schema PRIVATE project_options project_fp_options project_warnings)
set_target_properties(generate_embeddable_epJSON_schema PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/scripts")

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/EmbeddedEpJSONSchema.cc"
  COMMAND
    ${CMAKE_COMMAND}
        -D "EnergyPlus_SOURCE_DIR:PATH=${CMAKE_CURRENT_SOURCE_DIR}"
        -D "EnergyPlus_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
        -D "EnergyPlus_BINARY_DIR:PATH=${CMAKE_CURRENT_BINARY_DIR}"
        -D "EnergyPlus_CURRENT_BINARY_DIR:PATH=${CMAKE_CURRENT_BINARY_DIR}"
        -D "EnergyPlus_embeddable_epJSON_schema:PATH=$<TARGET_FILE:generate_embeddable_epJSON_schema>"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/generate_embeddable_epJSON_schema/generate_embedded_epJSON_schema.cmake"
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
    generate_embeddable_epJSON_schema
    "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.schema.epJSON" # File-level dependency
    epjson_schema                                             # target-level dependency, to avoid race conditions
    "${CMAKE_CURRENT_SOURCE_DIR}/generate_embeddable_epJSON_schema/EmbeddedEpJSONSchema.in.cc"
  VERBATIM)

add_library(embedded_epjson_source STATIC
  generate_embeddable_epJSON_schema/EmbeddedEpJSONSchema.hh
  "${CMAKE_CURRENT_BINARY_DIR}/EmbeddedEpJSONSchema.cc"
)
target_include_directories(embedded_epjson_source PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/generate_embeddable_epJSON_schema")
target_link_libraries(embedded_epjson_source PRIVATE project_options project_fp_options project_warnings)

#install(FILES "V7-2-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-0-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-1-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-2-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-3-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-4-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-5-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-6-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-7-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-8-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "V8-9-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V9-0-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V9-1-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V9-2-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V9-3-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V9-4-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V9-5-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V9-6-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V22-1-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V22-2-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "V23-1-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
