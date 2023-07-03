configure_file("Energy+.idd.in" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.idd")


add_custom_command(
  OUTPUT "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.schema.epJSON"
  COMMAND ${Python_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/schema/generate_epJSON_schema.py" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
  MAIN_DEPENDENCY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.idd"
  DEPENDS
    schema/generate_epJSON_schema.py
    schema/idd_parser.py
    schema/modify_schema.py
  VERBATIM
)

# Since multiple targets depend on the JSON Schema, we need to make a custom target for it so the dependent targets don't try to run the custom command at the same time.
# (see: https://cmake.org/cmake/help/latest/command/add_custom_command.html?highlight=add_custom_command#example-generating-files-for-multiple-targets)
add_custom_target(epjson_schema ALL DEPENDS "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.schema.epJSON")
set_target_properties(epjson_schema PROPERTIES FOLDER "Internal")

add_executable(generate_embeddable_epJSON_schema embedded/generate_embeddable_epJSON_schema.cpp)
target_link_libraries(generate_embeddable_epJSON_schema PRIVATE project_options project_fp_options project_warnings)
set_target_properties(generate_embeddable_epJSON_schema PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/scripts")

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/embedded/EmbeddedEpJSONSchema.cc"
  COMMAND
    ${CMAKE_COMMAND}
        # We call the executable on the epJSON schema
        -D "EnergyPlus_embeddable_epJSON_schema:PATH=$<TARGET_FILE:generate_embeddable_epJSON_schema>"
        -D "EnergyPlus_EPJSON_SCHEMA:FILEPATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.schema.epJSON"
        # And configure_file the template with to produce the final cc file
        -D "EnergyPlus_EMBEDDED_OUTPUT_TEMPLATE:FILEPATH=${CMAKE_CURRENT_SOURCE_DIR}/embedded/EmbeddedEpJSONSchema.in.cc"
        -D "EnergyPlus_EMBEDDED_OUTPUT:FILEPATH=${CMAKE_CURRENT_BINARY_DIR}/embedded/EmbeddedEpJSONSchema.cc"
        # Using this helper cmake script
        -P "${CMAKE_CURRENT_SOURCE_DIR}/embedded/generate_embedded_epJSON_schema.cmake"
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
    generate_embeddable_epJSON_schema
    "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.schema.epJSON" # File-level dependency
    epjson_schema                                             # target-level dependency, to avoid race conditions
    "${CMAKE_CURRENT_SOURCE_DIR}/embedded/EmbeddedEpJSONSchema.in.cc"
  VERBATIM)

add_library(embedded_epjson_source STATIC
  embedded/EmbeddedEpJSONSchema.hh
  "${CMAKE_CURRENT_BINARY_DIR}/embedded/EmbeddedEpJSONSchema.cc"
)
target_include_directories(embedded_epjson_source PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_libraries(embedded_epjson_source PRIVATE project_options project_fp_options project_warnings)

#install(FILES "versions/V7-2-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-0-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-1-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-2-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-3-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-4-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-5-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-6-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-7-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-8-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
#install(FILES "versions/V8-9-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "versions/V9-0-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "versions/V9-1-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "versions/V9-2-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "versions/V9-3-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "versions/V9-4-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "versions/V9-5-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "versions/V9-6-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "versions/V22-1-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")
install(FILES "versions/V22-2-0-Energy+.idd" DESTINATION "PreProcess/IDFVersionUpdater")

# For fortran...
set(PREVIOUS_IDD "${CMAKE_CURRENT_SOURCE_DIR}/versions/V23-1-0-Energy+.idd")
set(PREVIOUS_IDD ${PREVIOUS_IDD} PARENT_SCOPE)

install(FILES "${PREVIOUS_IDD}" DESTINATION "PreProcess/IDFVersionUpdater")
