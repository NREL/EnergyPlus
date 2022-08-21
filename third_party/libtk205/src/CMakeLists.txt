file(GLOB lib_headers "${PROJECT_SOURCE_DIR}/include/libtk205/*.h")
file(GLOB lib_src "${PROJECT_SOURCE_DIR}/src/*.cpp")

set (SOURCES "${lib_headers}"
             "${lib_src}")

add_library(libtk205 ${SOURCES})
target_include_directories(libtk205 PUBLIC ${PROJECT_SOURCE_DIR}/include/libtk205)
target_include_directories(libtk205 PUBLIC ${PROJECT_SOURCE_DIR}/vendor/btwxt/src)
target_include_directories(libtk205 PRIVATE ${PROJECT_SOURCE_DIR}/vendor/json/single_include)
target_include_directories(libtk205 PRIVATE ${PROJECT_SOURCE_DIR}/vendor/json/include)
target_include_directories(libtk205 PRIVATE ${PROJECT_SOURCE_DIR}/vendor/valijson/include)

# target_compile_features(libtk205 PUBLIC cxx_std_17)

