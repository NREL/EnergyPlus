project(GLAD)

include_directories(include)

if (UNIX)
    add_definitions("-fPIC")
endif()

add_library(glad src/glad.c)
