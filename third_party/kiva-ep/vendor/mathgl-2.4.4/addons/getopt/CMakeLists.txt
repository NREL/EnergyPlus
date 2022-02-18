add_library(getopt-static STATIC getopt.c getopt.h)
target_compile_definitions(getopt-static PUBLIC MGL_STATIC_DEFINE)
