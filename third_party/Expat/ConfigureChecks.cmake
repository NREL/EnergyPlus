include(CheckIncludeFile)
include(CheckFunctionExists)
include(CheckSymbolExists)

check_include_file("dlfcn.h" HAVE_DLFCN_H)
check_include_file("fcntl.h" HAVE_FCNTL_H)
check_include_file("inttypes.h" HAVE_INTTYPES_H)
check_include_file("memory.h" HAVE_MEMORY_H)
check_include_file("stdint.h" HAVE_STDINT_H)
check_include_file("stdlib.h" HAVE_STDLIB_H)
check_include_file("strings.h" HAVE_STRINGS_H)
check_include_file("string.h" HAVE_STRING_H)
check_include_file("sys/stat.h" HAVE_SYS_STAT_H)
check_include_file("sys/types.h" HAVE_SYS_TYPES_H)
check_include_file("unistd.h" HAVE_UNISTD_H)

check_function_exists("getpagesize" HAVE_GETPAGESIZE)
check_function_exists("bcopy" HAVE_BCOPY)
check_symbol_exists("memmove" "string.h" HAVE_MEMMOVE)
check_function_exists("mmap" HAVE_MMAP)

#/* Define to 1 if you have the ANSI C header files. */
set(STDC_HEADERS 1)
# to lazy to implement:
#set(WORDS_BIGENDIAN ON)
#/* 1234 = LIL_ENDIAN, 4321 = BIGENDIAN */
set(BYTEORDER 1234)
#/* Define to specify how much context to retain around the current parse point. */
set(XML_CONTEXT_BYTES 1024)
#/* Define to make parameter entity parsing functionality available. */
set(XML_DTD 1)
#/* Define to make XML Namespaces functionality available. */
set(XML_NS 1)

if(HAVE_SYS_TYPES_H)
    check_symbol_exists("off_t" "sys/types.h" OFF_T)
    check_symbol_exists("size_t" "sys/types.h" SIZE_T)
else(HAVE_SYS_TYPES_H)
    set(OFF_T "long")
    set(SIZE_T "unsigned")
endif(HAVE_SYS_TYPES_H)

configure_file(expat_config.h.cmake expat_config.h)
add_definitions(-DHAVE_EXPAT_CONFIG_H)
