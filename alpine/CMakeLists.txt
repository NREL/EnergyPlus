cmake_minimum_required (VERSION 2.8)

project(Alpine)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

find_package(Java REQUIRED)
include(UseJava)
find_package(JNI)
find_package(Curses)
if( APPLE )
  find_package(GFortranLibs)
endif()

# Boost
set(BOOST_ROOT "" CACHE PATH "Path to Boost Install")
if(NOT EXISTS ${BOOST_ROOT})
  include(FindOrDownloadBoost)
endif()

add_subdirectory(third-party/FMUParser ${CMAKE_CURRENT_BINARY_DIR}/FMUParser)
include_directories(third-party/FMUParser/)

set(LLVM_PROJECT_ROOT_DIR "${CMAKE_SOURCE_DIR}/build-third-party/llvm-prefix/src/llvm-install/" CACHE PATH "The root of the llvm project install tree")
find_package(LLVM REQUIRED CONFIG NO_DEFAULT_PATH PATHS "${LLVM_PROJECT_ROOT_DIR}")

set(CLANG_PROJECT_ROOT_DIR "${CMAKE_SOURCE_DIR}/build-third-party/llvm-prefix/src/llvm-install/" CACHE PATH "The root of the clang project install tree")
find_package(Clang REQUIRED CONFIG NO_DEFAULT_PATH PATHS "${CLANG_PROJECT_ROOT_DIR}")

if( APPLE )
  set(avian_platform_path ${CMAKE_SOURCE_DIR}/build-third-party/avian-prefix/src/avian/build/macosx-x86_64-openjdk-src/)
elseif( UNIX )
  set(avian_platform_path ${CMAKE_SOURCE_DIR}/build-third-party/avian-prefix/src/avian/build/linux-x86_64-openjdk-src/)
endif()

set(AVIAN_ROOT_DIR "${avian_platform_path}" CACHE PATH "The root of the avian build tree")
find_program(AVIAN_BIN_TOOL NAMES binaryToObject  HINTS "${AVIAN_ROOT_DIR}/binaryToObject" NO_DEFAULT_PATH)
find_file(AVIAN_RUNTIME_JAR NAMES classpath.jar  HINTS "${AVIAN_ROOT_DIR}" NO_DEFAULT_PATH)
find_library(AVIAN_RUNTIME_LIB NAMES avian  HINTS "${AVIAN_ROOT_DIR}" NO_DEFAULT_PATH)

set(JMODELICA_ROOT_DIR "${CMAKE_SOURCE_DIR}/build-third-party/jmodelica-prefix/src/jmodelica-install/" CACHE PATH "The root of the JModelica build tree")
find_file(JMODELICA_COMPILER_JAR NAMES ModelicaCompiler.jar  HINTS "${JMODELICA_ROOT_DIR}/lib" NO_DEFAULT_PATH)
find_file(JMODELICA_UTIL_JAR NAMES util.jar  HINTS "${JMODELICA_ROOT_DIR}/lib" NO_DEFAULT_PATH)
find_file(JMODELICA_BEAVER_JAR NAMES beaver-rt.jar  HINTS "${JMODELICA_ROOT_DIR}/ThirdParty/Beaver/lib" NO_DEFAULT_PATH)

add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/Main.class" 
  COMMAND "${Java_JAVAC_EXECUTABLE}" -bootclasspath "${AVIAN_RUNTIME_JAR}:${JMODELICA_COMPILER_JAR}:${JMODELICA_UTIL_JAR}:${JMODELICA_BEAVER_JAR}" -d "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}/Main.java"
  DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/Main.java"
)

add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/boot_jar/jar.stamp"
  COMMAND "${CMAKE_COMMAND}" -E make_directory boot_jar
  COMMAND "${CMAKE_COMMAND}" -E chdir "${CMAKE_CURRENT_BINARY_DIR}/boot_jar" "${Java_JAR_EXECUTABLE}" xf "${AVIAN_RUNTIME_JAR}" 
  COMMAND "${CMAKE_COMMAND}" -E chdir "${CMAKE_CURRENT_BINARY_DIR}/boot_jar" "${Java_JAR_EXECUTABLE}" xf "${JMODELICA_COMPILER_JAR}" 
  COMMAND "${CMAKE_COMMAND}" -E chdir "${CMAKE_CURRENT_BINARY_DIR}/boot_jar" "${Java_JAR_EXECUTABLE}" xf "${JMODELICA_UTIL_JAR}" 
  COMMAND "${CMAKE_COMMAND}" -E chdir "${CMAKE_CURRENT_BINARY_DIR}/boot_jar" "${Java_JAR_EXECUTABLE}" xf "${JMODELICA_BEAVER_JAR}" 
  COMMAND "${CMAKE_COMMAND}" -E touch boot_jar/jar.stamp
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  DEPENDS "${AVIAN_RUNTIME_JAR}" "${JMODELICA_COMPILER_JAR}" "${JMODELICA_UTIL_JAR}" "${JMODELICA_BEAVER_JAR}"
)

if( APPLE )
  set(avian_platform macosx)
elseif( WIN32 )
  set(avian_platform windows)
else()
  set(avian_platform linux)
endif()

add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/boot-jar.o"
  COMMAND "${Java_JAR_EXECUTABLE}" cf "${CMAKE_CURRENT_BINARY_DIR}/boot.jar" -C "${CMAKE_CURRENT_BINARY_DIR}/boot_jar" .
  COMMAND "${Java_JAR_EXECUTABLE}" u0f "${CMAKE_CURRENT_BINARY_DIR}/boot.jar" "Main.class"
  COMMAND "${AVIAN_BIN_TOOL}" "${CMAKE_CURRENT_BINARY_DIR}/boot.jar" "${CMAKE_CURRENT_BINARY_DIR}/boot-jar.o" _binary_boot_jar_start _binary_boot_jar_end ${avian_platform} x86_64
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/boot_jar/jar.stamp" "${CMAKE_CURRENT_BINARY_DIR}/Main.class"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
)

add_definitions("-D_JNI_IMPLEMENTATION_")
set(CMAKE_CXX_FLAGS "-fno-rtti")

include_directories("${JAVA_INCLUDE_PATH}")
include_directories("${JAVA_INCLUDE_PATH2}")
include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${CLANG_INCLUDE_DIRS})
include_directories("${JMODELICA_ROOT_DIR}/include/RuntimeLibrary")

add_library(alpine 
  #main.cpp 
  cc.hpp
  cc.cpp
  EModelica.hpp
  EModelica.cpp
  stub.cpp
  "${CMAKE_CURRENT_BINARY_DIR}/boot-jar.o" 
)

target_link_libraries(alpine
  LLVMMCJIT
  LLVMMIRParser
  LLVMX86AsmParser
  LLVMX86AsmPrinter
  LLVMX86CodeGen
  libclang
  clangBasic
  clangLex
  clangParse
  clangCodeGen
  alpparser
)

if( APPLE )
  target_link_libraries(alpine "-force_load ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libfmi1_me.a")
  target_link_libraries(alpine "-force_load ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libjmi.a")
  target_link_libraries(alpine "-force_load ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libjmi_get_set_default.a")
  target_link_libraries(alpine "-force_load ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libzlib.a")
  target_link_libraries(alpine "-force_load ${AVIAN_RUNTIME_LIB}")
  target_link_libraries(alpine "${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/liblapack.a")
  target_link_libraries(alpine "${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libblas.a")
  target_link_libraries(alpine "${JMODELICA_ROOT_DIR}/ThirdParty/Sundials/lib/libsundials_cvode.a")
  target_link_libraries(alpine "${JMODELICA_ROOT_DIR}/ThirdParty/Sundials/lib/libsundials_cvodes.a")
  target_link_libraries(alpine "${JMODELICA_ROOT_DIR}/ThirdParty/Sundials/lib/libsundials_kinsol.a")
  target_link_libraries(alpine "${JMODELICA_ROOT_DIR}/ThirdParty/Sundials/lib/libsundials_nvecserial.a")
  target_link_libraries(alpine "${JMODELICA_ROOT_DIR}/ThirdParty/Minpack/lib/libcminpack.a")
  target_link_libraries(alpine "-framework CoreFoundation")
  target_link_libraries(alpine "-framework Foundation")
  target_link_libraries(alpine "-framework SystemConfiguration")
  target_link_libraries(alpine "-framework Security")
  target_link_libraries(alpine "${LIBGFORTRAN_LIBRARIES}")
  target_link_libraries(alpine "${LIBQUADMATH_LIBRARIES}")
  target_link_libraries(alpine "${LIBGCC_LIBRARIES}")
  target_link_libraries(alpine "${Boost_LIBRARIES}")
else()
  target_link_libraries(alpine "-Wl,--whole-archive ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libfmi1_me.a")
  target_link_libraries(alpine "-Wl,--whole-archive ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libjmi.a")
  target_link_libraries(alpine "-Wl,--whole-archive ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libjmi_get_set_default.a")
  target_link_libraries(alpine "-Wl,--whole-archive ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libzlib.a")
  target_link_libraries(alpine "-Wl,--whole-archive ${AVIAN_RUNTIME_LIB}")
  target_link_libraries(alpine "-Wl,--no-whole-archive ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/liblapack.a")
  target_link_libraries(alpine "-Wl,--no-whole-archive ${JMODELICA_ROOT_DIR}/lib/RuntimeLibrary/libblas.a")
  target_link_libraries(alpine "-Wl,--no-whole-archive ${JMODELICA_ROOT_DIR}/ThirdParty/Sundials/lib/libsundials_cvode.a")
  target_link_libraries(alpine "-Wl,--no-whole-archive ${JMODELICA_ROOT_DIR}/ThirdParty/Sundials/lib/libsundials_cvodes.a")
  target_link_libraries(alpine "-Wl,--no-whole-archive ${JMODELICA_ROOT_DIR}/ThirdParty/Sundials/lib/libsundials_kinsol.a")
  target_link_libraries(alpine "-Wl,--no-whole-archive ${JMODELICA_ROOT_DIR}/ThirdParty/Sundials/lib/libsundials_nvecserial.a")
  target_link_libraries(alpine "-Wl,--no-whole-archive ${JMODELICA_ROOT_DIR}/ThirdParty/Minpack/lib/libcminpack.a")
  target_link_libraries(alpine gfortran)
  target_link_libraries(alpine ${Boost_LIBRARIES})
endif()

add_custom_command(TARGET alpine POST_BUILD
  COMMAND "${CMAKE_COMMAND}" -E copy_directory "${JMODELICA_ROOT_DIR}/CodeGenTemplates" "${CMAKE_CURRENT_BINARY_DIR}/CodeGenTemplates"
)

