# Use ccache if available, has to be before "project()"
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  # Support both Unix Makefiles and Ninja
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
endif()

project(EnergyPlus)

cmake_minimum_required(VERSION 3.10)

if(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} VERSION_GREATER "3.0")
  cmake_policy(SET CMP0054 NEW) # CMake 3.1 added this policy
endif()

cmake_policy(SET CMP0012 NEW) # if() recognizes boolean constants

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

add_library(project_options INTERFACE)
target_compile_features(project_options INTERFACE cxx_std_17)

add_library(project_fp_options INTERFACE)

add_library(project_warnings INTERFACE)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads)
target_link_libraries(project_options INTERFACE Threads::Threads)

include(cmake/Sanitizers.cmake)
enable_sanitizers(project_options)

include(cmake/Version.cmake)

set(OPENGL_REQUIRED
    ON
    CACHE BOOL "Require OpenGL in order to build")
if(OPENGL_REQUIRED)
  # On Linux specifically, OpenGL is complaining about being able to find OpenGL using both the
  # exported module target or the library variables.  If it has to choose, and doesn't have a user-preference,
  # a warning is shown on every configure step.  This next line just sets the preference so the warning won't show up.
  # See this for more info: https://cmake.org/cmake/help/v3.10/module/FindOpenGL.html#linux-specific
  set(OpenGL_GL_PREFERENCE GLVND)
  find_package(OpenGL)
  if(NOT OPENGL_FOUND)
    set(CMAKE_VERSION_BUILD
        "${CMAKE_VERSION_BUILD} (No OpenGL)"
        CACHE STRING "Build number" FORCE) # git sha
    message(WARNING "OpenGL libraries were not found. EnergyPlus will be compiled without GPU acceleration capabilities.")
  endif()
else()
  set(OPENGL_FOUND FALSE)
endif()

option(BUILD_PACKAGE "Build package" OFF)
option(BUILD_TESTING "Build testing targets" OFF)
option(BUILD_FORTRAN "Build Fortran stuff" OFF)
option(USE_PSYCHROMETRICS_CACHING "Use psychrometrics cashing routines" ON)
option(USE_GLYCOL_CACHING "Use glycol cashing routines" ON)
option(USE_PSYCH_STATS "Compute psychrometric stats" OFF)
option(USE_PSYCH_ERRORS "Report psychrometric errors " ON)

#######################################################################

# TODO: remove at next release
# Temporary workaround to default the correct DOCUMENTATION_BUILD value if BUILD_DOCS existed in the cache or is passed via the command line
# This warning will be triggered only once if it's from the Cache since I'm unsetting it
# (such as when using the GUI or running `cmake .` instead of passing all command line arguments)
if(DEFINED BUILD_DOCS)
  if(NOT DEFINED DOCUMENTATION_BUILD)
    if(BUILD_DOCS)
      message(AUTHOR_WARNING "BUILD_DOCS (=ON) was deprecated in favor of the 'DOCUMENTATION_BUILD' choice, which was defaulted to 'BuildWithAll'")
      set(DOCUMENTATION_BUILD
          "BuildWithAll"
          CACHE STRING "Build LaTeX-pdf documentation")
    else()
      message(AUTHOR_WARNING "BUILD_DOCS (=OFF) was deprecated in favor of the 'DOCUMENTATION_BUILD' choice, which was defaulted to 'DoNotBuild")
    endif()
  else()
    message(AUTHOR_WARNING "DOCUMENTATION_BUILD is already defined and has value '${DOCUMENTATION_BUILD}', ignoring BUILD_DOCS")
  endif()
  unset(BUILD_DOCS CACHE)
endif()
# END TODO

set(DocumentationBuildValues
    "DoNotBuild;BuildWithAll;BuildOnlyWithPackage"
    CACHE INTERNAL "List of possible values for the LaTeX-pdf documentation")
set(DOCUMENTATION_BUILD
    "DoNotBuild"
    CACHE STRING "Build LaTeX-pdf documentation")
set_property(CACHE DOCUMENTATION_BUILD PROPERTY STRINGS ${DocumentationBuildValues})

if(NOT DOCUMENTATION_BUILD IN_LIST DocumentationBuildValues)
  message(FATAL_ERROR "DOCUMENTATION_BUILD must be one of ${DocumentationBuildValues}")
endif()

# Local only variables for ease of use in if blocks
set(BUILD_DOCS OFF)
set(BUILD_DOCS_ONLY_WITH_PACKAGE OFF)

if(DOCUMENTATION_BUILD STREQUAL "BuildWithAll")
  set(BUILD_DOCS ON)
  set(BUILD_DOCS_ONLY_WITH_PACKAGE OFF)
elseif(DOCUMENTATION_BUILD STREQUAL "BuildOnlyWithPackage")
  set(BUILD_DOCS ON)
  set(BUILD_DOCS_ONLY_WITH_PACKAGE ON)
endif()
#######################################################################

option(LINK_WITH_PYTHON "Link with Python Library for Python Plugin Builds" OFF)

# Turning ENABLE_GTEST_DEBUG_MODE ON will cause assertions and exceptions to halt the test case and unwind.
# Turn this option OFF for automated testing.
option(ENABLE_GTEST_DEBUG_MODE "Enable options to help debug test failures" ON)
option(ENABLE_GTEST_SHUFFLE "Enable shuffle to eliminate order dependency" ON)
option(ENABLE_INSTALL_REMOTE "Enable install_remote and install_remote_plist commands to install files from remote resources on the internet" ON)

mark_as_advanced(ENABLE_INSTALL_REMOTE)

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      Release
      CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# we are making *a Python 3.6 Interpreter* a required dependency, so find it here
find_package(PythonInterp 3.6 REQUIRED)

if(BUILD_TESTING)
  option(ENABLE_REGRESSION_TESTING "Enable Regression Tests" OFF)
  option(BUILD_WCE_TESTING "Build testing targets for Windows Calculation Engine" OFF)
  if(ENABLE_REGRESSION_TESTING)
    if(CMAKE_CL_64)
      set(ARCH_FLAG "-64bit")
    else()
      set(ARCH_FLAG "")
    endif()
    set(REGRESSION_BASELINE_SHA
        ""
        CACHE STRING "SHA of baseline comparison")
    set(COMMIT_SHA
        ""
        CACHE STRING "Commit sha of this build")
    set(REGRESSION_SCRIPT_PATH
        ""
        CACHE PATH "Path to regression scripts")
    set(REGRESSION_BASELINE_PATH
        ""
        CACHE PATH "Path to energyplus baseline folder to compare against")
    set(DEVICE_ID
        "${CMAKE_SYSTEM_NAME}-${CMAKE_CXX_COMPILER_ID}${ARCH_FLAG}"
        CACHE STRING "Identifier for this device configuration")
  endif()

  enable_testing()
endif()

if(ENABLE_REGRESSION_TESTING)
  string(STRIP "${REGRESSION_SCRIPT_PATH}" REG_PATH_STRIPPED)
  string(STRIP "${REGRESSION_BASELINE_PATH}" REG_BASELINE_STRIPPED)
  if(REG_PATH_STRIPPED STREQUAL "" OR REG_BASELINE_STRIPPED STREQUAL "")
    message(AUTHOR_WARNING "Regression testing is enabled, but no script or baseline has been provided, turning it off.")
    set(DO_REGRESSION_TESTING OFF)
  else()
    if(BUILD_FORTRAN)
      # No Problem
      set(DO_REGRESSION_TESTING ON)
    else()
      message(AUTHOR_WARNING "For regression testing, you must enable BUILD_FORTRAN in order to call ReadVarEso, turning it off.")
      set(DO_REGRESSION_TESTING OFF)
    endif()
  endif()
endif()

if(UNIX AND NOT APPLE)
  set(CMAKE_INSTALL_RPATH "$ORIGIN")
endif()

if(MSVC AND NOT ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel"))
  if(CMAKE_C_COMPILER_VERSION VERSION_LESS "18.00.30723")
    message(FATAL_ERROR "Visual Studio 2013 Update 3 or Greater Required")
  endif()
  set(gtest_force_shared_crt ON)
endif()

if(MINGW)
  set(CMAKE_SHARED_LIBRARY_PREFIX "")
endif()

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/Products")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/Products")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/Products")

include(cmake/ProjectMacros.cmake)
include(cmake/CompilerFlags.cmake)

add_subdirectory(third_party)

target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/src)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/btwxt/src)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/re2)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/doj)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/nlohmann)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/valijson)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/ObjexxFCL/src/)
target_include_directories(project_options SYSTEM INTERFACE ${PROJECT_SOURCE_DIR}/third_party/SQLite/)
target_include_directories(project_options SYSTEM INTERFACE "${PROJECT_SOURCE_DIR}/third_party/Expat" "${PROJECT_SOURCE_DIR}/third_party/Expat/lib")
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/CLI/)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/eigen/)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/Windows-CalcEngine/src/Chromogenics/include)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/Windows-CalcEngine/src/Common/include)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/Windows-CalcEngine/src/Gases/include)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/Windows-CalcEngine/src/MultiLayerOptics/include)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/Windows-CalcEngine/src/SingleLayerOptics/include)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/Windows-CalcEngine/src/SpectralAveraging/include)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/Windows-CalcEngine/src/Tarcog/include)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/Windows-CalcEngine/src/Viewer/include)
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/ssc)
if(OPENGL_FOUND)
  target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/penumbra/include)
endif()
target_include_directories(project_options INTERFACE ${PROJECT_SOURCE_DIR}/third_party/kiva-ep/src)
target_include_directories(project_options INTERFACE "${kiva_BINARY_DIR}/src/libkiva")
target_include_directories(project_options SYSTEM INTERFACE "${kiva_SOURCE_DIR}/vendor/boost-1.61.0/")

add_subdirectory(scripts/dev/generate_embeddable_epJSON_schema)
set_target_properties(generate_embeddedable_epJSON_schema PROPERTIES FOLDER "Internal")

if(OPENGL_FOUND)
  set(BUILD_PENUMBRA_TESTING
      OFF
      CACHE BOOL "")
endif()

# E+ required libraries
add_subdirectory(idd)

execute_process(
  COMMAND ${PYTHON_EXECUTABLE} "${PROJECT_SOURCE_DIR}/scripts/dev/generate_epJSON_schema/generate_epJSON_schema.py" "${PROJECT_SOURCE_DIR}"
  TIMEOUT 30
  RESULT_VARIABLE generate_epJSON_schema_result)
if(${generate_epJSON_schema_result} MATCHES ".*timeout.*")
  message(FATAL_ERROR "Generating epJSON Schema from IDD failed: ${generate_epJSON_schema_result}")
endif()
configure_file(idd/Energy+.idd.in "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.idd")
configure_file(idd/Energy+.schema.epJSON.in "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Energy+.schema.epJSON")
configure_file(idd/BasementGHT.idd "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/BasementGHT.idd")
configure_file(idd/SlabGHT.idd "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/SlabGHT.idd")

# EPLaunch workflow files should be configured
configure_file("workflows/app_g_postprocess.py" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/app_g_postprocess.py")
configure_file("workflows/calc_soil_surface_temp.py" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/calc_soil_surface_temp.py")
configure_file("workflows/coeff_check.py" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/coeff_check.py")
configure_file("workflows/coeff_conv.py" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/coeff_conv.py")
configure_file("workflows/energyplus.py" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/energyplus.py")
configure_file("workflows/transition.py" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/workflows/transition.py")

# of course E+ itself
add_subdirectory(src/EnergyPlus)
set_target_properties(airflownetworklib PROPERTIES FOLDER "Internal")
set_target_properties(GenerateEmbeddedEpJSONSchema PROPERTIES FOLDER "Internal")
set_target_properties(energyplusparser PROPERTIES FOLDER "Internal")

if(BUILD_TESTING)
  add_subdirectory(testfiles)
  add_subdirectory(tst/EnergyPlus/unit)
  option(BUILD_PERFORMANCE_TESTS "Build performance testing targets" OFF)
  if(BUILD_PERFORMANCE_TESTS)
    if(UNIX AND NOT APPLE)
      option(VALGRIND_ANALYZE_PERFORMANCE_TESTS "Analyze performance tests via valgrind / callgrind" OFF)
      if(VALGRIND_ANALYZE_PERFORMANCE_TESTS)
        find_program(VALGRIND NAMES valgrind PATH /usr/bin /usr/local/bin)
      endif()
    endif()
    add_subdirectory(performance_tests)
  endif()
  set_target_properties(energyplus_tests PROPERTIES FOLDER Testing)
  # set_target_properties(TestAPI_Autosizing_C PROPERTIES FOLDER Testing)
  set_target_properties(TestAPI_DataTransfer_C PROPERTIES FOLDER Testing)
  set_target_properties(TestAPI_Runtime_C PROPERTIES FOLDER Testing)
  set_target_properties(TestAPI_Functional_C PROPERTIES FOLDER Testing)
  set_target_properties(TestEnergyPlusCallbacks PROPERTIES FOLDER Testing)

  set_target_properties(btwxt_tests PROPERTIES FOLDER ThirdParty/Btwxt)
  set_target_properties(kiva_tests PROPERTIES FOLDER ThirdParty/Kiva)
  if(OPENGL_FOUND AND BUILD_PENUMBRA_TESTING)
    set_target_properties(penumbra_tests PROPERTIES FOLDER ThirdParty/Penumbra)
  endif()
endif()

add_subdirectory(src/ConvertInputFormat)

if(BUILD_FORTRAN)
  include(CMakeAddFortranSubdirectory)
  cmake_add_fortran_subdirectory(
    src/ExpandObjects
    PROJECT ExpandObjects
    NO_EXTERNAL_INSTALL)
  cmake_add_fortran_subdirectory(
    src/ReadVars
    PROJECT ReadVars
    NO_EXTERNAL_INSTALL)
  set(TRANSITION_EXTRA_FLAGS "-DCMAKE_VERSION_MAJOR:STRING=${CMAKE_VERSION_MAJOR}" "-DCMAKE_VERSION_MINOR:STRING=${CMAKE_VERSION_MINOR}"
                             "-DCMAKE_VERSION_PATCH:STRING=${CMAKE_VERSION_PATCH}")
  cmake_add_fortran_subdirectory(
    src/Transition
    PROJECT Transition
    CMAKE_COMMAND_LINE ${TRANSITION_EXTRA_FLAGS}
    NO_EXTERNAL_INSTALL)
  file(COPY "idd/V9-4-0-Energy+.idd" DESTINATION "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
  configure_file(idd/Energy+.idd.in "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/V9-5-0-Energy+.idd")
  file(COPY "src/Transition/SupportFiles/Report Variables 9-3-0 to 9-4-0.csv" DESTINATION "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
  cmake_add_fortran_subdirectory(
    src/Basement
    PROJECT Basement
    NO_EXTERNAL_INSTALL)
  cmake_add_fortran_subdirectory(
    src/HVAC-Diagram
    PROJECT HVAC-Diagram
    NO_EXTERNAL_INSTALL)
  cmake_add_fortran_subdirectory(
    src/ParametricPreprocessor
    PROJECT ParametricPreprocessor
    NO_EXTERNAL_INSTALL)
  cmake_add_fortran_subdirectory(
    src/Slab
    PROJECT Slab
    NO_EXTERNAL_INSTALL)
  cmake_add_fortran_subdirectory(
    src/ConvertESOMTR
    PROJECT ConvertESOMTR
    NO_EXTERNAL_INSTALL)
  cmake_add_fortran_subdirectory(
    src/CalcSoilSurfTemp
    PROJECT CalcSoilSurfTemp
    NO_EXTERNAL_INSTALL)
  cmake_add_fortran_subdirectory(
    src/AppGPostProcess
    PROJECT AppGPostProcess
    NO_EXTERNAL_INSTALL)
endif()

if(BUILD_PACKAGE)
  # We handle everything in a dedicated cmake file
  include(${PROJECT_SOURCE_DIR}/cmake/Install.cmake)
endif()

if(BUILD_DOCS)
  set(DOCS_TESTING
      ${BUILD_TESTING}
      CACHE INTERNAL "" FORCE)
  add_subdirectory(doc)
endif()

# some parameters are simply not needed for everyday building
mark_as_advanced(CMAKE_INSTALL_PREFIX)
mark_as_advanced(BUILD_SHARED_LIBS)
mark_as_advanced(ENABLE_GTEST_DEBUG_MODE)
mark_as_advanced(ENABLE_GTEST_SHUFFLE)
mark_as_advanced(ENABLE_MEMORY_SANITIZER)
mark_as_advanced(ENABLE_OPENMP)
mark_as_advanced(KIVA_3D)
mark_as_advanced(KIVA_COVERAGE)
mark_as_advanced(KIVA_EXE_BUILD)
mark_as_advanced(KIVA_GROUND_PLOT)
mark_as_advanced(KIVA_STATIC_LIB)
mark_as_advanced(KIVA_TESTING)
mark_as_advanced(BTWXT_COVERAGE)
mark_as_advanced(BUILD_BTWXT_TESTING)
mark_as_advanced(BTWXT_STATIC_LIB)
mark_as_advanced(RE2_BUILD_TESTING)
mark_as_advanced(USEPCRE)
mark_as_advanced(CCACHE_PROGRAM)
mark_as_advanced(CMAKE_VERSION_BUILD)
mark_as_advanced(ENABLE_REGRESSION_TESTING)
mark_as_advanced(TEST_ANNUAL_SIMULATION)
mark_as_advanced(gtest_build_samples)
mark_as_advanced(gtest_build_tests)
mark_as_advanced(gtest_disable_pthreads)
mark_as_advanced(gtest_force_shared_crt)
mark_as_advanced(gtest_hide_internal_symbols)
mark_as_advanced(FMT_CUDA_TEST)
mark_as_advanced(FMT_DOC)
mark_as_advanced(FMT_FUZZ)
mark_as_advanced(FMT_INSTALL)
mark_as_advanced(FMT_PEDANTIC)
mark_as_advanced(FMT_TEST)
mark_as_advanced(FMT_WERROR)
mark_as_advanced(GLFW_BUILD_DOCS)
mark_as_advanced(GLFW_BUILD_EXAMPLES)
mark_as_advanced(GLFW_BUILD_TESTS)
mark_as_advanced(GLFW_INSTALL)
mark_as_advanced(GLFW_USE_OSMESA)
mark_as_advanced(GLFW_USE_WAYLAND)
mark_as_advanced(GLFW_VULKAN_STATIC)
mark_as_advanced(BUILD_PENUMBRA_TESTING)
mark_as_advanced(PENUMBRA_COVERAGE)
mark_as_advanced(PENUMBRA_STATIC_LIB)
mark_as_advanced(BUILD_GMOCK)
mark_as_advanced(BUILD_GTEST)
mark_as_advanced(gmock_build_tests)
mark_as_advanced(BUILD_WCE_TESTING)
mark_as_advanced(USE_PSYCHROMETRICS_CACHING)
mark_as_advanced(USE_GLYCOL_CACHING)
mark_as_advanced(USE_PSYCH_STATS)
mark_as_advanced(USE_PSYCH_ERRORS)
