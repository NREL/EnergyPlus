#### Create interface for JSON for Modern C++ so we can include it in targets
# add_library( ModernJSON INTERFACE )
# target_include_directories( ModernJSON INTERFACE "${PROJECT_SOURCE_DIR}/nlohmann" "${PROJECT_SOURCE_DIR}/doj" )


# To avoid modifying every third_party library
# we link the project_options in here for every library in this
# subdirectory

set(CMAKE_CXX_STANDARD 17)

if (NOT MSVC)
  add_compile_options(-Wno-pedantic -Wno-unused-parameter)
else()
  add_compile_options(/wd4267 /wd4996 /wd4068 /wd4244 )
endif()

set(INSTALL_GTEST CACHE BOOL OFF)
mark_as_advanced(INSTALL_GTEST)

set(RE2_BUILD_TESTING
    OFF
    CACHE BOOL "" FORCE)

if(OPENGL_FOUND)
  set(BUILD_PENUMBRA_TESTING
      ${BUILD_TESTING}
      CACHE BOOL "" FORCE)
endif()

# We don't want to enable the global warnings for any of the third party projects
link_libraries(project_options)
add_subdirectory(SQLite)
set_target_properties(sqlite PROPERTIES FOLDER ThirdParty/SQLite)

# Then add in the FP options after SQLite project
# because it cannot compile with any of the checked FP options
link_libraries(project_fp_options)

if(BUILD_TESTING)
  set(gtest_force_shared_crt
      ON
      CACHE BOOL "" FORCE)
  set(BUILD_GTEST
      ON
      CACHE BOOL "" FORCE)
  set(BUILD_GMOCK
      ON
      CACHE BOOL "" FORCE)
  option(TEST_ANNUAL_SIMULATION "Use annual simulations for tests instead of only design days" OFF)
  enable_testing()
  add_subdirectory(gtest)
  set_target_properties(gtest PROPERTIES FOLDER ThirdParty/gtest)
  set_target_properties(gtest_main PROPERTIES FOLDER ThirdParty/gtest)
  set_target_properties(gmock PROPERTIES FOLDER ThirdParty/gtest)
  set_target_properties(gmock_main PROPERTIES FOLDER ThirdParty/gtest)
endif()


add_subdirectory(ObjexxFCL)
set_target_properties(objexx PROPERTIES FOLDER ThirdParty/ObjexxFCL)
add_subdirectory(BCVTB)
set_target_properties(bcvtb PROPERTIES FOLDER ThirdParty/BCVTB)
add_subdirectory(Expat)
set_target_properties(epexpat PROPERTIES FOLDER ThirdParty/Expat)
add_subdirectory(FMI)
set_target_properties(epfmiimport PROPERTIES FOLDER ThirdParty/FMI)
add_subdirectory(zlib)
set_target_properties(miniziplib PROPERTIES FOLDER ThirdParty/zlib)
add_subdirectory(DElight)
set_target_properties(DElight PROPERTIES FOLDER ThirdParty/DElight)
add_subdirectory(re2)
set_target_properties(re2 PROPERTIES FOLDER ThirdParty/re2)
add_subdirectory(fmt-6.1.2)
set_target_properties(fmt PROPERTIES FOLDER ThirdParty/fmt)


# Kiva
include(cmake/kiva.cmake)
set_target_properties(libkiva PROPERTIES FOLDER ThirdParty/Kiva)


if(NOT APPLE)
  add_subdirectory(FMUParser)
  set_target_properties(parser PROPERTIES FOLDER ThirdParty/FMI)
endif()

add_subdirectory(Windows-CalcEngine)
set_target_properties(Windows-CalcEngine PROPERTIES FOLDER ThirdParty/Windows-CalcEngine)

# Btwxt
include_directories(${gtest_SOURCE_DIR}/include/ SYSTEM)
set(BUILD_BTWXT_TESTING
    ${BUILD_TESTING}
    CACHE BOOL "" FORCE)
add_subdirectory(btwxt)
set_target_properties(btwxt PROPERTIES FOLDER ThirdParty/Btwxt)

if(OPENGL_FOUND)
  add_subdirectory(penumbra)
  set_target_properties(awning PROPERTIES FOLDER ThirdParty/Penumbra)
  set_target_properties(glfw PROPERTIES FOLDER ThirdParty/Penumbra/GLFW)
  set_target_properties(glad PROPERTIES FOLDER ThirdParty/Penumbra)
  set_target_properties(penumbra PROPERTIES FOLDER ThirdParty/Penumbra)
  set_target_properties(tess2 PROPERTIES FOLDER ThirdParty/Penumbra)
endif()


