if (CMAKE_VERSION VERSION_LESS 3.2)
  set(UPDATE_DISCONNECTED_IF_AVAILABLE "")
else()
  # UPDATE_DISCONNECTED 1: Don't look for remote updates on sub-sequent builds (e.g., git pull)
  # Change to 0 to check for updates to branch
  set(UPDATE_DISCONNECTED_IF_AVAILABLE "UPDATE_DISCONNECTED 1")
endif()

include(DownloadProject)

# GLAD library
add_subdirectory(glad)

# GLFW libaray
set(GLFW_BUILD_DOCS OFF CACHE BOOL "" FORCE)
set(GLFW_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(GLFW_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)

if (APPLE)
  set(GLFW_USE_MENUBAR OFF CACHE BOOL "" FORCE)
endif()

add_subdirectory(glfw-3.2.1)
add_subdirectory(libtess2)



if (BUILD_PENUMBRA_TESTING AND NOT TARGET gtest)

  download_project(PROJ                googletest
    GIT_REPOSITORY      https://github.com/google/googletest.git
    GIT_TAG             release-1.8.0
    DOWNLOAD_DIR        "${CMAKE_CURRENT_BINARY_DIR}/googletest-dl"
    SOURCE_DIR          "${CMAKE_CURRENT_SOURCE_DIR}/googletest"
    BINARY_DIR          "${CMAKE_CURRENT_BINARY_DIR}/googletest"
    ${UPDATE_DISCONNECTED_IF_AVAILABLE}
    )

  # Prevent GoogleTest from overriding our compiler/linker options
  # when building with Visual Studio
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  set(BUILD_GTEST ON CACHE BOOL "" FORCE)

  add_subdirectory(${googletest_SOURCE_DIR} ${googletest_BINARY_DIR})

endif()
