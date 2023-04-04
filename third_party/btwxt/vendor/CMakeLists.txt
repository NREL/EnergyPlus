include(initialize-submodules)
initialize_submodules()

if (NOT TARGET fmt)
    add_subdirectory(fmt)
    set(FMT_INSTALL OFF CACHE BOOL "" FORCE)
    mark_as_advanced(FMT_CMAKE_DIR FMT_CUDA_TEST FMT_DEBUG_POSTFIX FMT_DOC FMT_FUZZ FMT_INC_DIR FMT_INSTALL FMT_INSTALL
            FMT_LIB_DIR FMT_MODULE FMT_OS FMT_PEDANTIC FMT_PKGCONFIG_DIR FMT_SYSTEM_HEADERS FMT_TEST FMT_WERROR)
endif ()

# googletest library
if (${PROJECT_NAME}_BUILD_TESTING AND NOT TARGET gtest)

    # Prevent GoogleTest from overriding our compiler/linker options
    # when building with Visual Studio
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    set(BUILD_GTEST ON CACHE BOOL "" FORCE MARK)
    set(BUILD_GMOCK ON CACHE BOOL "" FORCE)
    set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
    mark_as_advanced(BUILD_GTEST BUILD_GMOCK INSTALL_GTEST)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/googletest)

endif ()

if (NOT TARGET courierr)
    add_subdirectory(courierr)
    mark_as_advanced(courierr_BUILD_TESTING courierr_COVERAGE courierr_STATIC_LIB)
endif ()
