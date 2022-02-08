macro(SET_CPYTHON_VARIABLES)
    # sets the following variables:
    # - CPYTHON_DIR: the folder containing the clone of CPython itself
    # - CPYTHON_STDLIB_DIR: the folder containing the Python standard library implementation
    # - CPYTHON_INCLUDE_DIR: the C include directories for compiling
    # - CPYTHON_LIBRARY_DIR: the folder containing the compiled Python shared object library file
    # - CPYTHON_BUILT_BIN: the full path to the Python shared object library file
    set(CPYTHON_DIR "${PROJECT_SOURCE_DIR}/third_party/CPython")
    set(CPYTHON_STDLIB_DIR ${CPYTHON_DIR}/Lib)
    if (MSVC)
        if (CMAKE_CL_64)
            set(CPYTHON_PLATFORM x64)
            set(CPYTHON_BUILD_DIR amd64)
        else ()
            set(CPYTHON_PLATFORM x86)
            set(CPYTHON_BUILD_DIR win32)
        endif ()
        set(CPYTHON_INCLUDE_DIR ${CPYTHON_DIR}/Include ${CPYTHON_DIR}/PC)
        set(CPYTHON_LIBRARY_DIR ${CPYTHON_DIR}/PCBuild/${CPYTHON_BUILD_DIR})
        if (CMAKE_BUILD_TYPE MATCHES "Debug")
            set(CPYTHON_BUILT_BIN ${CPYTHON_LIBRARY_DIR}/python310_d.dll)
            set(CPYTHON_BUILT_LIB ${CPYTHON_LIBRARY_DIR}/python310_d.lib)
        else ()
            set(CPYTHON_BUILT_BIN ${CPYTHON_LIBRARY_DIR}/python310.dll)
            set(CPYTHON_BUILT_LIB ${CPYTHON_LIBRARY_DIR}/python310.lib)
        endif ()
    else ()
        set(CPYTHON_INCLUDE_DIR ${CPYTHON_DIR}/Include ${CPYTHON_DIR})
        set(CPYTHON_LIBRARY_DIR ${CPYTHON_DIR})
        if (UNIX AND NOT APPLE)
            set(CPYTHON_BIN_NAME "libpython3.10.so.1.0")
        elseif (APPLE)
            set(CPYTHON_BIN_NAME "libpython3.10.dylib")
        endif ()
        set(CPYTHON_BUILT_BIN "${CPYTHON_DIR}/${CPYTHON_BIN_NAME}")
    endif ()
endmacro()

macro(CREATE_CPYTHON_PROJECT)
    # creates the external project for building CPython
    include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)
    set(CPYTHON_BUILD_TYPE_FLAG "")  # blank indicates release build
    if (CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(CPYTHON_BUILD_TYPE_FLAG " -d ")  # override to debug mode if applicable
    endif ()
    # Add cpython as an external project that will be included in the build
    if (MSVC)
        ExternalProject_Add(CPYTHON
                SOURCE_DIR ${CPYTHON_DIR}
                CONFIGURE_COMMAND ""
                BUILD_COMMAND cd ${CPYTHON_DIR} && ${CPYTHON_DIR}/PCbuild/build.bat ${CPYTHON_BUILD_TYPE_FLAG} -p ${CPYTHON_PLATFORM}
                INSTALL_COMMAND ""
                TEST_COMMAND ""
                )
    else ()
        ExternalProject_Add(CPYTHON
                SOURCE_DIR ${CPYTHON_DIR}
                CONFIGURE_COMMAND cd ${CPYTHON_DIR} && ./configure --enable-shared # --enable-optimizations
                BUILD_COMMAND cd ${CPYTHON_DIR} && make -j 4
                INSTALL_COMMAND ""
                TEST_COMMAND ""
                )
    endif ()
endmacro()

macro(CREATE_CPYTHON_LIBRARY)
    # Creates a library target for cpython that depends on the python shared object that is built via external project
    add_library(cpython_library SHARED IMPORTED)
    if (MSVC)
        set_target_properties(cpython_library PROPERTIES IMPORTED_LOCATION_DEBUG ${CPYTHON_BUILT_BIN} IMPORTED_LOCATION_RELEASE ${CPYTHON_BUILT_BIN} IMPORTED_IMPLIB_DEBUG ${CPYTHON_BUILT_LIB} IMPORTED_IMPLIB_RELEASE ${CPYTHON_BUILT_LIB})
    else ()
        set_property(TARGET cpython_library PROPERTY IMPORTED_LOCATION ${CPYTHON_BUILT_BIN})
    endif ()
    add_dependencies(cpython_library CPYTHON)
    target_link_directories(cpython_library INTERFACE ${CPYTHON_LIBRARY_DIR})
endmacro()

macro(CPYTHON_POST_EXE_BUILD_OPERATIONS)
    add_custom_command(
            TARGET energyplus
            POST_BUILD
            DEPENDS
            __ALWAYSRUNME
            COMMAND ${CMAKE_COMMAND}
            -E copy_directory ${CPYTHON_STDLIB_DIR} $<TARGET_FILE_DIR:energyplus>/python_standard_lib
    )
    add_custom_command(
            TARGET energyplus
            POST_BUILD
            COMMAND ${Python_EXECUTABLE} "${PROJECT_SOURCE_DIR}/cmake/PythonCopyStandardLib.py" ${CPYTHON_LIBRARY_DIR} $<TARGET_FILE_DIR:energyplus>/python_standard_lib ${PROJECT_SOURCE_DIR})
    #    # Then also copy python standard library built modules into the standard library folder
    #    if (MSVC)
    #        file(GLOB CPYTHON_DLLS ${CPYTHON_LIBRARY_DIR}/*.dll)
    #        file(GLOB CPYTHON_PYDS ${CPYTHON_LIBRARY_DIR}/*.pyd)
    #        file(GLOB CPYTHON_EXES ${CPYTHON_LIBRARY_DIR}/*.exe)
    #        set(CPYTHON_ALL ${CPYTHON_DLLS} ${CPYTHON_PYDS} ${CPYTHON_EXES})
    #        foreach (MODULE IN LISTS CPYTHON_ALL)
    #            # message("Copying module: ${MODULE}")
    #            add_custom_command(
    #                    TARGET energyplusapi
    #                    POST_BUILD
    #                    COMMAND ${CMAKE_COMMAND}
    #                    -E copy "${MODULE}" $<TARGET_FILE_DIR:energyplusapi>/python_standard_lib
    #            )
    #        endforeach ()
    #    else ()
    #        file(GLOB MODULES ${PROJECT_SOURCE_DIR}/third_party/CPython/build/lib*/*)  # TODO: Verify this on Windows/Mac
    #        foreach (MODULE IN LISTS MODULES)
    #            message("Copying module: ${MODULE}")
    #            add_custom_command(
    #                    TARGET energyplusapi
    #                    POST_BUILD
    #                    DEPENDS
    #                    __ALWAYSRUNME
    #                    COMMAND ${CMAKE_COMMAND}
    #                    -E copy "${MODULE}" $<TARGET_FILE_DIR:energyplusapi>/python_standard_lib
    #            )
    #        endforeach ()
    #    endif ()
    if (APPLE)
        add_custom_command(
                TARGET energyplusapi
                POST_BUILD
                DEPENDS
                __ALWAYSRUNME
                COMMAND ${CMAKE_INSTALL_NAME_TOOL} -id "@executable_path/libpython3.10.dylib" "${CPYTHON_BUILT_BIN}"
        )
        add_custom_command(
                TARGET energyplus
                POST_BUILD
                DEPENDS
                __ALWAYSRUNME
                COMMAND ${CMAKE_COMMAND} -E copy "${CPYTHON_BUILT_BIN}" $<TARGET_FILE_DIR:energyplus>
        )
        #        add_custom_command(
        #                TARGET energyplusapi
        #                POST_BUILD
        #                DEPENDS
        #                __ALWAYSRUNME
        #                COMMAND ${CMAKE_INSTALL_NAME_TOOL} -change "@executable_path/libpython3.10.dylib" "@loader_path/libpython3.10.dylib" $<TARGET_FILE:energyplusapi>
        #        )
        add_custom_command(
                TARGET energyplus
                POST_BUILD
                DEPENDS
                __ALWAYSRUNME
                COMMAND ${CMAKE_INSTALL_NAME_TOOL} -change "/usr/local/lib/libpython3.10.dylib" "@executable_path/libpython3.10.dylib" $<TARGET_FILE:energyplus>
        )
    endif ()
    add_custom_command(
            TARGET energyplusapi
            POST_BUILD
            DEPENDS
            __ALWAYSRUNME
            COMMAND ${CMAKE_COMMAND} -E copy "${CPYTHON_BUILT_BIN}" $<TARGET_FILE_DIR:energyplusapi>
    )
endmacro()