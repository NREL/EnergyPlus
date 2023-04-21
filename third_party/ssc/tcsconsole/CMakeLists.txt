#####################################################################################################################
#
# Project Settings
#
#####################################################################################################################

include_directories(. $ENV{WEXDIR}/include $ENV{LKDIR}/include ../tcs)

set(TCSCONSOLE_SRC
	tcsscript.cpp
	tcsmain.cpp
	tcslayout.cpp)


#####################################################################################################################
#
# Packages
#
#####################################################################################################################

if (UNIX)
	set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
	find_package(wxWidgets REQUIRED xrc stc richtext ribbon propgrid aui gl html qa adv net core xml net base)
else()
	set(wxWidgets_ROOT_DIR $ENV{WXMSW3})
	find_package(wxWidgets REQUIRED xrc stc richtext ribbon propgrid aui gl html qa adv core xml net base scintilla)
endif()
include(${wxWidgets_USE_FILE})


#####################################################################################################################
#
# CMake Targets
#
#####################################################################################################################

# Setup Application
if (APPLE)
	set(MACOSX_BUNDLE_ICON_FILE ${CMAKE_CURRENT_SOURCE_DIR}/../build_resources/TCSConsole.icns)
	set(TCSCONSOLE_ICON ${CMAKE_CURRENT_SOURCE_DIR}/../build_resources/TCSConsole.icns)
	set_source_files_properties(${TCSCONSOLE_ICON} PROPERTIES
		MACOSX_PACKAGE_LOCATION "Resources")
elseif(WIN32)
	#set(TCSCONSOLE_ICON ${CMAKE_CURRENT_SOURCE_DIR}/../build_resources/TCSConsole.rc)
endif()

add_executable(TCSConsole
	WIN32
	MACOSX_BUNDLE
	${TCSCONSOLE_SRC}
	${TCSCONSOLE_ICON})
set_target_properties(TCSConsole
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)

set_default_compile_options(TCSConsole)
set_tools_compile_options(TCSConsole)

if (APPLE)
	set_target_properties(TCSConsole PROPERTIES
		MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/../build_resources/Info-TCSConsole.plist
	)
endif()

if (MSVC)
	set_target_properties(TCSConsole PROPERTIES
        LINK_FLAGS /SUBSYSTEM:WINDOWS)
endif()


#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

target_link_libraries(TCSConsole ssc)

if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
	target_link_libraries(TCSConsole wex lk)
else()
    if (CMAKE_BUILD_TYPE STREQUAL "Release" OR MSVC)
        unset(WEX_LIB CACHE)
        unset(LK_LIB CACHE)
        find_library( WEX_LIB
                NAMES wex.a wex.lib
                PATHS $ENV{WEX_LIB} $ENV{WEXDIR}/build $ENV{WEXDIR}/build/Release $ENV{CMAKEBUILDDIR}/wex/Release)
        find_library( LK_LIB
                NAMES lk.a lk.lib
                PATHS $ENV{LK_LIB} $ENV{LKDIR}/build $ENV{LKDIR}/build/Release $ENV{CMAKEBUILDDIR}/lk/Release)
        target_link_libraries(TCSConsole optimized ${WEX_LIB} optimized ${LK_LIB})
    endif()

	if (CMAKE_BUILD_TYPE STREQUAL "Debug" OR MSVC)
		unset(LKD_LIB CACHE)
		unset(WEXD_LIB CACHE)
		find_library( WEXD_LIB
				NAMES wexd.a wexd.lib
				PATHS $ENV{WEXD_LIB} $ENV{WEXDIR}/build $ENV{WEXDIR}/build/Debug $ENV{CMAKEBUILDDIR}/wex/Debug)
		find_library( LKD_LIB
				NAMES lkd.a lkd.lib
				PATHS $ENV{LKD_LIB} $ENV{LKDIR}/build $ENV{LKDIR}/build/Debug $ENV{CMAKEBUILDDIR}/lk/Debug)
		target_link_libraries(TCSConsole debug ${WEXD_LIB} debug ${LKD_LIB})
	endif()
endif()

target_link_libraries(TCSConsole ${wxWidgets_LIBRARIES})

if (UNIX)
	target_link_libraries(TCSConsole -lm -lcurl)
elseif (MSVC)
	find_library( CURL_LIB
			NAMES libcurl.lib
			PATHS $ENV{WEXDIR}/build_resources/libcurl_ssl_x64/lib)
	target_link_libraries(TCSConsole ${CURL_LIB})
	target_link_libraries(TCSConsole Winhttp)
endif()

if (UNIX AND NOT APPLE)
	target_link_libraries(TCSConsole -lfontconfig -ldl)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

if(MSVC)
	set( LIBCURL_FILES
	   dbghelp.dll
	   libcurl.dll
	   libeay32.dll
	   libssh2.dll
	   msvcp140.dll
	   openssl.exe
	   ssleay32.dll
	   vcruntime140.dll
	)

	set(TCSCONSOLE_DIR ${CMAKE_CURRENT_BINARY_DIR}/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)

	foreach( file_i ${LIBCURL_FILES})
		add_custom_command(
		TARGET TCSConsole
		POST_BUILD
		COMMAND ${CMAKE_COMMAND}
		ARGS -E copy $ENV{WEXDIR}/build_resources/libcurl_ssl_x64/bin/${file_i} ${TCSCONSOLE_DIR}
	)
	endforeach( file_i )
endif()
