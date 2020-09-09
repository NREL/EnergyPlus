#####################################################################################################################
#
# CMake and System Settings
#
#####################################################################################################################

set(CMAKE_VERBOSE_MAKEFILE ON)

if(APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.9" CACHE STRING "Minimum OS X deployment version")
endif()


#####################################################################################################################
#
# Project Settings 
#
#####################################################################################################################

Project(sdktool)

if ( NOT APPLE)
	set(CURL_DIR build_resources/libcurl_ssl_x64)
endif()

include_directories(. $ENV{WEXDIR}/include $ENV{LKDIR}/include ../ssc ../shared ../splinter)

set(SDKTOOL_SRC
	dataview.cpp
	dllinvoke.cpp
	editvariable.cpp
	scripting.cpp
	sscdev.cpp)


#####################################################################################################################
#
# Compile Options per Platform
#
#####################################################################################################################

cmake_policy(SET CMP0043 NEW)

if(MSVC)
	add_compile_options(/W3 /MP)
    add_compile_definitions( __64BIT__ LK_USE_WXWIDGETS _CRT_SECURE_NO_DEPRECATE=1 )
	add_compile_definitions(_CRT_NON_CONFORMING_SWPRINTFS=1 _SCL_SECURE_NO_WARNINGS=1)
	add_compile_definitions(__WXMSW__ _UNICODE NOPCH )
	foreach (flag_var CMAKE_C_FLAGS_DEBUG CMAKE_CXX_FLAGS_DEBUG)
		set(${flag_var} "${${flag_var}} /D_DEBUG" CACHE STRING "compile flags" FORCE)
	endforeach()
else(MSVC)
	if (APPLE)
		set(CMAKE_POSITION_INDEPENDENT_CODE ON)
		add_compile_options(-arch x86_64)
		add_definitions(-D__WXOSX__)
	else()
		add_definitions(-D__64BIT__)
	endif()
	add_compile_options(-Wall -Wno-deprecated -Wno-unknown-pragmas -Werror -Wno-overloaded-virtual -fno-common -std=c++11)
	add_definitions(-DLK_USE_WXWIDGETS -DWX_PRECOMP )
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		add_compile_definitions(_DEBUG)
	else()
		SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3" )
	endif()
endif(MSVC)


#####################################################################################################################
#
# Packages
#
#####################################################################################################################

if (UNIX)
	set(wxWidgets_CONFIG_EXECUTABLE /usr/local/bin/wx-config-3)
	find_package(wxWidgets COMPONENTS qa aui richtext html propgrid adv net stc core xml base REQUIRED)
else()
	set(wxWidgets_ROOT_DIR $ENV{WXMSW3})
	find_package(wxWidgets COMPONENTS qa webview aui richtext html propgrid adv net stc core xml base scintilla REQUIRED)
endif()

include(${wxWidgets_USE_FILE})


#####################################################################################################################
# 
# CMake Targets
#
#####################################################################################################################

# Setup Application
if (APPLE)
	set(MACOSX_BUNDLE_ICON_FILE ${CMAKE_CURRENT_SOURCE_DIR}/../build_resources/SDKTool.icns)
	set(SDKTOOL_ICON ${CMAKE_CURRENT_SOURCE_DIR}/../build_resources/SDKTool.icns)
	set_source_files_properties(${SDKTOOL_ICON} PROPERTIES
		MACOSX_PACKAGE_LOCATION "Resources")
elseif(WIN32)
	set(SDKTOOL_ICON ${CMAKE_CURRENT_SOURCE_DIR}/../build_resources/app.rc)
endif()

# Setup Target
add_executable(SDKtool
		WIN32
		MACOSX_BUNDLE 
		${SDKTOOL_SRC}
		${SDKTOOL_ICON})
set_target_properties(SDKtool
    PROPERTIES
    DEBUG_POSTFIX "d"
	PREFIX ""
	)

if (APPLE)
	set_target_properties(SDKtool PROPERTIES
		MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/../build_resources/Info-SDKTool.plist
	)
endif()

if (MSVC)
	set_target_properties(SDKtool PROPERTIES
        LINK_FLAGS /SUBSYSTEM:WINDOWS)
endif()

#####################################################################################################################
#
# Link Libraries and Options
#
#####################################################################################################################

if (${CMAKE_PROJECT_NAME} STREQUAL system_advisor_model)
	target_link_libraries(SDKtool wex lk)
else()
	unset(WEX_LIB CACHE)
	unset(LK_LIB CACHE)
	find_library( WEX_LIB
			NAMES wex.a wex.lib
			PATHS $ENV{WEXDIR}/build $ENV{WEXDIR}/build/Release)
	find_library( LK_LIB
			NAMES lk.a lk.lib
			PATHS $ENV{LKDIR}/build $ENV{LKDIR}/build/Release)
	target_link_libraries(SDKtool optimized ${WEX_LIB} optimized ${LK_LIB})

	if (CMAKE_BUILD_TYPE STREQUAL "Debug" OR MSVC)
		unset(LKD_LIB CACHE)
		unset(WEXD_LIB CACHE)
		find_library( WEXD_LIB
				NAMES wexd.a wexd.lib
				PATHS $ENV{WEXDIR}/build $ENV{WEXDIR}/build/Debug)
		find_library( LKD_LIB
				NAMES lkd.a lkd.lib
				PATHS $ENV{LKDIR}/build $ENV{LKDIR}/build/Debug)
		target_link_libraries(SDKtool debug ${WEXD_LIB} debug ${LKD_LIB})
	endif()
endif()

target_link_libraries(SDKtool ${wxWidgets_LIBRARIES})

if (UNIX AND NOT APPLE)
	target_link_libraries(SDKtool -ldl -lfontconfig)
endif()

if (UNIX)
	target_link_libraries(SDKtool -lm -lcurl)
elseif (MSVC)
	find_library( CURL_LIB
			NAMES libcurl.lib
			PATHS $ENV{WEXDIR}/build_resources/libcurl_ssl_x64/lib)
	target_link_libraries(SDKtool ${CURL_LIB})
	target_link_libraries(SDKtool Winhttp)
endif()


#####################################################################################################################
#
# Target Installation
#
#####################################################################################################################

if (MSVC)
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

	set(SDKTOOL_DIR ${CMAKE_CURRENT_BINARY_DIR}/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>)

	foreach( file_i ${LIBCURL_FILES})
		add_custom_command(
		TARGET SDKtool
		POST_BUILD
		COMMAND ${CMAKE_COMMAND}
		ARGS -E copy $ENV{WEXDIR}/build_resources/libcurl_ssl_x64/bin/${file_i} ${SDKTOOL_DIR}
	)
	endforeach( file_i )
endif()
