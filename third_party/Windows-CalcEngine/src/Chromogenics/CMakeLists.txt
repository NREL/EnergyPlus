cmake_minimum_required( VERSION 2.8.7 )

set( target_name Chromogenics )

# first include header files of this project itself
# include_directories( include )

# make sure to connect to header files of common variables
include_directories( ../Common/include )
include_directories( ../Gases/include )
include_directories( ../Tarcog/include )

# collect all source files and add them to the SOURCES list
file( GLOB SOURCES_CPP "src/*.cpp" )
file( GLOB SOURCES_HPP "src/*.hpp" )

LIST(APPEND SOURCES ${SOURCES_HPP} ${SOURCES_CPP} )

# Generates the static library from the SOURCES
add_library( ${target_name} STATIC ${SOURCES} )

target_link_libraries( ${target_name} ${LINK_TO_Common} )
target_link_libraries( ${target_name} ${LINK_TO_Tarcog} )

# Install will be used by master projects to get information on destination of library files
install(TARGETS ${target_name}
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)

if( BUILD_WCE_TESTING )
  include_directories( include )
	file( GLOB all_test_src RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" tst/*.cpp )
	set( test_src ${all_test_src} )
	CREATE_TEST_TARGETS_WCE( ${target_name} "${test_src}" "" )
endif ()

warning_level_update_wce()