# Add google tests macro
macro(ADD_GOOGLE_TESTS_WCE executable)
  foreach ( source ${ARGN} )
    string(REGEX MATCH .*cpp|.*cc source "${source}")
    if(source)
      file(READ "${source}" contents)
      string(REGEX MATCHALL "TEST_?F?\\(([A-Za-z_0-9 ,]+)\\)" found_tests ${contents})
      foreach(hit ${found_tests})
        string(REGEX REPLACE ".*\\(( )*([A-Za-z_0-9]+)( )*,( )*([A-Za-z_0-9]+)( )*\\).*" "\\2.\\5" test_name ${hit})
        add_test(NAME ${test_name} 
                 COMMAND "${executable}" "--gtest_filter=${test_name}")
      endforeach(hit)
    endif()
  endforeach()
endmacro()

# Create source groups automatically based on file path
macro( CREATE_SRC_GROUPS_WCE SRC )
  foreach( F ${SRC} )
    string( REGEX MATCH "(^.*)([/\\].*$)" M ${F} )
    if(CMAKE_MATCH_1)
      string( REGEX REPLACE "[/\\]" "\\\\" DIR ${CMAKE_MATCH_1} )
      source_group( ${DIR} FILES ${F} )
    else()
      source_group( \\ FILES ${F} )
    endif()
  endforeach()
endmacro()

# Create test targets
macro( CREATE_TEST_TARGETS_WCE BASE_NAME SRC DEPENDENCIES )
  if( BUILD_WCE_TESTING )
    add_executable( ${BASE_NAME}_tests ${SRC} )

    if( ENABLE_GTEST_DEBUG_MODE )
    set_target_properties(${BASE_NAME}_tests PROPERTIES COMPILE_DEFINITIONS ENABLE_GTEST_DEBUG_MODE)
    endif()

    CREATE_SRC_GROUPS_WCE( "${SRC}" )
    
    get_target_property(BASE_NAME_TYPE ${BASE_NAME} TYPE)
    if ("${BASE_NAME_TYPE}" STREQUAL "EXECUTABLE")
      # don't link base name
      set(ALL_DEPENDENCIES ${DEPENDENCIES} )
    else()
      # also link base name
      set(ALL_DEPENDENCIES ${BASE_NAME} ${DEPENDENCIES} )
    endif()
      
    target_link_libraries( ${BASE_NAME}_tests 
      ${ALL_DEPENDENCIES} 
      gtest 
    )

    ADD_GOOGLE_TESTS_WCE( ${BASE_NAME}_tests ${SRC} )
  endif()
endmacro()

macro( ADD_CXX_DEFINITIONS NEWFLAGS )
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${NEWFLAGS}")
endmacro()

macro( ADD_CXX_DEBUG_DEFINITIONS NEWFLAGS )
  SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${NEWFLAGS}")
endmacro()

macro( ADD_CXX_RELEASE_DEFINITIONS NEWFLAGS )
  SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${NEWFLAGS}")
endmacro()