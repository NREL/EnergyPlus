# Make sure expat is compiled as a static library
# ADD_DEFINITIONS("-DXML_STATIC")

IF ( CMAKE_COMPILER_IS_GNUCXX OR "x${CMAKE_CXX_COMPILER_ID}" STREQUAL "xClang" ) # g++/Clang
	set(LINKER_FLAGS "")
	mark_as_advanced(ENABLE_THREAD_SANITIZER ENABLE_ADDRESS_SANITIZER ENABLE_UNDEFINED_SANITIZER)
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LINKER_FLAGS}")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${LINKER_FLAGS}")
    set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${LINKER_FLAGS}")

    # COMPILER FLAGS
    ADD_CXX_DEFINITIONS("-pipe") # Faster compiler processing
    # set (CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++")
    if( MINGW )
        ADD_CXX_DEFINITIONS("-std=gnu++11") # Enable C++11 features in g++
    else()
        ADD_CXX_DEFINITIONS("-std=c++11") # Enable C++11 features in g++
        ADD_CXX_DEFINITIONS("-fPIC")
    endif()
    ADD_CXX_DEFINITIONS("-pedantic") # Turn on warnings about constructs/situations that may be non-portable or outside of the standard
    ADD_CXX_DEFINITIONS("-ffor-scope")
    ADD_CXX_DEFINITIONS("-Wall -Wextra") # Turn on warnings
    ADD_CXX_DEFINITIONS("-Wno-unknown-pragmas")
    if( CMAKE_COMPILER_IS_GNUCXX ) # g++
      ADD_CXX_DEFINITIONS("-Wno-unused-but-set-parameter -Wno-unused-but-set-variable") # Suppress unused-but-set warnings until more serious ones are addressed
      ADD_CXX_DEFINITIONS("-Wno-maybe-uninitialized")
    elseif( "x${CMAKE_CXX_COMPILER_ID}" STREQUAL "xClang" )
      ADD_CXX_DEFINITIONS("-Wno-invalid-source-encoding")
    endif()

    # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
    if ( CMAKE_COMPILER_IS_GNUCXX ) # g++
      ADD_CXX_DEBUG_DEFINITIONS("-ffloat-store") # Improve debug run solution stability
      ADD_CXX_DEBUG_DEFINITIONS("-fsignaling-nans") # Disable optimizations that may have concealed NaN behavior
    endif ()
  
  ADD_CXX_DEBUG_DEFINITIONS("-ggdb") # Produces debugging information specifically for gdb
ENDIF ()

macro( warning_level_update_wce )
  foreach (flag_var
    CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
    CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
    # Replaces /W3 with /W4 in defaults.
    string(REPLACE "/W3" "-W4" ${flag_var} "${${flag_var}}")
  endforeach()
  if( MSVC )
    ADD_CXX_DEFINITIONS( "-wd4589" )
  endif()
endmacro()
