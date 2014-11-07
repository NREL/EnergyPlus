
# Compiler-agnostic compiler flags first
ADD_CXX_DEBUG_DEFINITIONS("-DOBJEXXFCL_FARRAY_INIT -DOBJEXXFCL_FARRAY_INIT_DEBUG") # Objexx DEFinition
    
IF ( MSVC ) # visual c++ (VS 2013)

    # Disabled Warnings:
    #  4244  Narrowing conversions
    #  4258  Definition from the loop is ignored
    #  4355  Passing this pointer in class initializer (object is incomplete so bases/members can only use this in limited ways)
    #  4996  Deprecated" STL functions (that MS has safer, non-std alternatives for)

    # need to figure out how to set this to avoid the major slow-down in debugging:
    # Configuration Properties ->Debugging -> Environment, use drop-down list to choose <Edit> and type _NO_DEBUG_HEAP=1 then click OK 

    # COMPILER FLAGS
    ADD_CXX_DEFINITIONS("-MP") # Enables multi-processor compilation of source within a single project

    # Za must be set in the individual projects because gtest uses win.h and cannot compile with it
    #ADD_DEFINITIONS("-Za") # Disables MS language extensions

    ADD_CXX_DEFINITIONS("-wd4244 -wd4258 -wd4355 -wd4996") # Disables warning messages listed above 
    ADD_CXX_DEFINITIONS("-DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
    ADD_CXX_DEFINITIONS("-W1")

    # -D_CRT_SECURE_NO_DEPRECATE hides function calls which make the library thread-unsafe
    # -D_SCL_SECURE_NO_DEPRECATE is itself deprecated and replaced by _SCL_SECURE_NO_WARNING which is made irrelevant by -wd4996 above
    # Todo remove this line entirely
    #ADD_DEFINITIONS("-D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES") # ???

    # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
    ADD_CXX_RELEASE_DEFINITIONS("-GS-") # Disable buffer overrun checks for performance in release mode
    
ELSEIF ( CMAKE_COMPILER_IS_GNUCXX OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" ) # g++/Clang
    option(ENABLE_THREAD_SANITIZER "Enable thread sanitizer testing in gcc/clang" FALSE)
    set(LINKER_FLAGS "")
    if(ENABLE_THREAD_SANITIZER)
      ADD_CXX_DEFINITIONS(-fsanitize=thread )
      add_definitions(-ggdb -fno-omit-frame-pointer)
      set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=thread -ggdb")
    endif()

    option(ENABLE_ADDRESS_SANITIZER "Enable address sanitizer testing in gcc/clang" FALSE)
    if(ENABLE_ADDRESS_SANITIZER)
      ADD_CXX_DEFINITIONS(-fsanitize=address)
      add_definitions(-ggdb -fno-omit-frame-pointer)
      set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=address -ggdb")
    endif()

    option(ENABLE_UNDEFINED_SANITIZER "Enable undefined behavior sanitizer testing in gcc/clang" FALSE)
    if(ENABLE_UNDEFINED_SANITIZER)
      ADD_CXX_DEFINITIONS(-fsanitize=undefined )
      add_definitions(-ggdb -fno-omit-frame-pointer)
      set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=undefined -ggdb")
    endif()

    mark_as_advanced(ENABLE_THREAD_SANITIZER ENABLE_ADDRESS_SANITIZER ENABLE_UNDEFINED_SANITIZER)

    if(CMAKE_HOST_UNIX)
      if(NOT APPLE)
        set(LINKER_FLAGS "${LINKER_FLAGS} -pthread")
        add_definitions(-pthread)
      endif()
    endif()

    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LINKER_FLAGS}")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${LINKER_FLAGS}")
    set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${LINKER_FLAGS}")


    # COMPILER FLAGS
    ADD_CXX_DEFINITIONS("-std=c++11") # Enable C++11 features in g++
    ADD_CXX_DEFINITIONS("-pedantic") # Turn on warnings about constructs/situations that may be non-portable or outside of the standard
    # ADD_CXX_DEFINITIONS("-Wall -Wextra -Wno-unused-parameter") # Turn on warnings (all, extra, "???don't warn about unused parameters???")
    ADD_CXX_DEFINITIONS("-Wno-invalid-source-encoding")
    
    # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
    ADD_CXX_DEBUG_DEFINITIONS("-fsignaling-nans") # Disable optimizations that may have concealed NaN behavior
    ADD_CXX_DEBUG_DEFINITIONS("-ggdb") # Produces debugging information specifically for gdb
             
ELSEIF ( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel" )
    
    # Warnings ignored:
    #  1786: Use of deprecated items
    #  2259: Non-pointer conversion from "type" to "type" may lose significant bits
    
    # COMPILER FLAGS
    #ADD_CXX_DEFINITIONS("/Wall") # Enable "all" warnings
    ADD_CXX_DEFINITIONS("/Qdiag-disable:1786,2259") # Disable warnings listed above
    ADD_CXX_DEFINITIONS("/DVC_EXTRALEAN /DWIN32_LEAN_AND_MEAN") # Excludes rarely used services and headers from compilation
    ADD_CXX_DEFINITIONS("/DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
    
    # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
    ADD_CXX_RELEASE_DEFINITIONS("/fp:fast") # Enables more aggressive optimizations on floating-point data
    ADD_CXX_RELEASE_DEFINITIONS("/Qprec-div-") # ???If this is equivalent to /Qno-prec-div, it disables the improved division accuracy in favor of speed
    ADD_CXX_RELEASE_DEFINITIONS("/Qip") # Enables inter-procedural optimnization within a single file
    ADD_CXX_RELEASE_DEFINITIONS("/Qoption,c,-ip_ninl_max_stats=500") # Sets the max increase in the # of intermediate language statements to 500 for each function
    ADD_CXX_RELEASE_DEFINITIONS("/Qoption,c,-ip_ninl_max_total_stats=5000") # Sets the total max increase in the # of intermediate language statements to 5000
        
    # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
    ADD_CXX_DEBUG_DEFINITIONS("/check:stack,uninit") # Enables runtime checking of the stack (buffer over and underruns; pointer verification) and uninitialized variables
    ADD_CXX_DEBUG_DEFINITIONS("/Gs0") # ??? Disable/Enable stack checking
    ADD_CXX_DEBUG_DEFINITIONS("/Qfp-stack-check") # Tells the compiler to generate extra code after every function call to ensure fp stack is as expected
    ADD_CXX_DEBUG_DEFINITIONS("/Qtrapuv") # ??? Initializes variables with NaN
    
ENDIF () # COMPILER TYPE
