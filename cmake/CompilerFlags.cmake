
# Compiler-agnostic compiler flags first
SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DOBJEXXFCL_FARRAY_INIT -DOBJEXXFCL_FARRAY_INIT_DEBUG") # Objexx DEFinition
    
IF ( MSVC ) # visual c++ (VS 2013)

    # Disabled Warnings:
    #  4244  Narrowing conversions
    #  4258  Definition from the loop is ignored
    #  4355  Passing this pointer in class initializer (object is incomplete so bases/members can only use this in limited ways)
    #  4996  Deprecated" STL functions (that MS has safer, non-std alternatives for)
    
    # need to figure out how to set this to avoid the major slow-down in debugging:
    # Configuration Properties ->Debugging -> Environment, use drop-down list to choose <Edit> and type _NO_DEBUG_HEAP=1 then click OK 
            
    # COMPILER FLAGS
    ADD_DEFINITIONS("-MP") # Enables multi-processor compilation of source within a single project

    # Za must be set in the individual projects because gtest uses win.h and cannot compile with it
    #ADD_DEFINITIONS("-Za") # Disables MS language extensions

    ADD_DEFINITIONS("-wd4244 -wd4258 -wd4355 -wd4996") # Disables warning messages listed above 
    ADD_DEFINITIONS("-DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
    ADD_DEFINITIONS("-W3")

    # -D_CRT_SECURE_NO_DEPRECATE hides function calls which make the library thread-unsafe
    # -D_SCL_SECURE_NO_DEPRECATE is itself deprecated and replaced by _SCL_SECURE_NO_WARNING which is made irrelevant by -wd4996 above
    # Todo remove this line entirely
    #ADD_DEFINITIONS("-D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES") # ???

    # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
    SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -GS-") # Disable buffer overrun checks for performance in release mode
    
ELSEIF ( CMAKE_COMPILER_IS_GNUCXX OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" ) # g++/Clang

    # COMPILER FLAGS
    ADD_DEFINITIONS("-std=c++11") # Enable C++11 features in g++
    ADD_DEFINITIONS("-pedantic") # Turn on warnings about constructs/situations that may be non-portable or outside of the standard
    ADD_DEFINITIONS("-Wall -Wextra -Wno-unused-parameter") # Turn on warnings (all, extra, "???don't warn about unused parameters???")
    
    # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
    SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsignaling-nans") # Disable optimizations that may have concealed NaN behavior
    SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -ggdb") # Produces debugging information specifically for gdb
             
ELSEIF ( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel" )
    
    # Warnings ignored:
    #  1786: Use of deprecated items
    #  2259: Non-pointer conversion from "type" to "type" may lose significant bits
    
    # COMPILER FLAGS
    ADD_DEFINITIONS("${CMAKE_CXX_FLAGS} /Wall") # Enable "all" warnings
    ADD_DEFINITIONS("${CMAKE_CXX_FLAGS} /Qdiag-disable:1786,2259") # Disable warnings listed above
    ADD_DEFINITIONS("${CMAKE_CXX_FLAGS} /DVC_EXTRALEAN /DWIN32_LEAN_AND_MEAN") # Excludes rarely used services and headers from compilation
    ADD_DEFINITIONS("${CMAKE_CXX_FLAGS} /DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
    
    # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
    SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /fp:fast") # Enables more aggressive optimizations on floating-point data
    SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Qprec-div-") # ???If this is equivalent to /Qno-prec-div, it disables the improved division accuracy in favor of speed
    SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Qip") # Enables inter-procedural optimnization within a single file
    SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Qoption,c,-ip_ninl_max_stats=500") # Sets the max increase in the # of intermediate language statements to 500 for each function
    SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Qoption,c,-ip_ninl_max_total_stats=5000") # Sets the total max increase in the # of intermediate language statements to 5000
        
    # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
    SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /check:stack,uninit") # Enables runtime checking of the stack (buffer over and underruns; pointer verification) and uninitialized variables
    SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /Gs0") # ??? Disable/Enable stack checking
    SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /Qfp-stack-check") # Tells the compiler to generate extra code after every function call to ensure fp stack is as expected
    SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /Qtrapuv") # ??? Initializes variables with NaN
    
ENDIF () # COMPILER TYPE
