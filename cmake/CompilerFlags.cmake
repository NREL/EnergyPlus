# Still to flush out:
#   32-bit flags
#   Fortran / C flags
#   Clang flags
#   Mac flags

# Look for ??? for questions that still need to be answered

# convert the build type string to upper case for comparisons
#  http://www.cmake.org/pipermail/cmake/2012-June/050651.html
# don't do this for multiple build-type generators, such as MSVC; Others???
if ( NOT MSVC )
	STRING( TOUPPER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE_B )
endif () 

# **** WINDOWS **** #
# UNIX clause in IF block required to filter out cygwin installations
if ( WIN32 AND NOT UNIX )  
    
    # need to figure out how to set this to avoid the major slow-down in debugging:
    # Configuration Properties ->Debugging -> Environment, use drop-down list to choose <Edit> and type _NO_DEBUG_HEAP=1 then click OK 
    
    # visual c++ (VS 2013)
    if ( MSVC )    
	
        # Disabled Warnings:
        #  4101
		#  4102
		#  4244  Narrowing conversions
        #  4258  Definition from the loop is ignored
		#  4305
        #  4355  Passing this pointer in class initializer (object is incomplete so bases/members can only use this in limited ways)
        #  4521
		#  4800 
		#  4996  Deprecated" STL functions (that MS has safer, non-std alternatives for)
		
		# RELEASE MODE FLAGS
        SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -nologo") # Suppresses compiler copyright notice and informational messages
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -MP") # Enables multi-processor compilation of source within a single project
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -Za") # Disables MS language extensions
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -EHsc") # Specifies that exceptions are caught from C++ code only
		#SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -GR") # Adds code to check object types at runtime -- ON by DEFAULT 
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -wd4101 -wd4102 -wd4244 -wd4258 -wd4305 -wd4355 -wd4521 -wd4800 -wd4996") # Disables warning messages listed above 
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -DVC_EXTRALEAN -DWIN32_LEAN_AND_MEAN") # Excludes rarely used services and headers from compilation
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES") # ???
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -TP") # Globally treat all source files as C++
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -O2") # Optimize code to level 2
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -DNDEBUG") # Define the "NDEBUG" flag; disables standard-C assertions
		SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -GS-") # Disable buffer overrun checks for performance in release mode
		
		# DEBUG MODE FLAGS
        # A note from Stuart: -RTCc gave exe blocked by Windows 8.1 Defender with ostringstream
        SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -nologo") # Suppresses compiler copyright notice and informational messages
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -MP") # Enables multi-processor compilation of source within a single project
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Za") # Disables MS language extensions
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -EHsc") # Specifies that exceptions are caught from C++ code only
		#SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -GR") # Adds code to check object types at runtime -- ON by DEFAULT 
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -wd4101 -wd4102 -wd4244 -wd4258 -wd4305 -wd4355 -wd4521 -wd4800 -wd4996") # Disables warning messages listed above 
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DVC_EXTRALEAN -DWIN32_LEAN_AND_MEAN") # Excludes rarely used services and headers from compilation
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES") # ???
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -TP") # Globally treat all source files as C++
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Z7") # Produces obj file with symbolic debugging info; no pdb file
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Od") # Turns off all optimization
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Ob0") # Disables inline expansion
        
		# LINKER FLAGS
		SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -nologo") # Suppresses compiler copyright notice and informational messages
        SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -F2097152") # Increases stack size to ~ 2 MiB
        		
    endif () #MSVC
	
    # g++
    if ( CMAKE_COMPILER_IS_GNUCXX )
    
        # RELEASE
        if ( CMAKE_BUILD_TYPE_B MATCHES RELEASE )
        
            # COMPILER FLAGS
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe") # Enable using pipes instead of temporary files during compilation -> assembling 
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11") # Enable C++11 features in g++
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic") # Turn on warnings about constructs/situations that may be non-portable or outside of the standard
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter") # Turn on warnings (all, extra, "???don't warn about unused parameters???")
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ffor-scope") # Variables in for-init statements are scoped to the for loop itself
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmessage-length=0") # Disable automatic line wrapping on error messages
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m64") # Build a 64-bit binary
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native") # Enables optimizations for the local machine .. can result in binaries that don't run on other machines .. -mtune is safer, but less agressive optimization
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3") # Optimization: level 3  (-Ofast is available, but disregards compliance to IEEE floating point standards so accuracy can be an issue)
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DNDEBUG") # Define the "NDEBUG" flag; disables standard-C assertions
            #SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -s") # Strip the binary down .. necessary here??? .. linker flag right?
            
            # LINKER FLAGS
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pipe") # Enable using pipes instead of temporary files during compilation -> assembling 
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wall") # Turn on all linker warnings
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -s") # Strip the binary down
            
        endif () # RELEASE
        
        # DEBUG
        if ( CMAKE_BUILD_TYPE_B MATCHES DEBUG )
        
            # COMPILER FLAGS
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe") # Enable using pipes instead of temporary files during compilation -> assembling 
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11") # Enable C++11 features in g++
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic") # Turn on warnings about constructs/situations that may be non-portable or outside of the standard
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter") # Turn on warnings (all, extra, "???don't warn about unused parameters???")
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ffor-scope") # Variables in for-init statements are scoped to the for loop itself
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmessage-length=0") # Disable automatic line wrapping on error messages
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m64") # Build a 64-bit binary
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native") # Enables optimizations for the local machine .. can result in binaries that don't run on other machines .. -mtune is safer, but less agressive optimization
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ffloat-store") # Ensures floating point calculations are equivalent with different optimization levels, a very (unnecessarily???) high level of accuracy is attained
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsignaling-nans") # Disable optimizations that may have concealed NaN behavior
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DOBJEXXFCL_FARRAY_INIT -DOBJEXXFCL_FARRAY_INIT_DEBUG") # Objexx DEFinition
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0") # Turns off all optimization
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ggdb") # Produces debugging information specifically for gdb
            
            # LINKER FLAGS
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pipe") # Enable using pipes instead of temporary files during compilation -> assembling 
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wall") # Turn on all linker warnings
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -ggdb") # Produces debugging information specifically for gdb
            
        endif () # DEBUG
        
    endif () # g++
	
    # INTEL
    if ( ${CMAKE_CXX_COMPILER_ID} STREQUAL "Intel" ) # ??? Not sure if this is a good match as I don't have intel C++ (http://www.cmake.org/pipermail/cmake/2011-May/044445.html)
        
        # Warnings ignored:
        #  1786: Use of deprecated items
        #  2259: Non-pointer conversion from "type" to "type" may lose significant bits
        
        # RELEASE
        if ( CMAKE_BUILD_TYPE_B MATCHES RELEASE )
            
            # COMPILER FLAGS
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /nologo") # Suppresses compiler copyright notice and informational messages
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qstd=c++11") # Enabled C++11 features; ON by default in Visual Studio 2010 and 2012
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qcxx-features") # ???
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Wall") # Enable "all" warnings
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Wp64") # Look for warnings that may occur related to 64-bit code compilation
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qdiag-disable:1786,2259") # Disable warnings listed above
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /DVC_EXTRALEAN /DWIN32_LEAN_AND_MEAN") # Excludes rarely used services and headers from compilation
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O3") # Optimization level 3
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /DNDEBUG") # Define the "NDEBUG" flag; disables standard-C assertions
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /fp:fast") # Enables more aggressive optimizations on floating-point data
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qprec-div-") # ???If this is equivalent to /Qno-prec-div, it disables the improved division accuracy in favor of speed
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qip") # Enables inter-procedural optimnization within a single file
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qoption,c,-ip_ninl_max_stats=500") # Sets the max increase in the # of intermediate language statements to 500 for each function
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qoption,c,-ip_ninl_max_total_stats=5000") # Sets the total max increase in the # of intermediate language statements to 5000
            
            # LINKER FLAGS
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /nologo") # Suppresses linker copyright notice and informational messages
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /F2097152") # Increases stack size to ~ 2 MiB
            
        endif () # RELEASE
        
        # DEBUG
        if ( CMAKE_BUILD_TYPE_B MATCHES DEBUG )
            
            # COMPILER FLAGS
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /nologo") # Suppresses compiler copyright notice and informational messages
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qstd=c++11") # Enabled C++11 features; ON by default in Visual Studio 2010 and 2012
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qcxx-features") # ???
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Wall") # Enable "all" warnings
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Wp64") # Look for warnings that may occur related to 64-bit code compilation
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qdiag-disable:1786,2259") # Disable warnings listed above
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /DVC_EXTRALEAN /DWIN32_LEAN_AND_MEAN") # Excludes rarely used services and headers from compilation
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Od") # No optimization
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Z7") # Generates full debugging information
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /traceback") # Generates trackback information 
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /check:stack,uninit") # Enables runtime checking of the stack (buffer over and underruns; pointer verification) and uninitialized variables
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Gs0") # ??? Disable/Enable stack checking
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qfp-stack-check") # Tells the compiler to generate extra code after every function call to ensure fp stack is as expected
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Qtrapuv") # ??? Initializes variables with NaN
            
            # LINKER FLAGS
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /nologo") # Suppresses linker copyright notice and informational messages
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /F2097152") # Increases stack size to ~ 2 MiB
            
        endif () # DEBUG
            
    endif () # INTEL
    
endif () # WINDOWS

# **** LINUX-ish **** #
# Unix-based, not apple and not cygwin
if ( UNIX AND (NOT APPLE) AND (NOT WIN32) ) 
    
    # g++
    if ( CMAKE_COMPILER_IS_GNUCXX )
    
        # RELEASE
        if ( CMAKE_BUILD_TYPE_B MATCHES RELEASE )
        
            # RELEASE MODE FLAGS
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe") # Enable using pipes instead of temporary files during compilation -> assembling 
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11") # Enable C++11 features in g++
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic") # Turn on warnings about constructs/situations that may be non-portable or outside of the standard
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter") # Turn on warnings (all, extra, "???don't warn about unused parameters???")
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ffor-scope") # Variables in for-init statements are scoped to the for loop itself
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmessage-length=0") # Disable automatic line wrapping on error messages
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m64") # Build a 64-bit binary
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native") # Enables optimizations for the local machine .. can result in binaries that don't run on other machines .. -mtune is safer, but less agressive optimization
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3") # Optimization: level 3  (-Ofast is available, but disregards compliance to IEEE floating point standards so accuracy can be an issue)
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DNDEBUG") # Define the "NDEBUG" flag; disables standard-C assertions
            #SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -s") # Strip the binary down .. necessary here??? .. linker flag right?
            
            # LINKER FLAGS
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pipe") # Enable using pipes instead of temporary files during compilation -> assembling 
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wall") # Turn on all linker warnings
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -s") # Strip the binary down
            
        endif () # RELEASE
        
        # DEBUG
        if ( CMAKE_BUILD_TYPE_B MATCHES DEBUG )
        
            # DEBUG MODE FLAGS
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe") # Enable using pipes instead of temporary files during compilation -> assembling 
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11") # Enable C++11 features in g++
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic") # Turn on warnings about constructs/situations that may be non-portable or outside of the standard
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter") # Turn on warnings (all, extra, "???don't warn about unused parameters???")
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ffor-scope") # Variables in for-init statements are scoped to the for loop itself
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmessage-length=0") # Disable automatic line wrapping on error messages
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m64") # Build a 64-bit binary
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native") # Enables optimizations for the local machine .. can result in binaries that don't run on other machines .. -mtune is safer, but less agressive optimization
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ffloat-store") # Ensures floating point calculations are equivalent with different optimization levels, a very (unnecessarily???) high level of accuracy is attained
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsignaling-nans") # Disable optimizations that may have concealed NaN behavior
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DOBJEXXFCL_FARRAY_INIT -DOBJEXXFCL_FARRAY_INIT_DEBUG") # Objexx DEFinition
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0") # Turns off all optimization
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ggdb") # Produces debugging information specifically for gdb
            
            # LINKER FLAGS
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pipe") # Enable using pipes instead of temporary files during compilation -> assembling 
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wall") # Turn on all linker warnings
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -ggdb") # Produces debugging information specifically for gdb
            
        endif () # DEBUG
        
    endif () # g++
    
    # INTEL
    if ( CMAKE_CXX_COMPILER MATCHES "icpc" )
    
        MESSAGE ( "Linux Intel C++ flags not yet implemented" )
    
    endif () # INTEL
    
endif () # Linux
    
# **** APPLE **** #
if ( APPLE )

    MESSAGE ( "Mac C++ flags not yet implemented" )

endif () # Apple
