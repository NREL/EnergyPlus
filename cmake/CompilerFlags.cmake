# for now, only copying the 64-bit, debug and release build compiler options, and only setting cxx flags, not fortran or C, and not for Intel or Clang

# convert the build type string to upper case for comparisons
#  http://www.cmake.org/pipermail/cmake/2012-June/050651.html
if ( NOT MSVC )
	STRING( TOUPPER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE_B )
endif ()

# **** WINDOWS **** #
if (WIN32 AND NOT UNIX)  # UNIX clause required to filter out cygwin installations
    
    # need to figure out how to set this to avoid the major slow-down in debugging:
    # Configuration Properties ->Debugging -> Environment, use drop-down list to choose <Edit> and type _NO_DEBUG_HEAP=1 then click OK 
    
    # visual c++ (VS 2013)
    if (MSVC)    
	
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
        SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -nologo") #Suppresses compiler copyright notice and informational messages
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
        SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -nologo")
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -MP")
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Za")
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -EHsc")
		#SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -GR")
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -wd4101 -wd4102 -wd4244 -wd4258 -wd4305 -wd4355 -wd4521 -wd4800 -wd4996")
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DVC_EXTRALEAN -DWIN32_LEAN_AND_MEAN")
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DNOMINMAX")
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES")
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -TP")
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Z7") # Produces obj file with symbolic debugging info; no pdb file
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Od") # Turns off all optimization
		SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Ob0") # Disables inline expansion
        
		# LINKER FLAGS
		SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -nologo -F2097152")  # was LDFLAGS
        		
    endif ()
	
    # g++
    if (CMAKE_COMPILER_IS_GNUCXX)
        if (CMAKE_BUILD_TYPE_B MATCHES RELEASE)
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe -std=c++11 -pedantic -Wall -Wextra -Wno-unused-parameter -ffor-scope -fmessage-length=0 -m64 -march=native -O3 -DNDEBUG -s")
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pipe -Wall -s")
        endif ()
        if (CMAKE_BUILD_TYPE_B MATCHES DEBUG)
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe -std=c++11 -pedantic -Wall -Wextra -Wno-unused-parameter -ffor-scope -fmessage-length=0 -m64 -march=native -ffloat-store -fsignaling-nans -DOBJEXXFCL_FARRAY_INIT -DOBJEXXFCL_FARRAY_INIT_DEBUG -O0 -ggdb")
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pipe -Wall -ggdb")
        endif ()
    endif ()
	
endif ()

# **** LINUX-ish **** #
if (UNIX AND (NOT APPLE) AND (NOT WIN32)) # Unix-based, not apple and not cygwin
    # g++
    if (CMAKE_COMPILER_IS_GNUCXX)
        if (CMAKE_BUILD_TYPE_B MATCHES RELEASE)
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe -std=c++11 -pedantic -Wall -Wextra -Wno-unused-parameter -ffor-scope -fmessage-length=0 -m64 -march=native -O3 -DNDEBUG -s")
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pipe -Wall -s")
        endif ()
        if (CMAKE_BUILD_TYPE_B MATCHES DEBUG)
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe -std=c++11 -pedantic -Wall -Wextra -Wno-unused-parameter -ffor-scope -fmessage-length=0 -m64 -march=native -ffloat-store -fsignaling-nans -DOBJEXXFCL_FARRAY_INIT -DOBJEXXFCL_FARRAY_INIT_DEBUG -O0 -ggdb")
            SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pipe -Wall -ggdb")
        endif ()
    endif ()

endif ()
    
