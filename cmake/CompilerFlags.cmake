# for now, only copying the 64-bit, debug and release build compiler options, and only setting cxx flags, not fortran or C, and not for Intel or Clang

# convert the build type string to upper case for comparisons
#  http://www.cmake.org/pipermail/cmake/2012-June/050651.html
if ( NOT MSVC )
	STRING( TOUPPER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE_B )
endif ()

# **** WINDOWS **** #
if (WIN32 AND NOT UNIX)  # UNIX clause required to filter out cygwin installations
    
    # need to figure out how to set these:
    # Configuration Properties ->Debugging -> Environment, use drop-down list to choose <Edit> and type _NO_DEBUG_HEAP=1 then click OK 
    # C/C++ -> General -> Multi-processor Compilation use drop-down to choose Yes (/MP) 
    # C/C++ -> Code Generation -> Basic Runtime Checks use drop-down to choose Default 
    # C/C++ -> Language -> Disable Language Extensions use drop-down to choose Yes (/Za) -- IN USE BELOW
    
    # visual c++ (VS 2013)
    if (MSVC)    
        # Disabled Warnings:
        #  4244  Narrowing conversions
        #  4258  Definition from the loop is ignored
        #  4355  Passing this pointer in class initializer (object is incomplete so bases/members can only use this in limited ways)
        #  4996  Deprecated" STL functions (that MS has safer, non-std alternatives for)
        SET(CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -nologo -Za -EHsc -GR -wd4244 -wd4258 -wd4355 -wd4996 -DVC_EXTRALEAN -DWIN32_LEAN_AND_MEAN -DNOMINMAX -D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES -TP -O2 -DNDEBUG")
        # -RTCc gave exe blocked by Windows 8.1 Defender with ostringstream
        SET(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -nologo -Za -EHsc -GR -wd4244 -wd4258 -wd4355 -wd4996 -DVC_EXTRALEAN -DWIN32_LEAN_AND_MEAN -DNOMINMAX -D_CRT_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_DEPRECATE -D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES -TP -Z7 -Od -Ob0 -RTCsu")
        SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -nologo -F2097152")  # was LDFLAGS
        SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -link -NODEFAULTLIB:libcd")  # was LINKFLAGS
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
    
