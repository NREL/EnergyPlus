
# Compiler-agnostic compiler flags first
ADD_CXX_DEFINITIONS("-DOBJEXXFCL_ALIGN=64") # Align ObjexxFCL arrays to 64B
ADD_CXX_DEBUG_DEFINITIONS("-DOBJEXXFCL_ARRAY_INIT_DEBUG") # Initialize ObjexxFCL arrays to aid debugging

# Make sure expat is compiled as a static library
ADD_DEFINITIONS("-DXML_STATIC")

IF ( MSVC AND NOT ( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel" ) ) # Visual C++ (VS 2013)

    # Disabled Warnings: Enable some of these as more serious warnings are addressed
    #  4068 Unknown pragma
    #  4101 Unreferenced local variable
    #  4102 Unreferenced label
    #  4244 Narrowing conversions
    #  4258 Definition from the loop is ignored
    #  4355 Passing this pointer in class initializer (object is incomplete so bases/members can only use this in limited ways)
    #  4996 Deprecated functions (/D_SCL_SECURE_NO_WARNINGS /D_CRT_SECURE_NO_WARNINGS /D_CRT_NONSTDC_NO_WARNINGS)

    # need to figure out how to set this to avoid the major slow-down in debugging:
    # Configuration Properties ->Debugging -> Environment, use drop-down list to choose <Edit> and type _NO_DEBUG_HEAP=1 then click OK

    # COMPILER FLAGS
    ADD_CXX_DEFINITIONS("/nologo")
    ADD_CXX_DEFINITIONS("/EHsc")
    ADD_CXX_DEFINITIONS("/MP") # Enables multi-processor compilation of source within a single project
    STRING (REGEX REPLACE "/W3" "/W1" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}") # Increase to /W2 then /W3 as more serious warnings are addressed (using regex to avoid VC override warnings)

    ADD_CXX_DEFINITIONS("/wd4068 /wd4101 /wd4102 /wd4244 /wd4258 /wd4355 /wd4996") # Disables warning messages listed above
    ADD_CXX_DEFINITIONS("/DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
    ADD_CXX_DEFINITIONS("/DWIN32_LEAN_AND_MEAN") # Excludes rarely used services and headers from compilation

    # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
    ADD_CXX_RELEASE_DEFINITIONS("/GS-") # Disable buffer overrun checks for performance in release mode

    # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
    ADD_CXX_DEBUG_DEFINITIONS("/Ob0") # Disable inlining
    ADD_CXX_DEBUG_DEFINITIONS("/RTCsu") # Runtime checks

ELSEIF ( CMAKE_COMPILER_IS_GNUCXX OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" ) # g++/Clang
    option(ENABLE_THREAD_SANITIZER "Enable thread sanitizer testing in gcc/clang" FALSE)
    set(LINKER_FLAGS "")
    if (ENABLE_THREAD_SANITIZER)
      ADD_CXX_DEFINITIONS("-fsanitize=thread")
      ADD_DEFINITIONS("-ggdb -fno-omit-frame-pointer")
      set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=thread -ggdb")
    endif()

    option(ENABLE_ADDRESS_SANITIZER "Enable address sanitizer testing in gcc/clang" FALSE)
    if (ENABLE_ADDRESS_SANITIZER)
      ADD_CXX_DEFINITIONS("-fsanitize=address")
      ADD_DEFINITIONS("-ggdb -fno-omit-frame-pointer")
      set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=address -ggdb")
    endif()

    option(ENABLE_MEMORY_SANITIZER "Enable reads of unintialized memory sanitizer testing in gcc/clang" FALSE)
    if (ENABLE_MEMORY_SANITIZER)
      ADD_CXX_DEFINITIONS("-fsanitize=memory")
      ADD_DEFINITIONS("-ggdb -fno-omit-frame-pointer")
      set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=memory -ggdb")
    endif()

    option(ENABLE_UNDEFINED_SANITIZER "Enable undefined behavior sanitizer testing in gcc/clang" FALSE)
    if (ENABLE_UNDEFINED_SANITIZER)
      ADD_CXX_DEFINITIONS("-fsanitize=undefined")
      ADD_DEFINITIONS("-ggdb -fno-omit-frame-pointer")
      set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=undefined -ggdb")
    endif()

    option(ENABLE_COVERAGE "Enable Coverage Reporting in GCC" FALSE)
    if (ENABLE_COVERAGE)
      ADD_DEFINITIONS("--coverage -O0")
      set(LINKER_FLAGS "${LINKER_FLAGS} --coverage")
    endif()

    mark_as_advanced(ENABLE_THREAD_SANITIZER ENABLE_ADDRESS_SANITIZER ENABLE_UNDEFINED_SANITIZER)

    if(CMAKE_HOST_UNIX)
      if(NOT APPLE)
        set(LINKER_FLAGS "${LINKER_FLAGS} -pthread")
        ADD_DEFINITIONS("-pthread")
      endif()
    endif()

    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LINKER_FLAGS}")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${LINKER_FLAGS}")
    set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${LINKER_FLAGS}")

    # COMPILER FLAGS
    ADD_CXX_DEFINITIONS("-pipe") # Faster compiler processing
    ADD_CXX_DEFINITIONS("-std=c++11") # Enable C++11 features in g++
    ADD_CXX_DEFINITIONS("-pedantic") # Turn on warnings about constructs/situations that may be non-portable or outside of the standard
    ADD_CXX_DEFINITIONS("-ffor-scope")
    ADD_CXX_DEFINITIONS("-Wall -Wextra") # Turn on warnings
    ADD_CXX_DEFINITIONS("-Wno-unknown-pragmas")
    if ( CMAKE_COMPILER_IS_GNUCXX ) # g++
      ADD_CXX_DEFINITIONS("-Wno-unused-but-set-parameter -Wno-unused-but-set-variable") # Suppress unused-but-set warnings until more serious ones are addressed
      ADD_CXX_DEFINITIONS("-Wno-maybe-uninitialized")
    elseif( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" )
      ADD_CXX_DEFINITIONS("-Wno-invalid-source-encoding")
    endif()

    # ADDITIONAL GCC-SPECIFIC FLAGS
    if ( CMAKE_COMPILER_IS_GNUCXX ) # g++
      ADD_CXX_DEBUG_DEFINITIONS("-ffloat-store") # Improve debug run solution stability
      ADD_CXX_DEBUG_DEFINITIONS("-fsignaling-nans") # Disable optimizations that may have concealed NaN behavior
      ADD_CXX_DEBUG_DEFINITIONS("-D_GLIBCXX_DEBUG") # Standard container debug mode (bounds checking, ...)
      # ADD_CXX_RELEASE_DEFINITIONS("-finline-limit=2000") # More aggressive inlining   This is causing unit test failures on Ubuntu 14.04
    endif()

  ADD_CXX_DEBUG_DEFINITIONS("-ggdb") # Produces debugging information specifically for gdb
  ADD_CXX_RELEASE_DEFINITIONS("-fno-stack-protector")
  # ADD_CXX_RELEASE_DEFINITIONS("-Ofast") # -Ofast (or -ffast-math) needed to auto-vectorize floating point loops

ELSEIF ( WIN32 AND "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel" )

    # Disabled Warnings: Enable some of these as more serious warnings are addressed
    #   161 Unrecognized pragma
    #   177 Variable declared but never referenced
    #   488 Template parameter not used ...
    #   809 Exception specification consistency warnings that fire in gtest code
    #   869 Parameter never referenced
    #  1786 Use of deprecated items
    #  2259 Non-pointer conversions may lose significant bits
    #  3280 Declaration hides variable
    # 10382 xHOST remark
    # 11074 Inlining inhibited
    # 11075 Inlining inhibited

    # COMPILER FLAGS
    ADD_CXX_DEFINITIONS("/nologo") # Skip banner text
    ADD_CXX_DEFINITIONS("/Qstd=c++11") # Specify C++11 language
    ADD_CXX_DEFINITIONS("/Qcxx-features") # Enables standard C++ features without disabling Microsoft extensions
    ADD_CXX_DEFINITIONS("/Wall") # Enable "all" warnings
    ADD_CXX_DEFINITIONS("/Qdiag-disable:161,177,488,809,869,1786,2259,3280,10382,11074,11075") # Disable warnings listed above
    ADD_CXX_DEFINITIONS("/DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
    ADD_CXX_DEFINITIONS("/DWIN32_LEAN_AND_MEAN") # Excludes rarely used services and headers from compilation

    # Optimization options that had no significant benefit for EnergyPlus
    #  /Qipo instead of /Qip
    #  /Qopt-prefetch
    #  /Qparallel
    #  /Qunroll-aggressive

    # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
    ADD_CXX_RELEASE_DEFINITIONS("/O3") # Agressive optimization
    ADD_CXX_RELEASE_DEFINITIONS("/Qprec-div-") # Faster division
    ADD_CXX_RELEASE_DEFINITIONS("/Qansi-alias") # Better optimization via strict aliasing rules
    ADD_CXX_RELEASE_DEFINITIONS("/Qip") # Inter-procedural optimnization within a single file
    ADD_CXX_RELEASE_DEFINITIONS("/Qinline-factor:225") # Aggressive inlining
    # ADD_CXX_RELEASE_DEFINITIONS("/fp:fast=2") # Aggressive optimizations on floating-point data

    # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
    ADD_CXX_DEBUG_DEFINITIONS("/fp:source") # Use source-specified floating point precision
    ADD_CXX_DEBUG_DEFINITIONS("/Qtrapuv") # Initialize local variables to unusual values to help detect use uninitialized
    ADD_CXX_DEBUG_DEFINITIONS("/check:stack,uninit") # Enables runtime checking of the stack (buffer over and underruns; pointer verification) and uninitialized variables
    ADD_CXX_DEBUG_DEFINITIONS("/Gs0") # Enable stack checking for all functions
    ADD_CXX_DEBUG_DEFINITIONS("/GS") # Buffer overrun detection
    ADD_CXX_DEBUG_DEFINITIONS("/Qfp-stack-check") # Tells the compiler to generate extra code after every function call to ensure fp stack is as expected
    ADD_CXX_DEBUG_DEFINITIONS("/traceback") # Enables traceback on error

ELSEIF ( UNIX AND "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel" )

    # Disabled Warnings: Enable some of these as more serious warnings are addressed
    #   161 Unrecognized pragma
    #   177 Variable declared but never referenced
    #   488 Template parameter not used ...
    #   809 Exception specification consistency warnings that fire in gtest code
    #   869 Parameter never referenced
    #  1786 Use of deprecated items
    #  2259 Non-pointer conversions may lose significant bits
    #  3280 Declaration hides variable
    # 10382 xHOST remark
    # 11074 Inlining inhibited
    # 11075 Inlining inhibited

    # COMPILER FLAGS
    ADD_CXX_DEFINITIONS("-std=c++11") # Specify C++11 language
    ADD_CXX_DEFINITIONS("-Wall") # Enable "all" warnings
    ADD_CXX_DEFINITIONS("-diag-disable:161,177,488,809,869,1786,2259,3280,10382,11074,11075") # Disable warnings listed above

    IF(NOT APPLE)
      ADD_CXX_DEFINITIONS(-pthread)
    ENDIF()

    # Optimization options that had no significant benefit for EnergyPlus
    #  -ipo instead of -ip
    #  -opt-prefetch
    #  -parallel
    #  -unroll-aggressive

    # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
    ADD_CXX_RELEASE_DEFINITIONS("-O3") # Agressive optimization
    # ADD_CXX_RELEASE_DEFINITIONS("-Ofast") # More aggressive optimizations (instead of -O3) (enables -no-prec-div and -fp-model fast=2)
    ADD_CXX_RELEASE_DEFINITIONS("-no-prec-div") # Faster division (enabled by -Ofast)
    ADD_CXX_RELEASE_DEFINITIONS("-ansi-alias") # Enables more aggressive optimizations on floating-point data
    ADD_CXX_RELEASE_DEFINITIONS("-ip") # Enables inter-procedural optimnization within a single file
    ADD_CXX_RELEASE_DEFINITIONS("-inline-factor=225") # Enables more aggressive inlining

    # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
    ADD_CXX_DEBUG_DEFINITIONS("-strict-ansi") # Strict language conformance: Performance impact so limit to debug build
    ADD_CXX_DEBUG_DEFINITIONS("-fp-model source") # Use source-specified floating point precision
    ADD_CXX_DEBUG_DEFINITIONS("-ftrapuv") # Initialize local variables to unusual values to help detect use uninitialized
    ADD_CXX_DEBUG_DEFINITIONS("-check=stack,uninit") # Enables runtime checking of the stack (buffer over and underruns; pointer verification) and uninitialized variables
    ADD_CXX_DEBUG_DEFINITIONS("-fstack-security-check") # Buffer overrun detection
    ADD_CXX_DEBUG_DEFINITIONS("-fp-stack-check") # Check the floating point stack after every function call
    ADD_CXX_DEBUG_DEFINITIONS("-traceback") # Enables traceback on error

ENDIF () # COMPILER TYPE
