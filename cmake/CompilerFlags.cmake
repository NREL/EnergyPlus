# Compiler-agnostic compiler flags first
add_cxx_definitions("-DOBJEXXFCL_ALIGN=64") # Align ObjexxFCL arrays to 64B
add_cxx_debug_definitions("-DOBJEXXFCL_ARRAY_INIT_DEBUG") # Initialize ObjexxFCL arrays to aid debugging

if(NOT OPENGL_FOUND)
  add_definitions("-DEP_NO_OPENGL")
endif()

# Make sure expat is compiled as a static library
add_definitions("-DXML_STATIC")

set(CMAKE_CXX_STANDARD 17)

if(APPLE)
  # Force no auto ptr
  # TODO remove this after kiva/boost is updated to a version that supports
  # C++17
  add_definitions("-DBOOST_NO_AUTO_PTR")
endif()

if(MSVC AND NOT ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")) # Visual C++ (VS 2013)

  # COMPILER FLAGS
  add_compile_options("/nologo")
  add_compile_options("/EHsc")
  add_compile_options("/MP") # Enables multi-processor compilation of source within a single project
  string(REGEX REPLACE "/W3" "/W1" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}"
  )# Increase to /W2 then /W3 as more serious warnings are addressed (using regex to avoid VC override warnings)

  # Disabled Warnings: Enable some of these as more serious warnings are addressed
  #  4068 Unknown pragma
  #  4101 Unreferenced local variable
  #  4102 Unreferenced label
  #  4244 Narrowing conversions
  #  4258 Definition from the loop is ignored
  #  4355 Passing this pointer in class initializer (object is incomplete so bases/members can only use this in limited ways)
  #  4996 Deprecated functions (/D_SCL_SECURE_NO_WARNINGS /D_CRT_SECURE_NO_WARNINGS /D_CRT_NONSTDC_NO_WARNINGS)
  #  4503 The decorated name was longer than the compiler limit (4096), and was truncated.
  add_compile_options(
    /wd4068
    /wd4101
    /wd4102
    /wd4244
    /wd4258
    /wd4355
    /wd4996
    /wd4503) # Disables warning messages listed above

  add_definitions(/DNOMINMAX) # Avoid build errors due to STL/Windows min-max conflicts
  add_definitions(/DWIN32_LEAN_AND_MEAN) # Excludes rarely used services and headers from compilation
  #    ADD_CXX_DEFINITIONS("-d2SSAOptimizer-") # this disables this optimizer which has known major issues

  # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
  add_cxx_release_definitions("/GS-") # Disable buffer overrun checks for performance in release mode

  # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
  add_cxx_debug_definitions("/Ob0") # Disable inlining
  add_cxx_debug_definitions("/RTCsu") # Runtime checks
  add_cxx_debug_definitions("/fp:strict") # Floating point model
  add_cxx_debug_definitions("/DMSVC_DEBUG") # Triggers code in main.cc to catch floating point NaNs

elseif(
  CMAKE_COMPILER_IS_GNUCXX
  OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang"
  OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang") # g++/Clang
  option(ENABLE_THREAD_SANITIZER "Enable thread sanitizer testing in gcc/clang" FALSE)
  set(LINKER_FLAGS "")
  if(ENABLE_THREAD_SANITIZER)
    add_cxx_definitions("-fsanitize=thread")
    add_definitions("-ggdb -fno-omit-frame-pointer")
    set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=thread -ggdb")
  endif()

  option(ENABLE_ADDRESS_SANITIZER "Enable address sanitizer testing in gcc/clang" FALSE)
  if(ENABLE_ADDRESS_SANITIZER)
    add_cxx_definitions("-fsanitize=address")
    add_definitions("-ggdb -fno-omit-frame-pointer")
    set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=address -ggdb")
  endif()

  option(ENABLE_MEMORY_SANITIZER "Enable reads of unintialized memory sanitizer testing in gcc/clang" FALSE)
  if(ENABLE_MEMORY_SANITIZER)
    add_cxx_definitions("-fsanitize=memory")
    add_definitions("-ggdb -fno-omit-frame-pointer")
    set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=memory -ggdb")
  endif()

  option(ENABLE_UNDEFINED_SANITIZER "Enable undefined behavior sanitizer testing in gcc/clang" FALSE)
  if(ENABLE_UNDEFINED_SANITIZER)
    add_cxx_definitions("-fsanitize=undefined")
    add_definitions("-ggdb -fno-omit-frame-pointer")
    set(LINKER_FLAGS "${LINKER_FLAGS} -fsanitize=undefined -ggdb")
  endif()

  option(ENABLE_COVERAGE "Enable Coverage Reporting in GCC" FALSE)
  if(ENABLE_COVERAGE)
    add_definitions("--coverage -O0")
    set(LINKER_FLAGS "${LINKER_FLAGS} --coverage")
  endif()

  mark_as_advanced(ENABLE_THREAD_SANITIZER ENABLE_ADDRESS_SANITIZER ENABLE_UNDEFINED_SANITIZER)

  if(CMAKE_HOST_UNIX)
    if(NOT APPLE)
      set(LINKER_FLAGS "${LINKER_FLAGS} -pthread")
      add_definitions("-pthread")
    endif()
  endif()

  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LINKER_FLAGS}")
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${LINKER_FLAGS}")
  set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${LINKER_FLAGS}")

  # COMPILER FLAGS
  add_cxx_definitions("-pipe") # Faster compiler processing
  add_cxx_definitions("-pedantic") # Turn on warnings about constructs/situations that may be non-portable or outside of the standard
  add_cxx_definitions("-Wall -Wextra") # Turn on warnings
  add_cxx_definitions("-Wno-unknown-pragmas")
  if(CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_VERSION VERSION_GREATER 9.0)
    add_cxx_definitions("-Wno-deprecated-copy")
  endif()
  add_cxx_definitions("-Wno-attributes") # Don't warn on attributes Clang doesn't know
  add_cxx_definitions("-Wno-delete-non-virtual-dtor")
  add_cxx_definitions("-Wno-missing-braces")
  if(CMAKE_COMPILER_IS_GNUCXX) # g++
    add_cxx_definitions("-Wno-unused-but-set-parameter -Wno-unused-but-set-variable"
    )# Suppress unused-but-set warnings until more serious ones are addressed
    add_cxx_definitions("-Wno-maybe-uninitialized")
    add_cxx_definitions("-Wno-aggressive-loop-optimizations")
  elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
    add_cxx_definitions("-Wno-vexing-parse")
    add_cxx_definitions("-Wno-invalid-source-encoding")
  endif()

  # ADDITIONAL GCC-SPECIFIC FLAGS
  if(CMAKE_COMPILER_IS_GNUCXX) # g++
    add_cxx_debug_definitions("-ffloat-store") # Improve debug run solution stability
    add_cxx_debug_definitions("-fsignaling-nans") # Disable optimizations that may have concealed NaN behavior
    add_cxx_debug_definitions("-D_GLIBCXX_DEBUG") # Standard container debug mode (bounds checking, ...)
    # ADD_CXX_RELEASE_DEFINITIONS("-finline-limit=2000") # More aggressive inlining   This is causing unit test failures on Ubuntu 14.04
  endif()

  add_cxx_debug_definitions("-ggdb") # Produces debugging information specifically for gdb
  add_cxx_release_definitions("-fno-stack-protector")
  # ADD_CXX_RELEASE_DEFINITIONS("-Ofast") # -Ofast (or -ffast-math) needed to auto-vectorize floating point loops

elseif(WIN32 AND "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")

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
  add_cxx_definitions("/nologo") # Skip banner text
  add_cxx_definitions("/Qcxx-features") # Enables standard C++ features without disabling Microsoft extensions
  add_cxx_definitions("/Wall") # Enable "all" warnings
  add_cxx_definitions("/Qdiag-disable:161,177,488,809,869,1786,2259,3280,10382,11074,11075") # Disable warnings listed above
  add_cxx_definitions("/DNOMINMAX") # Avoid build errors due to STL/Windows min-max conflicts
  add_cxx_definitions("/DWIN32_LEAN_AND_MEAN") # Excludes rarely used services and headers from compilation

  # Optimization options that had no significant benefit for EnergyPlus
  #  /Qipo instead of /Qip
  #  /Qopt-prefetch
  #  /Qparallel
  #  /Qunroll-aggressive

  # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
  add_cxx_release_definitions("/O3") # Agressive optimization
  add_cxx_release_definitions("/Qprec-div-") # Faster division
  add_cxx_release_definitions("/Qansi-alias") # Better optimization via strict aliasing rules
  add_cxx_release_definitions("/Qip") # Inter-procedural optimnization within a single file
  add_cxx_release_definitions("/Qinline-factor:225") # Aggressive inlining
  # ADD_CXX_RELEASE_DEFINITIONS("/fp:fast=2") # Aggressive optimizations on floating-point data

  # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
  add_cxx_debug_definitions("/fp:source") # Use source-specified floating point precision
  add_cxx_debug_definitions("/Qtrapuv") # Initialize local variables to unusual values to help detect use uninitialized
  add_cxx_debug_definitions("/check:stack,uninit"
  )# Enables runtime checking of the stack (buffer over and underruns; pointer verification) and uninitialized variables
  add_cxx_debug_definitions("/Gs0") # Enable stack checking for all functions
  add_cxx_debug_definitions("/GS") # Buffer overrun detection
  add_cxx_debug_definitions("/Qfp-stack-check"
  )# Tells the compiler to generate extra code after every function call to ensure fp stack is as expected
  add_cxx_debug_definitions("/traceback") # Enables traceback on error

elseif(UNIX AND "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")

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
  add_cxx_definitions("-Wall") # Enable "all" warnings
  add_cxx_definitions("-diag-disable:161,177,488,809,869,1786,2259,3280,10382,11074,11075") # Disable warnings listed above

  if(NOT APPLE)
    add_cxx_definitions(-pthread)
  endif()

  # Optimization options that had no significant benefit for EnergyPlus
  #  -ipo instead of -ip
  #  -opt-prefetch
  #  -parallel
  #  -unroll-aggressive

  # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
  add_cxx_release_definitions("-O3") # Agressive optimization
  # ADD_CXX_RELEASE_DEFINITIONS("-Ofast") # More aggressive optimizations (instead of -O3) (enables -no-prec-div and -fp-model fast=2)
  add_cxx_release_definitions("-no-prec-div") # Faster division (enabled by -Ofast)
  add_cxx_release_definitions("-ansi-alias") # Enables more aggressive optimizations on floating-point data
  add_cxx_release_definitions("-ip") # Enables inter-procedural optimnization within a single file
  add_cxx_release_definitions("-inline-factor=225") # Enables more aggressive inlining

  # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
  add_cxx_debug_definitions("-strict-ansi") # Strict language conformance: Performance impact so limit to debug build
  add_cxx_debug_definitions("-fp-model source") # Use source-specified floating point precision
  add_cxx_debug_definitions("-ftrapuv") # Initialize local variables to unusual values to help detect use uninitialized
  add_cxx_debug_definitions("-check=stack,uninit"
  )# Enables runtime checking of the stack (buffer over and underruns; pointer verification) and uninitialized variables
  add_cxx_debug_definitions("-fstack-security-check") # Buffer overrun detection
  add_cxx_debug_definitions("-fp-stack-check") # Check the floating point stack after every function call
  add_cxx_debug_definitions("-traceback") # Enables traceback on error

endif() # COMPILER TYPE

# Add Color Output if Using Ninja:
# Wave to do it before the folders are imported etc (here is the perfect place)

# We use "add_compile_options" instead of just appending to CXX_FLAGS
# That way it'll work for pretty much everything including Fortran stuff
macro(AddFlagIfSupported flag test)
  check_cxx_compiler_flag(${flag} ${test})
  if(${${test}})
    message(STATUS "Adding ${flag}")
    # On Mac with Ninja (kitware binary for fortran support) and brew gfortran, I get build errors due to this flag.
    if(("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang") AND BUILD_FORTRAN)
      add_compile_options($<$<NOT:$<COMPILE_LANGUAGE:Fortran>>:${flag}>)
    else()
      add_compile_options("${flag}")
    endif()
  else()
    message(STATUS "Flag ${flag} isn't supported")
  endif()
endmacro()

if("Ninja" STREQUAL ${CMAKE_GENERATOR})
  include(CheckCXXCompilerFlag)
  # Clang
  if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
    addflagifsupported(-fcolor-diagnostics COMPILER_SUPPORTS_fdiagnostics_color)
  endif()

  # g++
  if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    addflagifsupported(-fdiagnostics-color=always COMPILER_SUPPORTS_fdiagnostics_color)

    # On some older gcc, it doesn't say that it's supported, but it works anyways
    if(NOT COMPILER_SUPPORTS_fdiagnostics_color)
      message(STATUS "Forcing -fdiagnostics-color=always")
      add_compile_options(-fdiagnostics-color=always)
    endif()

  endif()
endif()

# Xcode/Ninja generators undefined MAKE
if(CMAKE_GENERATOR MATCHES "Make")
  set(MAKE "$(MAKE)")
else()
  set(MAKE make)
endif()
