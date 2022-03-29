# Compiler-agnostic compiler flags first
target_compile_definitions(project_options INTERFACE -DOBJEXXFCL_ALIGN=64) # Align ObjexxFCL arrays to 64B
target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:-DOBJEXXFCL_ARRAY_INIT_DEBUG>) # Initialize ObjexxFCL arrays to aid debugging

if(NOT OPENGL_FOUND)
  target_compile_definitions(project_options INTERFACE -DEP_NO_OPENGL)
endif()

# Make sure expat is compiled as a static library
target_compile_definitions(project_options INTERFACE -DXML_STATIC)
if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  option(BUILD_TIME_TRACE "Enable -ftime-trace for investigating build times on clang" OFF)
  mark_as_advanced(BUILD_TIME_TRACE)
  if (BUILD_TIME_TRACE)
    target_compile_options(project_options INTERFACE -ftime-trace)
  endif()
endif()

if(MSVC AND NOT ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")) # Visual C++ (VS 2013)

  # COMPILER FLAGS
  target_compile_options(project_options INTERFACE /bigobj)
  target_compile_options(project_options INTERFACE /nologo)
  target_compile_options(project_options INTERFACE /EHsc)
  target_compile_options(project_options INTERFACE /MP) # Enables multi-processor compilation of source within a single project
  # string(REGEX REPLACE "/W3" "/W1" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}"
  # )# Increase to /W2 then /W3 as more serious warnings are addressed (using regex to avoid VC override warnings)

  # Disabled Warnings: Enable some of these as more serious warnings are addressed
  #  4068 Unknown pragma
  #  4101 Unreferenced local variable
  #  4102 Unreferenced label
  #  4244 Narrowing conversions
  #  4258 Definition from the loop is ignored
  #  4267 Narrowing conversions
  #  4355 Passing this pointer in class initializer (object is incomplete so bases/members can only use this in limited ways)
  #  4996 Deprecated functions (/D_SCL_SECURE_NO_WARNINGS /D_CRT_SECURE_NO_WARNINGS /D_CRT_NONSTDC_NO_WARNINGS)
  #  4503 The decorated name was longer than the compiler limit (4096), and was truncated.
  target_compile_options(
    project_warnings
    INTERFACE /wd4068
              /wd4101
              /wd4102
              /wd4244
              /wd4258
              /wd4267
              /wd4355
              /wd4996
              /wd4503) # Disables warning messages listed above

  target_compile_definitions(project_options INTERFACE NOMINMAX) # Avoid build errors due to STL/Windows min-max conflicts
  target_compile_definitions(project_options INTERFACE WIN32_LEAN_AND_MEAN) # Excludes rarely used services and headers from compilation
  #    ADD_CXX_DEFINITIONS("-d2SSAOptimizer-") # this disables this optimizer which has known major issues

  # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:/GS->) # Disable buffer overrun checks for performance in release mode

  # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:/Ob0>) # Disable inlining
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:/RTCsu>) # Runtime checks
  target_compile_options(project_fp_options INTERFACE $<$<CONFIG:Debug>:/fp:strict>) # Floating point model
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:/DMSVC_DEBUG>) # Triggers code in main.cc to catch floating point NaNs
elseif(CMAKE_COMPILER_IS_GNUCXX OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang") # g++/Clang

  # COMPILER FLAGS
  target_compile_options(project_options INTERFACE -pipe) # Faster compiler processing
  target_compile_options(project_warnings INTERFACE -Wpedantic
  )# Turn on warnings about constructs/situations that may be non-portable or outside of the standard
  target_compile_options(project_warnings INTERFACE -Wall -Wextra) # Turn on warnings
  target_compile_options(project_warnings INTERFACE -Wno-unknown-pragmas)
  if(CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_VERSION VERSION_GREATER 9.0)
    target_compile_options(project_warnings INTERFACE -Wno-deprecated-copy)
  endif()
  target_compile_options(project_warnings INTERFACE -Wno-attributes) # Don't warn on attributes Clang doesn't know
  target_compile_options(project_warnings INTERFACE -Wno-delete-non-virtual-dtor)
  target_compile_options(project_warnings INTERFACE -Wno-missing-braces)
  if(CMAKE_COMPILER_IS_GNUCXX) # g++
    # Suppress unused-but-set warnings until more serious ones are addressed
    target_compile_options(project_warnings INTERFACE -Wno-unused-but-set-parameter -Wno-unused-but-set-variable)
    target_compile_options(project_warnings INTERFACE -Wno-maybe-uninitialized)
    target_compile_options(project_warnings INTERFACE -Wno-aggressive-loop-optimizations)
  elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 13.0)
      # Suppress unused-but-set warnings until more serious ones are addressed
      target_compile_options(project_warnings INTERFACE -Wno-unused-but-set-parameter -Wno-unused-but-set-variable)
    endif()
    target_compile_options(project_warnings INTERFACE -Wno-vexing-parse)
    target_compile_options(project_warnings INTERFACE -Wno-invalid-source-encoding)
  endif()

  # ADDITIONAL GCC-SPECIFIC FLAGS
  if(CMAKE_COMPILER_IS_GNUCXX) # g++
    target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:-ffloat-store>) # Improve debug run solution stability
    target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:-fsignaling-nans>) # Disable optimizations that may have concealed NaN behavior
    target_compile_definitions(project_options INTERFACE $<$<CONFIG:Debug>:_GLIBCXX_DEBUG>) # Standard container debug mode (bounds checking, ...>)
    # ADD_CXX_RELEASE_DEFINITIONS("-finline-limit=2000") # More aggressive inlining   This is causing unit test failures on Ubuntu 14.04
  endif()

  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:-fno-stack-protector>)
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
  target_compile_options(project_options INTERFACE /nologo) # Skip banner text
  target_compile_options(project_options INTERFACE /Qcxx-features) # Enables standard C++ features without disabling Microsoft extensions
  target_compile_options(project_options INTERFACE /Wall) # Enable all warnings

  target_compile_options(project_options INTERFACE /Qdiag-disable:161,177,488,809,869,1786,2259,3280,10382,11074,11075)
  target_compile_definitions(project_options INTERFACE /DNOMINMAX) # Avoid build errors due to STL/Windows min-max conflicts
  target_compile_definitions(project_options INTERFACE /DWIN32_LEAN_AND_MEAN) # Excludes rarely used services and headers from compilation

  # Optimization options that had no significant benefit for EnergyPlus
  #  /Qipo instead of /Qip
  #  /Qopt-prefetch
  #  /Qparallel
  #  /Qunroll-aggressive

  # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:/O3>) # Agressive optimization
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:/Qprec-div->) # Faster division
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:/Qansi-alias>) # Better optimization via strict aliasing rules
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:/Qip>) # Inter-procedural optimnization within a single file
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:/Qinline-factor:225>) # Aggressive inlining
  # target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:/fp:fast=2>) # Aggressive optimizations on floating-point data

  # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
  target_compile_options(project_fp_options INTERFACE $<$<CONFIG:Debug>:/fp:source>) # Use source-specified floating point precision

  # Initialize local variables to unusual values to help detect use uninitialized
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:/Qtrapuv>)

  # Enables runtime checking of the stack (buffer over and underruns; pointer verification>) and uninitialized variables
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:/check:stack,uninit)
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:/Gs0>) # Enable stack checking for all functions
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:/GS>) # Buffer overrun detection

  # Tells the compiler to generate extra code after every function call to ensure fp stack is as expected
  target_compile_options(project_fp_options INTERFACE $<$<CONFIG:Debug>:/Qfp-stack-check>)
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:/traceback>) # Enables traceback on error

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
  target_compile_options(project_warnings INTERFACE -Wall) # Enable all warnings
  target_compile_options(project_warnings INTERFACE -diag-disable:161,177,488,809,869,1786,2259,3280,10382,11074,11075)# Disable warnings listed above

  # Optimization options that had no significant benefit for EnergyPlus
  #  -ipo instead of -ip
  #  -opt-prefetch
  #  -parallel
  #  -unroll-aggressive

  # ADDITIONAL RELEASE-MODE-SPECIFIC FLAGS
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:-O3>) # Agressive optimization
  # ADD_CXX_RELEASE_DEFINITIONS(-Ofast) # More aggressive optimizations (instead of -O3) (enables -no-prec-div and -fp-model fast=2)
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:-no-prec-div) # Faster division (enabled by -Ofast>)
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:-ansi-alias>) # Enables more aggressive optimizations on floating-point data
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:-ip>) # Enables inter-procedural optimnization within a single file
  target_compile_options(project_options INTERFACE $<$<CONFIG:Release>:-inline-factor=225>) # Enables more aggressive inlining

  # ADDITIONAL DEBUG-MODE-SPECIFIC FLAGS
  # Strict language conformance: Performance impact so limit to debug build
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:-strict-ansi>)
  target_compile_options(project_fp_options INTERFACE $<$<CONFIG:Debug>:-fp-model source>) # Use source-specified floating point precision
  # Initialize local variables to unusual values to help detect use uninitialized
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:-ftrapuv>)
  # Enables runtime checking of the stack (buffer over and underruns; pointer verification>) and uninitialized variables
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:-check=stack,uninit)
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:-fstack-security-check>) # Buffer overrun detection
  target_compile_options(project_fp_options INTERFACE $<$<CONFIG:Debug>:-fp-stack-check>) # Check the floating point stack after every function call
  target_compile_options(project_options INTERFACE $<$<CONFIG:Debug>:-traceback>) # Enables traceback on error

endif() # COMPILER TYPE

# Add Color Output if Using Ninja:
# Wave to do it before the folders are imported etc (here is the perfect place)

# We use "add_compile_options" instead of just appending to CXX_FLAGS
# That way it'll work for pretty much everything including Fortran stuff
macro(add_flag_if_supported flag test)
  check_cxx_compiler_flag(${flag} ${test})
  if(${${test}})
    message(STATUS "Adding ${flag}")
    target_compile_options(project_options INTERFACE "${flag}")
  else()
    message(STATUS "Flag ${flag} isn't supported")
  endif()
endmacro()

if("Ninja" STREQUAL ${CMAKE_GENERATOR})
  include(CheckCXXCompilerFlag)
  # Clang
  if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
    add_flag_if_supported(-fcolor-diagnostics COMPILER_SUPPORTS_fdiagnostics_color)
  endif()

  # g++
  if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    add_flag_if_supported(-fdiagnostics-color=always COMPILER_SUPPORTS_fdiagnostics_color)

    # On some older gcc, it doesn't say that it's supported, but it works anyways
    if(NOT COMPILER_SUPPORTS_fdiagnostics_color)
      message(STATUS "Forcing -fdiagnostics-color=always")
      target_compile_options(project_options INTERFACE -fdiagnostics-color=always)
    endif()

  endif()
endif()

# Xcode/Ninja generators undefined MAKE
if(CMAKE_GENERATOR MATCHES "Make")
  set(MAKE "$(MAKE)")
else()
  set(MAKE make)
endif()
