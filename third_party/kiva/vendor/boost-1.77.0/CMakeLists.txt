project(BOOST)

if (KIVA_EXE_BUILD)
  add_subdirectory(libs/program_options)
  target_compile_options(boost_program_options PRIVATE
    $<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:AppleClang>>:
     -Wno-deprecated-declarations #  sprintf in lexical_cast
    >
  )
endif()