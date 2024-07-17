add_library(kiva_common_interface INTERFACE)

  #================#
  # Compiler flags #
  #================#


cmake_policy(PUSH)
# This is needed to have <CXX_COMPILER_ID> differentiate between Clang and AppleClang
cmake_policy(SET CMP0025 NEW)

# AppleClang version numbering does not follow the upstream LLVM clang version.
# In order to properly silence the -Wenum-constexpr-conversion warning without adding another
# warning for "unknown warning option", we need to add this only for versions that support this flag.
set(clang_16_or_greater "$<AND:$<CXX_COMPILER_ID:Clang>,$<VERSION_GREATER_EQUAL:$<CXX_COMPILER_VERSION>,16>>")
set(apple_clang_15_or_greater "$<AND:$<CXX_COMPILER_ID:AppleClang>,$<VERSION_GREATER_EQUAL:$<CXX_COMPILER_VERSION>,15>>")
set(has_enum_constexpr_option "$<OR:${clang_16_or_greater},${apple_clang_15_or_greater}>")

target_compile_options(kiva_common_interface INTERFACE
$<$<CXX_COMPILER_ID:MSVC>:
  /W4 # Warning level (default is W3)
  $<$<BOOL:${KIVA_WERROR}>:
    /WX # Turn warnings into errors
  >
>
$<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:AppleClang>>:
  -Wall
  -Wextra
  -Wpedantic
  $<${has_enum_constexpr_option}:
    -Wno-enum-constexpr-conversion
  >
  $<$<BOOL:${KIVA_WERROR}>:
    -Werror # Turn warnings into errors
  >
>

)

cmake_policy(POP)
