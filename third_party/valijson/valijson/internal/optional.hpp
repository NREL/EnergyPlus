#pragma once
#ifndef __VALIJSON_OPTIONAL_HPP
#define __VALIJSON_OPTIONAL_HPP

// This should be removed once C++17 is widely available
#if __has_include(<optional>) && __cplusplus >= 201703
#  include <optional>
namespace opt = std;
#else
#  include <compat/optional.hpp>
namespace opt = std::experimental;
#endif

#endif
