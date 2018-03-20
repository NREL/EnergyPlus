#pragma once
#ifndef __VALIJSON_OPTIONAL_HPP
#define __VALIJSON_OPTIONAL_HPP

// This should be removed once C++17 is widely available
#if (defined(__cplusplus) && __cplusplus >= 201703L) || (defined(_HAS_CXX17) && _HAS_CXX17 == 1)
	#include <optional>
	namespace opt = std;
#else
	#include <compat/optional.hpp>
	namespace opt = std::experimental;
#endif

#endif
