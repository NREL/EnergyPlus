#ifndef EnergyPlus_hh_INCLUDED
#define EnergyPlus_hh_INCLUDED

// EnergyPlus Project-Wide Header File
//
// Language: C++

// C++ Headers
#include <cstdint> // C++11

// macro to supress unused parameter
// UNUSED( foo );
#define EP_UNUSED( expr )

// ObjexxFCL
namespace ObjexxFCL {
namespace fmt {
} // fmt
} // ObjexxFCL
using namespace ObjexxFCL;
using namespace ObjexxFCL::fmt;

// Types
typedef  std::int32_t  Int32;
typedef  std::int64_t  Int64;
typedef  float         Real32; // Platform-specific: C++ has no defined precision floating point types
typedef  double        Real64; // Platform-specific: C++ has no defined precision floating point types

#endif
