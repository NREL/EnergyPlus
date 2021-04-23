#ifndef ObjexxFCL_ArrayInitializer_fwd_hh_INCLUDED
#define ObjexxFCL_ArrayInitializer_fwd_hh_INCLUDED

// ArrayInitializer Forward Declarations
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// C++ Headers
#include <cstddef>
#include <cstdint>
#include <string>

namespace ObjexxFCL {

// Forward
template< typename > class ArrayInitializer;
class byte;

// Types
typedef  ArrayInitializer< short int >           ArrayInitializer_short;
typedef  ArrayInitializer< int >                 ArrayInitializer_int;
typedef  ArrayInitializer< signed char >         ArrayInitializer_schar;
typedef  ArrayInitializer< std::string >         ArrayInitializer_string;

} // ObjexxFCL

#endif // ObjexxFCL_ArrayInitializer_fwd_hh_INCLUDED
