#ifndef ObjexxFCL_Array_fwd_hh_INCLUDED
#define ObjexxFCL_Array_fwd_hh_INCLUDED

// Array Forward Declarations
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
template< typename > class Array;

// Types
typedef  Array< bool >                Array_bool;
typedef  Array< short int >           Array_short;
typedef  Array< int >                 Array_int;
typedef  Array< std::size_t >         Array_size;
typedef  Array< double >              Array_double;
typedef  Array< signed char >         Array_schar;

} // ObjexxFCL

#endif // ObjexxFCL_Array_fwd_hh_INCLUDED
