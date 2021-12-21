#ifndef ObjexxFCL_Array1_fwd_hh_INCLUDED
#define ObjexxFCL_Array1_fwd_hh_INCLUDED

// Array1 Forward Declarations
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
template< typename > class Array1;

// Types
typedef  Array1< int >                 Array1_int;
typedef  Array1< std::string >         Array1_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array1_fwd_hh_INCLUDED
