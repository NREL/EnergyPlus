#ifndef ObjexxFCL_Array1S_fwd_hh_INCLUDED
#define ObjexxFCL_Array1S_fwd_hh_INCLUDED

// Array1S Forward Declarations
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
template< typename > class Array1S;

// Types
typedef  Array1S< bool >                Array1S_bool;
typedef  Array1S< int >                 Array1S_int;
typedef  Array1S< std::string >         Array1S_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array1S_fwd_hh_INCLUDED
