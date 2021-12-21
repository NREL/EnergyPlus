#ifndef ObjexxFCL_Array2S_fwd_hh_INCLUDED
#define ObjexxFCL_Array2S_fwd_hh_INCLUDED

// Array2S Forward Declarations
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
template< typename > class Array2S;

// Types
typedef  Array2S< bool >                Array2S_bool;
typedef  Array2S< int >                 Array2S_int;
typedef  Array2S< double >              Array2S_double;
typedef  Array2S< std::string >         Array2S_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array2S_fwd_hh_INCLUDED
