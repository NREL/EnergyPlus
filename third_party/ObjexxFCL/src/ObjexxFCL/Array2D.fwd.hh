#ifndef ObjexxFCL_Array2D_fwd_hh_INCLUDED
#define ObjexxFCL_Array2D_fwd_hh_INCLUDED

// Array2D Forward Declarations
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
template< typename > class Array2D;

// Types
typedef  Array2D< bool >                Array2D_bool;
typedef  Array2D< short int >           Array2D_short;
typedef  Array2D< int >                 Array2D_int;
typedef  Array2D< unsigned short int >  Array2D_ushort;
typedef  Array2D< double >              Array2D_double;
typedef  Array2D< std::string >         Array2D_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array2D_fwd_hh_INCLUDED
