#ifndef ObjexxFCL_Array1D_fwd_hh_INCLUDED
#define ObjexxFCL_Array1D_fwd_hh_INCLUDED

// Array1D Forward Declarations
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
template< typename > class Array1D;

// Types
typedef  Array1D< bool >                Array1D_bool;
typedef  Array1D< short int >           Array1D_short;
typedef  Array1D< int >                 Array1D_int;
typedef  Array1D< unsigned short int >  Array1D_ushort;
typedef  Array1D< std::size_t >         Array1D_size;
typedef  Array1D< std::int32_t >        Array1D_int32;
typedef  Array1D< double >              Array1D_double;
typedef  Array1D< signed char >         Array1D_schar;
typedef  Array1D< std::string >         Array1D_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array1D_fwd_hh_INCLUDED
