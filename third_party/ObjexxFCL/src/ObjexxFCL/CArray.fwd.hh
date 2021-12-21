#ifndef ObjexxFCL_CArray_fwd_hh_INCLUDED
#define ObjexxFCL_CArray_fwd_hh_INCLUDED

// CArray Forward Declarations
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
template< typename > class CArray;

// Types
typedef  CArray< bool >                CArray_bool;
typedef  CArray< short int >           CArray_short;
typedef  CArray< int >                 CArray_int;
typedef  CArray< std::size_t >         CArray_size;
typedef  CArray< double >              CArray_double;
typedef  CArray< signed char >         CArray_schar;
typedef  CArray< std::string >         CArray_string;

} // ObjexxFCL

#endif // ObjexxFCL_CArray_fwd_hh_INCLUDED
