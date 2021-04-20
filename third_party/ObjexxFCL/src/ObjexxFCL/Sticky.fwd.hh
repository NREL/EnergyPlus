#ifndef ObjexxFCL_Sticky_fwd_hh_INCLUDED
#define ObjexxFCL_Sticky_fwd_hh_INCLUDED

// Sticky Forward Declarations
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
template< typename > class Sticky;
class byte;

// Types
typedef  byte  sbyte;
typedef  Sticky< int >                 Sticky_int;
typedef  Sticky< signed char >         Sticky_schar;

} // ObjexxFCL

#endif // ObjexxFCL_Sticky_fwd_hh_INCLUDED
