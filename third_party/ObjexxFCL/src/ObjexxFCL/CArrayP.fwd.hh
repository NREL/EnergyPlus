#ifndef ObjexxFCL_CArrayP_fwd_hh_INCLUDED
#define ObjexxFCL_CArrayP_fwd_hh_INCLUDED

// CArrayP Forward Declarations
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
template< typename > class CArrayP;
class byte;

// Types
typedef  byte  sbyte;
typedef  CArrayP< short int >           CArrayP_short;
typedef  CArrayP< std::size_t >         CArrayP_size;
typedef  CArrayP< std::string >         CArrayP_string;

} // ObjexxFCL

#endif // ObjexxFCL_CArrayP_fwd_hh_INCLUDED
