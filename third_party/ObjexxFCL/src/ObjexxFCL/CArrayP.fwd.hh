#ifndef ObjexxFCL_CArrayP_fwd_hh_INCLUDED
#define ObjexxFCL_CArrayP_fwd_hh_INCLUDED

// CArrayP Forward Declarations
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
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
class ubyte;

// Types
typedef  byte  sbyte;
typedef  CArrayP< bool >                CArrayP_bool;
typedef  CArrayP< byte >                CArrayP_byte;
typedef  CArrayP< sbyte >               CArrayP_sbyte;
typedef  CArrayP< ubyte >               CArrayP_ubyte;
typedef  CArrayP< short int >           CArrayP_short;
typedef  CArrayP< int >                 CArrayP_int;
typedef  CArrayP< long int >            CArrayP_long;
typedef  CArrayP< unsigned short int >  CArrayP_ushort;
typedef  CArrayP< unsigned int >        CArrayP_uint;
typedef  CArrayP< unsigned long int >   CArrayP_ulong;
typedef  CArrayP< std::size_t >         CArrayP_size;
typedef  CArrayP< std::int8_t >         CArrayP_int8;
typedef  CArrayP< std::int16_t >        CArrayP_int16;
typedef  CArrayP< std::int32_t >        CArrayP_int32;
typedef  CArrayP< std::int64_t >        CArrayP_int64;
typedef  CArrayP< std::uint8_t >        CArrayP_uint8;
typedef  CArrayP< std::uint16_t >       CArrayP_uint16;
typedef  CArrayP< std::uint32_t >       CArrayP_uint32;
typedef  CArrayP< std::uint64_t >       CArrayP_uint64;
typedef  CArrayP< float >               CArrayP_float;
typedef  CArrayP< double >              CArrayP_double;
typedef  CArrayP< long double >         CArrayP_longdouble;
typedef  CArrayP< char >                CArrayP_char;
typedef  CArrayP< unsigned char >       CArrayP_uchar;
typedef  CArrayP< signed char >         CArrayP_schar;
typedef  CArrayP< std::string >         CArrayP_string;

} // ObjexxFCL

#endif // ObjexxFCL_CArrayP_fwd_hh_INCLUDED
