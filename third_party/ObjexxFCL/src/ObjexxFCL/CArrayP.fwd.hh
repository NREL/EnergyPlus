#ifndef ObjexxFCL_CArrayP_fwd_hh_INCLUDED
#define ObjexxFCL_CArrayP_fwd_hh_INCLUDED

// CArrayP Forward Declarations
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

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
using sbyte = byte;
using CArrayP_bool = CArrayP< bool >;
using CArrayP_byte = CArrayP< byte >;
using CArrayP_sbyte = CArrayP< sbyte >;
using CArrayP_ubyte = CArrayP< ubyte >;
using CArrayP_short = CArrayP< short int >;
using CArrayP_int = CArrayP< int >;
using CArrayP_long = CArrayP< long int >;
using CArrayP_ushort = CArrayP< unsigned short int >;
using CArrayP_uint = CArrayP< unsigned int >;
using CArrayP_ulong = CArrayP< unsigned long int >;
using CArrayP_size = CArrayP< std::size_t >;
using CArrayP_int8 = CArrayP< std::int8_t >;
using CArrayP_int16 = CArrayP< std::int16_t >;
using CArrayP_int32 = CArrayP< std::int32_t >;
using CArrayP_int64 = CArrayP< std::int64_t >;
using CArrayP_uint8 = CArrayP< std::uint8_t >;
using CArrayP_uint16 = CArrayP< std::uint16_t >;
using CArrayP_uint32 = CArrayP< std::uint32_t >;
using CArrayP_uint64 = CArrayP< std::uint64_t >;
using CArrayP_float = CArrayP< float >;
using CArrayP_double = CArrayP< double >;
using CArrayP_longdouble = CArrayP< long double >;
using CArrayP_char = CArrayP< char >;
using CArrayP_uchar = CArrayP< unsigned char >;
using CArrayP_schar = CArrayP< signed char >;
using CArrayP_string = CArrayP< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_CArrayP_fwd_hh_INCLUDED
