#ifndef ObjexxFCL_CArrayA_fwd_hh_INCLUDED
#define ObjexxFCL_CArrayA_fwd_hh_INCLUDED

// CArrayA Forward Declarations
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
template< typename > class CArrayA;
class byte;
class ubyte;

// Types
using sbyte = byte;
using CArrayA_bool = CArrayA< bool >;
using CArrayA_byte = CArrayA< byte >;
using CArrayA_sbyte = CArrayA< sbyte >;
using CArrayA_ubyte = CArrayA< ubyte >;
using CArrayA_short = CArrayA< short int >;
using CArrayA_int = CArrayA< int >;
using CArrayA_long = CArrayA< long int >;
using CArrayA_ushort = CArrayA< unsigned short int >;
using CArrayA_uint = CArrayA< unsigned int >;
using CArrayA_ulong = CArrayA< unsigned long int >;
using CArrayA_size = CArrayA< std::size_t >;
using CArrayA_int8 = CArrayA< std::int8_t >;
using CArrayA_int16 = CArrayA< std::int16_t >;
using CArrayA_int32 = CArrayA< std::int32_t >;
using CArrayA_int64 = CArrayA< std::int64_t >;
using CArrayA_uint8 = CArrayA< std::uint8_t >;
using CArrayA_uint16 = CArrayA< std::uint16_t >;
using CArrayA_uint32 = CArrayA< std::uint32_t >;
using CArrayA_uint64 = CArrayA< std::uint64_t >;
using CArrayA_float = CArrayA< float >;
using CArrayA_double = CArrayA< double >;
using CArrayA_longdouble = CArrayA< long double >;
using CArrayA_char = CArrayA< char >;
using CArrayA_uchar = CArrayA< unsigned char >;
using CArrayA_schar = CArrayA< signed char >;
using CArrayA_string = CArrayA< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_CArrayA_fwd_hh_INCLUDED
