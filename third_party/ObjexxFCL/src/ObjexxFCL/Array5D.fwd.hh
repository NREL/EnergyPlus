#ifndef ObjexxFCL_Array5D_fwd_hh_INCLUDED
#define ObjexxFCL_Array5D_fwd_hh_INCLUDED

// Array5D Forward Declarations
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
template< typename > class Array5D;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array5D_bool = Array5D< bool >;
using Array5D_byte = Array5D< byte >;
using Array5D_sbyte = Array5D< sbyte >;
using Array5D_ubyte = Array5D< ubyte >;
using Array5D_short = Array5D< short int >;
using Array5D_int = Array5D< int >;
using Array5D_long = Array5D< long int >;
using Array5D_ushort = Array5D< unsigned short int >;
using Array5D_uint = Array5D< unsigned int >;
using Array5D_ulong = Array5D< unsigned long int >;
using Array5D_size = Array5D< std::size_t >;
using Array5D_int8 = Array5D< std::int8_t >;
using Array5D_int16 = Array5D< std::int16_t >;
using Array5D_int32 = Array5D< std::int32_t >;
using Array5D_int64 = Array5D< std::int64_t >;
using Array5D_uint8 = Array5D< std::uint8_t >;
using Array5D_uint16 = Array5D< std::uint16_t >;
using Array5D_uint32 = Array5D< std::uint32_t >;
using Array5D_uint64 = Array5D< std::uint64_t >;
using Array5D_float = Array5D< float >;
using Array5D_double = Array5D< double >;
using Array5D_longdouble = Array5D< long double >;
using Array5D_char = Array5D< char >;
using Array5D_uchar = Array5D< unsigned char >;
using Array5D_schar = Array5D< signed char >;
using Array5D_string = Array5D< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array5D_fwd_hh_INCLUDED
