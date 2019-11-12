#ifndef ObjexxFCL_Array4D_fwd_hh_INCLUDED
#define ObjexxFCL_Array4D_fwd_hh_INCLUDED

// Array4D Forward Declarations
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
template< typename > class Array4D;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array4D_bool = Array4D< bool >;
using Array4D_byte = Array4D< byte >;
using Array4D_sbyte = Array4D< sbyte >;
using Array4D_ubyte = Array4D< ubyte >;
using Array4D_short = Array4D< short int >;
using Array4D_int = Array4D< int >;
using Array4D_long = Array4D< long int >;
using Array4D_ushort = Array4D< unsigned short int >;
using Array4D_uint = Array4D< unsigned int >;
using Array4D_ulong = Array4D< unsigned long int >;
using Array4D_size = Array4D< std::size_t >;
using Array4D_int8 = Array4D< std::int8_t >;
using Array4D_int16 = Array4D< std::int16_t >;
using Array4D_int32 = Array4D< std::int32_t >;
using Array4D_int64 = Array4D< std::int64_t >;
using Array4D_uint8 = Array4D< std::uint8_t >;
using Array4D_uint16 = Array4D< std::uint16_t >;
using Array4D_uint32 = Array4D< std::uint32_t >;
using Array4D_uint64 = Array4D< std::uint64_t >;
using Array4D_float = Array4D< float >;
using Array4D_double = Array4D< double >;
using Array4D_longdouble = Array4D< long double >;
using Array4D_char = Array4D< char >;
using Array4D_uchar = Array4D< unsigned char >;
using Array4D_schar = Array4D< signed char >;
using Array4D_string = Array4D< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array4D_fwd_hh_INCLUDED
