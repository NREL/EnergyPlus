#ifndef ObjexxFCL_Array4_fwd_hh_INCLUDED
#define ObjexxFCL_Array4_fwd_hh_INCLUDED

// Array4 Forward Declarations
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
template< typename > class Array4;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array4_bool = Array4< bool >;
using Array4_byte = Array4< byte >;
using Array4_sbyte = Array4< sbyte >;
using Array4_ubyte = Array4< ubyte >;
using Array4_short = Array4< short int >;
using Array4_int = Array4< int >;
using Array4_long = Array4< long int >;
using Array4_ushort = Array4< unsigned short int >;
using Array4_uint = Array4< unsigned int >;
using Array4_ulong = Array4< unsigned long int >;
using Array4_size = Array4< std::size_t >;
using Array4_int8 = Array4< std::int8_t >;
using Array4_int16 = Array4< std::int16_t >;
using Array4_int32 = Array4< std::int32_t >;
using Array4_int64 = Array4< std::int64_t >;
using Array4_uint8 = Array4< std::uint8_t >;
using Array4_uint16 = Array4< std::uint16_t >;
using Array4_uint32 = Array4< std::uint32_t >;
using Array4_uint64 = Array4< std::uint64_t >;
using Array4_float = Array4< float >;
using Array4_double = Array4< double >;
using Array4_longdouble = Array4< long double >;
using Array4_char = Array4< char >;
using Array4_uchar = Array4< unsigned char >;
using Array4_schar = Array4< signed char >;
using Array4_string = Array4< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array4_fwd_hh_INCLUDED
