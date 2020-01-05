#ifndef ObjexxFCL_Array6_fwd_hh_INCLUDED
#define ObjexxFCL_Array6_fwd_hh_INCLUDED

// Array6 Forward Declarations
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2020 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// C++ Headers
#include <cstddef>
#include <cstdint>
#include <string>

namespace ObjexxFCL {

// Forward
template< typename > class Array6;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array6_bool = Array6< bool >;
using Array6_byte = Array6< byte >;
using Array6_sbyte = Array6< sbyte >;
using Array6_ubyte = Array6< ubyte >;
using Array6_short = Array6< short int >;
using Array6_int = Array6< int >;
using Array6_long = Array6< long int >;
using Array6_ushort = Array6< unsigned short int >;
using Array6_uint = Array6< unsigned int >;
using Array6_ulong = Array6< unsigned long int >;
using Array6_size = Array6< std::size_t >;
using Array6_int8 = Array6< std::int8_t >;
using Array6_int16 = Array6< std::int16_t >;
using Array6_int32 = Array6< std::int32_t >;
using Array6_int64 = Array6< std::int64_t >;
using Array6_uint8 = Array6< std::uint8_t >;
using Array6_uint16 = Array6< std::uint16_t >;
using Array6_uint32 = Array6< std::uint32_t >;
using Array6_uint64 = Array6< std::uint64_t >;
using Array6_float = Array6< float >;
using Array6_double = Array6< double >;
using Array6_longdouble = Array6< long double >;
using Array6_char = Array6< char >;
using Array6_uchar = Array6< unsigned char >;
using Array6_schar = Array6< signed char >;
using Array6_string = Array6< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array6_fwd_hh_INCLUDED
