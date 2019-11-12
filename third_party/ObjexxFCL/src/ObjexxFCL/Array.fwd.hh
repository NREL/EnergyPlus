#ifndef ObjexxFCL_Array_fwd_hh_INCLUDED
#define ObjexxFCL_Array_fwd_hh_INCLUDED

// Array Forward Declarations
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
template< typename > class Array;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array_bool = Array< bool >;
using Array_byte = Array< byte >;
using Array_sbyte = Array< sbyte >;
using Array_ubyte = Array< ubyte >;
using Array_short = Array< short int >;
using Array_int = Array< int >;
using Array_long = Array< long int >;
using Array_ushort = Array< unsigned short int >;
using Array_uint = Array< unsigned int >;
using Array_ulong = Array< unsigned long int >;
using Array_size = Array< std::size_t >;
using Array_int8 = Array< std::int8_t >;
using Array_int16 = Array< std::int16_t >;
using Array_int32 = Array< std::int32_t >;
using Array_int64 = Array< std::int64_t >;
using Array_uint8 = Array< std::uint8_t >;
using Array_uint16 = Array< std::uint16_t >;
using Array_uint32 = Array< std::uint32_t >;
using Array_uint64 = Array< std::uint64_t >;
using Array_float = Array< float >;
using Array_double = Array< double >;
using Array_longdouble = Array< long double >;
using Array_char = Array< char >;
using Array_uchar = Array< unsigned char >;
using Array_schar = Array< signed char >;
using Array_string = Array< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array_fwd_hh_INCLUDED
