#ifndef ObjexxFCL_Array6S_fwd_hh_INCLUDED
#define ObjexxFCL_Array6S_fwd_hh_INCLUDED

// Array6S Forward Declarations
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
template< typename > class Array6S;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array6S_bool = Array6S< bool >;
using Array6S_byte = Array6S< byte >;
using Array6S_sbyte = Array6S< sbyte >;
using Array6S_ubyte = Array6S< ubyte >;
using Array6S_short = Array6S< short int >;
using Array6S_int = Array6S< int >;
using Array6S_long = Array6S< long int >;
using Array6S_ushort = Array6S< unsigned short int >;
using Array6S_uint = Array6S< unsigned int >;
using Array6S_ulong = Array6S< unsigned long int >;
using Array6S_size = Array6S< std::size_t >;
using Array6S_int8 = Array6S< std::int8_t >;
using Array6S_int16 = Array6S< std::int16_t >;
using Array6S_int32 = Array6S< std::int32_t >;
using Array6S_int64 = Array6S< std::int64_t >;
using Array6S_uint8 = Array6S< std::uint8_t >;
using Array6S_uint16 = Array6S< std::uint16_t >;
using Array6S_uint32 = Array6S< std::uint32_t >;
using Array6S_uint64 = Array6S< std::uint64_t >;
using Array6S_float = Array6S< float >;
using Array6S_double = Array6S< double >;
using Array6S_longdouble = Array6S< long double >;
using Array6S_char = Array6S< char >;
using Array6S_uchar = Array6S< unsigned char >;
using Array6S_schar = Array6S< signed char >;
using Array6S_string = Array6S< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array6S_fwd_hh_INCLUDED
