#ifndef ObjexxFCL_Array5S_fwd_hh_INCLUDED
#define ObjexxFCL_Array5S_fwd_hh_INCLUDED

// Array5S Forward Declarations
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
template< typename > class Array5S;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array5S_bool = Array5S< bool >;
using Array5S_byte = Array5S< byte >;
using Array5S_sbyte = Array5S< sbyte >;
using Array5S_ubyte = Array5S< ubyte >;
using Array5S_short = Array5S< short int >;
using Array5S_int = Array5S< int >;
using Array5S_long = Array5S< long int >;
using Array5S_ushort = Array5S< unsigned short int >;
using Array5S_uint = Array5S< unsigned int >;
using Array5S_ulong = Array5S< unsigned long int >;
using Array5S_size = Array5S< std::size_t >;
using Array5S_int8 = Array5S< std::int8_t >;
using Array5S_int16 = Array5S< std::int16_t >;
using Array5S_int32 = Array5S< std::int32_t >;
using Array5S_int64 = Array5S< std::int64_t >;
using Array5S_uint8 = Array5S< std::uint8_t >;
using Array5S_uint16 = Array5S< std::uint16_t >;
using Array5S_uint32 = Array5S< std::uint32_t >;
using Array5S_uint64 = Array5S< std::uint64_t >;
using Array5S_float = Array5S< float >;
using Array5S_double = Array5S< double >;
using Array5S_longdouble = Array5S< long double >;
using Array5S_char = Array5S< char >;
using Array5S_uchar = Array5S< unsigned char >;
using Array5S_schar = Array5S< signed char >;
using Array5S_string = Array5S< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array5S_fwd_hh_INCLUDED
