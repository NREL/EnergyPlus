#ifndef ObjexxFCL_Array4S_fwd_hh_INCLUDED
#define ObjexxFCL_Array4S_fwd_hh_INCLUDED

// Array4S Forward Declarations
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
template< typename > class Array4S;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array4S_bool = Array4S< bool >;
using Array4S_byte = Array4S< byte >;
using Array4S_sbyte = Array4S< sbyte >;
using Array4S_ubyte = Array4S< ubyte >;
using Array4S_short = Array4S< short int >;
using Array4S_int = Array4S< int >;
using Array4S_long = Array4S< long int >;
using Array4S_ushort = Array4S< unsigned short int >;
using Array4S_uint = Array4S< unsigned int >;
using Array4S_ulong = Array4S< unsigned long int >;
using Array4S_size = Array4S< std::size_t >;
using Array4S_int8 = Array4S< std::int8_t >;
using Array4S_int16 = Array4S< std::int16_t >;
using Array4S_int32 = Array4S< std::int32_t >;
using Array4S_int64 = Array4S< std::int64_t >;
using Array4S_uint8 = Array4S< std::uint8_t >;
using Array4S_uint16 = Array4S< std::uint16_t >;
using Array4S_uint32 = Array4S< std::uint32_t >;
using Array4S_uint64 = Array4S< std::uint64_t >;
using Array4S_float = Array4S< float >;
using Array4S_double = Array4S< double >;
using Array4S_longdouble = Array4S< long double >;
using Array4S_char = Array4S< char >;
using Array4S_uchar = Array4S< unsigned char >;
using Array4S_schar = Array4S< signed char >;
using Array4S_string = Array4S< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array4S_fwd_hh_INCLUDED
