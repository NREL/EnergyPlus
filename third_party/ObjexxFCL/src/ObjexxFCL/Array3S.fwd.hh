#ifndef ObjexxFCL_Array3S_fwd_hh_INCLUDED
#define ObjexxFCL_Array3S_fwd_hh_INCLUDED

// Array3S Forward Declarations
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
template< typename > class Array3S;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array3S_bool = Array3S< bool >;
using Array3S_byte = Array3S< byte >;
using Array3S_sbyte = Array3S< sbyte >;
using Array3S_ubyte = Array3S< ubyte >;
using Array3S_short = Array3S< short int >;
using Array3S_int = Array3S< int >;
using Array3S_long = Array3S< long int >;
using Array3S_ushort = Array3S< unsigned short int >;
using Array3S_uint = Array3S< unsigned int >;
using Array3S_ulong = Array3S< unsigned long int >;
using Array3S_size = Array3S< std::size_t >;
using Array3S_int8 = Array3S< std::int8_t >;
using Array3S_int16 = Array3S< std::int16_t >;
using Array3S_int32 = Array3S< std::int32_t >;
using Array3S_int64 = Array3S< std::int64_t >;
using Array3S_uint8 = Array3S< std::uint8_t >;
using Array3S_uint16 = Array3S< std::uint16_t >;
using Array3S_uint32 = Array3S< std::uint32_t >;
using Array3S_uint64 = Array3S< std::uint64_t >;
using Array3S_float = Array3S< float >;
using Array3S_double = Array3S< double >;
using Array3S_longdouble = Array3S< long double >;
using Array3S_char = Array3S< char >;
using Array3S_uchar = Array3S< unsigned char >;
using Array3S_schar = Array3S< signed char >;
using Array3S_string = Array3S< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array3S_fwd_hh_INCLUDED
