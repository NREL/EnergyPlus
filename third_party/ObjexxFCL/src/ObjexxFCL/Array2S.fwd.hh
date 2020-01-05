#ifndef ObjexxFCL_Array2S_fwd_hh_INCLUDED
#define ObjexxFCL_Array2S_fwd_hh_INCLUDED

// Array2S Forward Declarations
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
template< typename > class Array2S;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array2S_bool = Array2S< bool >;
using Array2S_byte = Array2S< byte >;
using Array2S_sbyte = Array2S< sbyte >;
using Array2S_ubyte = Array2S< ubyte >;
using Array2S_short = Array2S< short int >;
using Array2S_int = Array2S< int >;
using Array2S_long = Array2S< long int >;
using Array2S_ushort = Array2S< unsigned short int >;
using Array2S_uint = Array2S< unsigned int >;
using Array2S_ulong = Array2S< unsigned long int >;
using Array2S_size = Array2S< std::size_t >;
using Array2S_int8 = Array2S< std::int8_t >;
using Array2S_int16 = Array2S< std::int16_t >;
using Array2S_int32 = Array2S< std::int32_t >;
using Array2S_int64 = Array2S< std::int64_t >;
using Array2S_uint8 = Array2S< std::uint8_t >;
using Array2S_uint16 = Array2S< std::uint16_t >;
using Array2S_uint32 = Array2S< std::uint32_t >;
using Array2S_uint64 = Array2S< std::uint64_t >;
using Array2S_float = Array2S< float >;
using Array2S_double = Array2S< double >;
using Array2S_longdouble = Array2S< long double >;
using Array2S_char = Array2S< char >;
using Array2S_uchar = Array2S< unsigned char >;
using Array2S_schar = Array2S< signed char >;
using Array2S_string = Array2S< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array2S_fwd_hh_INCLUDED
