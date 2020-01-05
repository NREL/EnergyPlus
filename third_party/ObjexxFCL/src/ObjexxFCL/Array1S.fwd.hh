#ifndef ObjexxFCL_Array1S_fwd_hh_INCLUDED
#define ObjexxFCL_Array1S_fwd_hh_INCLUDED

// Array1S Forward Declarations
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
template< typename > class Array1S;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array1S_bool = Array1S< bool >;
using Array1S_byte = Array1S< byte >;
using Array1S_sbyte = Array1S< sbyte >;
using Array1S_ubyte = Array1S< ubyte >;
using Array1S_short = Array1S< short int >;
using Array1S_int = Array1S< int >;
using Array1S_long = Array1S< long int >;
using Array1S_ushort = Array1S< unsigned short int >;
using Array1S_uint = Array1S< unsigned int >;
using Array1S_ulong = Array1S< unsigned long int >;
using Array1S_size = Array1S< std::size_t >;
using Array1S_int8 = Array1S< std::int8_t >;
using Array1S_int16 = Array1S< std::int16_t >;
using Array1S_int32 = Array1S< std::int32_t >;
using Array1S_int64 = Array1S< std::int64_t >;
using Array1S_uint8 = Array1S< std::uint8_t >;
using Array1S_uint16 = Array1S< std::uint16_t >;
using Array1S_uint32 = Array1S< std::uint32_t >;
using Array1S_uint64 = Array1S< std::uint64_t >;
using Array1S_float = Array1S< float >;
using Array1S_double = Array1S< double >;
using Array1S_longdouble = Array1S< long double >;
using Array1S_char = Array1S< char >;
using Array1S_uchar = Array1S< unsigned char >;
using Array1S_schar = Array1S< signed char >;
using Array1S_string = Array1S< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array1S_fwd_hh_INCLUDED
