#ifndef ObjexxFCL_Array1_fwd_hh_INCLUDED
#define ObjexxFCL_Array1_fwd_hh_INCLUDED

// Array1 Forward Declarations
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
template< typename > class Array1;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array1_bool = Array1< bool >;
using Array1_byte = Array1< byte >;
using Array1_sbyte = Array1< sbyte >;
using Array1_ubyte = Array1< ubyte >;
using Array1_short = Array1< short int >;
using Array1_int = Array1< int >;
using Array1_long = Array1< long int >;
using Array1_ushort = Array1< unsigned short int >;
using Array1_uint = Array1< unsigned int >;
using Array1_ulong = Array1< unsigned long int >;
using Array1_size = Array1< std::size_t >;
using Array1_int8 = Array1< std::int8_t >;
using Array1_int16 = Array1< std::int16_t >;
using Array1_int32 = Array1< std::int32_t >;
using Array1_int64 = Array1< std::int64_t >;
using Array1_uint8 = Array1< std::uint8_t >;
using Array1_uint16 = Array1< std::uint16_t >;
using Array1_uint32 = Array1< std::uint32_t >;
using Array1_uint64 = Array1< std::uint64_t >;
using Array1_float = Array1< float >;
using Array1_double = Array1< double >;
using Array1_longdouble = Array1< long double >;
using Array1_char = Array1< char >;
using Array1_uchar = Array1< unsigned char >;
using Array1_schar = Array1< signed char >;
using Array1_string = Array1< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array1_fwd_hh_INCLUDED
