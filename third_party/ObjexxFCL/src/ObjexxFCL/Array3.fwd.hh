#ifndef ObjexxFCL_Array3_fwd_hh_INCLUDED
#define ObjexxFCL_Array3_fwd_hh_INCLUDED

// Array3 Forward Declarations
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
template< typename > class Array3;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array3_bool = Array3< bool >;
using Array3_byte = Array3< byte >;
using Array3_sbyte = Array3< sbyte >;
using Array3_ubyte = Array3< ubyte >;
using Array3_short = Array3< short int >;
using Array3_int = Array3< int >;
using Array3_long = Array3< long int >;
using Array3_ushort = Array3< unsigned short int >;
using Array3_uint = Array3< unsigned int >;
using Array3_ulong = Array3< unsigned long int >;
using Array3_size = Array3< std::size_t >;
using Array3_int8 = Array3< std::int8_t >;
using Array3_int16 = Array3< std::int16_t >;
using Array3_int32 = Array3< std::int32_t >;
using Array3_int64 = Array3< std::int64_t >;
using Array3_uint8 = Array3< std::uint8_t >;
using Array3_uint16 = Array3< std::uint16_t >;
using Array3_uint32 = Array3< std::uint32_t >;
using Array3_uint64 = Array3< std::uint64_t >;
using Array3_float = Array3< float >;
using Array3_double = Array3< double >;
using Array3_longdouble = Array3< long double >;
using Array3_char = Array3< char >;
using Array3_uchar = Array3< unsigned char >;
using Array3_schar = Array3< signed char >;
using Array3_string = Array3< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array3_fwd_hh_INCLUDED
