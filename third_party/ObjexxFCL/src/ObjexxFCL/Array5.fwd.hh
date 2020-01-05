#ifndef ObjexxFCL_Array5_fwd_hh_INCLUDED
#define ObjexxFCL_Array5_fwd_hh_INCLUDED

// Array5 Forward Declarations
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
template< typename > class Array5;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array5_bool = Array5< bool >;
using Array5_byte = Array5< byte >;
using Array5_sbyte = Array5< sbyte >;
using Array5_ubyte = Array5< ubyte >;
using Array5_short = Array5< short int >;
using Array5_int = Array5< int >;
using Array5_long = Array5< long int >;
using Array5_ushort = Array5< unsigned short int >;
using Array5_uint = Array5< unsigned int >;
using Array5_ulong = Array5< unsigned long int >;
using Array5_size = Array5< std::size_t >;
using Array5_int8 = Array5< std::int8_t >;
using Array5_int16 = Array5< std::int16_t >;
using Array5_int32 = Array5< std::int32_t >;
using Array5_int64 = Array5< std::int64_t >;
using Array5_uint8 = Array5< std::uint8_t >;
using Array5_uint16 = Array5< std::uint16_t >;
using Array5_uint32 = Array5< std::uint32_t >;
using Array5_uint64 = Array5< std::uint64_t >;
using Array5_float = Array5< float >;
using Array5_double = Array5< double >;
using Array5_longdouble = Array5< long double >;
using Array5_char = Array5< char >;
using Array5_uchar = Array5< unsigned char >;
using Array5_schar = Array5< signed char >;
using Array5_string = Array5< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array5_fwd_hh_INCLUDED
