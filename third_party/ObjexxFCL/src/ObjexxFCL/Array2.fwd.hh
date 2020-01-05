#ifndef ObjexxFCL_Array2_fwd_hh_INCLUDED
#define ObjexxFCL_Array2_fwd_hh_INCLUDED

// Array2 Forward Declarations
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
template< typename > class Array2;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array2_bool = Array2< bool >;
using Array2_byte = Array2< byte >;
using Array2_sbyte = Array2< sbyte >;
using Array2_ubyte = Array2< ubyte >;
using Array2_short = Array2< short int >;
using Array2_int = Array2< int >;
using Array2_long = Array2< long int >;
using Array2_ushort = Array2< unsigned short int >;
using Array2_uint = Array2< unsigned int >;
using Array2_ulong = Array2< unsigned long int >;
using Array2_size = Array2< std::size_t >;
using Array2_int8 = Array2< std::int8_t >;
using Array2_int16 = Array2< std::int16_t >;
using Array2_int32 = Array2< std::int32_t >;
using Array2_int64 = Array2< std::int64_t >;
using Array2_uint8 = Array2< std::uint8_t >;
using Array2_uint16 = Array2< std::uint16_t >;
using Array2_uint32 = Array2< std::uint32_t >;
using Array2_uint64 = Array2< std::uint64_t >;
using Array2_float = Array2< float >;
using Array2_double = Array2< double >;
using Array2_longdouble = Array2< long double >;
using Array2_char = Array2< char >;
using Array2_uchar = Array2< unsigned char >;
using Array2_schar = Array2< signed char >;
using Array2_string = Array2< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array2_fwd_hh_INCLUDED
