#ifndef ObjexxFCL_Vector2_fwd_hh_INCLUDED
#define ObjexxFCL_Vector2_fwd_hh_INCLUDED

// Vector2 Forward Declarations
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
template< typename > class Vector2;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Vector2_bool = Vector2< bool >;
using Vector2_byte = Vector2< byte >;
using Vector2_sbyte = Vector2< sbyte >;
using Vector2_ubyte = Vector2< ubyte >;
using Vector2_short = Vector2< short int >;
using Vector2_int = Vector2< int >;
using Vector2_long = Vector2< long int >;
using Vector2_ushort = Vector2< unsigned short int >;
using Vector2_uint = Vector2< unsigned int >;
using Vector2_ulong = Vector2< unsigned long int >;
using Vector2_size = Vector2< std::size_t >;
using Vector2_int8 = Vector2< std::int8_t >;
using Vector2_int16 = Vector2< std::int16_t >;
using Vector2_int32 = Vector2< std::int32_t >;
using Vector2_int64 = Vector2< std::int64_t >;
using Vector2_uint8 = Vector2< std::uint8_t >;
using Vector2_uint16 = Vector2< std::uint16_t >;
using Vector2_uint32 = Vector2< std::uint32_t >;
using Vector2_uint64 = Vector2< std::uint64_t >;
using Vector2_float = Vector2< float >;
using Vector2_double = Vector2< double >;
using Vector2_longdouble = Vector2< long double >;
using Vector2_char = Vector2< char >;
using Vector2_uchar = Vector2< unsigned char >;
using Vector2_schar = Vector2< signed char >;
using Vector2_string = Vector2< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Vector2_fwd_hh_INCLUDED
