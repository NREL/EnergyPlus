#ifndef ObjexxFCL_Vector2_fwd_hh_INCLUDED
#define ObjexxFCL_Vector2_fwd_hh_INCLUDED

// Vector2 Forward Declarations
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

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
typedef  byte  sbyte;
typedef  Vector2< bool >                Vector2_bool;
typedef  Vector2< byte >                Vector2_byte;
typedef  Vector2< sbyte >               Vector2_sbyte;
typedef  Vector2< ubyte >               Vector2_ubyte;
typedef  Vector2< short int >           Vector2_short;
typedef  Vector2< int >                 Vector2_int;
typedef  Vector2< long int >            Vector2_long;
typedef  Vector2< unsigned short int >  Vector2_ushort;
typedef  Vector2< unsigned int >        Vector2_uint;
typedef  Vector2< unsigned long int >   Vector2_ulong;
typedef  Vector2< std::size_t >         Vector2_size;
typedef  Vector2< std::int8_t >         Vector2_int8;
typedef  Vector2< std::int16_t >        Vector2_int16;
typedef  Vector2< std::int32_t >        Vector2_int32;
typedef  Vector2< std::int64_t >        Vector2_int64;
typedef  Vector2< std::uint8_t >        Vector2_uint8;
typedef  Vector2< std::uint16_t >       Vector2_uint16;
typedef  Vector2< std::uint32_t >       Vector2_uint32;
typedef  Vector2< std::uint64_t >       Vector2_uint64;
typedef  Vector2< float >               Vector2_float;
typedef  Vector2< double >              Vector2_double;
typedef  Vector2< long double >         Vector2_longdouble;
typedef  Vector2< char >                Vector2_char;
typedef  Vector2< unsigned char >       Vector2_uchar;
typedef  Vector2< signed char >         Vector2_schar;
typedef  Vector2< std::string >         Vector2_string;

} // ObjexxFCL

#endif // ObjexxFCL_Vector2_fwd_hh_INCLUDED
