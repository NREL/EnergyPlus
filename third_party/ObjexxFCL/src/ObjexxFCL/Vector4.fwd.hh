#ifndef ObjexxFCL_Vector4_fwd_hh_INCLUDED
#define ObjexxFCL_Vector4_fwd_hh_INCLUDED

// Vector3 Forward Declarations
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
template< typename > class Vector4;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Vector4< bool >                Vector4_bool;
typedef  Vector4< byte >                Vector4_byte;
typedef  Vector4< sbyte >               Vector4_sbyte;
typedef  Vector4< ubyte >               Vector4_ubyte;
typedef  Vector4< short int >           Vector4_short;
typedef  Vector4< int >                 Vector4_int;
typedef  Vector4< long int >            Vector4_long;
typedef  Vector4< unsigned short int >  Vector4_ushort;
typedef  Vector4< unsigned int >        Vector4_uint;
typedef  Vector4< unsigned long int >   Vector4_ulong;
typedef  Vector4< std::size_t >         Vector4_size;
typedef  Vector4< std::int8_t >         Vector4_int8;
typedef  Vector4< std::int16_t >        Vector4_int16;
typedef  Vector4< std::int32_t >        Vector4_int32;
typedef  Vector4< std::int64_t >        Vector4_int64;
typedef  Vector4< std::uint8_t >        Vector4_uint8;
typedef  Vector4< std::uint16_t >       Vector4_uint16;
typedef  Vector4< std::uint32_t >       Vector4_uint32;
typedef  Vector4< std::uint64_t >       Vector4_uint64;
typedef  Vector4< float >               Vector4_float;
typedef  Vector4< double >              Vector4_double;
typedef  Vector4< long double >         Vector4_longdouble;
typedef  Vector4< char >                Vector4_char;
typedef  Vector4< unsigned char >       Vector4_uchar;
typedef  Vector4< signed char >         Vector4_schar;
typedef  Vector4< std::string >         Vector4_string;

} // ObjexxFCL

#endif // ObjexxFCL_Vector3_fwd_hh_INCLUDED
