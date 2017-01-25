#ifndef ObjexxFCL_Vector3_fwd_hh_INCLUDED
#define ObjexxFCL_Vector3_fwd_hh_INCLUDED

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
template< typename > class Vector3;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Vector3< bool >                Vector3_bool;
typedef  Vector3< byte >                Vector3_byte;
typedef  Vector3< sbyte >               Vector3_sbyte;
typedef  Vector3< ubyte >               Vector3_ubyte;
typedef  Vector3< short int >           Vector3_short;
typedef  Vector3< int >                 Vector3_int;
typedef  Vector3< long int >            Vector3_long;
typedef  Vector3< unsigned short int >  Vector3_ushort;
typedef  Vector3< unsigned int >        Vector3_uint;
typedef  Vector3< unsigned long int >   Vector3_ulong;
typedef  Vector3< std::size_t >         Vector3_size;
typedef  Vector3< std::int8_t >         Vector3_int8;
typedef  Vector3< std::int16_t >        Vector3_int16;
typedef  Vector3< std::int32_t >        Vector3_int32;
typedef  Vector3< std::int64_t >        Vector3_int64;
typedef  Vector3< std::uint8_t >        Vector3_uint8;
typedef  Vector3< std::uint16_t >       Vector3_uint16;
typedef  Vector3< std::uint32_t >       Vector3_uint32;
typedef  Vector3< std::uint64_t >       Vector3_uint64;
typedef  Vector3< float >               Vector3_float;
typedef  Vector3< double >              Vector3_double;
typedef  Vector3< long double >         Vector3_longdouble;
typedef  Vector3< char >                Vector3_char;
typedef  Vector3< unsigned char >       Vector3_uchar;
typedef  Vector3< signed char >         Vector3_schar;
typedef  Vector3< std::string >         Vector3_string;

} // ObjexxFCL

#endif // ObjexxFCL_Vector3_fwd_hh_INCLUDED
