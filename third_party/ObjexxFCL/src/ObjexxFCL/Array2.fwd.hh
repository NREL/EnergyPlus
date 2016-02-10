#ifndef ObjexxFCL_Array2_fwd_hh_INCLUDED
#define ObjexxFCL_Array2_fwd_hh_INCLUDED

// Array2 Forward Declarations
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
template< typename > class Array2;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array2< bool >                Array2_bool;
typedef  Array2< byte >                Array2_byte;
typedef  Array2< sbyte >               Array2_sbyte;
typedef  Array2< ubyte >               Array2_ubyte;
typedef  Array2< short int >           Array2_short;
typedef  Array2< int >                 Array2_int;
typedef  Array2< long int >            Array2_long;
typedef  Array2< unsigned short int >  Array2_ushort;
typedef  Array2< unsigned int >        Array2_uint;
typedef  Array2< unsigned long int >   Array2_ulong;
typedef  Array2< std::size_t >         Array2_size;
typedef  Array2< std::int8_t >         Array2_int8;
typedef  Array2< std::int16_t >        Array2_int16;
typedef  Array2< std::int32_t >        Array2_int32;
typedef  Array2< std::int64_t >        Array2_int64;
typedef  Array2< std::uint8_t >        Array2_uint8;
typedef  Array2< std::uint16_t >       Array2_uint16;
typedef  Array2< std::uint32_t >       Array2_uint32;
typedef  Array2< std::uint64_t >       Array2_uint64;
typedef  Array2< float >               Array2_float;
typedef  Array2< double >              Array2_double;
typedef  Array2< long double >         Array2_longdouble;
typedef  Array2< char >                Array2_char;
typedef  Array2< unsigned char >       Array2_uchar;
typedef  Array2< signed char >         Array2_schar;
typedef  Array2< std::string >         Array2_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array2_fwd_hh_INCLUDED
