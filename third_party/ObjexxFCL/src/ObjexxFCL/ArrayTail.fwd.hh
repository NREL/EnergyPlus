#ifndef ObjexxFCL_ArrayTail_fwd_hh_INCLUDED
#define ObjexxFCL_ArrayTail_fwd_hh_INCLUDED

// ArrayTail Forward Declarations
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
template< typename > class ArrayTail;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  ArrayTail< bool >                ArrayTail_bool;
typedef  ArrayTail< byte >                ArrayTail_byte;
typedef  ArrayTail< sbyte >               ArrayTail_sbyte;
typedef  ArrayTail< ubyte >               ArrayTail_ubyte;
typedef  ArrayTail< short int >           ArrayTail_short;
typedef  ArrayTail< int >                 ArrayTail_int;
typedef  ArrayTail< long int >            ArrayTail_long;
typedef  ArrayTail< unsigned short int >  ArrayTail_ushort;
typedef  ArrayTail< unsigned int >        ArrayTail_uint;
typedef  ArrayTail< unsigned long int >   ArrayTail_ulong;
typedef  ArrayTail< std::size_t >         ArrayTail_size;
typedef  ArrayTail< std::int8_t >         ArrayTail_int8;
typedef  ArrayTail< std::int16_t >        ArrayTail_int16;
typedef  ArrayTail< std::int32_t >        ArrayTail_int32;
typedef  ArrayTail< std::int64_t >        ArrayTail_int64;
typedef  ArrayTail< std::uint8_t >        ArrayTail_uint8;
typedef  ArrayTail< std::uint16_t >       ArrayTail_uint16;
typedef  ArrayTail< std::uint32_t >       ArrayTail_uint32;
typedef  ArrayTail< std::uint64_t >       ArrayTail_uint64;
typedef  ArrayTail< float >               ArrayTail_float;
typedef  ArrayTail< double >              ArrayTail_double;
typedef  ArrayTail< long double >         ArrayTail_longdouble;
typedef  ArrayTail< char >                ArrayTail_char;
typedef  ArrayTail< unsigned char >       ArrayTail_uchar;
typedef  ArrayTail< signed char >         ArrayTail_schar;
typedef  ArrayTail< std::string >         ArrayTail_string;

} // ObjexxFCL

#endif // ObjexxFCL_ArrayTail_fwd_hh_INCLUDED
