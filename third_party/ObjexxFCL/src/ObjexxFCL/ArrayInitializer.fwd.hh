#ifndef ObjexxFCL_ArrayInitializer_fwd_hh_INCLUDED
#define ObjexxFCL_ArrayInitializer_fwd_hh_INCLUDED

// ArrayInitializer Forward Declarations
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
template< typename > class ArrayInitializer;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  ArrayInitializer< bool >                ArrayInitializer_bool;
typedef  ArrayInitializer< byte >                ArrayInitializer_byte;
typedef  ArrayInitializer< sbyte >               ArrayInitializer_sbyte;
typedef  ArrayInitializer< ubyte >               ArrayInitializer_ubyte;
typedef  ArrayInitializer< short int >           ArrayInitializer_short;
typedef  ArrayInitializer< int >                 ArrayInitializer_int;
typedef  ArrayInitializer< long int >            ArrayInitializer_long;
typedef  ArrayInitializer< unsigned short int >  ArrayInitializer_ushort;
typedef  ArrayInitializer< unsigned int >        ArrayInitializer_uint;
typedef  ArrayInitializer< unsigned long int >   ArrayInitializer_ulong;
typedef  ArrayInitializer< std::size_t >         ArrayInitializer_size;
typedef  ArrayInitializer< std::int8_t >         ArrayInitializer_int8;
typedef  ArrayInitializer< std::int16_t >        ArrayInitializer_int16;
typedef  ArrayInitializer< std::int32_t >        ArrayInitializer_int32;
typedef  ArrayInitializer< std::int64_t >        ArrayInitializer_int64;
typedef  ArrayInitializer< std::uint8_t >        ArrayInitializer_uint8;
typedef  ArrayInitializer< std::uint16_t >       ArrayInitializer_uint16;
typedef  ArrayInitializer< std::uint32_t >       ArrayInitializer_uint32;
typedef  ArrayInitializer< std::uint64_t >       ArrayInitializer_uint64;
typedef  ArrayInitializer< float >               ArrayInitializer_float;
typedef  ArrayInitializer< double >              ArrayInitializer_double;
typedef  ArrayInitializer< long double >         ArrayInitializer_longdouble;
typedef  ArrayInitializer< char >                ArrayInitializer_char;
typedef  ArrayInitializer< unsigned char >       ArrayInitializer_uchar;
typedef  ArrayInitializer< signed char >         ArrayInitializer_schar;
typedef  ArrayInitializer< std::string >         ArrayInitializer_string;

} // ObjexxFCL

#endif // ObjexxFCL_ArrayInitializer_fwd_hh_INCLUDED
