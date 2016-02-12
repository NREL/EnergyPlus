#ifndef ObjexxFCL_ArrayS_fwd_hh_INCLUDED
#define ObjexxFCL_ArrayS_fwd_hh_INCLUDED

// ArrayS Forward Declarations
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
template< typename > class ArrayS;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  ArrayS< bool >                ArrayS_bool;
typedef  ArrayS< byte >                ArrayS_byte;
typedef  ArrayS< sbyte >               ArrayS_sbyte;
typedef  ArrayS< ubyte >               ArrayS_ubyte;
typedef  ArrayS< short int >           ArrayS_short;
typedef  ArrayS< int >                 ArrayS_int;
typedef  ArrayS< long int >            ArrayS_long;
typedef  ArrayS< unsigned short int >  ArrayS_ushort;
typedef  ArrayS< unsigned int >        ArrayS_uint;
typedef  ArrayS< unsigned long int >   ArrayS_ulong;
typedef  ArrayS< std::size_t >         ArrayS_size;
typedef  ArrayS< std::int8_t >         ArrayS_int8;
typedef  ArrayS< std::int16_t >        ArrayS_int16;
typedef  ArrayS< std::int32_t >        ArrayS_int32;
typedef  ArrayS< std::int64_t >        ArrayS_int64;
typedef  ArrayS< std::uint8_t >        ArrayS_uint8;
typedef  ArrayS< std::uint16_t >       ArrayS_uint16;
typedef  ArrayS< std::uint32_t >       ArrayS_uint32;
typedef  ArrayS< std::uint64_t >       ArrayS_uint64;
typedef  ArrayS< float >               ArrayS_float;
typedef  ArrayS< double >              ArrayS_double;
typedef  ArrayS< long double >         ArrayS_longdouble;
typedef  ArrayS< char >                ArrayS_char;
typedef  ArrayS< unsigned char >       ArrayS_uchar;
typedef  ArrayS< signed char >         ArrayS_schar;
typedef  ArrayS< std::string >         ArrayS_string;

} // ObjexxFCL

#endif // ObjexxFCL_ArrayS_fwd_hh_INCLUDED
