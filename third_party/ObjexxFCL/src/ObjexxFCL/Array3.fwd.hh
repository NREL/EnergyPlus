#ifndef ObjexxFCL_Array3_fwd_hh_INCLUDED
#define ObjexxFCL_Array3_fwd_hh_INCLUDED

// Array3 Forward Declarations
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
template< typename > class Array3;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array3< bool >                Array3_bool;
typedef  Array3< byte >                Array3_byte;
typedef  Array3< sbyte >               Array3_sbyte;
typedef  Array3< ubyte >               Array3_ubyte;
typedef  Array3< short int >           Array3_short;
typedef  Array3< int >                 Array3_int;
typedef  Array3< long int >            Array3_long;
typedef  Array3< unsigned short int >  Array3_ushort;
typedef  Array3< unsigned int >        Array3_uint;
typedef  Array3< unsigned long int >   Array3_ulong;
typedef  Array3< std::size_t >         Array3_size;
typedef  Array3< std::int8_t >         Array3_int8;
typedef  Array3< std::int16_t >        Array3_int16;
typedef  Array3< std::int32_t >        Array3_int32;
typedef  Array3< std::int64_t >        Array3_int64;
typedef  Array3< std::uint8_t >        Array3_uint8;
typedef  Array3< std::uint16_t >       Array3_uint16;
typedef  Array3< std::uint32_t >       Array3_uint32;
typedef  Array3< std::uint64_t >       Array3_uint64;
typedef  Array3< float >               Array3_float;
typedef  Array3< double >              Array3_double;
typedef  Array3< long double >         Array3_longdouble;
typedef  Array3< char >                Array3_char;
typedef  Array3< unsigned char >       Array3_uchar;
typedef  Array3< signed char >         Array3_schar;
typedef  Array3< std::string >         Array3_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array3_fwd_hh_INCLUDED
