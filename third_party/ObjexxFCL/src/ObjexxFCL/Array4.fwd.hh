#ifndef ObjexxFCL_Array4_fwd_hh_INCLUDED
#define ObjexxFCL_Array4_fwd_hh_INCLUDED

// Array4 Forward Declarations
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
template< typename > class Array4;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array4< bool >                Array4_bool;
typedef  Array4< byte >                Array4_byte;
typedef  Array4< sbyte >               Array4_sbyte;
typedef  Array4< ubyte >               Array4_ubyte;
typedef  Array4< short int >           Array4_short;
typedef  Array4< int >                 Array4_int;
typedef  Array4< long int >            Array4_long;
typedef  Array4< unsigned short int >  Array4_ushort;
typedef  Array4< unsigned int >        Array4_uint;
typedef  Array4< unsigned long int >   Array4_ulong;
typedef  Array4< std::size_t >         Array4_size;
typedef  Array4< std::int8_t >         Array4_int8;
typedef  Array4< std::int16_t >        Array4_int16;
typedef  Array4< std::int32_t >        Array4_int32;
typedef  Array4< std::int64_t >        Array4_int64;
typedef  Array4< std::uint8_t >        Array4_uint8;
typedef  Array4< std::uint16_t >       Array4_uint16;
typedef  Array4< std::uint32_t >       Array4_uint32;
typedef  Array4< std::uint64_t >       Array4_uint64;
typedef  Array4< float >               Array4_float;
typedef  Array4< double >              Array4_double;
typedef  Array4< long double >         Array4_longdouble;
typedef  Array4< char >                Array4_char;
typedef  Array4< unsigned char >       Array4_uchar;
typedef  Array4< signed char >         Array4_schar;
typedef  Array4< std::string >         Array4_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array4_fwd_hh_INCLUDED
