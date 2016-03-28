#ifndef ObjexxFCL_Array_fwd_hh_INCLUDED
#define ObjexxFCL_Array_fwd_hh_INCLUDED

// Array Forward Declarations
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
template< typename > class Array;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array< bool >                Array_bool;
typedef  Array< byte >                Array_byte;
typedef  Array< sbyte >               Array_sbyte;
typedef  Array< ubyte >               Array_ubyte;
typedef  Array< short int >           Array_short;
typedef  Array< int >                 Array_int;
typedef  Array< long int >            Array_long;
typedef  Array< unsigned short int >  Array_ushort;
typedef  Array< unsigned int >        Array_uint;
typedef  Array< unsigned long int >   Array_ulong;
typedef  Array< std::size_t >         Array_size;
typedef  Array< std::int8_t >         Array_int8;
typedef  Array< std::int16_t >        Array_int16;
typedef  Array< std::int32_t >        Array_int32;
typedef  Array< std::int64_t >        Array_int64;
typedef  Array< std::uint8_t >        Array_uint8;
typedef  Array< std::uint16_t >       Array_uint16;
typedef  Array< std::uint32_t >       Array_uint32;
typedef  Array< std::uint64_t >       Array_uint64;
typedef  Array< float >               Array_float;
typedef  Array< double >              Array_double;
typedef  Array< long double >         Array_longdouble;
typedef  Array< char >                Array_char;
typedef  Array< unsigned char >       Array_uchar;
typedef  Array< signed char >         Array_schar;
typedef  Array< std::string >         Array_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array_fwd_hh_INCLUDED
