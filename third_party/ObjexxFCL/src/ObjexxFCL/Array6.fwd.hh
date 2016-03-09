#ifndef ObjexxFCL_Array6_fwd_hh_INCLUDED
#define ObjexxFCL_Array6_fwd_hh_INCLUDED

// Array6 Forward Declarations
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
template< typename > class Array6;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array6< bool >                Array6_bool;
typedef  Array6< byte >                Array6_byte;
typedef  Array6< sbyte >               Array6_sbyte;
typedef  Array6< ubyte >               Array6_ubyte;
typedef  Array6< short int >           Array6_short;
typedef  Array6< int >                 Array6_int;
typedef  Array6< long int >            Array6_long;
typedef  Array6< unsigned short int >  Array6_ushort;
typedef  Array6< unsigned int >        Array6_uint;
typedef  Array6< unsigned long int >   Array6_ulong;
typedef  Array6< std::size_t >         Array6_size;
typedef  Array6< std::int8_t >         Array6_int8;
typedef  Array6< std::int16_t >        Array6_int16;
typedef  Array6< std::int32_t >        Array6_int32;
typedef  Array6< std::int64_t >        Array6_int64;
typedef  Array6< std::uint8_t >        Array6_uint8;
typedef  Array6< std::uint16_t >       Array6_uint16;
typedef  Array6< std::uint32_t >       Array6_uint32;
typedef  Array6< std::uint64_t >       Array6_uint64;
typedef  Array6< float >               Array6_float;
typedef  Array6< double >              Array6_double;
typedef  Array6< long double >         Array6_longdouble;
typedef  Array6< char >                Array6_char;
typedef  Array6< unsigned char >       Array6_uchar;
typedef  Array6< signed char >         Array6_schar;
typedef  Array6< std::string >         Array6_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array6_fwd_hh_INCLUDED
