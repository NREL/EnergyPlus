#ifndef ObjexxFCL_Array6S_fwd_hh_INCLUDED
#define ObjexxFCL_Array6S_fwd_hh_INCLUDED

// Array6S Forward Declarations
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
template< typename > class Array6S;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array6S< bool >                Array6S_bool;
typedef  Array6S< byte >                Array6S_byte;
typedef  Array6S< sbyte >               Array6S_sbyte;
typedef  Array6S< ubyte >               Array6S_ubyte;
typedef  Array6S< short int >           Array6S_short;
typedef  Array6S< int >                 Array6S_int;
typedef  Array6S< long int >            Array6S_long;
typedef  Array6S< unsigned short int >  Array6S_ushort;
typedef  Array6S< unsigned int >        Array6S_uint;
typedef  Array6S< unsigned long int >   Array6S_ulong;
typedef  Array6S< std::size_t >         Array6S_size;
typedef  Array6S< std::int8_t >         Array6S_int8;
typedef  Array6S< std::int16_t >        Array6S_int16;
typedef  Array6S< std::int32_t >        Array6S_int32;
typedef  Array6S< std::int64_t >        Array6S_int64;
typedef  Array6S< std::uint8_t >        Array6S_uint8;
typedef  Array6S< std::uint16_t >       Array6S_uint16;
typedef  Array6S< std::uint32_t >       Array6S_uint32;
typedef  Array6S< std::uint64_t >       Array6S_uint64;
typedef  Array6S< float >               Array6S_float;
typedef  Array6S< double >              Array6S_double;
typedef  Array6S< long double >         Array6S_longdouble;
typedef  Array6S< char >                Array6S_char;
typedef  Array6S< unsigned char >       Array6S_uchar;
typedef  Array6S< signed char >         Array6S_schar;
typedef  Array6S< std::string >         Array6S_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array6S_fwd_hh_INCLUDED
