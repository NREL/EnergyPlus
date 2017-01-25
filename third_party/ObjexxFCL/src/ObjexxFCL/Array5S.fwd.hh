#ifndef ObjexxFCL_Array5S_fwd_hh_INCLUDED
#define ObjexxFCL_Array5S_fwd_hh_INCLUDED

// Array5S Forward Declarations
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
template< typename > class Array5S;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array5S< bool >                Array5S_bool;
typedef  Array5S< byte >                Array5S_byte;
typedef  Array5S< sbyte >               Array5S_sbyte;
typedef  Array5S< ubyte >               Array5S_ubyte;
typedef  Array5S< short int >           Array5S_short;
typedef  Array5S< int >                 Array5S_int;
typedef  Array5S< long int >            Array5S_long;
typedef  Array5S< unsigned short int >  Array5S_ushort;
typedef  Array5S< unsigned int >        Array5S_uint;
typedef  Array5S< unsigned long int >   Array5S_ulong;
typedef  Array5S< std::size_t >         Array5S_size;
typedef  Array5S< std::int8_t >         Array5S_int8;
typedef  Array5S< std::int16_t >        Array5S_int16;
typedef  Array5S< std::int32_t >        Array5S_int32;
typedef  Array5S< std::int64_t >        Array5S_int64;
typedef  Array5S< std::uint8_t >        Array5S_uint8;
typedef  Array5S< std::uint16_t >       Array5S_uint16;
typedef  Array5S< std::uint32_t >       Array5S_uint32;
typedef  Array5S< std::uint64_t >       Array5S_uint64;
typedef  Array5S< float >               Array5S_float;
typedef  Array5S< double >              Array5S_double;
typedef  Array5S< long double >         Array5S_longdouble;
typedef  Array5S< char >                Array5S_char;
typedef  Array5S< unsigned char >       Array5S_uchar;
typedef  Array5S< signed char >         Array5S_schar;
typedef  Array5S< std::string >         Array5S_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array5S_fwd_hh_INCLUDED
