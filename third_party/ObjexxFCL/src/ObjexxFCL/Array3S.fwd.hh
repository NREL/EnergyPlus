#ifndef ObjexxFCL_Array3S_fwd_hh_INCLUDED
#define ObjexxFCL_Array3S_fwd_hh_INCLUDED

// Array3S Forward Declarations
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
template< typename > class Array3S;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array3S< bool >                Array3S_bool;
typedef  Array3S< byte >                Array3S_byte;
typedef  Array3S< sbyte >               Array3S_sbyte;
typedef  Array3S< ubyte >               Array3S_ubyte;
typedef  Array3S< short int >           Array3S_short;
typedef  Array3S< int >                 Array3S_int;
typedef  Array3S< long int >            Array3S_long;
typedef  Array3S< unsigned short int >  Array3S_ushort;
typedef  Array3S< unsigned int >        Array3S_uint;
typedef  Array3S< unsigned long int >   Array3S_ulong;
typedef  Array3S< std::size_t >         Array3S_size;
typedef  Array3S< std::int8_t >         Array3S_int8;
typedef  Array3S< std::int16_t >        Array3S_int16;
typedef  Array3S< std::int32_t >        Array3S_int32;
typedef  Array3S< std::int64_t >        Array3S_int64;
typedef  Array3S< std::uint8_t >        Array3S_uint8;
typedef  Array3S< std::uint16_t >       Array3S_uint16;
typedef  Array3S< std::uint32_t >       Array3S_uint32;
typedef  Array3S< std::uint64_t >       Array3S_uint64;
typedef  Array3S< float >               Array3S_float;
typedef  Array3S< double >              Array3S_double;
typedef  Array3S< long double >         Array3S_longdouble;
typedef  Array3S< char >                Array3S_char;
typedef  Array3S< unsigned char >       Array3S_uchar;
typedef  Array3S< signed char >         Array3S_schar;
typedef  Array3S< std::string >         Array3S_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array3S_fwd_hh_INCLUDED
