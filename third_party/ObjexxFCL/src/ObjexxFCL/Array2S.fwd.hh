#ifndef ObjexxFCL_Array2S_fwd_hh_INCLUDED
#define ObjexxFCL_Array2S_fwd_hh_INCLUDED

// Array2S Forward Declarations
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
template< typename > class Array2S;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array2S< bool >                Array2S_bool;
typedef  Array2S< byte >                Array2S_byte;
typedef  Array2S< sbyte >               Array2S_sbyte;
typedef  Array2S< ubyte >               Array2S_ubyte;
typedef  Array2S< short int >           Array2S_short;
typedef  Array2S< int >                 Array2S_int;
typedef  Array2S< long int >            Array2S_long;
typedef  Array2S< unsigned short int >  Array2S_ushort;
typedef  Array2S< unsigned int >        Array2S_uint;
typedef  Array2S< unsigned long int >   Array2S_ulong;
typedef  Array2S< std::size_t >         Array2S_size;
typedef  Array2S< std::int8_t >         Array2S_int8;
typedef  Array2S< std::int16_t >        Array2S_int16;
typedef  Array2S< std::int32_t >        Array2S_int32;
typedef  Array2S< std::int64_t >        Array2S_int64;
typedef  Array2S< std::uint8_t >        Array2S_uint8;
typedef  Array2S< std::uint16_t >       Array2S_uint16;
typedef  Array2S< std::uint32_t >       Array2S_uint32;
typedef  Array2S< std::uint64_t >       Array2S_uint64;
typedef  Array2S< float >               Array2S_float;
typedef  Array2S< double >              Array2S_double;
typedef  Array2S< long double >         Array2S_longdouble;
typedef  Array2S< char >                Array2S_char;
typedef  Array2S< unsigned char >       Array2S_uchar;
typedef  Array2S< signed char >         Array2S_schar;
typedef  Array2S< std::string >         Array2S_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array2S_fwd_hh_INCLUDED
