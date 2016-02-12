#ifndef ObjexxFCL_Array1_fwd_hh_INCLUDED
#define ObjexxFCL_Array1_fwd_hh_INCLUDED

// Array1 Forward Declarations
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
template< typename > class Array1;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array1< bool >                Array1_bool;
typedef  Array1< byte >                Array1_byte;
typedef  Array1< sbyte >               Array1_sbyte;
typedef  Array1< ubyte >               Array1_ubyte;
typedef  Array1< short int >           Array1_short;
typedef  Array1< int >                 Array1_int;
typedef  Array1< long int >            Array1_long;
typedef  Array1< unsigned short int >  Array1_ushort;
typedef  Array1< unsigned int >        Array1_uint;
typedef  Array1< unsigned long int >   Array1_ulong;
typedef  Array1< std::size_t >         Array1_size;
typedef  Array1< std::int8_t >         Array1_int8;
typedef  Array1< std::int16_t >        Array1_int16;
typedef  Array1< std::int32_t >        Array1_int32;
typedef  Array1< std::int64_t >        Array1_int64;
typedef  Array1< std::uint8_t >        Array1_uint8;
typedef  Array1< std::uint16_t >       Array1_uint16;
typedef  Array1< std::uint32_t >       Array1_uint32;
typedef  Array1< std::uint64_t >       Array1_uint64;
typedef  Array1< float >               Array1_float;
typedef  Array1< double >              Array1_double;
typedef  Array1< long double >         Array1_longdouble;
typedef  Array1< char >                Array1_char;
typedef  Array1< unsigned char >       Array1_uchar;
typedef  Array1< signed char >         Array1_schar;
typedef  Array1< std::string >         Array1_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array1_fwd_hh_INCLUDED
