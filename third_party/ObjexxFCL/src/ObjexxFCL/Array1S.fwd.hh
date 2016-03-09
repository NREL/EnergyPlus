#ifndef ObjexxFCL_Array1S_fwd_hh_INCLUDED
#define ObjexxFCL_Array1S_fwd_hh_INCLUDED

// Array1S Forward Declarations
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
template< typename > class Array1S;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array1S< bool >                Array1S_bool;
typedef  Array1S< byte >                Array1S_byte;
typedef  Array1S< sbyte >               Array1S_sbyte;
typedef  Array1S< ubyte >               Array1S_ubyte;
typedef  Array1S< short int >           Array1S_short;
typedef  Array1S< int >                 Array1S_int;
typedef  Array1S< long int >            Array1S_long;
typedef  Array1S< unsigned short int >  Array1S_ushort;
typedef  Array1S< unsigned int >        Array1S_uint;
typedef  Array1S< unsigned long int >   Array1S_ulong;
typedef  Array1S< std::size_t >         Array1S_size;
typedef  Array1S< std::int8_t >         Array1S_int8;
typedef  Array1S< std::int16_t >        Array1S_int16;
typedef  Array1S< std::int32_t >        Array1S_int32;
typedef  Array1S< std::int64_t >        Array1S_int64;
typedef  Array1S< std::uint8_t >        Array1S_uint8;
typedef  Array1S< std::uint16_t >       Array1S_uint16;
typedef  Array1S< std::uint32_t >       Array1S_uint32;
typedef  Array1S< std::uint64_t >       Array1S_uint64;
typedef  Array1S< float >               Array1S_float;
typedef  Array1S< double >              Array1S_double;
typedef  Array1S< long double >         Array1S_longdouble;
typedef  Array1S< char >                Array1S_char;
typedef  Array1S< unsigned char >       Array1S_uchar;
typedef  Array1S< signed char >         Array1S_schar;
typedef  Array1S< std::string >         Array1S_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array1S_fwd_hh_INCLUDED
