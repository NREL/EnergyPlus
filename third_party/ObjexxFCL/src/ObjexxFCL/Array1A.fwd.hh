#ifndef ObjexxFCL_Array1A_fwd_hh_INCLUDED
#define ObjexxFCL_Array1A_fwd_hh_INCLUDED

// Array1A Forward Declarations
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
template< typename > class Array1A;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array1A< bool >                Array1A_bool;
typedef  Array1A< byte >                Array1A_byte;
typedef  Array1A< sbyte >               Array1A_sbyte;
typedef  Array1A< ubyte >               Array1A_ubyte;
typedef  Array1A< short int >           Array1A_short;
typedef  Array1A< int >                 Array1A_int;
typedef  Array1A< long int >            Array1A_long;
typedef  Array1A< unsigned short int >  Array1A_ushort;
typedef  Array1A< unsigned int >        Array1A_uint;
typedef  Array1A< unsigned long int >   Array1A_ulong;
typedef  Array1A< std::size_t >         Array1A_size;
typedef  Array1A< std::int8_t >         Array1A_int8;
typedef  Array1A< std::int16_t >        Array1A_int16;
typedef  Array1A< std::int32_t >        Array1A_int32;
typedef  Array1A< std::int64_t >        Array1A_int64;
typedef  Array1A< std::uint8_t >        Array1A_uint8;
typedef  Array1A< std::uint16_t >       Array1A_uint16;
typedef  Array1A< std::uint32_t >       Array1A_uint32;
typedef  Array1A< std::uint64_t >       Array1A_uint64;
typedef  Array1A< float >               Array1A_float;
typedef  Array1A< double >              Array1A_double;
typedef  Array1A< long double >         Array1A_longdouble;
typedef  Array1A< char >                Array1A_char;
typedef  Array1A< unsigned char >       Array1A_uchar;
typedef  Array1A< signed char >         Array1A_schar;
typedef  Array1A< std::string >         Array1A_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array1A_fwd_hh_INCLUDED
