#ifndef ObjexxFCL_Array3A_fwd_hh_INCLUDED
#define ObjexxFCL_Array3A_fwd_hh_INCLUDED

// Array3A Forward Declarations
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
template< typename > class Array3A;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array3A< bool >                Array3A_bool;
typedef  Array3A< byte >                Array3A_byte;
typedef  Array3A< sbyte >               Array3A_sbyte;
typedef  Array3A< ubyte >               Array3A_ubyte;
typedef  Array3A< short int >           Array3A_short;
typedef  Array3A< int >                 Array3A_int;
typedef  Array3A< long int >            Array3A_long;
typedef  Array3A< unsigned short int >  Array3A_ushort;
typedef  Array3A< unsigned int >        Array3A_uint;
typedef  Array3A< unsigned long int >   Array3A_ulong;
typedef  Array3A< std::size_t >         Array3A_size;
typedef  Array3A< std::int8_t >         Array3A_int8;
typedef  Array3A< std::int16_t >        Array3A_int16;
typedef  Array3A< std::int32_t >        Array3A_int32;
typedef  Array3A< std::int64_t >        Array3A_int64;
typedef  Array3A< std::uint8_t >        Array3A_uint8;
typedef  Array3A< std::uint16_t >       Array3A_uint16;
typedef  Array3A< std::uint32_t >       Array3A_uint32;
typedef  Array3A< std::uint64_t >       Array3A_uint64;
typedef  Array3A< float >               Array3A_float;
typedef  Array3A< double >              Array3A_double;
typedef  Array3A< long double >         Array3A_longdouble;
typedef  Array3A< char >                Array3A_char;
typedef  Array3A< unsigned char >       Array3A_uchar;
typedef  Array3A< signed char >         Array3A_schar;
typedef  Array3A< std::string >         Array3A_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array3A_fwd_hh_INCLUDED
