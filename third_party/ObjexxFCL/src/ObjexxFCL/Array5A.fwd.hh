#ifndef ObjexxFCL_Array5A_fwd_hh_INCLUDED
#define ObjexxFCL_Array5A_fwd_hh_INCLUDED

// Array5A Forward Declarations
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
template< typename > class Array5A;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array5A< bool >                Array5A_bool;
typedef  Array5A< byte >                Array5A_byte;
typedef  Array5A< sbyte >               Array5A_sbyte;
typedef  Array5A< ubyte >               Array5A_ubyte;
typedef  Array5A< short int >           Array5A_short;
typedef  Array5A< int >                 Array5A_int;
typedef  Array5A< long int >            Array5A_long;
typedef  Array5A< unsigned short int >  Array5A_ushort;
typedef  Array5A< unsigned int >        Array5A_uint;
typedef  Array5A< unsigned long int >   Array5A_ulong;
typedef  Array5A< std::size_t >         Array5A_size;
typedef  Array5A< std::int8_t >         Array5A_int8;
typedef  Array5A< std::int16_t >        Array5A_int16;
typedef  Array5A< std::int32_t >        Array5A_int32;
typedef  Array5A< std::int64_t >        Array5A_int64;
typedef  Array5A< std::uint8_t >        Array5A_uint8;
typedef  Array5A< std::uint16_t >       Array5A_uint16;
typedef  Array5A< std::uint32_t >       Array5A_uint32;
typedef  Array5A< std::uint64_t >       Array5A_uint64;
typedef  Array5A< float >               Array5A_float;
typedef  Array5A< double >              Array5A_double;
typedef  Array5A< long double >         Array5A_longdouble;
typedef  Array5A< char >                Array5A_char;
typedef  Array5A< unsigned char >       Array5A_uchar;
typedef  Array5A< signed char >         Array5A_schar;
typedef  Array5A< std::string >         Array5A_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array5A_fwd_hh_INCLUDED
