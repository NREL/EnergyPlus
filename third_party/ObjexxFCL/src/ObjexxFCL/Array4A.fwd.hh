#ifndef ObjexxFCL_Array4A_fwd_hh_INCLUDED
#define ObjexxFCL_Array4A_fwd_hh_INCLUDED

// Array4A Forward Declarations
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
template< typename > class Array4A;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array4A< bool >                Array4A_bool;
typedef  Array4A< byte >                Array4A_byte;
typedef  Array4A< sbyte >               Array4A_sbyte;
typedef  Array4A< ubyte >               Array4A_ubyte;
typedef  Array4A< short int >           Array4A_short;
typedef  Array4A< int >                 Array4A_int;
typedef  Array4A< long int >            Array4A_long;
typedef  Array4A< unsigned short int >  Array4A_ushort;
typedef  Array4A< unsigned int >        Array4A_uint;
typedef  Array4A< unsigned long int >   Array4A_ulong;
typedef  Array4A< std::size_t >         Array4A_size;
typedef  Array4A< std::int8_t >         Array4A_int8;
typedef  Array4A< std::int16_t >        Array4A_int16;
typedef  Array4A< std::int32_t >        Array4A_int32;
typedef  Array4A< std::int64_t >        Array4A_int64;
typedef  Array4A< std::uint8_t >        Array4A_uint8;
typedef  Array4A< std::uint16_t >       Array4A_uint16;
typedef  Array4A< std::uint32_t >       Array4A_uint32;
typedef  Array4A< std::uint64_t >       Array4A_uint64;
typedef  Array4A< float >               Array4A_float;
typedef  Array4A< double >              Array4A_double;
typedef  Array4A< long double >         Array4A_longdouble;
typedef  Array4A< char >                Array4A_char;
typedef  Array4A< unsigned char >       Array4A_uchar;
typedef  Array4A< signed char >         Array4A_schar;
typedef  Array4A< std::string >         Array4A_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array4A_fwd_hh_INCLUDED
