#ifndef ObjexxFCL_Array4D_fwd_hh_INCLUDED
#define ObjexxFCL_Array4D_fwd_hh_INCLUDED

// Array4D Forward Declarations
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
template< typename > class Array4D;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array4D< bool >                Array4D_bool;
typedef  Array4D< byte >                Array4D_byte;
typedef  Array4D< sbyte >               Array4D_sbyte;
typedef  Array4D< ubyte >               Array4D_ubyte;
typedef  Array4D< short int >           Array4D_short;
typedef  Array4D< int >                 Array4D_int;
typedef  Array4D< long int >            Array4D_long;
typedef  Array4D< unsigned short int >  Array4D_ushort;
typedef  Array4D< unsigned int >        Array4D_uint;
typedef  Array4D< unsigned long int >   Array4D_ulong;
typedef  Array4D< std::size_t >         Array4D_size;
typedef  Array4D< std::int8_t >         Array4D_int8;
typedef  Array4D< std::int16_t >        Array4D_int16;
typedef  Array4D< std::int32_t >        Array4D_int32;
typedef  Array4D< std::int64_t >        Array4D_int64;
typedef  Array4D< std::uint8_t >        Array4D_uint8;
typedef  Array4D< std::uint16_t >       Array4D_uint16;
typedef  Array4D< std::uint32_t >       Array4D_uint32;
typedef  Array4D< std::uint64_t >       Array4D_uint64;
typedef  Array4D< float >               Array4D_float;
typedef  Array4D< double >              Array4D_double;
typedef  Array4D< long double >         Array4D_longdouble;
typedef  Array4D< char >                Array4D_char;
typedef  Array4D< unsigned char >       Array4D_uchar;
typedef  Array4D< signed char >         Array4D_schar;
typedef  Array4D< std::string >         Array4D_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array4D_fwd_hh_INCLUDED
