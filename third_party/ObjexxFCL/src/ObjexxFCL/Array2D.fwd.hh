#ifndef ObjexxFCL_Array2D_fwd_hh_INCLUDED
#define ObjexxFCL_Array2D_fwd_hh_INCLUDED

// Array2D Forward Declarations
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
template< typename > class Array2D;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array2D< bool >                Array2D_bool;
typedef  Array2D< byte >                Array2D_byte;
typedef  Array2D< sbyte >               Array2D_sbyte;
typedef  Array2D< ubyte >               Array2D_ubyte;
typedef  Array2D< short int >           Array2D_short;
typedef  Array2D< int >                 Array2D_int;
typedef  Array2D< long int >            Array2D_long;
typedef  Array2D< unsigned short int >  Array2D_ushort;
typedef  Array2D< unsigned int >        Array2D_uint;
typedef  Array2D< unsigned long int >   Array2D_ulong;
typedef  Array2D< std::size_t >         Array2D_size;
typedef  Array2D< std::int8_t >         Array2D_int8;
typedef  Array2D< std::int16_t >        Array2D_int16;
typedef  Array2D< std::int32_t >        Array2D_int32;
typedef  Array2D< std::int64_t >        Array2D_int64;
typedef  Array2D< std::uint8_t >        Array2D_uint8;
typedef  Array2D< std::uint16_t >       Array2D_uint16;
typedef  Array2D< std::uint32_t >       Array2D_uint32;
typedef  Array2D< std::uint64_t >       Array2D_uint64;
typedef  Array2D< float >               Array2D_float;
typedef  Array2D< double >              Array2D_double;
typedef  Array2D< long double >         Array2D_longdouble;
typedef  Array2D< char >                Array2D_char;
typedef  Array2D< unsigned char >       Array2D_uchar;
typedef  Array2D< signed char >         Array2D_schar;
typedef  Array2D< std::string >         Array2D_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array2D_fwd_hh_INCLUDED
