#ifndef ObjexxFCL_Array3D_fwd_hh_INCLUDED
#define ObjexxFCL_Array3D_fwd_hh_INCLUDED

// Array3D Forward Declarations
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
template< typename > class Array3D;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array3D< bool >                Array3D_bool;
typedef  Array3D< byte >                Array3D_byte;
typedef  Array3D< sbyte >               Array3D_sbyte;
typedef  Array3D< ubyte >               Array3D_ubyte;
typedef  Array3D< short int >           Array3D_short;
typedef  Array3D< int >                 Array3D_int;
typedef  Array3D< long int >            Array3D_long;
typedef  Array3D< unsigned short int >  Array3D_ushort;
typedef  Array3D< unsigned int >        Array3D_uint;
typedef  Array3D< unsigned long int >   Array3D_ulong;
typedef  Array3D< std::size_t >         Array3D_size;
typedef  Array3D< std::int8_t >         Array3D_int8;
typedef  Array3D< std::int16_t >        Array3D_int16;
typedef  Array3D< std::int32_t >        Array3D_int32;
typedef  Array3D< std::int64_t >        Array3D_int64;
typedef  Array3D< std::uint8_t >        Array3D_uint8;
typedef  Array3D< std::uint16_t >       Array3D_uint16;
typedef  Array3D< std::uint32_t >       Array3D_uint32;
typedef  Array3D< std::uint64_t >       Array3D_uint64;
typedef  Array3D< float >               Array3D_float;
typedef  Array3D< double >              Array3D_double;
typedef  Array3D< long double >         Array3D_longdouble;
typedef  Array3D< char >                Array3D_char;
typedef  Array3D< unsigned char >       Array3D_uchar;
typedef  Array3D< signed char >         Array3D_schar;
typedef  Array3D< std::string >         Array3D_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array3D_fwd_hh_INCLUDED
