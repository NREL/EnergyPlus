#ifndef ObjexxFCL_Array1D_fwd_hh_INCLUDED
#define ObjexxFCL_Array1D_fwd_hh_INCLUDED

// Array1D Forward Declarations
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
template< typename > class Array1D;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array1D< bool >                Array1D_bool;
typedef  Array1D< byte >                Array1D_byte;
typedef  Array1D< sbyte >               Array1D_sbyte;
typedef  Array1D< ubyte >               Array1D_ubyte;
typedef  Array1D< short int >           Array1D_short;
typedef  Array1D< int >                 Array1D_int;
typedef  Array1D< long int >            Array1D_long;
typedef  Array1D< unsigned short int >  Array1D_ushort;
typedef  Array1D< unsigned int >        Array1D_uint;
typedef  Array1D< unsigned long int >   Array1D_ulong;
typedef  Array1D< std::size_t >         Array1D_size;
typedef  Array1D< std::int8_t >         Array1D_int8;
typedef  Array1D< std::int16_t >        Array1D_int16;
typedef  Array1D< std::int32_t >        Array1D_int32;
typedef  Array1D< std::int64_t >        Array1D_int64;
typedef  Array1D< std::uint8_t >        Array1D_uint8;
typedef  Array1D< std::uint16_t >       Array1D_uint16;
typedef  Array1D< std::uint32_t >       Array1D_uint32;
typedef  Array1D< std::uint64_t >       Array1D_uint64;
typedef  Array1D< float >               Array1D_float;
typedef  Array1D< double >              Array1D_double;
typedef  Array1D< long double >         Array1D_longdouble;
typedef  Array1D< char >                Array1D_char;
typedef  Array1D< unsigned char >       Array1D_uchar;
typedef  Array1D< signed char >         Array1D_schar;
typedef  Array1D< std::string >         Array1D_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array1D_fwd_hh_INCLUDED
