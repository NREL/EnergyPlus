#ifndef ObjexxFCL_Array5D_fwd_hh_INCLUDED
#define ObjexxFCL_Array5D_fwd_hh_INCLUDED

// Array5D Forward Declarations
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
template< typename > class Array5D;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array5D< bool >                Array5D_bool;
typedef  Array5D< byte >                Array5D_byte;
typedef  Array5D< sbyte >               Array5D_sbyte;
typedef  Array5D< ubyte >               Array5D_ubyte;
typedef  Array5D< short int >           Array5D_short;
typedef  Array5D< int >                 Array5D_int;
typedef  Array5D< long int >            Array5D_long;
typedef  Array5D< unsigned short int >  Array5D_ushort;
typedef  Array5D< unsigned int >        Array5D_uint;
typedef  Array5D< unsigned long int >   Array5D_ulong;
typedef  Array5D< std::size_t >         Array5D_size;
typedef  Array5D< std::int8_t >         Array5D_int8;
typedef  Array5D< std::int16_t >        Array5D_int16;
typedef  Array5D< std::int32_t >        Array5D_int32;
typedef  Array5D< std::int64_t >        Array5D_int64;
typedef  Array5D< std::uint8_t >        Array5D_uint8;
typedef  Array5D< std::uint16_t >       Array5D_uint16;
typedef  Array5D< std::uint32_t >       Array5D_uint32;
typedef  Array5D< std::uint64_t >       Array5D_uint64;
typedef  Array5D< float >               Array5D_float;
typedef  Array5D< double >              Array5D_double;
typedef  Array5D< long double >         Array5D_longdouble;
typedef  Array5D< char >                Array5D_char;
typedef  Array5D< unsigned char >       Array5D_uchar;
typedef  Array5D< signed char >         Array5D_schar;
typedef  Array5D< std::string >         Array5D_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array5D_fwd_hh_INCLUDED
