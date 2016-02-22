#ifndef ObjexxFCL_Array6D_fwd_hh_INCLUDED
#define ObjexxFCL_Array6D_fwd_hh_INCLUDED

// Array6D Forward Declarations
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
template< typename > class Array6D;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array6D< bool >                Array6D_bool;
typedef  Array6D< byte >                Array6D_byte;
typedef  Array6D< sbyte >               Array6D_sbyte;
typedef  Array6D< ubyte >               Array6D_ubyte;
typedef  Array6D< short int >           Array6D_short;
typedef  Array6D< int >                 Array6D_int;
typedef  Array6D< long int >            Array6D_long;
typedef  Array6D< unsigned short int >  Array6D_ushort;
typedef  Array6D< unsigned int >        Array6D_uint;
typedef  Array6D< unsigned long int >   Array6D_ulong;
typedef  Array6D< std::size_t >         Array6D_size;
typedef  Array6D< std::int8_t >         Array6D_int8;
typedef  Array6D< std::int16_t >        Array6D_int16;
typedef  Array6D< std::int32_t >        Array6D_int32;
typedef  Array6D< std::int64_t >        Array6D_int64;
typedef  Array6D< std::uint8_t >        Array6D_uint8;
typedef  Array6D< std::uint16_t >       Array6D_uint16;
typedef  Array6D< std::uint32_t >       Array6D_uint32;
typedef  Array6D< std::uint64_t >       Array6D_uint64;
typedef  Array6D< float >               Array6D_float;
typedef  Array6D< double >              Array6D_double;
typedef  Array6D< long double >         Array6D_longdouble;
typedef  Array6D< char >                Array6D_char;
typedef  Array6D< unsigned char >       Array6D_uchar;
typedef  Array6D< signed char >         Array6D_schar;
typedef  Array6D< std::string >         Array6D_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array6D_fwd_hh_INCLUDED
