#ifndef ObjexxFCL_FArray2D_fwd_hh_INCLUDED
#define ObjexxFCL_FArray2D_fwd_hh_INCLUDED

// FArray2D Forward Declarations
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2014 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// C++ Headers
#include <cstddef>
#include <cstdint>
#include <string>

namespace ObjexxFCL {

// Forward
template< typename > class FArray2D;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray2D< bool >                FArray2D_bool;
typedef  FArray2D< byte >                FArray2D_byte;
typedef  FArray2D< sbyte >               FArray2D_sbyte;
typedef  FArray2D< ubyte >               FArray2D_ubyte;
typedef  FArray2D< short int >           FArray2D_short;
typedef  FArray2D< int >                 FArray2D_int;
typedef  FArray2D< long int >            FArray2D_long;
typedef  FArray2D< unsigned short int >  FArray2D_ushort;
typedef  FArray2D< unsigned int >        FArray2D_uint;
typedef  FArray2D< unsigned long int >   FArray2D_ulong;
typedef  FArray2D< std::size_t >         FArray2D_size;
typedef  FArray2D< std::int8_t >         FArray2D_int8;
typedef  FArray2D< std::int16_t >        FArray2D_int16;
typedef  FArray2D< std::int32_t >        FArray2D_int32;
typedef  FArray2D< std::int64_t >        FArray2D_int64;
typedef  FArray2D< std::uint8_t >        FArray2D_uint8;
typedef  FArray2D< std::uint16_t >       FArray2D_uint16;
typedef  FArray2D< std::uint32_t >       FArray2D_uint32;
typedef  FArray2D< std::uint64_t >       FArray2D_uint64;
typedef  FArray2D< float >               FArray2D_float;
typedef  FArray2D< double >              FArray2D_double;
typedef  FArray2D< long double >         FArray2D_longdouble;
typedef  FArray2D< char >                FArray2D_char;
typedef  FArray2D< unsigned char >       FArray2D_uchar;
typedef  FArray2D< signed char >         FArray2D_schar;
typedef  FArray2D< std::string >         FArray2D_string;
typedef  FArray2D< Fstring >             FArray2D_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray2D_fwd_hh_INCLUDED
