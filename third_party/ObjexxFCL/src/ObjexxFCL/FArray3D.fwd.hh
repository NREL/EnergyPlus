#ifndef ObjexxFCL_FArray3D_fwd_hh_INCLUDED
#define ObjexxFCL_FArray3D_fwd_hh_INCLUDED

// FArray3D Forward Declarations
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
template< typename > class FArray3D;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray3D< bool >                FArray3D_bool;
typedef  FArray3D< byte >                FArray3D_byte;
typedef  FArray3D< sbyte >               FArray3D_sbyte;
typedef  FArray3D< ubyte >               FArray3D_ubyte;
typedef  FArray3D< short int >           FArray3D_short;
typedef  FArray3D< int >                 FArray3D_int;
typedef  FArray3D< long int >            FArray3D_long;
typedef  FArray3D< unsigned short int >  FArray3D_ushort;
typedef  FArray3D< unsigned int >        FArray3D_uint;
typedef  FArray3D< unsigned long int >   FArray3D_ulong;
typedef  FArray3D< std::size_t >         FArray3D_size;
typedef  FArray3D< std::int8_t >         FArray3D_int8;
typedef  FArray3D< std::int16_t >        FArray3D_int16;
typedef  FArray3D< std::int32_t >        FArray3D_int32;
typedef  FArray3D< std::int64_t >        FArray3D_int64;
typedef  FArray3D< std::uint8_t >        FArray3D_uint8;
typedef  FArray3D< std::uint16_t >       FArray3D_uint16;
typedef  FArray3D< std::uint32_t >       FArray3D_uint32;
typedef  FArray3D< std::uint64_t >       FArray3D_uint64;
typedef  FArray3D< float >               FArray3D_float;
typedef  FArray3D< double >              FArray3D_double;
typedef  FArray3D< long double >         FArray3D_longdouble;
typedef  FArray3D< char >                FArray3D_char;
typedef  FArray3D< unsigned char >       FArray3D_uchar;
typedef  FArray3D< signed char >         FArray3D_schar;
typedef  FArray3D< std::string >         FArray3D_string;
typedef  FArray3D< Fstring >             FArray3D_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray3D_fwd_hh_INCLUDED
