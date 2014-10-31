#ifndef ObjexxFCL_FArray4D_fwd_hh_INCLUDED
#define ObjexxFCL_FArray4D_fwd_hh_INCLUDED

// FArray4D Forward Declarations
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
template< typename > class FArray4D;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray4D< bool >                FArray4D_bool;
typedef  FArray4D< byte >                FArray4D_byte;
typedef  FArray4D< sbyte >               FArray4D_sbyte;
typedef  FArray4D< ubyte >               FArray4D_ubyte;
typedef  FArray4D< short int >           FArray4D_short;
typedef  FArray4D< int >                 FArray4D_int;
typedef  FArray4D< long int >            FArray4D_long;
typedef  FArray4D< unsigned short int >  FArray4D_ushort;
typedef  FArray4D< unsigned int >        FArray4D_uint;
typedef  FArray4D< unsigned long int >   FArray4D_ulong;
typedef  FArray4D< std::size_t >         FArray4D_size;
typedef  FArray4D< std::int8_t >         FArray4D_int8;
typedef  FArray4D< std::int16_t >        FArray4D_int16;
typedef  FArray4D< std::int32_t >        FArray4D_int32;
typedef  FArray4D< std::int64_t >        FArray4D_int64;
typedef  FArray4D< std::uint8_t >        FArray4D_uint8;
typedef  FArray4D< std::uint16_t >       FArray4D_uint16;
typedef  FArray4D< std::uint32_t >       FArray4D_uint32;
typedef  FArray4D< std::uint64_t >       FArray4D_uint64;
typedef  FArray4D< float >               FArray4D_float;
typedef  FArray4D< double >              FArray4D_double;
typedef  FArray4D< long double >         FArray4D_longdouble;
typedef  FArray4D< char >                FArray4D_char;
typedef  FArray4D< unsigned char >       FArray4D_uchar;
typedef  FArray4D< signed char >         FArray4D_schar;
typedef  FArray4D< std::string >         FArray4D_string;
typedef  FArray4D< Fstring >             FArray4D_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray4D_fwd_hh_INCLUDED
