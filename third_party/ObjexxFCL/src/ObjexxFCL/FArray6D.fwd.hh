#ifndef ObjexxFCL_FArray6D_fwd_hh_INCLUDED
#define ObjexxFCL_FArray6D_fwd_hh_INCLUDED

// FArray6D Forward Declarations
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
template< typename > class FArray6D;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray6D< bool >                FArray6D_bool;
typedef  FArray6D< byte >                FArray6D_byte;
typedef  FArray6D< sbyte >               FArray6D_sbyte;
typedef  FArray6D< ubyte >               FArray6D_ubyte;
typedef  FArray6D< short int >           FArray6D_short;
typedef  FArray6D< int >                 FArray6D_int;
typedef  FArray6D< long int >            FArray6D_long;
typedef  FArray6D< unsigned short int >  FArray6D_ushort;
typedef  FArray6D< unsigned int >        FArray6D_uint;
typedef  FArray6D< unsigned long int >   FArray6D_ulong;
typedef  FArray6D< std::size_t >         FArray6D_size;
typedef  FArray6D< std::int8_t >         FArray6D_int8;
typedef  FArray6D< std::int16_t >        FArray6D_int16;
typedef  FArray6D< std::int32_t >        FArray6D_int32;
typedef  FArray6D< std::int64_t >        FArray6D_int64;
typedef  FArray6D< std::uint8_t >        FArray6D_uint8;
typedef  FArray6D< std::uint16_t >       FArray6D_uint16;
typedef  FArray6D< std::uint32_t >       FArray6D_uint32;
typedef  FArray6D< std::uint64_t >       FArray6D_uint64;
typedef  FArray6D< float >               FArray6D_float;
typedef  FArray6D< double >              FArray6D_double;
typedef  FArray6D< long double >         FArray6D_longdouble;
typedef  FArray6D< char >                FArray6D_char;
typedef  FArray6D< unsigned char >       FArray6D_uchar;
typedef  FArray6D< signed char >         FArray6D_schar;
typedef  FArray6D< std::string >         FArray6D_string;
typedef  FArray6D< Fstring >             FArray6D_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray6D_fwd_hh_INCLUDED
