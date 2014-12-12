#ifndef ObjexxFCL_FArray5D_fwd_hh_INCLUDED
#define ObjexxFCL_FArray5D_fwd_hh_INCLUDED

// FArray5D Forward Declarations
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
template< typename > class FArray5D;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray5D< bool >                FArray5D_bool;
typedef  FArray5D< byte >                FArray5D_byte;
typedef  FArray5D< sbyte >               FArray5D_sbyte;
typedef  FArray5D< ubyte >               FArray5D_ubyte;
typedef  FArray5D< short int >           FArray5D_short;
typedef  FArray5D< int >                 FArray5D_int;
typedef  FArray5D< long int >            FArray5D_long;
typedef  FArray5D< unsigned short int >  FArray5D_ushort;
typedef  FArray5D< unsigned int >        FArray5D_uint;
typedef  FArray5D< unsigned long int >   FArray5D_ulong;
typedef  FArray5D< std::size_t >         FArray5D_size;
typedef  FArray5D< std::int8_t >         FArray5D_int8;
typedef  FArray5D< std::int16_t >        FArray5D_int16;
typedef  FArray5D< std::int32_t >        FArray5D_int32;
typedef  FArray5D< std::int64_t >        FArray5D_int64;
typedef  FArray5D< std::uint8_t >        FArray5D_uint8;
typedef  FArray5D< std::uint16_t >       FArray5D_uint16;
typedef  FArray5D< std::uint32_t >       FArray5D_uint32;
typedef  FArray5D< std::uint64_t >       FArray5D_uint64;
typedef  FArray5D< float >               FArray5D_float;
typedef  FArray5D< double >              FArray5D_double;
typedef  FArray5D< long double >         FArray5D_longdouble;
typedef  FArray5D< char >                FArray5D_char;
typedef  FArray5D< unsigned char >       FArray5D_uchar;
typedef  FArray5D< signed char >         FArray5D_schar;
typedef  FArray5D< std::string >         FArray5D_string;
typedef  FArray5D< Fstring >             FArray5D_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray5D_fwd_hh_INCLUDED
