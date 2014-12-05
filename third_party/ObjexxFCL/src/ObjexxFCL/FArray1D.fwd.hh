#ifndef ObjexxFCL_FArray1D_fwd_hh_INCLUDED
#define ObjexxFCL_FArray1D_fwd_hh_INCLUDED

// FArray1D Forward Declarations
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
template< typename > class FArray1D;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray1D< bool >                FArray1D_bool;
typedef  FArray1D< byte >                FArray1D_byte;
typedef  FArray1D< sbyte >               FArray1D_sbyte;
typedef  FArray1D< ubyte >               FArray1D_ubyte;
typedef  FArray1D< short int >           FArray1D_short;
typedef  FArray1D< int >                 FArray1D_int;
typedef  FArray1D< long int >            FArray1D_long;
typedef  FArray1D< unsigned short int >  FArray1D_ushort;
typedef  FArray1D< unsigned int >        FArray1D_uint;
typedef  FArray1D< unsigned long int >   FArray1D_ulong;
typedef  FArray1D< std::size_t >         FArray1D_size;
typedef  FArray1D< std::int8_t >         FArray1D_int8;
typedef  FArray1D< std::int16_t >        FArray1D_int16;
typedef  FArray1D< std::int32_t >        FArray1D_int32;
typedef  FArray1D< std::int64_t >        FArray1D_int64;
typedef  FArray1D< std::uint8_t >        FArray1D_uint8;
typedef  FArray1D< std::uint16_t >       FArray1D_uint16;
typedef  FArray1D< std::uint32_t >       FArray1D_uint32;
typedef  FArray1D< std::uint64_t >       FArray1D_uint64;
typedef  FArray1D< float >               FArray1D_float;
typedef  FArray1D< double >              FArray1D_double;
typedef  FArray1D< long double >         FArray1D_longdouble;
typedef  FArray1D< char >                FArray1D_char;
typedef  FArray1D< unsigned char >       FArray1D_uchar;
typedef  FArray1D< signed char >         FArray1D_schar;
typedef  FArray1D< std::string >         FArray1D_string;
typedef  FArray1D< Fstring >             FArray1D_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray1D_fwd_hh_INCLUDED
