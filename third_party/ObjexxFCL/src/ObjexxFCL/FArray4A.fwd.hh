#ifndef ObjexxFCL_FArray4A_fwd_hh_INCLUDED
#define ObjexxFCL_FArray4A_fwd_hh_INCLUDED

// FArray4A Forward Declarations
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
template< typename > class FArray4A;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray4A< bool >                FArray4A_bool;
typedef  FArray4A< byte >                FArray4A_byte;
typedef  FArray4A< sbyte >               FArray4A_sbyte;
typedef  FArray4A< ubyte >               FArray4A_ubyte;
typedef  FArray4A< short int >           FArray4A_short;
typedef  FArray4A< int >                 FArray4A_int;
typedef  FArray4A< long int >            FArray4A_long;
typedef  FArray4A< unsigned short int >  FArray4A_ushort;
typedef  FArray4A< unsigned int >        FArray4A_uint;
typedef  FArray4A< unsigned long int >   FArray4A_ulong;
typedef  FArray4A< std::size_t >         FArray4A_size;
typedef  FArray4A< std::int8_t >         FArray4A_int8;
typedef  FArray4A< std::int16_t >        FArray4A_int16;
typedef  FArray4A< std::int32_t >        FArray4A_int32;
typedef  FArray4A< std::int64_t >        FArray4A_int64;
typedef  FArray4A< std::uint8_t >        FArray4A_uint8;
typedef  FArray4A< std::uint16_t >       FArray4A_uint16;
typedef  FArray4A< std::uint32_t >       FArray4A_uint32;
typedef  FArray4A< std::uint64_t >       FArray4A_uint64;
typedef  FArray4A< float >               FArray4A_float;
typedef  FArray4A< double >              FArray4A_double;
typedef  FArray4A< long double >         FArray4A_longdouble;
typedef  FArray4A< char >                FArray4A_char;
typedef  FArray4A< unsigned char >       FArray4A_uchar;
typedef  FArray4A< signed char >         FArray4A_schar;
typedef  FArray4A< std::string >         FArray4A_string;
typedef  FArray4A< Fstring >             FArray4A_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray4A_fwd_hh_INCLUDED
