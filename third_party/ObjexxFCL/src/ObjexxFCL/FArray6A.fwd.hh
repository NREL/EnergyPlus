#ifndef ObjexxFCL_FArray6A_fwd_hh_INCLUDED
#define ObjexxFCL_FArray6A_fwd_hh_INCLUDED

// FArray6A Forward Declarations
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
template< typename > class FArray6A;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray6A< bool >                FArray6A_bool;
typedef  FArray6A< byte >                FArray6A_byte;
typedef  FArray6A< sbyte >               FArray6A_sbyte;
typedef  FArray6A< ubyte >               FArray6A_ubyte;
typedef  FArray6A< short int >           FArray6A_short;
typedef  FArray6A< int >                 FArray6A_int;
typedef  FArray6A< long int >            FArray6A_long;
typedef  FArray6A< unsigned short int >  FArray6A_ushort;
typedef  FArray6A< unsigned int >        FArray6A_uint;
typedef  FArray6A< unsigned long int >   FArray6A_ulong;
typedef  FArray6A< std::size_t >         FArray6A_size;
typedef  FArray6A< std::int8_t >         FArray6A_int8;
typedef  FArray6A< std::int16_t >        FArray6A_int16;
typedef  FArray6A< std::int32_t >        FArray6A_int32;
typedef  FArray6A< std::int64_t >        FArray6A_int64;
typedef  FArray6A< std::uint8_t >        FArray6A_uint8;
typedef  FArray6A< std::uint16_t >       FArray6A_uint16;
typedef  FArray6A< std::uint32_t >       FArray6A_uint32;
typedef  FArray6A< std::uint64_t >       FArray6A_uint64;
typedef  FArray6A< float >               FArray6A_float;
typedef  FArray6A< double >              FArray6A_double;
typedef  FArray6A< long double >         FArray6A_longdouble;
typedef  FArray6A< char >                FArray6A_char;
typedef  FArray6A< unsigned char >       FArray6A_uchar;
typedef  FArray6A< signed char >         FArray6A_schar;
typedef  FArray6A< std::string >         FArray6A_string;
typedef  FArray6A< Fstring >             FArray6A_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray6A_fwd_hh_INCLUDED
