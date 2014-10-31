#ifndef ObjexxFCL_FArray6_fwd_hh_INCLUDED
#define ObjexxFCL_FArray6_fwd_hh_INCLUDED

// FArray6 Forward Declarations
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
template< typename > class FArray6;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray6< bool >                FArray6_bool;
typedef  FArray6< byte >                FArray6_byte;
typedef  FArray6< sbyte >               FArray6_sbyte;
typedef  FArray6< ubyte >               FArray6_ubyte;
typedef  FArray6< short int >           FArray6_short;
typedef  FArray6< int >                 FArray6_int;
typedef  FArray6< long int >            FArray6_long;
typedef  FArray6< unsigned short int >  FArray6_ushort;
typedef  FArray6< unsigned int >        FArray6_uint;
typedef  FArray6< unsigned long int >   FArray6_ulong;
typedef  FArray6< std::size_t >         FArray6_size;
typedef  FArray6< std::int8_t >         FArray6_int8;
typedef  FArray6< std::int16_t >        FArray6_int16;
typedef  FArray6< std::int32_t >        FArray6_int32;
typedef  FArray6< std::int64_t >        FArray6_int64;
typedef  FArray6< std::uint8_t >        FArray6_uint8;
typedef  FArray6< std::uint16_t >       FArray6_uint16;
typedef  FArray6< std::uint32_t >       FArray6_uint32;
typedef  FArray6< std::uint64_t >       FArray6_uint64;
typedef  FArray6< float >               FArray6_float;
typedef  FArray6< double >              FArray6_double;
typedef  FArray6< long double >         FArray6_longdouble;
typedef  FArray6< char >                FArray6_char;
typedef  FArray6< unsigned char >       FArray6_uchar;
typedef  FArray6< signed char >         FArray6_schar;
typedef  FArray6< std::string >         FArray6_string;
typedef  FArray6< Fstring >             FArray6_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray6_fwd_hh_INCLUDED
