#ifndef ObjexxFCL_FArray6S_fwd_hh_INCLUDED
#define ObjexxFCL_FArray6S_fwd_hh_INCLUDED

// FArray6S Forward Declarations
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
template< typename > class FArray6S;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray6S< bool >                FArray6S_bool;
typedef  FArray6S< byte >                FArray6S_byte;
typedef  FArray6S< sbyte >               FArray6S_sbyte;
typedef  FArray6S< ubyte >               FArray6S_ubyte;
typedef  FArray6S< short int >           FArray6S_short;
typedef  FArray6S< int >                 FArray6S_int;
typedef  FArray6S< long int >            FArray6S_long;
typedef  FArray6S< unsigned short int >  FArray6S_ushort;
typedef  FArray6S< unsigned int >        FArray6S_uint;
typedef  FArray6S< unsigned long int >   FArray6S_ulong;
typedef  FArray6S< std::size_t >         FArray6S_size;
typedef  FArray6S< std::int8_t >         FArray6S_int8;
typedef  FArray6S< std::int16_t >        FArray6S_int16;
typedef  FArray6S< std::int32_t >        FArray6S_int32;
typedef  FArray6S< std::int64_t >        FArray6S_int64;
typedef  FArray6S< std::uint8_t >        FArray6S_uint8;
typedef  FArray6S< std::uint16_t >       FArray6S_uint16;
typedef  FArray6S< std::uint32_t >       FArray6S_uint32;
typedef  FArray6S< std::uint64_t >       FArray6S_uint64;
typedef  FArray6S< float >               FArray6S_float;
typedef  FArray6S< double >              FArray6S_double;
typedef  FArray6S< long double >         FArray6S_longdouble;
typedef  FArray6S< char >                FArray6S_char;
typedef  FArray6S< unsigned char >       FArray6S_uchar;
typedef  FArray6S< signed char >         FArray6S_schar;
typedef  FArray6S< std::string >         FArray6S_string;
typedef  FArray6S< Fstring >             FArray6S_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray6S_fwd_hh_INCLUDED
