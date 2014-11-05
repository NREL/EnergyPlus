#ifndef ObjexxFCL_FArray4S_fwd_hh_INCLUDED
#define ObjexxFCL_FArray4S_fwd_hh_INCLUDED

// FArray4S Forward Declarations
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
template< typename > class FArray4S;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray4S< bool >                FArray4S_bool;
typedef  FArray4S< byte >                FArray4S_byte;
typedef  FArray4S< sbyte >               FArray4S_sbyte;
typedef  FArray4S< ubyte >               FArray4S_ubyte;
typedef  FArray4S< short int >           FArray4S_short;
typedef  FArray4S< int >                 FArray4S_int;
typedef  FArray4S< long int >            FArray4S_long;
typedef  FArray4S< unsigned short int >  FArray4S_ushort;
typedef  FArray4S< unsigned int >        FArray4S_uint;
typedef  FArray4S< unsigned long int >   FArray4S_ulong;
typedef  FArray4S< std::size_t >         FArray4S_size;
typedef  FArray4S< std::int8_t >         FArray4S_int8;
typedef  FArray4S< std::int16_t >        FArray4S_int16;
typedef  FArray4S< std::int32_t >        FArray4S_int32;
typedef  FArray4S< std::int64_t >        FArray4S_int64;
typedef  FArray4S< std::uint8_t >        FArray4S_uint8;
typedef  FArray4S< std::uint16_t >       FArray4S_uint16;
typedef  FArray4S< std::uint32_t >       FArray4S_uint32;
typedef  FArray4S< std::uint64_t >       FArray4S_uint64;
typedef  FArray4S< float >               FArray4S_float;
typedef  FArray4S< double >              FArray4S_double;
typedef  FArray4S< long double >         FArray4S_longdouble;
typedef  FArray4S< char >                FArray4S_char;
typedef  FArray4S< unsigned char >       FArray4S_uchar;
typedef  FArray4S< signed char >         FArray4S_schar;
typedef  FArray4S< std::string >         FArray4S_string;
typedef  FArray4S< Fstring >             FArray4S_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray4S_fwd_hh_INCLUDED
