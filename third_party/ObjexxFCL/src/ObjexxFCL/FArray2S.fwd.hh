#ifndef ObjexxFCL_FArray2S_fwd_hh_INCLUDED
#define ObjexxFCL_FArray2S_fwd_hh_INCLUDED

// FArray2S Forward Declarations
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
template< typename > class FArray2S;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray2S< bool >                FArray2S_bool;
typedef  FArray2S< byte >                FArray2S_byte;
typedef  FArray2S< sbyte >               FArray2S_sbyte;
typedef  FArray2S< ubyte >               FArray2S_ubyte;
typedef  FArray2S< short int >           FArray2S_short;
typedef  FArray2S< int >                 FArray2S_int;
typedef  FArray2S< long int >            FArray2S_long;
typedef  FArray2S< unsigned short int >  FArray2S_ushort;
typedef  FArray2S< unsigned int >        FArray2S_uint;
typedef  FArray2S< unsigned long int >   FArray2S_ulong;
typedef  FArray2S< std::size_t >         FArray2S_size;
typedef  FArray2S< std::int8_t >         FArray2S_int8;
typedef  FArray2S< std::int16_t >        FArray2S_int16;
typedef  FArray2S< std::int32_t >        FArray2S_int32;
typedef  FArray2S< std::int64_t >        FArray2S_int64;
typedef  FArray2S< std::uint8_t >        FArray2S_uint8;
typedef  FArray2S< std::uint16_t >       FArray2S_uint16;
typedef  FArray2S< std::uint32_t >       FArray2S_uint32;
typedef  FArray2S< std::uint64_t >       FArray2S_uint64;
typedef  FArray2S< float >               FArray2S_float;
typedef  FArray2S< double >              FArray2S_double;
typedef  FArray2S< long double >         FArray2S_longdouble;
typedef  FArray2S< char >                FArray2S_char;
typedef  FArray2S< unsigned char >       FArray2S_uchar;
typedef  FArray2S< signed char >         FArray2S_schar;
typedef  FArray2S< std::string >         FArray2S_string;
typedef  FArray2S< Fstring >             FArray2S_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray2S_fwd_hh_INCLUDED
