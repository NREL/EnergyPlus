#ifndef ObjexxFCL_FArray3S_fwd_hh_INCLUDED
#define ObjexxFCL_FArray3S_fwd_hh_INCLUDED

// FArray3S Forward Declarations
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
template< typename > class FArray3S;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray3S< bool >                FArray3S_bool;
typedef  FArray3S< byte >                FArray3S_byte;
typedef  FArray3S< sbyte >               FArray3S_sbyte;
typedef  FArray3S< ubyte >               FArray3S_ubyte;
typedef  FArray3S< short int >           FArray3S_short;
typedef  FArray3S< int >                 FArray3S_int;
typedef  FArray3S< long int >            FArray3S_long;
typedef  FArray3S< unsigned short int >  FArray3S_ushort;
typedef  FArray3S< unsigned int >        FArray3S_uint;
typedef  FArray3S< unsigned long int >   FArray3S_ulong;
typedef  FArray3S< std::size_t >         FArray3S_size;
typedef  FArray3S< std::int8_t >         FArray3S_int8;
typedef  FArray3S< std::int16_t >        FArray3S_int16;
typedef  FArray3S< std::int32_t >        FArray3S_int32;
typedef  FArray3S< std::int64_t >        FArray3S_int64;
typedef  FArray3S< std::uint8_t >        FArray3S_uint8;
typedef  FArray3S< std::uint16_t >       FArray3S_uint16;
typedef  FArray3S< std::uint32_t >       FArray3S_uint32;
typedef  FArray3S< std::uint64_t >       FArray3S_uint64;
typedef  FArray3S< float >               FArray3S_float;
typedef  FArray3S< double >              FArray3S_double;
typedef  FArray3S< long double >         FArray3S_longdouble;
typedef  FArray3S< char >                FArray3S_char;
typedef  FArray3S< unsigned char >       FArray3S_uchar;
typedef  FArray3S< signed char >         FArray3S_schar;
typedef  FArray3S< std::string >         FArray3S_string;
typedef  FArray3S< Fstring >             FArray3S_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray3S_fwd_hh_INCLUDED
