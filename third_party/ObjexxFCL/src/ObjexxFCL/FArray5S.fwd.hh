#ifndef ObjexxFCL_FArray5S_fwd_hh_INCLUDED
#define ObjexxFCL_FArray5S_fwd_hh_INCLUDED

// FArray5S Forward Declarations
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
template< typename > class FArray5S;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray5S< bool >                FArray5S_bool;
typedef  FArray5S< byte >                FArray5S_byte;
typedef  FArray5S< sbyte >               FArray5S_sbyte;
typedef  FArray5S< ubyte >               FArray5S_ubyte;
typedef  FArray5S< short int >           FArray5S_short;
typedef  FArray5S< int >                 FArray5S_int;
typedef  FArray5S< long int >            FArray5S_long;
typedef  FArray5S< unsigned short int >  FArray5S_ushort;
typedef  FArray5S< unsigned int >        FArray5S_uint;
typedef  FArray5S< unsigned long int >   FArray5S_ulong;
typedef  FArray5S< std::size_t >         FArray5S_size;
typedef  FArray5S< std::int8_t >         FArray5S_int8;
typedef  FArray5S< std::int16_t >        FArray5S_int16;
typedef  FArray5S< std::int32_t >        FArray5S_int32;
typedef  FArray5S< std::int64_t >        FArray5S_int64;
typedef  FArray5S< std::uint8_t >        FArray5S_uint8;
typedef  FArray5S< std::uint16_t >       FArray5S_uint16;
typedef  FArray5S< std::uint32_t >       FArray5S_uint32;
typedef  FArray5S< std::uint64_t >       FArray5S_uint64;
typedef  FArray5S< float >               FArray5S_float;
typedef  FArray5S< double >              FArray5S_double;
typedef  FArray5S< long double >         FArray5S_longdouble;
typedef  FArray5S< char >                FArray5S_char;
typedef  FArray5S< unsigned char >       FArray5S_uchar;
typedef  FArray5S< signed char >         FArray5S_schar;
typedef  FArray5S< std::string >         FArray5S_string;
typedef  FArray5S< Fstring >             FArray5S_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray5S_fwd_hh_INCLUDED
