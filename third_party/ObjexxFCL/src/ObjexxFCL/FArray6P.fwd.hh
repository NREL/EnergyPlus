#ifndef ObjexxFCL_FArray6P_fwd_hh_INCLUDED
#define ObjexxFCL_FArray6P_fwd_hh_INCLUDED

// FArray6P Forward Declarations
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
template< typename > class FArray6P;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray6P< bool >                FArray6P_bool;
typedef  FArray6P< byte >                FArray6P_byte;
typedef  FArray6P< sbyte >               FArray6P_sbyte;
typedef  FArray6P< ubyte >               FArray6P_ubyte;
typedef  FArray6P< short int >           FArray6P_short;
typedef  FArray6P< int >                 FArray6P_int;
typedef  FArray6P< long int >            FArray6P_long;
typedef  FArray6P< unsigned short int >  FArray6P_ushort;
typedef  FArray6P< unsigned int >        FArray6P_uint;
typedef  FArray6P< unsigned long int >   FArray6P_ulong;
typedef  FArray6P< std::size_t >         FArray6P_size;
typedef  FArray6P< std::int8_t >         FArray6P_int8;
typedef  FArray6P< std::int16_t >        FArray6P_int16;
typedef  FArray6P< std::int32_t >        FArray6P_int32;
typedef  FArray6P< std::int64_t >        FArray6P_int64;
typedef  FArray6P< std::uint8_t >        FArray6P_uint8;
typedef  FArray6P< std::uint16_t >       FArray6P_uint16;
typedef  FArray6P< std::uint32_t >       FArray6P_uint32;
typedef  FArray6P< std::uint64_t >       FArray6P_uint64;
typedef  FArray6P< float >               FArray6P_float;
typedef  FArray6P< double >              FArray6P_double;
typedef  FArray6P< long double >         FArray6P_longdouble;
typedef  FArray6P< char >                FArray6P_char;
typedef  FArray6P< unsigned char >       FArray6P_uchar;
typedef  FArray6P< signed char >         FArray6P_schar;
typedef  FArray6P< std::string >         FArray6P_string;
typedef  FArray6P< Fstring >             FArray6P_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray6P_fwd_hh_INCLUDED
