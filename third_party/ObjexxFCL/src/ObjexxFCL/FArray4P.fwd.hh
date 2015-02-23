#ifndef ObjexxFCL_FArray4P_fwd_hh_INCLUDED
#define ObjexxFCL_FArray4P_fwd_hh_INCLUDED

// FArray4P Forward Declarations
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
template< typename > class FArray4P;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray4P< bool >                FArray4P_bool;
typedef  FArray4P< byte >                FArray4P_byte;
typedef  FArray4P< sbyte >               FArray4P_sbyte;
typedef  FArray4P< ubyte >               FArray4P_ubyte;
typedef  FArray4P< short int >           FArray4P_short;
typedef  FArray4P< int >                 FArray4P_int;
typedef  FArray4P< long int >            FArray4P_long;
typedef  FArray4P< unsigned short int >  FArray4P_ushort;
typedef  FArray4P< unsigned int >        FArray4P_uint;
typedef  FArray4P< unsigned long int >   FArray4P_ulong;
typedef  FArray4P< std::size_t >         FArray4P_size;
typedef  FArray4P< std::int8_t >         FArray4P_int8;
typedef  FArray4P< std::int16_t >        FArray4P_int16;
typedef  FArray4P< std::int32_t >        FArray4P_int32;
typedef  FArray4P< std::int64_t >        FArray4P_int64;
typedef  FArray4P< std::uint8_t >        FArray4P_uint8;
typedef  FArray4P< std::uint16_t >       FArray4P_uint16;
typedef  FArray4P< std::uint32_t >       FArray4P_uint32;
typedef  FArray4P< std::uint64_t >       FArray4P_uint64;
typedef  FArray4P< float >               FArray4P_float;
typedef  FArray4P< double >              FArray4P_double;
typedef  FArray4P< long double >         FArray4P_longdouble;
typedef  FArray4P< char >                FArray4P_char;
typedef  FArray4P< unsigned char >       FArray4P_uchar;
typedef  FArray4P< signed char >         FArray4P_schar;
typedef  FArray4P< std::string >         FArray4P_string;
typedef  FArray4P< Fstring >             FArray4P_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray4P_fwd_hh_INCLUDED
