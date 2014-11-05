#ifndef ObjexxFCL_FArray3P_fwd_hh_INCLUDED
#define ObjexxFCL_FArray3P_fwd_hh_INCLUDED

// FArray3P Forward Declarations
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
template< typename > class FArray3P;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray3P< bool >                FArray3P_bool;
typedef  FArray3P< byte >                FArray3P_byte;
typedef  FArray3P< sbyte >               FArray3P_sbyte;
typedef  FArray3P< ubyte >               FArray3P_ubyte;
typedef  FArray3P< short int >           FArray3P_short;
typedef  FArray3P< int >                 FArray3P_int;
typedef  FArray3P< long int >            FArray3P_long;
typedef  FArray3P< unsigned short int >  FArray3P_ushort;
typedef  FArray3P< unsigned int >        FArray3P_uint;
typedef  FArray3P< unsigned long int >   FArray3P_ulong;
typedef  FArray3P< std::size_t >         FArray3P_size;
typedef  FArray3P< std::int8_t >         FArray3P_int8;
typedef  FArray3P< std::int16_t >        FArray3P_int16;
typedef  FArray3P< std::int32_t >        FArray3P_int32;
typedef  FArray3P< std::int64_t >        FArray3P_int64;
typedef  FArray3P< std::uint8_t >        FArray3P_uint8;
typedef  FArray3P< std::uint16_t >       FArray3P_uint16;
typedef  FArray3P< std::uint32_t >       FArray3P_uint32;
typedef  FArray3P< std::uint64_t >       FArray3P_uint64;
typedef  FArray3P< float >               FArray3P_float;
typedef  FArray3P< double >              FArray3P_double;
typedef  FArray3P< long double >         FArray3P_longdouble;
typedef  FArray3P< char >                FArray3P_char;
typedef  FArray3P< unsigned char >       FArray3P_uchar;
typedef  FArray3P< signed char >         FArray3P_schar;
typedef  FArray3P< std::string >         FArray3P_string;
typedef  FArray3P< Fstring >             FArray3P_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray3P_fwd_hh_INCLUDED
