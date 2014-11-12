#ifndef ObjexxFCL_FArray1P_fwd_hh_INCLUDED
#define ObjexxFCL_FArray1P_fwd_hh_INCLUDED

// FArray1P Forward Declarations
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
template< typename > class FArray1P;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray1P< bool >                FArray1P_bool;
typedef  FArray1P< byte >                FArray1P_byte;
typedef  FArray1P< sbyte >               FArray1P_sbyte;
typedef  FArray1P< ubyte >               FArray1P_ubyte;
typedef  FArray1P< short int >           FArray1P_short;
typedef  FArray1P< int >                 FArray1P_int;
typedef  FArray1P< long int >            FArray1P_long;
typedef  FArray1P< unsigned short int >  FArray1P_ushort;
typedef  FArray1P< unsigned int >        FArray1P_uint;
typedef  FArray1P< unsigned long int >   FArray1P_ulong;
typedef  FArray1P< std::size_t >         FArray1P_size;
typedef  FArray1P< std::int8_t >         FArray1P_int8;
typedef  FArray1P< std::int16_t >        FArray1P_int16;
typedef  FArray1P< std::int32_t >        FArray1P_int32;
typedef  FArray1P< std::int64_t >        FArray1P_int64;
typedef  FArray1P< std::uint8_t >        FArray1P_uint8;
typedef  FArray1P< std::uint16_t >       FArray1P_uint16;
typedef  FArray1P< std::uint32_t >       FArray1P_uint32;
typedef  FArray1P< std::uint64_t >       FArray1P_uint64;
typedef  FArray1P< float >               FArray1P_float;
typedef  FArray1P< double >              FArray1P_double;
typedef  FArray1P< long double >         FArray1P_longdouble;
typedef  FArray1P< char >                FArray1P_char;
typedef  FArray1P< unsigned char >       FArray1P_uchar;
typedef  FArray1P< signed char >         FArray1P_schar;
typedef  FArray1P< std::string >         FArray1P_string;
typedef  FArray1P< Fstring >             FArray1P_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray1P_fwd_hh_INCLUDED
