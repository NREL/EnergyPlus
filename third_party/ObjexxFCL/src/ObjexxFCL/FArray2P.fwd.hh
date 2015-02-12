#ifndef ObjexxFCL_FArray2P_fwd_hh_INCLUDED
#define ObjexxFCL_FArray2P_fwd_hh_INCLUDED

// FArray2P Forward Declarations
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
template< typename > class FArray2P;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray2P< bool >                FArray2P_bool;
typedef  FArray2P< byte >                FArray2P_byte;
typedef  FArray2P< sbyte >               FArray2P_sbyte;
typedef  FArray2P< ubyte >               FArray2P_ubyte;
typedef  FArray2P< short int >           FArray2P_short;
typedef  FArray2P< int >                 FArray2P_int;
typedef  FArray2P< long int >            FArray2P_long;
typedef  FArray2P< unsigned short int >  FArray2P_ushort;
typedef  FArray2P< unsigned int >        FArray2P_uint;
typedef  FArray2P< unsigned long int >   FArray2P_ulong;
typedef  FArray2P< std::size_t >         FArray2P_size;
typedef  FArray2P< std::int8_t >         FArray2P_int8;
typedef  FArray2P< std::int16_t >        FArray2P_int16;
typedef  FArray2P< std::int32_t >        FArray2P_int32;
typedef  FArray2P< std::int64_t >        FArray2P_int64;
typedef  FArray2P< std::uint8_t >        FArray2P_uint8;
typedef  FArray2P< std::uint16_t >       FArray2P_uint16;
typedef  FArray2P< std::uint32_t >       FArray2P_uint32;
typedef  FArray2P< std::uint64_t >       FArray2P_uint64;
typedef  FArray2P< float >               FArray2P_float;
typedef  FArray2P< double >              FArray2P_double;
typedef  FArray2P< long double >         FArray2P_longdouble;
typedef  FArray2P< char >                FArray2P_char;
typedef  FArray2P< unsigned char >       FArray2P_uchar;
typedef  FArray2P< signed char >         FArray2P_schar;
typedef  FArray2P< std::string >         FArray2P_string;
typedef  FArray2P< Fstring >             FArray2P_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray2P_fwd_hh_INCLUDED
