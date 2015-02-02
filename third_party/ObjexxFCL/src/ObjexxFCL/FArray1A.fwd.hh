#ifndef ObjexxFCL_FArray1A_fwd_hh_INCLUDED
#define ObjexxFCL_FArray1A_fwd_hh_INCLUDED

// FArray1A Forward Declarations
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
template< typename > class FArray1A;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray1A< bool >                FArray1A_bool;
typedef  FArray1A< byte >                FArray1A_byte;
typedef  FArray1A< sbyte >               FArray1A_sbyte;
typedef  FArray1A< ubyte >               FArray1A_ubyte;
typedef  FArray1A< short int >           FArray1A_short;
typedef  FArray1A< int >                 FArray1A_int;
typedef  FArray1A< long int >            FArray1A_long;
typedef  FArray1A< unsigned short int >  FArray1A_ushort;
typedef  FArray1A< unsigned int >        FArray1A_uint;
typedef  FArray1A< unsigned long int >   FArray1A_ulong;
typedef  FArray1A< std::size_t >         FArray1A_size;
typedef  FArray1A< std::int8_t >         FArray1A_int8;
typedef  FArray1A< std::int16_t >        FArray1A_int16;
typedef  FArray1A< std::int32_t >        FArray1A_int32;
typedef  FArray1A< std::int64_t >        FArray1A_int64;
typedef  FArray1A< std::uint8_t >        FArray1A_uint8;
typedef  FArray1A< std::uint16_t >       FArray1A_uint16;
typedef  FArray1A< std::uint32_t >       FArray1A_uint32;
typedef  FArray1A< std::uint64_t >       FArray1A_uint64;
typedef  FArray1A< float >               FArray1A_float;
typedef  FArray1A< double >              FArray1A_double;
typedef  FArray1A< long double >         FArray1A_longdouble;
typedef  FArray1A< char >                FArray1A_char;
typedef  FArray1A< unsigned char >       FArray1A_uchar;
typedef  FArray1A< signed char >         FArray1A_schar;
typedef  FArray1A< std::string >         FArray1A_string;
typedef  FArray1A< Fstring >             FArray1A_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray1A_fwd_hh_INCLUDED
