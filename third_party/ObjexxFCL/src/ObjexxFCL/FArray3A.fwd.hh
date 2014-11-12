#ifndef ObjexxFCL_FArray3A_fwd_hh_INCLUDED
#define ObjexxFCL_FArray3A_fwd_hh_INCLUDED

// FArray3A Forward Declarations
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
template< typename > class FArray3A;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray3A< bool >                FArray3A_bool;
typedef  FArray3A< byte >                FArray3A_byte;
typedef  FArray3A< sbyte >               FArray3A_sbyte;
typedef  FArray3A< ubyte >               FArray3A_ubyte;
typedef  FArray3A< short int >           FArray3A_short;
typedef  FArray3A< int >                 FArray3A_int;
typedef  FArray3A< long int >            FArray3A_long;
typedef  FArray3A< unsigned short int >  FArray3A_ushort;
typedef  FArray3A< unsigned int >        FArray3A_uint;
typedef  FArray3A< unsigned long int >   FArray3A_ulong;
typedef  FArray3A< std::size_t >         FArray3A_size;
typedef  FArray3A< std::int8_t >         FArray3A_int8;
typedef  FArray3A< std::int16_t >        FArray3A_int16;
typedef  FArray3A< std::int32_t >        FArray3A_int32;
typedef  FArray3A< std::int64_t >        FArray3A_int64;
typedef  FArray3A< std::uint8_t >        FArray3A_uint8;
typedef  FArray3A< std::uint16_t >       FArray3A_uint16;
typedef  FArray3A< std::uint32_t >       FArray3A_uint32;
typedef  FArray3A< std::uint64_t >       FArray3A_uint64;
typedef  FArray3A< float >               FArray3A_float;
typedef  FArray3A< double >              FArray3A_double;
typedef  FArray3A< long double >         FArray3A_longdouble;
typedef  FArray3A< char >                FArray3A_char;
typedef  FArray3A< unsigned char >       FArray3A_uchar;
typedef  FArray3A< signed char >         FArray3A_schar;
typedef  FArray3A< std::string >         FArray3A_string;
typedef  FArray3A< Fstring >             FArray3A_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray3A_fwd_hh_INCLUDED
