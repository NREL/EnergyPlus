#ifndef ObjexxFCL_FArray3_fwd_hh_INCLUDED
#define ObjexxFCL_FArray3_fwd_hh_INCLUDED

// FArray3 Forward Declarations
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
template< typename > class FArray3;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray3< bool >                FArray3_bool;
typedef  FArray3< byte >                FArray3_byte;
typedef  FArray3< sbyte >               FArray3_sbyte;
typedef  FArray3< ubyte >               FArray3_ubyte;
typedef  FArray3< short int >           FArray3_short;
typedef  FArray3< int >                 FArray3_int;
typedef  FArray3< long int >            FArray3_long;
typedef  FArray3< unsigned short int >  FArray3_ushort;
typedef  FArray3< unsigned int >        FArray3_uint;
typedef  FArray3< unsigned long int >   FArray3_ulong;
typedef  FArray3< std::size_t >         FArray3_size;
typedef  FArray3< std::int8_t >         FArray3_int8;
typedef  FArray3< std::int16_t >        FArray3_int16;
typedef  FArray3< std::int32_t >        FArray3_int32;
typedef  FArray3< std::int64_t >        FArray3_int64;
typedef  FArray3< std::uint8_t >        FArray3_uint8;
typedef  FArray3< std::uint16_t >       FArray3_uint16;
typedef  FArray3< std::uint32_t >       FArray3_uint32;
typedef  FArray3< std::uint64_t >       FArray3_uint64;
typedef  FArray3< float >               FArray3_float;
typedef  FArray3< double >              FArray3_double;
typedef  FArray3< long double >         FArray3_longdouble;
typedef  FArray3< char >                FArray3_char;
typedef  FArray3< unsigned char >       FArray3_uchar;
typedef  FArray3< signed char >         FArray3_schar;
typedef  FArray3< std::string >         FArray3_string;
typedef  FArray3< Fstring >             FArray3_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray3_fwd_hh_INCLUDED
