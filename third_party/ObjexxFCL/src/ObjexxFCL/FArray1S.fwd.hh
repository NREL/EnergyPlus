#ifndef ObjexxFCL_FArray1S_fwd_hh_INCLUDED
#define ObjexxFCL_FArray1S_fwd_hh_INCLUDED

// FArray1S Forward Declarations
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
template< typename > class FArray1S;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray1S< bool >                FArray1S_bool;
typedef  FArray1S< byte >                FArray1S_byte;
typedef  FArray1S< sbyte >               FArray1S_sbyte;
typedef  FArray1S< ubyte >               FArray1S_ubyte;
typedef  FArray1S< short int >           FArray1S_short;
typedef  FArray1S< int >                 FArray1S_int;
typedef  FArray1S< long int >            FArray1S_long;
typedef  FArray1S< unsigned short int >  FArray1S_ushort;
typedef  FArray1S< unsigned int >        FArray1S_uint;
typedef  FArray1S< unsigned long int >   FArray1S_ulong;
typedef  FArray1S< std::size_t >         FArray1S_size;
typedef  FArray1S< std::int8_t >         FArray1S_int8;
typedef  FArray1S< std::int16_t >        FArray1S_int16;
typedef  FArray1S< std::int32_t >        FArray1S_int32;
typedef  FArray1S< std::int64_t >        FArray1S_int64;
typedef  FArray1S< std::uint8_t >        FArray1S_uint8;
typedef  FArray1S< std::uint16_t >       FArray1S_uint16;
typedef  FArray1S< std::uint32_t >       FArray1S_uint32;
typedef  FArray1S< std::uint64_t >       FArray1S_uint64;
typedef  FArray1S< float >               FArray1S_float;
typedef  FArray1S< double >              FArray1S_double;
typedef  FArray1S< long double >         FArray1S_longdouble;
typedef  FArray1S< char >                FArray1S_char;
typedef  FArray1S< unsigned char >       FArray1S_uchar;
typedef  FArray1S< signed char >         FArray1S_schar;
typedef  FArray1S< std::string >         FArray1S_string;
typedef  FArray1S< Fstring >             FArray1S_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray1S_fwd_hh_INCLUDED
