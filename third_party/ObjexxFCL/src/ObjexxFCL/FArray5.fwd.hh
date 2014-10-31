#ifndef ObjexxFCL_FArray5_fwd_hh_INCLUDED
#define ObjexxFCL_FArray5_fwd_hh_INCLUDED

// FArray5 Forward Declarations
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
template< typename > class FArray5;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray5< bool >                FArray5_bool;
typedef  FArray5< byte >                FArray5_byte;
typedef  FArray5< sbyte >               FArray5_sbyte;
typedef  FArray5< ubyte >               FArray5_ubyte;
typedef  FArray5< short int >           FArray5_short;
typedef  FArray5< int >                 FArray5_int;
typedef  FArray5< long int >            FArray5_long;
typedef  FArray5< unsigned short int >  FArray5_ushort;
typedef  FArray5< unsigned int >        FArray5_uint;
typedef  FArray5< unsigned long int >   FArray5_ulong;
typedef  FArray5< std::size_t >         FArray5_size;
typedef  FArray5< std::int8_t >         FArray5_int8;
typedef  FArray5< std::int16_t >        FArray5_int16;
typedef  FArray5< std::int32_t >        FArray5_int32;
typedef  FArray5< std::int64_t >        FArray5_int64;
typedef  FArray5< std::uint8_t >        FArray5_uint8;
typedef  FArray5< std::uint16_t >       FArray5_uint16;
typedef  FArray5< std::uint32_t >       FArray5_uint32;
typedef  FArray5< std::uint64_t >       FArray5_uint64;
typedef  FArray5< float >               FArray5_float;
typedef  FArray5< double >              FArray5_double;
typedef  FArray5< long double >         FArray5_longdouble;
typedef  FArray5< char >                FArray5_char;
typedef  FArray5< unsigned char >       FArray5_uchar;
typedef  FArray5< signed char >         FArray5_schar;
typedef  FArray5< std::string >         FArray5_string;
typedef  FArray5< Fstring >             FArray5_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray5_fwd_hh_INCLUDED
