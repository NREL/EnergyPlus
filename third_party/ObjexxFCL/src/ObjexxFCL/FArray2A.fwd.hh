#ifndef ObjexxFCL_FArray2A_fwd_hh_INCLUDED
#define ObjexxFCL_FArray2A_fwd_hh_INCLUDED

// FArray2A Forward Declarations
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
template< typename > class FArray2A;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray2A< bool >                FArray2A_bool;
typedef  FArray2A< byte >                FArray2A_byte;
typedef  FArray2A< sbyte >               FArray2A_sbyte;
typedef  FArray2A< ubyte >               FArray2A_ubyte;
typedef  FArray2A< short int >           FArray2A_short;
typedef  FArray2A< int >                 FArray2A_int;
typedef  FArray2A< long int >            FArray2A_long;
typedef  FArray2A< unsigned short int >  FArray2A_ushort;
typedef  FArray2A< unsigned int >        FArray2A_uint;
typedef  FArray2A< unsigned long int >   FArray2A_ulong;
typedef  FArray2A< std::size_t >         FArray2A_size;
typedef  FArray2A< std::int8_t >         FArray2A_int8;
typedef  FArray2A< std::int16_t >        FArray2A_int16;
typedef  FArray2A< std::int32_t >        FArray2A_int32;
typedef  FArray2A< std::int64_t >        FArray2A_int64;
typedef  FArray2A< std::uint8_t >        FArray2A_uint8;
typedef  FArray2A< std::uint16_t >       FArray2A_uint16;
typedef  FArray2A< std::uint32_t >       FArray2A_uint32;
typedef  FArray2A< std::uint64_t >       FArray2A_uint64;
typedef  FArray2A< float >               FArray2A_float;
typedef  FArray2A< double >              FArray2A_double;
typedef  FArray2A< long double >         FArray2A_longdouble;
typedef  FArray2A< char >                FArray2A_char;
typedef  FArray2A< unsigned char >       FArray2A_uchar;
typedef  FArray2A< signed char >         FArray2A_schar;
typedef  FArray2A< std::string >         FArray2A_string;
typedef  FArray2A< Fstring >             FArray2A_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray2A_fwd_hh_INCLUDED
