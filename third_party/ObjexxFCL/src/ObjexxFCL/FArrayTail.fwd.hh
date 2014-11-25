#ifndef ObjexxFCL_FArrayTail_fwd_hh_INCLUDED
#define ObjexxFCL_FArrayTail_fwd_hh_INCLUDED

// FArrayTail Forward Declarations
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
template< typename > class FArrayTail;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArrayTail< bool >                FArrayTail_bool;
typedef  FArrayTail< byte >                FArrayTail_byte;
typedef  FArrayTail< sbyte >               FArrayTail_sbyte;
typedef  FArrayTail< ubyte >               FArrayTail_ubyte;
typedef  FArrayTail< short int >           FArrayTail_short;
typedef  FArrayTail< int >                 FArrayTail_int;
typedef  FArrayTail< long int >            FArrayTail_long;
typedef  FArrayTail< unsigned short int >  FArrayTail_ushort;
typedef  FArrayTail< unsigned int >        FArrayTail_uint;
typedef  FArrayTail< unsigned long int >   FArrayTail_ulong;
typedef  FArrayTail< std::size_t >         FArrayTail_size;
typedef  FArrayTail< std::int8_t >         FArrayTail_int8;
typedef  FArrayTail< std::int16_t >        FArrayTail_int16;
typedef  FArrayTail< std::int32_t >        FArrayTail_int32;
typedef  FArrayTail< std::int64_t >        FArrayTail_int64;
typedef  FArrayTail< std::uint8_t >        FArrayTail_uint8;
typedef  FArrayTail< std::uint16_t >       FArrayTail_uint16;
typedef  FArrayTail< std::uint32_t >       FArrayTail_uint32;
typedef  FArrayTail< std::uint64_t >       FArrayTail_uint64;
typedef  FArrayTail< float >               FArrayTail_float;
typedef  FArrayTail< double >              FArrayTail_double;
typedef  FArrayTail< long double >         FArrayTail_longdouble;
typedef  FArrayTail< char >                FArrayTail_char;
typedef  FArrayTail< unsigned char >       FArrayTail_uchar;
typedef  FArrayTail< signed char >         FArrayTail_schar;
typedef  FArrayTail< std::string >         FArrayTail_string;
typedef  FArrayTail< Fstring >             FArrayTail_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArrayTail_fwd_hh_INCLUDED
