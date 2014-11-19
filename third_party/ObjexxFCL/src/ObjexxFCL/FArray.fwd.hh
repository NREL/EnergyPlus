#ifndef ObjexxFCL_FArray_fwd_hh_INCLUDED
#define ObjexxFCL_FArray_fwd_hh_INCLUDED

// FArray Forward Declarations
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
template< typename > class FArray;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArray< bool >                FArray_bool;
typedef  FArray< byte >                FArray_byte;
typedef  FArray< sbyte >               FArray_sbyte;
typedef  FArray< ubyte >               FArray_ubyte;
typedef  FArray< short int >           FArray_short;
typedef  FArray< int >                 FArray_int;
typedef  FArray< long int >            FArray_long;
typedef  FArray< unsigned short int >  FArray_ushort;
typedef  FArray< unsigned int >        FArray_uint;
typedef  FArray< unsigned long int >   FArray_ulong;
typedef  FArray< std::size_t >         FArray_size;
typedef  FArray< std::int8_t >         FArray_int8;
typedef  FArray< std::int16_t >        FArray_int16;
typedef  FArray< std::int32_t >        FArray_int32;
typedef  FArray< std::int64_t >        FArray_int64;
typedef  FArray< std::uint8_t >        FArray_uint8;
typedef  FArray< std::uint16_t >       FArray_uint16;
typedef  FArray< std::uint32_t >       FArray_uint32;
typedef  FArray< std::uint64_t >       FArray_uint64;
typedef  FArray< float >               FArray_float;
typedef  FArray< double >              FArray_double;
typedef  FArray< long double >         FArray_longdouble;
typedef  FArray< char >                FArray_char;
typedef  FArray< unsigned char >       FArray_uchar;
typedef  FArray< signed char >         FArray_schar;
typedef  FArray< std::string >         FArray_string;
typedef  FArray< Fstring >             FArray_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArray_fwd_hh_INCLUDED
