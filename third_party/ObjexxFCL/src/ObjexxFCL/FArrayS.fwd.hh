#ifndef ObjexxFCL_FArrayS_fwd_hh_INCLUDED
#define ObjexxFCL_FArrayS_fwd_hh_INCLUDED

// FArrayS Forward Declarations
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
template< typename > class FArrayS;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
typedef  FArrayS< bool >                FArrayS_bool;
typedef  FArrayS< byte >                FArrayS_byte;
typedef  FArrayS< sbyte >               FArrayS_sbyte;
typedef  FArrayS< ubyte >               FArrayS_ubyte;
typedef  FArrayS< short int >           FArrayS_short;
typedef  FArrayS< int >                 FArrayS_int;
typedef  FArrayS< long int >            FArrayS_long;
typedef  FArrayS< unsigned short int >  FArrayS_ushort;
typedef  FArrayS< unsigned int >        FArrayS_uint;
typedef  FArrayS< unsigned long int >   FArrayS_ulong;
typedef  FArrayS< std::size_t >         FArrayS_size;
typedef  FArrayS< std::int8_t >         FArrayS_int8;
typedef  FArrayS< std::int16_t >        FArrayS_int16;
typedef  FArrayS< std::int32_t >        FArrayS_int32;
typedef  FArrayS< std::int64_t >        FArrayS_int64;
typedef  FArrayS< std::uint8_t >        FArrayS_uint8;
typedef  FArrayS< std::uint16_t >       FArrayS_uint16;
typedef  FArrayS< std::uint32_t >       FArrayS_uint32;
typedef  FArrayS< std::uint64_t >       FArrayS_uint64;
typedef  FArrayS< float >               FArrayS_float;
typedef  FArrayS< double >              FArrayS_double;
typedef  FArrayS< long double >         FArrayS_longdouble;
typedef  FArrayS< char >                FArrayS_char;
typedef  FArrayS< unsigned char >       FArrayS_uchar;
typedef  FArrayS< signed char >         FArrayS_schar;
typedef  FArrayS< std::string >         FArrayS_string;
typedef  FArrayS< Fstring >             FArrayS_Fstring;

} // ObjexxFCL

#endif // ObjexxFCL_FArrayS_fwd_hh_INCLUDED
