#ifndef ObjexxFCL_CArrayA_fwd_hh_INCLUDED
#define ObjexxFCL_CArrayA_fwd_hh_INCLUDED

// CArrayA Forward Declarations
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// C++ Headers
#include <cstddef>
#include <cstdint>
#include <string>

namespace ObjexxFCL {

// Forward
template< typename > class CArrayA;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  CArrayA< bool >                CArrayA_bool;
typedef  CArrayA< byte >                CArrayA_byte;
typedef  CArrayA< sbyte >               CArrayA_sbyte;
typedef  CArrayA< ubyte >               CArrayA_ubyte;
typedef  CArrayA< short int >           CArrayA_short;
typedef  CArrayA< int >                 CArrayA_int;
typedef  CArrayA< long int >            CArrayA_long;
typedef  CArrayA< unsigned short int >  CArrayA_ushort;
typedef  CArrayA< unsigned int >        CArrayA_uint;
typedef  CArrayA< unsigned long int >   CArrayA_ulong;
typedef  CArrayA< std::size_t >         CArrayA_size;
typedef  CArrayA< std::int8_t >         CArrayA_int8;
typedef  CArrayA< std::int16_t >        CArrayA_int16;
typedef  CArrayA< std::int32_t >        CArrayA_int32;
typedef  CArrayA< std::int64_t >        CArrayA_int64;
typedef  CArrayA< std::uint8_t >        CArrayA_uint8;
typedef  CArrayA< std::uint16_t >       CArrayA_uint16;
typedef  CArrayA< std::uint32_t >       CArrayA_uint32;
typedef  CArrayA< std::uint64_t >       CArrayA_uint64;
typedef  CArrayA< float >               CArrayA_float;
typedef  CArrayA< double >              CArrayA_double;
typedef  CArrayA< long double >         CArrayA_longdouble;
typedef  CArrayA< char >                CArrayA_char;
typedef  CArrayA< unsigned char >       CArrayA_uchar;
typedef  CArrayA< signed char >         CArrayA_schar;
typedef  CArrayA< std::string >         CArrayA_string;

} // ObjexxFCL

#endif // ObjexxFCL_CArrayA_fwd_hh_INCLUDED
