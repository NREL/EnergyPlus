#ifndef ObjexxFCL_CArray_fwd_hh_INCLUDED
#define ObjexxFCL_CArray_fwd_hh_INCLUDED

// CArray Forward Declarations
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
template< typename > class CArray;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  CArray< bool >                CArray_bool;
typedef  CArray< byte >                CArray_byte;
typedef  CArray< sbyte >               CArray_sbyte;
typedef  CArray< ubyte >               CArray_ubyte;
typedef  CArray< short int >           CArray_short;
typedef  CArray< int >                 CArray_int;
typedef  CArray< long int >            CArray_long;
typedef  CArray< unsigned short int >  CArray_ushort;
typedef  CArray< unsigned int >        CArray_uint;
typedef  CArray< unsigned long int >   CArray_ulong;
typedef  CArray< std::size_t >         CArray_size;
typedef  CArray< std::int8_t >         CArray_int8;
typedef  CArray< std::int16_t >        CArray_int16;
typedef  CArray< std::int32_t >        CArray_int32;
typedef  CArray< std::int64_t >        CArray_int64;
typedef  CArray< std::uint8_t >        CArray_uint8;
typedef  CArray< std::uint16_t >       CArray_uint16;
typedef  CArray< std::uint32_t >       CArray_uint32;
typedef  CArray< std::uint64_t >       CArray_uint64;
typedef  CArray< float >               CArray_float;
typedef  CArray< double >              CArray_double;
typedef  CArray< long double >         CArray_longdouble;
typedef  CArray< char >                CArray_char;
typedef  CArray< unsigned char >       CArray_uchar;
typedef  CArray< signed char >         CArray_schar;
typedef  CArray< std::string >         CArray_string;

} // ObjexxFCL

#endif // ObjexxFCL_CArray_fwd_hh_INCLUDED
