#ifndef ObjexxFCL_CArray_fwd_hh_INCLUDED
#define ObjexxFCL_CArray_fwd_hh_INCLUDED

// CArray Forward Declarations
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

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
using sbyte = byte;
using CArray_bool = CArray< bool >;
using CArray_byte = CArray< byte >;
using CArray_sbyte = CArray< sbyte >;
using CArray_ubyte = CArray< ubyte >;
using CArray_short = CArray< short int >;
using CArray_int = CArray< int >;
using CArray_long = CArray< long int >;
using CArray_ushort = CArray< unsigned short int >;
using CArray_uint = CArray< unsigned int >;
using CArray_ulong = CArray< unsigned long int >;
using CArray_size = CArray< std::size_t >;
using CArray_int8 = CArray< std::int8_t >;
using CArray_int16 = CArray< std::int16_t >;
using CArray_int32 = CArray< std::int32_t >;
using CArray_int64 = CArray< std::int64_t >;
using CArray_uint8 = CArray< std::uint8_t >;
using CArray_uint16 = CArray< std::uint16_t >;
using CArray_uint32 = CArray< std::uint32_t >;
using CArray_uint64 = CArray< std::uint64_t >;
using CArray_float = CArray< float >;
using CArray_double = CArray< double >;
using CArray_longdouble = CArray< long double >;
using CArray_char = CArray< char >;
using CArray_uchar = CArray< unsigned char >;
using CArray_schar = CArray< signed char >;
using CArray_string = CArray< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_CArray_fwd_hh_INCLUDED
