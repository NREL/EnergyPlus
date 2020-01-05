#ifndef ObjexxFCL_ArrayTail_fwd_hh_INCLUDED
#define ObjexxFCL_ArrayTail_fwd_hh_INCLUDED

// ArrayTail Forward Declarations
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2020 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// C++ Headers
#include <cstddef>
#include <cstdint>
#include <string>

namespace ObjexxFCL {

// Forward
template< typename > class ArrayTail;
class byte;
class ubyte;

// Types
using sbyte = byte;
using ArrayTail_bool = ArrayTail< bool >;
using ArrayTail_byte = ArrayTail< byte >;
using ArrayTail_sbyte = ArrayTail< sbyte >;
using ArrayTail_ubyte = ArrayTail< ubyte >;
using ArrayTail_short = ArrayTail< short int >;
using ArrayTail_int = ArrayTail< int >;
using ArrayTail_long = ArrayTail< long int >;
using ArrayTail_ushort = ArrayTail< unsigned short int >;
using ArrayTail_uint = ArrayTail< unsigned int >;
using ArrayTail_ulong = ArrayTail< unsigned long int >;
using ArrayTail_size = ArrayTail< std::size_t >;
using ArrayTail_int8 = ArrayTail< std::int8_t >;
using ArrayTail_int16 = ArrayTail< std::int16_t >;
using ArrayTail_int32 = ArrayTail< std::int32_t >;
using ArrayTail_int64 = ArrayTail< std::int64_t >;
using ArrayTail_uint8 = ArrayTail< std::uint8_t >;
using ArrayTail_uint16 = ArrayTail< std::uint16_t >;
using ArrayTail_uint32 = ArrayTail< std::uint32_t >;
using ArrayTail_uint64 = ArrayTail< std::uint64_t >;
using ArrayTail_float = ArrayTail< float >;
using ArrayTail_double = ArrayTail< double >;
using ArrayTail_longdouble = ArrayTail< long double >;
using ArrayTail_char = ArrayTail< char >;
using ArrayTail_uchar = ArrayTail< unsigned char >;
using ArrayTail_schar = ArrayTail< signed char >;
using ArrayTail_string = ArrayTail< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_ArrayTail_fwd_hh_INCLUDED
