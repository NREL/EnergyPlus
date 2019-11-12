#ifndef ObjexxFCL_ArrayInitializer_fwd_hh_INCLUDED
#define ObjexxFCL_ArrayInitializer_fwd_hh_INCLUDED

// ArrayInitializer Forward Declarations
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
template< typename > class ArrayInitializer;
class byte;
class ubyte;

// Types
using sbyte = byte;
using ArrayInitializer_bool = ArrayInitializer< bool >;
using ArrayInitializer_byte = ArrayInitializer< byte >;
using ArrayInitializer_sbyte = ArrayInitializer< sbyte >;
using ArrayInitializer_ubyte = ArrayInitializer< ubyte >;
using ArrayInitializer_short = ArrayInitializer< short int >;
using ArrayInitializer_int = ArrayInitializer< int >;
using ArrayInitializer_long = ArrayInitializer< long int >;
using ArrayInitializer_ushort = ArrayInitializer< unsigned short int >;
using ArrayInitializer_uint = ArrayInitializer< unsigned int >;
using ArrayInitializer_ulong = ArrayInitializer< unsigned long int >;
using ArrayInitializer_size = ArrayInitializer< std::size_t >;
using ArrayInitializer_int8 = ArrayInitializer< std::int8_t >;
using ArrayInitializer_int16 = ArrayInitializer< std::int16_t >;
using ArrayInitializer_int32 = ArrayInitializer< std::int32_t >;
using ArrayInitializer_int64 = ArrayInitializer< std::int64_t >;
using ArrayInitializer_uint8 = ArrayInitializer< std::uint8_t >;
using ArrayInitializer_uint16 = ArrayInitializer< std::uint16_t >;
using ArrayInitializer_uint32 = ArrayInitializer< std::uint32_t >;
using ArrayInitializer_uint64 = ArrayInitializer< std::uint64_t >;
using ArrayInitializer_float = ArrayInitializer< float >;
using ArrayInitializer_double = ArrayInitializer< double >;
using ArrayInitializer_longdouble = ArrayInitializer< long double >;
using ArrayInitializer_char = ArrayInitializer< char >;
using ArrayInitializer_uchar = ArrayInitializer< unsigned char >;
using ArrayInitializer_schar = ArrayInitializer< signed char >;
using ArrayInitializer_string = ArrayInitializer< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_ArrayInitializer_fwd_hh_INCLUDED
