#ifndef ObjexxFCL_Vector4_fwd_hh_INCLUDED
#define ObjexxFCL_Vector4_fwd_hh_INCLUDED

// Vector3 Forward Declarations
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
template< typename > class Vector4;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Vector4_bool = Vector4< bool >;
using Vector4_byte = Vector4< byte >;
using Vector4_sbyte = Vector4< sbyte >;
using Vector4_ubyte = Vector4< ubyte >;
using Vector4_short = Vector4< short int >;
using Vector4_int = Vector4< int >;
using Vector4_long = Vector4< long int >;
using Vector4_ushort = Vector4< unsigned short int >;
using Vector4_uint = Vector4< unsigned int >;
using Vector4_ulong = Vector4< unsigned long int >;
using Vector4_size = Vector4< std::size_t >;
using Vector4_int8 = Vector4< std::int8_t >;
using Vector4_int16 = Vector4< std::int16_t >;
using Vector4_int32 = Vector4< std::int32_t >;
using Vector4_int64 = Vector4< std::int64_t >;
using Vector4_uint8 = Vector4< std::uint8_t >;
using Vector4_uint16 = Vector4< std::uint16_t >;
using Vector4_uint32 = Vector4< std::uint32_t >;
using Vector4_uint64 = Vector4< std::uint64_t >;
using Vector4_float = Vector4< float >;
using Vector4_double = Vector4< double >;
using Vector4_longdouble = Vector4< long double >;
using Vector4_char = Vector4< char >;
using Vector4_uchar = Vector4< unsigned char >;
using Vector4_schar = Vector4< signed char >;
using Vector4_string = Vector4< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Vector3_fwd_hh_INCLUDED
