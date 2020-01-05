#ifndef ObjexxFCL_Vector3_fwd_hh_INCLUDED
#define ObjexxFCL_Vector3_fwd_hh_INCLUDED

// Vector3 Forward Declarations
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
template< typename > class Vector3;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Vector3_bool = Vector3< bool >;
using Vector3_byte = Vector3< byte >;
using Vector3_sbyte = Vector3< sbyte >;
using Vector3_ubyte = Vector3< ubyte >;
using Vector3_short = Vector3< short int >;
using Vector3_int = Vector3< int >;
using Vector3_long = Vector3< long int >;
using Vector3_ushort = Vector3< unsigned short int >;
using Vector3_uint = Vector3< unsigned int >;
using Vector3_ulong = Vector3< unsigned long int >;
using Vector3_size = Vector3< std::size_t >;
using Vector3_int8 = Vector3< std::int8_t >;
using Vector3_int16 = Vector3< std::int16_t >;
using Vector3_int32 = Vector3< std::int32_t >;
using Vector3_int64 = Vector3< std::int64_t >;
using Vector3_uint8 = Vector3< std::uint8_t >;
using Vector3_uint16 = Vector3< std::uint16_t >;
using Vector3_uint32 = Vector3< std::uint32_t >;
using Vector3_uint64 = Vector3< std::uint64_t >;
using Vector3_float = Vector3< float >;
using Vector3_double = Vector3< double >;
using Vector3_longdouble = Vector3< long double >;
using Vector3_char = Vector3< char >;
using Vector3_uchar = Vector3< unsigned char >;
using Vector3_schar = Vector3< signed char >;
using Vector3_string = Vector3< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Vector3_fwd_hh_INCLUDED
