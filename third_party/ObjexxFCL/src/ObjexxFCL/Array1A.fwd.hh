#ifndef ObjexxFCL_Array1A_fwd_hh_INCLUDED
#define ObjexxFCL_Array1A_fwd_hh_INCLUDED

// Array1A Forward Declarations
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
template< typename > class Array1A;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array1A_bool = Array1A< bool >;
using Array1A_byte = Array1A< byte >;
using Array1A_sbyte = Array1A< sbyte >;
using Array1A_ubyte = Array1A< ubyte >;
using Array1A_short = Array1A< short int >;
using Array1A_int = Array1A< int >;
using Array1A_long = Array1A< long int >;
using Array1A_ushort = Array1A< unsigned short int >;
using Array1A_uint = Array1A< unsigned int >;
using Array1A_ulong = Array1A< unsigned long int >;
using Array1A_size = Array1A< std::size_t >;
using Array1A_int8 = Array1A< std::int8_t >;
using Array1A_int16 = Array1A< std::int16_t >;
using Array1A_int32 = Array1A< std::int32_t >;
using Array1A_int64 = Array1A< std::int64_t >;
using Array1A_uint8 = Array1A< std::uint8_t >;
using Array1A_uint16 = Array1A< std::uint16_t >;
using Array1A_uint32 = Array1A< std::uint32_t >;
using Array1A_uint64 = Array1A< std::uint64_t >;
using Array1A_float = Array1A< float >;
using Array1A_double = Array1A< double >;
using Array1A_longdouble = Array1A< long double >;
using Array1A_char = Array1A< char >;
using Array1A_uchar = Array1A< unsigned char >;
using Array1A_schar = Array1A< signed char >;
using Array1A_string = Array1A< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array1A_fwd_hh_INCLUDED
