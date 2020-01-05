#ifndef ObjexxFCL_Array5A_fwd_hh_INCLUDED
#define ObjexxFCL_Array5A_fwd_hh_INCLUDED

// Array5A Forward Declarations
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
template< typename > class Array5A;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array5A_bool = Array5A< bool >;
using Array5A_byte = Array5A< byte >;
using Array5A_sbyte = Array5A< sbyte >;
using Array5A_ubyte = Array5A< ubyte >;
using Array5A_short = Array5A< short int >;
using Array5A_int = Array5A< int >;
using Array5A_long = Array5A< long int >;
using Array5A_ushort = Array5A< unsigned short int >;
using Array5A_uint = Array5A< unsigned int >;
using Array5A_ulong = Array5A< unsigned long int >;
using Array5A_size = Array5A< std::size_t >;
using Array5A_int8 = Array5A< std::int8_t >;
using Array5A_int16 = Array5A< std::int16_t >;
using Array5A_int32 = Array5A< std::int32_t >;
using Array5A_int64 = Array5A< std::int64_t >;
using Array5A_uint8 = Array5A< std::uint8_t >;
using Array5A_uint16 = Array5A< std::uint16_t >;
using Array5A_uint32 = Array5A< std::uint32_t >;
using Array5A_uint64 = Array5A< std::uint64_t >;
using Array5A_float = Array5A< float >;
using Array5A_double = Array5A< double >;
using Array5A_longdouble = Array5A< long double >;
using Array5A_char = Array5A< char >;
using Array5A_uchar = Array5A< unsigned char >;
using Array5A_schar = Array5A< signed char >;
using Array5A_string = Array5A< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array5A_fwd_hh_INCLUDED
