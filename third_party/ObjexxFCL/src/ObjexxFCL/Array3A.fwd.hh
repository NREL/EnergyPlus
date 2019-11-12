#ifndef ObjexxFCL_Array3A_fwd_hh_INCLUDED
#define ObjexxFCL_Array3A_fwd_hh_INCLUDED

// Array3A Forward Declarations
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
template< typename > class Array3A;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array3A_bool = Array3A< bool >;
using Array3A_byte = Array3A< byte >;
using Array3A_sbyte = Array3A< sbyte >;
using Array3A_ubyte = Array3A< ubyte >;
using Array3A_short = Array3A< short int >;
using Array3A_int = Array3A< int >;
using Array3A_long = Array3A< long int >;
using Array3A_ushort = Array3A< unsigned short int >;
using Array3A_uint = Array3A< unsigned int >;
using Array3A_ulong = Array3A< unsigned long int >;
using Array3A_size = Array3A< std::size_t >;
using Array3A_int8 = Array3A< std::int8_t >;
using Array3A_int16 = Array3A< std::int16_t >;
using Array3A_int32 = Array3A< std::int32_t >;
using Array3A_int64 = Array3A< std::int64_t >;
using Array3A_uint8 = Array3A< std::uint8_t >;
using Array3A_uint16 = Array3A< std::uint16_t >;
using Array3A_uint32 = Array3A< std::uint32_t >;
using Array3A_uint64 = Array3A< std::uint64_t >;
using Array3A_float = Array3A< float >;
using Array3A_double = Array3A< double >;
using Array3A_longdouble = Array3A< long double >;
using Array3A_char = Array3A< char >;
using Array3A_uchar = Array3A< unsigned char >;
using Array3A_schar = Array3A< signed char >;
using Array3A_string = Array3A< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array3A_fwd_hh_INCLUDED
