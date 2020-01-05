#ifndef ObjexxFCL_Array4A_fwd_hh_INCLUDED
#define ObjexxFCL_Array4A_fwd_hh_INCLUDED

// Array4A Forward Declarations
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
template< typename > class Array4A;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array4A_bool = Array4A< bool >;
using Array4A_byte = Array4A< byte >;
using Array4A_sbyte = Array4A< sbyte >;
using Array4A_ubyte = Array4A< ubyte >;
using Array4A_short = Array4A< short int >;
using Array4A_int = Array4A< int >;
using Array4A_long = Array4A< long int >;
using Array4A_ushort = Array4A< unsigned short int >;
using Array4A_uint = Array4A< unsigned int >;
using Array4A_ulong = Array4A< unsigned long int >;
using Array4A_size = Array4A< std::size_t >;
using Array4A_int8 = Array4A< std::int8_t >;
using Array4A_int16 = Array4A< std::int16_t >;
using Array4A_int32 = Array4A< std::int32_t >;
using Array4A_int64 = Array4A< std::int64_t >;
using Array4A_uint8 = Array4A< std::uint8_t >;
using Array4A_uint16 = Array4A< std::uint16_t >;
using Array4A_uint32 = Array4A< std::uint32_t >;
using Array4A_uint64 = Array4A< std::uint64_t >;
using Array4A_float = Array4A< float >;
using Array4A_double = Array4A< double >;
using Array4A_longdouble = Array4A< long double >;
using Array4A_char = Array4A< char >;
using Array4A_uchar = Array4A< unsigned char >;
using Array4A_schar = Array4A< signed char >;
using Array4A_string = Array4A< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array4A_fwd_hh_INCLUDED
