#ifndef ObjexxFCL_Array6A_fwd_hh_INCLUDED
#define ObjexxFCL_Array6A_fwd_hh_INCLUDED

// Array6A Forward Declarations
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
template< typename > class Array6A;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array6A_bool = Array6A< bool >;
using Array6A_byte = Array6A< byte >;
using Array6A_sbyte = Array6A< sbyte >;
using Array6A_ubyte = Array6A< ubyte >;
using Array6A_short = Array6A< short int >;
using Array6A_int = Array6A< int >;
using Array6A_long = Array6A< long int >;
using Array6A_ushort = Array6A< unsigned short int >;
using Array6A_uint = Array6A< unsigned int >;
using Array6A_ulong = Array6A< unsigned long int >;
using Array6A_size = Array6A< std::size_t >;
using Array6A_int8 = Array6A< std::int8_t >;
using Array6A_int16 = Array6A< std::int16_t >;
using Array6A_int32 = Array6A< std::int32_t >;
using Array6A_int64 = Array6A< std::int64_t >;
using Array6A_uint8 = Array6A< std::uint8_t >;
using Array6A_uint16 = Array6A< std::uint16_t >;
using Array6A_uint32 = Array6A< std::uint32_t >;
using Array6A_uint64 = Array6A< std::uint64_t >;
using Array6A_float = Array6A< float >;
using Array6A_double = Array6A< double >;
using Array6A_longdouble = Array6A< long double >;
using Array6A_char = Array6A< char >;
using Array6A_uchar = Array6A< unsigned char >;
using Array6A_schar = Array6A< signed char >;
using Array6A_string = Array6A< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array6A_fwd_hh_INCLUDED
