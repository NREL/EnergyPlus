#ifndef ObjexxFCL_Array6D_fwd_hh_INCLUDED
#define ObjexxFCL_Array6D_fwd_hh_INCLUDED

// Array6D Forward Declarations
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
template< typename > class Array6D;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array6D_bool = Array6D< bool >;
using Array6D_byte = Array6D< byte >;
using Array6D_sbyte = Array6D< sbyte >;
using Array6D_ubyte = Array6D< ubyte >;
using Array6D_short = Array6D< short int >;
using Array6D_int = Array6D< int >;
using Array6D_long = Array6D< long int >;
using Array6D_ushort = Array6D< unsigned short int >;
using Array6D_uint = Array6D< unsigned int >;
using Array6D_ulong = Array6D< unsigned long int >;
using Array6D_size = Array6D< std::size_t >;
using Array6D_int8 = Array6D< std::int8_t >;
using Array6D_int16 = Array6D< std::int16_t >;
using Array6D_int32 = Array6D< std::int32_t >;
using Array6D_int64 = Array6D< std::int64_t >;
using Array6D_uint8 = Array6D< std::uint8_t >;
using Array6D_uint16 = Array6D< std::uint16_t >;
using Array6D_uint32 = Array6D< std::uint32_t >;
using Array6D_uint64 = Array6D< std::uint64_t >;
using Array6D_float = Array6D< float >;
using Array6D_double = Array6D< double >;
using Array6D_longdouble = Array6D< long double >;
using Array6D_char = Array6D< char >;
using Array6D_uchar = Array6D< unsigned char >;
using Array6D_schar = Array6D< signed char >;
using Array6D_string = Array6D< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array6D_fwd_hh_INCLUDED
