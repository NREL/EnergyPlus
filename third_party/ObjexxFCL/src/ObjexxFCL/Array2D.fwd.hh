#ifndef ObjexxFCL_Array2D_fwd_hh_INCLUDED
#define ObjexxFCL_Array2D_fwd_hh_INCLUDED

// Array2D Forward Declarations
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
template< typename > class Array2D;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array2D_bool = Array2D< bool >;
using Array2D_byte = Array2D< byte >;
using Array2D_sbyte = Array2D< sbyte >;
using Array2D_ubyte = Array2D< ubyte >;
using Array2D_short = Array2D< short int >;
using Array2D_int = Array2D< int >;
using Array2D_long = Array2D< long int >;
using Array2D_ushort = Array2D< unsigned short int >;
using Array2D_uint = Array2D< unsigned int >;
using Array2D_ulong = Array2D< unsigned long int >;
using Array2D_size = Array2D< std::size_t >;
using Array2D_int8 = Array2D< std::int8_t >;
using Array2D_int16 = Array2D< std::int16_t >;
using Array2D_int32 = Array2D< std::int32_t >;
using Array2D_int64 = Array2D< std::int64_t >;
using Array2D_uint8 = Array2D< std::uint8_t >;
using Array2D_uint16 = Array2D< std::uint16_t >;
using Array2D_uint32 = Array2D< std::uint32_t >;
using Array2D_uint64 = Array2D< std::uint64_t >;
using Array2D_float = Array2D< float >;
using Array2D_double = Array2D< double >;
using Array2D_longdouble = Array2D< long double >;
using Array2D_char = Array2D< char >;
using Array2D_uchar = Array2D< unsigned char >;
using Array2D_schar = Array2D< signed char >;
using Array2D_string = Array2D< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array2D_fwd_hh_INCLUDED
