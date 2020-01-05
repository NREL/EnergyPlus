#ifndef ObjexxFCL_Array3D_fwd_hh_INCLUDED
#define ObjexxFCL_Array3D_fwd_hh_INCLUDED

// Array3D Forward Declarations
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
template< typename > class Array3D;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array3D_bool = Array3D< bool >;
using Array3D_byte = Array3D< byte >;
using Array3D_sbyte = Array3D< sbyte >;
using Array3D_ubyte = Array3D< ubyte >;
using Array3D_short = Array3D< short int >;
using Array3D_int = Array3D< int >;
using Array3D_long = Array3D< long int >;
using Array3D_ushort = Array3D< unsigned short int >;
using Array3D_uint = Array3D< unsigned int >;
using Array3D_ulong = Array3D< unsigned long int >;
using Array3D_size = Array3D< std::size_t >;
using Array3D_int8 = Array3D< std::int8_t >;
using Array3D_int16 = Array3D< std::int16_t >;
using Array3D_int32 = Array3D< std::int32_t >;
using Array3D_int64 = Array3D< std::int64_t >;
using Array3D_uint8 = Array3D< std::uint8_t >;
using Array3D_uint16 = Array3D< std::uint16_t >;
using Array3D_uint32 = Array3D< std::uint32_t >;
using Array3D_uint64 = Array3D< std::uint64_t >;
using Array3D_float = Array3D< float >;
using Array3D_double = Array3D< double >;
using Array3D_longdouble = Array3D< long double >;
using Array3D_char = Array3D< char >;
using Array3D_uchar = Array3D< unsigned char >;
using Array3D_schar = Array3D< signed char >;
using Array3D_string = Array3D< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array3D_fwd_hh_INCLUDED
