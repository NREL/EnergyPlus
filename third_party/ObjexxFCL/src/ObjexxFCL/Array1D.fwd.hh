#ifndef ObjexxFCL_Array1D_fwd_hh_INCLUDED
#define ObjexxFCL_Array1D_fwd_hh_INCLUDED

// Array1D Forward Declarations
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
template< typename > class Array1D;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array1D_bool = Array1D< bool >;
using Array1D_byte = Array1D< byte >;
using Array1D_sbyte = Array1D< sbyte >;
using Array1D_ubyte = Array1D< ubyte >;
using Array1D_short = Array1D< short int >;
using Array1D_int = Array1D< int >;
using Array1D_long = Array1D< long int >;
using Array1D_ushort = Array1D< unsigned short int >;
using Array1D_uint = Array1D< unsigned int >;
using Array1D_ulong = Array1D< unsigned long int >;
using Array1D_size = Array1D< std::size_t >;
using Array1D_int8 = Array1D< std::int8_t >;
using Array1D_int16 = Array1D< std::int16_t >;
using Array1D_int32 = Array1D< std::int32_t >;
using Array1D_int64 = Array1D< std::int64_t >;
using Array1D_uint8 = Array1D< std::uint8_t >;
using Array1D_uint16 = Array1D< std::uint16_t >;
using Array1D_uint32 = Array1D< std::uint32_t >;
using Array1D_uint64 = Array1D< std::uint64_t >;
using Array1D_float = Array1D< float >;
using Array1D_double = Array1D< double >;
using Array1D_longdouble = Array1D< long double >;
using Array1D_char = Array1D< char >;
using Array1D_uchar = Array1D< unsigned char >;
using Array1D_schar = Array1D< signed char >;
using Array1D_string = Array1D< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array1D_fwd_hh_INCLUDED
