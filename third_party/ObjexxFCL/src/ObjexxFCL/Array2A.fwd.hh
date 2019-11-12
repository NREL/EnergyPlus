#ifndef ObjexxFCL_Array2A_fwd_hh_INCLUDED
#define ObjexxFCL_Array2A_fwd_hh_INCLUDED

// Array2A Forward Declarations
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
template< typename > class Array2A;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Array2A_bool = Array2A< bool >;
using Array2A_byte = Array2A< byte >;
using Array2A_sbyte = Array2A< sbyte >;
using Array2A_ubyte = Array2A< ubyte >;
using Array2A_short = Array2A< short int >;
using Array2A_int = Array2A< int >;
using Array2A_long = Array2A< long int >;
using Array2A_ushort = Array2A< unsigned short int >;
using Array2A_uint = Array2A< unsigned int >;
using Array2A_ulong = Array2A< unsigned long int >;
using Array2A_size = Array2A< std::size_t >;
using Array2A_int8 = Array2A< std::int8_t >;
using Array2A_int16 = Array2A< std::int16_t >;
using Array2A_int32 = Array2A< std::int32_t >;
using Array2A_int64 = Array2A< std::int64_t >;
using Array2A_uint8 = Array2A< std::uint8_t >;
using Array2A_uint16 = Array2A< std::uint16_t >;
using Array2A_uint32 = Array2A< std::uint32_t >;
using Array2A_uint64 = Array2A< std::uint64_t >;
using Array2A_float = Array2A< float >;
using Array2A_double = Array2A< double >;
using Array2A_longdouble = Array2A< long double >;
using Array2A_char = Array2A< char >;
using Array2A_uchar = Array2A< unsigned char >;
using Array2A_schar = Array2A< signed char >;
using Array2A_string = Array2A< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_Array2A_fwd_hh_INCLUDED
