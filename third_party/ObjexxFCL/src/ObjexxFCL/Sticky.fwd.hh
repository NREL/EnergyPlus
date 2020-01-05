#ifndef ObjexxFCL_Sticky_fwd_hh_INCLUDED
#define ObjexxFCL_Sticky_fwd_hh_INCLUDED

// Sticky Forward Declarations
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
template< typename > class Sticky;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Sticky_bool = Sticky< bool >;
using Sticky_byte = Sticky< byte >;
using Sticky_sbyte = Sticky< sbyte >;
using Sticky_ubyte = Sticky< ubyte >;
using Sticky_short = Sticky< short int >;
using Sticky_int = Sticky< int >;
using Sticky_long = Sticky< long int >;
using Sticky_ushort = Sticky< unsigned short int >;
using Sticky_uint = Sticky< unsigned int >;
using Sticky_ulong = Sticky< unsigned long int >;
using Sticky_size = Sticky< std::size_t >;
using Sticky_int8 = Sticky< std::int8_t >;
using Sticky_int16 = Sticky< std::int16_t >;
using Sticky_int32 = Sticky< std::int32_t >;
using Sticky_int64 = Sticky< std::int64_t >;
using Sticky_uint8 = Sticky< std::uint8_t >;
using Sticky_uint16 = Sticky< std::uint16_t >;
using Sticky_uint32 = Sticky< std::uint32_t >;
using Sticky_uint64 = Sticky< std::uint64_t >;
using Sticky_float = Sticky< float >;
using Sticky_double = Sticky< double >;
using Sticky_longdouble = Sticky< long double >;
using Sticky_char = Sticky< char >;
using Sticky_uchar = Sticky< unsigned char >;
using Sticky_schar = Sticky< signed char >;
using Sticky_string = Sticky< std::string >;

// Const Types
using Sticky_bool_const = Sticky< bool const >;
using Sticky_byte_const = Sticky< byte const >;
using Sticky_sbyte_const = Sticky< sbyte const >;
using Sticky_ubyte_const = Sticky< ubyte const >;
using Sticky_short_const = Sticky< short int const >;
using Sticky_int_const = Sticky< int const >;
using Sticky_long_const = Sticky< long int const >;
using Sticky_ushort_const = Sticky< unsigned short int const >;
using Sticky_uint_const = Sticky< unsigned int const >;
using Sticky_ulong_const = Sticky< unsigned long int const >;
using Sticky_size_const = Sticky< std::size_t const >;
using Sticky_int8_const = Sticky< std::int8_t const >;
using Sticky_int16_const = Sticky< std::int16_t const >;
using Sticky_int32_const = Sticky< std::int32_t const >;
using Sticky_int64_const = Sticky< std::int64_t const >;
using Sticky_uint8_const = Sticky< std::uint8_t const >;
using Sticky_uint16_const = Sticky< std::uint16_t const >;
using Sticky_uint32_const = Sticky< std::uint32_t const >;
using Sticky_uint64_const = Sticky< std::uint64_t const >;
using Sticky_float_const = Sticky< float const >;
using Sticky_double_const = Sticky< double const >;
using Sticky_longdouble_const = Sticky< long double const >;
using Sticky_char_const = Sticky< char const >;
using Sticky_uchar_const = Sticky< unsigned char const >;
using Sticky_schar_const = Sticky< signed char const >;
using Sticky_string_const = Sticky< std::string const >;

} // ObjexxFCL

#endif // ObjexxFCL_Sticky_fwd_hh_INCLUDED
