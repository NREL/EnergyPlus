#ifndef ObjexxFCL_Required_fwd_hh_INCLUDED
#define ObjexxFCL_Required_fwd_hh_INCLUDED

// Required Forward Declarations
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
template< typename, typename = void > class Required;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Required_bool = Required< bool >;
using Required_byte = Required< byte >;
using Required_sbyte = Required< sbyte >;
using Required_ubyte = Required< ubyte >;
using Required_short = Required< short int >;
using Required_int = Required< int >;
using Required_long = Required< long int >;
using Required_ushort = Required< unsigned short int >;
using Required_uint = Required< unsigned int >;
using Required_ulong = Required< unsigned long int >;
using Required_size = Required< std::size_t >;
using Required_int8 = Required< std::int8_t >;
using Required_int16 = Required< std::int16_t >;
using Required_int32 = Required< std::int32_t >;
using Required_int64 = Required< std::int64_t >;
using Required_uint8 = Required< std::uint8_t >;
using Required_uint16 = Required< std::uint16_t >;
using Required_uint32 = Required< std::uint32_t >;
using Required_uint64 = Required< std::uint64_t >;
using Required_float = Required< float >;
using Required_double = Required< double >;
using Required_longdouble = Required< long double >;
using Required_char = Required< char >;
using Required_uchar = Required< unsigned char >;
using Required_schar = Required< signed char >;
using Required_string = Required< std::string >;

// Const Types
using Required_bool_const = Required< bool const >;
using Required_byte_const = Required< byte const >;
using Required_sbyte_const = Required< sbyte const >;
using Required_ubyte_const = Required< ubyte const >;
using Required_short_const = Required< short int const >;
using Required_int_const = Required< int const >;
using Required_long_const = Required< long int const >;
using Required_ushort_const = Required< unsigned short int const >;
using Required_uint_const = Required< unsigned int const >;
using Required_ulong_const = Required< unsigned long int const >;
using Required_size_const = Required< std::size_t const >;
using Required_int8_const = Required< std::int8_t const >;
using Required_int16_const = Required< std::int16_t const >;
using Required_int32_const = Required< std::int32_t const >;
using Required_int64_const = Required< std::int64_t const >;
using Required_uint8_const = Required< std::uint8_t const >;
using Required_uint16_const = Required< std::uint16_t const >;
using Required_uint32_const = Required< std::uint32_t const >;
using Required_uint64_const = Required< std::uint64_t const >;
using Required_float_const = Required< float const >;
using Required_double_const = Required< double const >;
using Required_longdouble_const = Required< long double const >;
using Required_char_const = Required< char const >;
using Required_uchar_const = Required< unsigned char const >;
using Required_schar_const = Required< signed char const >;
using Required_string_const = Required< std::string const >;

} // ObjexxFCL

#endif // ObjexxFCL_Required_fwd_hh_INCLUDED
