#ifndef ObjexxFCL_Optional_fwd_hh_INCLUDED
#define ObjexxFCL_Optional_fwd_hh_INCLUDED

// Optional Forward Declarations
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
template< typename, typename = void > class Optional;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Optional_bool = Optional< bool >;
using Optional_byte = Optional< byte >;
using Optional_sbyte = Optional< sbyte >;
using Optional_ubyte = Optional< ubyte >;
using Optional_short = Optional< short int >;
using Optional_int = Optional< int >;
using Optional_long = Optional< long int >;
using Optional_ushort = Optional< unsigned short int >;
using Optional_uint = Optional< unsigned int >;
using Optional_ulong = Optional< unsigned long int >;
using Optional_size = Optional< std::size_t >;
using Optional_int8 = Optional< std::int8_t >;
using Optional_int16 = Optional< std::int16_t >;
using Optional_int32 = Optional< std::int32_t >;
using Optional_int64 = Optional< std::int64_t >;
using Optional_uint8 = Optional< std::uint8_t >;
using Optional_uint16 = Optional< std::uint16_t >;
using Optional_uint32 = Optional< std::uint32_t >;
using Optional_uint64 = Optional< std::uint64_t >;
using Optional_float = Optional< float >;
using Optional_double = Optional< double >;
using Optional_longdouble = Optional< long double >;
using Optional_char = Optional< char >;
using Optional_uchar = Optional< unsigned char >;
using Optional_schar = Optional< signed char >;
using Optional_string = Optional< std::string >;

// Const Types
using Optional_bool_const = Optional< bool const >;
using Optional_byte_const = Optional< byte const >;
using Optional_sbyte_const = Optional< sbyte const >;
using Optional_ubyte_const = Optional< ubyte const >;
using Optional_short_const = Optional< short int const >;
using Optional_int_const = Optional< int const >;
using Optional_long_const = Optional< long int const >;
using Optional_ushort_const = Optional< unsigned short int const >;
using Optional_uint_const = Optional< unsigned int const >;
using Optional_ulong_const = Optional< unsigned long int const >;
using Optional_size_const = Optional< std::size_t const >;
using Optional_int8_const = Optional< std::int8_t const >;
using Optional_int16_const = Optional< std::int16_t const >;
using Optional_int32_const = Optional< std::int32_t const >;
using Optional_int64_const = Optional< std::int64_t const >;
using Optional_uint8_const = Optional< std::uint8_t const >;
using Optional_uint16_const = Optional< std::uint16_t const >;
using Optional_uint32_const = Optional< std::uint32_t const >;
using Optional_uint64_const = Optional< std::uint64_t const >;
using Optional_float_const = Optional< float const >;
using Optional_double_const = Optional< double const >;
using Optional_longdouble_const = Optional< long double const >;
using Optional_char_const = Optional< char const >;
using Optional_uchar_const = Optional< unsigned char const >;
using Optional_schar_const = Optional< signed char const >;
using Optional_string_const = Optional< std::string const >;

} // ObjexxFCL

#endif // ObjexxFCL_Optional_fwd_hh_INCLUDED
