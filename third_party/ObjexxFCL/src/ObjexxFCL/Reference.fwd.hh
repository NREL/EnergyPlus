#ifndef ObjexxFCL_Reference_fwd_hh_INCLUDED
#define ObjexxFCL_Reference_fwd_hh_INCLUDED

// Reference Forward Declarations
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
template< typename > class Reference;
class byte;
class ubyte;

// Types
using sbyte = byte;
using Reference_bool = Reference< bool >;
using Reference_byte = Reference< byte >;
using Reference_sbyte = Reference< sbyte >;
using Reference_ubyte = Reference< ubyte >;
using Reference_short = Reference< short int >;
using Reference_int = Reference< int >;
using Reference_long = Reference< long int >;
using Reference_ushort = Reference< unsigned short int >;
using Reference_uint = Reference< unsigned int >;
using Reference_ulong = Reference< unsigned long int >;
using Reference_size = Reference< std::size_t >;
using Reference_int8 = Reference< std::int8_t >;
using Reference_int16 = Reference< std::int16_t >;
using Reference_int32 = Reference< std::int32_t >;
using Reference_int64 = Reference< std::int64_t >;
using Reference_uint8 = Reference< std::uint8_t >;
using Reference_uint16 = Reference< std::uint16_t >;
using Reference_uint32 = Reference< std::uint32_t >;
using Reference_uint64 = Reference< std::uint64_t >;
using Reference_float = Reference< float >;
using Reference_double = Reference< double >;
using Reference_longdouble = Reference< long double >;
using Reference_char = Reference< char >;
using Reference_uchar = Reference< unsigned char >;
using Reference_schar = Reference< signed char >;
using Reference_string = Reference< std::string >;

// Const Types
using Reference_bool_const = Reference< bool const >;
using Reference_byte_const = Reference< byte const >;
using Reference_sbyte_const = Reference< sbyte const >;
using Reference_ubyte_const = Reference< ubyte const >;
using Reference_short_const = Reference< short int const >;
using Reference_int_const = Reference< int const >;
using Reference_long_const = Reference< long int const >;
using Reference_ushort_const = Reference< unsigned short int const >;
using Reference_uint_const = Reference< unsigned int const >;
using Reference_ulong_const = Reference< unsigned long int const >;
using Reference_size_const = Reference< std::size_t const >;
using Reference_int8_const = Reference< std::int8_t const >;
using Reference_int16_const = Reference< std::int16_t const >;
using Reference_int32_const = Reference< std::int32_t const >;
using Reference_int64_const = Reference< std::int64_t const >;
using Reference_uint8_const = Reference< std::uint8_t const >;
using Reference_uint16_const = Reference< std::uint16_t const >;
using Reference_uint32_const = Reference< std::uint32_t const >;
using Reference_uint64_const = Reference< std::uint64_t const >;
using Reference_float_const = Reference< float const >;
using Reference_double_const = Reference< double const >;
using Reference_longdouble_const = Reference< long double const >;
using Reference_char_const = Reference< char const >;
using Reference_uchar_const = Reference< unsigned char const >;
using Reference_schar_const = Reference< signed char const >;
using Reference_string_const = Reference< std::string const >;

} // ObjexxFCL

#endif // ObjexxFCL_Reference_fwd_hh_INCLUDED
