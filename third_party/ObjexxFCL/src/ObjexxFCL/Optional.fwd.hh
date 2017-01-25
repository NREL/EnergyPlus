#ifndef ObjexxFCL_Optional_fwd_hh_INCLUDED
#define ObjexxFCL_Optional_fwd_hh_INCLUDED

// Optional Forward Declarations
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

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
typedef  byte  sbyte;
typedef  Optional< bool >                Optional_bool;
typedef  Optional< byte >                Optional_byte;
typedef  Optional< sbyte >               Optional_sbyte;
typedef  Optional< ubyte >               Optional_ubyte;
typedef  Optional< short int >           Optional_short;
typedef  Optional< int >                 Optional_int;
typedef  Optional< long int >            Optional_long;
typedef  Optional< unsigned short int >  Optional_ushort;
typedef  Optional< unsigned int >        Optional_uint;
typedef  Optional< unsigned long int >   Optional_ulong;
typedef  Optional< std::size_t >         Optional_size;
typedef  Optional< std::int8_t >         Optional_int8;
typedef  Optional< std::int16_t >        Optional_int16;
typedef  Optional< std::int32_t >        Optional_int32;
typedef  Optional< std::int64_t >        Optional_int64;
typedef  Optional< std::uint8_t >        Optional_uint8;
typedef  Optional< std::uint16_t >       Optional_uint16;
typedef  Optional< std::uint32_t >       Optional_uint32;
typedef  Optional< std::uint64_t >       Optional_uint64;
typedef  Optional< float >               Optional_float;
typedef  Optional< double >              Optional_double;
typedef  Optional< long double >         Optional_longdouble;
typedef  Optional< char >                Optional_char;
typedef  Optional< unsigned char >       Optional_uchar;
typedef  Optional< signed char >         Optional_schar;
typedef  Optional< std::string >         Optional_string;

// Const Types
typedef  Optional< bool const >                Optional_bool_const;
typedef  Optional< byte const >                Optional_byte_const;
typedef  Optional< sbyte const >               Optional_sbyte_const;
typedef  Optional< ubyte const >               Optional_ubyte_const;
typedef  Optional< short int const >           Optional_short_const;
typedef  Optional< int const >                 Optional_int_const;
typedef  Optional< long int const >            Optional_long_const;
typedef  Optional< unsigned short int const >  Optional_ushort_const;
typedef  Optional< unsigned int const >        Optional_uint_const;
typedef  Optional< unsigned long int const >   Optional_ulong_const;
typedef  Optional< std::size_t const >         Optional_size_const;
typedef  Optional< std::int8_t const >         Optional_int8_const;
typedef  Optional< std::int16_t const >        Optional_int16_const;
typedef  Optional< std::int32_t const >        Optional_int32_const;
typedef  Optional< std::int64_t const >        Optional_int64_const;
typedef  Optional< std::uint8_t const >        Optional_uint8_const;
typedef  Optional< std::uint16_t const >       Optional_uint16_const;
typedef  Optional< std::uint32_t const >       Optional_uint32_const;
typedef  Optional< std::uint64_t const >       Optional_uint64_const;
typedef  Optional< float const >               Optional_float_const;
typedef  Optional< double const >              Optional_double_const;
typedef  Optional< long double const >         Optional_longdouble_const;
typedef  Optional< char const >                Optional_char_const;
typedef  Optional< unsigned char const >       Optional_uchar_const;
typedef  Optional< signed char const >         Optional_schar_const;
typedef  Optional< std::string const >         Optional_string_const;

} // ObjexxFCL

#endif // ObjexxFCL_Optional_fwd_hh_INCLUDED
