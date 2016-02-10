#ifndef ObjexxFCL_Required_fwd_hh_INCLUDED
#define ObjexxFCL_Required_fwd_hh_INCLUDED

// Required Forward Declarations
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
template< typename, typename = void > class Required;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Required< bool >                Required_bool;
typedef  Required< byte >                Required_byte;
typedef  Required< sbyte >               Required_sbyte;
typedef  Required< ubyte >               Required_ubyte;
typedef  Required< short int >           Required_short;
typedef  Required< int >                 Required_int;
typedef  Required< long int >            Required_long;
typedef  Required< unsigned short int >  Required_ushort;
typedef  Required< unsigned int >        Required_uint;
typedef  Required< unsigned long int >   Required_ulong;
typedef  Required< std::size_t >         Required_size;
typedef  Required< std::int8_t >         Required_int8;
typedef  Required< std::int16_t >        Required_int16;
typedef  Required< std::int32_t >        Required_int32;
typedef  Required< std::int64_t >        Required_int64;
typedef  Required< std::uint8_t >        Required_uint8;
typedef  Required< std::uint16_t >       Required_uint16;
typedef  Required< std::uint32_t >       Required_uint32;
typedef  Required< std::uint64_t >       Required_uint64;
typedef  Required< float >               Required_float;
typedef  Required< double >              Required_double;
typedef  Required< long double >         Required_longdouble;
typedef  Required< char >                Required_char;
typedef  Required< unsigned char >       Required_uchar;
typedef  Required< signed char >         Required_schar;
typedef  Required< std::string >         Required_string;

// Const Types
typedef  Required< bool const >                Required_bool_const;
typedef  Required< byte const >                Required_byte_const;
typedef  Required< sbyte const >               Required_sbyte_const;
typedef  Required< ubyte const >               Required_ubyte_const;
typedef  Required< short int const >           Required_short_const;
typedef  Required< int const >                 Required_int_const;
typedef  Required< long int const >            Required_long_const;
typedef  Required< unsigned short int const >  Required_ushort_const;
typedef  Required< unsigned int const >        Required_uint_const;
typedef  Required< unsigned long int const >   Required_ulong_const;
typedef  Required< std::size_t const >         Required_size_const;
typedef  Required< std::int8_t const >         Required_int8_const;
typedef  Required< std::int16_t const >        Required_int16_const;
typedef  Required< std::int32_t const >        Required_int32_const;
typedef  Required< std::int64_t const >        Required_int64_const;
typedef  Required< std::uint8_t const >        Required_uint8_const;
typedef  Required< std::uint16_t const >       Required_uint16_const;
typedef  Required< std::uint32_t const >       Required_uint32_const;
typedef  Required< std::uint64_t const >       Required_uint64_const;
typedef  Required< float const >               Required_float_const;
typedef  Required< double const >              Required_double_const;
typedef  Required< long double const >         Required_longdouble_const;
typedef  Required< char const >                Required_char_const;
typedef  Required< unsigned char const >       Required_uchar_const;
typedef  Required< signed char const >         Required_schar_const;
typedef  Required< std::string const >         Required_string_const;

} // ObjexxFCL

#endif // ObjexxFCL_Required_fwd_hh_INCLUDED
