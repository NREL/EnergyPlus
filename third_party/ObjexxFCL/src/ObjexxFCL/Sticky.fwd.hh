#ifndef ObjexxFCL_Sticky_fwd_hh_INCLUDED
#define ObjexxFCL_Sticky_fwd_hh_INCLUDED

// Sticky Forward Declarations
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
template< typename > class Sticky;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Sticky< bool >                Sticky_bool;
typedef  Sticky< byte >                Sticky_byte;
typedef  Sticky< sbyte >               Sticky_sbyte;
typedef  Sticky< ubyte >               Sticky_ubyte;
typedef  Sticky< short int >           Sticky_short;
typedef  Sticky< int >                 Sticky_int;
typedef  Sticky< long int >            Sticky_long;
typedef  Sticky< unsigned short int >  Sticky_ushort;
typedef  Sticky< unsigned int >        Sticky_uint;
typedef  Sticky< unsigned long int >   Sticky_ulong;
typedef  Sticky< std::size_t >         Sticky_size;
typedef  Sticky< std::int8_t >         Sticky_int8;
typedef  Sticky< std::int16_t >        Sticky_int16;
typedef  Sticky< std::int32_t >        Sticky_int32;
typedef  Sticky< std::int64_t >        Sticky_int64;
typedef  Sticky< std::uint8_t >        Sticky_uint8;
typedef  Sticky< std::uint16_t >       Sticky_uint16;
typedef  Sticky< std::uint32_t >       Sticky_uint32;
typedef  Sticky< std::uint64_t >       Sticky_uint64;
typedef  Sticky< float >               Sticky_float;
typedef  Sticky< double >              Sticky_double;
typedef  Sticky< long double >         Sticky_longdouble;
typedef  Sticky< char >                Sticky_char;
typedef  Sticky< unsigned char >       Sticky_uchar;
typedef  Sticky< signed char >         Sticky_schar;
typedef  Sticky< std::string >         Sticky_string;

// Const Types
typedef  Sticky< bool const >                Sticky_bool_const;
typedef  Sticky< byte const >                Sticky_byte_const;
typedef  Sticky< sbyte const >               Sticky_sbyte_const;
typedef  Sticky< ubyte const >               Sticky_ubyte_const;
typedef  Sticky< short int const >           Sticky_short_const;
typedef  Sticky< int const >                 Sticky_int_const;
typedef  Sticky< long int const >            Sticky_long_const;
typedef  Sticky< unsigned short int const >  Sticky_ushort_const;
typedef  Sticky< unsigned int const >        Sticky_uint_const;
typedef  Sticky< unsigned long int const >   Sticky_ulong_const;
typedef  Sticky< std::size_t const >         Sticky_size_const;
typedef  Sticky< std::int8_t const >         Sticky_int8_const;
typedef  Sticky< std::int16_t const >        Sticky_int16_const;
typedef  Sticky< std::int32_t const >        Sticky_int32_const;
typedef  Sticky< std::int64_t const >        Sticky_int64_const;
typedef  Sticky< std::uint8_t const >        Sticky_uint8_const;
typedef  Sticky< std::uint16_t const >       Sticky_uint16_const;
typedef  Sticky< std::uint32_t const >       Sticky_uint32_const;
typedef  Sticky< std::uint64_t const >       Sticky_uint64_const;
typedef  Sticky< float const >               Sticky_float_const;
typedef  Sticky< double const >              Sticky_double_const;
typedef  Sticky< long double const >         Sticky_longdouble_const;
typedef  Sticky< char const >                Sticky_char_const;
typedef  Sticky< unsigned char const >       Sticky_uchar_const;
typedef  Sticky< signed char const >         Sticky_schar_const;
typedef  Sticky< std::string const >         Sticky_string_const;

} // ObjexxFCL

#endif // ObjexxFCL_Sticky_fwd_hh_INCLUDED
