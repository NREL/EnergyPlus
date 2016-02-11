#ifndef ObjexxFCL_Reference_fwd_hh_INCLUDED
#define ObjexxFCL_Reference_fwd_hh_INCLUDED

// Reference Forward Declarations
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
template< typename > class Reference;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Reference< bool >                Reference_bool;
typedef  Reference< byte >                Reference_byte;
typedef  Reference< sbyte >               Reference_sbyte;
typedef  Reference< ubyte >               Reference_ubyte;
typedef  Reference< short int >           Reference_short;
typedef  Reference< int >                 Reference_int;
typedef  Reference< long int >            Reference_long;
typedef  Reference< unsigned short int >  Reference_ushort;
typedef  Reference< unsigned int >        Reference_uint;
typedef  Reference< unsigned long int >   Reference_ulong;
typedef  Reference< std::size_t >         Reference_size;
typedef  Reference< std::int8_t >         Reference_int8;
typedef  Reference< std::int16_t >        Reference_int16;
typedef  Reference< std::int32_t >        Reference_int32;
typedef  Reference< std::int64_t >        Reference_int64;
typedef  Reference< std::uint8_t >        Reference_uint8;
typedef  Reference< std::uint16_t >       Reference_uint16;
typedef  Reference< std::uint32_t >       Reference_uint32;
typedef  Reference< std::uint64_t >       Reference_uint64;
typedef  Reference< float >               Reference_float;
typedef  Reference< double >              Reference_double;
typedef  Reference< long double >         Reference_longdouble;
typedef  Reference< char >                Reference_char;
typedef  Reference< unsigned char >       Reference_uchar;
typedef  Reference< signed char >         Reference_schar;
typedef  Reference< std::string >         Reference_string;

// Const Types
typedef  Reference< bool const >                Reference_bool_const;
typedef  Reference< byte const >                Reference_byte_const;
typedef  Reference< sbyte const >               Reference_sbyte_const;
typedef  Reference< ubyte const >               Reference_ubyte_const;
typedef  Reference< short int const >           Reference_short_const;
typedef  Reference< int const >                 Reference_int_const;
typedef  Reference< long int const >            Reference_long_const;
typedef  Reference< unsigned short int const >  Reference_ushort_const;
typedef  Reference< unsigned int const >        Reference_uint_const;
typedef  Reference< unsigned long int const >   Reference_ulong_const;
typedef  Reference< std::size_t const >         Reference_size_const;
typedef  Reference< std::int8_t const >         Reference_int8_const;
typedef  Reference< std::int16_t const >        Reference_int16_const;
typedef  Reference< std::int32_t const >        Reference_int32_const;
typedef  Reference< std::int64_t const >        Reference_int64_const;
typedef  Reference< std::uint8_t const >        Reference_uint8_const;
typedef  Reference< std::uint16_t const >       Reference_uint16_const;
typedef  Reference< std::uint32_t const >       Reference_uint32_const;
typedef  Reference< std::uint64_t const >       Reference_uint64_const;
typedef  Reference< float const >               Reference_float_const;
typedef  Reference< double const >              Reference_double_const;
typedef  Reference< long double const >         Reference_longdouble_const;
typedef  Reference< char const >                Reference_char_const;
typedef  Reference< unsigned char const >       Reference_uchar_const;
typedef  Reference< signed char const >         Reference_schar_const;
typedef  Reference< std::string const >         Reference_string_const;

} // ObjexxFCL

#endif // ObjexxFCL_Reference_fwd_hh_INCLUDED
