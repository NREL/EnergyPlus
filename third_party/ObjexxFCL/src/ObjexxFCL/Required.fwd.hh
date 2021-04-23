#ifndef ObjexxFCL_Required_fwd_hh_INCLUDED
#define ObjexxFCL_Required_fwd_hh_INCLUDED

// Required Forward Declarations
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
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

// Types
typedef  Required< bool >                Required_bool;
typedef  Required< byte >                Required_byte;
typedef  Required< short int >           Required_short;
typedef  Required< int >                 Required_int;
typedef  Required< unsigned short int >  Required_ushort;
typedef  Required< std::size_t >         Required_size;
typedef  Required< std::int32_t >        Required_int32;
typedef  Required< double >              Required_double;
typedef  Required< signed char >         Required_schar;
typedef  Required< std::string >         Required_string;

// Const Types
typedef  Required< bool const >                Required_bool_const;
typedef  Required< byte const >                Required_byte_const;
typedef  Required< short int const >           Required_short_const;
typedef  Required< int const >                 Required_int_const;
typedef  Required< long int const >            Required_long_const;
typedef  Required< unsigned short int const >  Required_ushort_const;
typedef  Required< std::size_t const >         Required_size_const;
typedef  Required< std::int8_t const >         Required_int8_const;
typedef  Required< float const >               Required_float_const;
typedef  Required< double const >              Required_double_const;
typedef  Required< char const >                Required_char_const;
typedef  Required< signed char const >         Required_schar_const;
typedef  Required< std::string const >         Required_string_const;

} // ObjexxFCL

#endif // ObjexxFCL_Required_fwd_hh_INCLUDED
