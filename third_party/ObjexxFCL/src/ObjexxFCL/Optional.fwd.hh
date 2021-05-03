#ifndef ObjexxFCL_Optional_fwd_hh_INCLUDED
#define ObjexxFCL_Optional_fwd_hh_INCLUDED

// Optional Forward Declarations
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
template< typename > class Optional;

// Types
typedef  Optional< bool >                Optional_bool;
typedef  Optional< short int >           Optional_short;
typedef  Optional< int >                 Optional_int;
typedef  Optional< long int >            Optional_long;
typedef  Optional< unsigned short int >  Optional_ushort;
typedef  Optional< std::size_t >         Optional_size;
typedef  Optional< float >               Optional_float;
typedef  Optional< char >                Optional_char;
typedef  Optional< signed char >         Optional_schar;
typedef  Optional< std::string >         Optional_string;

// Const Types
typedef  Optional< bool const >                Optional_bool_const;
typedef  Optional< short int const >           Optional_short_const;
typedef  Optional< int const >                 Optional_int_const;
typedef  Optional< long int const >            Optional_long_const;
typedef  Optional< std::size_t const >         Optional_size_const;
typedef  Optional< float const >               Optional_float_const;
typedef  Optional< double const >              Optional_double_const;
typedef  Optional< signed char const >         Optional_schar_const;
typedef  Optional< std::string const >         Optional_string_const;

} // ObjexxFCL

#endif // ObjexxFCL_Optional_fwd_hh_INCLUDED
