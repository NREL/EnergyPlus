#ifndef ObjexxFCL_Reference_fwd_hh_INCLUDED
#define ObjexxFCL_Reference_fwd_hh_INCLUDED

// Reference Forward Declarations
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
template< typename > class Reference;

// Types
typedef  Reference< bool >                Reference_bool;
typedef  Reference< short int >           Reference_short;
typedef  Reference< int >                 Reference_int;
typedef  Reference< std::size_t >         Reference_size;
typedef  Reference< std::int32_t >        Reference_int32;
typedef  Reference< double >              Reference_double;
typedef  Reference< signed char >         Reference_schar;
typedef  Reference< std::string >         Reference_string;

// Const Types
typedef  Reference< bool const >                Reference_bool_const;
typedef  Reference< short int const >           Reference_short_const;
typedef  Reference< int const >                 Reference_int_const;
typedef  Reference< long int const >            Reference_long_const;
typedef  Reference< unsigned short int const >  Reference_ushort_const;
typedef  Reference< unsigned int const >        Reference_uint_const;
typedef  Reference< std::size_t const >         Reference_size_const;
typedef  Reference< float const >               Reference_float_const;
typedef  Reference< double const >              Reference_double_const;
typedef  Reference< char const >                Reference_char_const;
typedef  Reference< signed char const >         Reference_schar_const;
typedef  Reference< std::string const >         Reference_string_const;

} // ObjexxFCL

#endif // ObjexxFCL_Reference_fwd_hh_INCLUDED
