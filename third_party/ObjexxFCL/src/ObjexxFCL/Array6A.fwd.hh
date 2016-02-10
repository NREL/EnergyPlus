#ifndef ObjexxFCL_Array6A_fwd_hh_INCLUDED
#define ObjexxFCL_Array6A_fwd_hh_INCLUDED

// Array6A Forward Declarations
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
template< typename > class Array6A;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array6A< bool >                Array6A_bool;
typedef  Array6A< byte >                Array6A_byte;
typedef  Array6A< sbyte >               Array6A_sbyte;
typedef  Array6A< ubyte >               Array6A_ubyte;
typedef  Array6A< short int >           Array6A_short;
typedef  Array6A< int >                 Array6A_int;
typedef  Array6A< long int >            Array6A_long;
typedef  Array6A< unsigned short int >  Array6A_ushort;
typedef  Array6A< unsigned int >        Array6A_uint;
typedef  Array6A< unsigned long int >   Array6A_ulong;
typedef  Array6A< std::size_t >         Array6A_size;
typedef  Array6A< std::int8_t >         Array6A_int8;
typedef  Array6A< std::int16_t >        Array6A_int16;
typedef  Array6A< std::int32_t >        Array6A_int32;
typedef  Array6A< std::int64_t >        Array6A_int64;
typedef  Array6A< std::uint8_t >        Array6A_uint8;
typedef  Array6A< std::uint16_t >       Array6A_uint16;
typedef  Array6A< std::uint32_t >       Array6A_uint32;
typedef  Array6A< std::uint64_t >       Array6A_uint64;
typedef  Array6A< float >               Array6A_float;
typedef  Array6A< double >              Array6A_double;
typedef  Array6A< long double >         Array6A_longdouble;
typedef  Array6A< char >                Array6A_char;
typedef  Array6A< unsigned char >       Array6A_uchar;
typedef  Array6A< signed char >         Array6A_schar;
typedef  Array6A< std::string >         Array6A_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array6A_fwd_hh_INCLUDED
