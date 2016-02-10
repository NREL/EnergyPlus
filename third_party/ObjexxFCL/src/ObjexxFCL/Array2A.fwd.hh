#ifndef ObjexxFCL_Array2A_fwd_hh_INCLUDED
#define ObjexxFCL_Array2A_fwd_hh_INCLUDED

// Array2A Forward Declarations
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
template< typename > class Array2A;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array2A< bool >                Array2A_bool;
typedef  Array2A< byte >                Array2A_byte;
typedef  Array2A< sbyte >               Array2A_sbyte;
typedef  Array2A< ubyte >               Array2A_ubyte;
typedef  Array2A< short int >           Array2A_short;
typedef  Array2A< int >                 Array2A_int;
typedef  Array2A< long int >            Array2A_long;
typedef  Array2A< unsigned short int >  Array2A_ushort;
typedef  Array2A< unsigned int >        Array2A_uint;
typedef  Array2A< unsigned long int >   Array2A_ulong;
typedef  Array2A< std::size_t >         Array2A_size;
typedef  Array2A< std::int8_t >         Array2A_int8;
typedef  Array2A< std::int16_t >        Array2A_int16;
typedef  Array2A< std::int32_t >        Array2A_int32;
typedef  Array2A< std::int64_t >        Array2A_int64;
typedef  Array2A< std::uint8_t >        Array2A_uint8;
typedef  Array2A< std::uint16_t >       Array2A_uint16;
typedef  Array2A< std::uint32_t >       Array2A_uint32;
typedef  Array2A< std::uint64_t >       Array2A_uint64;
typedef  Array2A< float >               Array2A_float;
typedef  Array2A< double >              Array2A_double;
typedef  Array2A< long double >         Array2A_longdouble;
typedef  Array2A< char >                Array2A_char;
typedef  Array2A< unsigned char >       Array2A_uchar;
typedef  Array2A< signed char >         Array2A_schar;
typedef  Array2A< std::string >         Array2A_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array2A_fwd_hh_INCLUDED
