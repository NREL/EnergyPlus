#ifndef ObjexxFCL_Array5_fwd_hh_INCLUDED
#define ObjexxFCL_Array5_fwd_hh_INCLUDED

// Array5 Forward Declarations
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
template< typename > class Array5;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array5< bool >                Array5_bool;
typedef  Array5< byte >                Array5_byte;
typedef  Array5< sbyte >               Array5_sbyte;
typedef  Array5< ubyte >               Array5_ubyte;
typedef  Array5< short int >           Array5_short;
typedef  Array5< int >                 Array5_int;
typedef  Array5< long int >            Array5_long;
typedef  Array5< unsigned short int >  Array5_ushort;
typedef  Array5< unsigned int >        Array5_uint;
typedef  Array5< unsigned long int >   Array5_ulong;
typedef  Array5< std::size_t >         Array5_size;
typedef  Array5< std::int8_t >         Array5_int8;
typedef  Array5< std::int16_t >        Array5_int16;
typedef  Array5< std::int32_t >        Array5_int32;
typedef  Array5< std::int64_t >        Array5_int64;
typedef  Array5< std::uint8_t >        Array5_uint8;
typedef  Array5< std::uint16_t >       Array5_uint16;
typedef  Array5< std::uint32_t >       Array5_uint32;
typedef  Array5< std::uint64_t >       Array5_uint64;
typedef  Array5< float >               Array5_float;
typedef  Array5< double >              Array5_double;
typedef  Array5< long double >         Array5_longdouble;
typedef  Array5< char >                Array5_char;
typedef  Array5< unsigned char >       Array5_uchar;
typedef  Array5< signed char >         Array5_schar;
typedef  Array5< std::string >         Array5_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array5_fwd_hh_INCLUDED
