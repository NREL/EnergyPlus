#ifndef ObjexxFCL_Array4S_fwd_hh_INCLUDED
#define ObjexxFCL_Array4S_fwd_hh_INCLUDED

// Array4S Forward Declarations
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
template< typename > class Array4S;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  Array4S< bool >                Array4S_bool;
typedef  Array4S< byte >                Array4S_byte;
typedef  Array4S< sbyte >               Array4S_sbyte;
typedef  Array4S< ubyte >               Array4S_ubyte;
typedef  Array4S< short int >           Array4S_short;
typedef  Array4S< int >                 Array4S_int;
typedef  Array4S< long int >            Array4S_long;
typedef  Array4S< unsigned short int >  Array4S_ushort;
typedef  Array4S< unsigned int >        Array4S_uint;
typedef  Array4S< unsigned long int >   Array4S_ulong;
typedef  Array4S< std::size_t >         Array4S_size;
typedef  Array4S< std::int8_t >         Array4S_int8;
typedef  Array4S< std::int16_t >        Array4S_int16;
typedef  Array4S< std::int32_t >        Array4S_int32;
typedef  Array4S< std::int64_t >        Array4S_int64;
typedef  Array4S< std::uint8_t >        Array4S_uint8;
typedef  Array4S< std::uint16_t >       Array4S_uint16;
typedef  Array4S< std::uint32_t >       Array4S_uint32;
typedef  Array4S< std::uint64_t >       Array4S_uint64;
typedef  Array4S< float >               Array4S_float;
typedef  Array4S< double >              Array4S_double;
typedef  Array4S< long double >         Array4S_longdouble;
typedef  Array4S< char >                Array4S_char;
typedef  Array4S< unsigned char >       Array4S_uchar;
typedef  Array4S< signed char >         Array4S_schar;
typedef  Array4S< std::string >         Array4S_string;

} // ObjexxFCL

#endif // ObjexxFCL_Array4S_fwd_hh_INCLUDED
