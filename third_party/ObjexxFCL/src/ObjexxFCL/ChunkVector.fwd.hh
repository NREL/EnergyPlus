#ifndef ObjexxFCL_ChunkVector_fwd_hh_INCLUDED
#define ObjexxFCL_ChunkVector_fwd_hh_INCLUDED

// ChunkVector Forward Declarations
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
template< typename > class ChunkVector;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
typedef  ChunkVector< bool >                ChunkVector_bool;
typedef  ChunkVector< byte >                ChunkVector_byte;
typedef  ChunkVector< sbyte >               ChunkVector_sbyte;
typedef  ChunkVector< ubyte >               ChunkVector_ubyte;
typedef  ChunkVector< short int >           ChunkVector_short;
typedef  ChunkVector< int >                 ChunkVector_int;
typedef  ChunkVector< long int >            ChunkVector_long;
typedef  ChunkVector< unsigned short int >  ChunkVector_ushort;
typedef  ChunkVector< unsigned int >        ChunkVector_uint;
typedef  ChunkVector< unsigned long int >   ChunkVector_ulong;
typedef  ChunkVector< std::size_t >         ChunkVector_size;
typedef  ChunkVector< std::int8_t >         ChunkVector_int8;
typedef  ChunkVector< std::int16_t >        ChunkVector_int16;
typedef  ChunkVector< std::int32_t >        ChunkVector_int32;
typedef  ChunkVector< std::int64_t >        ChunkVector_int64;
typedef  ChunkVector< std::uint8_t >        ChunkVector_uint8;
typedef  ChunkVector< std::uint16_t >       ChunkVector_uint16;
typedef  ChunkVector< std::uint32_t >       ChunkVector_uint32;
typedef  ChunkVector< std::uint64_t >       ChunkVector_uint64;
typedef  ChunkVector< float >               ChunkVector_float;
typedef  ChunkVector< double >              ChunkVector_double;
typedef  ChunkVector< long double >         ChunkVector_longdouble;
typedef  ChunkVector< char >                ChunkVector_char;
typedef  ChunkVector< unsigned char >       ChunkVector_uchar;
typedef  ChunkVector< signed char >         ChunkVector_schar;
typedef  ChunkVector< std::string >         ChunkVector_string;

} // ObjexxFCL

#endif // ObjexxFCL_ChunkVector_fwd_hh_INCLUDED
