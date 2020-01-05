#ifndef ObjexxFCL_ChunkVector_fwd_hh_INCLUDED
#define ObjexxFCL_ChunkVector_fwd_hh_INCLUDED

// ChunkVector Forward Declarations
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2020 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

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
using sbyte = byte;
using ChunkVector_bool = ChunkVector< bool >;
using ChunkVector_byte = ChunkVector< byte >;
using ChunkVector_sbyte = ChunkVector< sbyte >;
using ChunkVector_ubyte = ChunkVector< ubyte >;
using ChunkVector_short = ChunkVector< short int >;
using ChunkVector_int = ChunkVector< int >;
using ChunkVector_long = ChunkVector< long int >;
using ChunkVector_ushort = ChunkVector< unsigned short int >;
using ChunkVector_uint = ChunkVector< unsigned int >;
using ChunkVector_ulong = ChunkVector< unsigned long int >;
using ChunkVector_size = ChunkVector< std::size_t >;
using ChunkVector_int8 = ChunkVector< std::int8_t >;
using ChunkVector_int16 = ChunkVector< std::int16_t >;
using ChunkVector_int32 = ChunkVector< std::int32_t >;
using ChunkVector_int64 = ChunkVector< std::int64_t >;
using ChunkVector_uint8 = ChunkVector< std::uint8_t >;
using ChunkVector_uint16 = ChunkVector< std::uint16_t >;
using ChunkVector_uint32 = ChunkVector< std::uint32_t >;
using ChunkVector_uint64 = ChunkVector< std::uint64_t >;
using ChunkVector_float = ChunkVector< float >;
using ChunkVector_double = ChunkVector< double >;
using ChunkVector_longdouble = ChunkVector< long double >;
using ChunkVector_char = ChunkVector< char >;
using ChunkVector_uchar = ChunkVector< unsigned char >;
using ChunkVector_schar = ChunkVector< signed char >;
using ChunkVector_string = ChunkVector< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_ChunkVector_fwd_hh_INCLUDED
