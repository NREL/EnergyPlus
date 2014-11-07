#ifndef ObjexxFCL_FArrayRS_fwd_hh_INCLUDED
#define ObjexxFCL_FArrayRS_fwd_hh_INCLUDED

// FArrayRS Forward Declarations
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2014 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// C++ Headers
#include <cstddef>
#include <cstdint>
#include <string>

namespace ObjexxFCL {

// Forward
template< typename, int > class FArrayRS;
class byte;
class ubyte;
class Fstring;

// Types
typedef  byte  sbyte;
template< int Rank > using FArrayRS_bool       = FArrayRS< bool, Rank >;
template< int Rank > using FArrayRS_byte       = FArrayRS< byte, Rank >;
template< int Rank > using FArrayRS_sbyte      = FArrayRS< sbyte, Rank >;
template< int Rank > using FArrayRS_ubyte      = FArrayRS< ubyte, Rank >;
template< int Rank > using FArrayRS_short      = FArrayRS< short int, Rank >;
template< int Rank > using FArrayRS_int        = FArrayRS< int, Rank >;
template< int Rank > using FArrayRS_long       = FArrayRS< long int, Rank >;
template< int Rank > using FArrayRS_ushort     = FArrayRS< unsigned short int, Rank >;
template< int Rank > using FArrayRS_uint       = FArrayRS< unsigned int, Rank >;
template< int Rank > using FArrayRS_ulong      = FArrayRS< unsigned long int, Rank >;
template< int Rank > using FArrayRS_size       = FArrayRS< std::size_t, Rank >;
template< int Rank > using FArrayRS_int8       = FArrayRS< std::int8_t, Rank >;
template< int Rank > using FArrayRS_int16      = FArrayRS< std::int16_t, Rank >;
template< int Rank > using FArrayRS_int32      = FArrayRS< std::int32_t, Rank >;
template< int Rank > using FArrayRS_int64      = FArrayRS< std::int64_t, Rank >;
template< int Rank > using FArrayRS_uint8      = FArrayRS< std::uint8_t, Rank >;
template< int Rank > using FArrayRS_uint16     = FArrayRS< std::uint16_t, Rank >;
template< int Rank > using FArrayRS_uint32     = FArrayRS< std::uint32_t, Rank >;
template< int Rank > using FArrayRS_uint64     = FArrayRS< std::uint64_t, Rank >;
template< int Rank > using FArrayRS_float      = FArrayRS< float, Rank >;
template< int Rank > using FArrayRS_double     = FArrayRS< double, Rank >;
template< int Rank > using FArrayRS_longdouble = FArrayRS< long double, Rank >;
template< int Rank > using FArrayRS_char       = FArrayRS< char, Rank >;
template< int Rank > using FArrayRS_uchar      = FArrayRS< unsigned char, Rank >;
template< int Rank > using FArrayRS_schar      = FArrayRS< signed char, Rank >;
template< int Rank > using FArrayRS_string     = FArrayRS< std::string, Rank >;
template< int Rank > using FArrayRS_Fstring    = FArrayRS< Fstring, Rank >;

} // ObjexxFCL

#endif // ObjexxFCL_FArrayRS_fwd_hh_INCLUDED
