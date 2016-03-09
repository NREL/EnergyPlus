#ifndef ObjexxFCL_ArrayRS_fwd_hh_INCLUDED
#define ObjexxFCL_ArrayRS_fwd_hh_INCLUDED

// ArrayRS Forward Declarations
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
template< typename, int > class ArrayRS;
class byte;
class ubyte;

// Types
typedef  byte  sbyte;
template< int Rank > using ArrayRS_bool       = ArrayRS< bool, Rank >;
template< int Rank > using ArrayRS_byte       = ArrayRS< byte, Rank >;
template< int Rank > using ArrayRS_sbyte      = ArrayRS< sbyte, Rank >;
template< int Rank > using ArrayRS_ubyte      = ArrayRS< ubyte, Rank >;
template< int Rank > using ArrayRS_short      = ArrayRS< short int, Rank >;
template< int Rank > using ArrayRS_int        = ArrayRS< int, Rank >;
template< int Rank > using ArrayRS_long       = ArrayRS< long int, Rank >;
template< int Rank > using ArrayRS_ushort     = ArrayRS< unsigned short int, Rank >;
template< int Rank > using ArrayRS_uint       = ArrayRS< unsigned int, Rank >;
template< int Rank > using ArrayRS_ulong      = ArrayRS< unsigned long int, Rank >;
template< int Rank > using ArrayRS_size       = ArrayRS< std::size_t, Rank >;
template< int Rank > using ArrayRS_int8       = ArrayRS< std::int8_t, Rank >;
template< int Rank > using ArrayRS_int16      = ArrayRS< std::int16_t, Rank >;
template< int Rank > using ArrayRS_int32      = ArrayRS< std::int32_t, Rank >;
template< int Rank > using ArrayRS_int64      = ArrayRS< std::int64_t, Rank >;
template< int Rank > using ArrayRS_uint8      = ArrayRS< std::uint8_t, Rank >;
template< int Rank > using ArrayRS_uint16     = ArrayRS< std::uint16_t, Rank >;
template< int Rank > using ArrayRS_uint32     = ArrayRS< std::uint32_t, Rank >;
template< int Rank > using ArrayRS_uint64     = ArrayRS< std::uint64_t, Rank >;
template< int Rank > using ArrayRS_float      = ArrayRS< float, Rank >;
template< int Rank > using ArrayRS_double     = ArrayRS< double, Rank >;
template< int Rank > using ArrayRS_longdouble = ArrayRS< long double, Rank >;
template< int Rank > using ArrayRS_char       = ArrayRS< char, Rank >;
template< int Rank > using ArrayRS_uchar      = ArrayRS< unsigned char, Rank >;
template< int Rank > using ArrayRS_schar      = ArrayRS< signed char, Rank >;
template< int Rank > using ArrayRS_string     = ArrayRS< std::string, Rank >;

} // ObjexxFCL

#endif // ObjexxFCL_ArrayRS_fwd_hh_INCLUDED
