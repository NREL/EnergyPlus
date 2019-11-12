#ifndef ObjexxFCL_ArrayS_fwd_hh_INCLUDED
#define ObjexxFCL_ArrayS_fwd_hh_INCLUDED

// ArrayS Forward Declarations
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// C++ Headers
#include <cstddef>
#include <cstdint>
#include <string>

namespace ObjexxFCL {

// Forward
template< typename > class ArrayS;
class byte;
class ubyte;

// Types
using sbyte = byte;
using ArrayS_bool = ArrayS< bool >;
using ArrayS_byte = ArrayS< byte >;
using ArrayS_sbyte = ArrayS< sbyte >;
using ArrayS_ubyte = ArrayS< ubyte >;
using ArrayS_short = ArrayS< short int >;
using ArrayS_int = ArrayS< int >;
using ArrayS_long = ArrayS< long int >;
using ArrayS_ushort = ArrayS< unsigned short int >;
using ArrayS_uint = ArrayS< unsigned int >;
using ArrayS_ulong = ArrayS< unsigned long int >;
using ArrayS_size = ArrayS< std::size_t >;
using ArrayS_int8 = ArrayS< std::int8_t >;
using ArrayS_int16 = ArrayS< std::int16_t >;
using ArrayS_int32 = ArrayS< std::int32_t >;
using ArrayS_int64 = ArrayS< std::int64_t >;
using ArrayS_uint8 = ArrayS< std::uint8_t >;
using ArrayS_uint16 = ArrayS< std::uint16_t >;
using ArrayS_uint32 = ArrayS< std::uint32_t >;
using ArrayS_uint64 = ArrayS< std::uint64_t >;
using ArrayS_float = ArrayS< float >;
using ArrayS_double = ArrayS< double >;
using ArrayS_longdouble = ArrayS< long double >;
using ArrayS_char = ArrayS< char >;
using ArrayS_uchar = ArrayS< unsigned char >;
using ArrayS_schar = ArrayS< signed char >;
using ArrayS_string = ArrayS< std::string >;

} // ObjexxFCL

#endif // ObjexxFCL_ArrayS_fwd_hh_INCLUDED
