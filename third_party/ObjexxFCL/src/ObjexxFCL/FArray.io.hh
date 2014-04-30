#ifndef ObjexxFCL_FArray_io_hh_INCLUDED
#define ObjexxFCL_FArray_io_hh_INCLUDED

// FArray Input/Output Functions
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

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.hh>
#include <ObjexxFCL/fmt.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <cstddef>
#include <iomanip>
#include <istream>
#include <ostream>

namespace ObjexxFCL {

// Read an FArray from a Binary File
template< typename T >
std::istream &
read_binary( std::istream & stream, FArray< T > & a )
{
	std::size_t const n( a.size() );
	if ( ( stream ) && ( n > 0u ) ) { // Read array from stream in column-major (Fortran) order
		std::size_t const type_size( sizeof( T ) / sizeof( std::istream::char_type ) );
		for ( std::size_t i = 0; i < n; ++i ) {
			if ( stream ) {
				stream.read( ( std::istream::char_type * )&a[ i ], type_size );
			} else {
				break;
			}
		}
	}
	return stream;
}

// Write an FArray to a Binary File
template< typename T >
std::ostream &
write_binary( std::ostream & stream, FArray< T > const & a )
{
	std::size_t const n( a.size() );
	if ( ( stream ) && ( n > 0u ) ) { // Write array to stream in column-major (Fortran) order
		std::size_t const type_size( sizeof( T ) / sizeof( std::ostream::char_type ) );
		for ( std::size_t i = 0; i < n; ++i ) {
			if ( stream ) {
				stream.write( ( std::ostream::char_type const * )&a[ i ], type_size );
			} else {
				break;
			}
		}
	}
	return stream;
}

namespace fmt {

// List-Directed Format: FArray
template< typename T >
inline
std::string
LD( FArray< T > const & a )
{
	std::string s;
	std::size_t const n( a.size() );
	if ( n > 0u ) {
		s.reserve( n * TypeTraits< T >::width() );
		for ( std::size_t i = 0; i < n; ++i ) {
			s.append( fmt::LD( a[ i ] ) );
		}
	}
	return s;
}

} // fmt

} // ObjexxFCL

#endif // ObjexxFCL_FArray_io_hh_INCLUDED
