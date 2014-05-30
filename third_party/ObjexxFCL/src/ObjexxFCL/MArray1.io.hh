#ifndef ObjexxFCL_MArray1_io_hh_INCLUDED
#define ObjexxFCL_MArray1_io_hh_INCLUDED

// MArray1 Input/Output Functions
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
#include <ObjexxFCL/MArray1.hh>
#include <ObjexxFCL/MArray.io.hh>

namespace ObjexxFCL {

// stream >> MArray1
template< class A, typename T >
std::istream &
operator >>( std::istream & stream, MArray1< A, T > & a )
{
	if ( ( stream ) && ( a.size() > 0u ) ) { // Read array from stream
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			stream >> a( i );
			if ( ! stream ) break;
		}
	}
	return stream;
}

// stream << MArray1
template< class A, typename T >
std::ostream &
operator <<( std::ostream & stream, MArray1< A, T > const & a )
{
	if ( ( stream ) && ( a.size() > 0u ) ) { // Write array to stream

		// Types
		using std::setw;
		typedef  TypeTraits< T >  Traits;

		// Save current stream state and set persistent state
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision() ) );
		stream << std::right << std::showpoint << std::uppercase;

		// Output array to stream
		int const w( Traits::iwidth() );
		for ( int i = 1, e = a.u(); i < e; ++i ) {
			stream << setw( w ) << a( i ) << ' ';
			if ( ! stream ) break;
		} stream << setw( w ) << a( a.u() );

		// Restore previous stream state
		stream.precision( old_precision );
		stream.flags( old_flags );

	}

	return stream;
}

// Read an MArray1 from a Binary File
template< class A, typename T >
std::istream &
read_binary( std::istream & stream, MArray1< A, T > & a )
{
	std::size_t const n( a.size() );
	if ( ( stream ) && ( n > 0u ) ) { // Read array from stream
		std::size_t const type_size( sizeof( T ) / sizeof( std::istream::char_type ) );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( stream ) {
				stream.read( ( std::istream::char_type * )&a( i ), type_size );
			} else {
				break;
			}
		}
	}
	return stream;
}

// Write an MArray1 to a Binary File
template< class A, typename T >
std::ostream &
write_binary( std::ostream & stream, MArray1< A, T > const & a )
{
	std::size_t const n( a.size() );
	if ( ( stream ) && ( n > 0u ) ) { // Write array to stream
		std::size_t const type_size( sizeof( T ) / sizeof( std::ostream::char_type ) );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( stream ) {
				stream.write( ( std::ostream::char_type const * )&a( i ), type_size );
			} else {
				break;
			}
		}
	}
	return stream;
}

namespace fmt {

// List-Directed Format: MArray1
template< class A, typename T >
inline
std::string
LD( MArray1< A, T > const & a )
{
	std::string s;
	std::size_t const n( a.size() );
	if ( n > 0u ) {
		s.reserve( n * TypeTraits< T >::width() );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			s.append( fmt::LD( a( i ) ) );
		}
	}
	return s;
}

} // fmt

} // ObjexxFCL

#endif // ObjexxFCL_MArray1_io_hh_INCLUDED
