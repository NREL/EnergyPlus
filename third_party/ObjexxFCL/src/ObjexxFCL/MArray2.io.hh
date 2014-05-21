#ifndef ObjexxFCL_MArray2_io_hh_INCLUDED
#define ObjexxFCL_MArray2_io_hh_INCLUDED

// MArray2 Input/Output Functions
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
#include <ObjexxFCL/MArray2.hh>
#include <ObjexxFCL/MArray.io.hh>

namespace ObjexxFCL {

// stream >> MArray2
template< class A, typename T >
std::istream &
operator >>( std::istream & stream, MArray2< A, T > & a )
{
	if ( ( stream ) && ( a.size() > 0u ) ) { // Read array from stream in row-major order
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				stream >> a( i1, i2 );
				if ( ! stream ) break;
			} if ( ! stream ) break;
		}
	}
	return stream;
}

// stream << MArray2
template< class A, typename T >
std::ostream &
operator <<( std::ostream & stream, MArray2< A, T > const & a )
{
	if ( ( stream ) && ( a.size() > 0u ) ) { // Write array to stream in row-major order

		// Types
		using std::setw;
		typedef  TypeTraits< T >  Traits;

		// Save current stream state and set persistent state
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision() ) );
		stream << std::right << std::showpoint << std::uppercase;

		// Output array to stream
		int const w( Traits::iwidth() );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 < e2; ++i2 ) {
				stream << setw( w ) << a( i1, i2 ) << ' ';
				if ( ! stream ) break;
			} stream << setw( w ) << a( i1, a.u2() ) << '\n';
		}

		// Restore previous stream state
		stream.precision( old_precision );
		stream.flags( old_flags );

	}

	return stream;
}

// Read an MArray2 from a Binary File
template< class A, typename T >
std::istream &
read_binary( std::istream & stream, MArray2< A, T > & a )
{
	std::size_t const n( a.size() );
	if ( ( stream ) && ( n > 0u ) ) { // Read array from stream in column-major (Fortran) order
		std::size_t const type_size( sizeof( T ) / sizeof( std::istream::char_type ) );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( stream ) {
					stream.read( ( std::istream::char_type * )&a( i1, i2 ), type_size );
				} else {
					break;
				}
			}
		}
	}
	return stream;
}

// Write an MArray2 to a Binary File
template< class A, typename T >
std::ostream &
write_binary( std::ostream & stream, MArray2< A, T > const & a )
{
	std::size_t const n( a.size() );
	if ( ( stream ) && ( n > 0u ) ) { // Write array to stream in column-major (Fortran) order
		std::size_t const type_size( sizeof( T ) / sizeof( std::ostream::char_type ) );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( stream ) {
					stream.write( ( std::ostream::char_type const * )&a( i1, i2 ), type_size );
				} else {
					break;
				}
			}
		}
	}
	return stream;
}

namespace fmt {

// List-Directed Format: MArray2
template< class A, typename T >
inline
std::string
LD( MArray2< A, T > const & a )
{
	std::string s;
	std::size_t const n( a.size() );
	if ( n > 0u ) {
		s.reserve( n * TypeTraits< T >::width() );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				s.append( fmt::LD( a( i1, i2 ) ) );
			}
		}
	}
	return s;
}

} // fmt

} // ObjexxFCL

#endif // ObjexxFCL_MArray2_io_hh_INCLUDED
