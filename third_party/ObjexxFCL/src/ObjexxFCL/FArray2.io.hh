#ifndef ObjexxFCL_FArray2_io_hh_INCLUDED
#define ObjexxFCL_FArray2_io_hh_INCLUDED

// FArray2 Input/Output Functions
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
#include <ObjexxFCL/FArray2.hh>
#include <ObjexxFCL/FArray.io.hh>

namespace ObjexxFCL {

// stream >> FArray2
template< typename T >
std::istream &
operator >>( std::istream & stream, FArray2< T > & a )
{
	if ( ( stream ) && ( a.size() > 0u ) ) { // Read array from stream in row-major order
		for ( int i1 = a.l1(), e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = a.l2(), e2 = a.u2(); i2 <= e2; ++i2 ) {
				stream >> a( i1, i2 );
				if ( ! stream ) break;
			} if ( ! stream ) break;
		}
	}
	return stream;
}

// stream << FArray2
template< typename T >
std::ostream &
operator <<( std::ostream & stream, FArray2< T > const & a )
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
		for ( int i1 = a.l1(), e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = a.l2(), e2 = a.u2(); i2 < e2; ++i2 ) {
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

} // ObjexxFCL

#endif // ObjexxFCL_FArray2_io_hh_INCLUDED
