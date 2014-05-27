#ifndef ObjexxFCL_FArray1_io_hh_INCLUDED
#define ObjexxFCL_FArray1_io_hh_INCLUDED

// FArray1 Input/Output Functions
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
#include <ObjexxFCL/FArray1.hh>
#include <ObjexxFCL/FArray.io.hh>

namespace ObjexxFCL {

// stream >> FArray1
template< typename T >
std::istream &
operator >>( std::istream & stream, FArray1< T > & a )
{
	if ( ( stream ) && ( a.size() > 0u ) ) { // Read array from stream
		for ( int i = a.l(), e = a.u(); i <= e; ++i ) {
			stream >> a( i );
			if ( ! stream ) break;
		}
	}
	return stream;
}

// stream << FArray1
template< typename T >
std::ostream &
operator <<( std::ostream & stream, FArray1< T > const & a )
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
		for ( int i = a.l(), e = a.u(); i < e; ++i ) {
			stream << setw( w ) << a( i ) << ' ';
			if ( ! stream ) break;
		} stream << setw( w ) << a( a.u() );

		// Restore previous stream state
		stream.precision( old_precision );
		stream.flags( old_flags );

	}

	return stream;
}

} // ObjexxFCL

#endif // ObjexxFCL_FArray1_io_hh_INCLUDED
