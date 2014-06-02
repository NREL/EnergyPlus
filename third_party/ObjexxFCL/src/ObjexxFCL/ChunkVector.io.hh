#ifndef ObjexxFCL_ChunkVector_io_hh_INCLUDED
#define ObjexxFCL_ChunkVector_io_hh_INCLUDED

// ChunkVector.io: Chunk-Contiguous Vector Input/Output Functions
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
#include <ObjexxFCL/ChunkVector.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <iomanip>
#include <ostream>

namespace ObjexxFCL {

// stream << ChunkVector
template< typename T >
std::ostream &
operator <<( std::ostream & stream, ChunkVector< T > const & v )
{
	if ( v.empty() ) return stream;

	if ( stream ) {

		// Types
		using std::setw;
		typedef  TypeTraits< T >  Traits;
		typedef  typename ChunkVector< T >::size_type  size_type;

		// Save current stream state and set persistent state
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision() ) );
		stream << std::right << std::showpoint << std::uppercase;

		// Output array to stream
		size_type const e( v.size() - 1 );
		int const w( Traits::iwidth() );
		for ( size_type i = 0; i < e; ++i ) {
			stream << setw( w ) << v[ i ] << ' ';
		} stream << setw( w ) << v[ e ];

		// Restore previous stream state
		stream.precision( old_precision );
		stream.flags( old_flags );

	}

	return stream;
}

} // ObjexxFCL

#endif // ObjexxFCL_ChunkVector_io_hh_INCLUDED
