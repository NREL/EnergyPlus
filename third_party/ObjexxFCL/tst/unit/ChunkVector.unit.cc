// ObjexxFCL::ChunkVector Unit Tests
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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/ChunkVector.hh>
#include <ObjexxFCL/TypeTraits.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <limits>
#include <iomanip>
#include <ostream>
#include <vector>

// Stream << vector
template< typename T >
std::ostream &
operator <<( std::ostream & stream, std::vector< T > const & v )
{
	using std::setw;
	typedef  ObjexxFCL::TypeTraits< T >  Traits;
	typedef  typename std::vector< T >::size_type  size_type;
	if ( stream && ( ! v.empty() ) ) {
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision ) );
		stream << std::right << std::showpoint << std::uppercase;
		size_type const e( v.size() - 1 );
		int const w( Traits::iwidth );
		for ( size_type i = 0; i < e; ++i ) {
			stream << setw( w ) << v[ i ] << ' ';
		} stream << setw( w ) << v[ e ];
		stream.precision( old_precision );
		stream.flags( old_flags );
	}
	return stream;
}

using namespace ObjexxFCL;

typedef  std::vector< int >  vector_int;
typedef  std::vector< float >  vector_float;

TEST( ChunkVectorTest, Construction )
{
	{ // Copy constructor and assignment
		ChunkVector_int v( 10, ChunkExponent( 2 ), 22 );
		ChunkVector_int w( v );
		EXPECT_EQ( v, w );
		EXPECT_EQ( w, v );
		w += 1;
		v = w;
		EXPECT_EQ( v, w );
		EXPECT_EQ( w, v );
	}

	{ // Copy constructor and assignment template
		ChunkVector_int v( 10, 2u, 22 );
		ChunkVector_float f( v );
		EXPECT_EQ( ChunkVector< float >( 10, 2u, 22.0f ), f );
		EXPECT_EQ( vector_float( 10, 22.0f ), f );
		v += 1;
		EXPECT_EQ( ChunkVector< int >( 10, 2u, 23 ), v );
		EXPECT_EQ( vector_int( 10, 23 ), v );
		f = v;
		EXPECT_EQ( vector_float( 10, 23.0f ), f );
	}

	{ // Size constructor
		ChunkVector_int v( 10, 2u ); // Uninitialized
		EXPECT_EQ( 10u, v.size() );
		EXPECT_EQ( 2u, v.chunk_exponent() );
		EXPECT_EQ( 4u, v.chunk_size() );
		EXPECT_EQ( 3u, v.n_chunk() );
	}

	{ // Size + value constructor
		ChunkVector_int v( 10, 2u, 22 );
		EXPECT_EQ( 10u, v.size() );
		EXPECT_EQ( 2u, v.chunk_exponent() );
		EXPECT_EQ( 4u, v.chunk_size() );
		EXPECT_EQ( 3u, v.n_chunk() );
		EXPECT_EQ( vector_int( 10, 22 ), v );
	}

	{ // Exponent too large trimmed
		ChunkVector_int v( 10, std::numeric_limits< ChunkVector_int::size_type >::digits );
		EXPECT_EQ( ChunkVector_int::size_type( std::numeric_limits< ChunkVector_int::size_type >::digits - 1 ), v.chunk_exponent() );
	}

	{ // Iterator constructor and assignment template
		vector_float s( 10, 22.0 );
		ChunkVector_int v( s.begin(), s.end(), 2 );
		EXPECT_EQ( v, vector_int( 10, 22 ) );
		EXPECT_EQ( vector_int( 10, 22 ), v );
		s.push_back( 1.0 );
		s.push_back( 2.0 );
		s.push_back( 3.0 );
		s.push_back( 4.0 );
		s.push_back( 5.0 );
		v.assign( s.begin(), s.end(), 3 );
		ChunkVector_int w( v );
		EXPECT_EQ( v, w );
		EXPECT_EQ( w, v );
		EXPECT_EQ( v.size(), s.size() );
		EXPECT_EQ( 15u, v.size() );
	}

	{ // std::vector constructor and assignment template
		vector_float s( 10, 22.0 );
		ChunkVector_int v( s, 2u );
		EXPECT_EQ( v, ChunkVector_int( s, 2u ) );
		EXPECT_EQ( ChunkVector_int( s, 2u ), v );
		s[ 0 ] = 11.0;
		v = s;
		EXPECT_EQ( v, ChunkVector_int( s, 2u ) );
		EXPECT_EQ( ChunkVector_int( s, 2u ), v );
		s.push_back( 99.0 );
		v = s;
		EXPECT_EQ( v, ChunkVector_int( s, 2u ) );
		EXPECT_EQ( ChunkVector_int( s, 2u ), v );
		EXPECT_EQ( v.size(), s.size() );
	}
}

TEST( ChunkVectorTest, Assignment )
{
	{ // std::vector assignment
		ChunkVector_int v( 10, 2u, 22 );
		vector_int s( 10, 33 );
		EXPECT_TRUE( v != s );
		EXPECT_TRUE( s != v );
		v = s;
		EXPECT_EQ( v, s );
		EXPECT_EQ( s, v );
		v.assign( s, 3 );
		EXPECT_EQ( v, s );
		EXPECT_EQ( s, v );
	}

	{ // ChunkVector Assignment
		ChunkVector_int v( 10, 2u, 22 );
		v += 2;
		EXPECT_EQ( vector_int( 10, 24 ), v );
		v -= 2;
		EXPECT_EQ( vector_int( 10, 22 ), v );
		v *= 2;
		EXPECT_EQ( vector_int( 10, 44 ), v );
		v /= 2;
		EXPECT_EQ( vector_int( 10, 22 ), v );
		v.assign( 20, 2u, 33 );
		EXPECT_EQ( vector_int( 20, 33 ), v );
		v += v;
		EXPECT_EQ( vector_int( 20, 66 ), v );
		v -= v;
		EXPECT_EQ( vector_int( 20, 0 ), v );
		v = 55;
		EXPECT_EQ( vector_int( 20, 55 ), v );
	}
}

TEST( ChunkVectorTest, Subscripting )
{
	ChunkVector_int v( 10, 2u, 22 );
	v[ 3 ] = 33;
	EXPECT_EQ( 33, v[ 3 ] );
	EXPECT_EQ( 33, v( 4 ) );
	v( 5 ) = 44;
	EXPECT_EQ( 44, v( 5 ) );
	EXPECT_EQ( 44, v[ 4 ] );
}

TEST( ChunkVectorTest, Functions )
{
	ChunkVector_int u( std::vector< int >{ 1, 2, 3 }, 2u );
	ChunkVector_int v( std::vector< int >{ 2, 3, 4 }, 2u );
	EXPECT_EQ( 14, magnitude_squared( u ) );
	EXPECT_EQ( 3, distance_squared( u, v ) );
	EXPECT_EQ( 20, dot( u, v ) );
}

TEST( ChunkVectorTest, Swap )
{
	ChunkVector_int a( 10, 2u, 22 ), A( a );
	ChunkVector_int b( 8, 2u, 33 ), B( b );
	a.swap( b );
	EXPECT_EQ( a, B );
	EXPECT_EQ( b, A );
	b.swap( a );
	EXPECT_EQ( a, A );
	EXPECT_EQ( b, B );
	swap( a, b );
	EXPECT_EQ( a, B );
	EXPECT_EQ( b, A );
}

TEST( ChunkVectorTest, FrontBack )
{
	ChunkVector_int v( 10, 2u, 1 );
	v[ 0 ] = 11;
	v[ v.size() - 1 ] = 99;
	EXPECT_EQ( 11, v.front() );
	EXPECT_EQ( 99, v.back() );
}

TEST( ChunkVectorTest, Append )
{
	ChunkVector_int v( 10, 2u, 1 );
	ChunkVector_int const w( 10, 2u, 2 );
	vector_float const s( 10, 22.0 );
	v.append( w );
	EXPECT_EQ( 20u, v.size() );
	v.append( s );
	EXPECT_EQ( 30u, v.size() );
	EXPECT_EQ( 1, v[ 0 ] );
	EXPECT_EQ( 1, v[ 9 ] );
	EXPECT_EQ( 2, v[ 10 ] );
	EXPECT_EQ( 2, v[ 19 ] );
	EXPECT_EQ( 22, v[ 20 ] );
	EXPECT_EQ( 22, v[ 29 ] );
}

TEST( ChunkVectorTest, ResizeGrowShrink )
{
	ChunkVector_int v( 10, 2u, 1 );
	EXPECT_EQ( 10u, v.size() );
	v.push_back( 2 );
	EXPECT_TRUE( v != ChunkVector_int( 10, 2u, 1 ) );
	EXPECT_EQ( 11u, v.size() );
	EXPECT_EQ( 2, v( 11 ) );
	v.pop_back();
	EXPECT_EQ( 10u, v.size() );
	EXPECT_EQ( ChunkVector_int( 10, 2u, 1 ), v );
	v.shrink();
	EXPECT_EQ( 10u, v.size() );
	EXPECT_EQ( ChunkVector_int( 10, 2u, 1 ), v );
	v.resize( 20 );
	EXPECT_EQ( 20u, v.size() );
	EXPECT_EQ( 1, v( 10 ) );
	EXPECT_EQ( 0, v( 11 ) );
	v.resize( 10 );
	EXPECT_EQ( ChunkVector_int( 10, 2u, 1 ), v );
	v.non_preserving_reshape( 30, 3u );
}

TEST( ChunkVectorTest, ResizeFill )
{
	ChunkVector_int v( 10, 2u, 22 );
	v.resize( 20, 33 );
	EXPECT_EQ( 20u, v.size() );
	for ( ChunkVector_int::size_type i = 0; i < 10u; ++i ) {
		EXPECT_EQ( 22, v[ i ] );
	}
	for ( ChunkVector_int::size_type i = 10u; i < 20u; ++i ) {
		EXPECT_EQ( 33, v[ i ] );
	}
}

TEST( ChunkVectorTest, NonpreservingResize )
{
	ChunkVector_int v( 10, 2u, 22 );
	v.non_preserving_resize( 20U ); // Added element values are arbitrary
	EXPECT_EQ( 20u, v.size() );
}

TEST( ChunkVectorTest, ReservePushPopShrink )
{
	ChunkVector_int v( 10, 2u, 22 );
	EXPECT_EQ( 10u, v.size() );
	EXPECT_EQ( 10u, v.size() );
	for ( ChunkVector_int::size_type i = 0; i < 10u; ++i ) {
		EXPECT_EQ( 22, v[ i ] );
	}
	v.push_back( 33 );
	EXPECT_EQ( 11u, v.size() );
	v.push_back( 44 );
	EXPECT_EQ( 12u, v.size() );
	v.push_back( 55 );
	EXPECT_EQ( 13u, v.size() );
	v.pop_back();
	EXPECT_EQ( 12u, v.size() );
	v.pop_back();
	EXPECT_EQ( 11u, v.size() );
	v.shrink();
	EXPECT_EQ( 11u, v.size() );
}
