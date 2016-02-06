// ObjexxFCL::CArrayP Unit Tests
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
#include <ObjexxFCL/CArrayP.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( CArrayPTest, Construction )
{
	{ // Copy constructor and assignment
		CArrayP_int v( 10u, 22 );
		CArrayP_int w( v );
		EXPECT_EQ( v, w );
		EXPECT_EQ( w, v );
		w += 1;
		v = w;
		EXPECT_EQ( v, w );
		EXPECT_EQ( w, v );
	}

	{ // Copy constructor and assignment template
		CArrayP_int v( 10u, 22 );
		CArrayP_float f( v );
		EXPECT_EQ( CArrayP_float( 10u, 22.0f ), f );
		v += 1;
		EXPECT_EQ( CArrayP_int( 10u, 23 ), v );
		f = v;
		EXPECT_EQ( CArrayP_float( 10u, 23.0f ), f );
	}

	{ // Size constructor
		CArrayP_int v( 10 ); // Uninitialized
		EXPECT_EQ( v.size(), 10u );
	}

	{ // Size + value constructor
		CArrayP_int v( 10u, 22 );
		EXPECT_EQ( v.size(), 10u );
		EXPECT_EQ( v[ 0u ], 22 );
		EXPECT_EQ( v[ 9u ], 22 );
	}
}

TEST( CArrayPTest, Assignment )
{
	CArrayP_int v( 10u, 22 );
	v += 2;
	EXPECT_EQ( CArrayP_int( 10u, 24 ), v );
	v -= 2;
	EXPECT_EQ( CArrayP_int( 10u, 22 ), v );
	v *= 2;
	EXPECT_EQ( CArrayP_int( 10u, 44 ), v );
	v /= 2;
	EXPECT_EQ( CArrayP_int( 10u, 22 ), v );
	v = CArrayP_int( 20u, 33 );
	EXPECT_EQ( CArrayP_int( 20u, 33 ), v );
	v += v;
	EXPECT_EQ( CArrayP_int( 20u, 66 ), v );
	v -= v;
	EXPECT_EQ( CArrayP_int( 20u, 0 ), v );
	v = 55;
	EXPECT_EQ( CArrayP_int( 20u, 55 ), v );
}

TEST( CArrayPTest, Subscripting )
{
	CArrayP_int v( 10u, 22 );
	v[ 3u ] = 33;
	EXPECT_EQ( 22, v[ 0u ] );
	EXPECT_EQ( 33, v[ 3u ] );
	EXPECT_EQ( 22, v[ 9u ] );
	EXPECT_EQ( 33, v( 4 ) );
	v( 5 ) = 44;
	EXPECT_EQ( 44, v( 5 ) );
}

TEST( CArrayPTest, Functions )
{
	CArrayP_int u{ 1, 2, 3 };
	CArrayP_int v{ 2, 3, 4 };
	EXPECT_EQ( 14, magnitude_squared( u ) );
	EXPECT_EQ( 3, distance_squared( u, v ) );
	EXPECT_EQ( 20, dot( u, v ) );
}

TEST( CArrayPTest, Swap )
{
	CArrayP_int a( 10u, 22 ), A( a );
	CArrayP_int b( 8u, 33 ), B( b );
	a.swap( b );
	EXPECT_EQ( B, a );
	EXPECT_EQ( A, b );
	b.swap( a );
	EXPECT_EQ( A, a );
	EXPECT_EQ( B, b );
	swap( a, b );
	EXPECT_EQ( B, a );
	EXPECT_EQ( A, b );
}

TEST( CArrayPTest, Proxy )
{
	{ // Non-const proxy
		CArrayP_int v( 10u, 22 ); // Real array
		CArrayP_int w( CArrayP_int::Proxy( v ) ); // Proxy for non-const
		++w[ 0 ];
		EXPECT_EQ( 23, w[ 0 ] );
		EXPECT_EQ( v, w );
	}

	{ // Const proxy
		CArrayP_int const v( 10u, 22 ); // Real array
		CArrayP_int w( CArrayP_int::Proxy( v ) ); // Proxy for const
		EXPECT_EQ( 10u, w.size() );
		EXPECT_EQ( v, w );
		CArrayP_int const x( CArrayP_int::Proxy( v ) ); // Const proxy for const
		EXPECT_EQ( 22, x[ 5 ] ); // OK: Uses const subscript lookup
	}
}
