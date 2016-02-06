// ObjexxFCL::MArray Unit Tests
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
#include <ObjexxFCL/MArray.all.hh>
#include <ObjexxFCL/MArray.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

struct C
{
	C( int const m_ = 0, float const x_ = 0.0f ) :
	 m( m_ ),
	 x( x_ )
	{}

	int m;
	float x;
};

TEST( MArrayTest, Basic1D )
{
	Array1D< C > a( 5 );
	for ( int i = a.l(); i <= a.u(); ++i ) a( i ).m = i;
//	int C::*pm( &C::m ); // Can give the pointer to member a name like this also
//	MArray1< Array1D< C >, int > ma( a, pm );
	MArray1< Array1D< C >, int > ma( a, &C::m );
	EXPECT_EQ( a( 1 ).m, ma( 1 ) );
	EXPECT_EQ( a( 2 ).m, ma( 2 ) );
	EXPECT_EQ( a( 3 ).m, ma( 3 ) );
	EXPECT_EQ( a( 4 ).m, ma( 4 ) );
	EXPECT_EQ( a( 5 ).m, ma( 5 ) );
	EXPECT_EQ( 1, ma( 1 ) );
	EXPECT_EQ( 2, ma( 2 ) );
	EXPECT_EQ( 3, ma( 3 ) );
	EXPECT_EQ( 4, ma( 4 ) );
	EXPECT_EQ( 5, ma( 5 ) );
	ma += 1;
	EXPECT_EQ( 2, ma( 1 ) );
	EXPECT_EQ( 3, ma( 2 ) );
	EXPECT_EQ( 4, ma( 3 ) );
	EXPECT_EQ( 5, ma( 4 ) );
	EXPECT_EQ( 6, ma( 5 ) );
}

TEST( MArrayTest, Range1D )
{
	Array1D< C > a( Array1D< C >::IR( -3, 3 ) );
	for ( int i = a.l(); i <= a.u(); ++i ) a( i ).m = i;
	MArray1< Array1D< C >, int > ma( a, &C::m ); // MArray has 1-based indexing
	EXPECT_EQ( 1, ma.l() );
	EXPECT_EQ( 1, ma.l1() );
	EXPECT_EQ( 1, ma.l( 1 ) );
	EXPECT_EQ( 7, ma.u() );
	EXPECT_EQ( 7, ma.u1() );
	EXPECT_EQ( 7, ma.u( 1 ) );
	EXPECT_EQ( a( -3 ).m, ma( 1 ) );
	EXPECT_EQ( a( -2 ).m, ma( 2 ) );
	EXPECT_EQ( a( -1 ).m, ma( 3 ) );
	EXPECT_EQ( a( 0 ).m, ma( 4 ) );
	EXPECT_EQ( a( 1 ).m, ma( 5 ) );
	EXPECT_EQ( a( 2 ).m, ma( 6 ) );
	EXPECT_EQ( a( 3 ).m, ma( 7 ) );
	EXPECT_EQ( -3, ma( 1 ) );
	EXPECT_EQ( -2, ma( 2 ) );
	EXPECT_EQ( -1, ma( 3 ) );
	EXPECT_EQ( 0, ma( 4 ) );
	EXPECT_EQ( 1, ma( 5 ) );
	EXPECT_EQ( 2, ma( 6 ) );
	EXPECT_EQ( 3, ma( 7 ) );
}

TEST( MArrayTest, MakerFree1D )
{
	Array1D< C > a( 5 );
	for ( int i = a.l(); i <= a.u(); ++i ) a( i ).m = i;
	auto ma( make_MArray1( a, &C::m ) ); // MArray maker function (shorthand MA1() version avail)
	EXPECT_EQ( a( 1 ).m, ma( 1 ) );
	EXPECT_EQ( a( 2 ).m, ma( 2 ) );
	EXPECT_EQ( a( 3 ).m, ma( 3 ) );
	EXPECT_EQ( a( 4 ).m, ma( 4 ) );
	EXPECT_EQ( a( 5 ).m, ma( 5 ) );
}

TEST( MArrayTest, MakerMethod1D )
{
	Array1D< C > a( 5 );
	for ( int i = 1; i <= a.u(); ++i ) a( i ).m = i;
	auto ma( a.ma( &C::m ) ); // Array ma maker method
//	auto ma( a.ma( &decltype( a )::Value::m ) ); // Array ma maker method when you don't want to look up the value type of a // Intel C++ 14.x doesn't support this yet
	EXPECT_EQ( a( 1 ).m, ma( 1 ) );
	EXPECT_EQ( a( 2 ).m, ma( 2 ) );
	EXPECT_EQ( a( 3 ).m, ma( 3 ) );
	EXPECT_EQ( a( 4 ).m, ma( 4 ) );
	EXPECT_EQ( a( 5 ).m, ma( 5 ) );
}

TEST( MArrayTest, Basic2D )
{
	C const c( 42, 123.5 );
	Array2D< C > a( 2, 2, c );
//	int C::*pm( &C::m ); // Can give the pointer to member a name like this also
//	MArray2< Array2D< C >, int > ma( a, pm );
	MArray2< Array2D< C >, int > ma( a, &C::m );
	EXPECT_EQ( a( 1, 1 ).m, ma( 1, 1 ) );
	EXPECT_EQ( a( 2, 2 ).m, ma( 2, 2 ) );
	ma += 1;
	EXPECT_EQ( 43, ma( 1, 1 ) );
	EXPECT_EQ( 43, ma( 1, 2 ) );
	EXPECT_EQ( 43, ma( 2, 1 ) );
	EXPECT_EQ( 43, ma( 2, 2 ) );
}

TEST( MArrayTest, Functions1D )
{
	C const ca( 42, 5.0f );
	Array1D< C > a( 3, ca );
	auto A( make_MArray1( a, &C::x ) );
	C const cb( 42, 4.0f );
	Array1D< C > b( 3, cb );
	auto B( make_MArray1( b, &C::x ) );
	EXPECT_EQ( 75.0f, magnitude_squared( A ) );
	EXPECT_EQ( 3.0f, distance_squared( A, B ) );
	EXPECT_EQ( 60.0f, dot( A, B ) );
}

TEST( MArrayTest, dot1D )
{
	C const c( 42, 5.0f );
	Array1D< C > a( 5, c );
	auto A( make_MArray1( a, &C::x ) );
	auto B( make_MArray1( a, &C::x ) );
	EXPECT_EQ( 125.0f, dot( A, B ) );
	EXPECT_EQ( 125.0f, dot( B, A ) );
	EXPECT_EQ( 125.0f, dot_product( A, B ) );
	EXPECT_EQ( 125.0f, dot_product( B, A ) );
}

//TEST( MArrayTest, EoshiftPos1D )
//{
//	Array1D< C > a( 5 );
//	for ( int i = a.l(); i <= a.u(); ++i ) a( i ).m = i;
//	auto A( make_MArray1( a, &C::m ) );
//	EXPECT_TRUE( eq( Array1D_int( { 3, 4, 5, 0, 0 } ), eoshift( A, 2 ) ) );
//	EXPECT_TRUE( eq( Array1D_int( { 3, 4, 5, 9, 9 } ), eoshift( A, 2, 9 ) ) );
//}

TEST( MArrayTest, AnyOp2D )
{
	Array2D< C > const A( 3, 3, { C( 1 ), C( 2 ), C( 3 ), C( 4 ), C( 5 ), C( 6 ), C( 7 ), C( 8 ), C( 9 ) } );
	auto M( make_MArray2( A, &C::m ) );
	EXPECT_TRUE( any_eq( M, 6 ) );
	EXPECT_FALSE( any_eq( M, 22 ) );
	EXPECT_TRUE( any_ne( M, 6 ) );
	EXPECT_TRUE( any_lt( M, 2 ) );
	EXPECT_TRUE( any_ge( M, 9 ) );
	EXPECT_FALSE( any_lt( M, 1 ) );
	EXPECT_FALSE( any_gt( M, 9 ) );
}

TEST( MArrayTest, AllOp2D )
{
	Array2D< C > const A( 3, 3, { C( 1 ), C( 2 ), C( 3 ), C( 4 ), C( 5 ), C( 6 ), C( 7 ), C( 8 ), C( 9 ) } );
	auto M( make_MArray2( A, &C::m ) );
	EXPECT_FALSE( all_eq( M, 6 ) );
	EXPECT_FALSE( all_eq( M, 22 ) );
	EXPECT_TRUE( all_ne( M, 22 ) );
	EXPECT_FALSE( all_ne( M, 2 ) );
	EXPECT_FALSE( all_lt( M, 2 ) );
	EXPECT_FALSE( all_ge( M, 9 ) );
	EXPECT_TRUE( all_lt( M, 11 ) );
	EXPECT_TRUE( all_gt( M, 0 ) );
}

TEST( MArrayTest, CountOp2D )
{
	Array2D< C > const A( 3, 3, { C( 1 ), C( 2 ), C( 2 ), C( 3 ), C( 3 ), C( 3 ), C( 7 ), C( 8 ), C( 9 ) } );
	auto M( make_MArray2( A, &C::m ) );
	EXPECT_EQ( 0u, count_eq( M, 0 ) );
	EXPECT_EQ( 1u, count_eq( M, 1 ) );
	EXPECT_EQ( 2u, count_eq( M, 2 ) );
	EXPECT_EQ( 3u, count_eq( M, 3 ) );
	EXPECT_EQ( 6u, count_lt( M, 7 ) );
	EXPECT_EQ( 1u, count_ge( M, 9 ) );
	EXPECT_EQ( 9u, count_lt( M, 11 ) );
	EXPECT_EQ( 3u, count_gt( M, 3 ) );
}
