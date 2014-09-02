// ObjexxFCL::Dimension Unit Tests
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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Dimension.hh>
#include <ObjexxFCL/DimensionExpressions.hh>
#include <ObjexxFCL/ObserverMediator.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( DimensionTest, Construction )
{
	Dimension a;
	Dimension b;
	Dimension c( min( a, b ) );
	a = 5;
	b = 3;
	EXPECT_EQ( 3, c() );
	c = min( a, 2 );
	EXPECT_EQ( 2, c() );
}

TEST( DimensionTest, Max )
{
	Dimension a;
	Dimension b;
	Dimension c( max( a, b ) );
	a = 5;
	b = 3;
	EXPECT_EQ( 5, c() );
	c = max( a, 6 );
	EXPECT_EQ( 6, c() );
}

TEST( DimensionTest, Pow )
{
	Dimension a;
	Dimension b;
	Dimension c( pow( a, b ) );
	a = 5;
	b = 3;
	EXPECT_EQ( 125, c() );
	c = pow( a, 2 );
	EXPECT_EQ( 25, c() );
}

TEST( DimensionTest, Square )
{
	Dimension a;
	Dimension c( square( a ) );
	a = 5;
	EXPECT_EQ( 25, c() );
	c = square( 3 );
	EXPECT_EQ( 9, c() );
	c = square( 3.3 );
	EXPECT_EQ( 10, c() );
}

TEST( DimensionTest, Cube )
{
	Dimension a;
	Dimension c( cube( a ) );
	a = 5;
	EXPECT_EQ( 125, c() );
	c = cube( 3 );
	EXPECT_EQ( 27, c() );
	c = cube( 3.3 );
	EXPECT_EQ( 35, c() );
}

TEST( DimensionTest, Lifetime )
{
	Dimension * a = new Dimension( 12 );
	Dimension b( square( *a ) );
	Dimension c( *a + *a );
	EXPECT_EQ( 144, b() );
	EXPECT_EQ( 24, c() );
	delete a;
	// b and c should destruct cleanly
}

TEST( DimensionTest, Lifetime2 )
{
	Dimension * a = new Dimension( 12 );
	Dimension * b = new Dimension( square( *a ) );
	Dimension * c = new Dimension( *a + *a );
	EXPECT_EQ( 144, b->value() );
	EXPECT_EQ( 24, c->value() );
	delete a;
	delete b;
	delete c;
}

TEST( DimensionTest, Lifetime3 )
{
	Dimension * a = new Dimension( 12 );
	Dimension * b = new Dimension( square( *a ) );
	Dimension * c = new Dimension( *a + *a );
	EXPECT_EQ( 144, b->value() );
	EXPECT_EQ( 24, c->value() );
	delete c;
	delete a;
	delete b;
}

TEST( DimensionTest, Lifetime4 )
{
	Dimension * a = new Dimension( 12 );
	Dimension * b = new Dimension( square( *a ) );
	Dimension * c = new Dimension( *a + *a );
	EXPECT_EQ( 144, b->value() );
	EXPECT_EQ( 24, c->value() );
	delete b;
	delete c;
	delete a;
}

TEST( DimensionTest, OperatorExpression )
{
	Dimension h;
	Dimension n( 500 );
	h = n;
	EXPECT_EQ( n, h );
	EXPECT_EQ( 500, h );
//	EXPECT_EQ( n + 2, h + 2 ); // gtest can't compile this
//	EXPECT_EQ( 502, h + 2 ); // gtest can't compile this
	h += n;
	EXPECT_EQ( 1000, h );
	h *= 3.33;
	EXPECT_EQ( 3330, h );
}

TEST( DimensionTest, IntegerDivision )
{
	Dimension n( 44.5 );
	Dimension d( 22.2 );
	EXPECT_EQ( 44, n );
	EXPECT_EQ( 22, d );
	Dimension q( n / d );
	EXPECT_EQ( 2, q );
}

TEST( DimensionTest, SelfAssignment )
{
	Dimension m( 88 );
	EXPECT_EQ( 88, m );
	m *= m;
	EXPECT_EQ( 88 * 88, m );
}

TEST( DimensionTest, constDimensionUpdateViaNotify )
{
	Dimension m( 5 );
	Dimension const c( m );
	EXPECT_EQ( 5, m );
	EXPECT_EQ( c, m );
	m = 7;
	EXPECT_EQ( 7, m );
	EXPECT_EQ( c, m );
}

TEST( DimensionTest, DimensionIncrementDecrement )
{
	Dimension n( 0 );
	EXPECT_EQ( 0, n );
	EXPECT_EQ( 1, ++n );
	EXPECT_EQ( 2, ++n );
	EXPECT_EQ( 3, ++n );
	EXPECT_EQ( 2, --n );
	EXPECT_EQ( 1, --n );
	EXPECT_EQ( 0, --n );
}

TEST( DimensionTest, Notification )
{
	Dimension x, y, z, w, a;
	y = x + 3;
	z = y * y + a;
	w = y + z + a;
	x = 2;
	a = 1;
	EXPECT_EQ( 1, a );
	EXPECT_EQ( 2, x );
	EXPECT_EQ( 5, y );
	EXPECT_EQ( 26, z );
	EXPECT_EQ( 32, w );
	x = 3;
	a = 5;
	EXPECT_EQ( 5, a );
	EXPECT_EQ( 3, x );
	EXPECT_EQ( 6, y );
	EXPECT_EQ( 41, z );
	EXPECT_EQ( 52, w );
}

TEST( DimensionTest, assignIf )
{
	Dimension x( 100 );
	EXPECT_EQ( 100, x );
	x.assign_if( 200 );
	EXPECT_EQ( 200, x );
	x.assign_if( 20, .25 );
	EXPECT_EQ( 20, x );
	x.assign_if_half( 9 );
	EXPECT_EQ( 9, x );
	x.assign_if_half( 4.0 );
	EXPECT_EQ( 4, x );
	x.assign_if_half( 9 );
	EXPECT_EQ( 9, x );
}

TEST( DimensionTest, Acyclic )
{
	Dimension x( 1 );
	Dimension y( x + 3 );
	Dimension q( y - 3 );
	EXPECT_EQ( q, x );
//		EXPECT_EQ( Dimension( y - 3 ), x ); // GCC 3.4.5 for some reason can't do this with Dimension copy ctor explicit (but doesn't call it if implicit)
	Dimension z( y * y );
//		EXPECT_EQ( Dimension( y * y ), z ); // GCC 3.4.5 for some reason can't do this with Dimension copy ctor explicit (but doesn't call it if implicit)
	EXPECT_FALSE( internal::ObserverMediator::acyclic( z, x ) ); // Something like x = z * 2; would create a cycle
}

TEST( DimensionTest, Swap )
{
	Dimension x( 1 );
	Dimension y( 4 );
	EXPECT_EQ( 1, x );
	EXPECT_EQ( 4, y );
	swap( x, y );
	EXPECT_EQ( 4, x );
	EXPECT_EQ( 1, y );
}

TEST( DimensionTest, SwapComplex )
{
	Dimension d( 123 );
	Dimension x( d + 1 );
	Dimension y( 2 * d );
	EXPECT_EQ( 124, x );
	EXPECT_EQ( 246, y );
	x.swap( y );
	EXPECT_EQ( 246, x );
	EXPECT_EQ( 124, y );
	swap( y, x );
	EXPECT_EQ( 124, x );
	EXPECT_EQ( 246, y );
}
