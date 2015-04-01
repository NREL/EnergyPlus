// ObjexxFCL::DynamicIndexRange Unit Tests
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
#include <ObjexxFCL/DynamicIndexRange.hh>
#include <ObjexxFCL/DimensionExpressions.hh>
#include <ObjexxFCL/ObserverMediator.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( DynamicIndexRangeTest, ConstructionConstantRange )
{
	DynamicIndexRange r( -3, 3 );
	EXPECT_EQ( DynamicIndexRange( -3, 3 ), r );
	EXPECT_EQ( -3, r.l() );
	EXPECT_EQ( 3, r.u() );
	EXPECT_EQ( 7u, r.size() );
}

TEST( DynamicIndexRangeTest, ConstructionDimensionRange )
{
	Dimension d( 2 );
	DynamicIndexRange r( d + 1, 3 * d );
	EXPECT_EQ( DynamicIndexRange( 3, 6 ), r );
	d = 3;
	EXPECT_EQ( DynamicIndexRange( 4, 9 ), r );
	EXPECT_EQ( 4, r.l() );
	EXPECT_EQ( 9, r.u() );
}

TEST( DynamicIndexRangeTest, Lifetime1 )
{
	Dimension * dp = new Dimension( 2 );
	Dimension & d( *dp );
	DynamicIndexRange r( d + 1, 3 * d );
	EXPECT_EQ( DynamicIndexRange( 3, 6 ), r );
	delete dp;
}

TEST( DynamicIndexRangeTest, Lifetime2 )
{
	Dimension * dp = new Dimension( 2 );
	Dimension & d( *dp );
	DynamicIndexRange * rp = new DynamicIndexRange( d + 1, 3 * d );
	DynamicIndexRange & r( *rp );
	EXPECT_EQ( DynamicIndexRange( 3, 6 ), r );
	delete dp; // Delete Dimension first
	delete rp;
}

TEST( DynamicIndexRangeTest, Lifetime22 )
{
	Dimension * dp = new Dimension( 2 );
	Dimension & d( *dp );
	DynamicIndexRange * rp = new DynamicIndexRange( d + 1, 3 * d );
	DynamicIndexRange & r( *rp );
	EXPECT_EQ( DynamicIndexRange( 3, 6 ), r );
	delete rp; // Delete range first
	delete dp;
}

TEST( DynamicIndexRangeTest, Lifetime2Ranges )
{
	Dimension * dp = new Dimension( 2 );
	Dimension & d( *dp );
	DynamicIndexRange * rp = new DynamicIndexRange( d + 1, 3 * d );
	DynamicIndexRange & r( *rp );
	EXPECT_EQ( DynamicIndexRange( 3, 6 ), r );
	DynamicIndexRange * sp = new DynamicIndexRange( d + 1, 3 * d );
	DynamicIndexRange & s( *sp );
	EXPECT_EQ( DynamicIndexRange( 3, 6 ), s );
	delete rp; // Delete range first
	delete dp;
	delete sp;
}

TEST( DynamicIndexRangeTest, Lifetime2Ranges2 )
{
	Dimension * dp = new Dimension( 2 );
	Dimension & d( *dp );
	DynamicIndexRange * rp = new DynamicIndexRange( d + 1, 3 * d );
	DynamicIndexRange & r( *rp );
	EXPECT_EQ( DynamicIndexRange( 3, 6 ), r );
	DynamicIndexRange * sp = new DynamicIndexRange( d + 1, 3 * d );
	DynamicIndexRange & s( *sp );
	EXPECT_EQ( DynamicIndexRange( 3, 6 ), s );
	delete dp; // Delete Dimension first
	delete rp;
	delete sp;
}

TEST( DynamicIndexRangeTest, Swap )
{
	Dimension l( -3 );
	Dimension u( 9 );
	DynamicIndexRange r( 2*l, u );
	DynamicIndexRange s( l, u+3 );
	EXPECT_EQ( -6, r.l() );
	EXPECT_EQ( 9, r.u() );
	EXPECT_EQ( -3, s.l() );
	EXPECT_EQ( 12, s.u() );
	r.swap( s );
	EXPECT_EQ( -6, s.l() );
	EXPECT_EQ( 9, s.u() );
	EXPECT_EQ( -3, r.l() );
	EXPECT_EQ( 12, r.u() );
	swap( s, r );
	EXPECT_EQ( -6, r.l() );
	EXPECT_EQ( 9, r.u() );
	EXPECT_EQ( -3, s.l() );
	EXPECT_EQ( 12, s.u() );
}
