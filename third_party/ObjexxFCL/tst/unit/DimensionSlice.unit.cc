// ObjexxFCL::DimensionSlice Unit Tests
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
#include <ObjexxFCL/DimensionSlice.hh>
#include <ObjexxFCL/IndexRange.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( DimensionSliceTest, ConstructionIndexSlice )
{
	IndexSlice s( -3, 3, 2 );
	DimensionSlice d( s );
	EXPECT_EQ( s.s(), d.m() );
	EXPECT_EQ( 2, d.m() );
	EXPECT_EQ( s.l() - s.s(), d.k() );
	EXPECT_EQ( -5, d.k() );
	EXPECT_EQ( int( s.size() ), d.u() );
	EXPECT_EQ( 4, d.u() );
}

TEST( DimensionSliceTest, ConstructionIndexSliceMultiplier )
{
	IndexSlice s( -3, 3, 2 );
	DimensionSlice d( s, 3 );
	EXPECT_EQ( s.s() * 3, d.m() );
	EXPECT_EQ( 6, d.m() );
	EXPECT_EQ( ( s.l() - s.s() ) * 3, d.k() );
	EXPECT_EQ( -15, d.k() );
	EXPECT_EQ( int( s.size() ), d.u() );
	EXPECT_EQ( 4, d.u() );
	DimensionSlice dc( d ); // Copy construction
	EXPECT_EQ( 6, dc.m() );
	EXPECT_EQ( -15, dc.k() );
	EXPECT_EQ( 4, dc.u() );
	EXPECT_EQ( d.m(), dc.m() );
	EXPECT_EQ( d.k(), dc.k() );
	EXPECT_EQ( d.u(), dc.u() );
}

TEST( DimensionSliceTest, ConstructionIndexRangeSliceMultiplier )
{
	IndexRange r( -5, 5 );
	IndexSlice s( -3, 3, 2 );
	DimensionSlice d( r, s, 3 );
	EXPECT_EQ( s.s() * 3, d.m() );
	EXPECT_EQ( 6, d.m() );
	EXPECT_EQ( ( s.l() - s.s() ) * 3, d.k() );
	EXPECT_EQ( -15, d.k() );
	EXPECT_EQ( int( s.size() ), d.u() );
	EXPECT_EQ( 4, d.u() );
}

TEST( DimensionSliceTest, ConstructionIndexRangeSliceOmitMultiplier )
{
	IndexRange r( -5, 3 );
	IndexSlice s( -3, _, 2 );
	DimensionSlice d( r, s, 3 );
	EXPECT_EQ( s.s() * 3, d.m() );
	EXPECT_EQ( 6, d.m() );
	EXPECT_EQ( ( s.l() - s.s() ) * 3, d.k() );
	EXPECT_EQ( -15, d.k() );
	EXPECT_EQ( 4, d.u() );
}
