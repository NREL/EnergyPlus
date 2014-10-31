// ObjexxFCL::StaticIndexRange Unit Tests
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
#include <ObjexxFCL/StaticIndexRange.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( StaticIndexRangeTest, ConstructionConstantRange )
{
	StaticIndexRange r( -3, 3 );
	EXPECT_EQ( StaticIndexRange( -3, 3 ), r );
	EXPECT_EQ( -3, r.l() );
	EXPECT_EQ( 3, r.u() );
	EXPECT_EQ( 7u, r.size() );
}

TEST( StaticIndexRangeTest, ConstructionCopy )
{
	StaticIndexRange r( -3, 3 );
	StaticIndexRange s( r );
	EXPECT_EQ( r, s );
}

TEST( StaticIndexRangeTest, ConstructionStar )
{
	StaticIndexRange r( -3, Star() );
	EXPECT_EQ( StaticIndexRange( -3, Star() ), r );
	EXPECT_EQ( -3, r.l() );
	EXPECT_EQ( StaticIndexRange::npos, r.size() );
}

TEST( StaticIndexRangeTest, Assignment )
{
	StaticIndexRange r( -3, 3 );
	StaticIndexRange s;
	s = r;
	EXPECT_EQ( r, s );
}

TEST( StaticIndexRangeTest, Lifetime )
{
	StaticIndexRange * rp = new StaticIndexRange( -3, 5 );
	StaticIndexRange & r( *rp );
	EXPECT_EQ( StaticIndexRange( -3, 5 ), r );
	delete rp;
}

TEST( StaticIndexRangeTest, Swap )
{
	int const l( -3 );
	int const u( 9 );
	StaticIndexRange r( 2*l, u );
	StaticIndexRange s( l, u+3 );
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
