// ObjexxFCL::IndexRange Unit Tests
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
#include <ObjexxFCL/IndexRange.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( IndexRangeTest, ConstructionConstantRange )
{
	IndexRange r( -3, 3 );
	EXPECT_EQ( IndexRange( -3, 3 ), r );
	EXPECT_EQ( -3, r.l() );
	EXPECT_EQ( 3, r.u() );
	EXPECT_EQ( 7u, r.size() );
}

TEST( IndexRangeTest, ConstructionCopy )
{
	IndexRange r( -3, 3 );
	IndexRange s( r );
	EXPECT_EQ( r, s );
}

TEST( IndexRangeTest, Assignment )
{
	IndexRange r( -3, 3 );
	IndexRange s;
	s = r;
	EXPECT_EQ( r, s );
}

TEST( IndexRangeTest, Lifetime )
{
	IndexRange * rp = new IndexRange( -3, 5 );
	IndexRange & r( *rp );
	EXPECT_EQ( IndexRange( -3, 5 ), r );
	delete rp;
}

TEST( IndexRangeTest, Swap )
{
	int const l( -3 );
	int const u( 9 );
	IndexRange r( 2*l, u );
	IndexRange s( l, u+3 );
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
