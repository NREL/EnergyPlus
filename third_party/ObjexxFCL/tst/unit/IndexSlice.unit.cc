// ObjexxFCL::IndexSlice Unit Tests
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
#include <ObjexxFCL/IndexSlice.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( IndexSliceTest, ConstructionConstantRange )
{
	IndexSlice r( -3, 3, 2 );
	EXPECT_EQ( IndexSlice( -3, 3, 2 ), r );
	EXPECT_NE( IndexSlice( -3, 3, 1 ), r );
	EXPECT_EQ( -3, r.l() );
	EXPECT_EQ( 3, r.u() );
	EXPECT_EQ( 2, r.s() );
	EXPECT_EQ( 4u, r.size() );
	EXPECT_EQ( 3, r.last() );
}

TEST( IndexSliceTest, ConstructionCopy )
{
	IndexSlice r( -3, 3 );
	IndexSlice s( r );
	EXPECT_EQ( r, s );
	EXPECT_EQ( 3, r.last() );
}

TEST( IndexSliceTest, ConstructionList )
{
	IndexSlice r{ -5, 5, 2 };
	EXPECT_EQ( IndexSlice( -5, 5, 2 ), r );
	EXPECT_EQ( 5, r.last() );
}

TEST( IndexSliceTest, ConstructionOmit )
{
	IndexSlice r( -3, _, 3 );
	EXPECT_EQ( IndexSlice( -3, _, 3 ), r );
	EXPECT_EQ( -3, r.l() );
	EXPECT_TRUE( ! r.u_initialized() );
	EXPECT_EQ( 0u, r.size() );
}

TEST( IndexSliceTest, Assignment )
{
	IndexSlice r( -3, 3 );
	IndexSlice s;
	s = r;
	EXPECT_EQ( r, s );
}

TEST( IndexSliceTest, Lifetime )
{
	IndexSlice * rp = new IndexSlice( -3, 5 );
	IndexSlice & r( *rp );
	EXPECT_EQ( IndexSlice( -3, 5 ), r );
	delete rp;
}

TEST( IndexSliceTest, Swap )
{
	int const l( -3 );
	int const u( 9 );
	int const s( 3 );
	IndexSlice r( 2*l, u, s );
	IndexSlice q( l, u+3, s+1 );
	EXPECT_EQ( -6, r.l() );
	EXPECT_EQ( 9, r.u() );
	EXPECT_EQ( 3, r.s() );
	EXPECT_EQ( 9, r.last() );
	EXPECT_EQ( -3, q.l() );
	EXPECT_EQ( 12, q.u() );
	EXPECT_EQ( 4, q.s() );
	EXPECT_EQ( 9, q.last() );
	r.swap( q );
	EXPECT_EQ( -6, q.l() );
	EXPECT_EQ( 9, q.u() );
	EXPECT_EQ( 3, q.s() );
	EXPECT_EQ( -3, r.l() );
	EXPECT_EQ( 12, r.u() );
	EXPECT_EQ( 4, r.s() );
	swap( q, r );
	EXPECT_EQ( -6, r.l() );
	EXPECT_EQ( 9, r.u() );
	EXPECT_EQ( 3, r.s() );
	EXPECT_EQ( -3, q.l() );
	EXPECT_EQ( 12, q.u() );
	EXPECT_EQ( 4, q.s() );
}

TEST( IndexSliceTest, InitializerListConstructionDefault )
{
	IndexSlice s{}; // Default construction
	EXPECT_FALSE( s.l_initialized() );
	EXPECT_FALSE( s.u_initialized() );
	EXPECT_EQ( 1, s.s() );
	EXPECT_EQ( 0u, s.size() );
}

TEST( IndexSliceTest, InitializerListConstructionInt1 )
{
	IndexSlice s{ 9 };
	EXPECT_TRUE( s.l_initialized() );
	EXPECT_FALSE( s.u_initialized() );
	EXPECT_EQ( 9, s.l() );
	EXPECT_EQ( 1, s.s() );
	EXPECT_EQ( 0u, s.size() );
}

TEST( IndexSliceTest, InitializerListConstructionInt2 )
{
	IndexSlice s{ 9, 19 };
	EXPECT_TRUE( s.l_initialized() );
	EXPECT_TRUE( s.u_initialized() );
	EXPECT_EQ( 9, s.l() );
	EXPECT_EQ( 19, s.u() );
	EXPECT_EQ( 1, s.s() );
	EXPECT_EQ( 11u, s.size() );
}

TEST( IndexSliceTest, InitializerListConstructionIndex2 )
{
	IndexSlice s{ 9, _ }; // Should call List of Index ctor
	EXPECT_TRUE( s.l_initialized() );
	EXPECT_FALSE( s.u_initialized() );
	EXPECT_EQ( 9, s.l() );
	EXPECT_EQ( 1, s.s() );
	EXPECT_EQ( 0u, s.size() );
}

TEST( IndexSliceTest, InitializerListConstructionIndexExplicit2 )
{
	IndexSlice s{ Index( 9 ), _ }; // Should call List of Index ctor
	EXPECT_TRUE( s.l_initialized() );
	EXPECT_FALSE( s.u_initialized() );
	EXPECT_EQ( 9, s.l() );
	EXPECT_EQ( 1, s.s() );
	EXPECT_EQ( 0u, s.size() );
}

TEST( IndexSliceTest, SliceInitializerList )
{
	IndexSlice r( -3, 3 );
	IndexSlice s( 33 );
	EXPECT_EQ( 33, s.l() );
	EXPECT_EQ( 33, s.u() );
	EXPECT_EQ( 1, s.s() );
	EXPECT_EQ( 1u, s.size() );
	EXPECT_TRUE( s.scalar() );
	std::initializer_list< IndexSlice > l{ r, 33 }; // List will convert integer to IndexSlice for you
	EXPECT_EQ( 2u, l.size() );
	auto i( l.begin() );
	EXPECT_EQ( r, *i );
	EXPECT_EQ( s, *(++i) );
}
