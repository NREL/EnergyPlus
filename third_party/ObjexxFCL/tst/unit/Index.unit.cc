// ObjexxFCL::Index Unit Tests
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
#include <ObjexxFCL/Index.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( IndexTest, ConstructionConstantRange )
{
	Index r( 3 );
	EXPECT_EQ( Index( 3 ), r );
	EXPECT_NE( Index( 2 ), r );
	EXPECT_NE( Index( _ ), r );
	EXPECT_EQ( 3, r.i() );
	EXPECT_EQ( 3, int( r ) ); // Conversion operator
	EXPECT_EQ( 3, r ); // Conversion operator
}

TEST( IndexTest, ConstructionCopy )
{
	Index r( -3 );
	Index s( r );
	EXPECT_EQ( r, s );
}

TEST( IndexTest, ConstructionOmit )
{
	Index r( _ );
	EXPECT_EQ( Index( _ ), r );
	EXPECT_FALSE( r.initialized() );
}

TEST( IndexTest, Assignment )
{
	Index r( -3 );
	Index s;
	s = r;
	EXPECT_EQ( r, s );
}

TEST( IndexTest, Swap )
{
	int const l( -3 );
	Index r( 2*l );
	Index q( l );
	EXPECT_EQ( -6, r.i() );
	EXPECT_EQ( -3, q.i() );
	r.swap( q );
	EXPECT_EQ( -6, q.i() );
	EXPECT_EQ( -3, r.i() );
	swap( q, r );
	EXPECT_EQ( -6, r.i() );
	EXPECT_EQ( -3, q.i() );
}
