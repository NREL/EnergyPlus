// ObjexxFCL::floops Unit Tests
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
#include <ObjexxFCL/floops.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <cstddef>

using namespace ObjexxFCL;

TEST( floopsTest, floops1 )
{
	int i, b = -3, e = 3, s = 2;
	EXPECT_EQ( std::size_t( 4 ), floops( i, b, e, s ) );
	EXPECT_EQ( b, i );
}

TEST( floopsTest, floops2 )
{
	int i, b = -3, e = 3, s = -2;
	EXPECT_EQ( std::size_t( 0 ), floops( i, b, e, s ) );
	EXPECT_EQ( b, i );
}

TEST( floopsTest, floops3 )
{
	int i, b = -3, e = 3;
	EXPECT_EQ( std::size_t( 7 ), floops( i, b, e ) );
	EXPECT_EQ( b, i );
}

TEST( floopsTest, floops4 )
{
	int i, b = 3, e = -3, s = -2;
	EXPECT_EQ( std::size_t( 4 ), floops( i, b, e, s ) );
	EXPECT_EQ( b, i );
}

TEST( floopsTest, floops5 )
{
	double i, b = 2.5, e = -2.5, s = -1.0;
	EXPECT_EQ( std::size_t( 6 ), floops( i, b, e, s ) );
	EXPECT_EQ( b, i );
}
