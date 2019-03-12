// ObjexxFCL::member.functions Unit Tests
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// Google Test Headers
#include <gtest/gtest.h>

// C++ Headers
#include <iterator>

// ObjexxFCL Headers
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

struct S
{
	S() :
	 i( 5 )
	{}

	S( int const ii ) :
	 i( ii )
	{}

	int i;
};

TEST( MemberFunctionsTest, SumContainer )
{
	Array1D< S > a( 5, S( 2 ) );
	EXPECT_EQ( 10, sum( a, &S::i ) );
}

TEST( MemberFunctionsTest, SumIterator )
{
	Array1D< S > a( 5, S( 2 ) );
	EXPECT_EQ( 10, sum( a.begin(), a.end(), &S::i ) );
	EXPECT_EQ( 10, sum( std::begin( a ), std::end( a ), &S::i ) );
}

TEST( MemberFunctionsTest, SumSlice )
{
	Array1D< S > a( 5, S( 2 ) );
	Array1S< S > s( a( {2,4} ) );
	EXPECT_EQ( 6, sum( s, &S::i ) );
}

TEST( MemberFunctionsTest, SumSub )
{
	Array1D< S > a( {1,2,3,4,5} );
	Array1D< int > sub( {2,3,4} );
	EXPECT_EQ( 9, sum_sub( a, &S::i, sub ) );
}

TEST( MemberFunctionsTest, SumProductSub1Array )
{
	Array1D< S > a( {1,2,3,4,5} );
	Array1D< int > sub( {2,3,4} );
	EXPECT_EQ( 29, sum_product_sub( a, &S::i, &S::i, sub ) );
}

TEST( MemberFunctionsTest, SumProductSub2Array )
{
	Array1D< int > a( {1,2,1,2,1} );
	Array1D< S > b( {1,2,3,4,5} );
	Array1D< int > sub( {2,3,4} );
	EXPECT_EQ( 15, sum_product_sub( a, b, &S::i, sub ) );
}

TEST( MemberFunctionsTest, ArraySub )
{
	Array1D< S > a( {1,2,3,4,5} );
	Array1D< int > sub( {2,3,4} );
	EXPECT_TRUE( eq( Array1D< int >( {2,3,4} ), array_sub( a, &S::i, sub ) ) );
}

TEST( MemberFunctionsTest, MinvalContainer )
{
	Array1D< S > a( {3,5,3,2,6} );
	EXPECT_EQ( 2, minval( a, &S::i ) );
}

TEST( MemberFunctionsTest, MinvalIterator )
{
	Array1D< S > a( {3,5,3,2,6} );
	EXPECT_EQ( 2, minval( a.begin(), a.end(), &S::i ) );
	EXPECT_EQ( 2, minval( std::begin( a ), std::end( a ), &S::i ) );
}

TEST( MemberFunctionsTest, MinvalSlice )
{
	Array1D< S > a( {3,5,3,2,6} );
	Array1S< S > s( a( {2,4} ) );
	EXPECT_EQ( 2, minval( s, &S::i ) );
}

TEST( MemberFunctionsTest, MaxvalContainer )
{
	Array1D< S > a( {3,5,3,2,6} );
	EXPECT_EQ( 6, maxval( a, &S::i ) );
}

TEST( MemberFunctionsTest, MaxvalIterator )
{
	Array1D< S > a( {3,5,3,2,6} );
	EXPECT_EQ( 6, maxval( a.begin(), a.end(), &S::i ) );
	EXPECT_EQ( 6, maxval( std::begin( a ), std::end( a ), &S::i ) );
}

TEST( MemberFunctionsTest, MaxvalSlice )
{
	Array1D< S > a( {3,5,3,2,6} );
	Array1S< S > s( a( {2,4} ) );
	EXPECT_EQ( 5, maxval( s, &S::i ) );
}

TEST( MemberFunctionsTest, Minloc )
{
	Array1D< S > a( {3,5,3,2,6} );
	EXPECT_EQ( 4, minloc( a, &S::i ) );
	Array1S< S > s( a( {2,4} ) );
	EXPECT_EQ( 3, minloc( s, &S::i ) );
}

TEST( MemberFunctionsTest, Maxloc )
{
	Array1D< S > a( {3,5,3,2,6} );
	EXPECT_EQ( 5, maxloc( a, &S::i ) );
	Array1S< S > s( a( {2,4} ) );
	EXPECT_EQ( 1, maxloc( s, &S::i ) );
}
