// ObjexxFCL::MArray6 Unit Tests
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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4244) // Suppress conversion warnings: Intentional narrowing assignments present
#endif

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/MArray6.hh>
#include <ObjexxFCL/Array6D.hh>
#include <ObjexxFCL/MArray.functions.hh>
#include "ObjexxFCL.unit.hh"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

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

TEST( MArray6Test, FunctionMerge )
{
	{
		Array6D< C > A( 2, 2, 2, 2, 2, 2, C( 1 ) );
		Array6D< C > B( 2, 2, 2, 2, 2, 2, C( 2 ) );
		MArray6< Array6D< C >, int > a( A, &C::m );
		MArray6< Array6D< C >, int > b( B, &C::m );
		EXPECT_TRUE( eq( Array6D_int( 2, 2, 2, 2, 2, 2, 1 ), merge( a, b, true ) ) );
		EXPECT_TRUE( eq( Array6D_int( 2, 2, 2, 2, 2, 2, 2 ), merge( a, b, false ) ) );
	}

	{
		Array6D< C > A( 2, 2, 2, 2, 2, 2, C( 1 ) );
		int const b( 2 );
		MArray6< Array6D< C >, int > a( A, &C::m );
		EXPECT_TRUE( eq( Array6D_int( 2, 2, 2, 2, 2, 2, 1 ), merge( a, b, true ) ) );
		EXPECT_TRUE( eq( Array6D_int( 2, 2, 2, 2, 2, 2, 2 ), merge( a, b, false ) ) );
	}

	{
		int const a( 1 );
		Array6D< C > B( 2, 2, 2, 2, 2, 2, C( 2 ) );
		MArray6< Array6D< C >, int > b( B, &C::m );
		EXPECT_TRUE( eq( Array6D_int( 2, 2, 2, 2, 2, 2, 1 ), merge( a, b, true ) ) );
		EXPECT_TRUE( eq( Array6D_int( 2, 2, 2, 2, 2, 2, 2 ), merge( a, b, false ) ) );
	}

	{
		Array6D< C > A( 2, 2, 2, 2, 2, 2, C( 1 ) );
		Array6D< C > B( 2, 2, 2, 2, 2, 2, C( 2 ) );
		MArray6< Array6D< C >, int > a( A, &C::m );
		MArray6< Array6D< C >, int > b( B, &C::m );
		Array6D_bool const mask( 2, 2, 2, 2, 2, 2, { true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true } );
		Array6D_int const m( 2, 2, 2, 2, 2, 2, { 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1 } );
		EXPECT_TRUE( eq( m, merge( a, b, mask ) ) );
	}

	{
		Array6D< C > A( 2, 2, 2, 2, 2, 2, C( 1 ) );
		int const b( 2 );
		MArray6< Array6D< C >, int > a( A, &C::m );
		Array6D_bool const mask( 2, 2, 2, 2, 2, 2, { true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true } );
		Array6D_int const m( 2, 2, 2, 2, 2, 2, { 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1 } );
		EXPECT_TRUE( eq( m, merge( a, b, mask ) ) );
	}

	{
		int const a( 1 );
		Array6D< C > B( 2, 2, 2, 2, 2, 2, C( 2 ) );
		MArray6< Array6D< C >, int > b( B, &C::m );
		Array6D_bool const mask( 2, 2, 2, 2, 2, 2, { true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true, true, false, false, true } );
		Array6D_int const m( 2, 2, 2, 2, 2, 2, { 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1 } );
		EXPECT_TRUE( eq( m, merge( a, b, mask ) ) );
	}
}
