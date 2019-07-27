// ObjexxFCL::MArray1 Unit Tests
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
#include <ObjexxFCL/MArray1.hh>
#include <ObjexxFCL/Array1D.hh>
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

TEST( MArray1Test, FunctionMerge )
{
	{
		Array1D< C > A( 3, C( 1 ) );
		Array1D< C > B( 3, C( 2 ) );
		MArray1< Array1D< C >, int > a( A, &C::m );
		MArray1< Array1D< C >, int > b( B, &C::m );
		EXPECT_TRUE( eq( Array1D_int( 3, 1 ), merge( a, b, true ) ) );
		EXPECT_TRUE( eq( Array1D_int( 3, 2 ), merge( a, b, false ) ) );
	}

	{
		Array1D< C > A( 3, C( 1 ) );
		int const b( 2 );
		MArray1< Array1D< C >, int > a( A, &C::m );
		EXPECT_TRUE( eq( Array1D_int( 3, 1 ), merge( a, b, true ) ) );
		EXPECT_TRUE( eq( Array1D_int( 3, 2 ), merge( a, b, false ) ) );
	}

	{
		int const a( 1 );
		Array1D< C > B( 3, C( 2 ) );
		MArray1< Array1D< C >, int > b( B, &C::m );
		EXPECT_TRUE( eq( Array1D_int( 3, 1 ), merge( a, b, true ) ) );
		EXPECT_TRUE( eq( Array1D_int( 3, 2 ), merge( a, b, false ) ) );
	}

	{
		Array1D< C > A( 3, C( 1 ) );
		Array1D< C > B( 3, C( 2 ) );
		MArray1< Array1D< C >, int > a( A, &C::m );
		MArray1< Array1D< C >, int > b( B, &C::m );
		Array1D_bool const mask( 3, { true, false, true } );
		Array1D_int const m( 3, { 1, 2, 1 } );
		EXPECT_TRUE( eq( m, merge( a, b, mask ) ) );
	}

	{
		Array1D< C > A( 3, C( 1 ) );
		int const b( 2 );
		MArray1< Array1D< C >, int > a( A, &C::m );
		Array1D_bool const mask( 3, { true, false, true } );
		Array1D_int const m( 3, { 1, 2, 1 } );
		EXPECT_TRUE( eq( m, merge( a, b, mask ) ) );
	}

	{
		int const a( 1 );
		Array1D< C > B( 3, C( 2 ) );
		MArray1< Array1D< C >, int > b( B, &C::m );
		Array1D_bool const mask( 3, { true, false, true } );
		Array1D_int const m( 3, { 1, 2, 1 } );
		EXPECT_TRUE( eq( m, merge( a, b, mask ) ) );
	}
}
