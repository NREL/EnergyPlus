// ObjexxFCL::random Unit Tests
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

// ObjexxFCL Headers
#include <ObjexxFCL/random.hh>
#include <ObjexxFCL/Array1D.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( RandomTest, RandomNumber )
{
	double r;
	RANDOM_NUMBER( r );
	EXPECT_TRUE( 0.0 <= r );
	EXPECT_TRUE( r < 1.0 );

	Array1D< float > a( 3 );
	RANDOM_NUMBER( a );
	for ( int i = a.l(); i <= a.u(); ++i ) {
		EXPECT_TRUE( 0.0f <= a( i ) );
		EXPECT_TRUE( a( i ) < 1.0f );
	}
}

TEST( RandomTest, Random )
{
	{
		float r;
		RANDOM( r );
		EXPECT_TRUE( 0.0f <= r );
		EXPECT_TRUE( r < 1.0f );
	}

	{
		float const r( RANDOM( 42 ) );
		EXPECT_TRUE( 0.0f <= r );
		EXPECT_TRUE( r < 1.0f );
	}
}

TEST( RandomTest, Rand )
{
	{
		float const r( RAND() );
		EXPECT_TRUE( 0.0f <= r );
		EXPECT_TRUE( r < 1.0f );
	}

	{
		float const r( RAND( 42 ) );
		EXPECT_TRUE( 0.0f <= r );
		EXPECT_TRUE( r < 1.0f );
	}
}

TEST( RandomTest, Ranf )
{
	{
		float const r( RANF() );
		EXPECT_TRUE( 0.0 <= r );
		EXPECT_TRUE( r < 2147483647.0 );
	}

	{
		float const r( RANF( 42 ) );
		EXPECT_TRUE( 0.0 <= r );
		EXPECT_TRUE( r < 2147483647.0 );
	}
}

TEST( RandomTest, DRandm )
{
	double const r( DRANDM( 42 ) );
	EXPECT_TRUE( 0.0 <= r );
	EXPECT_TRUE( r < 1.0 );
}

TEST( RandomTest, DRand )
{
	double const r( DRAND( 42 ) );
	EXPECT_TRUE( 0.0 <= r );
	EXPECT_TRUE( r < 1.0 );
}

TEST( RandomTest, Randu )
{
	float r;
	RANDU( 24, 42, r );
	EXPECT_TRUE( 0.0f <= r );
	EXPECT_TRUE( r < 1.0f );
}

TEST( RandomTest, Irandm )
{
	{
		std::int32_t const r( IRANDM() );
		EXPECT_TRUE( 0 <= r );
		EXPECT_TRUE( r <= 32767 );
	}

	{
		std::int32_t const r( IRANDM( 42 ) );
		EXPECT_TRUE( 0 <= r );
		EXPECT_TRUE( r <= 2147483647 );
	}
}

TEST( RandomTest, Irand )
{
	{
		std::int32_t const r( IRAND() );
		EXPECT_TRUE( 0 <= r );
		EXPECT_TRUE( r <= 32767 );
	}

	{
		std::int32_t const r( IRAND( 42 ) );
		EXPECT_TRUE( 0 <= r );
		EXPECT_TRUE( r <= 2147483647 );
	}
}

TEST( RandomTest, RandomSeed )
{
	int size;
	Array1D< int > put( { 11, 22, 33 } );
	Array1D< int > get( 3 );
	// Just run them
	RANDOM_SEED();
	RANDOM_SEED( size );
	RANDOM_SEED( _, put );
	RANDOM_SEED( _, _, get );
}

TEST( RandomTest, Srand )
{
	SRAND( 42 );
}
