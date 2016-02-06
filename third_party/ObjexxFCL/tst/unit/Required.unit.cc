// ObjexxFCL::Required Unit Tests
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
#include <ObjexxFCL/Required.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <string>

using namespace ObjexxFCL;

TEST( RequiredTest, ConstructionDefault )
{
	EXPECT_DEBUG_DEATH( Required_int_const(), ".*Assertion.*" );
}

TEST( RequiredTest, ConstructionOmit )
{
#if defined(__INTEL_COMPILER) && __INTEL_COMPILER < 1500
	EXPECT_DEBUG_DEATH( Required_int_const( Omit{} ), ".*Assertion.*" ); // Intel C++ before 15.0 gives wrong remark about hiding ObjexxFCL::_ if we use _ arg
#else
	EXPECT_DEBUG_DEATH( Required_int_const( _ ), ".*Assertion.*" );
#endif
}

TEST( RequiredTest, ConstructionValue )
{
	Required_int_const r( 33 );
	EXPECT_EQ( 33, r() );
	EXPECT_EQ( 33, int( r ) ); // Conversion operator
	EXPECT_EQ( 33, r ); // Conversion operator
}

TEST( RequiredTest, ConstructionExpression )
{
	Required_int_const r( 33 * 2 );
	EXPECT_EQ( 66, r() );
	EXPECT_EQ( 66, int( r ) ); // Conversion operator
	EXPECT_EQ( 66, r ); // Conversion operator
}

TEST( RequiredTest, ConstructionCopy )
{
	Required_int r( 33 );
	Required_int c( r );
	EXPECT_EQ( 33, c() );
	EXPECT_EQ( 33, int( c ) ); // Conversion operator
	EXPECT_EQ( 33, c ); // Conversion operator
}

TEST( RequiredTest, ConstructionCopyConst )
{
	Required_int_const r( 33 );
	Required_int_const c( r );
	EXPECT_EQ( 33, c() );
	EXPECT_EQ( 33, int( c ) ); // Conversion operator
	EXPECT_EQ( 33, c ); // Conversion operator
}

TEST( RequiredTest, AssignmentValue )
{
	int i( -3 );
	Required_int r( i );
	r = 42;
	EXPECT_EQ( 42, r );
}

TEST( RequiredTest, StringFromLiteral )
{
	Required_string_const r( "A literal string" );
	EXPECT_EQ( std::string( "A literal string" ), r );
	EXPECT_EQ( "A literal string", r() ); // Need the () on r() when types don't match exactly
	EXPECT_TRUE( r.present() );
	EXPECT_NE( std::string( "Some other string" ), r );
}

TEST( RequiredTest, StringAssignment )
{
	std::string s( "A literal string" );
	Required_string o( s );
	EXPECT_EQ( std::string( "A literal string" ), o );
	EXPECT_EQ( "A literal string", o() ); // Need the () on o() when types don't match exactly
	EXPECT_TRUE( o.present() );
	EXPECT_NE( std::string( "Some other string" ), o );
	o = "New string";
	EXPECT_EQ( "New string", o() ); // Need the () on o() when types don't match exactly
	EXPECT_EQ( "New string", s );
}

TEST( RequiredTest, ConstReference )
{
	int i( 42 );
	int const & j( i );
	Required_int_const o( j );
	EXPECT_EQ( 42, o() );
	i = 56;
	EXPECT_EQ( 56, o() );
}

TEST( RequiredTest, ConstRequiredFromNonConstRequired )
{
	int i( 42 );
	Required_int o( i );
	Required_int_const c( o );
	EXPECT_EQ( 42, c() );
	i = 56;
	EXPECT_EQ( 56, c() ); // Proves that c is not holding a local copy
	EXPECT_FALSE( c.own() ); // So does this
	o = 123;
	EXPECT_EQ( 123, c() );
}
