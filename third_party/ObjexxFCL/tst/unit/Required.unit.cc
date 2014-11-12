// ObjexxFCL::Required Unit Tests
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
#include <ObjexxFCL/Required.hh>
#include <ObjexxFCL/Fstring.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( RequiredTest, ConstructionDefault )
{
	EXPECT_DEBUG_DEATH( Required_int_const(), ".*Assertion.*" );
}

TEST( RequiredTest, ConstructionOmit )
{
#ifdef __INTEL_COMPILER
	EXPECT_DEBUG_DEATH( Required_int_const( Omit{} ), ".*Assertion.*" ); // Intel C++ gives wrong remark about hiding ObjexxFCL::_ if we use _ arg
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

//TEST( RequiredTest, AssignmentOmit )
//{
//	int i( -3 );
//	Required_int r( i );
//	EXPECT_DEBUG_DEATH( r = _, ".*Assertion.*" ); // Required = Omit operator removed
//}

TEST( RequiredTest, FstringFromLiteral )
{
	Required_Fstring_const r( "A literal string" );
	EXPECT_EQ( Fstring( "A literal string" ), r );
	EXPECT_EQ( "A literal string", r() ); // Need the () on r() when types don't match exactly
	EXPECT_TRUE( r.present() );
	EXPECT_NE( Fstring( "Some other string" ), r );
}

TEST( RequiredTest, FstringAssignment )
{
	Fstring s( "A literal string" );
	Required_Fstring o( s );
	EXPECT_EQ( Fstring( "A literal string" ), o );
	EXPECT_EQ( "A literal string", o() ); // Need the () on o() when types don't match exactly
	EXPECT_TRUE( o.present() );
	EXPECT_NE( Fstring( "Some other string" ), o );
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
