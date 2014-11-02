// ObjexxFCL::FArray1 Unit Tests
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
#include <ObjexxFCL/FArray1.all.hh>
#include <ObjexxFCL/FArray1.io.hh>
#include <ObjexxFCL/FArray2.all.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/DimensionExpressions.hh>
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Vector2.hh>
#include <ObjexxFCL/Vector3.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <array>
#include <vector>

using namespace ObjexxFCL;

TEST( FArray1Test, ConstructionEmpty )
{
	FArray1D_int v;
	EXPECT_EQ( 0u, v.size() );
	EXPECT_EQ( 0u, v.size1() );
	EXPECT_EQ( 1, v.l() );
	EXPECT_EQ( 1, v.l1() );
	EXPECT_EQ( 0, v.u() );
	EXPECT_EQ( 0, v.u1() );
	EXPECT_EQ( FArray1D_int::IR(), v.I() );
	EXPECT_EQ( FArray1D_int::IR(), v.I1() );
}

TEST( FArray1Test, AllocateZero )
{
	FArray1D_int i;
	EXPECT_FALSE( i.allocated() );
	EXPECT_FALSE( allocated( i ) );
	i.allocate( 0 );
	EXPECT_TRUE( i.allocated() );
	EXPECT_TRUE( allocated( i ) );
	EXPECT_EQ( 0u, i.size() );
	EXPECT_EQ( 0u, size( i ) );
}

TEST( FArray1Test, ConstructionCopyEmpty )
{
	FArray1D_int v;
	FArray1D_int c( v );
	EXPECT_EQ( v.size(), c.size() );
	EXPECT_EQ( v.size1(), c.size1() );
	EXPECT_EQ( v.l(),  c.l() );
	EXPECT_EQ( v.l1(), c.l1() );
	EXPECT_EQ( v.u(),  c.u() );
	EXPECT_EQ( v.u1(), c.u1() );
	EXPECT_EQ( v.I(),  c.I() );
	EXPECT_EQ( v.I1(), c.I1() );
}

TEST( FArray1Test, ConstructionUninitialized )
{
	FArray1D_int v( 22 );
	EXPECT_EQ( 22u, v.size() );
	EXPECT_EQ( 1, v.l() );
	EXPECT_EQ( 1, lbound( v, 1 ) );
	EXPECT_EQ( 22, v.u() );
	EXPECT_EQ( 22, ubound( v, 1 ) );
}

TEST( FArray1Test, ConstructionValueInitialized )
{
	FArray1D_int v( 22, 33 );
	EXPECT_EQ( 22u, v.size() );
	EXPECT_EQ( 1, v.l() );
	EXPECT_EQ( 22, v.u() );
	for ( int i = 1; i <= 22; ++i ) {
		EXPECT_EQ( 33, v( i ) );
	}
}

TEST( FArray1Test, ConstructionInitializerListIndexRange )
{
//	FArray1D_double r( FArray1D_double::IR( { 1, 3 } ) ); // Unambiguous
	FArray1D_double r{ 1, 3 }; // Calls the initializer_list constructor but special code in that treats it as an IndexRange
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
}

TEST( FArray1Test, ConstructionInitializerListOnlyInt )
{
	FArray1D_double r{ 1, 2, 3 };
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( 1.0, r( 1 ) );
	EXPECT_EQ( 2.0, r( 2 ) );
	EXPECT_EQ( 3.0, r( 3 ) );
}

TEST( FArray1Test, ConstructionInitializerListOnlyUnsigned )
{
	FArray1D_double r{ 1u, 2u, 3u };
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( 1.0, r( 1 ) );
	EXPECT_EQ( 2.0, r( 2 ) );
	EXPECT_EQ( 3.0, r( 3 ) );
}

TEST( FArray1Test, ConstructionInitializerListOnlyDouble )
{
	FArray1D_double r{ 1.0, 2.0, 3.0 };
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( 1.0, r( 1 ) );
	EXPECT_EQ( 2.0, r( 2 ) );
	EXPECT_EQ( 3.0, r( 3 ) );
}

TEST( FArray1Test, ConstructionInitializerListOnlyFstring )
{
	FArray1D_Fstring r{ "Food", "Hat", "Eggs" };
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( 4u, r( 1 ).length() );
	EXPECT_EQ( 3u, r( 2 ).length() );
	EXPECT_EQ( 4u, r( 3 ).length() );
	EXPECT_EQ( "Food", r( 1 ) );
	EXPECT_EQ( "Hat", r( 2 ) );
	EXPECT_EQ( "Eggs", r( 3 ) );
}

TEST( FArray1Test, ConstructionStdArray )
{
	FArray1D_int v( std::array< int, 3 >{ { 11, 22, 33 } } );
	EXPECT_EQ( 3u, v.size() );
	EXPECT_EQ( 3u, v.size1() );
	EXPECT_EQ( 1, v.l() );
	EXPECT_EQ( 1, v.l1() );
	EXPECT_EQ( 3, v.u() );
	EXPECT_EQ( 3, v.u1() );
	EXPECT_EQ( 11, v( 1 ) );
	EXPECT_EQ( 22, v( 2 ) );
	EXPECT_EQ( 33, v( 3 ) );
}

TEST( FArray1Test, ConstructionStdVector )
{
	FArray1D_int v( std::vector< int >{ 11, 22, 33 } );
	EXPECT_EQ( 3u, v.size() );
	EXPECT_EQ( 3u, v.size1() );
	EXPECT_EQ( 1, v.l() );
	EXPECT_EQ( 1, v.l1() );
	EXPECT_EQ( 3, v.u() );
	EXPECT_EQ( 3, v.u1() );
	EXPECT_EQ( FArray1D_int::IR( 1, 3 ), v.I() );
	EXPECT_EQ( FArray1D_int::IR( 1, 3 ), v.I1() );
	EXPECT_EQ( 11, v( 1 ) );
	EXPECT_EQ( 22, v( 2 ) );
	EXPECT_EQ( 33, v( 3 ) );
}

TEST( FArray1Test, ConstructionVector2 )
{
	FArray1D_int v( Vector2_int{ { 11, 22 } } );
	EXPECT_EQ( 2u, v.size() );
	EXPECT_EQ( 2u, v.size1() );
	EXPECT_EQ( 1, v.l() );
	EXPECT_EQ( 1, v.l1() );
	EXPECT_EQ( 2, v.u() );
	EXPECT_EQ( 2, v.u1() );
	EXPECT_EQ( 11, v( 1 ) );
	EXPECT_EQ( 22, v( 2 ) );
}

TEST( FArray1Test, ConstructionVector3 )
{
	FArray1D_int v( Vector3_int{ { 11, 22, 33 } } );
	EXPECT_EQ( 3u, v.size() );
	EXPECT_EQ( 3u, v.size1() );
	EXPECT_EQ( 1, v.l() );
	EXPECT_EQ( 1, v.l1() );
	EXPECT_EQ( 3, v.u() );
	EXPECT_EQ( 3, v.u1() );
	EXPECT_EQ( 11, v( 1 ) );
	EXPECT_EQ( 22, v( 2 ) );
	EXPECT_EQ( 33, v( 3 ) );
}

static void initializer_function( FArray1D_string & array )
{
	array( 1 ) = "This";
	array( 2 ) = "is";
	array( 3 ) = "a";
	array( 4 ) = "string";
}

template< typename T >
static void initializer_function_template( FArray1D< T > & array )
{
	array( 1 ) = T( 1 );
	array( 2 ) = T( 2 );
	array( 3 ) = T( 3 );
	array( 4 ) = T( 4 );
}

TEST( FArray1Test, ConstructionInitializerFunction )
{
	FArray1D_string r( 4, { "This", "is", "a", "stub" } );
	initializer_function( r );
	EXPECT_EQ( "This", r( 1 ) );
	EXPECT_EQ( "is", r( 2 ) );
	EXPECT_EQ( "a", r( 3 ) );
	EXPECT_EQ( "string", r( 4 ) );

	FArray1D_string const cr( 4, initializer_function );
	EXPECT_EQ( "This", cr( 1 ) );
	EXPECT_EQ( "is", cr( 2 ) );
	EXPECT_EQ( "a", cr( 3 ) );
	EXPECT_EQ( "string", cr( 4 ) );

	FArray1D_double const tr( 4, initializer_function_template< double > );
	EXPECT_EQ( 1.0, tr( 1 ) );
	EXPECT_EQ( 2.0, tr( 2 ) );
	EXPECT_EQ( 3.0, tr( 3 ) );
	EXPECT_EQ( 4.0, tr( 4 ) );
}

TEST( FArray1Test, ConstructionIndexRange )
{
	FArray1D_int r( SRange( 1, 5 ), 33 );
	EXPECT_EQ( 1, r.l() );
	EXPECT_EQ( 5, r.u() );
	for ( int i = 1; i <= 5; ++i ) {
		EXPECT_EQ( 33, r( i ) );
		EXPECT_EQ( 33, r[ i - 1 ] );
	}
	FArray1P_int p( r, SRange( 1, 4 ) );
	EXPECT_EQ( 1, p.l() );
	EXPECT_EQ( 4, p.u() );
	EXPECT_EQ( 33, p[ 0 ] );
	EXPECT_EQ( 33, p[ 3 ] );
	FArray1A_int a( r, DRange( 1, 3 ) );
	EXPECT_EQ( 1, a.l() );
	EXPECT_EQ( 3, a.u() );
	EXPECT_EQ( 33, a[ 0 ] );
	EXPECT_EQ( 33, a[ 2 ] );
}

TEST( FArray1Test, ConstructionIndexRangeList )
{
	FArray1D_int r( {1,5}, 33 );
	EXPECT_EQ( 1, r.l() );
	EXPECT_EQ( 5, r.u() );
	for ( int i = 1; i <= 5; ++i ) {
		EXPECT_EQ( 33, r( i ) );
		EXPECT_EQ( 33, r[ i - 1 ] );
	}
}

TEST( FArray1Test, ConstructionFstring )
{
	FArray1D_Fstring r( 3, Fstring( 3 ) );
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( "   ", r( 1 ) );
}

TEST( FArray1Test, ConstructionFstringSticky )
{
	FArray1D_Fstring r( 3, Sticky_Fstring( Fstring( 3 ) ) );
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( "   ", r( 1 ) );
	EXPECT_EQ( "   ", r( 2 ) );
	EXPECT_EQ( "   ", r( 3 ) );
}

TEST( FArray1Test, ConstructionFstringmakeSticky )
{
	FArray1D_Fstring r( 3, sticky( Fstring( 3 ) ) );
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( "   ", r( 1 ) );
	EXPECT_EQ( "   ", r( 2 ) );
	EXPECT_EQ( "   ", r( 3 ) );
}

TEST( FArray1Test, ConstructionsFstringSticky )
{
	FArray1D_Fstring r( 3, sFstring( 3 ) );
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( "   ", r( 1 ) );
	EXPECT_EQ( "   ", r( 2 ) );
	EXPECT_EQ( "   ", r( 3 ) );
}

TEST( FArray1Test, ConstructionsFstringStickyScalar )
{
	FArray1D_Fstring r( 3, sFstring( 3 ), "Mapping" ); // Sticky arg sets string size: Scalar sets uniform value
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( 3u, r( 1 ).length() );
	EXPECT_EQ( 3u, r( 2 ).length() );
	EXPECT_EQ( 3u, r( 3 ).length() );
	EXPECT_EQ( "Map", r( 1 ) );
	EXPECT_EQ( "Map", r( 2 ) );
	EXPECT_EQ( "Map", r( 3 ) );
	r.allocate( 2 );
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 2, r.u1() );
	EXPECT_EQ( 2u, r.size() );
	EXPECT_EQ( 3u, r( 1 ).length() );
	EXPECT_EQ( 3u, r( 2 ).length() );
	EXPECT_EQ( Fstring( 3 ), r( 1 ) );
	EXPECT_EQ( Fstring( 3 ), r( 2 ) );
}

TEST( FArray1Test, ConstructionFstringInitializerList )
{
	FArray1D_Fstring r( 3, { "Food", "Hat", "Eggs" } );
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( 4u, r( 1 ).length() );
	EXPECT_EQ( 3u, r( 2 ).length() );
	EXPECT_EQ( 4u, r( 3 ).length() );
	EXPECT_EQ( "Food", r( 1 ) );
	EXPECT_EQ( "Hat", r( 2 ) );
	EXPECT_EQ( "Eggs", r( 3 ) );
}

TEST( FArray1Test, ConstructionFstringMakeStickyInitializerList )
{
	FArray1D_Fstring r( 3, sticky( Fstring( 4 ) ), { "Food", "Hat", "Eggs" } ); // Not all items are 4 chars long
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( 4u, r( 1 ).length() );
	EXPECT_EQ( 4u, r( 2 ).length() );
	EXPECT_EQ( 4u, r( 3 ).length() );
	EXPECT_EQ( "Food", r( 1 ) );
	EXPECT_EQ( "Hat ", r( 2 ) );
	EXPECT_EQ( "Eggs", r( 3 ) );
}

TEST( FArray1Test, ConstructionIndexRangeInitializerList )
{
	FArray1D_int r( SRange( -1, 1 ), { 1, 2, 3 } );
	EXPECT_EQ( -1, r.l() );
	EXPECT_EQ( 1, r.u() );
	for ( int i = -1; i <= 1; ++i ) {
		EXPECT_EQ( i + 2, r( i ) );
	}
}

TEST( FArray1Test, ConstructionIndexRangeInitializerArray )
{
	FArray1D_int r( {-1,1}, FArray1D_int( 3, 33 ) );
	EXPECT_EQ( -1, r.l() );
	EXPECT_EQ( 1, r.u() );
	EXPECT_EQ( 33, r( -1 ) );
	EXPECT_EQ( 33, r( 0 ) );
	EXPECT_EQ( 33, r( 1 ) );
}

TEST( FArray1Test, ConstructionRange )
{
	FArray1D_int c( 3 );
	c( 1 ) = 11;
	c( 2 ) = 22;
	c( 3 ) = 33;
	FArray1D_int r1( FArray1D_int::range( c ) );
	EXPECT_EQ( 3u, r1.size() );
	EXPECT_EQ( 3u, r1.size1() );
	EXPECT_EQ( 1, r1.l() );
	EXPECT_EQ( 1, r1.l1() );
	EXPECT_EQ( 3, r1.u() );
	EXPECT_EQ( 3, r1.u1() );
	// Values remain uninitialized
	FArray1D_int r2( FArray1D_int::range( c, 17 ) );
	EXPECT_EQ( 3u, r2.size() );
	EXPECT_EQ( 3u, r2.size1() );
	EXPECT_EQ( 1, r2.l() );
	EXPECT_EQ( 1, r2.l1() );
	EXPECT_EQ( 3, r2.u() );
	EXPECT_EQ( 3, r2.u1() );
	EXPECT_EQ( 17, r2( 1 ) );
	EXPECT_EQ( 17, r2( 2 ) );
	EXPECT_EQ( 17, r2( 3 ) );
}

TEST( FArray1Test, ConstructionShape )
{
	FArray1D_int c( 3 );
	c( 1 ) = 11;
	c( 2 ) = 22;
	c( 3 ) = 33;
	FArray1D_int r1( FArray1D_int::shape( c ) );
	EXPECT_EQ( 3u, r1.size() );
	EXPECT_EQ( 3u, r1.size1() );
	EXPECT_EQ( 1, r1.l() );
	EXPECT_EQ( 1, r1.l1() );
	EXPECT_EQ( 3, r1.u() );
	EXPECT_EQ( 3, r1.u1() );
	// Values remain uninitialized
	FArray1D_int r2( FArray1D_int::shape( c, 17 ) );
	EXPECT_EQ( 3u, r2.size() );
	EXPECT_EQ( 3u, r2.size1() );
	EXPECT_EQ( 1, r2.l() );
	EXPECT_EQ( 1, r2.l1() );
	EXPECT_EQ( 3, r2.u() );
	EXPECT_EQ( 3, r2.u1() );
	EXPECT_EQ( 17, r2( 1 ) );
	EXPECT_EQ( 17, r2( 2 ) );
	EXPECT_EQ( 17, r2( 3 ) );
}

TEST( FArray1Test, ConstructionOneBased )
{
	FArray1D_int c( 3 );
	c( 1 ) = 11;
	c( 2 ) = 22;
	c( 3 ) = 33;
	FArray1D_int r1( FArray1D_int::one_based( c ) );
	EXPECT_EQ( 3u, r1.size() );
	EXPECT_EQ( 3u, r1.size1() );
	EXPECT_EQ( 1, r1.l() );
	EXPECT_EQ( 1, r1.l1() );
	EXPECT_EQ( 3, r1.u() );
	EXPECT_EQ( 3, r1.u1() );
	// Values remain uninitialized
}

TEST( FArray1Test, ConstructionOneBasedInitializerList )
{
	FArray1D_int c( 3 );
	c( 1 ) = 11;
	c( 2 ) = 22;
	c( 3 ) = 33;
	FArray1D_int r( FArray1D_int::one_based( { 11, 22, 33 } ) );
	EXPECT_EQ( 3u, r.size() );
	EXPECT_EQ( 3u, r.size1() );
	EXPECT_EQ( 1, r.l() );
	EXPECT_EQ( 1, r.l1() );
	EXPECT_EQ( 3, r.u() );
	EXPECT_EQ( 3, r.u1() );
	EXPECT_EQ( 11, r( 1 ) );
	EXPECT_EQ( 22, r( 2 ) );
	EXPECT_EQ( 33, r( 3 ) );
}

TEST( FArray1Test, AssignmentBasic )
{
	FArray1D_double v( 22, 55.5 );
	FArray1D_double const w( 13, 6.789 );
	v = w;
	EXPECT_TRUE( eq( w, v ) );
	v = 45.6;
	EXPECT_EQ( 13u, v.size() );
	for ( int i = 1; i <= 13; ++i ) {
		EXPECT_EQ( 45.6, v( i ) );
	}
}

TEST( FArray1Test, FArrayTail )
{
	FArray1D_double v( 22, 55.5 );
	v( 20 ) = 20.0;
	v( 21 ) = 21.0;
	v( 22 ) = 22.0;
	FArrayTail_double t( v.a( 20 ) ); // Tail of last 3 values
	EXPECT_EQ( 3u, t.size() );
	FArray1P_double p( t );
	EXPECT_EQ( 3u, p.size() );
	EXPECT_EQ( 20.0, p( 1 ) );
	p( 1 ) = 99.0;
	EXPECT_EQ( 99.0, p( 1 ) );
	EXPECT_EQ( 21.0, p( 2 ) );
	EXPECT_EQ( 22.0, p( 3 ) );
}

TEST( FArray1Test, FArrayTailConst )
{
	FArray1D_double v( 22, 55.5 );
	v( 20 ) = 20.0;
	v( 21 ) = 21.0;
	v( 22 ) = 22.0;
	FArrayTail_double const t( v.a( 20 ) ); // Tail of last 3 values
	EXPECT_EQ( 3u, t.size() );
	FArray1P_double const p( t );
	EXPECT_EQ( 3u, p.size() );
	EXPECT_EQ( 20.0, p( 1 ) );
	EXPECT_EQ( 21.0, p( 2 ) );
	EXPECT_EQ( 22.0, p( 3 ) );
}

static void dimension_sizing_initializer( ObjexxFCL::FArray1D_int & a ) { a = 44; }

TEST( FArray1Test, DimensionSizingInitializer )
{
	Dimension N;
	FArray1D_int v( N + 1, dimension_sizing_initializer );
	N = 9;
	EXPECT_TRUE( eq( FArray1D_int( 10, 44 ), v ) );
	v = 99; // Flag RAM
	N = 5;
	EXPECT_TRUE( eq( FArray1D_int( 6, 44 ), v ) );
	v = 99; // Flag RAM
	v.dimension( 2 * N ); // Not initialized
	EXPECT_EQ( 10, v.u() );
	v = 99; // Flag RAM
	v.dimension( ( 2 * N ) + 1, 33 );
	EXPECT_TRUE( eq( FArray1D_int( 11, 33 ), v ) );
	v = 99; // Flag RAM
	v.dimension( 2 * N, dimension_sizing_initializer );
	EXPECT_TRUE( eq( FArray1D_int( 10, 44 ), v ) );
}

TEST( FArray1Test, ArgConstruct )
{
	FArray1D_int u( 10, 22 );
	FArray1A_int a( u );
	EXPECT_EQ( u.I(), a.I() );
	EXPECT_EQ( u( 3 ), a( 3 ) );
	EXPECT_TRUE( eq( FArray1D_int( 10, 22 ), a ) );
	++a( 3 );
	EXPECT_EQ( u( 3 ), 23 );
	EXPECT_EQ( u( 3 ), a( 3 ) );
}

TEST( FArray1Test, ConstArgConstruct )
{
	FArray1D_int const u( 10, 22 );
	FArray1A_int const a( u );
	EXPECT_EQ( u.I(), a.I() );
	EXPECT_EQ( u( 3 ), a( 3 ) );
	EXPECT_TRUE( eq( FArray1D_int( 10, 22 ), a ) );
}

TEST( FArray1Test, ProxyConstruct )
{
	FArray1D_int u( 10, 22 );
	FArray1P_int a( u );
	EXPECT_EQ( u.I(), a.I() );
	EXPECT_EQ( u( 3 ), a( 3 ) );
	EXPECT_TRUE( eq( FArray1D_int( 10, 22 ), a ) );
	++a( 3 );
	EXPECT_EQ( u( 3 ), 23 );
	EXPECT_EQ( u( 3 ), a( 3 ) );
}

TEST( FArray1Test, ConstProxyConstruct )
{
	FArray1D_int const u( 10, 22 );
	FArray1P_int const a( u );
	EXPECT_EQ( u.I(), a.I() );
	EXPECT_EQ( u( 3 ), a( 3 ) );
	EXPECT_TRUE( eq( FArray1D_int( 10, 22 ), a ) );
}

TEST( FArray1Test, ProxyAttach )
{
	FArray1D_int u( 10, 22 );
	FArray1D_int v( 10, 33 );
	FArray1P_int p;
	p.attach( u ).dimension( 5 );
	EXPECT_TRUE( eq( FArray1D_int( 5, 22 ), p ) );
	p.attach( v );
	EXPECT_TRUE( eq( FArray1D_int( 10, 33 ), p ) );
}

TEST( FArray1Test, ProxyReattachAfterUpdate )
{
	FArray1D_int v( 10, 33 );
	FArray1P_int w( v, 5 ); // This will work: FArray1A_int w( v ) will fail when v is made smaller
	FArray1P_int x( w, 4 );
	EXPECT_TRUE( eq( FArray1D_int( 5, 33 ), w ) );
	EXPECT_TRUE( eq( FArray1D_int( 4, 33 ), x ) );
	v.dimension( 5, 44 );
	EXPECT_TRUE( eq( FArray1D_int( 5, 44 ), v ) );
	EXPECT_TRUE( eq( FArray1D_int( 5, 44 ), w ) );
	EXPECT_TRUE( eq( FArray1D_int( 4, 44 ), x ) );
}

TEST( FArray1Test, ProxyReattachAfterUpdateResizes )
{
	Dimension N( 5 );
	FArray1D_int v( N, 33 );
	FArray1P_int w( v );
	FArray1P_int x( w );
	EXPECT_TRUE( eq( FArray1D_int( 5, 33 ), v ) );
	EXPECT_TRUE( eq( FArray1D_int( 5, 33 ), w ) );
	EXPECT_TRUE( eq( FArray1D_int( 5, 33 ), x ) );
	N = 10;
	EXPECT_TRUE( eq( FArray1D_int( 10, 33 ), v ) );
	EXPECT_TRUE( eq( FArray1D_int( 10, 33 ), w ) );
	EXPECT_TRUE( eq( FArray1D_int( 10, 33 ), x ) );
}

TEST( FArray1Test, Operators )
{
	FArray1D_int A( 3, 33 );
	FArray1A_int B( A );
	FArray1D_int const C( A );
	A += B;
	EXPECT_TRUE( eq( FArray1D_int( 3, 66 ), A ) );
	EXPECT_TRUE( eq( FArray1D_int( 3, 66 ), B ) );
	A += 1;
	EXPECT_TRUE( eq( FArray1D_int( 3, 67 ), A ) );
	EXPECT_TRUE( eq( FArray1D_int( 3, 67 ), B ) );
	A -= 1;
	EXPECT_TRUE( eq( FArray1D_int( 3, 66 ), A ) );
	EXPECT_TRUE( eq( FArray1D_int( 3, 66 ), B ) );
	A -= C;
	EXPECT_TRUE( eq( FArray1D_int( 3, 33 ), A ) );
	EXPECT_TRUE( eq( FArray1D_int( 3, 33 ), B ) );
	A /= 3;
	EXPECT_TRUE( eq( FArray1D_int( 3, 11 ), A ) );
	EXPECT_TRUE( eq( FArray1D_int( 3, 11 ), B ) );
	A *= 3;
	EXPECT_TRUE( eq( FArray1D_int( 3, 33 ), A ) );
	EXPECT_TRUE( eq( FArray1D_int( 3, 33 ), B ) );
	A /= C;
	EXPECT_TRUE( eq( FArray1D_int( 3, 1 ), A ) );
	EXPECT_TRUE( eq( FArray1D_int( 3, 1 ), B ) );
	A *= C;
	EXPECT_TRUE( eq( FArray1D_int( 3, 33 ), A ) );
	EXPECT_TRUE( eq( FArray1D_int( 3, 33 ), B ) );
}

TEST( FArray1Test, Index )
{
	FArray1D_int A( 3, 6 );
	FArray1D_int const C( 3, 6 );

	EXPECT_EQ( 2u, A.index( 3 ) );
	EXPECT_EQ( 5u, A.index( 6 ) );
	EXPECT_EQ( 2u, C.index( 3 ) );
	EXPECT_EQ( 5u, C.index( 6 ) );

	EXPECT_EQ( 1u, A.index( 2 ) );
}

TEST( FArray1Test, OperatorBrackets )
{
	FArray1D_int A( 3 );
	A[ 0 ] = 11;
	A[ 1 ] = 22;
	A[ 2 ] = 33;
	EXPECT_EQ( 11, A[ 0 ] );
	EXPECT_EQ( 22, A[ 1 ] );
	EXPECT_EQ( 33, A[ 2 ] );

	FArray1D_int const C( 3, { 11, 22, 33 } );
	EXPECT_EQ( 11, C[ 0 ] );
	EXPECT_EQ( 22, C[ 1 ] );
	EXPECT_EQ( 33, C[ 2 ] );
}

TEST( FArray1Test, DimensionsInitialized )
{
	FArray1D_int A;
	FArray1D_int B( 3 );
	FArray1D_int C( 3, 20 );

	EXPECT_TRUE( A.dimensions_initialized() );
	EXPECT_TRUE( B.dimensions_initialized() );
	EXPECT_TRUE( C.dimensions_initialized() );

	Dimension N;
	FArray1D_int D( N );
	EXPECT_FALSE( D.dimensions_initialized() );
	N = 5;
	EXPECT_TRUE( D.dimensions_initialized() );
	EXPECT_EQ( 5u, D.size() );
}

TEST( FArray1Test, Contains )
{
	FArray1D_int A( { 11, 22, 33 } );

	EXPECT_TRUE( A.contains( 1 ) && A.contains( 2 ) && A.contains( 3 ) );
	EXPECT_FALSE( ! A.contains( 11 ) && A.contains( 22 ) && A.contains( 33 ) );
	EXPECT_FALSE( A.contains( 4 ) );
	EXPECT_FALSE( A.contains( -1 ) );
}

TEST( FArray1Test, AllocateDeallocate )
{
	FArray1D_double A1;
	EXPECT_EQ( 0u, A1.size() );
	EXPECT_EQ( 1, A1.l() );
	EXPECT_EQ( 0, A1.u() );
	EXPECT_FALSE( A1.allocated() );
	A1.allocate( 3 );
	EXPECT_EQ( 3u, A1.size() );
	EXPECT_EQ( 1, A1.l() );
	EXPECT_EQ( 3, A1.u() );
	EXPECT_TRUE( A1.allocated() );
	A1.deallocate();
	EXPECT_EQ( 0u, A1.size() );
	EXPECT_EQ( 1, A1.l() );
	EXPECT_EQ( 0, A1.u() );
	EXPECT_FALSE( allocated( A1 ) );

	FArray1D_double A2( { 1.1, 2.2, 3.3 } );
	EXPECT_EQ( 3u, A2.size() );
	EXPECT_EQ( 1, A2.l() );
	EXPECT_EQ( 3, A2.u() );
	EXPECT_TRUE( allocated( A2 ) );
	deallocate( A2 );
	EXPECT_EQ( 0u, A1.size() );
	EXPECT_EQ( 1, A1.l() );
	EXPECT_EQ( 0, A1.u() );
	EXPECT_FALSE( allocated( A2 ) );
}

static void dimension_initializer_function( FArray1D_int & A )
{
	for ( int i = A.l(); i <= A.u(); ++i ) {
		A( i ) = i * 10 + i;
	}
}

TEST( FArray1Test, Dimension )
{
	FArray1D_int A( 3 );
	EXPECT_EQ( 1, A.l() );
	EXPECT_EQ( 3, A.u() );
	EXPECT_EQ( 3u, A.size() );

	A.dimension( { 3, 7 } );
	EXPECT_EQ( 3, A.l() );
	EXPECT_EQ( 7, A.u() );
	EXPECT_EQ( 5u, A.size() );

	A.dimension( { 2, 4 }, 17 );
	EXPECT_EQ( 2, A.l() );
	EXPECT_EQ( 4, A.u() );
	EXPECT_EQ( 3u, A.size() );
	EXPECT_EQ( 17, A( 2 ) );
	EXPECT_EQ( 17, A( 3 ) );
	EXPECT_EQ( 17, A( 4 ) );

	A.dimension( { 4, 6 }, dimension_initializer_function );
	EXPECT_EQ( 4, A.l() );
	EXPECT_EQ( 6, A.u() );
	EXPECT_EQ( 3u, A.size() );
	EXPECT_EQ( 44, A( 4 ) );
	EXPECT_EQ( 55, A( 5 ) );
	EXPECT_EQ( 66, A( 6 ) );

	A.dimension( FArray1D_int( { 3, 7 } ) );
	EXPECT_EQ( 3, A.l() );
	EXPECT_EQ( 7, A.u() );
	EXPECT_EQ( 5u, A.size() );

	A.dimension( FArray1D_int( { 2, 4 } ), 17 );
	EXPECT_EQ( 2, A.l() );
	EXPECT_EQ( 4, A.u() );
	EXPECT_EQ( 3u, A.size() );
	EXPECT_EQ( 17, A( 2 ) );
	EXPECT_EQ( 17, A( 3 ) );
	EXPECT_EQ( 17, A( 4 ) );

	A.dimension( FArray1D_int( { 4, 6 } ), dimension_initializer_function );
	EXPECT_EQ( 4, A.l() );
	EXPECT_EQ( 6, A.u() );
	EXPECT_EQ( 3u, A.size() );
	EXPECT_EQ( 44, A( 4 ) );
	EXPECT_EQ( 55, A( 5 ) );
	EXPECT_EQ( 66, A( 6 ) );
}

TEST( FArray1Test, Redimension )
{
	FArray1D_int A( 5, { 11, 22, 33, 44, 55 } );

	EXPECT_EQ( 1, A.l() );
	EXPECT_EQ( 5, A.u() );
	EXPECT_EQ( 5u, A.size() );
	EXPECT_EQ( 11, A( 1 ) );
	EXPECT_EQ( 22, A( 2 ) );
	EXPECT_EQ( 33, A( 3 ) );
	EXPECT_EQ( 44, A( 4 ) );
	EXPECT_EQ( 55, A( 5 ) );

	A.redimension( { 3, 7 } );
	EXPECT_EQ( 3, A.l() );
	EXPECT_EQ( 7, A.u() );
	EXPECT_EQ( 5u, A.size() );
	EXPECT_EQ( 33, A( 3 ) );
	EXPECT_EQ( 44, A( 4 ) );
	EXPECT_EQ( 55, A( 5 ) );

	A.redimension( { 0, 4 }, 17 );
	EXPECT_EQ( 0, A.l() );
	EXPECT_EQ( 4, A.u() );
	EXPECT_EQ( 5u, A.size() );
	EXPECT_EQ( 17, A( 0 ) );
	EXPECT_EQ( 17, A( 1 ) );
	EXPECT_EQ( 17, A( 2 ) );
	EXPECT_EQ( 33, A( 3 ) );
	EXPECT_EQ( 44, A( 4 ) );

	FArray1D_int B( 5, { 11, 22, 33, 44, 55 } );

	B.redimension( FArray1D_int( { 3, 7 } ) );
	EXPECT_EQ( 3, B.l() );
	EXPECT_EQ( 7, B.u() );
	EXPECT_EQ( 5u, B.size() );
	EXPECT_EQ( 33, B( 3 ) );
	EXPECT_EQ( 44, B( 4 ) );
	EXPECT_EQ( 55, B( 5 ) );

	B.redimension( FArray1D_int( { 0, 4 } ), 17 );
	EXPECT_EQ( 0, B.l() );
	EXPECT_EQ( 4, B.u() );
	EXPECT_EQ( 5u, B.size() );
	EXPECT_EQ( 17, B( 0 ) );
	EXPECT_EQ( 17, B( 1 ) );
	EXPECT_EQ( 17, B( 2 ) );
	EXPECT_EQ( 33, B( 3 ) );
	EXPECT_EQ( 44, B( 4 ) );

	{
		FArray1D_int A( 5, 1 );
		A.redimension( { 1, 5 }, 2 );
		EXPECT_EQ( 1, A.l() );
		EXPECT_EQ( 5, A.u() );
		EXPECT_TRUE( eq( A, 1 ) );
	}

	{
		FArray1D_int A( 5, 1 );
		A.redimension( { 2, 4 }, 2 );
		EXPECT_EQ( 2, A.l() );
		EXPECT_EQ( 4, A.u() );
		EXPECT_TRUE( eq( A, 1 ) );
	}

	{
		FArray1D_int A( 5, 1 );
		A.redimension( { -2, 0 }, 2 );
		EXPECT_EQ( -2, A.l() );
		EXPECT_EQ( 0, A.u() );
		EXPECT_TRUE( eq( A, 2 ) );
	}

	{
		FArray1D_int A( 5, 1 );
		A.redimension( { 7, 9 }, 2 );
		EXPECT_EQ( 7, A.l() );
		EXPECT_EQ( 9, A.u() );
		EXPECT_TRUE( eq( A, 2 ) );
	}

	{
		FArray1D_int A( 2, 1 );
		A.redimension( { -1, 4 }, 2 );
		EXPECT_EQ( -1, A.l() );
		EXPECT_EQ( 4, A.u() );
		EXPECT_EQ( 2, A( -1 ) );
		EXPECT_EQ( 2, A( 0 ) );
		EXPECT_EQ( 1, A( 1 ) );
		EXPECT_EQ( 1, A( 2 ) );
		EXPECT_EQ( 2, A( 3 ) );
		EXPECT_EQ( 2, A( 4 ) );
	}

	{
		FArray1D_int A( 2, 1 );
		A.redimension( { -1, 2 }, 2 );
		EXPECT_EQ( -1, A.l() );
		EXPECT_EQ( 2, A.u() );
		EXPECT_EQ( 2, A( -1 ) );
		EXPECT_EQ( 2, A( 0 ) );
		EXPECT_EQ( 1, A( 1 ) );
		EXPECT_EQ( 1, A( 2 ) );
	}

	{
		FArray1D_int A( 2, 1 );
		A.redimension( { -1, 1 }, 2 );
		EXPECT_EQ( -1, A.l() );
		EXPECT_EQ( 1, A.u() );
		EXPECT_EQ( 2, A( -1 ) );
		EXPECT_EQ( 2, A( 0 ) );
		EXPECT_EQ( 1, A( 1 ) );
	}

	{
		FArray1D_int A( 2, 1 );
		A.redimension( { 2, 4 }, 2 );
		EXPECT_EQ( 2, A.l() );
		EXPECT_EQ( 4, A.u() );
		EXPECT_EQ( 1, A( 2 ) );
		EXPECT_EQ( 2, A( 3 ) );
		EXPECT_EQ( 2, A( 4 ) );
	}
}

TEST( FArray1Test, Swap )
{
	FArray1D_int A( 4, 11 );
	FArray1P_int P( A );
	EXPECT_TRUE( eq( A, P ) );
	FArray1D_int( 5, 22 ).swap( A );
	EXPECT_EQ( DRange( 1, 5 ), A.I() );
	EXPECT_EQ( 5u, A.size() );
	for ( int i = A.l(); i <= A.u(); ++i ) {
		EXPECT_EQ( 22, A( i ) );
	}
	for ( int i = P.l(); i <= P.u(); ++i ) { // Proxy should reattach after swap
		EXPECT_EQ( A( i ), P( i ) );
	}
	FArray1P_int P2( A, 4 );
	EXPECT_TRUE( eq( P, P2 ) );
}

TEST( FArray1Test, Functions )
{
	FArray1D_int u{ 1, 2, 3 };
	FArray1D_int v{ 2, 3, 4 };
	EXPECT_EQ( 14, magnitude_squared( u ) );
	EXPECT_EQ( 3, distance_squared( u, v ) );
	EXPECT_EQ( 20, dot( u, v ) );
}

TEST( FArray1Test, Dot )
{
	FArray1D_int A( 3 );
	FArray1D_int B( 3 );
	A( 1 ) = 4;
	A( 2 ) = 3;
	A( 3 ) = 5;
	B( 1 ) = 7;
	B( 2 ) = 5;
	B( 3 ) = 4;
	EXPECT_EQ( 63, dot( A, B ) );
	EXPECT_EQ( 63, dot( B, A ) );
	EXPECT_EQ( 63, dot_product( A, B ) );
	EXPECT_EQ( 63, dot_product( B, A ) );
}

TEST( FArray1Test, EoshiftPos )
{
	FArray1D_int A( 5, { 1, 2, 3, 4, 5 } );
	EXPECT_TRUE( eq( FArray1D_int( { 3, 4, 5, 0, 0 } ), eoshift( A, 2 ) ) );
	EXPECT_TRUE( eq( FArray1D_int( { 3, 4, 5, 9, 9 } ), eoshift( A, 2, 9 ) ) );
}

TEST( FArray1Test, EoshiftNeg )
{
	FArray1D_int A( 5, { 1, 2, 3, 4, 5 } );
	EXPECT_TRUE( eq( FArray1D_int( { 0, 0, 1, 2, 3 } ), eoshift( A, -2 ) ) );
	EXPECT_TRUE( eq( FArray1D_int( { 8, 8, 1, 2, 3 } ), eoshift( A, -2, 8 ) ) );
}

TEST( FArray1Test, CshiftPos )
{
	FArray1D_int A( 5, { 1, 2, 3, 4, 5 } );
	EXPECT_TRUE( eq( FArray1D_int( { 3, 4, 5, 1, 2 } ), cshift( A, 2 ) ) );
}

TEST( FArray1Test, CshiftNeg )
{
	FArray1D_int A( 5, { 1, 2, 3, 4, 5 } );
	EXPECT_TRUE( eq( FArray1D_int( { 4, 5, 1, 2, 3 } ), cshift( A, -2 ) ) );
}

TEST( FArray1Test, Cross )
{
	FArray1D_int A( 3, 33 ), B( 44 - A );
	EXPECT_TRUE( eq( cross( A, B ), cross_product( A, B ) ) );
}

TEST( FArray1Test, ProxyConstCorrectness )
{
	FArray1D_int const v( 3, 33 );
	FArray1P_int p( v ); // Proxy for const array
#ifndef OBJEXXFCL_PROXY_CONST_CHECKS
	EXPECT_EQ( 33, p( 1 ) ); // Lookup triggers assert fail with const proxy checks because non-const subscript op used: OK if p is declared const
	p( 2 ) *= 2;
	EXPECT_EQ( 66, p( 2 ) );
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	EXPECT_TRUE( eq( v, p ) );
	FArray1A_int s( v.a( 2 ) ); // Proxy for const array from section of elements (2,3)
	FArray1A_int const & sc( s ); // Const proxy for const array from section of elements (2,3)
	EXPECT_EQ( 33, sc( 2 ) ); // OK because const subscript op used
#ifndef OBJEXXFCL_PROXY_CONST_CHECKS
	EXPECT_EQ( 33, s( 2 ) ); // Lookup triggers assert fail with const proxy checks because non-const subscript op used: OK if p is declared const
	++s( 2 );
	EXPECT_EQ( 34, s( 2 ) ); // Lookup triggers assert fail with const proxy checks because non-const subscript op used: OK if p is declared const
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	EXPECT_EQ( 2u, s.size() ); // Lookup triggers assert fail with const proxy checks because non-const subscript op used: OK if p is declared const
}

TEST( FArray1Test, Generators )
{
	FArray1D_double A( 3, 22.0 ), B( 3, 11.0 );
	EXPECT_TRUE( eq( B, A / 2.0 ) );
	//EXPECT_TRUE( eq( B, A / 2 ) ); // This doesn't compile: Won't convert
}

TEST( FArray1FunctionsTest, AllocateDeallocate )
{
	FArray1D_double A1;
	EXPECT_FALSE( allocated( A1 ) );
	allocate( A1, 3 );
	EXPECT_TRUE( allocated( A1 ) );
	deallocate( A1 );
	EXPECT_FALSE( allocated( A1 ) );

	FArray1D_double A2( { 1.0, 2.01, 3.012 } );
	EXPECT_TRUE( allocated( A2 ) );
	deallocate( A2 );
	EXPECT_FALSE( allocated( A2 ) );
}

TEST( FArray1FunctionsTest, AllAny )
{
	FArray1D_bool A1( { true, true, true } );
	EXPECT_TRUE( all( A1 ) );
	EXPECT_TRUE( any( A1 ) );
	EXPECT_FALSE( all( ! A1 ) );
	EXPECT_FALSE( any( ! A1 ) );

	FArray1D_bool A2( { false, false, false } );
	EXPECT_FALSE( all( A2 ) );
	EXPECT_FALSE( any( A2 ) );
	EXPECT_TRUE( all( ! A2 ) );
	EXPECT_TRUE( any( ! A2 ) );

	FArray1D_bool A3( { false, false, true } );
	EXPECT_FALSE( all( A3 ) );
	EXPECT_TRUE( any( A3 ) );
	EXPECT_FALSE( all( ! A3 ) );
	EXPECT_TRUE( any( ! A3 ) );
}

TEST( FArray1FunctionsTest, Count )
{
	FArray1D_bool A1( { true, true, true, true, true } );
	EXPECT_EQ( 5u, count( A1 ) );
	EXPECT_EQ( 5u, count( A1, 1 ) );

	FArray1D_bool A2( { true, false, true, false, true } );
	EXPECT_EQ( 3u, count( A2 ) );
	EXPECT_EQ( 3u, count( A2, 1 ) );

	FArray1D_bool A3( { false, false, false, false, false } );
	EXPECT_EQ( 0u, count( A3 ) );
	EXPECT_EQ( 0u, count( A3, 1 ) );
}

TEST( FArray1FunctionsTest, IsContiguous )
{
	FArray1D_double A;
	EXPECT_TRUE( is_contiguous( A ) );
}

TEST( FArray1FunctionsTest, LUBound )
{
	FArray1D_double A( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	FArray1D_int E11( 1, { 1.0 } );
	FArray1D_int E12( 1, { 5.0 } );
	EXPECT_TRUE( eq( E11, lbound( A ) ) );
	EXPECT_TRUE( eq( E12, ubound( A ) ) );
	int const E21 = 1.0;
	int const E22 = 5.0;
	EXPECT_EQ( E21, lbound( A, 1 ) );
	EXPECT_EQ( E22, ubound( A, 1 ) );
}

TEST( FArray1FunctionsTest, Shape )
{
	FArray1D_double A( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	FArray1D_int E1( 1, 5 );
	EXPECT_TRUE( eq( E1, shape( A ) ) );
}

TEST( FArray1FunctionsTest, Size )
{
	int const N = 10;
	FArray1D_double A( N );
	EXPECT_EQ( unsigned( N ), size( A ) );
	EXPECT_EQ( unsigned( N ), size( A, 1 ) );
	EXPECT_EQ( A.size(), size( A ) );
}

TEST( FArray1FunctionsTest, ISize )
{
	int const N = 10;
	FArray1D_double A( N );
	EXPECT_EQ( N, isize( A ) );
	EXPECT_EQ( N, isize( A, 1 ) );
	EXPECT_EQ( int( size( A ) ), isize( A ) );
}

TEST( FArray1FunctionsTest, Reshape )
{
	FArray1D_double A( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	EXPECT_EQ( 5u, A.size() );
	FArray1D_double E( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	EXPECT_TRUE( eq( E, reshape( A, std::array< int, 1 >{ { 5 } } ) ) );
	EXPECT_TRUE( eq( E, reshape( { 1.0, 2.0, 3.0, 4.0, 5.0 }, std::array< int, 1 >{ { 5 } } ) ) );
	EXPECT_TRUE( eq( E, reshape1( A, FArray1D_int( 1, { 5 } ) ) ) );
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 bug work-around
	EXPECT_TRUE( eq( E, reshape1( A, std::initializer_list< int >( { 5 } ) ) ) );
#else
	EXPECT_TRUE( eq( E, reshape1( A, { 5 } ) ) );
#endif
	EXPECT_TRUE( eq( E, reshape1( { 1.0, 2.0, 3.0, 4.0, 5.0 }, FArray1D_int( 1, { 5 } ) ) ) );
}

TEST( FArray1FunctionsTest, Pack )
{
	FArray1D_double A1;
	FArray1D_double const E1;
	EXPECT_TRUE( eq( E1, pack( A1, false ) ) );
	EXPECT_TRUE( eq( E1, pack( A1, true ) ) );

	FArray1D_double A2( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	FArray1D_double const E2( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	EXPECT_TRUE( eq( E1, pack( A2, false ) ) );
	EXPECT_TRUE( eq( E2, pack( A2, true ) ) );

	FArray1D_double A3( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	FArray1D_double const E3( { 2.0, 4.0 } );
	FArray1D_bool const M( { false, true, false, true, false } );
	EXPECT_TRUE( eq( E3, pack( A3, M ) ) );
}

TEST( FArray1FunctionsTest, CShift )
{
	FArray1D_double A( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	EXPECT_TRUE( eq( A, cshift( A, 0 ) ) );
	EXPECT_TRUE( eq( A, cshift( A, 0, 1 ) ) );

	FArray1D_double const E1( { 2.0, 3.0, 4.0, 5.0, 1.0 } );
	EXPECT_TRUE( eq( E1, cshift( A, 1 ) ) );
	EXPECT_TRUE( eq( E1, cshift( A, 1, 1 ) ) );

	FArray1D_double const E2( { 3.0, 4.0, 5.0, 1.0, 2.0 } );
	EXPECT_TRUE( eq( E2, cshift( A, 2 ) ) );
	EXPECT_TRUE( eq( E2, cshift( A, 2, 1 ) ) );

	FArray1D_double const E3( { 5.0, 1.0, 2.0, 3.0, 4.0 } );
	EXPECT_TRUE( eq( E3, cshift( A, -1 ) ) );
	EXPECT_TRUE( eq( E3, cshift( A, -1, 1 ) ) );
}

TEST( FArray1FunctionsTest, EOShift )
{
	FArray1D_double A( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	EXPECT_TRUE( eq( A, eoshift( A, 0 ) ) );

	FArray1D_double const E11( { 2.0, 3.0, 4.0, 5.0, 0.0 } );
	FArray1D_double const E12( { 2.0, 3.0, 4.0, 5.0, 17.0 } );
	EXPECT_TRUE( eq( E11, eoshift( A, 1 ) ) );
	EXPECT_TRUE( eq( E12, eoshift( A, 1, 17.0 ) ) );
	EXPECT_TRUE( eq( E11, eoshift( A, 1, 0.0, 1 ) ) );

	FArray1D_double const E21( { 3.0, 4.0, 5.0, 0.0, 0.0 } );
	FArray1D_double const E22( { 3.0, 4.0, 5.0, 17.0, 17.0 } );
	EXPECT_TRUE( eq( E21, eoshift( A, 2 ) ) );
	EXPECT_TRUE( eq( E22, eoshift( A, 2, 17.0 ) ) );
	EXPECT_TRUE( eq( E21, eoshift( A, 2, 0.0, 1 ) ) );

	FArray1D_double const E31( { 0.0, 1.0, 2.0, 3.0, 4.0 } );
	FArray1D_double const E32( { 17.0, 1.0, 2.0, 3.0, 4.0 } );
	EXPECT_TRUE( eq( E31, eoshift( A, -1 ) ) );
	EXPECT_TRUE( eq( E32, eoshift( A, -1, 17.0 ) ) );
	EXPECT_TRUE( eq( E31, eoshift( A, -1, 0.0, 1 ) ) );
}

TEST( FArray1FunctionsTest, Sum )
{
	FArray1D_double A( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	double const E1 = 15.0;
	EXPECT_TRUE( E1 == sum( A ) );
	EXPECT_TRUE( E1 == sum( A, 1 ) );
	FArray1D_bool M( { true, false, true, false, true } );
	double const E2 = 9.0;
	EXPECT_TRUE( E2 == sum( A, M ) );
}

TEST( FArray1FunctionsTest, Product )
{
	FArray1D_double A( { 1.0, 2.0, 3.0, 4.0, 5.0 } );
	double const E1 = 120.0;
	EXPECT_TRUE( E1 == product( A ) );
	EXPECT_TRUE( E1 == product( A, 1 ) );
	FArray1D_bool M( { true, false, true, false, true } );
	double const E2 = 15.0;
	EXPECT_TRUE( E2 == product( A, M ) );
}

TEST( FArray1FunctionsTest, Abs )
{
	FArray1D_int A( { -1, -2, -3 } );
	FArray1D_int const E( { 1, 2, 3 } );
	EXPECT_TRUE( eq( E, abs( A ) ) );
}

TEST( FArray1FunctionsTest, Pow )
{
	FArray1D_int A( { 1, 2, 3 } );
	FArray1D_int const E( { 1, 4, 9 } );
	EXPECT_TRUE( eq( E, pow( A, 2 ) ) );
}

TEST( FArray1FunctionsTest, Sign )
{
	FArray1D_int A1( { 1, 2, 3 } );
	FArray1D_int const E11( { 1, 2, 3 } );
	FArray1D_int const E12( { -1, -2, -3 } );
	EXPECT_TRUE( eq( E11, sign( A1, 1 ) ) );
	EXPECT_TRUE( eq( E11, sign( A1, 0 ) ) );
	EXPECT_TRUE( eq( E12, sign( A1, -1 ) ) );

	FArray1D_int A2( { -1, -2, -3 } );
	FArray1D_int const E21( { 1, 2, 3 } );
	FArray1D_int const E22( { -1, -2, -3 } );
	EXPECT_TRUE( eq( E21, sign( A2, 1 ) ) );
	EXPECT_TRUE( eq( E21, sign( A2, 0 ) ) );
	EXPECT_TRUE( eq( E22, sign( A2, -1 ) ) );

	FArray1D_int A3( { 1, -2, 3 } );
	FArray1D_int const E31( { 1, -1, 1 } );
	FArray1D_int const E32( { 0, 0, 0 } );
	EXPECT_TRUE( eq( E31, sign( 1,  A3 ) ) );
	EXPECT_TRUE( eq( E32, sign( 0,  A3 ) ) );
	EXPECT_TRUE( eq( E31, sign( -1, A3 ) ) );
}

TEST( FArray1FunctionsTest, MinVal )
{
	FArray1D_int A( { -1000, -1, 0, 1, 1000 } );
	int const E = -1000;
	EXPECT_EQ( E, minval( A ) );
}

TEST( FArray1FunctionsTest, MaxVal )
{
	FArray1D_int A( { -1000, -1, 0, 1, 1000 } );
	int const E = 1000;
	EXPECT_EQ( E, maxval( A ) );
}

TEST( FArray1FunctionsTest, MinLoc )
{
	FArray1D_int A1( { 1, 2, 3 } );
	int const I1 = 1;
	FArray1D_int const E1( 1, I1 );
	EXPECT_TRUE( eq( E1, minloc( A1 ) ) );
	EXPECT_EQ( I1, minloc( A1, 1 ) );

	FArray1D_int A2( { 3, 2, 1 } );
	int const I2 = 3;
	FArray1D_int const E2( 1, I2 );
	EXPECT_TRUE( eq( E2, minloc( A2 ) ) );
	EXPECT_EQ( I2, minloc( A2, 1 ) );

	FArray1D_int A3( { 3, 2, 1, 2, 1 } );
	int const I3 = 3;
	FArray1D_int const E3( 1, I3 );
	EXPECT_TRUE( eq( E3, minloc( A3 ) ) );
	EXPECT_EQ( I3, minloc( A3, 1 ) );

	FArray1D_int A4( { 1, 2, 3, -2, -1 } );
	int const I4 = 4;
	FArray1D_int const E4( 1, I4 );
	EXPECT_TRUE( eq( E4, minloc( A4 ) ) );
	EXPECT_EQ( I4, minloc( A4, 1 ) );
}

TEST( FArray1FunctionsTest, MaxLoc )
{
	FArray1D_int A1( { 1, 2, 3 } );
	int const I1 = 3;
	FArray1D_int const E1( 1, I1 );
	EXPECT_TRUE( eq( E1, maxloc( A1 ) ) );
	EXPECT_EQ( I1, maxloc( A1, 1 ) );

	FArray1D_int A2( { 3, 2, 1 } );
	int const I2 = 1;
	FArray1D_int const E2( 1, I2 );
	EXPECT_TRUE( eq( E2, maxloc( A2 ) ) );
	EXPECT_EQ( I2, maxloc( A2, 1 ) );

	FArray1D_int A3( { 1, 2, 3, 2, 3 } );
	int const I3 = 3;
	FArray1D_int const E3( 1, I3 );
	EXPECT_TRUE( eq( E3, maxloc( A3 ) ) );
	EXPECT_EQ( I3, maxloc( A3, 1 ) );

	FArray1D_int A4( { 1, 2, 3, 4, 3 } );
	int const I4 = 4;
	FArray1D_int const E4( 1, I4 );
	EXPECT_TRUE( eq( E4, maxloc( A4 ) ) );
	EXPECT_EQ( I4, maxloc( A4, 1 ) );
}

TEST( FArray1FunctionsTest, MatMul )
{
	FArray1D_int A11( 2, { 2, 3 } );
	FArray1D_int A12( 2, { 5, 7 } );
	FArray2D_int E1( 2, 2, reshape( { 10, 15, 14, 21 }, std::array< int, 2 >{ { 2, 2 } } ) );

	EXPECT_TRUE( eq( E1, matmul( A11, A12 ) ) );

	FArray1D_bool A21( 2, { true, false } );
	FArray1D_bool A22( 2, { false, true } );
	FArray2D_bool E2( 2, 2, reshape( { false, false, true, false }, std::array< int, 2 >{ { 2, 2 } } ) );

	EXPECT_TRUE( eq( E2, matmul( A21, A22 ) ) );
}
