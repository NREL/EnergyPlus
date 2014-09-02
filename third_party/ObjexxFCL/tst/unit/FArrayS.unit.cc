// ObjexxFCL::FArrayS Unit Tests
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
#include <ObjexxFCL/FArrayS.all.hh>
#include <ObjexxFCL/FArrayS.all.io.hh>
#include <ObjexxFCL/FArray1A.hh>
#include <ObjexxFCL/FArray2D.hh>
#include <ObjexxFCL/FArray3D.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( FArraySTest, FArray1SBasic )
{
	FArray1D_int a( 5, {1,2,3,4,5} );
	FArray1S_int s( a( {2,3} ) );
	EXPECT_EQ( 2u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 2, s.u() );
	EXPECT_EQ( 2, s( 1 ) );
	EXPECT_EQ( 3, s( 2 ) );
}

TEST( FArraySTest, FArray1SSingleIndexSlice )
{
	FArray1D_int a( 5, {1,2,3,4,5} );
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 bug work-around
	FArray1S_int s( a( {2,_} ) ); // 2 acts as the lower index
#else
	FArray1S_int s( a( {2} ) ); // 2 acts as the lower index
#endif
	EXPECT_EQ( 4u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 4, s.u() );
	EXPECT_EQ( 2, s( 1 ) );
	EXPECT_EQ( 3, s( 2 ) );
	EXPECT_EQ( 4, s( 3 ) );
	EXPECT_EQ( 5, s( 4 ) );
}

TEST( FArraySTest, FArray1SEmptySlice )
{
	FArray1D_int a( 5, {1,2,3,4,5} );
	FArray1S_int s( a( {2,-2} ) ); // Empty slice
	EXPECT_EQ( 0u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 0, s.u() );
	EXPECT_EQ( 0u, s.size() );
}

TEST( FArraySTest, FArray1SEmptySliceOfEmptyArray )
{
	FArray1D_int a( {5,-5} ); // Empty array: Can only slice with empty slice
	FArray1S_int s( a( {2,-2} ) ); // Empty slice: Legal
	EXPECT_EQ( 0u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 0, s.u() );
	EXPECT_EQ( 0u, s.size() );
}

TEST( FArraySTest, FArray1SSliceOfUnboundedArray )
{
	FArray1D_int a( 5, {1,2,3,4,5} );
	FArray1A_int u( a( 2 ) );
	//u.dim( {1,_} ); // This is the default behavior
	FArray1S_int s( u( {2,4} ) );
	EXPECT_EQ( 3u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 3, s.u() );
	EXPECT_EQ( 3, s( 1 ) );
	EXPECT_EQ( 4, s( 2 ) );
	EXPECT_EQ( 5, s( 3 ) );
}

TEST( FArraySTest, FArray2SSingleIndexSlice )
{
	FArray2D_int A( 2, 2, { 11, 21, 12, 22 } );
	FArray2S_int S( A( {2,_}, {1,_} ) );
	EXPECT_EQ( 2u, S.size() );
	EXPECT_EQ( 1, S.l1() );
	EXPECT_EQ( 1, S.u1() );
	EXPECT_EQ( 1, S.l2() );
	EXPECT_EQ( 2, S.u2() );
	EXPECT_EQ( 21, S( 1, 1 ) );
	EXPECT_EQ( 22, S( 1, 2 ) );
}

TEST( FArraySTest, FArray2D1SSlice )
{
	FArray2D_int A( 2, 2, { 11, 21, 12, 22 } );
	FArray1S_int S( A( _, 2 ) );
	EXPECT_EQ( 2u, S.size() );
	EXPECT_EQ( 1, S.l() );
	EXPECT_EQ( 2, S.u() );
	EXPECT_EQ( 1, S.l1() );
	EXPECT_EQ( 2, S.u1() );
	EXPECT_EQ( 12, S( 1 ) );
	EXPECT_EQ( 22, S( 2 ) );
}

TEST( FArraySTest, FArray2D1DOTFSlice )
{
	FArray2D_int A( 2, 2, { 11, 21, 12, 22 } );
	FArray1D_int B( A( {1,2}, 2 ) ); // Make a real 1D array out of slice on the fly
	EXPECT_EQ( 1, B.l() );
	EXPECT_EQ( 2, B.u() );
	EXPECT_EQ( 12, B( 1 ) );
	EXPECT_EQ( 22, B( 2 ) );
}

TEST( FArraySTest, FArray1SWholeArraySlice )
{
	FArray1D_int a( {-2,2}, {1,2,3,4,5} );
	FArray1S_int s( a );
	EXPECT_EQ( 5u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 5, s.u() );
	EXPECT_EQ( 1, s( 1 ) );
	EXPECT_EQ( 2, s( 2 ) );
	EXPECT_EQ( 3, s( 3 ) );
	EXPECT_EQ( 4, s( 4 ) );
	EXPECT_EQ( 5, s( 5 ) );
}

TEST( FArraySTest, FArray2SWholeArraySlice )
{
	FArray2D_int a( {-1,1}, {-1,1}, {1,2,3,4,5,6,7,8,9} );
	FArray2S_int s( a );
	EXPECT_EQ( 9u, s.size() );
	EXPECT_EQ( 1, s.l1() );
	EXPECT_EQ( 1, s.l2() );
	EXPECT_EQ( 3, s.u1() );
	EXPECT_EQ( 3, s.u2() );
	EXPECT_EQ( 1, s( 1, 1 ) );
	EXPECT_EQ( 2, s( 2, 1 ) );
	EXPECT_EQ( 3, s( 3, 1 ) );
	EXPECT_EQ( 4, s( 1, 2 ) );
	EXPECT_EQ( 5, s( 2, 2 ) );
	EXPECT_EQ( 6, s( 3, 2 ) );
	EXPECT_EQ( 7, s( 1, 3 ) );
	EXPECT_EQ( 8, s( 2, 3 ) );
	EXPECT_EQ( 9, s( 3, 3 ) );
}

TEST( FArraySTest, FArray3SWholeArraySlice )
{
	FArray3D_int a( {-1,0}, {1,2}, {0,1}, {1,2,3,4,5,6,7,8} );
	FArray3S_int s( a );
	EXPECT_EQ( 8u, s.size() );
	EXPECT_EQ( 1, s.l1() );
	EXPECT_EQ( 1, s.l2() );
	EXPECT_EQ( 1, s.l3() );
	EXPECT_EQ( 2, s.u1() );
	EXPECT_EQ( 2, s.u2() );
	EXPECT_EQ( 2, s.u3() );
	EXPECT_EQ( 1, s( 1, 1, 1 ) );
	EXPECT_EQ( 2, s( 2, 1, 1 ) );
	EXPECT_EQ( 3, s( 1, 2, 1 ) );
	EXPECT_EQ( 4, s( 2, 2, 1 ) );
	EXPECT_EQ( 5, s( 1, 1, 2 ) );
	EXPECT_EQ( 6, s( 2, 1, 2 ) );
	EXPECT_EQ( 7, s( 1, 2, 2 ) );
	EXPECT_EQ( 8, s( 2, 2, 2 ) );
}

TEST( FArraySTest, FArray2SSlice3D )
{
// Expected results from this Fortran program
//PROGRAM main
//  INTEGER :: M(9,9,9)
//  INTEGER :: S(4,4)
//  INTEGER :: V(4)
//  i = 0
//  DO i3 = 1, UBOUND( M, 3 )
//    DO i2 = 1, UBOUND( M, 2 )
//      DO i1 = 1, UBOUND( M, 1 )
//        M(i1,i2,i3) = i ! Memory offset values
//        i = i + 1
//      END DO
//    END DO
//  END DO
//  S = M(7,8:2:-2,8:2:-2)
//  PRINT *
//  DO i1 = 1, UBOUND( S, 1 )
//    PRINT *, S(i1,:)
//  END DO
//  V = S(3,:)
//  PRINT *
//  PRINT *, V
//END PROGRAM

	FArray3D_int M( 9, 9, 9 );
	for ( FArray3D_int::size_type i = 0; i < 9*9*9; ++i ) M[ i ] = static_cast< int >( i ); // Memory offset values

	FArray2S_int S( M( 7, {8,2,-2}, {8,2,-2} ) );
	EXPECT_EQ( 1, S.l1() );
	EXPECT_EQ( 4, S.u1() );
	EXPECT_EQ( 4u, S.size1() );
	EXPECT_EQ( 1, S.l2() );
	EXPECT_EQ( 4, S.u2() );
	EXPECT_EQ( 4u, S.size2() );
	EXPECT_EQ( 16u, S.size() );

	EXPECT_EQ( 636, S( 1, 1 ) );
	EXPECT_EQ( 474, S( 1, 2 ) );
	EXPECT_EQ( 312, S( 1, 3 ) );
	EXPECT_EQ( 150, S( 1, 4 ) );

	EXPECT_EQ( 618, S( 2, 1 ) );
	EXPECT_EQ( 456, S( 2, 2 ) );
	EXPECT_EQ( 294, S( 2, 3 ) );
	EXPECT_EQ( 132, S( 2, 4 ) );

	EXPECT_EQ( 600, S( 3, 1 ) );
	EXPECT_EQ( 438, S( 3, 2 ) );
	EXPECT_EQ( 276, S( 3, 3 ) );
	EXPECT_EQ( 114, S( 3, 4 ) );

	EXPECT_EQ( 582, S( 4, 1 ) );
	EXPECT_EQ( 420, S( 4, 2 ) );
	EXPECT_EQ( 258, S( 4, 3 ) );
	EXPECT_EQ(  96, S( 4, 4 ) );

	FArray1S_int r( S( 3, _ ) ); // 3rd row of S
	EXPECT_EQ( 1, r.l() );
	EXPECT_EQ( 4, r.u() );
	EXPECT_EQ( 4u, r.size() );

	EXPECT_EQ( 600, r( 1 ) );
	EXPECT_EQ( 438, r( 2 ) );
	EXPECT_EQ( 276, r( 3 ) );
	EXPECT_EQ( 114, r( 4 ) );
}

TEST( FArraySTest, AnyOp2D )
{
	FArray2D_int const A( 3, 3, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	FArray2S_int S( A );
	EXPECT_TRUE( any_eq( S, 6 ) );
	EXPECT_FALSE( any_eq( S, 22 ) );
	EXPECT_TRUE( any_ne( S, 6 ) );
	EXPECT_TRUE( any_lt( S, 2 ) );
	EXPECT_TRUE( any_ge( S, 9 ) );
	EXPECT_FALSE( any_lt( S, 1 ) );
	EXPECT_FALSE( any_gt( S, 9 ) );
}

TEST( FArraySTest, AllOp2D )
{
	FArray2D_int const A( 3, 3, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	FArray2S_int S( A );
	EXPECT_FALSE( all_eq( S, 6 ) );
	EXPECT_FALSE( all_eq( S, 22 ) );
	EXPECT_TRUE( all_ne( S, 22 ) );
	EXPECT_FALSE( all_ne( S, 2 ) );
	EXPECT_FALSE( all_lt( S, 2 ) );
	EXPECT_FALSE( all_ge( S, 9 ) );
	EXPECT_TRUE( all_lt( S, 11 ) );
	EXPECT_TRUE( all_gt( S, 0 ) );
}

TEST( FArraySTest, CountOp2D )
{
	FArray2D_int const A( 3, 3, { 1, 2, 2, 3, 3, 3, 7, 8, 9 } );
	FArray2S_int S( A );
	EXPECT_EQ( 0u, count_eq( S, 0 ) );
	EXPECT_EQ( 1u, count_eq( S, 1 ) );
	EXPECT_EQ( 2u, count_eq( S, 2 ) );
	EXPECT_EQ( 3u, count_eq( S, 3 ) );
	EXPECT_EQ( 6u, count_lt( S, 7 ) );
	EXPECT_EQ( 1u, count_ge( S, 9 ) );
	EXPECT_EQ( 9u, count_lt( S, 11 ) );
	EXPECT_EQ( 3u, count_gt( S, 3 ) );
}

TEST( FArraySTest, Functions1D )
{
	FArray1D_int U{ 1, 2, 3 };
	FArray1D_int V{ 2, 3, 4 };
	FArray1S_int u( U );
	FArray1S_int v( V );
	EXPECT_EQ( 14, magnitude_squared( u ) );
	EXPECT_EQ( 3, distance_squared( u, v ) );
	EXPECT_EQ( 20, dot( u, v ) );
}

TEST( FArraySTest, StreamOut )
{
	FArray1D_int const A( 3, { 1, 2, 3 } );
	FArray1S_int S( A );
	std::ostringstream stream;
	stream << S;
	EXPECT_EQ( "           1            2            3", stream.str() );
}

TEST( FArraySTest, StreamIn )
{
	FArray1D_int const A( 3, { 1, 2, 3 } );
	FArray1S_int S( A );
	std::string const text( "1  2  3" );
	std::istringstream stream( text );
	stream >> S;
	EXPECT_TRUE( eq( A, S ) );
}
