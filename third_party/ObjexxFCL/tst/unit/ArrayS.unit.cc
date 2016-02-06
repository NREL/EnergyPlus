// ObjexxFCL::ArrayS Unit Tests
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
#include <ObjexxFCL/ArrayS.all.hh>
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array3A.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( ArraySTest, Array1SBasic )
{
	Array1D_int a( 5, { 1, 2, 3, 4, 5 } );
	Array1S_int s( a( {2,3} ) );
	EXPECT_EQ( 2u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 2, s.u() );
	EXPECT_EQ( 2, s( 1 ) );
	EXPECT_EQ( 3, s( 2 ) );
}

TEST( ArraySTest, Array1SSingleIndexSlice )
{
	Array1D_int a( 5, { 1, 2, 3, 4, 5 } );
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 bug work-around
	Array1S_int s( a( {2,_} ) ); // 2 acts as the lower index
#else
	Array1S_int s( a( {2} ) ); // 2 acts as the lower index
#endif
	EXPECT_EQ( 4u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 4, s.u() );
	EXPECT_EQ( 2, s( 1 ) );
	EXPECT_EQ( 3, s( 2 ) );
	EXPECT_EQ( 4, s( 3 ) );
	EXPECT_EQ( 5, s( 4 ) );
}

TEST( ArraySTest, Array1SEmptySlice )
{
	Array1D_int a( 5, { 1, 2, 3, 4, 5 } );
	Array1S_int s( a( {2,-2} ) ); // Empty slice
	EXPECT_EQ( 0u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 0, s.u() );
	EXPECT_EQ( 0u, s.size() );
}

TEST( ArraySTest, Array1SEmptySliceOfEmptyArray )
{
	Array1D_int a( {5,-5} ); // Empty array: Can only slice with empty slice
	Array1S_int s( a( {2,-2} ) ); // Empty slice: Legal
	EXPECT_EQ( 0u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 0, s.u() );
	EXPECT_EQ( 0u, s.size() );
}

TEST( ArraySTest, Array1SSliceOfUnboundedArray )
{
	Array1D_int a( 5, { 1, 2, 3, 4, 5 } );
	Array1A_int u( a( 2 ) );
	//u.dim( {1,_} ); // This is the default behavior
	Array1S_int s( u( {2,4} ) );
	EXPECT_EQ( 3u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 3, s.u() );
	EXPECT_EQ( 3, s( 1 ) );
	EXPECT_EQ( 4, s( 2 ) );
	EXPECT_EQ( 5, s( 3 ) );
}

TEST( ArraySTest, Array2SSingleIndexSlice )
{
	Array2D_int A( 2, 2, { 11, 12, 21, 22 } );
	Array2S_int S( A( {2,_}, {1,_} ) );
	EXPECT_EQ( 2u, S.size() );
	EXPECT_EQ( 1, S.l1() );
	EXPECT_EQ( 1, S.u1() );
	EXPECT_EQ( 1, S.l2() );
	EXPECT_EQ( 2, S.u2() );
	EXPECT_EQ( 21, S( 1, 1 ) );
	EXPECT_EQ( 22, S( 1, 2 ) );
}

TEST( ArraySTest, Array2D1SSlice )
{
	Array2D_int A( 2, 2, { 11, 12, 21, 22 } );
	Array1S_int S( A( _, 2 ) );
	EXPECT_EQ( 2u, S.size() );
	EXPECT_EQ( 1, S.l() );
	EXPECT_EQ( 2, S.u() );
	EXPECT_EQ( 1, S.l1() );
	EXPECT_EQ( 2, S.u1() );
	EXPECT_EQ( 12, S( 1 ) );
	EXPECT_EQ( 22, S( 2 ) );
}

TEST( ArraySTest, Array2D1SSlice65 )
{
	Array2D_int A( 6, 5, 999 );
	Array1S_int S( A( _, 2 ) );
	EXPECT_EQ( 6u, S.size() );
	EXPECT_EQ( 1, S.l() );
	EXPECT_EQ( 6, S.u() );
	EXPECT_EQ( 1, S.l1() );
	EXPECT_EQ( 6, S.u1() );
}

TEST( ArraySTest, Array2D2SSlice )
{
	Array2D_int A( 3, 3, { 11, 12, 13, 21, 22, 23, 31, 32, 33 } );
	Array2S_int S( A( {1,2}, _ ) );
	EXPECT_EQ( 6u, S.size() );
	EXPECT_EQ( 1, S.l1() );
	EXPECT_EQ( 2, S.u1() );
	EXPECT_EQ( 1, S.l2() );
	EXPECT_EQ( 3, S.u2() );
	EXPECT_EQ( 11, S( 1, 1 ) );
	EXPECT_EQ( 12, S( 1, 2 ) );
	EXPECT_EQ( 13, S( 1, 3 ) );
	EXPECT_EQ( 21, S( 2, 1 ) );
	EXPECT_EQ( 22, S( 2, 2 ) );
	EXPECT_EQ( 23, S( 2, 3 ) );
	Array2A_int P( S ); // OK because slice is contiguous
	EXPECT_EQ( 1, P.l1() );
	EXPECT_EQ( 2, P.u1() );
	EXPECT_EQ( 1, P.l2() );
	EXPECT_EQ( 3, P.u2() );
	EXPECT_EQ( 11, P( 1, 1 ) );
	EXPECT_EQ( 12, P( 1, 2 ) );
	EXPECT_EQ( 13, P( 1, 3 ) );
	EXPECT_EQ( 21, P( 2, 1 ) );
	EXPECT_EQ( 22, P( 2, 2 ) );
	EXPECT_EQ( 23, P( 2, 3 ) );
	EXPECT_DEBUG_DEATH( Array2A_int( A( {1,3,2}, _ ) ), ".*Assertion.*" ); // Can't make arg array from non-contiguous slice
	Array2S_int S2( A( {2,3}, _ ) );
	Array2A_int P2( S2 ); // OK because slice is contiguous
	EXPECT_EQ( 1, P2.l1() );
	EXPECT_EQ( 2, P2.u1() );
	EXPECT_EQ( 1, P2.l2() );
	EXPECT_EQ( 3, P2.u2() );
	EXPECT_EQ( 21, P2( 1, 1 ) );
	EXPECT_EQ( 22, P2( 1, 2 ) );
	EXPECT_EQ( 23, P2( 1, 3 ) );
	EXPECT_EQ( 31, P2( 2, 1 ) );
	EXPECT_EQ( 32, P2( 2, 2 ) );
	EXPECT_EQ( 33, P2( 2, 3 ) );
}

TEST( ArraySTest, Array2D1DOTFSlice )
{
	Array2D_int A( 2, 2, { 11, 12, 21, 22 } );
	Array1D_int B( A( {1,2}, 2 ) ); // Make a real 1D array out of slice on the fly
	EXPECT_EQ( 1, B.l() );
	EXPECT_EQ( 2, B.u() );
	EXPECT_EQ( 12, B( 1 ) );
	EXPECT_EQ( 22, B( 2 ) );
}

TEST( ArraySTest, Array1SWholeArraySlice )
{
	Array1D_int a( {-2,2}, { 1, 2, 3, 4, 5 } );
	Array1S_int s( a );
	EXPECT_EQ( 5u, s.size() );
	EXPECT_EQ( 1, s.l() );
	EXPECT_EQ( 5, s.u() );
	EXPECT_EQ( 1, s( 1 ) );
	EXPECT_EQ( 2, s( 2 ) );
	EXPECT_EQ( 3, s( 3 ) );
	EXPECT_EQ( 4, s( 4 ) );
	EXPECT_EQ( 5, s( 5 ) );
}

TEST( ArraySTest, Array2SWholeArraySlice )
{
	Array2D_int a( {-1,1}, {-1,1}, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	Array2S_int s( a );
	EXPECT_EQ( 9u, s.size() );
	EXPECT_EQ( 1, s.l1() );
	EXPECT_EQ( 1, s.l2() );
	EXPECT_EQ( 3, s.u1() );
	EXPECT_EQ( 3, s.u2() );
	EXPECT_EQ( 1, s( 1, 1 ) );
	EXPECT_EQ( 2, s( 1, 2 ) );
	EXPECT_EQ( 3, s( 1, 3 ) );
	EXPECT_EQ( 4, s( 2, 1 ) );
	EXPECT_EQ( 5, s( 2, 2 ) );
	EXPECT_EQ( 6, s( 2, 3 ) );
	EXPECT_EQ( 7, s( 3, 1 ) );
	EXPECT_EQ( 8, s( 3, 2 ) );
	EXPECT_EQ( 9, s( 3, 3 ) );
}

TEST( ArraySTest, Array3SWholeArraySlice )
{
	Array3D_int a( {-1,0}, {1,2}, {0,1}, { 1, 2, 3, 4, 5, 6, 7, 8 } );
	Array3S_int s( a );
	EXPECT_EQ( 8u, s.size() );
	EXPECT_EQ( 1, s.l1() );
	EXPECT_EQ( 1, s.l2() );
	EXPECT_EQ( 1, s.l3() );
	EXPECT_EQ( 2, s.u1() );
	EXPECT_EQ( 2, s.u2() );
	EXPECT_EQ( 2, s.u3() );
	EXPECT_EQ( 1, s( 1, 1, 1 ) );
	EXPECT_EQ( 2, s( 1, 1, 2 ) );
	EXPECT_EQ( 3, s( 1, 2, 1 ) );
	EXPECT_EQ( 4, s( 1, 2, 2 ) );
	EXPECT_EQ( 5, s( 2, 1, 1 ) );
	EXPECT_EQ( 6, s( 2, 1, 2 ) );
	EXPECT_EQ( 7, s( 2, 2, 1 ) );
	EXPECT_EQ( 8, s( 2, 2, 2 ) );
}

TEST( ArraySTest, Array3D3SSlice )
{
	Array3D_int a( 3, 2, 2, {
	 111,
	 112,
	 121,
	 122,
	 211,
	 212,
	 221,
	 222,
	 311,
	 312,
	 321,
	 322
	} );
	Array3S_int s( a( {2,3}, _, _ ) );
	EXPECT_EQ( 8u, s.size() );
	EXPECT_EQ( 1, s.l1() );
	EXPECT_EQ( 1, s.l2() );
	EXPECT_EQ( 1, s.l3() );
	EXPECT_EQ( 2, s.u1() );
	EXPECT_EQ( 2, s.u2() );
	EXPECT_EQ( 2, s.u3() );
	EXPECT_EQ( 211, s( 1, 1, 1 ) );
	EXPECT_EQ( 322, s( 2, 2, 2 ) );
	Array3A_int p( s ); // OK: Contiguous
	EXPECT_EQ( 8u, p.size() );
	EXPECT_EQ( 1, p.l1() );
	EXPECT_EQ( 1, p.l2() );
	EXPECT_EQ( 1, p.l3() );
	EXPECT_EQ( 2, p.u1() );
	EXPECT_EQ( 2, p.u2() );
	EXPECT_EQ( 2, p.u3() );
	EXPECT_EQ( 211, p( 1, 1, 1 ) );
	EXPECT_EQ( 322, p( 2, 2, 2 ) );
}

TEST( ArraySTest, Array2SSlice3D )
{
// Expected results from this Fortran program
//PROGRAM main
//  INTEGER :: M(9,9,9)
//  INTEGER :: S(4,4)
//  INTEGER :: V(4)
//  i = 0
//  DO i1 = 1, UBOUND( M, 1 )
//    DO i2 = 1, UBOUND( M, 2 )
//      DO i3 = 1, UBOUND( M, 3 )
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

	Array3D_int M( 9, 9, 9 );
	for ( Array3D_int::size_type i = 0; i < 9*9*9; ++i ) M[ i ] = static_cast< int >( i ); // Memory offset values

	Array2S_int S( M( 7, {8,2,-2}, {8,2,-2} ) );
	EXPECT_EQ( 1, S.l1() );
	EXPECT_EQ( 4, S.u1() );
	EXPECT_EQ( 4u, S.size1() );
	EXPECT_EQ( 1, S.l2() );
	EXPECT_EQ( 4, S.u2() );
	EXPECT_EQ( 4u, S.size2() );
	EXPECT_EQ( 16u, S.size() );

	Array2D_int const SF( 4, 4, { // Output from Fortran program
	 556, 554, 552, 550,
	 538, 536, 534, 532,
	 520, 518, 516, 514,
	 502, 500, 498, 496
	} );
	EXPECT_TRUE( eq( S, SF ) );

	Array1S_int r( S( 3, _ ) ); // 3rd row of S
	EXPECT_EQ( 1, r.l() );
	EXPECT_EQ( 4, r.u() );
	EXPECT_EQ( 4u, r.size() );
	EXPECT_TRUE( eq( r, Array1D_int( 4, { 520, 518, 516, 514 } ) ) );
}

TEST( ArraySTest, AnyOp2D )
{
	Array2D_int const A( 3, 3, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	Array2S_int S( A );
	EXPECT_TRUE( any_eq( S, 6 ) );
	EXPECT_FALSE( any_eq( S, 22 ) );
	EXPECT_TRUE( any_ne( S, 6 ) );
	EXPECT_TRUE( any_lt( S, 2 ) );
	EXPECT_TRUE( any_ge( S, 9 ) );
	EXPECT_FALSE( any_lt( S, 1 ) );
	EXPECT_FALSE( any_gt( S, 9 ) );
}

TEST( ArraySTest, AllOp2D )
{
	Array2D_int const A( 3, 3, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	Array2S_int S( A );
	EXPECT_FALSE( all_eq( S, 6 ) );
	EXPECT_FALSE( all_eq( S, 22 ) );
	EXPECT_TRUE( all_ne( S, 22 ) );
	EXPECT_FALSE( all_ne( S, 2 ) );
	EXPECT_FALSE( all_lt( S, 2 ) );
	EXPECT_FALSE( all_ge( S, 9 ) );
	EXPECT_TRUE( all_lt( S, 11 ) );
	EXPECT_TRUE( all_gt( S, 0 ) );
}

TEST( ArraySTest, CountOp2D )
{
	Array2D_int const A( 3, 3, { 1, 2, 2, 3, 3, 3, 7, 8, 9 } );
	Array2S_int S( A );
	EXPECT_EQ( 0u, count_eq( S, 0 ) );
	EXPECT_EQ( 1u, count_eq( S, 1 ) );
	EXPECT_EQ( 2u, count_eq( S, 2 ) );
	EXPECT_EQ( 3u, count_eq( S, 3 ) );
	EXPECT_EQ( 6u, count_lt( S, 7 ) );
	EXPECT_EQ( 1u, count_ge( S, 9 ) );
	EXPECT_EQ( 9u, count_lt( S, 11 ) );
	EXPECT_EQ( 3u, count_gt( S, 3 ) );
}

TEST( ArraySTest, Functions1D )
{
	Array1D_int U{ 1, 2, 3 };
	Array1D_int V{ 2, 3, 4 };
	Array1S_int u( U );
	Array1S_int v( V );
	EXPECT_EQ( 14, magnitude_squared( u ) );
	EXPECT_EQ( 3, distance_squared( u, v ) );
	EXPECT_EQ( 20, dot( u, v ) );
}

TEST( ArraySTest, StreamOut )
{
	Array1D_int const A( 3, { 1, 2, 3 } );
	Array1S_int S( A );
	std::ostringstream stream;
	stream << S;
	EXPECT_EQ( "           1            2            3 ", stream.str() );
}

TEST( ArraySTest, StreamIn )
{
	Array1D_int const A( 3, { 1, 2, 3 } );
	Array1S_int S( A );
	std::string const text( "1  2  3" );
	std::istringstream stream( text );
	stream >> S;
	EXPECT_TRUE( eq( A, S ) );
}
