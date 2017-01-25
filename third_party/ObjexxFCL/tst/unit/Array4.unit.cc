// ObjexxFCL::Array4 Unit Tests
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
#include <ObjexxFCL/Array4.all.hh>
#include <ObjexxFCL/Array.functions.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( Array4Test, ConstructDefault )
{
	Array4D_int A;
	EXPECT_EQ( 0u, A.size() );
	EXPECT_EQ( 0u, A.size1() );
	EXPECT_EQ( 0u, A.size2() );
	EXPECT_EQ( 0u, A.size3() );
	EXPECT_EQ( 0u, A.size4() );
	EXPECT_EQ( 1, A.l1() );
	EXPECT_EQ( 1, A.l2() );
	EXPECT_EQ( 1, A.l3() );
	EXPECT_EQ( 1, A.l4() );
	EXPECT_EQ( 0, A.u1() );
	EXPECT_EQ( 0, A.u2() );
	EXPECT_EQ( 0, A.u3() );
	EXPECT_EQ( 0, A.u4() );
	EXPECT_EQ( Array4D_int::IR(), A.I1() );
	EXPECT_EQ( Array4D_int::IR(), A.I2() );
	EXPECT_EQ( Array4D_int::IR(), A.I3() );
	EXPECT_EQ( Array4D_int::IR(), A.I4() );
	EXPECT_FALSE( A.initializer_active() );
}

TEST( Array4Test, ConstructCopy )
{
	Array4D_int A;
	Array4D_int B( A );
	EXPECT_EQ( A.size(), B.size() );
	EXPECT_EQ( A.size1(), B.size1() );
	EXPECT_EQ( A.size2(), B.size2() );
	EXPECT_EQ( A.size3(), B.size3() );
	EXPECT_EQ( A.size4(), B.size4() );
	EXPECT_EQ( A.l1(), B.l1() );
	EXPECT_EQ( A.l2(), B.l2() );
	EXPECT_EQ( A.l3(), B.l3() );
	EXPECT_EQ( A.l4(), B.l4() );
	EXPECT_EQ( A.u1(), B.u1() );
	EXPECT_EQ( A.u2(), B.u2() );
	EXPECT_EQ( A.u3(), B.u3() );
	EXPECT_EQ( A.u4(), B.u4() );
	EXPECT_EQ( A.I1(), B.I1() );
	EXPECT_EQ( A.I2(), B.I2() );
	EXPECT_EQ( A.I3(), B.I3() );
	EXPECT_EQ( A.I4(), B.I4() );
	EXPECT_EQ( A.initializer_active(), B.initializer_active() );
	EXPECT_TRUE( conformable( A, B ) );
	EXPECT_TRUE( equal_dimensions( A, B ) );
	EXPECT_TRUE( eq( A, B ) );
}

TEST( Array4Test, ConstructOtherData )
{
	Array4D_double A( 2, 2, 2, 2 );
	for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
		for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
			for ( int i3 = A.l3(); i3 <= A.u3(); ++i3 ) {
				for ( int i4 = A.l4(); i4 <= A.u4(); ++i4 ) {
					A( i1, i2, i3, i4 ) = i1 + i2 +i3 + i4;
				}
			}
		}
	}
	Array4D_int B( A );
	EXPECT_EQ( A.I1(), B.I1() );
	EXPECT_EQ( A.I2(), B.I2() );
	EXPECT_EQ( A.I3(), B.I3() );
	EXPECT_EQ( A.I4(), B.I4() );
	EXPECT_EQ( A.initializer_active(), B.initializer_active() );
	EXPECT_TRUE( conformable( A, B ) );
	EXPECT_TRUE( equal_dimensions( A, B ) );
	for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
		for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
			for ( int i3 = A.l3(); i3 <= A.u3(); ++i3 ) {
				for ( int i4 = A.l4(); i4 <= A.u4(); ++i4 ) {
					EXPECT_EQ( int( A( i1, i2, i3, i4 ) ), B( i1, i2, i3, i4 ) );
					EXPECT_DOUBLE_EQ( A( i1, i2, i3, i4 ), double( B( i1, i2, i3, i4 ) ) ); // Works because they are all integer values
				}
			}
		}
	}
}

TEST( Array4Test, ConstructIndexes )
{
	Array4D_int A( 3, 3, 3, 3 );
	EXPECT_EQ( 81u, A.size() );
	EXPECT_EQ( 3u, A.size1() );
	EXPECT_EQ( 3u, A.size2() );
	EXPECT_EQ( 3u, A.size3() );
	EXPECT_EQ( 3u, A.size4() );
	EXPECT_EQ( 1, A.l1() );
	EXPECT_EQ( 1, A.l2() );
	EXPECT_EQ( 1, A.l3() );
	EXPECT_EQ( 1, A.l4() );
	EXPECT_EQ( 3, A.u1() );
	EXPECT_EQ( 3, A.u2() );
	EXPECT_EQ( 3, A.u3() );
	EXPECT_EQ( 3, A.u4() );
	EXPECT_EQ( Array4D_int::IR( 1, 3 ), A.I1() );
	EXPECT_EQ( Array4D_int::IR( 1, 3 ), A.I2() );
	EXPECT_EQ( Array4D_int::IR( 1, 3 ), A.I3() );
	EXPECT_EQ( Array4D_int::IR( 1, 3 ), A.I4() );
	EXPECT_FALSE( A.initializer_active() );
}

TEST( Array4Test, RangeBasedFor )
{
	Array4D_int A( 2, 2, 2, 2, { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 } );
	int v( 0 );
	for ( auto const e : A ) {
		EXPECT_EQ( ++v, e );
	}
}

TEST( Array4Test, Subscript )
{
	Array4D_int A( 2, 2, 2, 2, {
	 1111,
	 1112,
	 1121,
	 1122,
	 1211,
	 1212,
	 1221,
	 1222,
	 2111,
	 2112,
	 2121,
	 2122,
	 2211,
	 2212,
	 2221,
	 2222
	} );
	EXPECT_EQ( 16u, A.size() );

	// Linear indexing
	EXPECT_EQ( 1111, A[ 0 ] );
	EXPECT_EQ( 1112, A[ 1 ] );
	EXPECT_EQ( 1121, A[ 2 ] );
	EXPECT_EQ( 1122, A[ 3 ] );
	EXPECT_EQ( 1211, A[ 4 ] );
	EXPECT_EQ( 1212, A[ 5 ] );
	EXPECT_EQ( 1221, A[ 6 ] );
	EXPECT_EQ( 1222, A[ 7 ] );
	EXPECT_EQ( 2111, A[ 8 ] );
	EXPECT_EQ( 2112, A[ 9 ] );
	EXPECT_EQ( 2121, A[ 10 ] );
	EXPECT_EQ( 2122, A[ 11 ] );
	EXPECT_EQ( 2211, A[ 12 ] );
	EXPECT_EQ( 2212, A[ 13 ] );
	EXPECT_EQ( 2221, A[ 14 ] );
	EXPECT_EQ( 2222, A[ 15 ] );

	// Array indexing
	EXPECT_EQ( 1111, A( 1, 1, 1, 1 ) );
	EXPECT_EQ( 1112, A( 1, 1, 1, 2 ) );
	EXPECT_EQ( 1121, A( 1, 1, 2, 1 ) );
	EXPECT_EQ( 1122, A( 1, 1, 2, 2 ) );
	EXPECT_EQ( 1211, A( 1, 2, 1, 1 ) );
	EXPECT_EQ( 1212, A( 1, 2, 1, 2 ) );
	EXPECT_EQ( 1221, A( 1, 2, 2, 1 ) );
	EXPECT_EQ( 1222, A( 1, 2, 2, 2 ) );
	EXPECT_EQ( 2111, A( 2, 1, 1, 1 ) );
	EXPECT_EQ( 2112, A( 2, 1, 1, 2 ) );
	EXPECT_EQ( 2121, A( 2, 1, 2, 1 ) );
	EXPECT_EQ( 2122, A( 2, 1, 2, 2 ) );
	EXPECT_EQ( 2211, A( 2, 2, 1, 1 ) );
	EXPECT_EQ( 2212, A( 2, 2, 1, 2 ) );
	EXPECT_EQ( 2221, A( 2, 2, 2, 1 ) );
	EXPECT_EQ( 2222, A( 2, 2, 2, 2 ) );
}

TEST( Array4Test, Predicates )
{
	Array4D_int A1;
	EXPECT_FALSE( A1.active() );
	EXPECT_FALSE( A1.allocated() );
	EXPECT_TRUE( A1.contiguous() );
	EXPECT_TRUE( A1.capacity_bounded() );
	EXPECT_FALSE( A1.capacity_unbounded() );
	EXPECT_TRUE( A1.empty() );
	EXPECT_TRUE( A1.size_bounded() );
	EXPECT_FALSE( A1.size_unbounded() );
	EXPECT_TRUE( A1.owner() );
	EXPECT_FALSE( A1.proxy() );
	EXPECT_TRUE( A1.is_default() );
	EXPECT_TRUE( A1.is_zero() );
	EXPECT_TRUE( A1.is_uniform() );
	EXPECT_TRUE( A1.is_uniform( 0 ) );

	Array4D_int A2( 2, 3, 2, 2 ); // Uninitialized
	EXPECT_TRUE( A2.active() );
	EXPECT_TRUE( A2.allocated() );
	EXPECT_TRUE( A2.contiguous() );
	EXPECT_TRUE( A2.capacity_bounded() );
	EXPECT_FALSE( A2.capacity_unbounded() );
	EXPECT_FALSE( A2.empty() );
	EXPECT_TRUE( A2.capacity_bounded() );
	EXPECT_FALSE( A2.capacity_unbounded() );
	EXPECT_TRUE( A2.owner() );
	EXPECT_FALSE( A2.proxy() );

	Array4D_int A3( 2, 3, 2, 2, 31459 );
	EXPECT_TRUE( A3.active() );
	EXPECT_TRUE( A3.allocated() );
	EXPECT_TRUE( A3.contiguous() );
	EXPECT_TRUE( A3.capacity_bounded() );
	EXPECT_FALSE( A3.capacity_unbounded() );
	EXPECT_FALSE( A3.empty() );
	EXPECT_TRUE( A3.capacity_bounded() );
	EXPECT_FALSE( A3.capacity_unbounded() );
	EXPECT_TRUE( A3.owner() );
	EXPECT_FALSE( A3.proxy() );
	EXPECT_FALSE( A3.is_default() );
	EXPECT_FALSE( A3.is_zero() );
	EXPECT_TRUE( A3.is_uniform() );
	EXPECT_TRUE( A3.is_uniform( 31459 ) );

	Array4D_int A4( 2, 2, 2, 2, {
	 1111,
	 1112,
	 1121,
	 1122,
	 1211,
	 1212,
	 1221,
	 1222,
	 2111,
	 2112,
	 2121,
	 2122,
	 2211,
	 2212,
	 2221,
	 2222
	} );
	EXPECT_TRUE( A4.active() );
	EXPECT_TRUE( A4.allocated() );
	EXPECT_TRUE( A4.contiguous() );
	EXPECT_TRUE( A4.capacity_bounded() );
	EXPECT_FALSE( A4.capacity_unbounded() );
	EXPECT_FALSE( A4.empty() );
	EXPECT_TRUE( A4.capacity_bounded() );
	EXPECT_FALSE( A4.capacity_unbounded() );
	EXPECT_TRUE( A4.owner() );
	EXPECT_FALSE( A4.proxy() );
	EXPECT_FALSE( A4.is_default() );
	EXPECT_FALSE( A4.is_zero() );
	EXPECT_FALSE( A4.is_uniform() );
	EXPECT_FALSE( A4.is_uniform( 0 ) );
	EXPECT_FALSE( A4.is_uniform( 1111 ) );
}

TEST( Array4Test, PredicateComparisonsValues )
{
	Array4D_int A1;
	EXPECT_TRUE( eq( A1, 0 ) && eq( 0, A1 ) ); // Empty array is considered to equal any scalar (no values don't equal the scalar)
	EXPECT_FALSE( ne( A1, 0 ) || ne( 0, A1 ) );
	EXPECT_FALSE( lt( A1, 0 ) || lt( 0, A1 ) );
	EXPECT_TRUE( le( A1, 0 ) && le( 0, A1 ) );
	EXPECT_FALSE( gt( A1, 0 ) || gt( 0, A1 ) );
	EXPECT_TRUE( ge( A1, 0 ) && ge( 0, A1 ) );

	Array4D_int A2( 2, 3, 2, 2, 31459 );
	EXPECT_TRUE( eq( A2, 31459 ) && eq( 31459, A1 ) );
	EXPECT_FALSE( ne( A2, 31459 ) || ne( 31459, A2 ) );
	EXPECT_TRUE( lt( A2, 31460 ) && lt( 31458, A2 ) );
	EXPECT_TRUE( le( A2, 31459 ) && le( 31459, A2 ) );
	EXPECT_TRUE( le( A2, 31460 ) && le( 31458, A2 ) );
	EXPECT_TRUE( gt( A2, 31458 ) && gt( 31460, A2 ) );
	EXPECT_TRUE( ge( A2, 31459 ) && ge( 31459, A2 ) );
	EXPECT_TRUE( ge( A2, 31458 ) && ge( 31460, A2 ) );

	Array4D_int A3( 2, 2, 2, 2, {
	 1111,
	 1112,
	 1121,
	 1122,
	 1211,
	 1212,
	 1221,
	 1222,
	 2111,
	 2112,
	 2121,
	 2122,
	 2211,
	 2212,
	 2221,
	 2222
	} );
	EXPECT_FALSE( eq( A3, 11 ) || eq( 23, A3 ) );
	EXPECT_TRUE( ne( A3, 11 ) && ne( 23, A3 ) );
	EXPECT_TRUE( lt( A3, 2444 ) && lt( 10, A3 ) );
	EXPECT_FALSE( lt( A3, 211 ) || lt( 1111, A3 ) );
	EXPECT_TRUE( le( A3, 2333 ) && le( 1111, A3 ) );
	EXPECT_TRUE( gt( A3, 10 ) && gt( 2444, A3 ) );
	EXPECT_FALSE( gt( A3, 1111 ) || gt( 2222, A3 ) );
	EXPECT_TRUE( ge( A3, 1111 ) && ge( 2333, A3 ) );
}
