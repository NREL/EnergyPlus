// ObjexxFCL::Array5 Unit Tests
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
#include <ObjexxFCL/Array5.all.hh>
#include <ObjexxFCL/Array.functions.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( Array5Test, ConstructDefault )
{
	Array5D_int A;
	EXPECT_EQ( 0u, A.size() );
	EXPECT_EQ( 0u, A.size1() );
	EXPECT_EQ( 0u, A.size2() );
	EXPECT_EQ( 0u, A.size3() );
	EXPECT_EQ( 0u, A.size4() );
	EXPECT_EQ( 0u, A.size5() );
	EXPECT_EQ( 1, A.l1() );
	EXPECT_EQ( 1, A.l2() );
	EXPECT_EQ( 1, A.l3() );
	EXPECT_EQ( 1, A.l4() );
	EXPECT_EQ( 1, A.l5() );
	EXPECT_EQ( 0, A.u1() );
	EXPECT_EQ( 0, A.u2() );
	EXPECT_EQ( 0, A.u3() );
	EXPECT_EQ( 0, A.u4() );
	EXPECT_EQ( 0, A.u5() );
	EXPECT_EQ( Array5D_int::IR(), A.I1() );
	EXPECT_EQ( Array5D_int::IR(), A.I2() );
	EXPECT_EQ( Array5D_int::IR(), A.I3() );
	EXPECT_EQ( Array5D_int::IR(), A.I4() );
	EXPECT_EQ( Array5D_int::IR(), A.I5() );
	EXPECT_FALSE( A.initializer_active() );
}

TEST( Array5Test, ConstructCopy )
{
	Array5D_int A;
	Array5D_int B( A );
	EXPECT_EQ( A.size(), B.size() );
	EXPECT_EQ( A.size1(), B.size1() );
	EXPECT_EQ( A.size2(), B.size2() );
	EXPECT_EQ( A.size3(), B.size3() );
	EXPECT_EQ( A.size4(), B.size4() );
	EXPECT_EQ( A.size5(), B.size5() );
	EXPECT_EQ( A.l1(), B.l1() );
	EXPECT_EQ( A.l2(), B.l2() );
	EXPECT_EQ( A.l3(), B.l3() );
	EXPECT_EQ( A.l4(), B.l4() );
	EXPECT_EQ( A.l5(), B.l5() );
	EXPECT_EQ( A.u1(), B.u1() );
	EXPECT_EQ( A.u2(), B.u2() );
	EXPECT_EQ( A.u3(), B.u3() );
	EXPECT_EQ( A.u4(), B.u4() );
	EXPECT_EQ( A.u5(), B.u5() );
	EXPECT_EQ( A.I1(), B.I1() );
	EXPECT_EQ( A.I2(), B.I2() );
	EXPECT_EQ( A.I3(), B.I3() );
	EXPECT_EQ( A.I4(), B.I4() );
	EXPECT_EQ( A.I5(), B.I5() );
	EXPECT_EQ( A.initializer_active(), B.initializer_active() );
	EXPECT_TRUE( conformable( A, B ) );
	EXPECT_TRUE( equal_dimensions( A, B ) );
	EXPECT_TRUE( eq( A, B ) );
}

TEST( Array5Test, ConstructOtherData )
{
	Array5D_double A( 2, 2, 2, 2, 2 );
	for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
		for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
			for ( int i3 = A.l3(); i3 <= A.u3(); ++i3 ) {
				for ( int i4 = A.l4(); i4 <= A.u4(); ++i4 ) {
					for ( int i5 = A.l5(); i5 <= A.u5(); ++i5 ) {
						A( i1, i2, i3, i4, i5 ) = i1 + i2 +i3 + i4 + i5;
					}
				}
			}
		}
	}
	Array5D_int B( A );
	EXPECT_EQ( A.I1(), B.I1() );
	EXPECT_EQ( A.I2(), B.I2() );
	EXPECT_EQ( A.I3(), B.I3() );
	EXPECT_EQ( A.I4(), B.I4() );
	EXPECT_EQ( A.I5(), B.I5() );
	EXPECT_EQ( A.initializer_active(), B.initializer_active() );
	EXPECT_TRUE( conformable( A, B ) );
	EXPECT_TRUE( equal_dimensions( A, B ) );
	for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
		for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
			for ( int i3 = A.l3(); i3 <= A.u3(); ++i3 ) {
				for ( int i4 = A.l4(); i4 <= A.u4(); ++i4 ) {
					for ( int i5 = A.l5(); i5 <= A.u5(); ++i5 ) {
						EXPECT_EQ( int( A( i1, i2, i3, i4, i5 ) ), B( i1, i2, i3, i4, i5 ) );
						EXPECT_DOUBLE_EQ( A( i1, i2, i3, i4, i5 ), double( B( i1, i2, i3, i4, i5 ) ) ); // Works because they are all integer values
					}
				}
			}
		}
	}
}

TEST( Array5Test, ConstructIndexes )
{
	Array5D_int A( 3, 3, 3, 3, 3 );
	EXPECT_EQ( 243u, A.size() );
	EXPECT_EQ( 3u, A.size1() );
	EXPECT_EQ( 3u, A.size2() );
	EXPECT_EQ( 3u, A.size3() );
	EXPECT_EQ( 3u, A.size4() );
	EXPECT_EQ( 3u, A.size5() );
	EXPECT_EQ( 1, A.l1() );
	EXPECT_EQ( 1, A.l2() );
	EXPECT_EQ( 1, A.l3() );
	EXPECT_EQ( 1, A.l4() );
	EXPECT_EQ( 1, A.l5() );
	EXPECT_EQ( 3, A.u1() );
	EXPECT_EQ( 3, A.u2() );
	EXPECT_EQ( 3, A.u3() );
	EXPECT_EQ( 3, A.u4() );
	EXPECT_EQ( 3, A.u5() );
	EXPECT_EQ( Array5D_int::IR( 1, 3 ), A.I1() );
	EXPECT_EQ( Array5D_int::IR( 1, 3 ), A.I2() );
	EXPECT_EQ( Array5D_int::IR( 1, 3 ), A.I3() );
	EXPECT_EQ( Array5D_int::IR( 1, 3 ), A.I4() );
	EXPECT_EQ( Array5D_int::IR( 1, 3 ), A.I5() );
	EXPECT_FALSE( A.initializer_active() );
}

TEST( Array5Test, RangeBasedFor )
{
	Array5D_int A( 2, 2, 2, 2, 2 );
	int v( 0 );
	for ( auto & e : A ) {
		e = ++v;
		EXPECT_EQ( v, e );
	}
}

TEST( Array5Test, Subscript )
{
	Array5D_int A( 2, 2, 2, 2, 2, {
	 11111,
	 11112,
	 11121,
	 11122,
	 11211,
	 11212,
	 11221,
	 11222,
	 12111,
	 12112,
	 12121,
	 12122,
	 12211,
	 12212,
	 12221,
	 12222,
	 21111,
	 21112,
	 21121,
	 21122,
	 21211,
	 21212,
	 21221,
	 21222,
	 22111,
	 22112,
	 22121,
	 22122,
	 22211,
	 22212,
	 22221,
	 22222
	} );
	EXPECT_EQ( 32u, A.size() );

	// Linear indexing
	EXPECT_EQ( 11111, A[ 0 ] );
	EXPECT_EQ( 11112, A[ 1 ] );
	EXPECT_EQ( 11121, A[ 2 ] );
	EXPECT_EQ( 11122, A[ 3 ] );
	EXPECT_EQ( 11211, A[ 4 ] );
	EXPECT_EQ( 11212, A[ 5 ] );
	EXPECT_EQ( 11221, A[ 6 ] );
	EXPECT_EQ( 11222, A[ 7 ] );
	EXPECT_EQ( 12111, A[ 8 ] );
	EXPECT_EQ( 12112, A[ 9 ] );
	EXPECT_EQ( 12121, A[ 10 ] );
	EXPECT_EQ( 12122, A[ 11 ] );
	EXPECT_EQ( 12211, A[ 12 ] );
	EXPECT_EQ( 12212, A[ 13 ] );
	EXPECT_EQ( 12221, A[ 14 ] );
	EXPECT_EQ( 12222, A[ 15 ] );
	EXPECT_EQ( 21111, A[ 16 ] );
	EXPECT_EQ( 21112, A[ 17 ] );
	EXPECT_EQ( 21121, A[ 18 ] );
	EXPECT_EQ( 21122, A[ 19 ] );
	EXPECT_EQ( 21211, A[ 20 ] );
	EXPECT_EQ( 21212, A[ 21 ] );
	EXPECT_EQ( 21221, A[ 22 ] );
	EXPECT_EQ( 21222, A[ 23 ] );
	EXPECT_EQ( 22111, A[ 24 ] );
	EXPECT_EQ( 22112, A[ 25 ] );
	EXPECT_EQ( 22121, A[ 26 ] );
	EXPECT_EQ( 22122, A[ 27 ] );
	EXPECT_EQ( 22211, A[ 28 ] );
	EXPECT_EQ( 22212, A[ 29 ] );
	EXPECT_EQ( 22221, A[ 30 ] );
	EXPECT_EQ( 22222, A[ 31 ] );

	// Array indexing
	EXPECT_EQ( 11111, A( 1, 1, 1, 1, 1 ) );
	EXPECT_EQ( 11112, A( 1, 1, 1, 1, 2 ) );
	EXPECT_EQ( 11121, A( 1, 1, 1, 2, 1 ) );
	EXPECT_EQ( 11122, A( 1, 1, 1, 2, 2 ) );
	EXPECT_EQ( 11211, A( 1, 1, 2, 1, 1 ) );
	EXPECT_EQ( 11212, A( 1, 1, 2, 1, 2 ) );
	EXPECT_EQ( 11221, A( 1, 1, 2, 2, 1 ) );
	EXPECT_EQ( 11222, A( 1, 1, 2, 2, 2 ) );
	EXPECT_EQ( 12111, A( 1, 2, 1, 1, 1 ) );
	EXPECT_EQ( 12112, A( 1, 2, 1, 1, 2 ) );
	EXPECT_EQ( 12121, A( 1, 2, 1, 2, 1 ) );
	EXPECT_EQ( 12122, A( 1, 2, 1, 2, 2 ) );
	EXPECT_EQ( 12211, A( 1, 2, 2, 1, 1 ) );
	EXPECT_EQ( 12212, A( 1, 2, 2, 1, 2 ) );
	EXPECT_EQ( 12221, A( 1, 2, 2, 2, 1 ) );
	EXPECT_EQ( 12222, A( 1, 2, 2, 2, 2 ) );
	EXPECT_EQ( 21111, A( 2, 1, 1, 1, 1 ) );
	EXPECT_EQ( 21112, A( 2, 1, 1, 1, 2 ) );
	EXPECT_EQ( 21121, A( 2, 1, 1, 2, 1 ) );
	EXPECT_EQ( 21122, A( 2, 1, 1, 2, 2 ) );
	EXPECT_EQ( 21211, A( 2, 1, 2, 1, 1 ) );
	EXPECT_EQ( 21212, A( 2, 1, 2, 1, 2 ) );
	EXPECT_EQ( 21221, A( 2, 1, 2, 2, 1 ) );
	EXPECT_EQ( 21222, A( 2, 1, 2, 2, 2 ) );
	EXPECT_EQ( 22111, A( 2, 2, 1, 1, 1 ) );
	EXPECT_EQ( 22112, A( 2, 2, 1, 1, 2 ) );
	EXPECT_EQ( 22121, A( 2, 2, 1, 2, 1 ) );
	EXPECT_EQ( 22122, A( 2, 2, 1, 2, 2 ) );
	EXPECT_EQ( 22211, A( 2, 2, 2, 1, 1 ) );
	EXPECT_EQ( 22212, A( 2, 2, 2, 1, 2 ) );
	EXPECT_EQ( 22221, A( 2, 2, 2, 2, 1 ) );
	EXPECT_EQ( 22222, A( 2, 2, 2, 2, 2 ) );
}

TEST( Array5Test, Predicates )
{
	Array5D_int A1;
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

	Array5D_int A2( 2, 3, 2, 2, 3 ); // Uninitialized
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

	Array5D_int A3( 2, 3, 2, 2, 3, 31459 );
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

	Array5D_int A4( 2, 2, 2, 2, 2, {
	 11111,
	 11112,
	 11121,
	 11122,
	 11211,
	 11212,
	 11221,
	 11222,
	 12111,
	 12112,
	 12121,
	 12122,
	 12211,
	 12212,
	 12221,
	 12222,
	 21111,
	 21112,
	 21121,
	 21122,
	 21211,
	 21212,
	 21221,
	 21222,
	 22111,
	 22112,
	 22121,
	 22122,
	 22211,
	 22212,
	 22221,
	 22222
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
	EXPECT_FALSE( A4.is_uniform( 11111 ) );
}

TEST( Array5Test, PredicateComparisonsValues )
{
	Array5D_int A1;
	EXPECT_TRUE( eq( A1, 0 ) && eq( 0, A1 ) ); // Empty array is considered to equal any scalar (no values don't equal the scalar)
	EXPECT_FALSE( ne( A1, 0 ) || ne( 0, A1 ) );
	EXPECT_FALSE( lt( A1, 0 ) || lt( 0, A1 ) );
	EXPECT_TRUE( le( A1, 0 ) && le( 0, A1 ) );
	EXPECT_FALSE( gt( A1, 0 ) || gt( 0, A1 ) );
	EXPECT_TRUE( ge( A1, 0 ) && ge( 0, A1 ) );

	Array5D_int A2( 2, 3, 2, 2, 3, 31459 );
	EXPECT_TRUE( eq( A2, 31459 ) && eq( 31459, A1 ) );
	EXPECT_FALSE( ne( A2, 31459 ) || ne( 31459, A2 ) );
	EXPECT_TRUE( lt( A2, 31460 ) && lt( 31458, A2 ) );
	EXPECT_TRUE( le( A2, 31459 ) && le( 31459, A2 ) );
	EXPECT_TRUE( le( A2, 31460 ) && le( 31458, A2 ) );
	EXPECT_TRUE( gt( A2, 31458 ) && gt( 31460, A2 ) );
	EXPECT_TRUE( ge( A2, 31459 ) && ge( 31459, A2 ) );
	EXPECT_TRUE( ge( A2, 31458 ) && ge( 31460, A2 ) );

	Array5D_int A3( 2, 2, 2, 2, 2, {
	 11111,
	 11112,
	 11121,
	 11122,
	 11211,
	 11212,
	 11221,
	 11222,
	 12111,
	 12112,
	 12121,
	 12122,
	 12211,
	 12212,
	 12221,
	 12222,
	 21111,
	 21112,
	 21121,
	 21122,
	 21211,
	 21212,
	 21221,
	 21222,
	 22111,
	 22112,
	 22121,
	 22122,
	 22211,
	 22212,
	 22221,
	 22222
	} );
	EXPECT_FALSE( eq( A3, 11111 ) || eq( 23333, A3 ) );
	EXPECT_TRUE( ne( A3, 11111 ) && ne( 23333, A3 ) );
	EXPECT_TRUE( lt( A3, 24444 ) && lt( 11110, A3 ) );
	EXPECT_FALSE( lt( A3, 211 ) || lt( 11111, A3 ) );
	EXPECT_TRUE( le( A3, 23333 ) && le( 11111, A3 ) );
	EXPECT_TRUE( gt( A3, 10 ) && gt( 24444, A3 ) );
	EXPECT_FALSE( gt( A3, 11111 ) || gt( 22222, A3 ) );
	EXPECT_TRUE( ge( A3, 11111 ) && ge( 23333, A3 ) );
}
