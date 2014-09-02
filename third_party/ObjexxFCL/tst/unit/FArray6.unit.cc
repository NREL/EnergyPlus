// ObjexxFCL::FArray6 Unit Tests
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
#include <ObjexxFCL/FArray6.all.hh>
#include <ObjexxFCL/FArray6.io.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/DimensionExpressions.hh>
#include <ObjexxFCL/FArray.functions.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <array>
#include <vector>

using namespace ObjexxFCL;

TEST( FArray6Test, ConstructDefault )
{
	FArray6D_int A;
	EXPECT_EQ( 0u, A.size() );
	EXPECT_EQ( 0u, A.size1() );
	EXPECT_EQ( 0u, A.size2() );
	EXPECT_EQ( 0u, A.size3() );
	EXPECT_EQ( 0u, A.size4() );
	EXPECT_EQ( 0u, A.size5() );
	EXPECT_EQ( 0u, A.size6() );
	EXPECT_EQ( 1, A.l1() );
	EXPECT_EQ( 1, A.l2() );
	EXPECT_EQ( 1, A.l3() );
	EXPECT_EQ( 1, A.l4() );
	EXPECT_EQ( 1, A.l5() );
	EXPECT_EQ( 1, A.l6() );
	EXPECT_EQ( 0, A.u1() );
	EXPECT_EQ( 0, A.u2() );
	EXPECT_EQ( 0, A.u3() );
	EXPECT_EQ( 0, A.u4() );
	EXPECT_EQ( 0, A.u5() );
	EXPECT_EQ( 0, A.u6() );
	EXPECT_EQ( FArray6D_int::IR(), A.I1() );
	EXPECT_EQ( FArray6D_int::IR(), A.I2() );
	EXPECT_EQ( FArray6D_int::IR(), A.I3() );
	EXPECT_EQ( FArray6D_int::IR(), A.I4() );
	EXPECT_EQ( FArray6D_int::IR(), A.I5() );
	EXPECT_EQ( FArray6D_int::IR(), A.I6() );
	EXPECT_TRUE( A.dimensions_initialized() );
	EXPECT_FALSE( A.initializer_active() );
}

TEST( FArray6Test, ConstructCopy )
{
	FArray6D_int A;
	FArray6D_int B( A );
	EXPECT_EQ( A.size(), B.size() );
	EXPECT_EQ( A.size1(), B.size1() );
	EXPECT_EQ( A.size2(), B.size2() );
	EXPECT_EQ( A.size3(), B.size3() );
	EXPECT_EQ( A.size4(), B.size4() );
	EXPECT_EQ( A.size5(), B.size5() );
	EXPECT_EQ( A.size6(), B.size6() );
	EXPECT_EQ( A.l1(), B.l1() );
	EXPECT_EQ( A.l2(), B.l2() );
	EXPECT_EQ( A.l3(), B.l3() );
	EXPECT_EQ( A.l4(), B.l4() );
	EXPECT_EQ( A.l5(), B.l5() );
	EXPECT_EQ( A.l6(), B.l6() );
	EXPECT_EQ( A.u1(), B.u1() );
	EXPECT_EQ( A.u2(), B.u2() );
	EXPECT_EQ( A.u3(), B.u3() );
	EXPECT_EQ( A.u4(), B.u4() );
	EXPECT_EQ( A.u5(), B.u5() );
	EXPECT_EQ( A.u6(), B.u6() );
	EXPECT_EQ( A.I1(), B.I1() );
	EXPECT_EQ( A.I2(), B.I2() );
	EXPECT_EQ( A.I3(), B.I3() );
	EXPECT_EQ( A.I4(), B.I4() );
	EXPECT_EQ( A.I5(), B.I5() );
	EXPECT_EQ( A.I6(), B.I6() );
	EXPECT_EQ( A.dimensions_initialized(), B.dimensions_initialized() );
	EXPECT_EQ( A.initializer_active(), B.initializer_active() );
	EXPECT_TRUE( conformable( A, B ) );
	EXPECT_TRUE( equal_dimensions( A, B ) );
	EXPECT_TRUE( eq( A, B ) );
}

TEST( FArray6Test, ConstructOtherData )
{
	FArray6D_double A( 2, 2, 2, 2, 2, 2 );
	for ( int i6 = A.l6(); i6 <= A.u6(); ++i6 )
		for ( int i5 = A.l5(); i5 <= A.u5(); ++i5 )
			for ( int i4 = A.l4(); i4 <= A.u4(); ++i4 )
				for ( int i3 = A.l3(); i3 <= A.u3(); ++i3 )
					for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 )
						for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 )
							A( i1, i2, i3, i4, i5, i6 ) = i1 + i2 +i3 + i4 + i5 + i6;
	FArray6D_int B( A );
	EXPECT_EQ( A.I1(), B.I1() );
	EXPECT_EQ( A.I2(), B.I2() );
	EXPECT_EQ( A.I3(), B.I3() );
	EXPECT_EQ( A.I4(), B.I4() );
	EXPECT_EQ( A.I5(), B.I5() );
	EXPECT_EQ( A.I6(), B.I6() );
	EXPECT_EQ( A.dimensions_initialized(), B.dimensions_initialized() );
	EXPECT_EQ( A.initializer_active(), B.initializer_active() );
	EXPECT_TRUE( conformable( A, B ) );
	EXPECT_TRUE( equal_dimensions( A, B ) );
	for ( int i6 = A.l6(); i6 <= A.u6(); ++i6 ) {
		for ( int i5 = A.l5(); i5 <= A.u5(); ++i5 ) {
			for ( int i4 = A.l4(); i4 <= A.u4(); ++i4 ) {
				for ( int i3 = A.l3(); i3 <= A.u3(); ++i3 ) {
					for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
						for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
							EXPECT_EQ( int( A( i1, i2, i3, i4, i5, i6 ) ), B( i1, i2, i3, i4, i5, i6 ) );
							EXPECT_DOUBLE_EQ( A( i1, i2, i3, i4, i5, i6 ), double( B( i1, i2, i3, i4, i5, i6 ) ) ); // Works because they are all integer values
						}
					}
				}
			}
		}
	}
}

TEST( FArray6Test, ConstructIndexes )
{
	FArray6D_int A( 3, 3, 3, 3, 3, 3 );
	EXPECT_EQ( 729u, A.size() );
	EXPECT_EQ( 3u, A.size1() );
	EXPECT_EQ( 3u, A.size2() );
	EXPECT_EQ( 3u, A.size3() );
	EXPECT_EQ( 3u, A.size4() );
	EXPECT_EQ( 3u, A.size5() );
	EXPECT_EQ( 3u, A.size6() );
	EXPECT_EQ( 1, A.l1() );
	EXPECT_EQ( 1, A.l2() );
	EXPECT_EQ( 1, A.l3() );
	EXPECT_EQ( 1, A.l4() );
	EXPECT_EQ( 1, A.l5() );
	EXPECT_EQ( 1, A.l6() );
	EXPECT_EQ( 3, A.u1() );
	EXPECT_EQ( 3, A.u2() );
	EXPECT_EQ( 3, A.u3() );
	EXPECT_EQ( 3, A.u4() );
	EXPECT_EQ( 3, A.u5() );
	EXPECT_EQ( 3, A.u6() );
	EXPECT_EQ( FArray6D_int::IR( 1, 3 ), A.I1() );
	EXPECT_EQ( FArray6D_int::IR( 1, 3 ), A.I2() );
	EXPECT_EQ( FArray6D_int::IR( 1, 3 ), A.I3() );
	EXPECT_EQ( FArray6D_int::IR( 1, 3 ), A.I4() );
	EXPECT_EQ( FArray6D_int::IR( 1, 3 ), A.I5() );
	EXPECT_EQ( FArray6D_int::IR( 1, 3 ), A.I6() );
	EXPECT_TRUE( A.dimensions_initialized() );
	EXPECT_FALSE( A.initializer_active() );
}

TEST( FArray6Test, Subscript )
{
	FArray6D_int A( 2, 2, 2, 2, 2, 2, { 111111, 211111, 121111, 221111, 112111, 212111, 122111, 222111, 111211, 211211, 121211, 221211, 112211, 212211, 122211, 222211, 111121, 211121, 121121, 221121, 112121, 212121, 122121, 222121, 111221, 211221, 121221, 221221, 112221, 212221, 122221, 222221,
	                                    111112, 211112, 121112, 221112, 112112, 212112, 122112, 222112, 111212, 211212, 121212, 221212, 112212, 212212, 122212, 222212, 111122, 211122, 121122, 221122, 112122, 212122, 122122, 222122, 111222, 211222, 121222, 221222, 112222, 212222, 122222, 222222 } );
	EXPECT_EQ( 64u, A.size() );

	// Linear indexing
	EXPECT_EQ( 111111, A[ 0 ] );
	EXPECT_EQ( 211111, A[ 1 ] );
	EXPECT_EQ( 121111, A[ 2 ] );
	EXPECT_EQ( 221111, A[ 3 ] );
	EXPECT_EQ( 112111, A[ 4 ] );
	EXPECT_EQ( 212111, A[ 5 ] );
	EXPECT_EQ( 122111, A[ 6 ] );
	EXPECT_EQ( 222111, A[ 7 ] );
	EXPECT_EQ( 111211, A[ 8 ] );
	EXPECT_EQ( 211211, A[ 9 ] );
	EXPECT_EQ( 121211, A[ 10 ] );
	EXPECT_EQ( 221211, A[ 11 ] );
	EXPECT_EQ( 112211, A[ 12 ] );
	EXPECT_EQ( 212211, A[ 13 ] );
	EXPECT_EQ( 122211, A[ 14 ] );
	EXPECT_EQ( 222211, A[ 15 ] );
	EXPECT_EQ( 111121, A[ 16 ] );
	EXPECT_EQ( 211121, A[ 17 ] );
	EXPECT_EQ( 121121, A[ 18 ] );
	EXPECT_EQ( 221121, A[ 19 ] );
	EXPECT_EQ( 112121, A[ 20 ] );
	EXPECT_EQ( 212121, A[ 21 ] );
	EXPECT_EQ( 122121, A[ 22 ] );
	EXPECT_EQ( 222121, A[ 23 ] );
	EXPECT_EQ( 111221, A[ 24 ] );
	EXPECT_EQ( 211221, A[ 25 ] );
	EXPECT_EQ( 121221, A[ 26 ] );
	EXPECT_EQ( 221221, A[ 27 ] );
	EXPECT_EQ( 112221, A[ 28 ] );
	EXPECT_EQ( 212221, A[ 29 ] );
	EXPECT_EQ( 122221, A[ 30 ] );
	EXPECT_EQ( 222221, A[ 31 ] );
	EXPECT_EQ( 111112, A[ 32 ] );
	EXPECT_EQ( 211112, A[ 33 ] );
	EXPECT_EQ( 121112, A[ 34 ] );
	EXPECT_EQ( 221112, A[ 35 ] );
	EXPECT_EQ( 112112, A[ 36 ] );
	EXPECT_EQ( 212112, A[ 37 ] );
	EXPECT_EQ( 122112, A[ 38 ] );
	EXPECT_EQ( 222112, A[ 39 ] );
	EXPECT_EQ( 111212, A[ 40 ] );
	EXPECT_EQ( 211212, A[ 41 ] );
	EXPECT_EQ( 121212, A[ 42 ] );
	EXPECT_EQ( 221212, A[ 43 ] );
	EXPECT_EQ( 112212, A[ 44 ] );
	EXPECT_EQ( 212212, A[ 45 ] );
	EXPECT_EQ( 122212, A[ 46 ] );
	EXPECT_EQ( 222212, A[ 47 ] );
	EXPECT_EQ( 111122, A[ 48 ] );
	EXPECT_EQ( 211122, A[ 49 ] );
	EXPECT_EQ( 121122, A[ 50 ] );
	EXPECT_EQ( 221122, A[ 51 ] );
	EXPECT_EQ( 112122, A[ 52 ] );
	EXPECT_EQ( 212122, A[ 53 ] );
	EXPECT_EQ( 122122, A[ 54 ] );
	EXPECT_EQ( 222122, A[ 55 ] );
	EXPECT_EQ( 111222, A[ 56 ] );
	EXPECT_EQ( 211222, A[ 57 ] );
	EXPECT_EQ( 121222, A[ 58 ] );
	EXPECT_EQ( 221222, A[ 59 ] );
	EXPECT_EQ( 112222, A[ 60 ] );
	EXPECT_EQ( 212222, A[ 61 ] );
	EXPECT_EQ( 122222, A[ 62 ] );
	EXPECT_EQ( 222222, A[ 63 ] );

	// Array indexing
	EXPECT_EQ( 111111, A( 1, 1, 1, 1, 1, 1 ) );
	EXPECT_EQ( 211111, A( 2, 1, 1, 1, 1, 1 ) );
	EXPECT_EQ( 121111, A( 1, 2, 1, 1, 1, 1 ) );
	EXPECT_EQ( 221111, A( 2, 2, 1, 1, 1, 1 ) );
	EXPECT_EQ( 112111, A( 1, 1, 2, 1, 1, 1 ) );
	EXPECT_EQ( 212111, A( 2, 1, 2, 1, 1, 1 ) );
	EXPECT_EQ( 122111, A( 1, 2, 2, 1, 1, 1 ) );
	EXPECT_EQ( 222111, A( 2, 2, 2, 1, 1, 1 ) );
	EXPECT_EQ( 111211, A( 1, 1, 1, 2, 1, 1 ) );
	EXPECT_EQ( 211211, A( 2, 1, 1, 2, 1, 1 ) );
	EXPECT_EQ( 121211, A( 1, 2, 1, 2, 1, 1 ) );
	EXPECT_EQ( 221211, A( 2, 2, 1, 2, 1, 1 ) );
	EXPECT_EQ( 112211, A( 1, 1, 2, 2, 1, 1 ) );
	EXPECT_EQ( 212211, A( 2, 1, 2, 2, 1, 1 ) );
	EXPECT_EQ( 122211, A( 1, 2, 2, 2, 1, 1 ) );
	EXPECT_EQ( 222211, A( 2, 2, 2, 2, 1, 1 ) );
	EXPECT_EQ( 111121, A( 1, 1, 1, 1, 2, 1 ) );
	EXPECT_EQ( 211121, A( 2, 1, 1, 1, 2, 1 ) );
	EXPECT_EQ( 121121, A( 1, 2, 1, 1, 2, 1 ) );
	EXPECT_EQ( 221121, A( 2, 2, 1, 1, 2, 1 ) );
	EXPECT_EQ( 112121, A( 1, 1, 2, 1, 2, 1 ) );
	EXPECT_EQ( 212121, A( 2, 1, 2, 1, 2, 1 ) );
	EXPECT_EQ( 122121, A( 1, 2, 2, 1, 2, 1 ) );
	EXPECT_EQ( 222121, A( 2, 2, 2, 1, 2, 1 ) );
	EXPECT_EQ( 111221, A( 1, 1, 1, 2, 2, 1 ) );
	EXPECT_EQ( 211221, A( 2, 1, 1, 2, 2, 1 ) );
	EXPECT_EQ( 121221, A( 1, 2, 1, 2, 2, 1 ) );
	EXPECT_EQ( 221221, A( 2, 2, 1, 2, 2, 1 ) );
	EXPECT_EQ( 112221, A( 1, 1, 2, 2, 2, 1 ) );
	EXPECT_EQ( 212221, A( 2, 1, 2, 2, 2, 1 ) );
	EXPECT_EQ( 122221, A( 1, 2, 2, 2, 2, 1 ) );
	EXPECT_EQ( 222221, A( 2, 2, 2, 2, 2, 1 ) );
	EXPECT_EQ( 111112, A( 1, 1, 1, 1, 1, 2 ) );
	EXPECT_EQ( 211112, A( 2, 1, 1, 1, 1, 2 ) );
	EXPECT_EQ( 121112, A( 1, 2, 1, 1, 1, 2 ) );
	EXPECT_EQ( 221112, A( 2, 2, 1, 1, 1, 2 ) );
	EXPECT_EQ( 112112, A( 1, 1, 2, 1, 1, 2 ) );
	EXPECT_EQ( 212112, A( 2, 1, 2, 1, 1, 2 ) );
	EXPECT_EQ( 122112, A( 1, 2, 2, 1, 1, 2 ) );
	EXPECT_EQ( 222112, A( 2, 2, 2, 1, 1, 2 ) );
	EXPECT_EQ( 111212, A( 1, 1, 1, 2, 1, 2 ) );
	EXPECT_EQ( 211212, A( 2, 1, 1, 2, 1, 2 ) );
	EXPECT_EQ( 121212, A( 1, 2, 1, 2, 1, 2 ) );
	EXPECT_EQ( 221212, A( 2, 2, 1, 2, 1, 2 ) );
	EXPECT_EQ( 112212, A( 1, 1, 2, 2, 1, 2 ) );
	EXPECT_EQ( 212212, A( 2, 1, 2, 2, 1, 2 ) );
	EXPECT_EQ( 122212, A( 1, 2, 2, 2, 1, 2 ) );
	EXPECT_EQ( 222212, A( 2, 2, 2, 2, 1, 2 ) );
	EXPECT_EQ( 111122, A( 1, 1, 1, 1, 2, 2 ) );
	EXPECT_EQ( 211122, A( 2, 1, 1, 1, 2, 2 ) );
	EXPECT_EQ( 121122, A( 1, 2, 1, 1, 2, 2 ) );
	EXPECT_EQ( 221122, A( 2, 2, 1, 1, 2, 2 ) );
	EXPECT_EQ( 112122, A( 1, 1, 2, 1, 2, 2 ) );
	EXPECT_EQ( 212122, A( 2, 1, 2, 1, 2, 2 ) );
	EXPECT_EQ( 122122, A( 1, 2, 2, 1, 2, 2 ) );
	EXPECT_EQ( 222122, A( 2, 2, 2, 1, 2, 2 ) );
	EXPECT_EQ( 111222, A( 1, 1, 1, 2, 2, 2 ) );
	EXPECT_EQ( 211222, A( 2, 1, 1, 2, 2, 2 ) );
	EXPECT_EQ( 121222, A( 1, 2, 1, 2, 2, 2 ) );
	EXPECT_EQ( 221222, A( 2, 2, 1, 2, 2, 2 ) );
	EXPECT_EQ( 112222, A( 1, 1, 2, 2, 2, 2 ) );
	EXPECT_EQ( 212222, A( 2, 1, 2, 2, 2, 2 ) );
	EXPECT_EQ( 122222, A( 1, 2, 2, 2, 2, 2 ) );
	EXPECT_EQ( 222222, A( 2, 2, 2, 2, 2, 2 ) );
}

TEST( FArray6Test, Predicates )
{
	FArray6D_int A1;
	EXPECT_FALSE( A1.active() );
	EXPECT_FALSE( A1.allocated() );
	EXPECT_TRUE( A1.is_contiguous() );
	EXPECT_TRUE( A1.data_size_bounded() );
	EXPECT_FALSE( A1.data_size_unbounded() );
	EXPECT_TRUE( A1.empty() );
	EXPECT_TRUE( A1.size_bounded() );
	EXPECT_FALSE( A1.size_unbounded() );
	EXPECT_TRUE( A1.owner() );
	EXPECT_FALSE( A1.proxy() );
	EXPECT_TRUE( A1.is_default() );
	EXPECT_TRUE( A1.is_zero() );
	EXPECT_TRUE( A1.is_uniform() );
	EXPECT_TRUE( A1.is_uniform( 0 ) );

	FArray6D_int A2( 2, 3, 2, 2, 3, 4 ); // Uninitialized
	EXPECT_TRUE( A2.active() );
	EXPECT_TRUE( A2.allocated() );
	EXPECT_TRUE( A2.is_contiguous() );
	EXPECT_TRUE( A2.data_size_bounded() );
	EXPECT_FALSE( A2.data_size_unbounded() );
	EXPECT_FALSE( A2.empty() );
	EXPECT_TRUE( A2.data_size_bounded() );
	EXPECT_FALSE( A2.data_size_unbounded() );
	EXPECT_TRUE( A2.owner() );
	EXPECT_FALSE( A2.proxy() );

	FArray6D_int A3( 2, 3, 2, 2, 3, 4, 31459 );
	EXPECT_TRUE( A3.active() );
	EXPECT_TRUE( A3.allocated() );
	EXPECT_TRUE( A3.is_contiguous() );
	EXPECT_TRUE( A3.data_size_bounded() );
	EXPECT_FALSE( A3.data_size_unbounded() );
	EXPECT_FALSE( A3.empty() );
	EXPECT_TRUE( A3.data_size_bounded() );
	EXPECT_FALSE( A3.data_size_unbounded() );
	EXPECT_TRUE( A3.owner() );
	EXPECT_FALSE( A3.proxy() );
	EXPECT_FALSE( A3.is_default() );
	EXPECT_FALSE( A3.is_zero() );
	EXPECT_TRUE( A3.is_uniform() );
	EXPECT_TRUE( A3.is_uniform( 31459 ) );

	FArray6D_int A4( 2, 2, 2, 2, 2, 2, { 111111, 211111, 121111, 221111, 112111, 212111, 122111, 222111, 111211, 211211, 121211, 221211, 112211, 212211, 122211, 222211, 111121, 211121, 121121, 221121, 112121, 212121, 122121, 222121, 111221, 211221, 121221, 221221, 112221, 212221, 122221, 222221,
	                                     111112, 211112, 121112, 221112, 112112, 212112, 122112, 222112, 111212, 211212, 121212, 221212, 112212, 212212, 122212, 222212, 111122, 211122, 121122, 221122, 112122, 212122, 122122, 222122, 111222, 211222, 121222, 221222, 112222, 212222, 122222, 222222 } );
	EXPECT_TRUE( A4.active() );
	EXPECT_TRUE( A4.allocated() );
	EXPECT_TRUE( A4.is_contiguous() );
	EXPECT_TRUE( A4.data_size_bounded() );
	EXPECT_FALSE( A4.data_size_unbounded() );
	EXPECT_FALSE( A4.empty() );
	EXPECT_TRUE( A4.data_size_bounded() );
	EXPECT_FALSE( A4.data_size_unbounded() );
	EXPECT_TRUE( A4.owner() );
	EXPECT_FALSE( A4.proxy() );
	EXPECT_FALSE( A4.is_default() );
	EXPECT_FALSE( A4.is_zero() );
	EXPECT_FALSE( A4.is_uniform() );
	EXPECT_FALSE( A4.is_uniform( 0 ) );
	EXPECT_FALSE( A4.is_uniform( 111111 ) );
}

TEST( FArray6Test, PredicateComparisonsValues )
{
	FArray6D_int A1;
	EXPECT_TRUE( eq( A1, 0 ) && eq( 0, A1 ) ); // Empty array is considered to equal any scalar (no values don't equal the scalar)
	EXPECT_FALSE( ne( A1, 0 ) || ne( 0, A1 ) );
	EXPECT_FALSE( lt( A1, 0 ) || lt( 0, A1 ) );
	EXPECT_TRUE( le( A1, 0 ) && le( 0, A1 ) );
	EXPECT_FALSE( gt( A1, 0 ) || gt( 0, A1 ) );
	EXPECT_TRUE( ge( A1, 0 ) && ge( 0, A1 ) );

	FArray6D_int A2( 2, 3, 2, 2, 3, 4, 31459 );
	EXPECT_TRUE( eq( A2, 31459 ) && eq( 31459, A1 ) );
	EXPECT_FALSE( ne( A2, 31459 ) || ne( 31459, A2 ) );
	EXPECT_TRUE( lt( A2, 31460 ) && lt( 31458, A2 ) );
	EXPECT_TRUE( le( A2, 31459 ) && le( 31459, A2 ) );
	EXPECT_TRUE( le( A2, 31460 ) && le( 31458, A2 ) );
	EXPECT_TRUE( gt( A2, 31458 ) && gt( 31460, A2 ) );
	EXPECT_TRUE( ge( A2, 31459 ) && ge( 31459, A2 ) );
	EXPECT_TRUE( ge( A2, 31458 ) && ge( 31460, A2 ) );

	// Elements compared in order
	FArray6D_int A3( 2, 2, 2, 2, 2, 2, { 111111, 211111, 121111, 221111, 112111, 212111, 122111, 222111, 111211, 211211, 121211, 221211, 112211, 212211, 122211, 222211, 111121, 211121, 121121, 221121, 112121, 212121, 122121, 222121, 111221, 211221, 121221, 221221, 112221, 212221, 122221, 222221,
	                                     111112, 211112, 121112, 221112, 112112, 212112, 122112, 222112, 111212, 211212, 121212, 221212, 112212, 212212, 122212, 222212, 111122, 211122, 121122, 221122, 112122, 212122, 122122, 222122, 111222, 211222, 121222, 221222, 112222, 212222, 122222, 222222 } );
	EXPECT_FALSE( eq( A3, 111111 ) || eq( 222222, A3 ) );
	EXPECT_TRUE( ne( A3, 111111 ) && ne( 222222, A3 ) );
	EXPECT_TRUE( lt( A3, 222223 ) && lt( 111110, A3 ) );
	EXPECT_FALSE( lt( A3, 211111 ) || lt( 111111, A3 ) );
	EXPECT_TRUE( le( A3, 233333 ) && le( 111111, A3 ) );
	EXPECT_TRUE( gt( A3, 111110 ) && gt( 244444, A3 ) );
	EXPECT_FALSE( gt( A3, 111111 ) || gt( 222222, A3 ) );
	EXPECT_TRUE( ge( A3, 111111 ) && ge( 233333, A3 ) );
}
