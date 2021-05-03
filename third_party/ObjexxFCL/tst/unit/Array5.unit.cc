// ObjexxFCL::Array5 Unit Tests
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
#include <ObjexxFCL/Array5D.hh>
#include "ObjexxFCL.unit.hh"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

using namespace ObjexxFCL;

TEST( Array5Test, ConstructDefault )
{
	Array5D<int> A;
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
	EXPECT_EQ( Array5D<int>::IR(), A.I1() );
	EXPECT_EQ( Array5D<int>::IR(), A.I2() );
	EXPECT_EQ( Array5D<int>::IR(), A.I3() );
	EXPECT_EQ( Array5D<int>::IR(), A.I4() );
	EXPECT_EQ( Array5D<int>::IR(), A.I5() );
}

TEST( Array5Test, ConstructCopy )
{
	Array5D<int> A;
	Array5D<int> B( A );
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
	EXPECT_TRUE( conformable( A, B ) );
	EXPECT_TRUE( equal_dimensions( A, B ) );
	EXPECT_TRUE( eq( A, B ) );
}

TEST( Array5Test, ConstructIndexes )
{
	Array5D<int> A;
        A.allocate( 3, 3, 3, 3, 3 );
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
	EXPECT_EQ( Array5D<int>::IR( 1, 3 ), A.I1() );
	EXPECT_EQ( Array5D<int>::IR( 1, 3 ), A.I2() );
	EXPECT_EQ( Array5D<int>::IR( 1, 3 ), A.I3() );
	EXPECT_EQ( Array5D<int>::IR( 1, 3 ), A.I4() );
	EXPECT_EQ( Array5D<int>::IR( 1, 3 ), A.I5() );
}

TEST( Array5Test, RangeBasedFor )
{
	Array5D<int> A;
        A.allocate( 2, 2, 2, 2, 2 );
	int v( 0 );
	for ( auto & e : A ) {
		e = ++v;
		EXPECT_EQ( v, e );
	}
}

TEST( Array5Test, Predicates )
{
	Array5D<int> A1;
	EXPECT_FALSE( A1.active() );
	EXPECT_FALSE( A1.allocated() );
	EXPECT_TRUE( A1.empty() );
	EXPECT_TRUE( A1.size_bounded() );
	EXPECT_TRUE( A1.owner() );
	EXPECT_FALSE( A1.proxy() );

	Array5D<int> A2;
        A2.allocate( 2, 3, 2, 2, 3 ); // Uninitialized
	EXPECT_TRUE( A2.active() );
	EXPECT_TRUE( A2.allocated() );
	EXPECT_FALSE( A2.empty() );
	EXPECT_TRUE( A2.owner() );
	EXPECT_FALSE( A2.proxy() );
}

TEST( Array5Test, PredicateComparisonsValues )
{
	Array5D<int> A1;
	EXPECT_TRUE( eq( A1, 0 ) && eq( 0, A1 ) ); // Empty array is considered to equal any scalar (no values don't equal the scalar)
}
