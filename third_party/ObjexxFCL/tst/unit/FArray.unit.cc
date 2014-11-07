// ObjexxFCL::FArray Unit Tests
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
#include <ObjexxFCL/FArray.all.hh>
#include <ObjexxFCL/FArray.all.io.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/DimensionExpressions.hh>
#include <ObjexxFCL/FArray.functions.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <array>

using namespace ObjexxFCL;

TEST( FArrayTest, DefaultConstruction )
{
	FArray2D_int A, B;
	EXPECT_EQ( 0u, A.size() );
	A.zero(); // Now safe against VC Checked Iterators
	EXPECT_EQ( 0u, A.size() );
	EXPECT_TRUE( eq( A.to_default(), B ) ); // Now safe against VC Checked Iterators
}

TEST( FArrayTest, Construction2DIndexRangeInitializerList )
{
	FArray2D_int r( SRange( -1, 1 ), SRange( -1, 1 ), { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	EXPECT_EQ( -1, r.l1() );
	EXPECT_EQ( -1, lbound( r, 1 ) );
	EXPECT_EQ( 1, r.u1() );
	EXPECT_EQ( 1, ubound( r, 1 ) );
	EXPECT_EQ( -1, r.l2() );
	EXPECT_EQ( -1, lbound( r, 2 ) );
	EXPECT_EQ( 1, r.u2() );
	EXPECT_EQ( 1, ubound( r, 2 ) );
	EXPECT_TRUE( eq( FArray1D< int >( 2, { -1, -1 } ), lbound( r ) ) );
	EXPECT_TRUE( eq( FArray1D< int >( 2, { 1, 1 } ), ubound( r ) ) );
	for ( int j = -1, k = 1; j <= 1; ++j ) {
		for ( int i = -1; i <= 1; ++i, ++k ) {
			EXPECT_EQ( k, r( i, j ) );
		}
	}
}

TEST( FArrayTest, Construction2DDifferentValueType )
{
	FArray2D_int A( 3, 3, 33 );
	FArray2D_double B( A );
	EXPECT_TRUE( eq( FArray2D_double( 3, 3, 33.0 ), B ) );
}

TEST( FArrayTest, Assignment2D )
{
	FArray2D_int A( 3, 3, 33 );
	FArray2D_int B( DRange( 0, 4 ), DRange( 0, 4 ), 44 );
	A = B;
	EXPECT_TRUE( eq( B, A ) );
}

TEST( FArrayTest, Assignment2DRedimensionDifferentValueType )
{
	FArray2D_int A( 3, 3, 33 );
	FArray2D_double B( DRange( 0, 4 ), DRange( 0, 4 ), 4.4 );
	A = B;
	EXPECT_TRUE( eq( FArray2D_int( DRange( 0, 4 ), DRange( 0, 4 ), 4 ), A ) );
}

TEST( FArrayTest, Assignment2DNoOverlapProxy )
{
	FArray2D_int A( 3, 3, 33 );
	FArray2P_int B( A( 2, 2 ), 2, 2 );
	B = 44;
	FArray2P_int C( A( 1, 1 ), 2, 2 );
	C = 55;
	EXPECT_TRUE( A.overlap( B ) );
	EXPECT_TRUE( A.overlap( C ) );
	EXPECT_TRUE( B.overlap( A ) );
	EXPECT_TRUE( C.overlap( A ) );
	EXPECT_FALSE( B.overlap( C ) );
	EXPECT_FALSE( C.overlap( B ) );
	B = C;
	EXPECT_TRUE( eq( C, B ) );
	FArray2P_int D( A( 3, 3 ), 1, 1 );
	EXPECT_FALSE( C.overlap( D ) );
	EXPECT_FALSE( D.overlap( C ) );
	EXPECT_FALSE( B.overlap( D ) );
	EXPECT_FALSE( D.overlap( B ) );
	EXPECT_EQ( 55, A( 1, 1 ) );
	EXPECT_EQ( 55, A( 2, 1 ) );
	EXPECT_EQ( 55, A( 3, 1 ) );
	EXPECT_EQ( 55, A( 1, 2 ) );
	EXPECT_EQ( 55, A( 2, 2 ) );
	EXPECT_EQ( 55, A( 3, 2 ) );
	EXPECT_EQ( 55, A( 1, 3 ) );
	EXPECT_EQ( 55, A( 2, 3 ) );
	EXPECT_EQ( 33, A( 3, 3 ) );
}

TEST( FArrayTest, Assignment2DOverlapProxy )
{
	FArray2D_int A( 3, 3, 33 );
	FArray2P_int B( A( 1, 1 ), 3, 2 ); // Cols 1-2 of A
	B = 44;
	EXPECT_EQ( 44, A( 1, 1 ) );
	EXPECT_EQ( 44, A( 2, 1 ) );
	EXPECT_EQ( 44, A( 3, 1 ) );
	EXPECT_EQ( 44, A( 1, 2 ) );
	EXPECT_EQ( 44, A( 2, 2 ) );
	EXPECT_EQ( 44, A( 3, 2 ) );
	EXPECT_EQ( 33, A( 1, 3 ) );
	EXPECT_EQ( 33, A( 2, 3 ) );
	EXPECT_EQ( 33, A( 3, 3 ) );
	FArray2P_int C( A( 1, 2 ), 3, 2 ); // Cols 2-3 of A
	C = 55;
	EXPECT_TRUE( A.overlap( B ) );
	EXPECT_TRUE( A.overlap( C ) );
	EXPECT_TRUE( B.overlap( A ) );
	EXPECT_TRUE( C.overlap( A ) );
	EXPECT_TRUE( B.overlap( C ) );
	EXPECT_TRUE( C.overlap( B ) );
	B = C;
	EXPECT_TRUE( eq( C, B ) );
	EXPECT_TRUE( eq( 55, A ) );
}

TEST( FArrayTest, Assignment2DOverlapProxyVarying )
{
	FArray2D_int A( 2, 2, 1 );
	FArray1P_int B( A( 1, 1 ), 3 ); // First 3 elements of A
	B( 1 ) = 1;
	B( 2 ) = 2;
	B( 3 ) = 3;
	EXPECT_EQ( 1, A( 1, 1 ) );
	EXPECT_EQ( 2, A( 2, 1 ) );
	EXPECT_EQ( 3, A( 1, 2 ) );
	EXPECT_EQ( 1, A( 2, 2 ) );
	FArray1P_int C( A( 2, 1 ), 3 ); // Last 3 elements of A
	EXPECT_TRUE( A.overlap( B ) );
	EXPECT_TRUE( A.overlap( C ) );
	EXPECT_TRUE( B.overlap( A ) );
	EXPECT_TRUE( C.overlap( A ) );
	EXPECT_TRUE( B.overlap( C ) );
	EXPECT_TRUE( C.overlap( B ) );
	C = B; // Need overlap-safe copy
	EXPECT_FALSE( eq( C, B ) ); // Differ after overlapping assignment
	EXPECT_FALSE( eq( B, C ) ); // Differ after overlapping assignment
	EXPECT_EQ( 1, A( 1, 1 ) );
	EXPECT_EQ( 1, A( 2, 1 ) );
	EXPECT_EQ( 2, A( 1, 2 ) );
	EXPECT_EQ( 3, A( 2, 2 ) );
}

TEST( FArrayTest, Operators2D )
{
	FArray2D_int A( 3, 3, 33 );
	FArray2A_int B( A );
	A += B;
	EXPECT_TRUE( eq( FArray2D_int( 3, 3, 66 ), A ) );
	EXPECT_TRUE( eq( FArray2D_int( 3, 3, 66 ), B ) );
	A += 1;
	EXPECT_TRUE( eq( FArray2D_int( 3, 3, 67 ), A ) );
	EXPECT_TRUE( eq( FArray2D_int( 3, 3, 67 ), B ) );
}

TEST( FArrayTest, Operators6D )
{
	FArray6D_int A( 3, 3, 3, 3, 3, 3, 33 );
	FArray6A_int B( A );
	A += B;
	EXPECT_TRUE( eq( FArray6D_int( 3, 3, 3, 3, 3, 3, 66 ), A ) );
	EXPECT_TRUE( eq( FArray6D_int( 3, 3, 3, 3, 3, 3, 66 ), B ) );
	A += 1;
	EXPECT_TRUE( eq( FArray6D_int( 3, 3, 3, 3, 3, 3, 67 ), A ) );
	EXPECT_TRUE( eq( FArray6D_int( 3, 3, 3, 3, 3, 3, 67 ), B ) );
}

TEST( FArrayTest, Redimension2D )
{
	FArray2D_int A( 4, 4, 44 );
	A.redimension( 5, 5, 55 ); // Redimension by index ranges
	EXPECT_EQ( DRange( 1, 5 ), A.I1() );
	EXPECT_EQ( DRange( 1, 5 ), A.I2() );
	EXPECT_EQ( 5U, A.size1() );
	EXPECT_EQ( 5U, A.size2() );
	EXPECT_EQ( 5U * 5U, A.size() );
	for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
		for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
			if ( i1 <= 4 && i2 <= 4 ) {
				EXPECT_EQ( 44, A( i1, i2 ) );
			} else {
				EXPECT_EQ( 55, A( i1, i2 ) );
			}
		}
	}
	FArray2D_float B( 6, 6 );
	A.redimension( B, 66 ); // Redimension by another array
	for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
		for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
			if ( i1 <= 4 && i2 <= 4 ) {
				EXPECT_EQ( 44, A( i1, i2 ) );
			} else if ( i1 <= 5 && i2 <= 5 ) {
				EXPECT_EQ( 55, A( i1, i2 ) );
			} else {
				EXPECT_EQ( 66, A( i1, i2 ) );
			}
		}
	}
}

TEST( FArrayTest, Transpose )
{
	FArray2D_int A( 2, 3 );
	A( 1, 1 ) = 4;
	A( 1, 2 ) = 3;
	A( 1, 3 ) = 5;
	A( 2, 1 ) = 9;
	A( 2, 2 ) = 2;
	A( 2, 3 ) = 8;
	FArray2D_int B( transpose( A ) );
	EXPECT_EQ( 3u, B.size1() );
	EXPECT_EQ( 2u, B.size2() );
	EXPECT_EQ( A( 1, 1 ), B( 1, 1 ) );
	EXPECT_EQ( A( 1, 2 ), B( 2, 1 ) );
	EXPECT_EQ( A( 1, 3 ), B( 3, 1 ) );
	EXPECT_EQ( A( 2, 1 ), B( 1, 2 ) );
	EXPECT_EQ( A( 2, 2 ), B( 2, 2 ) );
	EXPECT_EQ( A( 2, 3 ), B( 3, 2 ) );
}

TEST( FArrayTest, Redimension2DDimension )
{
	Dimension N( 5 );
	FArray2D_float v( N, 8, 1.23f );
	v.redimension( N, 3 );
	EXPECT_TRUE( eq( FArray2D_float( 5, 3, 1.23f ), v ) );
}

TEST( FArrayTest, Redimension3DFill )
{
	FArray3D_int A( 4, 4, 4, 44 );
	A.redimension( 5, 5, 5, 55 ); // Redimension by index ranges
	EXPECT_EQ( DRange( 1, 5 ), A.I1() );
	EXPECT_EQ( DRange( 1, 5 ), A.I2() );
	EXPECT_EQ( DRange( 1, 5 ), A.I3() );
	EXPECT_EQ( 5u, A.size1() );
	EXPECT_EQ( 5u, A.size2() );
	EXPECT_EQ( 5u, A.size3() );
	EXPECT_EQ( 5u * 5u * 5u, A.size() );
	for ( int i3 = A.l3(); i3 <= A.u3(); ++i3 ) {
		for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
			for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
				if ( i1 <= 4 && i2 <= 4 && i3 <= 4 ) {
					EXPECT_EQ( 44, A( i1, i2, i3 ) );
				} else {
					EXPECT_EQ( 55, A( i1, i2, i3 ) );
				}
			}
		}
	}
}

TEST( FArrayTest, Swap3D )
{
	FArray3D_int A( 4, 4, 4, 44 );
	FArray3D_int( 5, 5, 5, 55 ).swap( A );
	EXPECT_EQ( DRange( 1, 5 ), A.I1() );
	EXPECT_EQ( DRange( 1, 5 ), A.I2() );
	EXPECT_EQ( DRange( 1, 5 ), A.I3() );
	EXPECT_EQ( 5u, A.size1() );
	EXPECT_EQ( 5u, A.size2() );
	EXPECT_EQ( 5u, A.size3() );
	EXPECT_EQ( 5u * 5u * 5u, A.size() );
	for ( std::size_t i = 0; i < A.size(); ++i ) {
		EXPECT_EQ( 55, A[ i ] );
	}
}

TEST( FArrayTest, Eoshift2DDim1 )
{
	FArray2D_int A( 3, 3, reshape( { 11, 21, 31, 12, 22, 32, 13, 23, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	FArray2D_int B( 3, 3, reshape( { 31, 0, 0, 32, 0, 0, 33, 0, 0 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, eoshift( A, 2, 0, 1 ) ) );
	FArray2D_int C( 3, 3, reshape( { 31, 99, 99, 32, 99, 99, 33, 99, 99 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( C, eoshift( A, 2, 99, 1 ) ) );
}

TEST( FArrayTest, Eoshift2DDim2 )
{
	FArray2D_int A( 3, 3, reshape( { 11, 21, 31, 12, 22, 32, 13, 23, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	FArray2D_int B( 3, 3, reshape( { 0, 0, 0, 0, 0, 0, 11, 21, 31 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, eoshift( A, -2, 0, 2 ) ) );
	FArray2D_int C( 3, 3, reshape( { 99, 99, 99, 99, 99, 99, 11, 21, 31 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( C, eoshift( A, -2, 99, 2 ) ) );
}

TEST( FArrayTest, Cshift2DDim1 )
{
	FArray2D_int A( 3, 3, reshape( { 11, 21, 31, 12, 22, 32, 13, 23, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	FArray2D_int B( 3, 3, reshape( { 31, 11, 21, 32, 12, 22, 33, 13, 23 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, cshift( A, 2, 1 ) ) );
}

TEST( FArrayTest, Cshift2DDim2 )
{
	FArray2D_int A( 3, 3, reshape( { 11, 21, 31, 12, 22, 32, 13, 23, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	FArray2D_int B( 3, 3, reshape( { 12, 22, 32, 13, 23, 33, 11, 21, 31 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, cshift( A, -2, 2 ) ) );
}

TEST( FArrayTest, Cshift2DNonSquareDim2 )
{
	FArray2D_int A( 3, 2, reshape( { 11, 21, 31, 12, 22, 32 }, std::array< int, 2 >{ { 3, 2 } } ) );
	FArray2D_int B( 3, 2, reshape( { 12, 22, 32, 11, 21, 31 }, std::array< int, 2 >{ { 3, 2 } } ) );
	EXPECT_TRUE( eq( B, cshift( A, -1, 2 ) ) );
}

TEST( FArrayTest, Cshift2DDim1Array )
{
	FArray2D_int A( 3, 3, reshape( { 11, 21, 31, 12, 22, 32, 13, 23, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	FArray2D_int B( 3, 3, reshape( { 31, 11, 21, 32, 12, 22, 33, 13, 23 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, cshift( A, FArray1D_int( 3, 2 ), 1 ) ) );
	FArray2D_int C( 3, 3, reshape( { 31, 11, 21, 12, 22, 32, 33, 13, 23 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( C, cshift( A, FArray1D_int( 3, {2,0,2} ), 1 ) ) ); // Non-uniform shifts
}

TEST( FArrayTest, Pow2D )
{
	FArray2D_int A( 3, 3, 12 );
	FArray2D_int B( pow( A, 2 ) );
	FArray2D_int S( 3, 3, 144 );
	EXPECT_TRUE( eq( S, B ) );
}

TEST( FArrayTest, Matmul11 )
{
	FArray1D_int A( 2 );
	FArray1D_int B( 2 );
	A( 1 ) = 4;
	A( 2 ) = 3;
	B( 1 ) = 7;
	B( 2 ) = 5;
	FArray2D_int R( 2, 2 );
	R( 1, 1 ) = 28;
	R( 1, 2 ) = 20;
	R( 2, 1 ) = 21;
	R( 2, 2 ) = 15;
	EXPECT_TRUE( eq( R, matmul( A, B ) ) );
}

TEST( FArrayTest, Matmul12 )
{
	FArray1D_int A( 3 );
	FArray2D_int B( 3, 2 );
	A( 1 ) = 4;
	A( 2 ) = 3;
	A( 3 ) = 5;
	B( 1, 1 ) = 7;
	B( 1, 2 ) = 2;
	B( 2, 1 ) = 5;
	B( 2, 2 ) = 0;
	B( 3, 1 ) = 4;
	B( 3, 2 ) = 6;
	FArray1D_int R( 2 );
	R( 1 ) = 63;
	R( 2 ) = 38;
	EXPECT_TRUE( eq( R, matmul( A, B ) ) );
}

TEST( FArrayTest, Matmul21 )
{
	FArray2D_int A( 2, 3 );
	FArray1D_int B( 3 );
	A( 1, 1 ) = 4;
	A( 1, 2 ) = 3;
	A( 1, 3 ) = 5;
	A( 2, 1 ) = 9;
	A( 2, 2 ) = 2;
	A( 2, 3 ) = 8;
	B( 1 ) = 7;
	B( 2 ) = 5;
	B( 3 ) = 4;
	FArray1D_int R( 2 );
	R( 1 ) = 63;
	R( 2 ) = 105;
	EXPECT_TRUE( eq( R, matmul( A, B ) ) );
}

TEST( FArrayTest, Matmul22 )
{
	FArray2D_int A( 2, 3 );
	FArray2D_int B( 3, 2 );
	A( 1, 1 ) = 4;
	A( 1, 2 ) = 3;
	A( 1, 3 ) = 5;
	A( 2, 1 ) = 9;
	A( 2, 2 ) = 2;
	A( 2, 3 ) = 8;
	B( 1, 1 ) = 7;
	B( 1, 2 ) = 2;
	B( 2, 1 ) = 5;
	B( 2, 2 ) = 0;
	B( 3, 1 ) = 4;
	B( 3, 2 ) = 6;
	FArray2D_int R( 2, 2 );
	R( 1, 1 ) = 63;
	R( 1, 2 ) = 38;
	R( 2, 1 ) = 105;
	R( 2, 2 ) = 66;
	EXPECT_TRUE( eq( R, matmul( A, B ) ) );
}

TEST( FArrayTest, Matrix2DMultiplication )
{
	SRange I1( 3 ), I2( 3 );

	FArray2D_int A( I1, I2 );
	for ( int i = A.l1(), ie = A.u1(); i <= ie; ++i ) {
		for ( int j = A.l2(), je = A.u2(); j <= je; ++j ) {
			A( i, j ) = i + j * 2;
		}
	}

	FArray2D_int B( I1, I2 );
	for ( int i = B.l1(), ie = B.u1(); i <= ie; ++i ) {
		for ( int j = B.l2(), je = B.u2(); j <= je; ++j ) {
			B( i, j ) = i * 2 + j * 3;
		}
	}

	FArray2D_int C( I1, I2 );
	for ( int i = A.l1(), ie = A.u1(); i <= ie; ++i ) {
		for ( int j = A.l2(), je = A.u2(), jj = B.l2(); j <= je; ++j, ++jj ) {
			int sum = 0;
			for ( int k = A.l2(), ke = A.u2(), kk = B.l1(); k <= ke; ++k, ++kk ) {
				sum += A( i, k ) * B( kk, jj );
			}
			C( i, j ) = sum;
		}
	}

	A.right_multiply_by( B );
	EXPECT_TRUE( eq( C, A ) );
}

TEST( FArrayTest, Matrix2DMultiplicationTranspose )
{
	SRange I1( 3 ), I2( 3 );

	FArray2D_int A( I1, I2 );
	for ( int i = A.l1(), ie = A.u1(); i <= ie; ++i ) {
		for ( int j = A.l2(), je = A.u2(); j <= je; ++j ) {
			A( i, j ) = i + j * 2;
		}
	}

	FArray2D_int B( I1, I2 );
	for ( int i = B.l1(), ie = B.u1(); i <= ie; ++i ) {
		for ( int j = B.l2(), je = B.u2(); j <= je; ++j ) {
			B( i, j ) = i * 2 + j * 3;
		}
	}

	FArray2D_int C( I1, I2 );
	for ( int i = A.l1(), ie = A.u1(); i <= ie; ++i ) {
		for ( int j = A.l2(), je = A.u2(), jj = B.l2(); j <= je; ++j, ++jj ) {
			int sum = 0;
			for ( int k = A.l2(), ke = A.u2(), kk = B.l1(); k <= ke; ++k, ++kk ) {
				sum += A( i, k ) * B( jj, kk );
			}
			C( i, j ) = sum;
		}
	}

	A.right_multiply_by_transpose( B );
	EXPECT_TRUE( eq( C, A ) );
}

TEST( FArrayTest, Generation2DValueMinusArray )
{
	FArray2D_int A( 3, 3, 33 ), B( 44 - A );
	EXPECT_TRUE( eq( FArray2D_int( 3, 3, 11 ), B ) );
}

TEST( FArrayTest, Cross1D )
{
	FArray1D_int A( 3, 33 ), B( 44 - A );
	EXPECT_TRUE( eq( cross( A, B ), cross_product( A, B ) ) );
}

TEST( FArrayTest, LogicalNegation )
{
	FArray2D_bool const F( 2, 2, false );
	EXPECT_FALSE( F( 1, 1 ) );
	EXPECT_FALSE( F( 2, 1 ) );
	EXPECT_FALSE( F( 1, 2 ) );
	EXPECT_FALSE( F( 2, 2 ) );
	FArray2D_bool const T( ! F );
	EXPECT_TRUE( T( 1, 1 ) );
	EXPECT_TRUE( T( 2, 1 ) );
	EXPECT_TRUE( T( 1, 2 ) );
	EXPECT_TRUE( T( 2, 2 ) );
}

TEST( FArrayTest, UboundOfUnbounded )
{
	FArray2D_int r( {-1,1}, {-1,1}, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	FArray2A_int u( r( -1, -1 ) ); // Unbounded tail proxy
	u.dim( 3, _ );
	EXPECT_EQ( 1, lbound( u, 1 ) );
	EXPECT_EQ( 1, lbound( u, 2 ) );
	EXPECT_EQ( 3, ubound( u, 1 ) );
	EXPECT_DEBUG_DEATH( ubound( u, 2 ), ".*Assertion.*" ); // Can't take ubound of unbounded dimension
}

TEST( FArrayTest, EmptyComparisonPredicate )
{
	FArray1D_int a( 0 ), b( 0 ); // Empty
	EXPECT_EQ( 0u, a.size() );
	EXPECT_EQ( 0u, b.size() );
	EXPECT_EQ( 1, lbound( a, 1 ) );
	EXPECT_EQ( 0, ubound( a, 1 ) );
	EXPECT_TRUE( eq( a, b ) );
	EXPECT_FALSE( ne( a, b ) );
}

TEST( FArrayTest, EmptyComparisonElemental )
{
	FArray1D_int a( 0 ), b( 0 ); // Empty
	EXPECT_EQ( 0u, a.size() );
	EXPECT_EQ( 0u, b.size() );
	EXPECT_EQ( 1, lbound( a, 1 ) );
	EXPECT_EQ( 0, ubound( a, 1 ) );
	EXPECT_EQ( 0u, ( a == b ).size() );
	EXPECT_EQ( 0u, ( a > b ).size() );
}

TEST( FArrayTest, Unallocated )
{
	FArray1D_int a; // Empty
	EXPECT_FALSE( a.allocated() );
	EXPECT_FALSE( allocated( a ) );
	EXPECT_DEBUG_DEATH( a( 1 ), ".*Assertion.*" );
}

TEST( FArrayTest, AnyOp2D )
{
	FArray2D_int const A( 3, 3, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	EXPECT_TRUE( any_eq( A, 6 ) );
	EXPECT_FALSE( any_eq( A, 22 ) );
	EXPECT_TRUE( any_ne( A, 6 ) );
	EXPECT_TRUE( any_lt( A, 2 ) );
	EXPECT_TRUE( any_ge( A, 9 ) );
	EXPECT_FALSE( any_lt( A, 1 ) );
	EXPECT_FALSE( any_gt( A, 9 ) );
}

TEST( FArrayTest, AllOp2D )
{
	FArray2D_int const A( 3, 3, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	EXPECT_FALSE( all_eq( A, 6 ) );
	EXPECT_FALSE( all_eq( A, 22 ) );
	EXPECT_TRUE( all_ne( A, 22 ) );
	EXPECT_FALSE( all_ne( A, 2 ) );
	EXPECT_FALSE( all_lt( A, 2 ) );
	EXPECT_FALSE( all_ge( A, 9 ) );
	EXPECT_TRUE( all_lt( A, 11 ) );
	EXPECT_TRUE( all_gt( A, 0 ) );
}

TEST( FArrayTest, CountOp2D )
{
	FArray2D_int const A( 3, 3, { 1, 2, 2, 3, 3, 3, 7, 8, 9 } );
	EXPECT_EQ( 0u, count_eq( A, 0 ) );
	EXPECT_EQ( 1u, count_eq( A, 1 ) );
	EXPECT_EQ( 2u, count_eq( A, 2 ) );
	EXPECT_EQ( 3u, count_eq( A, 3 ) );
	EXPECT_EQ( 6u, count_lt( A, 7 ) );
	EXPECT_EQ( 1u, count_ge( A, 9 ) );
	EXPECT_EQ( 9u, count_lt( A, 11 ) );
	EXPECT_EQ( 3u, count_gt( A, 3 ) );
}

TEST( FArrayTest, Functions1D )
{
	FArray1D_int u{ 1, 2, 3 };
	FArray1D_int v{ 2, 3, 4 };
	EXPECT_EQ( 14, magnitude_squared( u ) );
	EXPECT_EQ( 3, distance_squared( u, v ) );
	EXPECT_EQ( 20, dot( u, v ) );
}
