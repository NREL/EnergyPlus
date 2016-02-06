// ObjexxFCL::Array Unit Tests
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
#include <ObjexxFCL/Array.all.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <array>

using namespace ObjexxFCL;
typedef  IndexRange  IR;

TEST( ArrayTest, DefaultConstruction )
{
	Array2D_int A, B;
	EXPECT_EQ( 0u, A.size() );
	A.zero(); // Now safe against VC Checked Iterators
	EXPECT_EQ( 0u, A.size() );
}

TEST( ArrayTest, Construction2DIndexRangeInitializerList )
{
	Array2D_int r( IR( -1, 1 ), IR( -1, 1 ), { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	EXPECT_EQ( -1, r.l1() );
	EXPECT_EQ( -1, lbound( r, 1 ) );
	EXPECT_EQ( 1, r.u1() );
	EXPECT_EQ( 1, ubound( r, 1 ) );
	EXPECT_EQ( -1, r.l2() );
	EXPECT_EQ( -1, lbound( r, 2 ) );
	EXPECT_EQ( 1, r.u2() );
	EXPECT_EQ( 1, ubound( r, 2 ) );
	EXPECT_TRUE( eq( Array1D_int( 2, { -1, -1 } ), lbound( r ) ) );
	EXPECT_TRUE( eq( Array1D_int( 2, { 1, 1 } ), ubound( r ) ) );
	for ( int i = -1, k = 1; i <= 1; ++i ) {
		for ( int j = -1; j <= 1; ++j, ++k ) {
			EXPECT_EQ( k, r( i, j ) );
		}
	}
	int v( 0 );
	for ( auto const e : r ) {
		EXPECT_EQ( ++v, e );
	}
	v = 10;
	for ( auto & e : r ) {
		e = ++v;
		EXPECT_EQ( v, e );
	}
}

TEST( ArrayTest, Construction2DDifferentValueType )
{
	Array2D_int A( 3, 3, 33 );
	Array2D_double B( A );
	EXPECT_TRUE( eq( Array2D_double( 3, 3, 33.0 ), B ) );
}

TEST( ArrayTest, Assignment2D )
{
	Array2D_int A( 3, 3, 33 );
	Array2D_int B( IR( 0, 4 ), IR( 0, 4 ), 44 );
	A = B;
	EXPECT_TRUE( eq( B, A ) );
}

TEST( ArrayTest, Assignment2DRedimensionDifferentValueType )
{
	Array2D_int A( 3, 3, 33 );
	Array2D_double B( IR( 0, 4 ), IR( 0, 4 ), 4.4 );
	A = B;
	EXPECT_TRUE( eq( Array2D_int( IR( 0, 4 ), IR( 0, 4 ), 4 ), A ) );
}

TEST( ArrayTest, Assignment2DNoOverlapProxy )
{
	Array2D_int A( 3, 3, 33 );
	Array2A_int B( A( 2, 2 ), 2, 2 );
	B = 44;
	Array2A_int C( A( 1, 1 ), 2, 2 );
	C = 55;
	EXPECT_TRUE( A.overlap( B ) );
	EXPECT_TRUE( A.overlap( C ) );
	EXPECT_TRUE( B.overlap( A ) );
	EXPECT_TRUE( C.overlap( A ) );
	EXPECT_FALSE( B.overlap( C ) );
	EXPECT_FALSE( C.overlap( B ) );
	B = C;
	EXPECT_TRUE( eq( C, B ) );
	Array2A_int D( A( 3, 3 ), 1, 1 );
	EXPECT_FALSE( C.overlap( D ) );
	EXPECT_FALSE( D.overlap( C ) );
	EXPECT_FALSE( B.overlap( D ) );
	EXPECT_FALSE( D.overlap( B ) );
	EXPECT_EQ( 55, A( 1, 1 ) );
	EXPECT_EQ( 55, A( 1, 2 ) );
	EXPECT_EQ( 55, A( 1, 3 ) );
	EXPECT_EQ( 55, A( 2, 1 ) );
	EXPECT_EQ( 55, A( 2, 2 ) );
	EXPECT_EQ( 55, A( 2, 3 ) );
	EXPECT_EQ( 55, A( 3, 1 ) );
	EXPECT_EQ( 55, A( 3, 2 ) );
	EXPECT_EQ( 33, A( 3, 3 ) );
}

TEST( ArrayTest, Assignment2DOverlapProxy )
{
	Array2D_int A( 3, 3, 33 );
	Array2A_int B( A( 1, 1 ), 2, 3 ); // Rows 1-2 of A
	B = 44;
	EXPECT_EQ( 44, A( 1, 1 ) );
	EXPECT_EQ( 44, A( 1, 2 ) );
	EXPECT_EQ( 44, A( 1, 3 ) );
	EXPECT_EQ( 44, A( 2, 1 ) );
	EXPECT_EQ( 44, A( 2, 2 ) );
	EXPECT_EQ( 44, A( 2, 3 ) );
	EXPECT_EQ( 33, A( 3, 1 ) );
	EXPECT_EQ( 33, A( 3, 2 ) );
	EXPECT_EQ( 33, A( 3, 3 ) );
	Array2A_int C( A( 2, 1 ), 2, 3 ); // Rows 2-3 of A
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

TEST( ArrayTest, Assignment2DOverlapProxyVarying )
{
	Array2D_int A( 2, 2, 1 );
	Array1A_int B( A( 1, 1 ), 3 ); // First 3 elements of A
	B( 1 ) = 1;
	B( 2 ) = 2;
	B( 3 ) = 3;
	EXPECT_EQ( 1, A( 1, 1 ) );
	EXPECT_EQ( 2, A( 1, 2 ) );
	EXPECT_EQ( 3, A( 2, 1 ) );
	EXPECT_EQ( 1, A( 2, 2 ) );
	Array1A_int C( A( 1, 2 ), 3 ); // Last 3 elements of A
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
	EXPECT_EQ( 1, A( 1, 2 ) );
	EXPECT_EQ( 2, A( 2, 1 ) );
	EXPECT_EQ( 3, A( 2, 2 ) );
}

TEST( ArrayTest, Operators2D )
{
	Array2D_int A( 3, 3, 33 );
	Array2A_int B( A );
	A += B;
	EXPECT_TRUE( eq( Array2D_int( 3, 3, 66 ), A ) );
	EXPECT_TRUE( eq( Array2D_int( 3, 3, 66 ), B ) );
	A += 1;
	EXPECT_TRUE( eq( Array2D_int( 3, 3, 67 ), A ) );
	EXPECT_TRUE( eq( Array2D_int( 3, 3, 67 ), B ) );
}

TEST( ArrayTest, Operators6D )
{
	Array6D_int A( 3, 3, 3, 3, 3, 3, 33 );
	Array6A_int B( A );
	A += B;
	EXPECT_TRUE( eq( Array6D_int( 3, 3, 3, 3, 3, 3, 66 ), A ) );
	EXPECT_TRUE( eq( Array6D_int( 3, 3, 3, 3, 3, 3, 66 ), B ) );
	A += 1;
	EXPECT_TRUE( eq( Array6D_int( 3, 3, 3, 3, 3, 3, 67 ), A ) );
	EXPECT_TRUE( eq( Array6D_int( 3, 3, 3, 3, 3, 3, 67 ), B ) );
}

TEST( ArrayTest, Redimension2D )
{
	Array2D_int A( 4, 4, 44 );
	A.redimension( 5, 5, 55 ); // Redimension by index ranges
	EXPECT_EQ( IR( 1, 5 ), A.I1() );
	EXPECT_EQ( IR( 1, 5 ), A.I2() );
	EXPECT_EQ( 5U, A.size1() );
	EXPECT_EQ( 5U, A.size2() );
	EXPECT_EQ( 5U * 5U, A.size() );
	for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
		for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
			if ( i1 <= 4 && i2 <= 4 ) {
				EXPECT_EQ( 44, A( i1, i2 ) );
			} else {
				EXPECT_EQ( 55, A( i1, i2 ) );
			}
		}
	}
	Array2D_float B( 6, 6 );
	A.redimension( B, 66 ); // Redimension by another array
	for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
		for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
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

TEST( ArrayTest, Redimension3DFill )
{
	Array3D_int A( 4, 4, 4, 44 );
	A.redimension( 5, 5, 5, 55 ); // Redimension by index ranges
	EXPECT_EQ( IR( 1, 5 ), A.I1() );
	EXPECT_EQ( IR( 1, 5 ), A.I2() );
	EXPECT_EQ( IR( 1, 5 ), A.I3() );
	EXPECT_EQ( 5u, A.size1() );
	EXPECT_EQ( 5u, A.size2() );
	EXPECT_EQ( 5u, A.size3() );
	EXPECT_EQ( 5u * 5u * 5u, A.size() );
	for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
		for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
			for ( int i3 = A.l3(); i3 <= A.u3(); ++i3 ) {
				if ( i1 <= 4 && i2 <= 4 && i3 <= 4 ) {
					EXPECT_EQ( 44, A( i1, i2, i3 ) );
				} else {
					EXPECT_EQ( 55, A( i1, i2, i3 ) );
				}
			}
		}
	}
}

TEST( ArrayTest, Swap3D )
{
	Array3D_int A( 4, 4, 4, 44 );
	Array3D_int( 5, 5, 5, 55 ).swap( A );
	EXPECT_EQ( IR( 1, 5 ), A.I1() );
	EXPECT_EQ( IR( 1, 5 ), A.I2() );
	EXPECT_EQ( IR( 1, 5 ), A.I3() );
	EXPECT_EQ( 5u, A.size1() );
	EXPECT_EQ( 5u, A.size2() );
	EXPECT_EQ( 5u, A.size3() );
	EXPECT_EQ( 5u * 5u * 5u, A.size() );
	for ( std::size_t i = 0; i < A.size(); ++i ) {
		EXPECT_EQ( 55, A[ i ] );
	}
}

TEST( ArrayTest, Cshift2DDim1 )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	Array2D_int B( 3, 3, reshape( { 31, 32, 33, 11, 12, 13, 21, 22, 23 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, cshift( A, 2, 1 ) ) );
}

TEST( ArrayTest, Cshift2DDim2 )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	Array2D_int B( 3, 3, reshape( { 12, 13, 11, 22, 23, 21, 32, 33, 31 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, cshift( A, -2, 2 ) ) );
}

TEST( ArrayTest, Cshift2DNonSquareDim2 )
{
	Array2D_int A( 3, 2, reshape( { 11, 12, 21, 22, 31, 32 }, std::array< int, 2 >{ { 3, 2 } } ) );
	Array2D_int B( 3, 2, reshape( { 12, 11, 22, 21, 32, 31 }, std::array< int, 2 >{ { 3, 2 } } ) );
	EXPECT_TRUE( eq( B, cshift( A, -1, 2 ) ) );
}

TEST( ArrayTest, Cshift2DDim1Array )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	Array2D_int B( 3, 3, reshape( { 31, 32, 33, 11, 12, 13, 21, 22, 23 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, cshift( A, Array1D_int( 3, 2 ), 1 ) ) );
	Array2D_int C( 3, 3, reshape( { 31, 12, 33, 11, 22, 13, 21, 32, 23 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( C, cshift( A, Array1D_int( 3, {2,0,2} ), 1 ) ) ); // Non-uniform shifts
}

TEST( ArrayTest, Cshift2DDim2Array )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	Array2D_int B( 3, 3, reshape( { 12, 13, 11, 22, 23, 21, 32, 33, 31 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, cshift( A, Array1D_int( 3, -2 ), 2 ) ) );
	Array2D_int C( 3, 3, reshape( { 13, 11, 12, 21, 22, 23, 33, 31, 32 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( C, cshift( A, Array1D_int( 3, {2,0,2} ), 2 ) ) ); // Non-uniform shifts
}

TEST( ArrayTest, Eoshift2DDim1 )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	Array2D_int B( 3, 3, reshape( { 31, 32, 33, 0, 0, 0, 0, 0, 0 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, eoshift( A, 2, 0, 1 ) ) );
	Array2D_int C( 3, 3, reshape( { 31, 32, 33, 99, 99, 99, 99, 99, 99 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( C, eoshift( A, 2, 99, 1 ) ) );
}

TEST( ArrayTest, Eoshift2DDim2 )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	Array2D_int B( 3, 3, reshape( { 0, 0, 11, 0, 0, 21, 0, 0, 31 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, eoshift( A, -2, 0, 2 ) ) );
	Array2D_int C( 3, 3, reshape( { 99, 99, 11, 99, 99, 21, 99, 99, 31 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( C, eoshift( A, -2, 99, 2 ) ) );
}

TEST( ArrayTest, Eoshift2DDim1Array )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	Array2D_int B( 3, 3, reshape( { 31, 32, 33, 99, 99, 99, 99, 99, 99 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, eoshift( A, Array1D_int( 3, 2 ), 99, 1 ) ) );
	Array2D_int C( 3, 3, reshape( { 31, 12, 33, 99, 22, 99, 99, 32, 99 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( C, eoshift( A, Array1D_int( 3, {2,0,2} ), 99, 1 ) ) ); // Non-uniform shifts
}

TEST( ArrayTest, Eoshift2DDim2Array )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	Array2D_int B( 3, 3, reshape( { 88, 88, 11, 88, 88, 21, 88, 88, 31 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( B, eoshift( A, Array1D_int( 3, -2 ), 88, 2 ) ) );
	Array2D_int C( 3, 3, reshape( { 13, 88, 88, 21, 22, 23, 33, 88, 88 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( C, eoshift( A, Array1D_int( 3, {2,0,2} ), 88, 2 ) ) ); // Non-uniform shifts
}

TEST( ArrayTest, Sum2DDim1 )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( Array1D_int( 3, { 63, 66, 69 } ), sum( A, 1 ) ) );
}

TEST( ArrayTest, Sum2DDim2 )
{
	Array2D_int A( 3, 3, reshape( { 11, 12, 13, 21, 22, 23, 31, 32, 33 }, std::array< int, 2 >{ { 3, 3 } } ) );
	EXPECT_TRUE( eq( Array1D_int( 3, { 36, 66, 96 } ), sum( A, 2 ) ) );
}

TEST( ArrayTest, Pow2D )
{
	Array2D_int A( 3, 3, 12 );
	Array2D_int B( pow( A, 2 ) );
	Array2D_int S( 3, 3, 144 );
	EXPECT_TRUE( eq( S, B ) );
}

TEST( ArrayTest, Matmul11 )
{
	Array1D_int A( 2 );
	Array1D_int B( 2 );
	A( 1 ) = 4;
	A( 2 ) = 3;
	B( 1 ) = 7;
	B( 2 ) = 5;
	Array2D_int R( 2, 2 );
	R( 1, 1 ) = 28;
	R( 1, 2 ) = 20;
	R( 2, 1 ) = 21;
	R( 2, 2 ) = 15;
	EXPECT_TRUE( eq( R, matmul( A, B ) ) );
}

TEST( ArrayTest, Matmul12 )
{
	Array1D_int A( 3 );
	Array2D_int B( 3, 2 );
	A( 1 ) = 4;
	A( 2 ) = 3;
	A( 3 ) = 5;
	B( 1, 1 ) = 7;
	B( 1, 2 ) = 2;
	B( 2, 1 ) = 5;
	B( 2, 2 ) = 0;
	B( 3, 1 ) = 4;
	B( 3, 2 ) = 6;
	Array1D_int R( 2 );
	R( 1 ) = 63;
	R( 2 ) = 38;
	EXPECT_TRUE( eq( R, matmul( A, B ) ) );
}

TEST( ArrayTest, Matmul21 )
{
	Array2D_int A( 2, 3 );
	Array1D_int B( 3 );
	A( 1, 1 ) = 4;
	A( 1, 2 ) = 3;
	A( 1, 3 ) = 5;
	A( 2, 1 ) = 9;
	A( 2, 2 ) = 2;
	A( 2, 3 ) = 8;
	B( 1 ) = 7;
	B( 2 ) = 5;
	B( 3 ) = 4;
	Array1D_int R( 2 );
	R( 1 ) = 63;
	R( 2 ) = 105;
	EXPECT_TRUE( eq( R, matmul( A, B ) ) );
}

TEST( ArrayTest, Matmul22 )
{
	Array2D_int A( 2, 3 );
	Array2D_int B( 3, 2 );
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
	Array2D_int R( 2, 2 );
	R( 1, 1 ) = 63;
	R( 1, 2 ) = 38;
	R( 2, 1 ) = 105;
	R( 2, 2 ) = 66;
	EXPECT_TRUE( eq( R, matmul( A, B ) ) );
	Array2D_int BT( transposed( B ) );
	EXPECT_TRUE( eq( R, matmul_T( A, BT ) ) );
}

TEST( ArrayTest, Matrix2DMultiplication )
{
	IR I1( 3 ), I2( 3 );

	Array2D_int A( I1, I2 );
	for ( int i = A.l1(), ie = A.u1(); i <= ie; ++i ) {
		for ( int j = A.l2(), je = A.u2(); j <= je; ++j ) {
			A( i, j ) = i + j * 2;
		}
	}

	Array2D_int B( I1, I2 );
	for ( int i = B.l1(), ie = B.u1(); i <= ie; ++i ) {
		for ( int j = B.l2(), je = B.u2(); j <= je; ++j ) {
			B( i, j ) = i * 2 + j * 3;
		}
	}

	Array2D_int C( I1, I2 );
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

TEST( ArrayTest, Matrix2DMultiplicationTranspose )
{
	IR I1( 3 ), I2( 3 );

	Array2D_int A( I1, I2 );
	for ( int i = A.l1(), ie = A.u1(); i <= ie; ++i ) {
		for ( int j = A.l2(), je = A.u2(); j <= je; ++j ) {
			A( i, j ) = i + j * 2;
		}
	}

	Array2D_int B( I1, I2 );
	for ( int i = B.l1(), ie = B.u1(); i <= ie; ++i ) {
		for ( int j = B.l2(), je = B.u2(); j <= je; ++j ) {
			B( i, j ) = i * 2 + j * 3;
		}
	}

	Array2D_int C( I1, I2 );
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

TEST( ArrayTest, Generation2DValueMinusArray )
{
	Array2D_int A( 3, 3, 33 ), B( 44 - A );
	EXPECT_TRUE( eq( Array2D_int( 3, 3, 11 ), B ) );
}

TEST( ArrayTest, Cross1D )
{
	Array1D_int A( 3, 33 ), B( 44 - A );
	EXPECT_TRUE( eq( cross( A, B ), cross_product( A, B ) ) );
}

TEST( ArrayTest, LogicalNegation )
{
	Array2D_bool const F( 2, 2, false );
	EXPECT_FALSE( F( 1, 1 ) );
	EXPECT_FALSE( F( 1, 2 ) );
	EXPECT_FALSE( F( 2, 1 ) );
	EXPECT_FALSE( F( 2, 2 ) );
	Array2D_bool const T( ! F );
	EXPECT_TRUE( T( 1, 1 ) );
	EXPECT_TRUE( T( 1, 2 ) );
	EXPECT_TRUE( T( 2, 1 ) );
	EXPECT_TRUE( T( 2, 2 ) );
}

TEST( ArrayTest, UboundOfUnbounded )
{
	Array2D_int r( {-1,1}, {-1,1}, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	Array2A_int u( r( -1, -1 ) ); // Unbounded tail proxy
	u.dim( _, 3 );
	EXPECT_EQ( 1, lbound( u, 1 ) );
	EXPECT_EQ( 1, lbound( u, 2 ) );
	EXPECT_EQ( 3, ubound( u, 2 ) );
	EXPECT_DEBUG_DEATH( ubound( u, 1 ), ".*Assertion.*" ); // Can't take ubound of unbounded dimension
}

TEST( ArrayTest, EmptyComparisonPredicate )
{
	Array1D_int a( 0 ), b( 0 ); // Empty
	EXPECT_EQ( 0u, a.size() );
	EXPECT_EQ( 0u, b.size() );
	EXPECT_EQ( 1, lbound( a, 1 ) );
	EXPECT_EQ( 0, ubound( a, 1 ) );
	EXPECT_TRUE( eq( a, b ) );
	EXPECT_FALSE( ne( a, b ) );
}

TEST( ArrayTest, EmptyComparisonElemental )
{
	Array1D_int a( 0 ), b( 0 ); // Empty
	EXPECT_EQ( 0u, a.size() );
	EXPECT_EQ( 0u, b.size() );
	EXPECT_EQ( 1, lbound( a, 1 ) );
	EXPECT_EQ( 0, ubound( a, 1 ) );
	EXPECT_EQ( 0u, ( a == b ).size() );
	EXPECT_EQ( 0u, ( a > b ).size() );
}

TEST( ArrayTest, Unallocated )
{
	Array1D_int a; // Empty
	EXPECT_FALSE( a.allocated() );
	EXPECT_FALSE( allocated( a ) );
	EXPECT_DEBUG_DEATH( a( 1 ), ".*Assertion.*" );
}

TEST( ArrayTest, AnyOp2D )
{
	Array2D_int const A( 3, 3, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	EXPECT_TRUE( any_eq( A, 6 ) );
	EXPECT_FALSE( any_eq( A, 22 ) );
	EXPECT_TRUE( any_ne( A, 6 ) );
	EXPECT_TRUE( any_lt( A, 2 ) );
	EXPECT_TRUE( any_ge( A, 9 ) );
	EXPECT_FALSE( any_lt( A, 1 ) );
	EXPECT_FALSE( any_gt( A, 9 ) );
}

TEST( ArrayTest, AllOp2D )
{
	Array2D_int const A( 3, 3, { 1, 2, 3, 4, 5, 6, 7, 8, 9 } );
	EXPECT_FALSE( all_eq( A, 6 ) );
	EXPECT_FALSE( all_eq( A, 22 ) );
	EXPECT_TRUE( all_ne( A, 22 ) );
	EXPECT_FALSE( all_ne( A, 2 ) );
	EXPECT_FALSE( all_lt( A, 2 ) );
	EXPECT_FALSE( all_ge( A, 9 ) );
	EXPECT_TRUE( all_lt( A, 11 ) );
	EXPECT_TRUE( all_gt( A, 0 ) );
}

TEST( ArrayTest, CountOp2D )
{
	Array2D_int const A( 3, 3, { 1, 2, 2, 3, 3, 3, 7, 8, 9 } );
	EXPECT_EQ( 0u, count_eq( A, 0 ) );
	EXPECT_EQ( 1u, count_eq( A, 1 ) );
	EXPECT_EQ( 2u, count_eq( A, 2 ) );
	EXPECT_EQ( 3u, count_eq( A, 3 ) );
	EXPECT_EQ( 6u, count_lt( A, 7 ) );
	EXPECT_EQ( 1u, count_ge( A, 9 ) );
	EXPECT_EQ( 9u, count_lt( A, 11 ) );
	EXPECT_EQ( 3u, count_gt( A, 3 ) );
}

TEST( ArrayTest, Functions1D )
{
	Array1D_int u{ 1, 2, 3 };
	Array1D_int v{ 2, 3, 4 };
	EXPECT_EQ( 14, magnitude_squared( u ) );
	EXPECT_EQ( 3, distance_squared( u, v ) );
	EXPECT_EQ( 20, dot( u, v ) );
}
