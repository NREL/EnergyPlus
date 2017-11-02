#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixInverse : public testing::Test {

protected:
	void SetUp() override {
	}

};

TEST_F( TestMatrixInverse, Test1 ) {
	SCOPED_TRACE( "Begin Test: Test inverse matrix (3 x 3)." );

	const size_t n = 3;

	auto a = CSquareMatrix( n );

	a[ 0 ] = { 3.12, 8.56, 4.19 };
	a[ 1 ] = { 6.87, 4.39, 7.11 };
	a[ 2 ] = { 6.59, 4.98, 7.69 };

	auto inverse = a.inverse();

	EXPECT_EQ( n, inverse->getSize() );

	auto inverseCorrect = CSquareMatrix( n );

	inverseCorrect[ 0 ] = { 0.048264485, 1.316176934, -1.243204967 };
	inverseCorrect[ 1 ] = { 0.17492546, 0.105952357, -0.193271643 };
	inverseCorrect[ 2 ] = { -0.15464132, -1.196521292, 1.320573929 };

	for ( size_t i = 0; i < n; ++i ) {
		for ( size_t j = 0; j < n; ++j ) {
			EXPECT_NEAR( ( *inverse )[ i ][ j ], inverseCorrect[ i ][ j ], 1e-6 );
		}
	}

}

TEST_F( TestMatrixInverse, Test2 ) {
	SCOPED_TRACE( "Begin Test: Test inverse matrix (4 x 4)." );

	const size_t n = 4;

	auto A = CSquareMatrix( n );

	A[ 0 ] = { 2.59, 1.48, 9.54, 4.16 };
	A[ 1 ] = { 9.45, 7.25, 6.58, 4.95 };
	A[ 2 ] = { 2.12, 5.36, 4.98, 8.23 };
	A[ 3 ] = { 4.89, 1.11, 7.45, 3.26 };

	auto inverse = A.inverse();

	EXPECT_EQ( n, inverse->getSize() );

	auto inverseCorrect = CSquareMatrix( n );

	inverseCorrect[ 0 ] = { -0.266190489, 0.003957093, -0.001994289, 0.338704853 };
	inverseCorrect[ 1 ] = { 0.313584868, 0.208591568, -0.07062952, -0.538576798 };
	inverseCorrect[ 2 ] = { 0.254839323, 0.035657535, -0.083907777, -0.167507784 };
	inverseCorrect[ 3 ] = { -0.289865235, -0.15844646, 0.21879257, 0.364873161 };

	for ( size_t i = 0; i < n; ++i ) {
		for ( size_t j = 0; j < n; ++j ) {
			EXPECT_NEAR( ( *inverse )[ i ][ j ], inverseCorrect[ i ][ j ], 1e-6 );
		}
	}

}
