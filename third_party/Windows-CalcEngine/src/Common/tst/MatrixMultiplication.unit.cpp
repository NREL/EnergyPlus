#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixMultiplication : public testing::Test {

protected:
	void SetUp() override {
	}

};

TEST_F( TestMatrixMultiplication, Test1 ) {
	SCOPED_TRACE( "Begin Test: Test matrix multiplication (3 x 3)." );

	const size_t n = 3;

	auto a = CSquareMatrix( n );

	a[ 0 ] = { 4, 3, 9 };
	a[ 1 ] = { 8, 8, 4 };
	a[ 2 ] = { 4, 3, 7 };

	auto b = CSquareMatrix( n );

	b[ 0 ] = { 6, 8, 5 };
	b[ 1 ] = { 3, 5, 6 };
	b[ 2 ] = { 1, 2, 3 };

	auto mult = a.mult( b );

	EXPECT_EQ( n, mult->getSize() );

	auto multCorrect = CSquareMatrix( n );

	multCorrect[ 0 ] = { 42, 65, 65 };
	multCorrect[ 1 ] = { 76, 112, 100 };
	multCorrect[ 2 ] = { 40, 61, 59 };

	for ( size_t i = 0; i < n; ++i ) {
		for ( size_t j = 0; j < n; ++j ) {
			EXPECT_NEAR( ( *mult )[ i ][ j ], multCorrect[ i ][ j ], 1e-6 );
		}
	}

}

TEST_F( TestMatrixMultiplication, Test2 ) {
	SCOPED_TRACE( "Begin Test: Test matrix and std::vector multiplication (3 x 3) and (1 x 3)." );

	const size_t n = 3;

	auto a = CSquareMatrix( n );

	a[ 0 ] = { 4, 3, 9 };
	a[ 1 ] = { 8, 8, 4 };
	a[ 2 ] = { 4, 3, 7 };

	const std::vector< double > b = { 8, 4, 6 };

	auto mult = a.multMxV( b );

	EXPECT_EQ( n, mult->size() );

	std::vector< double > multCorrect = { 98, 120, 86 };

	for ( size_t i = 0; i < n; ++i ) {
		EXPECT_NEAR( ( *mult )[ i ], multCorrect[ i ], 1e-6 );
	}

}

TEST_F( TestMatrixMultiplication, Test3 ) {
	SCOPED_TRACE( "Begin Test: Test matrix and std::vector multiplication (3 x 3) and (1 x 3)." );

	const size_t n = 3;

	auto a = CSquareMatrix( n );

	a[ 0 ] = { 4, 3, 9 };
	a[ 1 ] = { 8, 8, 4 };
	a[ 2 ] = { 4, 3, 7 };

	const std::vector< double > b = { 8, 4, 6 };

	auto mult = a.multVxM( b );

	EXPECT_EQ( n, mult->size() );

	std::vector< double > multCorrect = { 88, 74, 130 };

	for ( size_t i = 0; i < n; ++i ) {
		EXPECT_NEAR( ( *mult )[ i ], multCorrect[ i ], 1e-6 );
	}

}
