#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixAddition : public testing::Test {

protected:
	void SetUp() override {
	}


};

TEST_F( TestMatrixAddition, Test1 ) {
	SCOPED_TRACE( "Begin Test: Test matrix addition operation." );

	auto a = CSquareMatrix( 2 );
	auto b = CSquareMatrix( 2 );

	a[ 0 ][ 0 ] = 1;
	a[ 0 ][ 1 ] = 2;
	a[ 1 ][ 0 ] = 3;
	a[ 1 ][ 1 ] = 4;

	b[ 0 ][ 0 ] = 2;
	b[ 0 ][ 1 ] = 3;
	b[ 1 ][ 0 ] = 4;
	b[ 1 ][ 1 ] = 5;

	auto C = a.add( b );

	EXPECT_NEAR( 3, ( *C )[ 0 ][ 0 ], 1e-6 );
	EXPECT_NEAR( 5, ( *C )[ 0 ][ 1 ], 1e-6 );
	EXPECT_NEAR( 7, ( *C )[ 1 ][ 0 ], 1e-6 );
	EXPECT_NEAR( 9, ( *C )[ 1 ][ 1 ], 1e-6 );

}

TEST_F( TestMatrixAddition, Test2 ) {
	SCOPED_TRACE( "Begin Test: Test matrix subtraction operation." );

	auto a = CSquareMatrix( 2 );
	auto b = CSquareMatrix( 2 );

	a[ 0 ][ 0 ] = 1;
	a[ 0 ][ 1 ] = 2;
	a[ 1 ][ 0 ] = 3;
	a[ 1 ][ 1 ] = 4;

	b[ 0 ][ 0 ] = 2;
	b[ 0 ][ 1 ] = 3;
	b[ 1 ][ 0 ] = 4;
	b[ 1 ][ 1 ] = 5;

	auto C = a.sub( b );

	EXPECT_NEAR( -1, ( *C )[ 0 ][ 0 ], 1e-6 );
	EXPECT_NEAR( -1, ( *C )[ 0 ][ 1 ], 1e-6 );
	EXPECT_NEAR( -1, ( *C )[ 1 ][ 0 ], 1e-6 );
	EXPECT_NEAR( -1, ( *C )[ 1 ][ 1 ], 1e-6 );

}
