#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixCopyFrom : public testing::Test {

protected:
	void SetUp() override {
	}


};

TEST_F( TestMatrixCopyFrom, Test1 ) {
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

	a.copyFrom( b );

	EXPECT_NEAR( 2, a[0][0], 1e-6 );
	EXPECT_NEAR( 3, a[0][1], 1e-6 );
	EXPECT_NEAR( 4, a[1][0], 1e-6 );
	EXPECT_NEAR( 5, a[1][1], 1e-6 );

}
