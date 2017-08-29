#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

class TestMatrixCopyFrom : public testing::Test {

protected:
	void SetUp() override {
	}


};

TEST_F( TestMatrixCopyFrom, Test1 ) {
	SCOPED_TRACE( "Begin Test: Test matrix addition operation." );

	CSquareMatrix A = CSquareMatrix( 2 );
	CSquareMatrix B = CSquareMatrix( 2 );

	A[ 0 ][ 0 ] = 1;
	A[ 0 ][ 1 ] = 2;
	A[ 1 ][ 0 ] = 3;
	A[ 1 ][ 1 ] = 4;

	B[ 0 ][ 0 ] = 2;
	B[ 0 ][ 1 ] = 3;
	B[ 1 ][ 0 ] = 4;
	B[ 1 ][ 1 ] = 5;

	A.copyFrom( B );

	EXPECT_NEAR( 2, A[0][0], 1e-6 );
	EXPECT_NEAR( 3, A[0][1], 1e-6 );
	EXPECT_NEAR( 4, A[1][0], 1e-6 );
	EXPECT_NEAR( 5, A[1][1], 1e-6 );

}
