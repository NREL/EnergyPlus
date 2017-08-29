#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

class TestMatrixAddition : public testing::Test {

protected:
	void SetUp() override {
	}


};

TEST_F( TestMatrixAddition, Test1 ) {
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

	std::shared_ptr< CSquareMatrix > C = A.add( B );

	EXPECT_NEAR( 3, ( *C )[ 0 ][ 0 ], 1e-6 );
	EXPECT_NEAR( 5, ( *C )[ 0 ][ 1 ], 1e-6 );
	EXPECT_NEAR( 7, ( *C )[ 1 ][ 0 ], 1e-6 );
	EXPECT_NEAR( 9, ( *C )[ 1 ][ 1 ], 1e-6 );

}

TEST_F( TestMatrixAddition, Test2 ) {
	SCOPED_TRACE( "Begin Test: Test matrix subtraction operation." );

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

	std::shared_ptr< CSquareMatrix > C = A.sub( B );

	EXPECT_NEAR( -1, ( *C )[ 0 ][ 0 ], 1e-6 );
	EXPECT_NEAR( -1, ( *C )[ 0 ][ 1 ], 1e-6 );
	EXPECT_NEAR( -1, ( *C )[ 1 ][ 0 ], 1e-6 );
	EXPECT_NEAR( -1, ( *C )[ 1 ][ 1 ], 1e-6 );

}
