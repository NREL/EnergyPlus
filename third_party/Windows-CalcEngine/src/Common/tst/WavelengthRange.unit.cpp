#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

class WavelengthRangeTest : public testing::Test {

protected:
	void SetUp() override {
	}


};

TEST_F( WavelengthRangeTest, Test1 ) {
	SCOPED_TRACE( "Begin Test: Creation of IR range." );

	CWavelengthRange aRange = CWavelengthRange( WavelengthRange::IR );

	EXPECT_NEAR( 5.0, aRange.minLambda(), 1e-6 );
	EXPECT_NEAR( 100.0, aRange.maxLambda(), 1e-6 );

}

TEST_F( WavelengthRangeTest, Test2 ) {
	SCOPED_TRACE( "Begin Test: Creation of Solar range." );

	CWavelengthRange aRange = CWavelengthRange( WavelengthRange::Solar );

	EXPECT_NEAR( 0.3, aRange.minLambda(), 1e-6 );
	EXPECT_NEAR( 2.5, aRange.maxLambda(), 1e-6 );

}

TEST_F( WavelengthRangeTest, Test3 ) {
	SCOPED_TRACE( "Begin Test: Creation of Visible range." );

	CWavelengthRange aRange = CWavelengthRange( WavelengthRange::Visible );

	EXPECT_NEAR( 0.38, aRange.minLambda(), 1e-6 );
	EXPECT_NEAR( 0.78, aRange.maxLambda(), 1e-6 );

}
