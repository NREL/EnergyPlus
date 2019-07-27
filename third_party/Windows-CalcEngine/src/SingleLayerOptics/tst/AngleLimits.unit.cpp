#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCESingleLayerOptics.hpp"

using namespace SingleLayerOptics;

class TestAngleLimits : public testing::Test {


protected:
	virtual void SetUp() {

	}

};

TEST_F( TestAngleLimits, TestAngleLimits1 ) {
	SCOPED_TRACE( "Begin Test: Angle limits 1." );

	CAngleLimits aLimits = CAngleLimits( -15, 15 );

	const double angle = 350;

	bool isInLimits = aLimits.isInLimits( angle );
	EXPECT_EQ( isInLimits, true );

}
