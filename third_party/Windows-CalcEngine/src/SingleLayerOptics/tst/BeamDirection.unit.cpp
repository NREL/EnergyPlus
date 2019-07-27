#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCESingleLayerOptics.hpp"

using namespace SingleLayerOptics;

class TestBeamDirection : public testing::Test {


protected:
	virtual void SetUp() {

	}

};

TEST_F( TestBeamDirection, TestBeamDirectionProfileAngle1 ) {
	SCOPED_TRACE( "Begin Test: Beam direction profile angles." );

	CBeamDirection aDirection = CBeamDirection( 0, 0 );

	double profileAngle = aDirection.profileAngle();
	EXPECT_NEAR( 0, profileAngle, 1e-6 );

}

TEST_F( TestBeamDirection, TestBeamDirectionProfileAngle2 ) {
	SCOPED_TRACE( "Begin Test: Beam direction profile angles." );

	CBeamDirection aDirection = CBeamDirection( 18, 90 );

	double profileAngle = aDirection.profileAngle();
	EXPECT_NEAR( -18, profileAngle, 1e-6 );

}

TEST_F( TestBeamDirection, TestBeamDirectionProfileAngle3 ) {
	SCOPED_TRACE( "Begin Test: Beam direction profile angles." );

	CBeamDirection aDirection = CBeamDirection( 18, 270 );

	double profileAngle = aDirection.profileAngle();
	EXPECT_NEAR( 18, profileAngle, 1e-6 );

}

TEST_F( TestBeamDirection, TestBeamDirectionAssignment ) {
	SCOPED_TRACE( "Begin Test: Copying beam direction." );

	CBeamDirection aDirection = CBeamDirection( 18, 90 );
	CBeamDirection aCopyDirection = CBeamDirection( 0, 0 );

	aCopyDirection = aDirection;

	EXPECT_NEAR( 18, aCopyDirection.theta(), 1e-6 );
	EXPECT_NEAR( 90, aCopyDirection.phi(), 1e-6 );

}
