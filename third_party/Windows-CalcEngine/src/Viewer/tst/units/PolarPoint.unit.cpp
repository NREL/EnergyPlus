#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCEViewer.hpp"


using namespace Viewer;

class TestPolarPoint : public testing::Test {

protected:
	virtual void SetUp() {

	}

};

TEST_F( TestPolarPoint, PolarPointTest1 ) {
	SCOPED_TRACE( "Begin Test: Polar point conversions (1)." );

	CPolarPoint2D aPoint = CPolarPoint2D( 90, 1 );

	double x = aPoint.x();
	double y = aPoint.y();

	EXPECT_NEAR( 0, x, 1e-6 );
	EXPECT_NEAR( 1, y, 1e-6 );

	aPoint.setCartesian( 0, -1 );
	double theta = aPoint.theta();
	double radius = aPoint.radius();

	EXPECT_NEAR( 270, theta, 1e-6 );
	EXPECT_NEAR( 1, radius, 1e-6 );

	aPoint.setCartesian( 0, 0 );
	theta = aPoint.theta();
	radius = aPoint.radius();

	EXPECT_NEAR( 0, theta, 1e-6 );
	EXPECT_NEAR( 0, radius, 1e-6 );

	aPoint.setCartesian( 1, 1 );
	theta = aPoint.theta();
	radius = aPoint.radius();

	EXPECT_NEAR( 45, theta, 1e-6 );
	EXPECT_NEAR( 1.41421356, radius, 1e-6 );

	aPoint.setCartesian( 1, -1 );
	theta = aPoint.theta();
	radius = aPoint.radius();

	EXPECT_NEAR( -45, theta, 1e-6 );
	EXPECT_NEAR( 1.41421356, radius, 1e-6 );

}

TEST_F( TestPolarPoint, PolarPointTest2 ) {
	SCOPED_TRACE( "Begin Test: Polar point conversions (2)." );

	CPolarPoint2D aPoint = CPolarPoint2D( 259, 1.58 );

	double x = aPoint.x();
	double y = aPoint.y();

	EXPECT_NEAR( -0.301478213, x, 1e-6 );
	EXPECT_NEAR( -1.55097095, y, 1e-6 );

}

TEST_F( TestPolarPoint, PolarPointTest3 ) {
	SCOPED_TRACE( "Begin Test: Polar point conversions (3)." );

	CPolarPoint2D aPoint = CPolarPoint2D( 43, 0.76 );

	double x = aPoint.x();
	double y = aPoint.y();

	EXPECT_NEAR( 0.555828813, x, 1e-6 );
	EXPECT_NEAR( 0.518318754, y, 1e-6 );

}
