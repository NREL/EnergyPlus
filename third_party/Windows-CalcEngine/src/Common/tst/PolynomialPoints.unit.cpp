#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class PolynomialPointsTest : public testing::Test {

protected:
	void SetUp() override {
		const auto poly1 = Polynom( { -6.75, 8.65, -0.75 } );
		const auto poly2 = Polynom( { 1.5, -2.5, 0.3 } );
		const auto poly3 = Polynom( { 2.4, 20, 1.3, -0.24 } );

		m_Points.storePoint( 10, poly1 );
		m_Points.storePoint( 20, poly2 );
		m_Points.storePoint( 30, poly3 );

	}

	PolynomialPoints360deg getPoints() const {
		return m_Points;
	};

private:
	PolynomialPoints360deg m_Points;

};

TEST_F( PolynomialPointsTest, TestClosestPointInRange ) {
	SCOPED_TRACE( "Begin Test: Polynomial points." );

	auto val = getPoints().valueAt( 15, 12 );

	EXPECT_NEAR( 1.875, val, 1e-6 );

}

TEST_F( PolynomialPointsTest, TestClosestPointOnLowerRange ) {
	SCOPED_TRACE( "Begin Test: Polynomial points." );

	auto val = getPoints().valueAt( 5, 12 );

	EXPECT_NEAR( -10.570147, val, 1e-6 );

}

TEST_F( PolynomialPointsTest, TestClosestPointHigerRange ) {
	SCOPED_TRACE( "Begin Test: Polynomial points." );

	auto val = getPoints().valueAt( 150, 12 );

	EXPECT_NEAR( 5.763529, val, 1e-6 );

}
