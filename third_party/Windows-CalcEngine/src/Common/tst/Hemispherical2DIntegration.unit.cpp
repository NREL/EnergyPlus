#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

class TestHemispherical2DIntegration : public testing::Test {

public:
	std::shared_ptr< CHemispherical2DIntegrator > m_Integrator;

protected:
	void SetUp() override {
		auto aSeries = make_shared< CSeries >();

		// example taken from WINDOW 7 double layer (NFRC 102 and NFRC 103) angular dependency for Tsol
		// NOTE: It is not necessary to add angles in accending order. Series will sort out order before
		// performing integration
		aSeries->addProperty( 0, 0.652 );
		aSeries->addProperty( 10, 0.651 );
		aSeries->addProperty( 20, 0.648 );
		aSeries->addProperty( 30, 0.640 );
		aSeries->addProperty( 40, 0.624 );
		aSeries->addProperty( 50, 0.592 );
		aSeries->addProperty( 60, 0.527 );
		aSeries->addProperty( 70, 0.397 );
		aSeries->addProperty( 80, 0.185 );
		aSeries->addProperty( 90, 0.000 );

		m_Integrator = make_shared< CHemispherical2DIntegrator >( *aSeries, IntegrationType::Trapezoidal );

	}

};

TEST_F( TestHemispherical2DIntegration, TestHemisphericalIntegration ) {
	SCOPED_TRACE( "Begin Test: Test for 2D hemispherical integrator." );

	auto aValue = m_Integrator->value();

	EXPECT_NEAR( 0.552540, aValue, 1e-6 );

}
