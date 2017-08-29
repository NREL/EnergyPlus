#include <memory>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"

using namespace std;
using namespace Tarcog;

class TestSolidLayer : public testing::Test {

private:
	std::shared_ptr< CIGUSolidLayer > m_SolidLayer;

protected:
	void SetUp() override {
		auto surface1 = std::make_shared< CSurface >();
		ASSERT_TRUE( surface1 != nullptr );
		surface1->setTemperature( 280 );
		auto surface2 = std::make_shared< CSurface >();
		ASSERT_TRUE( surface2 != nullptr );
		surface2->setTemperature( 300 );

		m_SolidLayer = make_shared< CIGUSolidLayer >( 0.01, 2.5, surface1, surface2 );
		ASSERT_TRUE( m_SolidLayer != nullptr );
	}

public:
	std::shared_ptr< CIGUSolidLayer > GetLayer() const {
		return m_SolidLayer;
	};

};

TEST_F( TestSolidLayer, Test1 ) {
	SCOPED_TRACE( "Begin Test: Test Solid Layer - Conduction heat flow" );

	auto aLayer = GetLayer();
	ASSERT_TRUE( aLayer != nullptr );

	auto conductionHeatFlow = aLayer->getConvectionConductionFlow();

	EXPECT_NEAR( 5000, conductionHeatFlow, 1e-6 );

}
