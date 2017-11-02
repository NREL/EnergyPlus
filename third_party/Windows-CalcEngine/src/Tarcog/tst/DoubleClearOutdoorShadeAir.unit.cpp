#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

using namespace Tarcog;
using namespace FenestrationCommon;

class TestDoubleClearOutdoorShadeAir : public testing::Test {

private:
	std::shared_ptr< CSingleSystem > m_TarcogSystem;

protected:
	void SetUp() override {
		/////////////////////////////////////////////////////////
		// Outdoor
		/////////////////////////////////////////////////////////
		auto airTemperature = 255.15; // Kelvins
		auto pressure = 101325.0; // Pascals
		auto airSpeed = 5.5; // meters per second
		auto airDirection = AirHorizontalDirection::Windward;
		auto tSky = 255.15; // Kelvins
		auto solarRadiation = 0.0;

		std::shared_ptr< CEnvironment > Outdoor =
			std::make_shared< COutdoorEnvironment >( airTemperature, pressure, airSpeed, solarRadiation,
			                                    airDirection, tSky, SkyModel::AllSpecified );
		ASSERT_TRUE( Outdoor != nullptr );
		Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH );

		/////////////////////////////////////////////////////////
		// Indoor
		/////////////////////////////////////////////////////////

		auto roomTemperature = 295.15;

		std::shared_ptr< CEnvironment > Indoor = std::make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( Indoor != nullptr );

		/////////////////////////////////////////////////////////
		// IGU
		/////////////////////////////////////////////////////////
		auto solidLayerThickness = 0.005715; // [m]
		auto solidLayerConductance = 1.0;

		auto shadeLayerThickness = 0.01;
		auto shadeLayerConductance = 160.0;
		auto dtop = 0.1;
		auto dbot = 0.1;
		auto dleft = 0.1;
		auto dright = 0.1;
		auto Afront = 0.2;

		std::shared_ptr< CIGUSolidLayer > aLayer1 =
			std::make_shared< CIGUShadeLayer >( shadeLayerThickness, shadeLayerConductance,
			                               std::make_shared< CShadeOpenings >( dtop, dbot, dleft, dright, Afront ) );

		ASSERT_TRUE( aLayer1 != nullptr );

		auto aLayer2 = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( aLayer2 != nullptr );

		auto aLayer3 = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );

		auto gapThickness = 0.0127;
		auto gapPressure = 101325.0;
		auto GapLayer1 = std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( GapLayer1 != nullptr );

		auto GapLayer2 = std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( GapLayer2 != nullptr );

		auto windowWidth = 1.0;
		auto windowHeight = 1.0;
		auto aIGU = std::make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( aIGU != nullptr );
		aIGU->addLayer( aLayer1 );
		aIGU->addLayer( GapLayer1 );
		aIGU->addLayer( aLayer2 );
		aIGU->addLayer( GapLayer2 );
		aIGU->addLayer( aLayer3 );

		/////////////////////////////////////////////////////////
		// System
		/////////////////////////////////////////////////////////
		m_TarcogSystem = std::make_shared< CSingleSystem >( aIGU, Indoor, Outdoor );
		ASSERT_TRUE( m_TarcogSystem != nullptr );

		m_TarcogSystem->solve();
	}

public:
	std::shared_ptr< CSingleSystem > getSystem() const {
		return m_TarcogSystem;
	};

};

TEST_F( TestDoubleClearOutdoorShadeAir, Test1 ) {
	SCOPED_TRACE( "Begin Test: Outdoor Shade - Air" );

	auto aSystem = getSystem();

	auto temperature = *aSystem->getTemperatures();
	auto radiosity = *aSystem->getRadiosities();

	std::vector< double > correctTemp = { 256.984174, 256.987521, 269.436767, 269.879607, 284.039136, 284.481976 };
	std::vector< double > correctJ = { 246.160566, 254.396303, 291.699410, 310.189965, 359.623545, 380.772533 };

	EXPECT_EQ( correctTemp.size(), temperature.size() );
	EXPECT_EQ( correctJ.size(), radiosity.size() );

	for ( size_t i = 0; i < temperature.size(); ++i ) {
		EXPECT_NEAR( correctTemp[ i ], temperature[ i ], 1e-6 );
		EXPECT_NEAR( correctJ[ i ], radiosity[ i ], 1e-6 );
	}

	auto numOfIter = aSystem->getNumberOfIterations();
	EXPECT_EQ( 23, int( numOfIter ) );

	auto ventilatedFlow = aSystem->getVentilationFlow( Environment::Outdoor );
	EXPECT_NEAR( -23.931861, ventilatedFlow, 1e-6 );
}
