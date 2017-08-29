#include <memory>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"

using namespace std;
using namespace Tarcog;

class TestGapLayerInBetweenVentilation : public testing::Test {

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

		std::shared_ptr< CEnvironment > Indoor =
			std::make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( Indoor != nullptr );

		// IGU
		auto solidLayerThickness = 0.005715; // [m]
		auto solidLayerConductance = 1.0;

		auto SolidLayer1 = make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( SolidLayer1 != nullptr );

		auto SolidLayer2 = make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( SolidLayer2 != nullptr );

		auto shadeLayerThickness = 0.01;
		auto shadeLayerConductance = 160.0;
		auto Atop = 0.1;
		auto Abot = 0.1;
		auto Aleft = 0.1;
		auto Aright = 0.1;
		auto Afront = 0.2;

		auto shadeLayer = make_shared< CIGUShadeLayer >( shadeLayerThickness, shadeLayerConductance,
		                                                 make_shared< CShadeOpenings >( Atop, Abot, Aleft, Aright, Afront ) );

		ASSERT_TRUE( shadeLayer != nullptr );

		auto gapThickness = 0.0127;
		auto gapPressure = 101325.0;
		std::shared_ptr< CBaseIGULayer > m_GapLayer1 =
			std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( m_GapLayer1 != nullptr );

		std::shared_ptr< CBaseIGULayer > m_GapLayer2 =
			std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( m_GapLayer2 != nullptr );

		double windowWidth = 1;
		double windowHeight = 1;
		std::shared_ptr< CIGU > aIGU = make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( aIGU != nullptr );
		aIGU->addLayer( SolidLayer1 );
		aIGU->addLayer( m_GapLayer1 );
		aIGU->addLayer( shadeLayer );
		aIGU->addLayer( m_GapLayer2 );
		aIGU->addLayer( SolidLayer2 );

		/////////////////////////////////////////////////////////
		// System
		/////////////////////////////////////////////////////////
		m_TarcogSystem = make_shared< CSingleSystem >( aIGU, Indoor, Outdoor );
		ASSERT_TRUE( m_TarcogSystem != nullptr );
	}

public:
	std::shared_ptr< CBaseIGULayer > GetGap1() const {
		return m_TarcogSystem->getGapLayers()[ 0 ];
	};

	std::shared_ptr< CBaseIGULayer > GetGap2() const {
		return m_TarcogSystem->getGapLayers()[ 1 ];
	};

};

TEST_F( TestGapLayerInBetweenVentilation, VentilationFlow ) {
	SCOPED_TRACE( "Begin Test: Test Ventilated Gap Layer - Intial Airflow" );

	auto aLayer = GetGap1();

	// Airflow iterations are set to 1e-4 and it cannot exceed that precision

	ASSERT_TRUE( aLayer != nullptr );
	auto gainEnergy = aLayer->getGainFlow();
	EXPECT_NEAR( 32.414571203538848, gainEnergy, 1e-4 );

	aLayer = GetGap2();
	ASSERT_TRUE( aLayer != nullptr );
	gainEnergy = aLayer->getGainFlow();
	EXPECT_NEAR( -32.414571203538848, gainEnergy, 1e-4 );

}
