#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace Tarcog;
using namespace FenestrationCommon;

class TestDoubleClearSingleSystemWithSun : public testing::Test {

private:
	std::shared_ptr< CSingleSystem > m_TarcogSystem;

protected:
	void SetUp() override {
		/////////////////////////////////////////////////////////
		// Outdoor
		/////////////////////////////////////////////////////////
		auto airTemperature = 305.15; // Kelvins
		auto pressure = 101325.0; // Pascals
		auto airSpeed = 2.75; // meters per second
		auto airDirection = AirHorizontalDirection::Windward;
		auto tSky = 305.15; // Kelvins
		auto solarRadiation = 783.0;

		std::shared_ptr< CEnvironment > Outdoor =
			std::make_shared< COutdoorEnvironment >( airTemperature, pressure, airSpeed, solarRadiation,
			                                    airDirection, tSky, SkyModel::AllSpecified );
		ASSERT_TRUE( Outdoor != nullptr );
		Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH );

		/////////////////////////////////////////////////////////
		// Indoor
		/////////////////////////////////////////////////////////

		auto roomTemperature = 297.15;

		std::shared_ptr< CEnvironment > Indoor =
			std::make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( Indoor != nullptr );

		/////////////////////////////////////////////////////////
		// IGU
		/////////////////////////////////////////////////////////
		auto solidLayerThickness = 0.005715; // [m]
		auto solidLayerConductance = 1.0;
		auto solarAbsorptance = 0.187443971634;

		auto aSolidLayer1 = make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		aSolidLayer1->setSolarAbsorptance( solarAbsorptance );

		solarAbsorptance = 0.054178960621;
		auto aSolidLayer2 = make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		aSolidLayer2->setSolarAbsorptance( solarAbsorptance );

		auto gapThickness = 0.012;
		auto gapPressure = 101325.0;
		std::shared_ptr< CBaseIGULayer > m_GapLayer = make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( m_GapLayer != nullptr );

		double windowWidth = 1;
		double windowHeight = 1;
		auto aIGU = make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( aIGU != nullptr );
		aIGU->addLayer( aSolidLayer1 );
		aIGU->addLayer( m_GapLayer );
		aIGU->addLayer( aSolidLayer2 );

		/////////////////////////////////////////////////////////
		// System
		/////////////////////////////////////////////////////////
		m_TarcogSystem = make_shared< CSingleSystem >( aIGU, Indoor, Outdoor );
		ASSERT_TRUE( m_TarcogSystem != nullptr );

		m_TarcogSystem->solve();
	}

public:
	std::shared_ptr< CSingleSystem > GetSystem() const {
		return m_TarcogSystem;
	};

};

TEST_F( TestDoubleClearSingleSystemWithSun, Test1 ) {
	SCOPED_TRACE( "Begin Test: Double Clear Single System - Surface temperatures" );

	auto aSystem = GetSystem();
	ASSERT_TRUE( aSystem != nullptr );

	auto Temperature = *aSystem->getTemperatures();
	vector< double > correctTemperature = { 310.818074, 311.064868, 306.799522, 306.505704 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	auto Radiosity = *aSystem->getRadiosities();
	vector< double > correctRadiosity = { 523.148794, 526.906252, 506.252171, 491.059753 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

	auto heatFlow = aSystem->getHeatFlow( Environment::Indoor );
	EXPECT_NEAR( -72.622787, heatFlow, 1e-5 );

	auto Uvalue = aSystem->getUValue();
	EXPECT_NEAR( 9.077848, Uvalue, 1e-5 );

	auto numOfIter = aSystem->getNumberOfIterations();
	EXPECT_EQ( 20u, numOfIter );
}
