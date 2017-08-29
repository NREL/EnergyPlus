#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace Tarcog;
using namespace FenestrationCommon;

class TestSingleClearSolarCond001 : public testing::Test {

private:
	std::shared_ptr< CSingleSystem > m_TarcogSystem;

protected:
	void SetUp() override {
		/////////////////////////////////////////////////////////
		// Outdoor
		/////////////////////////////////////////////////////////
		auto airTemperature = 300.0; // Kelvins
		auto pressure = 101325.0; // Pascals
		auto airSpeed = 5.5; // meters per second
		auto airDirection = AirHorizontalDirection::Windward;
		auto tSky = 270.0; // Kelvins
		auto solarRadiation = 1000.0;

		std::shared_ptr< CEnvironment > Outdoor =
			std::make_shared< COutdoorEnvironment >( airTemperature, pressure, airSpeed, solarRadiation,
			                                    airDirection, tSky, SkyModel::AllSpecified );
		ASSERT_TRUE( Outdoor != nullptr );
		Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH );

		/////////////////////////////////////////////////////////
		// Indoor
		/////////////////////////////////////////////////////////

		auto roomTemperature = 294.15;

		std::shared_ptr< CEnvironment > aIndoor =
			std::make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( aIndoor != nullptr );

		/////////////////////////////////////////////////////////
		// IGU
		/////////////////////////////////////////////////////////
		auto solidLayerThickness = 0.003048; // [m]
		auto solidLayerConductance = 100.0;
		auto solarAbsorptance = 0.094189159572;

		auto aSolidLayer = make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		aSolidLayer->setSolarAbsorptance( solarAbsorptance );
		ASSERT_TRUE( aSolidLayer != nullptr );

		double windowWidth = 1;
		double windowHeight = 1;
		auto aIGU = make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( aIGU != nullptr );
		aIGU->addLayer( aSolidLayer );

		/////////////////////////////////////////////////////////
		// System
		/////////////////////////////////////////////////////////
		m_TarcogSystem = make_shared< CSingleSystem >( aIGU, aIndoor, Outdoor );
		ASSERT_TRUE( m_TarcogSystem != nullptr );

		m_TarcogSystem->solve();
	}

public:
	std::shared_ptr< CSingleSystem > GetSystem() const {
		return m_TarcogSystem;
	};

};

TEST_F( TestSingleClearSolarCond001, TestTempAndRad ) {
	SCOPED_TRACE( "Begin Test: Single Clear (Solar Radiation) - Temperatures and Radiosity." );

	auto aSystem = GetSystem();
	ASSERT_TRUE( aSystem != nullptr );

	auto Temperature = *aSystem->getTemperatures();
	vector< double > correctTemperature = { 299.627742, 299.627975 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	auto Radiosity = *aSystem->getRadiosities();
	vector< double > correctRadiosity = { 444.699763, 451.769813 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

}

TEST_F( TestSingleClearSolarCond001, TestIndoor ) {
	SCOPED_TRACE( "Begin Test: Single Clear (Solar Radiation) - Indoor heat flow." );

	auto aSystem = GetSystem();

	auto convectiveHF = aSystem->getConvectiveHeatFlow( Environment::Indoor );
	auto radiativeHF = aSystem->getRadiationHeatFlow( Environment::Indoor );
	auto totalHF = aSystem->getHeatFlow( Environment::Indoor );

	EXPECT_NEAR( -12.135453, convectiveHF, 1e-5 );
	EXPECT_NEAR( -27.311063, radiativeHF, 1e-5 );
	EXPECT_NEAR( -39.446516, totalHF, 1e-5 );
}
