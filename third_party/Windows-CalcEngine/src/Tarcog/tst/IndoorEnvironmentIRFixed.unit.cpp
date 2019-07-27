#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"

using namespace Tarcog;


class TestIndoorEnvironmentIRFixed : public testing::Test {

private:
	std::shared_ptr< CEnvironment > m_Indoor;
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
		auto solarRadiation = 0.0;

		std::shared_ptr< CEnvironment > Outdoor =
			std::make_shared< COutdoorEnvironment >( airTemperature, pressure, airSpeed, solarRadiation,
			                                    airDirection, tSky, SkyModel::AllSpecified );
		ASSERT_TRUE( Outdoor != nullptr );
		Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH );

		/////////////////////////////////////////////////////////
		// Indoor
		/////////////////////////////////////////////////////////

		auto roomTemperature = 294.15;
		// I just have picked up solution from WINDOW 7.4 run. There is no other way of
		// getting the results since current version of WINDOW does not support IR input.
		auto IRRadiation = 424.458750;

		m_Indoor = std::make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( m_Indoor != nullptr );
		m_Indoor->setEnvironmentIR( IRRadiation );

		/////////////////////////////////////////////////////////
		// IGU
		/////////////////////////////////////////////////////////
		auto solidLayerThickness = 0.003048; // [m]
		auto solidLayerConductance = 100.0;

		auto aSolidLayer = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( aSolidLayer != nullptr );

		auto windowWidth = 1.0;
		auto windowHeight = 1.0;
		auto aIGU = std::make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( aIGU != nullptr );
		aIGU->addLayer( aSolidLayer );

		/////////////////////////////////////////////////////////
		// System
		/////////////////////////////////////////////////////////
		m_TarcogSystem = std::make_shared< CSingleSystem >( aIGU, m_Indoor, Outdoor );
		m_TarcogSystem->solve();
		ASSERT_TRUE( m_TarcogSystem != nullptr );
	}

public:
	std::shared_ptr< CEnvironment > GetIndoors() const {
		return m_Indoor;
	};

};

TEST_F( TestIndoorEnvironmentIRFixed, IndoorRadiosity ) {
	SCOPED_TRACE( "Begin Test: Indoors -> Fixed radiosity (user input)." );

	auto aIndoor = GetIndoors();
	ASSERT_TRUE( aIndoor != nullptr );

	auto radiosity = aIndoor->getEnvironmentIR();
	EXPECT_NEAR( 424.458750, radiosity, 1e-6 );
}

TEST_F( TestIndoorEnvironmentIRFixed, IndoorConvection ) {
	SCOPED_TRACE( "Begin Test: Indoors -> Convection Flow (user input)." );

	auto aIndoor = GetIndoors();
	ASSERT_TRUE( aIndoor != nullptr );

	auto convectionFlow = aIndoor->getConvectionConductionFlow();
	EXPECT_NEAR( -5.826845, convectionFlow, 1e-6 );
}

TEST_F( TestIndoorEnvironmentIRFixed, IndoorHc ) {
	SCOPED_TRACE( "Begin Test: Indoors -> Convection Coefficient (user input)." );

	auto aIndoor = GetIndoors();
	ASSERT_TRUE( aIndoor != nullptr );

	auto hc = aIndoor->getHc();
	EXPECT_NEAR( 1.913874, hc, 1e-6 );
}
