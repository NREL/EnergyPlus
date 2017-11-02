#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"

using namespace Tarcog;


class TestOutdoorEnvironmentIRFixed : public testing::Test {

private:
	std::shared_ptr< CEnvironment > Outdoor;
	std::shared_ptr< CSingleSystem > m_TarcogSystem;

protected:
	void SetUp() override {
		/////////////////////////////////////////////////////////
		// Outdoor
		/////////////////////////////////////////////////////////
		auto airTemperature = 300.0; // Kelvins
		auto tSky = airTemperature;
		auto pressure = 101325.0; // Pascals
		auto airSpeed = 5.5; // meters per second
		auto airDirection = AirHorizontalDirection::Windward;
		auto solarRadiation = 0.0;
		auto IRRadiation = 370.0; // [ W/m2 ]

		Outdoor = std::make_shared< COutdoorEnvironment >( airTemperature, pressure, airSpeed, solarRadiation,
		                                              airDirection, tSky, SkyModel::AllSpecified );
		ASSERT_TRUE( Outdoor != nullptr );
		Outdoor->setEnvironmentIR( IRRadiation );

		/////////////////////////////////////////////////////////
		// Indoor
		/////////////////////////////////////////////////////////

		auto roomTemperature = 294.15;

		std::shared_ptr< CEnvironment > Indoor = std::make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( Indoor != nullptr );

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
		m_TarcogSystem = std::make_shared< CSingleSystem >( aIGU, Indoor, Outdoor );
		ASSERT_TRUE( m_TarcogSystem != nullptr );
	}

public:
	std::shared_ptr< CEnvironment > GetOutdoors() const {
		return Outdoor;
	};

};

TEST_F( TestOutdoorEnvironmentIRFixed, CalculateIRFixed ) {

	SCOPED_TRACE( "Begin Test: Outdoors -> Infrared radiation fixed (user input)." );

	auto aOutdoor = GetOutdoors();
	ASSERT_TRUE( aOutdoor != nullptr );

	auto radiosity = aOutdoor->getEnvironmentIR();
	EXPECT_NEAR( 370, radiosity, 1e-6 );

	auto hc = aOutdoor->getHc();
	EXPECT_NEAR( 26, hc, 1e-6 );

}
