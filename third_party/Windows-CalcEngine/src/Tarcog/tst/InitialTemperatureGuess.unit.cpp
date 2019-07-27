#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"


using namespace Tarcog;
using namespace FenestrationCommon;

class TestTemperatureInitialGuess : public testing::Test {

private:
	std::shared_ptr< CIGU > m_TarcogIGU;
	std::shared_ptr< CSingleSystem > m_TarcogSystem;
	std::shared_ptr< CIGUSolidLayer > m_solidLayer1;
	std::shared_ptr< CIGUSolidLayer > m_solidLayer2;

protected:
	void SetUp() override {
		/////////////////////////////////////////////////////////
		// Outdoor
		/////////////////////////////////////////////////////////
		auto airTemperature = 255.15; // Kelvins
		auto tSky = airTemperature;
		auto pressure = 101325.0; // Pascals
		auto airSpeed = 5.5; // meters per second
		auto airDirection = AirHorizontalDirection::Windward;
		auto solarRadiation = 0.0;

		std::shared_ptr< CEnvironment > Outdoor =
			std::make_shared< COutdoorEnvironment >( airTemperature, pressure, airSpeed, solarRadiation,
			                                    airDirection, tSky, SkyModel::AllSpecified );
		ASSERT_TRUE( Outdoor != nullptr );

		/////////////////////////////////////////////////////////
		// Indoor
		/////////////////////////////////////////////////////////
		auto roomTemperature = 294.15;
		std::shared_ptr< CEnvironment > Indoor = std::make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( Indoor != nullptr );

		/////////////////////////////////////////////////////////
		// IGU
		/////////////////////////////////////////////////////////
		auto solidLayerThickness = 0.005715; // [m]
		auto solidLayerConductance = 1.0;
		m_solidLayer1 = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( m_solidLayer1 != nullptr );
		m_solidLayer2 = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( m_solidLayer2 != nullptr );

		auto gapThickness = 0.012;
		auto gapPressure = 101325.0;
		auto gapLayer = std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( gapLayer != nullptr );

		auto windowWidth = 1.0;
		auto windowHeight = 1.0;
		auto TarcogIGU = std::make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( TarcogIGU != nullptr );
		TarcogIGU->addLayer( m_solidLayer1 );
		TarcogIGU->addLayer( gapLayer );
		TarcogIGU->addLayer( m_solidLayer2 );

		/////////////////////////////////////////////////////////
		// System
		/////////////////////////////////////////////////////////
		m_TarcogSystem = std::make_shared< CSingleSystem >( TarcogIGU, Indoor, Outdoor );
		ASSERT_TRUE( m_TarcogSystem != nullptr );
	}

public:
	std::shared_ptr< CIGUSolidLayer > getLayer1() const {
		return m_solidLayer1;
	}

	std::shared_ptr< CIGUSolidLayer > getLayer2() const {
		return m_solidLayer2;
	}

};

TEST_F( TestTemperatureInitialGuess, Test1 ) {
	SCOPED_TRACE( "Begin Test: Initial temperature and IR guess" );

	auto aSurface = getLayer1()->getSurface( Side::Front );
	ASSERT_TRUE( aSurface != nullptr );
	auto temperature = aSurface->getTemperature();
	auto J = aSurface->J();
	EXPECT_NEAR( 256.282733081615, temperature, 1e-6 );
	EXPECT_NEAR( 244.589307222020, J, 1e-6 );

	aSurface = getLayer1()->getSurface( Side::Back );
	ASSERT_TRUE( aSurface != nullptr );
	temperature = aSurface->getTemperature();
	J = aSurface->J();
	EXPECT_NEAR( 262.756302643044, temperature, 1e-6 );
	EXPECT_NEAR( 270.254322031419, J, 1e-6 );

	aSurface = getLayer2()->getSurface( Side::Front );
	ASSERT_TRUE( aSurface != nullptr );
	temperature = aSurface->getTemperature();
	J = aSurface->J();
	EXPECT_NEAR( 276.349099622422, temperature, 1e-6 );
	EXPECT_NEAR( 330.668096601357, J, 1e-6 );

	aSurface = getLayer2()->getSurface( Side::Back );
	ASSERT_TRUE( aSurface != nullptr );
	temperature = aSurface->getTemperature();
	J = aSurface->J();
	EXPECT_NEAR( 282.822669183851, temperature, 1e-6 );
	EXPECT_NEAR( 362.757956247504, J, 1e-6 );
}
