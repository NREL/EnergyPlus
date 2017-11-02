#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

using namespace Tarcog;
using namespace FenestrationCommon;

// Example of double clear window with inital guess for solution
class TestDoubleClear : public testing::Test {

private:
	std::shared_ptr< CSystem > m_TarcogSystem;

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
		auto solarRadiation = 789.0;

		std::shared_ptr< CEnvironment > Outdoor =
			std::make_shared< COutdoorEnvironment >( airTemperature, pressure, airSpeed, solarRadiation,
			                                    airDirection, tSky, SkyModel::AllSpecified );
		ASSERT_TRUE( Outdoor != nullptr );
		Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH );

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

		auto aSolidLayer1 = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		aSolidLayer1->setSolarAbsorptance( 0.166707709432 );

		auto aSolidLayer2 = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		aSolidLayer2->setSolarAbsorptance( 0.112737670541 );

		auto gapThickness = 0.012;
		auto gapPressure = 101325.0;
		std::shared_ptr< CBaseIGULayer > m_GapLayer = std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( m_GapLayer != nullptr );

		auto windowWidth = 1.0;
		auto windowHeight = 1.0;
		auto aIGU = std::make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( aIGU != nullptr );
		aIGU->addLayer( aSolidLayer1 );
		aIGU->addLayer( m_GapLayer );
		aIGU->addLayer( aSolidLayer2 );

		/////////////////////////////////////////////////////////
		// System
		/////////////////////////////////////////////////////////
		m_TarcogSystem = std::make_shared< CSystem >( aIGU, Indoor, Outdoor );
		ASSERT_TRUE( m_TarcogSystem != nullptr );
	}

public:
	std::shared_ptr< CSystem > GetSystem() const {
		return m_TarcogSystem;
	};

};

TEST_F( TestDoubleClear, Test1 ) {
	SCOPED_TRACE( "Begin Test: Double Clear - Surface temperatures" );

	auto aSystem = GetSystem();
	ASSERT_TRUE( aSystem != nullptr );

	//////////////////////////////////////////////////////////////////////
	// UValue run
	//////////////////////////////////////////////////////////////////////

	auto aRun = System::Uvalue;

	auto Temperature = *aSystem->getTemperatures( aRun );
	std::vector< double > correctTemperature = { 258.756688, 259.359226, 279.178510, 279.781048 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	auto Radiosity = *aSystem->getRadiosities( aRun );
	std::vector< double > correctRadiosity = { 251.950834, 268.667346, 332.299338, 359.731700 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

	auto numOfIter = aSystem->getNumberOfIterations( aRun );
	EXPECT_EQ( 20, int( numOfIter ) );

	//////////////////////////////////////////////////////////////////////
	// SHGC run
	//////////////////////////////////////////////////////////////////////

	aRun = System::SHGC;

	Temperature = *aSystem->getTemperatures( aRun );
	correctTemperature = { 264.022835, 265.134421, 287.947300, 288.428857 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	Radiosity = *aSystem->getRadiosities( aRun );
	correctRadiosity = { 269.869356, 295.289318, 374.655901, 397.518724 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

	numOfIter = aSystem->getNumberOfIterations( aRun );
	EXPECT_EQ( 21, int( numOfIter ) );

	//////////////////////////////////////////////////////////////////////
	// General results
	//////////////////////////////////////////////////////////////////////
	auto Uvalue = aSystem->getUValue();
	EXPECT_NEAR( Uvalue, 2.703359, 1e-5 );

	auto SHGC = aSystem->getSHGC( 0.606897 );
	EXPECT_NEAR( SHGC, 0.690096, 1e-5 );
}
