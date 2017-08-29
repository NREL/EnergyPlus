#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace Tarcog;
using namespace FenestrationCommon;

// Example of double clear window with inital guess for solution
class TestDoubleClearSingleSystemWithInitialGuess : public testing::Test {

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

		auto roomTemperature = 294.15;

		std::shared_ptr< CEnvironment > Indoor =
			std::make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( Indoor != nullptr );

		/////////////////////////////////////////////////////////
		// IGU
		/////////////////////////////////////////////////////////
		auto solidLayerThickness = 0.005715; // [m]
		auto solidLayerConductance = 1.0;

		std::shared_ptr< CBaseIGULayer > aSolidLayer1 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );

		std::shared_ptr< CBaseIGULayer > aSolidLayer2 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );

		auto gapThickness = 0.012;
		auto gapPressure = 101325.0;
		std::shared_ptr< CBaseIGULayer > m_GapLayer = make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( m_GapLayer != nullptr );

		auto windowWidth = 1.0;
		auto windowHeight = 1.0;
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

		// set up initial guess. It was taken to be close to solution.
		vector< double > aTemperatures = { 258.75, 259.36, 279.18, 279.78 };
		m_TarcogSystem->setInitialGuess( aTemperatures );

		m_TarcogSystem->solve();
	}

public:
	std::shared_ptr< CSingleSystem > GetSystem() const {
		return m_TarcogSystem;
	};

};

TEST_F( TestDoubleClearSingleSystemWithInitialGuess, Test1 ) {
	SCOPED_TRACE( "Begin Test: Double Clear Single System (Initial guess) - Surface temperatures" );

	auto aSystem = GetSystem();
	ASSERT_TRUE( aSystem != nullptr );

	auto Temperature = *aSystem->getTemperatures();
	vector< double > correctTemperature = { 258.756688, 259.359226, 279.178510, 279.781048 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	auto Radiosity = *aSystem->getRadiosities();
	vector< double > correctRadiosity = { 251.950834, 268.667346, 332.299338, 359.731700 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

	auto numOfIter = aSystem->getNumberOfIterations();
	EXPECT_EQ( 17u, numOfIter );
}
