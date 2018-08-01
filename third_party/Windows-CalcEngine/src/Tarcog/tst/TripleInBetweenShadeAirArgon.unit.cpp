#include <memory>
#include <stdexcept>
#include <string>
#include <gtest/gtest.h>

#include "WCEGases.hpp"
#include "WCETarcog.hpp"


using namespace Gases;
using namespace Tarcog;
using namespace FenestrationCommon;

class TestInBetweenShadeAirArgon : public testing::Test {

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

		/////////////////////////////////////////////////////////
		// IGU
		/////////////////////////////////////////////////////////

		// Solid layers
		auto solidLayerThickness = 0.005715; // [m]
		auto solidLayerConductance = 1.0;

		std::shared_ptr< CBaseIGULayer > aLayer1 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( aLayer1 != nullptr );

		std::shared_ptr< CBaseIGULayer > aLayer3 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( aLayer3 != nullptr );

		auto shadeLayerThickness = 0.01;
		auto shadeLayerConductance = 160.0;
		auto Atop = 0.1;
		auto Abot = 0.1;
		auto Aleft = 0.1;
		auto Aright = 0.1;
		auto Afront = 0.2;

		std::shared_ptr< CBaseIGULayer > aLayer2 = std::make_shared< CIGUShadeLayer >( shadeLayerThickness,
		                                                                     shadeLayerConductance, std::make_shared< CShadeOpenings >( Atop, Abot, Aleft, Aright, Afront ) );

		ASSERT_TRUE( aLayer2 != nullptr );

		// gap layers

		// Create coefficients for Air
		CIntCoeff AirCon{ 2.8733e-03, 7.76e-05, 0.0 };
		CIntCoeff AirCp{ 1.002737e+03, 1.2324e-02, 0.0 };
		CIntCoeff AirVisc{ 3.7233e-06, 4.94e-08, 0.0 };

		const std::string AirName = "Air";
		CGasData AirData = { AirName , 28.97, 1.4, AirCp, AirCon, AirVisc };

		// Create coefficients for Argon
		CIntCoeff ArgonCon{ 2.2848e-03, 5.1486e-05, 0.0 };
		CIntCoeff ArgonCp{ 5.21929e+02, 0.0, 0.0 };
		CIntCoeff ArgonVisc{ 3.3786e-06, 6.4514e-08, 0.0 };

		const std::string ArgonName = "Argon";
		CGasData ArgonData{ ArgonName, 39.948, 1.67, ArgonCp, ArgonCon, ArgonVisc };

		CGasItem Air{ 0.1, AirData };
		CGasItem Argon{ 0.9, ArgonData };

		// Create gas mixture
		auto Gas1 = std::make_shared< CGas >();

		Gas1->addGasItem( Air );
		Gas1->addGasItem( Argon );

		auto Gas2 = std::make_shared< CGas >();
		( *Gas2 ) = ( *Gas1 );

		auto gapThickness = 0.0127;
		auto gapPressure = 101325.0;
		std::shared_ptr< CBaseIGULayer > GapLayer1 =
			std::make_shared< CIGUGapLayer >( gapThickness, gapPressure, Gas1 );
		ASSERT_TRUE( GapLayer1 != nullptr );

		std::shared_ptr< CBaseIGULayer > GapLayer2 =
			std::make_shared< CIGUGapLayer >( gapThickness, gapPressure, Gas2 );
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
	std::shared_ptr< CSingleSystem > GetSystem() const {
		return m_TarcogSystem;
	};

};

TEST_F( TestInBetweenShadeAirArgon, Test1 ) {
	SCOPED_TRACE( "Begin Test: InBetween Shade - Air(10%)/Argon(90%)" );

	auto aSystem = GetSystem();
	ASSERT_TRUE( aSystem != nullptr );

	auto Temperature = *aSystem->getTemperatures();
	std::vector< double > correctTemperature = { 257.708586, 258.135737, 271.904015, 271.907455, 284.412841, 284.839992 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-6 );
	}

	auto Radiosity = *aSystem->getRadiosities();
	std::vector< double > correctRadiosity = { 248.512581, 259.762360, 301.878568, 318.339706, 362.562135, 382.345742 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-6 );
	}

	auto numOfIter = GetSystem()->getNumberOfIterations();
	EXPECT_EQ( 21u, numOfIter );
}
