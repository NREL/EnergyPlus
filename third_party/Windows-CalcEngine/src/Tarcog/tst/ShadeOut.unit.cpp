#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace Tarcog;
using namespace FenestrationCommon;

class TestShadeOut : public testing::Test {

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

		auto Outdoor = make_shared< COutdoorEnvironment >( airTemperature, pressure, airSpeed, solarRadiation,
		                                                   airDirection, tSky, SkyModel::AllSpecified );
		ASSERT_TRUE( Outdoor != nullptr );
		Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH );

		/////////////////////////////////////////////////////////
		// Indoor
		/////////////////////////////////////////////////////////

		auto roomTemperature = 294.15;

		auto Indoor = make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( Indoor != nullptr );

		/////////////////////////////////////////////////////////
		// IGU
		/////////////////////////////////////////////////////////
		auto emissivity = 0.832855582237;
		auto transmittance = 0.074604861438;
		auto surface1 = make_shared< CSurface >( emissivity, transmittance );
		auto surface2 = make_shared< CSurface >( emissivity, transmittance );

		auto shadeLayerThickness = 0.0006;
		auto shadeLayerConductance = 160.0;
		auto Atop = 0.0;
		auto Abot = 0.0;
		auto Aleft = 0.0;
		auto Aright = 0.0;
		auto Afront = 0.5;

		std::shared_ptr< CIGUSolidLayer > aSolidLayer1 = make_shared< CIGUShadeLayer >(
		                                                                          shadeLayerThickness, shadeLayerConductance,
		                                                                          make_shared< CShadeOpenings >( Atop, Abot, Aleft, Aright, Afront ), surface1, surface2 );

		ASSERT_TRUE( aSolidLayer1 != nullptr );

		auto solidLayerThickness = 0.0056134; // [m]
		auto solidLayerConductance = 1.0;
		auto emissivity1 = 0.84;
		auto emissivity2 = 0.038798544556;
		transmittance = 0.0;

		auto surface3 = make_shared< CSurface >( emissivity1, transmittance );
		auto surface4 = make_shared< CSurface >( emissivity2, transmittance );

		auto aSolidLayer2 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance, surface3, surface4 );
		ASSERT_TRUE( aSolidLayer2 != nullptr );

		auto gapThickness = 0.0127;
		auto gapPressure = 101325.0;
		auto aGapLayer = make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( aGapLayer != nullptr );

		auto windowWidth = 1.0;
		auto windowHeight = 1.0;
		auto aIGU = make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( aIGU != nullptr );
		aIGU->addLayer( aSolidLayer1 );
		aIGU->addLayer( aGapLayer );
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

TEST_F( TestShadeOut, Test1 ) {
	SCOPED_TRACE( "Begin Test: Single Clear - U-value" );

	auto aSystem = GetSystem();
	ASSERT_TRUE( aSystem != nullptr );

	auto Temperature = *aSystem->getTemperatures();
	vector< double > correctTemperature = { 256.991924, 256.992140, 269.666330, 270.128394 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	auto Radiosity = *aSystem->getRadiosities();
	vector< double > correctRadiosity = { 249.993042, 250.921069, 291.999868, 419.703053 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

}
