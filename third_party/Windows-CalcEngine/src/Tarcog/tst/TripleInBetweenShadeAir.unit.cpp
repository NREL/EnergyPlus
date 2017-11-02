#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCEGases.hpp"
#include "WCETarcog.hpp"
#include "WCECommon.hpp"


using namespace Tarcog;
using namespace FenestrationCommon;

class TestInBetweenShadeAir : public testing::Test {

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

		std::shared_ptr< CEnvironment > Indoor = std::make_shared< CIndoorEnvironment >( roomTemperature, pressure );
		ASSERT_TRUE( Indoor != nullptr );

		/////////////////////////////////////////////////////////
		// IGU
		/////////////////////////////////////////////////////////
		auto solidLayerThickness = 0.005715; // [m]
		auto solidLayerConductance = 1.0;

		auto aLayer1 = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( aLayer1 != nullptr );

		auto aLayer3 = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( aLayer3 != nullptr );

		auto shadeLayerThickness = 0.01;
		auto shadeLayerConductance = 160.0;
		auto Atop = 0.1;
		auto Abot = 0.1;
		auto Aleft = 0.1;
		auto Aright = 0.1;
		auto Afront = 0.2;

		std::shared_ptr< CIGUSolidLayer > aLayer2 = std::make_shared< CIGUShadeLayer >( shadeLayerThickness, shadeLayerConductance,
		                                                                      std::make_shared< CShadeOpenings >( Atop, Abot, Aleft, Aright, Afront ) );

		ASSERT_TRUE( aLayer2 != nullptr );

		auto gapThickness = 0.0127;
		auto gapPressure = 101325.0;
		auto GapLayer1 = std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( GapLayer1 != nullptr );

		auto GapLayer2 = std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
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

TEST_F( TestInBetweenShadeAir, Test1 ) {
	SCOPED_TRACE( "Begin Test: InBetween Shade - Air" );

	auto aSystem = GetSystem();

	auto temperature = *aSystem->getTemperatures();
	auto radiosity = *aSystem->getRadiosities();

	std::vector< double > correctTemp = { 257.908946, 258.369607, 271.538659, 271.542218, 283.615285, 284.075945 };
	std::vector< double > correctJ = { 249.166619, 260.320613, 300.571561, 316.335545, 358.760651, 378.995512 };

	EXPECT_EQ( correctTemp.size(), temperature.size() );
	EXPECT_EQ( correctJ.size(), radiosity.size() );

	for ( size_t i = 0; i < temperature.size(); ++i ) {
		EXPECT_NEAR( correctTemp[ i ], temperature[ i ], 1e-6 );
		EXPECT_NEAR( correctJ[ i ], radiosity[ i ], 1e-6 );
	}

	auto numOfIter = aSystem->getNumberOfIterations();
	EXPECT_EQ( 20, int( numOfIter ) );
}
