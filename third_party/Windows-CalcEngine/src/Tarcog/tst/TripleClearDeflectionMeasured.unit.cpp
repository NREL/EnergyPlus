#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"


using namespace Tarcog;
using namespace FenestrationCommon;

class TripleClearDeflectionMeasured : public testing::Test {

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
		auto solidLayerThickness1 = 0.003048; // [m]
		auto solidLayerThickness2 = 0.005715;
		auto solidLayerConductance = 1.0;

		std::shared_ptr< CBaseIGULayer > aSolidLayer1 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness1, solidLayerConductance );

		std::shared_ptr< CBaseIGULayer > aSolidLayer2 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness2, solidLayerConductance );

		std::shared_ptr< CBaseIGULayer > aSolidLayer3 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness1, solidLayerConductance );

		auto gapThickness = 0.0127;
		auto gapPressure = 101325.0;
		std::shared_ptr< CBaseIGULayer > aGapLayer1 = std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( aGapLayer1 != nullptr );

		std::shared_ptr< CBaseIGULayer > aGapLayer2 = std::make_shared< CIGUGapLayer >( gapThickness, gapPressure );
		ASSERT_TRUE( aGapLayer2 != nullptr );

		auto windowWidth = 1.0;
		auto windowHeight = 1.0;
		auto aIGU = std::make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( aIGU != nullptr );
		aIGU->addLayer( aSolidLayer1 );
		aIGU->addLayer( aGapLayer1 );
		aIGU->addLayer( aSolidLayer2 );
		aIGU->addLayer( aGapLayer2 );
		aIGU->addLayer( aSolidLayer3 );

		// Deflection properties
		std::vector< double > measuredGapsWidths = { 0.0135, 0.013 };
		aIGU->setDeflectionProperties( measuredGapsWidths );

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

TEST_F( TripleClearDeflectionMeasured, Test1 ) {
	SCOPED_TRACE( "Begin Test: Triple Clear - Measured Deflection." );

	auto aSystem = GetSystem();
	ASSERT_TRUE( aSystem != nullptr );

	auto Temperature = *aSystem->getTemperatures();
	std::vector< double > correctTemperature = { 257.493976, 257.702652, 271.535517, 271.926785, 284.395405, 284.604082 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	auto Radiosity = *aSystem->getRadiosities();
	std::vector< double > correctRadiosity = { 247.813715, 258.078374, 300.200818, 318.403140, 362.495875, 380.380188 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

	auto MaxDeflection = *aSystem->getMaxDeflections();
	std::vector< double > correctMaxDeflection = { 0.00074180, -5.820e-05, -0.0003582 };
	ASSERT_EQ( correctMaxDeflection.size(), MaxDeflection.size() );

	for ( auto i = 0u; i < correctMaxDeflection.size(); ++i ) {
		EXPECT_NEAR( correctMaxDeflection[ i ], MaxDeflection[ i ], 1e-7 );
	}

	auto MeanDeflection = *aSystem->getMeanDeflections();
	std::vector< double > correctMeanDeflection = { 0.00031076, -2.437e-05, -0.0001501 };
	ASSERT_EQ( correctMeanDeflection.size(), MeanDeflection.size() );

	for ( auto i = 0u; i < correctMaxDeflection.size(); ++i ) {
		EXPECT_NEAR( correctMeanDeflection[ i ], MeanDeflection[ i ], 1e-7 );
	}

	auto numOfIter = aSystem->getNumberOfIterations();
	EXPECT_EQ( 20u, numOfIter );
}
