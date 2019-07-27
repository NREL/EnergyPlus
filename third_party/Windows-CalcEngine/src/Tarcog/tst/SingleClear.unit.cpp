#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"


using namespace Tarcog;
using namespace FenestrationCommon;

class TestSingleClear : public testing::Test {

private:
	std::shared_ptr< CSystem > m_TarcogSystem;

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
		auto solidLayerThickness = 0.003048; // [m]
		auto solidLayerConductance = 1.0;

		auto aSolidLayer = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance );
		ASSERT_TRUE( aSolidLayer != nullptr );
		aSolidLayer->setSolarAbsorptance( 0.094189159572 );

		auto windowWidth = 1.0;
		auto windowHeight = 1.0;
		auto aIGU = std::make_shared< CIGU >( windowWidth, windowHeight );
		ASSERT_TRUE( aIGU != nullptr );
		aIGU->addLayer( aSolidLayer );

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

TEST_F( TestSingleClear, Test1 ) {
	SCOPED_TRACE( "Begin Test: Single Clear - U-value" );

	auto aSystem = GetSystem();
	ASSERT_TRUE( aSystem != nullptr );

	/////////////////////////////////////////////////////////////////////////
	//  U-value run
	/////////////////////////////////////////////////////////////////////////
	auto Temperature = *aSystem->getTemperatures( System::Uvalue );
	std::vector< double > correctTemperature = { 297.207035, 297.14470 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	auto Radiosity = *aSystem->getRadiosities( System::Uvalue );
	std::vector< double > correctRadiosity = { 432.444546, 439.201749 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

	auto numOfIterations = aSystem->getNumberOfIterations( System::Uvalue );
	EXPECT_EQ( 19u, numOfIterations );

	/////////////////////////////////////////////////////////////////////////
	//  SHGC run
	/////////////////////////////////////////////////////////////////////////
	Temperature = *aSystem->getTemperatures( System::SHGC );
	correctTemperature = { 299.116601, 299.121730 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	Radiosity = *aSystem->getRadiosities( System::SHGC );
	correctRadiosity = { 442.087153, 449.182158 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

	numOfIterations = aSystem->getNumberOfIterations( System::SHGC );
	EXPECT_EQ( 19u, numOfIterations );

	/////////////////////////////////////////////////////////////////////////
	//  Heat flows
	/////////////////////////////////////////////////////////////////////////
	auto heatFlow = aSystem->getHeatFlow( System::Uvalue, Environment::Indoor );
	EXPECT_NEAR( heatFlow, -20.450949, 1e-5 );

	heatFlow = aSystem->getHeatFlow( System::Uvalue, Environment::Outdoor );
	EXPECT_NEAR( heatFlow, -20.450949, 1e-5 );

	heatFlow = aSystem->getHeatFlow( System::SHGC, Environment::Indoor );
	EXPECT_NEAR( heatFlow, -35.474878, 1e-5 );

	heatFlow = aSystem->getHeatFlow( System::SHGC, Environment::Outdoor );
	EXPECT_NEAR( heatFlow, 38.840370, 1e-5 );

	/////////////////////////////////////////////////////////////////////////
	//  System properties
	/////////////////////////////////////////////////////////////////////////
	auto UValue = aSystem->getUValue();
	EXPECT_NEAR( UValue, 5.493806, 1e-5 );

	auto SHGC = aSystem->getSHGC( 0.831249 );
	EXPECT_NEAR( SHGC, 0.850291, 1e-5 );
}
