#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace Tarcog;
using namespace FenestrationCommon;

class DoubleLowEVacuumCircularPillar : public testing::Test {

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
		auto solidLayerThickness = 0.004; // [m]
		auto solidLayerConductance = 1.0;
		auto TransmittanceIR = 0.0;
		auto emissivityFrontIR = 0.84;
		auto emissivityBackIR = 0.036749500781;

		std::shared_ptr< CBaseIGULayer > aSolidLayer1 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance,
			                               emissivityFrontIR, TransmittanceIR, emissivityBackIR, TransmittanceIR );

		solidLayerThickness = 0.003962399904;
		emissivityBackIR = 0.84;

		std::shared_ptr< CBaseIGULayer > aSolidLayer2 =
			std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance,
			                               emissivityFrontIR, TransmittanceIR, emissivityBackIR, TransmittanceIR );

		auto gapThickness = 0.0001;
		auto gapPressure = 0.1333;
		auto aGapLayer = CIGUGapLayer( gapThickness, gapPressure );

		// Add support pillars
		auto pillarConductivity = 999.0;
		auto pillarSpacing = 0.03;
		auto pillarRadius = 0.0002;
		std::shared_ptr< CBaseIGULayer > m_GapLayer = make_shared< CCircularPillar >( aGapLayer,
		                                                                         pillarConductivity, pillarSpacing, pillarRadius );

		ASSERT_TRUE( m_GapLayer != nullptr );

		auto windowWidth = 1.0; //[m]
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

		m_TarcogSystem->solve();
	}

public:
	std::shared_ptr< CSingleSystem > GetSystem() const {
		return m_TarcogSystem;
	};

};

TEST_F( DoubleLowEVacuumCircularPillar, Test1 ) {
	SCOPED_TRACE( "Begin Test: Double Low-E - vacuum with circular pillar support" );

	auto aSystem = GetSystem();

	ASSERT_TRUE( aSystem != nullptr );

	auto Temperature = *aSystem->getTemperatures();
	vector< double > correctTemperature = { 255.997063, 256.095933, 290.398479, 290.496419 };
	ASSERT_EQ( correctTemperature.size(), Temperature.size() );

	for ( auto i = 0u; i < correctTemperature.size(); ++i ) {
		EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
	}

	auto Radiosity = *aSystem->getRadiosities();
	vector< double > correctRadiosity = { 242.987484, 396.293176, 402.108090, 407.071738 };
	ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

	for ( auto i = 0u; i < correctRadiosity.size(); ++i ) {
		EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
	}

	auto numOfIter = aSystem->getNumberOfIterations();
	EXPECT_EQ( 21u, numOfIter );
}
