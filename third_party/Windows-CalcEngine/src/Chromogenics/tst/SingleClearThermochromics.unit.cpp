#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCEChromogenics.hpp"
#include "WCECommon.hpp"

using namespace Tarcog;
using namespace FenestrationCommon;
using namespace Chromogenics;

class TestSingleClearThermochromics : public testing::Test {

  std::unique_ptr< CSystem > m_TarcogSystem;

protected:
  void SetUp() override {
    /////////////////////////////////////////////////////////
    // Outdoor
    /////////////////////////////////////////////////////////
    auto airTemperature = 300; // Kelvins
    auto pressure = 101325; // Pascals
    auto airSpeed = 5.5; // meters per second
    auto airDirection = AirHorizontalDirection::Windward;
    auto tSky = 270; // Kelvins
    auto solarRadiation = 789;

    std::shared_ptr< CEnvironment > Outdoor = 
      std::make_shared< COutdoorEnvironment >( airTemperature, pressure, airSpeed, solarRadiation, 
        airDirection, tSky, SkyModel::AllSpecified );
    ASSERT_TRUE( Outdoor != nullptr );
    Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH );

    /////////////////////////////////////////////////////////
    // Indoor
    /////////////////////////////////////////////////////////

    auto roomTemperature = 294.15;
    std::shared_ptr< CEnvironment > Indoor = std::make_shared< CIndoorEnvironment > ( roomTemperature, pressure );
    ASSERT_TRUE( Indoor != nullptr );

    /////////////////////////////////////////////////////////
    // IGU
    /////////////////////////////////////////////////////////
    auto solidLayerThickness = 0.003048; // [m]
    auto solidLayerConductance = 1;

    auto transmittance = 0;
    auto emissivity = 0.84;

    // Thermochromics property of the surface emissivity
    std::vector< std::pair< double, double > > emissivities = {
      std::make_pair( 288.15, 0.84 ),
      std::make_pair( 293.15, 0.74 ),
      std::make_pair( 296.15, 0.64 ),
      std::make_pair( 300.15, 0.54 ),
      std::make_pair( 303.15, 0.44 )
    };

    // Creates thermochromic surface on indoor side with variable emissivity and constant transmittance
    std::shared_ptr< ISurface > frontSurface = std::make_shared< CSurface >( emissivity, transmittance );
    std::shared_ptr< ISurface > backSurface = std::make_shared< CThermochromicSurface >( emissivities, transmittance );
    // std::shared_ptr< ISurface > backSurface = std::make_shared< CSurface >( 0.61350442289072993, transmittance );

    auto aSolidLayer = std::make_shared< CIGUSolidLayer >( solidLayerThickness, solidLayerConductance, 
      frontSurface, backSurface );
    ASSERT_TRUE( aSolidLayer != nullptr );
    aSolidLayer->setSolarAbsorptance( 0.094189159572 );

    auto windowWidth = 1;
    auto windowHeight = 1;
    auto aIGU = std::make_shared< CIGU >( windowWidth, windowHeight );
    ASSERT_TRUE( aIGU != nullptr );
    aIGU->addLayer( aSolidLayer );

    /////////////////////////////////////////////////////////
    // System
    /////////////////////////////////////////////////////////
		// TODO: This need to be changed. C++11 does not support make_unique
    m_TarcogSystem = std::unique_ptr< CSystem >( new CSystem( aIGU, Indoor, Outdoor ) );
    ASSERT_TRUE( m_TarcogSystem != nullptr );

    // m_TarcogSystem->solve();
  }

public:
  CSystem* GetSystem() const { return m_TarcogSystem.get(); };

};

TEST_F( TestSingleClearThermochromics, Test1 ) {
  SCOPED_TRACE( "Begin Test: Single Clear Thermochromics - U-value" );

  auto aSystem = GetSystem();
  ASSERT_TRUE( aSystem != nullptr );

  /////////////////////////////////////////////////////////////////////////
  //  U-value run
  /////////////////////////////////////////////////////////////////////////
  auto aSolidLayers = aSystem->getSolidLayers( System::Uvalue );

  auto aLayer = *aSolidLayers[ 0 ];

  auto emissivity = aLayer.getSurface( Side::Back )->getEmissivity();
  EXPECT_NEAR( emissivity, 0.610863, 1e-5 );

  auto Temperature = *aSystem->getTemperatures( System::Uvalue );
  std::vector< double > correctTemperature = { 297.313984, 297.261756 };
  ASSERT_EQ( correctTemperature.size(), Temperature.size() );

  for( auto i = 0u; i < correctTemperature.size(); ++i ) {
    EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
  }

  auto Radiosity = *aSystem->getRadiosities( System::Uvalue );
  std::vector< double > correctRadiosity = { 432.979711, 435.605837 };
  ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

  for( auto i = 0u; i < correctRadiosity.size(); ++i ) {
    EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
  }

  auto numOfIterations = aSystem->getNumberOfIterations( System::Uvalue );
  EXPECT_EQ( 19u, numOfIterations );

  /////////////////////////////////////////////////////////////////////////
  //  SHGC run
  /////////////////////////////////////////////////////////////////////////
  aSolidLayers = aSystem->getSolidLayers( System::SHGC );

  aLayer = *aSolidLayers[ 0 ];

  emissivity = aLayer.getSurface( Side::Back )->getEmissivity();
  EXPECT_NEAR( emissivity, 0.561212, 1e-5 );

  Temperature = *aSystem->getTemperatures( System::SHGC );
  correctTemperature = { 299.333611, 299.359313 };
  ASSERT_EQ( correctTemperature.size(), Temperature.size() );

  for( auto i = 0u; i < correctTemperature.size(); ++i ) {
    EXPECT_NEAR( correctTemperature[ i ], Temperature[ i ], 1e-5 );
  }

  Radiosity = *aSystem->getRadiosities( System::SHGC );
  correctRadiosity = { 443.194727, 441.786960 };
  ASSERT_EQ( correctRadiosity.size(), Radiosity.size() );

  for( auto i = 0u; i < correctRadiosity.size(); ++i ) {
    EXPECT_NEAR( correctRadiosity[ i ], Radiosity[ i ], 1e-5 );
  }

  numOfIterations = aSystem->getNumberOfIterations( System::SHGC );
  EXPECT_EQ( 19u, numOfIterations );

  /////////////////////////////////////////////////////////////////////////
  //  Heat flows
  /////////////////////////////////////////////////////////////////////////
  auto heatFlow = aSystem->getHeatFlow( System::Uvalue, Environment::Indoor );
  EXPECT_NEAR( heatFlow, -17.135106, 1e-5 );

  heatFlow = aSystem->getHeatFlow( System::Uvalue, Environment::Outdoor );
  EXPECT_NEAR( heatFlow, -17.135106, 1e-5 );

  heatFlow = aSystem->getHeatFlow( System::SHGC, Environment::Indoor );
  EXPECT_NEAR( heatFlow, -28.725048, 1e-5 );

  heatFlow = aSystem->getHeatFlow( System::SHGC, Environment::Outdoor );
  EXPECT_NEAR( heatFlow, 45.590199, 1e-5 );

  /////////////////////////////////////////////////////////////////////////
  //  System properties
  /////////////////////////////////////////////////////////////////////////
  auto UValue = aSystem->getUValue();
  EXPECT_NEAR( UValue, 4.604300, 1e-5 );

  auto SHGC = aSystem->getSHGC( 0.831249 );
  EXPECT_NEAR( SHGC, 0.845938, 1e-5 );
}
