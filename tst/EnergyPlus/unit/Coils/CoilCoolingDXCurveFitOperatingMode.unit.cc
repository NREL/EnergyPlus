
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDX.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;


TEST_F( EnergyPlusFixture, CoilCoolingDXCurveFitOperatingModeInput )
{

    std::string const idf_objects = delimited_string( {
      "Coil:Cooling:DX:CurveFit:OperatingMode,       ",
      " OperatingMode1Name,             ",
      " 12000,         ",
      " 100,        ",
      " 200,        ",
      " 20, ",
      " 0.5,                  ",
      " 100,                  ",
      " 300,                  ",
      " Yes,                  ",
      " Evaporative,                  ",
      " 5,                  ",
      " OperatingSpeed1,                  ",
      " OperatingSpeed2;                  "
    } );

    ASSERT_FALSE( process_idf( idf_objects, false ) );
//
//    GetBoilerInput();
//
//    EXPECT_EQ( Boiler( NumBoilers ).Name, "STEAM BOILER PLANT BOILER" );
//    EXPECT_EQ( Boiler( NumBoilers ).FuelType, AssignResourceTypeNum( "NATURALGAS" ) );
//    EXPECT_EQ( Boiler( NumBoilers ).BoilerMaxOperPress, 160000 );
//    EXPECT_EQ( Boiler( NumBoilers ).Effic, 0.8 );
//    EXPECT_EQ( Boiler( NumBoilers ).TempUpLimitBoilerOut, 115 );
//    EXPECT_EQ( Boiler( NumBoilers ).NomCap, AutoSize );
//    EXPECT_EQ( Boiler( NumBoilers ).MinPartLoadRat, 0.00001 );
//    EXPECT_EQ( Boiler( NumBoilers ).MaxPartLoadRat, 1.0 );
//    EXPECT_EQ( Boiler( NumBoilers ).OptPartLoadRat, 0.2 );
//    EXPECT_EQ( Boiler( NumBoilers ).FullLoadCoef( 1 ), 0.8 );
//    EXPECT_EQ( Boiler( NumBoilers ).FullLoadCoef( 2 ), 0.1 );
//    EXPECT_EQ( Boiler( NumBoilers ).FullLoadCoef( 3 ), 0.1 );
//    EXPECT_EQ( Boiler( NumBoilers ).SizFac, 1.0 );

}
