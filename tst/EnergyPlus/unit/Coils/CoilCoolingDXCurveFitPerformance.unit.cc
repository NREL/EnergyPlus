
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDX.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;


TEST_F( EnergyPlusFixture, CoilCoolingDXCurveFitPerformanceInput )
{

    std::string const idf_objects = delimited_string( {
      "Coil:Cooling:DX:CurveFit:Performance,",
      " PerformanceName,             ",
      " 100,         ",
      " 0,        ",
      " 1,        ",
      " 100, ",
      " SwitchingMethodName,                  ",
      " OperatingModeScheduleName,                  ",
      " 100,                  ",
      " 400,                  ",
      " BasinHeaterOpSchedule,                  ",
      " OperatingMode1,                  ",
      " OperatingMode2;                  "
    } );

    bool ok = !process_idf( idf_objects, false );
    int i = 1;
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
