
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDXCurveFitSpeed.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;


TEST_F( EnergyPlusFixture, CoilCoolingDXCurveFitSpeedInput )
{

    std::string const idf_objects = delimited_string( {
      "Coil:Cooling:DX:CurveFit:Speed,       ",
      " Speed1Name,             ",
      " 0.8,         ",
      " 0.745,        ",
      " 3.1415926,        ", // condenser
      " 0.9, ",
      " 0.9,                  ",  // COP
      " 0.5,                  ",
      " 300,                  ",
      " 6.9,                  ", // evaporative
      " 0.8,                  ", // effectiveness
      " CapFT,                  ",
      " CapFF,                  ",
      " EIRFT,                  ",
      " EIRFF,                  ",
      " PLFCurveName,                 ",
      " 0.6,              ",
      " WasteHeatFunctionCurve,                 ",
      " SHRFT,                ",
      " SHRFF;                "
    } );

    bool ok = !process_idf( idf_objects, false );
    CoilCoolingDXCurveFitSpeed thisSpeed("Speed1Name");
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

TEST_F( EnergyPlusFixture, CoilCoolingDXCurveFitSpeedTest )
{

	CoilCoolingDXCurveFitSpeed thisspeed;
	std::string const idf_objects = delimited_string( {
		"Coil:Cooling:DX:CurveFit:Speed,       ",
		" Speed1Name,             ",
		" 0.8,         ",
		" 0.745,        ",
		" 3.1415926,        ",
		" 0.9, ",
		" 0.9,                  ",
		" 0.5,                  ",
		" 300,                  ",
		" 6.9,                  ",
		" 0.8,                  ",
		" CapFT,                  ",
		" CapFF,                  ",
		" EIRFT,                  ",
		" EIRFF,                  ",
		" PLFCurveName,                 ",
		" 0.6,              ",
		" WasteHeatFunctionCurve,                 ",
		" SHRFT,                ",
		" SHRFF;                "
	} );

	bool ok = !process_idf( idf_objects, false );
	CoilCoolingDXCurveFitSpeed thisSpeed( "Speed1Name" );

	thisSpeed.PLR = 1.0;
	thisSpeed.coilInletT = 20.0;
	thisSpeed.coilInletW = 0.008;
	thisSpeed.coilInletWB = 14.43;
	thisSpeed.coilInletH = 40000.0;
	thisSpeed.CondInletTemp = 35.0;
	thisSpeed.ambPressure = 101325.0;
	thisSpeed.AirFF = 1.0;
	thisSpeed.RatedTotCap = 3000.0;
	thisSpeed.RatedAirMassFlowRate = 1.0;
	thisSpeed.RatedSHR = 0.75;
	thisSpeed.RatedCBF = 0.09;
	thisSpeed.RatedEIR = 0.30;
	thisSpeed.AirMassFlow = 1.0;
	thisSpeed.FanOpMode = 0;

	thisSpeed.CalcSpeedOutput();

	EXPECT_NEAR( thisSpeed.FullLoadOutAirTemp, 17.057, 0.001 );
	EXPECT_NEAR( thisSpeed.FullLoadOutAirHumRat, 0.0078, 0.0001 );
	EXPECT_NEAR( thisSpeed.FullLoadOutAirEnth, 37000.0, 0.1 );
	EXPECT_NEAR( thisSpeed.FullLoadPower, 900.0, 0.1 );
	EXPECT_NEAR( thisSpeed.RTF, 1.0, 0.01 );

}
