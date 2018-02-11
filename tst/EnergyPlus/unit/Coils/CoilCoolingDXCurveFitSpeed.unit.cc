
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <EnergyPlus/Coils/CoilCoolingDXCurveFitSpeed.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;


TEST_F( EnergyPlusFixture, CoilCoolingDXCurveFitSpeedInput )
{

    std::string const idf_objects = delimited_string( {
      "Coil:Cooling:DX:CurveFit:Speed, ",
      " Speed1Name,                    ",
      " 0.8,                           ",
      " 0.745,                         ",
      " 3.1415926,                     ", // condenser
      " 0.9,                           ",
      " 0.9,                           ", // COP
      " 0.5,                           ",
      " 300,                           ",
      " 6.9,                           ", // evaporative
      " 0.8,                           ", // effectiveness
      " CapFT,                         ",
      " CapFF,                         ",
      " EIRFT,                         ",
      " EIRFF,                         ",
      " PLFCurveName,                  ",
      " 0.6,                           ",
      " WasteHeatFunctionCurve,        ",
      " SHRFT,                         ",
      " SHRFF;                         ",
      "Curve:Biquadratic,              ",
      " CapFT,                         ",
      " 1, 0, 0, 0, 0, 0,              ",
      " 0, 1, 0, 1;                    ",
      "Curve:Linear,                   ",
      " CapFF,                         ",
      " 1, 0,                          ",
      " 0, 1;                          ",
      "Curve:Biquadratic,              ",
      " EIRFT,                         ",
      " 1, 0, 0, 0, 0, 0,              ",
      " 0, 1, 0, 1;                    ",
      "Curve:Linear,                   ",
      " EIRFF,                         ",
      " 1, 0,                          ",
      " 0, 1;                          ",
      "Curve:Linear,                   ",
      " PLFCurveName,                  ",
      " 0.85, 0.15,                    ",
      " 0, 1;                          ",
      "Curve:Biquadratic,              ",
      " WasteHeatFunctionCurve,        ",
      " 1, 0, 0, 0, 0, 0,              ",
      " 0, 1, 0, 1;                    ",
      "Curve:Biquadratic,              ",
      " SHRFT,                         ",
      " 1, 0, 0, 0, 0, 0,              ",
      " 0, 1, 0, 1;                    ",
      "Curve:Linear,                   ",
      " SHRFF,                         ",
      " 1, 0,                          ",
      " 0, 1;                          "
    } );

    bool ok = !process_idf( idf_objects, false );
    CoilCoolingDXCurveFitSpeed thisSpeed("Speed1Name", nullptr);

}

TEST_F( EnergyPlusFixture, CoilCoolingDXCurveFitSpeedTest )
{

	CoilCoolingDXCurveFitSpeed thisspeed;
	std::string const idf_objects = delimited_string( {
        "Coil:Cooling:DX:CurveFit:OperatingMode, ",
        " OperatingMode1Name,                    ",
        " 12000,                                 ",
        " 100,                                   ",
        " 200,                                   ",
        " 2.5,                                   ",
        " 0.5,                                   ",
        " 100,                                   ",
        " 300,                                   ",
        " Yes,                                   ",
        " Evaporative,                           ",
        " 200,                                   ",
        " DiscreteStagedContinuousOrNotBacon,    ",
        " 5,                                     ",
        " Speed1Name;                            ",
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
	CoilCoolingDXCurveFitOperatingMode thisMode( "OperatingMode1Name" );
	auto & thisSpeed = thisMode.speeds[0];

	Psychrometrics::PsychState inletState;

	thisSpeed.PLR = 1.0;
	thisSpeed.speedRatio = 1.0;
	inletState.tdb = 20.0;
	inletState.w = 0.008;
	inletState.twb = 14.43;
	inletState.h = 40000.0;
	thisSpeed.CondInletTemp = 35.0;
	thisSpeed.ambPressure = 101325.0;
	thisSpeed.AirFF = 1.0;
	thisSpeed.rated_total_capacity = 3000.0;
	thisSpeed.RatedAirMassFlowRate = 1.0;
	thisSpeed.RatedSHR = 0.75;
	thisSpeed.RatedCBF = 0.09;
	thisSpeed.RatedEIR = 0.30;
	thisSpeed.AirMassFlow = 1.0;
	int fanOpMode = 0;

	auto outletConditions = thisSpeed.CalcSpeedOutput(inletState, thisSpeed.PLR, thisSpeed.speedRatio, fanOpMode );

	EXPECT_NEAR( outletConditions.tdb, 17.057, 0.001 );
	EXPECT_NEAR( outletConditions.w, 0.0078, 0.0001 );
	EXPECT_NEAR( outletConditions.h, 37000.0, 0.1 );
	EXPECT_NEAR( thisSpeed.FullLoadPower, 900.0, 0.1 );
	EXPECT_NEAR( thisSpeed.RTF, 1.0, 0.01 );

}
