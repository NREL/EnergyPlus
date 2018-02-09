
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
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
    CoilCoolingDXCurveFitSpeed thisSpeed("Speed1Name");

}

TEST_F( EnergyPlusFixture, CoilCoolingDXCurveFitSpeedTest )
{

	CoilCoolingDXCurveFitSpeed thisspeed;

	thisspeed.coilInletT = 20.2;
	thisspeed.coilInletW = 0.01;
	thisspeed.coilInletWB = 19.0;
	thisspeed.coilInletH = 35000.0;
	thisspeed.CondInletTemp = 35.0;
	thisspeed.ambPressure = 101325.0;
	thisspeed.AirFF = 1.0;
	thisspeed.RatedTotCap = 3000.0;
	thisspeed.RatedAirMassFlowRate = 1.0;
	thisspeed.RatedSHR = 0.75;
	thisspeed.RatedCBF = 0.09;
	thisspeed.RatedEIR = 0.30;
	thisspeed.AirMassFlow = 1.0;
	thisspeed.FanOpMode = 0;

	thisspeed.CalcSpeedOutput();

}
