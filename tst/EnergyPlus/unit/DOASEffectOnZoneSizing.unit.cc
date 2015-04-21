// EnergyPlus::Standalone unit tests of DOAS effect on zone sizing feature

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <ZoneEquipmentManager.hh>
#include <InputProcessor.hh>
#include <DataStringGlobals.hh>
#include <DataEnvironment.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::ZoneEquipmentManager;
using namespace EnergyPlus::InputProcessor;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::Psychrometrics;

using namespace ObjexxFCL;

TEST(DOASEffectOnZoneSizing, CalcDOASSupCondsForSizing)
{
	ShowMessage( "Begin Test: DOASEffectOnZoneSizing, CalcDOASSupCondsForSizing" );
	// locals
  Real64 OutDB; // outside air temperature [C]
	Real64 OutHR; // outside humidity ratio [kg Water / kg Dry Air]
	int DOASControl; // dedicated outside air control strategy
	Real64 DOASLowTemp; // DOAS low setpoint [C]
	Real64 DOASHighTemp; // DOAS high setpoint [C]
	Real64 DOASSupTemp;  // DOAS supply temperature [C]
	Real64 DOASSupHR; // DOAS supply humidity ratio [kg H2O / kg dry air]
  // neutral supply air
  DOASControl = 1;
  DOASLowTemp = 21.1;
  DOASHighTemp = 23.9;
  OutDB = 10.0;
  OutHR = 0.005;
  StdBaroPress = 101325.0;
  CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, DOASSupTemp, DOASSupHR );  
  EXPECT_DOUBLE_EQ( 21.1, DOASSupTemp );
  EXPECT_DOUBLE_EQ( 0.005, DOASSupHR );
  OutDB = 35.6;
  OutHR = 0.0185;
  CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, DOASSupTemp, DOASSupHR );
  EXPECT_DOUBLE_EQ( 23.9, DOASSupTemp );
  EXPECT_DOUBLE_EQ( 0.017, DOASSupHR );
  OutDB = 22.3;
  OutHR = 0.0085;
  CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, DOASSupTemp, DOASSupHR );
  EXPECT_DOUBLE_EQ( 22.3, DOASSupTemp );
  EXPECT_DOUBLE_EQ( 0.0085, DOASSupHR );
  // neutral dehumidified supply air
  DOASControl = 2;
  DOASLowTemp = 14.4;
  DOASHighTemp = 22.2;
  OutDB = 11;
  OutHR = 0.004;
  CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, DOASSupTemp, DOASSupHR );
  EXPECT_DOUBLE_EQ( 22.2, DOASSupTemp );
  EXPECT_DOUBLE_EQ( 0.004, DOASSupHR );
  OutDB = 35.6;
  OutHR = 0.0185;
  CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, DOASSupTemp, DOASSupHR );
  EXPECT_DOUBLE_EQ( 22.2, DOASSupTemp );
  EXPECT_DOUBLE_EQ( 0.0092, DOASSupHR );
  // cold supply air
  DOASControl = 3;
  DOASLowTemp = 12.2;
  DOASHighTemp = 14.4;
  OutDB = 11;
  OutHR = 0.005;
  CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, DOASSupTemp, DOASSupHR );
  EXPECT_DOUBLE_EQ( 14.4, DOASSupTemp );
  EXPECT_DOUBLE_EQ( 0.005, DOASSupHR );
  OutDB = 35.6;
  OutHR = 0.0185;
  CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, DOASSupTemp, DOASSupHR );
  EXPECT_DOUBLE_EQ( 12.2, DOASSupTemp );
  EXPECT_DOUBLE_EQ( 0.008, DOASSupHR );
}
