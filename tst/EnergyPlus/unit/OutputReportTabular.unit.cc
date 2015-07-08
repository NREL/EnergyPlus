// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::OutputReportTabular;
using namespace ObjexxFCL;

TEST( OutputReportTabularTest, ConfirmSetUnitsStyleFromString )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, ConfirmSetUnitsStyleFromString" );

	EXPECT_EQ( unitsStyleNone, SetUnitsStyleFromString( "None" ) );
	EXPECT_EQ( unitsStyleJtoKWH, SetUnitsStyleFromString( "JTOKWH" ) );
	EXPECT_EQ( unitsStyleJtoMJ, SetUnitsStyleFromString( "JTOMJ" ) );
	EXPECT_EQ( unitsStyleJtoGJ, SetUnitsStyleFromString( "JTOGJ" ) );
	EXPECT_EQ( unitsStyleInchPound, SetUnitsStyleFromString( "INCHPOUND" ) );
	EXPECT_EQ( unitsStyleNotFound, SetUnitsStyleFromString( "qqq" ) );
}

TEST( OutputReportTabularTest, Basic )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, Basic" );

	OutputTableBinned.allocate( 10 );
	EXPECT_TRUE( warningAboutKeyNotFound( 0, 1, "moduleName" ) );
	EXPECT_FALSE( warningAboutKeyNotFound( 100, 1, "moduleName") );
}

TEST(OutputReportTabularTest, ConfirmResourceWarning)
{
	ShowMessage( "Begin Test: OutputReportTabularTest, ConfirmResourceWarning" );
	
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Electricity [kWh]",
		ResourceWarningMessage("Electricity [kWh]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Natural Gas [kWh]",
		ResourceWarningMessage("Natural Gas [kWh]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Additional Fuel [kWh]",
		ResourceWarningMessage("Additional Fuel [kWh]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: District Cooling [kBtu]",
		ResourceWarningMessage("District Cooling [kBtu]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: District Heating [kBtu]",
		ResourceWarningMessage("District Heating [kBtu]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Water [GJ]",
		ResourceWarningMessage("Water [GJ]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Electricity [GJ]",
		ResourceWarningMessage("Electricity [GJ]"));
	EXPECT_NE("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Gas [kWh]",
		ResourceWarningMessage("Electricity [kWh]")); 
}

TEST(OutputReportTabularTest, ConfirmWaterConversion)
{
	ShowMessage("Begin Test: OutputReportTabularTest, ConfirmWaterConversion");

	EXPECT_EQ(15, WaterConversionFunct(75, 5));
	EXPECT_EQ(1, WaterConversionFunct(1, 1));
	EXPECT_EQ(13.756, WaterConversionFunct(481.46, 35));
	EXPECT_EQ(-2, WaterConversionFunct(-12, 6));
	EXPECT_NE(15, WaterConversionFunct(135, 5));

}