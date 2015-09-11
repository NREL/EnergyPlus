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

TEST( OutputReportTabularTest, RealToStr )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, RealToStr" );
	EXPECT_EQ( "       0.001", RealToStr( 0.0011, 3 ) );
	EXPECT_NE( "       0.001", RealToStr( 0.0019, 3 ) );

	EXPECT_EQ( "          1.", RealToStr( 1.23456789, 0 ) );
	EXPECT_EQ( "         1.2", RealToStr( 1.23456789, 1 ) );
	EXPECT_EQ( "        1.23", RealToStr( 1.23456789, 2 ) );
	EXPECT_EQ( "       1.235", RealToStr( 1.23456789, 3 ) );
	EXPECT_EQ( "      1.2346", RealToStr( 1.23456789, 4 ) );
	EXPECT_EQ( "     1.23457", RealToStr( 1.23456789, 5 ) );
	EXPECT_EQ( "    1.234568", RealToStr( 1.23456789, 6 ) );
	EXPECT_EQ( "   1.2345679", RealToStr( 1.23456789, 7 ) );
	EXPECT_EQ( "  1.23456789", RealToStr( 1.23456789, 8 ) );

	EXPECT_EQ( "    1.234000", RealToStr( 1.234, 6 ) );
	EXPECT_EQ( "   1.2340000", RealToStr( 1.234, 7 ) );
	EXPECT_EQ( "  1.23400000", RealToStr( 1.234, 8 ) );

	EXPECT_EQ( "     123457.", RealToStr( 123456.789, 0 ) );
	EXPECT_EQ( "    123456.8", RealToStr( 123456.789, 1 ) );
	EXPECT_EQ( "   123456.79", RealToStr( 123456.789, 2 ) );
	EXPECT_EQ( "  123456.789", RealToStr( 123456.789, 3 ) );
	EXPECT_EQ( " 123456.7890", RealToStr( 123456.789, 4 ) );

	EXPECT_EQ( "0.123457E+06", RealToStr( 123456.789, 5 ) );

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

TEST( OutputReportTabularTest, GetUnitConversion )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, GetUnitConversion" );

	int indexUnitConv;
	std::string curUnits;
	Real64 curConversionFactor;
	Real64 curConversionOffset;
	std::string varNameWithUnits;

	SetupUnitConversions();

	varNameWithUnits = "ZONE AIR SYSTEM SENSIBLE COOLING RATE[W]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 70, indexUnitConv );
	EXPECT_EQ( "ton", curUnits );
	EXPECT_EQ( 0.0002843333, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "SITE OUTDOOR AIR DRYBULB TEMPERATURE[C]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 11, indexUnitConv );
	EXPECT_EQ( "F", curUnits );
	EXPECT_EQ( 1.8, curConversionFactor );
	EXPECT_EQ( 32., curConversionOffset );

	varNameWithUnits = "ZONE ELECTRIC EQUIPMENT ELECTRIC ENERGY[J]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 20, indexUnitConv );
	EXPECT_EQ( "kWh", curUnits );
	EXPECT_EQ( 0.000000277778, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "ZONE COOLING SETPOINT NOT MET TIME[hr]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 17, indexUnitConv );
	EXPECT_EQ( "hr", curUnits );
	EXPECT_EQ( 1.0, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "ZONE LIGHTS TOTAL HEATING ENERGY[Invalid/Undefined]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 94, indexUnitConv );
	EXPECT_EQ( "Invalid/Undefined", curUnits );
	EXPECT_EQ( 1.0, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "FICTIONAL VARIABLE[qqq]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 0, indexUnitConv );
	EXPECT_EQ( "", curUnits );
	EXPECT_EQ( 0.0, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

}