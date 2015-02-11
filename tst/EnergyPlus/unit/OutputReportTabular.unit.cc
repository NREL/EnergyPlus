// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/OutputReportTabular.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::OutputReportTabular;
using namespace ObjexxFCL;

TEST( OutputReportTabularTest, ConfirmSetUnitsStyleFromString )
{
	EXPECT_EQ( unitsStyleNone, SetUnitsStyleFromString( "None" ) );
	EXPECT_EQ( unitsStyleJtoKWH, SetUnitsStyleFromString( "JTOKWH" ) );
	EXPECT_EQ( unitsStyleJtoMJ, SetUnitsStyleFromString( "JTOMJ" ) );
	EXPECT_EQ( unitsStyleJtoGJ, SetUnitsStyleFromString( "JTOGJ" ) );
	EXPECT_EQ( unitsStyleInchPound, SetUnitsStyleFromString( "INCHPOUND" ) );
	EXPECT_EQ( unitsStyleNotFound, SetUnitsStyleFromString( "qqq" ) );
}

TEST( OutputReportTabularTest, Basic )
{
	OutputTableBinned.allocate( 10 );
	EXPECT_TRUE( warningAboutKeyNotFound( 0, 1, "moduleName" ) );
	EXPECT_FALSE( warningAboutKeyNotFound( 100, 1, "moduleName") );
}
