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
