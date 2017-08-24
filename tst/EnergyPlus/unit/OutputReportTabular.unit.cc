// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include "Fixtures/SQLiteFixture.hh"

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataGlobalConstants;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::OutputProcessor;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::OutputReportTabular;
using namespace EnergyPlus::OutputProcessor;
using namespace SimulationManager;
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

TEST_F( EnergyPlusFixture, OutputReportTabularTest_Basic )
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

TEST(OutputReportTabularTest, isNumber)
{
	ShowMessage("Begin Test: OutputReportTabularTest, isNumber");
	EXPECT_TRUE(isNumber("0"));
	EXPECT_TRUE(isNumber("0.12"));
	EXPECT_TRUE(isNumber("0.12E01"));
	EXPECT_TRUE(isNumber("-6"));
	EXPECT_TRUE(isNumber("-6.12"));
	EXPECT_TRUE(isNumber("-6.12E-09"));
	EXPECT_TRUE(isNumber(" 0"));
	EXPECT_TRUE(isNumber(" 0.12"));
	EXPECT_TRUE(isNumber(" 0.12E01"));
	EXPECT_TRUE(isNumber("0 "));
	EXPECT_TRUE(isNumber("0.12 "));
	EXPECT_TRUE(isNumber("0.12E01 "));
	EXPECT_TRUE(isNumber(" 0 "));
	EXPECT_TRUE(isNumber(" 0.12 "));
	EXPECT_TRUE(isNumber(" 0.12E01 "));
}

TEST(OutputReportTabularTest, digitsAferDecimal)
{
	ShowMessage("Begin Test: OutputReportTabularTest, digitsAferDecimal");
	EXPECT_EQ(0, digitsAferDecimal("0"));
	EXPECT_EQ(0, digitsAferDecimal("1."));
	EXPECT_EQ(2, digitsAferDecimal("0.12"));
	EXPECT_EQ(4, digitsAferDecimal("0.1234"));
	EXPECT_EQ(2, digitsAferDecimal("3.12E01"));
	EXPECT_EQ(0, digitsAferDecimal("-6"));
	EXPECT_EQ(0, digitsAferDecimal("-6."));
	EXPECT_EQ(2, digitsAferDecimal("-6.12"));
	EXPECT_EQ(5, digitsAferDecimal("-6.12765"));
	EXPECT_EQ(2, digitsAferDecimal("-6.12E-09"));
}

TEST(OutputReportTabularTest, splitCommaString)
{
	ShowMessage("Begin Test: OutputReportTabularTest, splitCommaString");
	std::vector<std::string> actual;
	actual.push_back("part1");
	EXPECT_EQ(actual, splitCommaString("part1"));
	actual.push_back("part2");
	EXPECT_EQ(actual, splitCommaString("part1,part2"));
	EXPECT_EQ(actual, splitCommaString(" part1,part2 "));
	EXPECT_EQ(actual, splitCommaString(" part1 , part2 "));
	actual.push_back("part3");
	EXPECT_EQ(actual, splitCommaString("part1,part2,part3"));
	EXPECT_EQ(actual, splitCommaString(" part1 , part2 , part3 "));
}

TEST(OutputReportTabularTest, unitsFromHeading)
{
	ShowMessage("Begin Test: OutputReportTabularTest, unitsFromHeading");
	std::string unitString;
	SetupUnitConversions();
	unitsStyle = unitsStyleInchPound;
	unitString = "";
	EXPECT_EQ(96, unitsFromHeading(unitString)); 
	EXPECT_EQ("", unitString);
	unitString = "Zone Floor Area {m2}";
	EXPECT_EQ(46, unitsFromHeading(unitString));
	EXPECT_EQ("Zone Floor Area {ft2}", unitString);
	unitString = "Fictional field {nonsense}";
	EXPECT_EQ(0, unitsFromHeading(unitString));
	EXPECT_EQ("Fictional field {nonsense}", unitString);

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

TEST_F( EnergyPlusFixture, OutputReportTabularTest_GetUnitConversion )
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
	EXPECT_EQ( 95, indexUnitConv );
	EXPECT_EQ( "Invalid/Undefined", curUnits );
	EXPECT_EQ( 1.0, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "FICTIONAL VARIABLE[qqq]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 0, indexUnitConv );
	EXPECT_EQ( "", curUnits );
	EXPECT_EQ( 1.0, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "ZONE PEOPLE OCCUPANT COUNT[]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 96, indexUnitConv );
	EXPECT_EQ( "", curUnits );
	EXPECT_EQ( 1.0, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	std::vector<std::string> units = {
		"[ ]",
		"[%]",
		"[]",
		"[A]",
		"[ach]",
		"[Ah]",
		"[C]",
		"[cd/m2]",
		"[clo]",
		"[deg]",
		"[deltaC]",
		"[hr]",
		"[J/kg]",
		"[J/kg-K]",
		"[J/kgWater]",
		"[J/m2]",
		"[J]",
		"[K/m]",
		"[kg/kg]",
		"[kg/m3]",
		"[kg/s]",
		"[kg]",
		"[kgWater/kgDryAir]",
		"[kgWater/s]",
		"[kmol/s]",
		"[L]",
		"[lum/W]",
		"[lux]",
		"[m/s]",
		"[m]",
		"[m2]",
		"[m3/s]",
		"[m3]",
		"[min]",
		"[Pa]",
		"[ppm]",
		"[rad]",
		"[rev/min]",
		"[s]",
		"[V]",
		"[W/K]",
		"[W/m2]",
		"[W/m2-C]",
		"[W/m2-K]",
		"[W/W]",
		"[W]"
	};

	for ( auto u : units ) {
		LookupSItoIP( u, indexUnitConv, curUnits );
		EXPECT_NE( indexUnitConv, 0 );
	}

}


TEST_F( EnergyPlusFixture, OutputReportTabularTest_LookupJtokWH )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, OutputReportTabularTest_LookupJtokWH" );

	int indexUnitConv;
	std::string curUnits;
	std::string varNameWithUnits;

	SetupUnitConversions();

	varNameWithUnits = "ZONE AIR SYSTEM SENSIBLE COOLING RATE[W]";
	LookupJtokWH( varNameWithUnits, indexUnitConv, curUnits );
	EXPECT_EQ( 0, indexUnitConv );
	EXPECT_EQ( "ZONE AIR SYSTEM SENSIBLE COOLING RATE[W]", curUnits );

	varNameWithUnits = "Electric Energy Use [GJ]";
	LookupJtokWH( varNameWithUnits, indexUnitConv, curUnits );
	EXPECT_EQ( 85, indexUnitConv );
	EXPECT_EQ( "Electric Energy Use [kWh]", curUnits );

	varNameWithUnits = "Electricty [MJ/m2]";
	LookupJtokWH( varNameWithUnits, indexUnitConv, curUnits );
	EXPECT_EQ( 94, indexUnitConv );
	EXPECT_EQ( "Electricty [kWh/m2]", curUnits );

}


TEST( OutputReportTabularTest, GetColumnUsingTabs )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, GetColumnUsingTabs" );
{
	std::string inString = " Col1 \t Col2 \t Col3 ";
	EXPECT_EQ( " Col1 ", GetColumnUsingTabs( inString, 1 ) );
	EXPECT_EQ( " Col2 ", GetColumnUsingTabs( inString, 2 ) );
	EXPECT_EQ( " Col3 ", GetColumnUsingTabs( inString, 3 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 4 ) );
}

{
	std::string inString = "Col1\tCol2\tCol3";
	EXPECT_EQ( "Col1", GetColumnUsingTabs( inString, 1 ) );
	EXPECT_EQ( "Col2", GetColumnUsingTabs( inString, 2 ) );
	EXPECT_EQ( "Col3", GetColumnUsingTabs( inString, 3 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 4 ) );
}

{
	std::string inString = "Col1\tCol2\tCol3\t";
	EXPECT_EQ( "Col1", GetColumnUsingTabs( inString, 1 ) );
	EXPECT_EQ( "Col2", GetColumnUsingTabs( inString, 2 ) );
	EXPECT_EQ( "Col3", GetColumnUsingTabs( inString, 3 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 4 )  );
}

{
	std::string inString = "";
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 1 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 2 ) );
}

{
	std::string inString = " ";
	EXPECT_EQ( " ", GetColumnUsingTabs( inString, 1 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 2 ) );
}

{
	std::string inString = "\t";
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 1 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 2 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 3 ) );
}

{
	std::string inString = " \t ";
	EXPECT_EQ( " ", GetColumnUsingTabs( inString, 1 ) );
	EXPECT_EQ( " ", GetColumnUsingTabs( inString, 2 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 3 ) );
}

{
	std::string inString = "\tCol1\tCol2\tCol3\t";
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 1 ) );
	EXPECT_EQ( "Col1", GetColumnUsingTabs( inString, 2 ) );
	EXPECT_EQ( "Col2", GetColumnUsingTabs( inString, 3 ) );
	EXPECT_EQ( "Col3", GetColumnUsingTabs( inString, 4 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 5 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 6 ) );
}

{
	std::string inString = "Col1\t\tCol2\tCol3\t";
	EXPECT_EQ( "Col1", GetColumnUsingTabs( inString, 1 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 2 ) );
	EXPECT_EQ( "Col2", GetColumnUsingTabs( inString, 3 ) );
	EXPECT_EQ( "Col3", GetColumnUsingTabs( inString, 4 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 5 ) );
	EXPECT_EQ( "", GetColumnUsingTabs( inString, 6 ) );
}

}

TEST_F( EnergyPlusFixture, OutputReportTabularTest_AllocateLoadComponentArraysTest )
{
	ShowMessage("Begin Test: EnergyPlusFixture, OutputReportTabularTest_AllocateLoadComponentArraysTest");

	TotDesDays = 2;
	TotRunDesPersDays = 3;
	NumOfZones = 4;
	TotSurfaces = 7;
	NumOfTimeStepInHour = 4;

	AllocateLoadComponentArrays();

	// radiantPulseUsed.allocate( { 0, TotDesDays + TotRunDesPersDays }, NumOfZones );
	EXPECT_EQ( radiantPulseUsed.size(), 24u );

	// radiantPulseTimestep.allocate( { 0, TotDesDays + TotRunDesPersDays }, NumOfZones );
	EXPECT_EQ( radiantPulseTimestep.size(), 24u );

	// radiantPulseReceived.allocate( { 0, TotDesDays + TotRunDesPersDays }, TotSurfaces );
	EXPECT_EQ( radiantPulseReceived.size(), 42u );

	// loadConvectedNormal.allocate( TotDesDays + TotRunDesPersDays, { 0, NumOfTimeStepInHour * 24 }, TotSurfaces );
	EXPECT_EQ( loadConvectedNormal.size(), 3395u );

	// loadConvectedWithPulse.allocate( TotDesDays + TotRunDesPersDays, { 0, NumOfTimeStepInHour * 24 }, TotSurfaces );
	EXPECT_EQ( loadConvectedWithPulse.size(), 3395u );

	// netSurfRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
	EXPECT_EQ( netSurfRadSeq.size(), 3360u );

	// decayCurveCool.allocate( NumOfTimeStepInHour * 24, TotSurfaces );
	EXPECT_EQ( decayCurveCool.size(), 672u );

	// decayCurveHeat.allocate( NumOfTimeStepInHour * 24, TotSurfaces );
	EXPECT_EQ( decayCurveHeat.size(), 672u );

	// ITABSFseq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
	EXPECT_EQ( ITABSFseq.size(), 3360u );

	// TMULTseq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( TMULTseq.size(), 1920u );

	// peopleInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( peopleInstantSeq.size(), 1920u );

	// peopleLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( peopleLatentSeq.size(), 1920u );

	// peopleRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( peopleRadSeq.size(), 1920u );

	// peopleDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	// EXPECT_EQ( peopleDelaySeq.size(), 1920u );

	// lightInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( lightInstantSeq.size(), 1920u );

	// lightRetAirSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( lightRetAirSeq.size(), 1920u );

	// lightLWRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( lightLWRadSeq.size(), 1920u );

	// lightSWRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
	EXPECT_EQ( lightSWRadSeq.size(), 3360u );

	// lightDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	// EXPECT_EQ( lightDelaySeq.size(), 1920u );

	// equipInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( equipInstantSeq.size(), 1920u );

	// equipLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( equipLatentSeq.size(), 1920u );

	// equipRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( equipRadSeq.size(), 1920u );

	// equipDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	// EXPECT_EQ( equipDelaySeq.size(), 1920u );

	// refrigInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( refrigInstantSeq.size(), 1920u );

	// refrigRetAirSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( refrigRetAirSeq.size(), 1920u );

	// refrigLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( refrigLatentSeq.size(), 1920u );

	// waterUseInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( waterUseInstantSeq.size(), 1920u );

	// waterUseLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( waterUseLatentSeq.size(), 1920u );

	// hvacLossInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( hvacLossInstantSeq.size(), 1920u );

	// hvacLossRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( hvacLossRadSeq.size(), 1920u );

	// hvacLossDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	// EXPECT_EQ( hvacLossDelaySeq.size(), 1920u );

	// powerGenInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( powerGenInstantSeq.size(), 1920u );

	// powerGenRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( powerGenRadSeq.size(), 1920u );

	// powerGenDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	// EXPECT_EQ( powerGenDelaySeq.size(), 1920u );

	// infilInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( infilInstantSeq.size(), 1920u );

	// infilLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( infilLatentSeq.size(), 1920u );

	// zoneVentInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( zoneVentInstantSeq.size(), 1920u );

	// zoneVentLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( zoneVentLatentSeq.size(), 1920u );

	// interZoneMixInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( interZoneMixInstantSeq.size(), 1920u );

	// interZoneMixLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( interZoneMixLatentSeq.size(), 1920u );

	// feneCondInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( feneCondInstantSeq.size(), 1920u );

	// feneSolarRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
	EXPECT_EQ( feneSolarRadSeq.size(), 3360u );

	// feneSolarDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	// EXPECT_EQ( feneSolarDelaySeq.size(), 1920u );

	// surfDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
	// EXPECT_EQ( surfDelaySeq.size(), 3360u );

}

TEST( OutputReportTabularTest, ConfirmConvertToEscaped )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, ConfirmConvertToEscaped" );
	EXPECT_EQ( "", ConvertToEscaped( "" ) );
	EXPECT_EQ( " ", ConvertToEscaped( " " ) );
	EXPECT_EQ( "String with &gt; in it", ConvertToEscaped( "String with > in it" ) );
	EXPECT_EQ( "String with &lt; in it", ConvertToEscaped( "String with < in it" ) );
	EXPECT_EQ( "String with &amp; in it", ConvertToEscaped( "String with & in it" ) );
	EXPECT_EQ( "String with &quot; in it", ConvertToEscaped( "String with \" in it" ) );
	EXPECT_EQ( "String with &apos; in it", ConvertToEscaped( "String with \' in it" ) );
	EXPECT_EQ( "String with &quot; in it", ConvertToEscaped( R"(String with \" in it)" ) );
	EXPECT_EQ( "String with &apos; in it", ConvertToEscaped( R"(String with \' in it)" ) );
	EXPECT_EQ( "String with &deg; in it", ConvertToEscaped( std::string( "String with " ) +  char( 176 ) + std::string( " in it" ) ) );
	EXPECT_EQ( "String with &deg; in it", ConvertToEscaped( "String with \u00B0 in it" ) );
	EXPECT_EQ( "String with &deg; in it", ConvertToEscaped( "String with \xB0 in it" ) );
	EXPECT_EQ( "String with &deg; in it", ConvertToEscaped( "String with \xC2\xB0 in it" ) );
	EXPECT_EQ( "String with \xC2 in it", ConvertToEscaped( "String with \xC2 in it" ) );
	EXPECT_EQ( "String with \xC2\xB1 in it", ConvertToEscaped( "String with \xC2\xB1 in it" ) );
	EXPECT_EQ( "String with &deg; in it", ConvertToEscaped( R"(String with \u00B0 in it)" ) );
	EXPECT_EQ( "String with &deg; in it", ConvertToEscaped( R"(String with \xB0 in it)" ) );
	EXPECT_ANY_THROW( ConvertToEscaped( R"(String with \u in it)" ) );
	EXPECT_ANY_THROW( ConvertToEscaped( R"(String with \x in it)" ) );
}

TEST( OutputReportTabularTest, ConvertUnicodeToUTF8 )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, ConvertUnicodeToUTF8" );

	{
		std::string test;
		test += static_cast<char>( 0 );
		EXPECT_EQ( test, ConvertUnicodeToUTF8( std::stoul( "0x0000", nullptr, 16 ) ) );
	}
	EXPECT_EQ( "\x7F", ConvertUnicodeToUTF8( std::stoul( "0x7F", nullptr, 16 ) ) );
	EXPECT_EQ( "\xC2\xB0", ConvertUnicodeToUTF8( std::stoul( "0xB0", nullptr, 16 ) ) );
	EXPECT_EQ( "\xC2\xB0", ConvertUnicodeToUTF8( std::stoul( "0x00B0", nullptr, 16 ) ) );
	EXPECT_EQ( "\xEF\xBF\xBF", ConvertUnicodeToUTF8( std::stoul( "0xFFFF", nullptr, 16 ) ) );
	EXPECT_EQ( "\xF4\x8F\xBF\xBF", ConvertUnicodeToUTF8( std::stoul( "0x10FFFF", nullptr, 16 ) ) );
	EXPECT_EQ( "", ConvertUnicodeToUTF8( std::stoul( "0x110000", nullptr, 16 ) ) );
	EXPECT_EQ( "", ConvertUnicodeToUTF8( std::stoul( "0x1FFFFF", nullptr, 16 ) ) );
}


TEST( OutputReportTabularTest, GetUnitSubStringTest )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, GetUnitSubStringTest" );
	EXPECT_EQ( "", GetUnitSubString( "" ) );
	EXPECT_EQ( "", GetUnitSubString( " " ) );
	EXPECT_EQ( "J/KG", GetUnitSubString( "[J/KG]" ) );
	EXPECT_EQ( "M3/S-PERSON", GetUnitSubString( " [M3/S-PERSON]" ) ); //leading space
	EXPECT_EQ( "W/M2-K", GetUnitSubString( "[W/M2-K] " ) ); // trailing space
	EXPECT_EQ( "MJ/m2", GetUnitSubString( " [MJ/m2] " ) ); // leading and trailing space
	EXPECT_EQ( "PA", GetUnitSubString( "This is a column header with units [PA] " ) );
	EXPECT_EQ( "W", GetUnitSubString( "This is a column header with units [W] " ) );
	EXPECT_EQ( "K/M", GetUnitSubString( "This is a column header with units [K/M] and trailing text." ) );
}


TEST_F( EnergyPlusFixture, OutputReportTabular_ZoneMultiplierTest )
{
	// AUTHOR: R. Raustad, FSEC
	// DATE WRITTEN: Sep 2015

	std::string const idf_objects = delimited_string( {
		"Version,8.3;",

		" Output:Diagnostics, DisplayExtraWarnings;",
		"  Timestep, 4;",

		"BUILDING, OutputReportTabular_ZoneMultiplierTest, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",

		"SimulationControl, YES, NO, NO, YES, NO;",

		"  Site:Location,",
		"    Miami Intl Ap FL USA TMY3 WMO=722020E,    !- Name",
		"    25.82,                   !- Latitude {deg}",
		"    -80.30,                  !- Longitude {deg}",
		"    -5.00,                   !- Time Zone {hr}",
		"    11;                      !- Elevation {m}",

		"SizingPeriod:DesignDay,",
		" Miami Intl Ap Ann Clg 1% Condns DB/MCWB, !- Name",
		" 7,                        !- Month",
		" 21,                       !- Day of Month",
		" SummerDesignDay,          !- Day Type",
		" 31.7,                     !- Maximum Dry - Bulb Temperature{ C }",
		" 10.0,                      !- Daily Dry - Bulb Temperature Range{ deltaC }",
		" ,                         !- Dry - Bulb Temperature Range Modifier Type",
		" ,                         !- Dry - Bulb Temperature Range Modifier Day Schedule Name",
		" Wetbulb,                  !- Humidity Condition Type",
		" 22.7,                     !- Wetbulb or DewPoint at Maximum Dry - Bulb{ C }",
		" ,                         !- Humidity Condition Day Schedule Name",
		" ,                         !- Humidity Ratio at Maximum Dry - Bulb{ kgWater / kgDryAir }",
		" ,                         !- Enthalpy at Maximum Dry - Bulb{ J / kg }",
		" ,                         !- Daily Wet - Bulb Temperature Range{ deltaC }",
		" 101217.,                  !- Barometric Pressure{ Pa }",
		" 3.8,                      !- Wind Speed{ m / s }",
		" 340,                      !- Wind Direction{ deg }",
		" No,                       !- Rain Indicator",
		" No,                       !- Snow Indicator",
		" No,                       !- Daylight Saving Time Indicator",
		" ASHRAEClearSky,           !- Solar Model Indicator",
		" ,                         !- Beam Solar Day Schedule Name",
		" ,                         !- Diffuse Solar Day Schedule Name",
		" ,                         !- ASHRAE Clear Sky Optical Depth for Beam Irradiance( taub ) { dimensionless }",
		" ,                         !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance( taud ) { dimensionless }",
		" 1.00;                     !- Sky Clearness",

		"SizingPeriod:DesignDay,",
		" Miami Intl Ap Ann Htg 99.6% Condns DB, !- Name",
		" 1,                        !- Month",
		" 21,                       !- Day of Month",
		" WinterDesignDay,          !- Day Type",
		" 8.7,                      !- Maximum Dry - Bulb Temperature{ C }",
		" 0.0,                      !- Daily Dry - Bulb Temperature Range{ deltaC }",
		" ,                         !- Dry - Bulb Temperature Range Modifier Type",
		" ,                         !- Dry - Bulb Temperature Range Modifier Day Schedule Name",
		" Wetbulb,                  !- Humidity Condition Type",
		" 8.7,                      !- Wetbulb or DewPoint at Maximum Dry - Bulb{ C }",
		" ,                         !- Humidity Condition Day Schedule Name",
		" ,                         !- Humidity Ratio at Maximum Dry - Bulb{ kgWater / kgDryAir }",
		" ,                         !- Enthalpy at Maximum Dry - Bulb{ J / kg }",
		" ,                         !- Daily Wet - Bulb Temperature Range{ deltaC }",
		" 101217.,                  !- Barometric Pressure{ Pa }",
		" 3.8,                      !- Wind Speed{ m / s }",
		" 340,                      !- Wind Direction{ deg }",
		" No,                       !- Rain Indicator",
		" No,                       !- Snow Indicator",
		" No,                       !- Daylight Saving Time Indicator",
		" ASHRAEClearSky,           !- Solar Model Indicator",
		" ,                         !- Beam Solar Day Schedule Name",
		" ,                         !- Diffuse Solar Day Schedule Name",
		" ,                         !- ASHRAE Clear Sky Optical Depth for Beam Irradiance( taub ) { dimensionless }",
		" ,                         !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance( taud ) { dimensionless }",
		" 0.00;                     !- Sky Clearness",

		"OutputControl:Table:Style,",
		"  HTML;                    !- Column Separator",
		" ",
		"Output:Table:SummaryReports,",
		"  AllSummary; !- Report 1 Name",
		" ",
		"Zone,",
		"  Space,                   !- Name",
		"  0.0000,                  !- Direction of Relative North {deg}",
		"  0.0000,                  !- X Origin {m}",
		"  0.0000,                  !- Y Origin {m}",
		"  0.0000,                  !- Z Origin {m}",
		"  1,                       !- Type",
		"  1,                       !- Multiplier",
		"  2.4,                     !- Ceiling Height {m}",
		"  ,                        !- Volume {m3}",
		"  autocalculate,           !- Floor Area {m2}",
		"  ,                        !- Zone Inside Convection Algorithm",
		"  ,                        !- Zone Outside Convection Algorithm",
		"  Yes;                     !- Part of Total Floor Area",
		" ",
		"ZoneGroup,",
		" Zone Group,               !- Name",
		" Zone List,                !- Zone List Name",
		" 10;                       !- Zone List Multiplier",
		" ",
		"ZoneList,",
		" Zone List,                !- Name",
		" Spacex10;                 !- Zone 1 Name",
		" ",
		"Zone,",
		"  Spacex10,                !- Name",
		"  0.0000,                  !- Direction of Relative North {deg}",
		"  0.0000,                  !- X Origin {m}",
		"  0.0000,                  !- Y Origin {m}",
		"  0.0000,                  !- Z Origin {m}",
		"  1,                       !- Type",
		"  1,                       !- Multiplier",
		"  2.4,                     !- Ceiling Height {m}",
		"  ,                        !- Volume {m3}",
		"  autocalculate,           !- Floor Area {m2}",
		"  ,                        !- Zone Inside Convection Algorithm",
		"  ,                        !- Zone Outside Convection Algorithm",
		"  Yes;                     !- Part of Total Floor Area",
		" ",
		"Sizing:Zone,",
		" Space,                    !- Zone or ZoneList Name",
		" SupplyAirTemperature,     !- Zone Cooling Design Supply Air Temperature Input Method",
		" 12.,                      !- Zone Cooling Design Supply Air Temperature{ C }",
		" ,                         !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
		" SupplyAirTemperature,     !- Zone Heating Design Supply Air Temperature Input Method",
		" 50.,                      !- Zone Heating Design Supply Air Temperature{ C }",
		" ,                         !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
		" 0.008,                    !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
		" 0.008,                    !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
		" SZ DSOA,                  !- Design Specification Outdoor Air Object Name",
		" 0.0,                      !- Zone Heating Sizing Factor",
		" 0.0,                      !- Zone Cooling Sizing Factor",
		" DesignDay,                !- Cooling Design Air Flow Method",
		" 0,                        !- Cooling Design Air Flow Rate{ m3 / s }",
		" ,                         !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
		" ,                         !- Cooling Minimum Air Flow{ m3 / s }",
		" ,                         !- Cooling Minimum Air Flow Fraction",
		" DesignDay,                !- Heating Design Air Flow Method",
		" 0,                        !- Heating Design Air Flow Rate{ m3 / s }",
		" ,                         !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
		" ,                         !- Heating Maximum Air Flow{ m3 / s }",
		" ;                         !- Heating Maximum Air Flow Fraction",
		" ",
		"Sizing:Zone,",
		" Spacex10,                 !- Zone or ZoneList Name",
		" SupplyAirTemperature,     !- Zone Cooling Design Supply Air Temperature Input Method",
		" 12.,                      !- Zone Cooling Design Supply Air Temperature{ C }",
		" ,                         !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
		" SupplyAirTemperature,     !- Zone Heating Design Supply Air Temperature Input Method",
		" 50.,                      !- Zone Heating Design Supply Air Temperature{ C }",
		" ,                         !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
		" 0.008,                    !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
		" 0.008,                    !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
		" SZ DSOA,                  !- Design Specification Outdoor Air Object Name",
		" 0.0,                      !- Zone Heating Sizing Factor",
		" 0.0,                      !- Zone Cooling Sizing Factor",
		" DesignDay,                !- Cooling Design Air Flow Method",
		" 0,                        !- Cooling Design Air Flow Rate{ m3 / s }",
		" ,                         !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
		" ,                         !- Cooling Minimum Air Flow{ m3 / s }",
		" ,                         !- Cooling Minimum Air Flow Fraction",
		" DesignDay,                !- Heating Design Air Flow Method",
		" 0,                        !- Heating Design Air Flow Rate{ m3 / s }",
		" ,                         !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
		" ,                         !- Heating Maximum Air Flow{ m3 / s }",
		" ;                         !- Heating Maximum Air Flow Fraction",
		" ",
		"DesignSpecification:OutdoorAir,",
		" SZ DSOA,                  !- Name",
		" flow/person,              !- Outdoor Air Method",
		" 0.00944,                  !- Outdoor Air Flow per Person{ m3 / s - person }",
		" 0.0,                      !- Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }",
		" 0.0;                      !- Outdoor Air Flow per Zone{ m3 / s }",
		" ",
		"ZoneHVAC:EquipmentConnections,",
		" Space,                    !- Zone Name",
		" Space Eq,                 !- Zone Conditioning Equipment List Name",
		" Space In Node,            !- Zone Air Inlet Node or NodeList Name",
		" Space Out Node,           !- Zone Air Exhaust Node or NodeList Name",
		" Space Node,               !- Zone Air Node Name",
		" Space Ret Node;           !- Zone Return Air Node Name",
		" ",
		"ZoneHVAC:EquipmentConnections,",
		" Spacex10,                 !- Zone Name",
		" Spacex10 Eq,              !- Zone Conditioning Equipment List Name",
		" Spacex10 In Node,         !- Zone Air Inlet Node or NodeList Name",
		" Spacex10 Out Node,        !- Zone Air Exhaust Node or NodeList Name",
		" Spacex10 Node,            !- Zone Air Node Name",
		" Spacex10 Ret Node;        !- Zone Return Air Node Name",
		" ",
		"ZoneHVAC:EquipmentList,",
		" Space Eq,                 !- Name",
		" ZoneHVAC:WindowAirConditioner, !- Zone Equipment 1 Object Type",
		" WindAC,                   !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",
		" ",
		"ZoneHVAC:EquipmentList,",
		" Spacex10 Eq,              !- Name",
		" ZoneHVAC:WindowAirConditioner, !- Zone Equipment 1 Object Type",
		" WindACx10,                !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",
		" ",
		"ZoneHVAC:WindowAirConditioner,",
		" WindAC,                   !- Name",
		" AvailSched,               !- Availability Schedule Name",
		" autosize,                 !- Maximum Supply Air Flow Rate{ m3 / s }",
		" autosize,                 !- Maximum Outdoor Air Flow Rate{ m3 / s }",
		" Space Out Node,           !- Air Inlet Node Name",
		" Space In Node,            !- Air Outlet Node Name",
		" OutdoorAir:Mixer,         !- Outdoor Air Mixer Object Type",
		" WindACOAMixer,            !- Outdoor Air Mixer Name",
		" Fan:OnOff,                !- Supply Air Fan Object Type",
		" WindACFan,                !- Supply Air Fan Name",
		" Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
		" WindACDXCoil,             !- DX Cooling Coil Name",
		" FanOpModeSchedule,        !- Supply Air Fan Operating Mode Schedule Name",
		" BlowThrough,              !- Fan Placement",
		" 0.001;                    !- Cooling Convergence Tolerance",
		" ",
		"ZoneHVAC:WindowAirConditioner,",
		" WindACx10,                !- Name",
		" AvailSched,               !- Availability Schedule Name",
		" autosize,                 !- Maximum Supply Air Flow Rate{ m3 / s }",
		" autosize,                 !- Maximum Outdoor Air Flow Rate{ m3 / s }",
		" Spacex10 Out Node,        !- Air Inlet Node Name",
		" Spacex10 In Node,         !- Air Outlet Node Name",
		" OutdoorAir:Mixer,         !- Outdoor Air Mixer Object Type",
		" WindACx10OAMixer,         !- Outdoor Air Mixer Name",
		" Fan:OnOff,                !- Supply Air Fan Object Type",
		" WindACx10Fan,             !- Supply Air Fan Name",
		" Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
		" WindACx10DXCoil,          !- DX Cooling Coil Name",
		" FanOpModeSchedule,        !- Supply Air Fan Operating Mode Schedule Name",
		" BlowThrough,              !- Fan Placement",
		" 0.001;                    !- Cooling Convergence Tolerance",
		" ",
		"OutdoorAir:Mixer,",
		" WindACOAMixer,            !- Name",
		" WindACOAMixerOutletNode,  !- Mixed Air Node Name",
		" WindACOAInNode,           !- Outdoor Air Stream Node Name",
		" WindACExhNode,            !- Relief Air Stream Node Name",
		" Space Out Node;           !- Return Air Stream Node Name",
		" ",
		"OutdoorAir:Mixer,",
		" WindACx10OAMixer,         !- Name",
		" WindACx10OAMixerOutletNode, !- Mixed Air Node Name",
		" WindACx10OAInNode,        !- Outdoor Air Stream Node Name",
		" WindACx10ExhNode,         !- Relief Air Stream Node Name",
		" Spacex10 Out Node;        !- Return Air Stream Node Name",
		" ",
		"Fan:OnOff,",
		" WindACFan,                !- Name",
		" AvailSched,               !- Availability Schedule Name",
		" 0.5,                      !- Fan Total Efficiency",
		" 75.0,                     !- Pressure Rise{ Pa }",
		" autosize,                 !- Maximum Flow Rate{ m3 / s }",
		" 0.9,                      !- Motor Efficiency",
		" 1.0,                      !- Motor In Airstream Fraction",
		" WindACOAMixerOutletNode,  !- Air Inlet Node Name",
		" WindACFanOutletNode;      !- Air Outlet Node Name",
		" ",
		"Fan:OnOff,",
		" WindACx10Fan,             !- Name",
		" AvailSched,               !- Availability Schedule Name",
		" 0.5,                      !- Fan Total Efficiency",
		" 75.0,                     !- Pressure Rise{ Pa }",
		" autosize,                 !- Maximum Flow Rate{ m3 / s }",
		" 0.9,                      !- Motor Efficiency",
		" 1.0,                      !- Motor In Airstream Fraction",
		" WindACx10OAMixerOutletNode, !- Air Inlet Node Name",
		" WindACx10FanOutletNode;   !- Air Outlet Node Name",
		" ",
		"Coil:Cooling:DX:SingleSpeed,",
		" WindACDXCoil,             !- Name",
		" AvailSched,               !- Availability Schedule Name",
		" autosize,                 !- Gross Rated Total Cooling Capacity{ W }",
		" autosize,                 !- Gross Rated Sensible Heat Ratio",
		" 3.0,                      !- Gross Rated Cooling COP{ W / W }",
		" autosize,                 !- Rated Air Flow Rate{ m3 / s }",
		" ,                         !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
		" WindACFanOutletNode,      !- Air Inlet Node Name",
		" Space In Node,            !- Air Outlet Node Name",
		" Biquadratic,              !- Total Cooling Capacity Function of Temperature Curve Name",
		" Cubic,                    !- Total Cooling Capacity Function of Flow Fraction Curve Name",
		" Biquadratic,              !- Energy Input Ratio Function of Temperature Curve Name",
		" Cubic,                    !- Energy Input Ratio Function of Flow Fraction Curve Name",
		" Cubic;                    !- Part Load Fraction Correlation Curve Name",
		" ",
		"Coil:Cooling:DX:SingleSpeed,",
		" WindACx10DXCoil,          !- Name",
		" AvailSched,               !- Availability Schedule Name",
		" autosize,                 !- Gross Rated Total Cooling Capacity{ W }",
		" autosize,                 !- Gross Rated Sensible Heat Ratio",
		" 3.0,                      !- Gross Rated Cooling COP{ W / W }",
		" autosize,                 !- Rated Air Flow Rate{ m3 / s }",
		" ,                         !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
		" WindACx10FanOutletNode,   !- Air Inlet Node Name",
		" Spacex10 In Node,         !- Air Outlet Node Name",
		" Biquadratic,              !- Total Cooling Capacity Function of Temperature Curve Name",
		" Cubic,                    !- Total Cooling Capacity Function of Flow Fraction Curve Name",
		" Biquadratic,              !- Energy Input Ratio Function of Temperature Curve Name",
		" Cubic,                    !- Energy Input Ratio Function of Flow Fraction Curve Name",
		" Cubic;                    !- Part Load Fraction Correlation Curve Name",
		" ",
		"People,",
		" Space People,             !- Name",
		" Space,                    !- Zone or ZoneList Name",
		" OnSched,                  !- Number of People Schedule Name",
		" people,                   !- Number of People Calculation Method",
		" 11,                       !- Number of People",
		" ,                         !- People per Zone Floor Area{ person / m2 }",
		" ,                         !- Zone Floor Area per Person{ m2 / person }",
		" 0.3,                      !- Fraction Radiant",
		" AutoCalculate,            !- Sensible Heat Fraction",
		" OnSched;                  !- Activity Level Schedule Name",
		" ",
		"People,",
		" Spacex10 People,          !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
		" OnSched,                  !- Number of People Schedule Name",
		" people,                   !- Number of People Calculation Method",
		" 11,                       !- Number of People",
		" ,                         !- People per Zone Floor Area{ person / m2 }",
		" ,                         !- Zone Floor Area per Person{ m2 / person }",
		" 0.3,                      !- Fraction Radiant",
		" AutoCalculate,            !- Sensible Heat Fraction",
		" OnSched;                  !- Activity Level Schedule Name",
		" ",
		"Lights,",
		" Space Lights,             !- Name",
		" Space,                    !- Zone or ZoneList Name",
		" OnSched,                  !- Schedule Name",
		" Watts/Area,               !- Design Level Calculation Method",
		" ,                         !- Lighting Level{ W }",
		" 10.0,                     !- Watts per Zone Floor Area{ W / m2 }",
		" ,                         !- Watts per Person{ W / person }",
		" 0.1,                      !- Return Air Fraction",
		" 0.59,                     !- Fraction Radiant",
		" 0.2,                      !- Fraction Visible",
		" 0,                        !- Fraction Replaceable",
		" GeneralLights;            !- End - Use Subcategory",
		" ",
		"Lights,",
		" Space Lights x10,         !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
		" OnSched,                  !- Schedule Name",
		" Watts/Area,               !- Design Level Calculation Method",
		" ,                         !- Lighting Level{ W }",
		" 10.0,                     !- Watts per Zone Floor Area{ W / m2 }",
		" ,                         !- Watts per Person{ W / person }",
		" 0.1,                      !- Return Air Fraction",
		" 0.59,                     !- Fraction Radiant",
		" 0.2,                      !- Fraction Visible",
		" 0,                        !- Fraction Replaceable",
		" GeneralLights;            !- End - Use Subcategory",
		" ",
		"ElectricEquipment,",
		" Space ElecEq,             !- Name",
		" Space,                    !- Zone or ZoneList Name",
		" OnSched,                  !- Schedule Name",
		" Watts/Area,               !- Design Level Calculation Method",
		" ,                         !- Design Level{ W }",
		" 20.0,                     !- Watts per Zone Floor Area{ W / m2 }",
		" ,                         !- Watts per Person{ W / person }",
		" 0.1,                      !- Fraction Latent",
		" 0.3,                      !- Fraction Radiant",
		" 0.1;                      !- Fraction Lost",
		" ",
		"ElectricEquipment,",
		" Space ElecEq x10,         !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
		" OnSched,                  !- Schedule Name",
		" Watts/Area,               !- Design Level Calculation Method",
		" ,                         !- Design Level{ W }",
		" 20.0,                     !- Watts per Zone Floor Area{ W / m2 }",
		" ,                         !- Watts per Person{ W / person }",
		" 0.1,                      !- Fraction Latent",
		" 0.3,                      !- Fraction Radiant",
		" 0.1;                      !- Fraction Lost",
		" ",
		"Schedule:Compact,",
		" OnSched,                  !- Name",
		" Fraction,                 !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0;        !- Field 26",
		" ",
		"ScheduleTypeLimits,",
		" Fraction, !- Name",
		" 0.0, !- Lower Limit Value",
		" 1.0, !- Upper Limit Value",
		" CONTINUOUS;              !- Numeric Type",
		" ",
		"Construction,",
		" INT-WALL-1,               !- Name",
		" GP02,                     !- Outside Layer",
		" AL21,                     !- Layer 2",
		" GP02;                     !- Layer 3",
		" ",
		"Material,",
		" GP02,                     !- Name",
		" MediumSmooth,             !- Roughness",
		" 1.5900001E-02,            !- Thickness{ m }",
		" 0.1600000,                !- Conductivity{ W / m - K }",
		" 801.0000,                 !- Density{ kg / m3 }",
		" 837.0000,                 !- Specific Heat{ J / kg - K }",
		" 0.9000000,                !- Thermal Absorptance",
		" 0.7500000,                !- Solar Absorptance",
		" 0.7500000;                !- Visible Absorptance",
		" ",
		"Material:AirGap,",
		" AL21,                     !- Name",
		" 0.1570000;                !- Thermal Resistance{ m2 - K / W }",
		" ",
		"Construction,",
		"FLOOR-SLAB-1,              !- Name",
		"CC03,                      !- Outside Layer",
		"CP01;                      !- Layer 2",
		" ",
		"Material,",
		" CC03,                     !- Name",
		" MediumRough,              !- Roughness",
		" 0.1016000,                !- Thickness{ m }",
		" 1.310000,                 !- Conductivity{ W / m - K }",
		" 2243.000,                 !- Density{ kg / m3 }",
		" 837.0000,                 !- Specific Heat{ J / kg - K }",
		" 0.9000000,                !- Thermal Absorptance",
		" 0.6500000,                !- Solar Absorptance",
		" 0.6500000;                !- Visible Absorptance",
		" ",
		"Material:NoMass,",
		" CP01,                     !- Name",
		" Rough,                    !- Roughness",
		" 0.3670000,                !- Thermal Resistance{ m2 - K / W }",
		" 0.9000000,                !- Thermal Absorptance",
		" 0.7500000,                !- Solar Absorptance",
		" 0.7500000;                !- Visible Absorptance",
		" ",
		"Construction,",
		" CLNG-1,                   !- Name",
		" MAT-CLNG-1;               !- Outside Layer",
		" ",
		"Material:NoMass,",
		" MAT-CLNG-1,               !- Name",
		" Rough,                    !- Roughness",
		" 0.652259290,              !- Thermal Resistance{ m2 - K / W }",
		" 0.65,                     !- Thermal Absorptance",
		" 0.65,                     !- Solar Absorptance",
		" 0.65;                     !- Visible Absorptance",
		" ",
		"BuildingSurface:Detailed,",
		" FRONT-1,                  !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Space,                    !- Zone Name",
		" Outdoors,                 !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" SunExposed,               !- Sun Exposure",
		" WindExposed,              !- Wind Exposure",
		" 0.50000,                  !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
		" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
		" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
		" 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" C1-1,                     !- Name",
		" CEILING,                  !- Surface Type",
		" CLNG-1,                   !- Construction Name",
		" Space,                    !- Zone Name",
		" Outdoors,                 !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
		" 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 2 {m}",
		" 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 3 {m}",
		" 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" F1-1,                     !- Name",
		" FLOOR,                    !- Surface Type",
		" FLOOR-SLAB-1,             !- Construction Name",
		" Space,                    !- Zone Name",
		" Ground,                   !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 1 {m}",
		" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
		" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
		" 3.7, 3.7, 0.0;            !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" SB12,                     !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Space,                    !- Zone Name",
		" Adiabatic,                !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
		" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
		" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
		" 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" SB14,                     !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Space,                    !- Zone Name",
		" Adiabatic,                !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
		" 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
		" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
		" 0.0, 0.0, 2.4;            !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" SB15,                     !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Space,                    !- Zone Name",
		" Adiabatic,                !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 26.8, 3.7, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
		" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
		" 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
		" 3.7, 3.7, 2.4;            !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"ZoneControl:Thermostat,",
		" Space Thermostat,         !- Name",
		" Space,                    !- Zone or ZoneList Name",
		" Dual Zone Control Type Sched,  !- Control Type Schedule Name",
		" ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
		" Space DualSPSched;        !- Control 1 Name",
		" ",
		"ZoneControl:Thermostat,",
		" Spacex10 Thermostat,      !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
		" Dual Zone Control Type Sched,  !- Control Type Schedule Name",
		" ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
		" Space DualSPSched;        !- Control 1 Name",
		" ",
		"Schedule:Compact,",
		" Dual Zone Control Type Sched,  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00,4;           !- Field 3",
		" ",
		"ThermostatSetpoint:DualSetpoint,",
		" Space DualSPSched,        !- Name",
		" HTGSETP_SCH,              !- Heating Setpoint Temperature Schedule Name",
		" CLGSETP_SCH;              !- Cooling Setpoint Temperature Schedule Name",
		" ",
		"Schedule:Compact,",
		" CLGSETP_SCH,              !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 19",
		" Until: 24:00,24.0;        !- Field 20",
		" ",
		"Schedule:Compact,",
		" HTGSETP_SCH,              !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 22",
		" Until: 24:00, 20.0;       !- Field 23",
		" ",
		"BuildingSurface:Detailed,",
		" FRONT-1x10,               !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Spacex10,                 !- Zone Name",
		" Outdoors,                 !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" SunExposed,               !- Sun Exposure",
		" WindExposed,              !- Wind Exposure",
		" 0.50000,                  !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
		" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
		" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
		" 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" C1-1x10,                  !- Name",
		" CEILING,                  !- Surface Type",
		" CLNG-1,                   !- Construction Name",
		" Spacex10,                 !- Zone Name",
		" Outdoors,                 !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
		" 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 2 {m}",
		" 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 3 {m}",
		" 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" F1-1x10,                  !- Name",
		" FLOOR,                    !- Surface Type",
		" FLOOR-SLAB-1,             !- Construction Name",
		" Spacex10,                 !- Zone Name",
		" Ground,                   !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 1 {m}",
		" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
		" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
		" 3.7, 3.7, 0.0;            !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" SB12x10,                  !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Spacex10,                 !- Zone Name",
		" Adiabatic,                !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
		" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
		" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
		" 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" SB14x10,                  !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Spacex10,                 !- Zone Name",
		" Adiabatic,                !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
		" 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
		" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
		" 0.0, 0.0, 2.4;            !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"BuildingSurface:Detailed,",
		" SB15x10,                  !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Spacex10,                 !- Zone Name",
		" Adiabatic,                !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" NoSun,                    !- Sun Exposure",
		" NoWind,                   !- Wind Exposure",
		" 0.0,                      !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 26.8, 3.7, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
		" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
		" 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
		" 3.7, 3.7, 2.4;            !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"OutdoorAir:NodeList,",
		"  OutsideAirInletNodes;    !- Node or NodeList Name 1",
		" ",
		"NodeList,",
		"  OutsideAirInletNodes,    !- Name",
		"  WindACOAInNode,          !- Node 1 Name",
		"  WindACx10OAInNode;       !- Node 1 Name",
		" ",
		"ScheduleTypeLimits,",
		"  Any Number;              !- Name",
		" ",
		"Schedule:Compact,",
		"  AvailSched,              !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 3",
		"  For: AllDays,            !- Field 4",
		"  Until: 24:00,1.0;        !- Field 5",
		" ",
		"Schedule:Compact,",
		"  FanOpModeSchedule,       !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 1",
		"  For: AllDays,            !- Field 2",
		"  Until: 24:00,1.0;        !- Field 7",
		" ",
		"Curve:Biquadratic,",
		"  Biquadratic,             !- Name",
		"  1.0,                     !- Coefficient1 Constant",
		"  0.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.0,                     !- Coefficient4 y",
		"  0.0,                     !- Coefficient5 y**2",
		"  0.0,                     !- Coefficient6 x*y",
		"  5,                       !- Minimum Value of x",
		"  40,                      !- Maximum Value of x",
		"  -5,                      !- Minimum Value of y",
		"  30,                      !- Maximum Value of y",
		"  ,                        !- Minimum Curve Output",
		"  ,                        !- Maximum Curve Output",
		"  Temperature,             !- Input Unit Type for X",
		"  Temperature,             !- Input Unit Type for Y",
		"  Dimensionless;           !- Output Unit Type",
		" ",
		"Curve:Cubic,",
		"  Cubic,                   !- Name",
		"  1.0,                     !- Coefficient1 Constant",
		"  0.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0,                       !- Coefficient4 x**3",
		"  11,                      !- Minimum Value of x",
		"  30,                      !- Maximum Value of x",
		"  ,                        !- Minimum Curve Output",
		"  ,                        !- Maximum Curve Output",
		"  Temperature,             !- Input Unit Type for X",
		"  Temperature;             !- Output Unit Type",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	OutputProcessor::TimeValue.allocate( 2 );

	ManageSimulation(); // run the design day over the warmup period (24 hrs, 25 days)

	EXPECT_EQ( 10.0, ( Zone( 2 ).Volume * Zone( 2 ).Multiplier * Zone( 2 ).ListMultiplier ) / ( Zone( 1 ).Volume * Zone( 1 ).Multiplier * Zone( 1 ).ListMultiplier ) );
	// leaving a little wiggle room on these
	EXPECT_NEAR( 10.0, ( DXCoils::DXCoil( 2 ).RatedTotCap( 1 ) / DXCoils::DXCoil( 1 ).RatedTotCap( 1 ) ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DXCoils::DXCoil( 2 ).RatedAirVolFlowRate( 1 ) / DXCoils::DXCoil( 1 ).RatedAirVolFlowRate( 1 ) ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataZoneEnergyDemands::ZoneSysEnergyDemand( 2 ).TotalOutputRequired / DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).TotalOutputRequired ), 0.00001 );

	DataGlobals::DoWeathSim = true; // flag to trick tabular reports to scan meters
	DataGlobals::KindOfSim = DataGlobals::ksRunPeriodWeather; // fake a weather run since a weather file can't be used (could it?)
	UpdateTabularReports( OutputReportTabular::stepTypeHVAC );

	//zone equipment should report single zone magnitude, multipliers do not apply, should be > 0 or what's the point
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleRadGain, DataHeatBalance::ZnRpt( 2 ).PeopleRadGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleConGain, DataHeatBalance::ZnRpt( 2 ).PeopleConGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleSenGain, DataHeatBalance::ZnRpt( 2 ).PeopleSenGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleNumOcc, DataHeatBalance::ZnRpt( 2 ).PeopleNumOcc );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleLatGain, DataHeatBalance::ZnRpt( 2 ).PeopleLatGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleTotGain, DataHeatBalance::ZnRpt( 2 ).PeopleTotGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleRadGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleRadGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleConGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleConGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleSenGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleSenGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleLatGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleLatGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleTotGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleTotGainRate );

	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsPower, DataHeatBalance::ZnRpt( 2 ).LtsPower );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsElecConsump, DataHeatBalance::ZnRpt( 2 ).LtsElecConsump );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsRadGain, DataHeatBalance::ZnRpt( 2 ).LtsRadGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsVisGain, DataHeatBalance::ZnRpt( 2 ).LtsVisGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsConGain, DataHeatBalance::ZnRpt( 2 ).LtsConGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsRetAirGain, DataHeatBalance::ZnRpt( 2 ).LtsRetAirGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsTotGain, DataHeatBalance::ZnRpt( 2 ).LtsTotGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsRadGainRate, DataHeatBalance::ZnRpt( 2 ).LtsRadGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsVisGainRate, DataHeatBalance::ZnRpt( 2 ).LtsVisGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsConGainRate, DataHeatBalance::ZnRpt( 2 ).LtsConGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsRetAirGainRate, DataHeatBalance::ZnRpt( 2 ).LtsRetAirGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsTotGainRate, DataHeatBalance::ZnRpt( 2 ).LtsTotGainRate );

	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecPower, DataHeatBalance::ZnRpt( 2 ).ElecPower );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecConsump, DataHeatBalance::ZnRpt( 2 ).ElecConsump );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecRadGain, DataHeatBalance::ZnRpt( 2 ).ElecRadGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecConGain, DataHeatBalance::ZnRpt( 2 ).ElecConGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecLatGain, DataHeatBalance::ZnRpt( 2 ).ElecLatGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecLost, DataHeatBalance::ZnRpt( 2 ).ElecLost );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecTotGain, DataHeatBalance::ZnRpt( 2 ).ElecTotGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecRadGainRate, DataHeatBalance::ZnRpt( 2 ).ElecRadGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecConGainRate, DataHeatBalance::ZnRpt( 2 ).ElecConGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecLatGainRate, DataHeatBalance::ZnRpt( 2 ).ElecLatGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecLostRate, DataHeatBalance::ZnRpt( 2 ).ElecLostRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecTotGainRate, DataHeatBalance::ZnRpt( 2 ).ElecTotGainRate );

	//expect occupancy time data to be equal
	EXPECT_EQ( DataHeatBalance::ZonePreDefRep( 1 ).NumOccAccumTime, DataHeatBalance::ZonePreDefRep( 2 ).NumOccAccumTime );
	EXPECT_EQ( DataHeatBalance::ZonePreDefRep( 1 ).TotTimeOcc, DataHeatBalance::ZonePreDefRep( 2 ).TotTimeOcc );

	// occupancy is reported on a zone basis without multipliers (until this changes, expect results to be equal)
	EXPECT_EQ( DataHeatBalance::ZonePreDefRep( 1 ).NumOccAccum, DataHeatBalance::ZonePreDefRep( 2 ).NumOccAccum );

	//expect energy to report according to multipliers
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).MechVentVolTotal / DataHeatBalance::ZonePreDefRep( 1 ).MechVentVolTotal ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).MechVentVolMin / DataHeatBalance::ZonePreDefRep( 1 ).MechVentVolMin ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnHvacCl / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnHvacCl ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnPeoplAdd / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnPeoplAdd ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnLiteAdd / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnLiteAdd ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnEquipAdd / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnEquipAdd ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnOtherRem / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnOtherRem ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).clPeak / DataHeatBalance::ZonePreDefRep( 1 ).clPeak ), 0.00001 );

}


TEST_F( EnergyPlusFixture, OutputReportTabularMonthly_ResetMonthlyGathering )
{
	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"Output:Table:Monthly,",
		"Space Gains Annual Report, !- Name",
		"2, !-  Digits After Decimal",
		"Exterior Lights Electric Energy, !- Variable or Meter 1 Name",
		"SumOrAverage, !- Aggregation Type for Variable or Meter 1",
		"Exterior Lights Electric Power, !- Variable or Meter 2 Name",
		"Maximum, !- Aggregation Type for Variable or Meter 2",
		"Exterior Lights Electric Power, !- Variable or Meter 2 Name",
		"Minimum; !- Aggregation Type for Variable or Meter 2",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	Real64 extLitUse;

	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite1", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite2", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite3", _, "Electricity", "Exterior Lights", "General" );

	DataGlobals::DoWeathSim = true;
	DataGlobals::TimeStepZone = 0.25;

	GetInputTabularMonthly();
	EXPECT_EQ( MonthlyInputCount, 1 );
	InitializeTabularMonthly();

	extLitUse = 1.01;

	DataEnvironment::Month = 12;

	GatherMonthlyResultsForTimestep( 1 );
	EXPECT_EQ( extLitUse * 1, MonthlyColumns( 1 ).reslt( 12 ));

	GatherMonthlyResultsForTimestep( 1 );
	EXPECT_EQ( extLitUse * 2, MonthlyColumns( 1 ).reslt( 12 ) );

	GatherMonthlyResultsForTimestep( 1 );
	EXPECT_EQ( extLitUse * 3, MonthlyColumns( 1 ).reslt( 12 ) );

	ResetMonthlyGathering();

	EXPECT_EQ( 0., MonthlyColumns( 1 ).reslt( 12 ) );

	GatherMonthlyResultsForTimestep( 1 );
	EXPECT_EQ( extLitUse * 1, MonthlyColumns( 1 ).reslt( 12 ) );

}

TEST_F( EnergyPlusFixture, OutputReportTabular_ConfirmResetBEPSGathering )
{

	Real64 extLitUse;

	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite1", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite2", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite3", _, "Electricity", "Exterior Lights", "General" );

	DataGlobals::DoWeathSim = true;
	DataGlobals::TimeStepZone = 1.0;
	displayTabularBEPS = true;
	TimeValue.allocate( 2 );

	auto timeStep = 1.0;

	SetupTimePointers( "Zone", timeStep );
	SetupTimePointers( "HVAC", timeStep );

	TimeValue( 1 ).TimeStep = 60;
	TimeValue( 2 ).TimeStep = 60;

	GetInputOutputTableSummaryReports();

	extLitUse = 1.01;

	DataEnvironment::Month = 12;

	UpdateMeterReporting();
	UpdateDataandReport( 1 );
	GatherBEPSResultsForTimestep( 1 );
	EXPECT_EQ( extLitUse * 3, gatherEndUseBEPS( 1, endUseExteriorLights ) );

	UpdateMeterReporting();
	UpdateDataandReport( 1 );
	GatherBEPSResultsForTimestep( 1 );
	EXPECT_EQ( extLitUse * 6, gatherEndUseBEPS( 1, endUseExteriorLights ) );

	UpdateMeterReporting();
	UpdateDataandReport( 1 );
	GatherBEPSResultsForTimestep( 1 );
	EXPECT_EQ( extLitUse * 9, gatherEndUseBEPS( 1, endUseExteriorLights ) );

	ResetBEPSGathering();

	EXPECT_EQ( 0., gatherEndUseBEPS( 1, endUseExteriorLights ) );

	UpdateMeterReporting();
	UpdateDataandReport( 1 );
	GatherBEPSResultsForTimestep( 1 );
	EXPECT_EQ( extLitUse * 3, gatherEndUseBEPS( 1, endUseExteriorLights ) );

}


TEST_F( EnergyPlusFixture, OutputTableTimeBins_GetInput )
{
	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"Output:Table:TimeBins,",
		"System1, !- Key Value",
		"Some Temperature Variable, !- Variable Name",
		"0.00, !- Interval Start",
		"0.20, !- Interval Size",
		"5,                       !- Interval Count",
		"Always1; !- Schedule Name"
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	DataGlobals::DoWeathSim = true;

	GetInputTabularTimeBins();

	EXPECT_EQ( OutputReportTabular::OutputTableBinned.size(), 1u );
	EXPECT_EQ( OutputTableBinned( 1 ).keyValue, "SYSTEM1" );
	EXPECT_EQ( OutputTableBinned( 1 ).varOrMeter, "SOME TEMPERATURE VARIABLE" );
	EXPECT_EQ( OutputTableBinned( 1 ).intervalStart, 0.0 );
	EXPECT_EQ( OutputTableBinned( 1 ).intervalSize, 0.20 );
	EXPECT_EQ( OutputTableBinned( 1 ).intervalCount, 5 );
	EXPECT_EQ( OutputTableBinned( 1 ).ScheduleName, "ALWAYS1" );
}


TEST_F( EnergyPlusFixture, OutputReportTabularTest_PredefinedTableRowMatchingTest )
{

	SetPredefinedTables();

	PreDefTableEntry( pdchLeedPerfElEneUse, "Exterior Lighting", 1000., 2 );
	EXPECT_EQ( "1000.00", RetrievePreDefTableEntry( pdchLeedPerfElEneUse, "Exterior Lighting" ) );
	EXPECT_EQ( "NOT FOUND", RetrievePreDefTableEntry( pdchLeedPerfElEneUse, "EXTERIOR LIGHTING" ) );

	PreDefTableEntry( pdchLeedPerfElEneUse, "EXTERIOR LIGHTING", 2000., 2 );
	EXPECT_EQ( "1000.00", RetrievePreDefTableEntry( pdchLeedPerfElEneUse, "Exterior Lighting" ) );
	EXPECT_EQ( "2000.00", RetrievePreDefTableEntry( pdchLeedPerfElEneUse, "EXTERIOR LIGHTING" ) );

}


TEST( OutputReportTabularTest, GetUnitSubstring_Test )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, GetUnitSubstring_Test" );
	EXPECT_EQ( "", GetUnitSubString( "" ) );
	EXPECT_EQ( "", GetUnitSubString( " " ) );
	EXPECT_EQ( "", GetUnitSubString( "String with no units" ) );
	EXPECT_EQ( "feet", GetUnitSubString( "[feet]" ) );
	EXPECT_EQ( "meters", GetUnitSubString( "String with  unit string at end [meters]" ) );
	EXPECT_EQ( "newtons", GetUnitSubString( "[newtons] String with unit string at beginning" ) );
	EXPECT_EQ( "m", GetUnitSubString( "String with unit string at end [m]" ) );
	EXPECT_EQ( "N", GetUnitSubString( "[N] String with unit string at beginning" ) );
	EXPECT_EQ( "", GetUnitSubString( "[]" ) );
	EXPECT_EQ( "", GetUnitSubString( "String with empty unit string at end []" ) );
	EXPECT_EQ( "", GetUnitSubString( "[] String with empty unit string at beginning" ) );
}


TEST_F( SQLiteFixture, WriteVeriSumTableAreasTest ) {
	sqlite_test->sqliteBegin();
	sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

	EnergyPlus::sqlite = std::move( sqlite_test );

	displayTabularVeriSum = true;
	Latitude = 12.3;
	Longitude = 45.6;

	TotSurfaces = 4;
	Surface.allocate( TotSurfaces );

	// walls
	Surface( 1 ).Class = SurfaceClass_Wall;
	Surface( 1 ).HeatTransSurf = true;
	Surface( 1 ).ExtBoundCond = ExternalEnvironment;
	Surface( 1 ).Azimuth = 0.;
	Surface( 1 ).GrossArea = 200.; // 20 x 10
	Surface( 1 ).FrameDivider = 0;
	Surface( 1 ).Tilt = 90.;
	Surface( 1 ).Zone = 1;

	Surface( 2 ).Class = SurfaceClass_Wall;
	Surface( 2 ).HeatTransSurf = true;
	Surface( 2 ).ExtBoundCond = ExternalEnvironment;
	Surface( 2 ).Azimuth = 90.;
	Surface( 2 ).GrossArea = 300.; // 30 x 10
	Surface( 2 ).FrameDivider = 0;
	Surface( 2 ).Tilt = 90.;
	Surface( 2 ).Zone = 1;

	// windows
	Surface( 3 ).Class = SurfaceClass_Window;
	Surface( 3 ).HeatTransSurf = true;
	Surface( 3 ).ExtBoundCond = ExternalEnvironment;
	Surface( 3 ).Azimuth = 0.;
	Surface( 3 ).GrossArea = 40.;
	Surface( 3 ).Height = 5;
	Surface( 3 ).Width = 8;
	Surface( 3 ).FrameDivider = 1;
	Surface( 3 ).Tilt = 90.;
	Surface( 3 ).Zone = 1;

	Surface( 4 ).Class = SurfaceClass_Window;
	Surface( 4 ).HeatTransSurf = true;
	Surface( 4 ).ExtBoundCond = ExternalEnvironment;
	Surface( 4 ).Azimuth = 90.;
	Surface( 4 ).GrossArea = 60.;
	Surface( 4 ).Height = 6;
	Surface( 4 ).Width = 10;
	Surface( 4 ).FrameDivider = 2;
	Surface( 4 ).Tilt = 90.;
	Surface( 4 ).Zone = 1;

	// frames
	TotFrameDivider = 2;
	FrameDivider.allocate( TotFrameDivider );
	FrameDivider( 1 ).FrameWidth = 0.3;
	FrameDivider( 2 ).FrameWidth = 0.2;

	// zone
	NumOfZones = 1;
	Zone.allocate( NumOfZones );
	Zone( 1 ).SystemZoneNodeNumber = 1;
	Zone( 1 ).Multiplier = 1.;
	Zone( 1 ).ListMultiplier = 1.;
	Zone( 1 ).FloorArea = 600.; // 20 x 30
	Zone( 1 ).Volume = 6000.; // 20 x 30 x 10
	Zone( 1 ).isPartOfTotalArea = true;
	Zone( 1 ).ExtGrossWallArea = 500.;
	Zone( 1 ).ExteriorTotalGroundSurfArea = 0;
	Zone( 1 ).ExtWindowArea = Surface( 3 ).GrossArea + Surface( 4 ).GrossArea;

	WriteVeriSumTable();

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto tabularData = queryResult( "SELECT * FROM TabularData;", "TabularData" );
	auto strings = queryResult( "SELECT * FROM Strings;", "Strings" );
	auto stringTypes = queryResult( "SELECT * FROM StringTypes;", "StringTypes" );
	sqlite_test->sqliteCommit();

	EXPECT_EQ( 123ul, tabularData.size() );
	// tabularDataIndex, reportNameIndex, reportForStringIndex, tableNameIndex, rowLabelIndex, columnLabelIndex, unitsIndex, simulationIndex, rowId, columnId, value
	EXPECT_EQ( "       12.30", tabularData[3][10] );
	EXPECT_EQ( "       45.60", tabularData[4][10] );
	// envelope - window-wall ratio subtable
	// north
	EXPECT_EQ( "      200.00", tabularData[15][10] );
	EXPECT_EQ( "      200.00", tabularData[16][10] );
	EXPECT_EQ( "       48.16", tabularData[17][10] );
	EXPECT_EQ( "       24.08", tabularData[18][10] );
	EXPECT_EQ( "       24.08", tabularData[19][10] );
	// east
	EXPECT_EQ( "      300.00", tabularData[20][10] );
	EXPECT_EQ( "      300.00", tabularData[21][10] );
	EXPECT_EQ( "       66.56", tabularData[22][10] );
	EXPECT_EQ( "       22.19", tabularData[23][10] );
	EXPECT_EQ( "       22.19", tabularData[24][10] );
    // Performance - zone summary table
	EXPECT_EQ( "      600.00", tabularData[63][10] ); //area
	EXPECT_EQ( "Yes", tabularData[68][10] ); // conditioned
	EXPECT_EQ( "Yes", tabularData[73][10] ); // part of total floor area
	EXPECT_EQ( "     6000.00", tabularData[78][10] ); // volume
	EXPECT_EQ( "        1.00", tabularData[83][10] ); // multiplier
	EXPECT_EQ( "      500.00", tabularData[88][10] ); // above ground gross floor area
	EXPECT_EQ( "      100.00", tabularData[98][10] ); // window glass area
	EXPECT_EQ( "      114.72", tabularData[103][10] ); // window opening area

}


TEST_F( EnergyPlusFixture, OutputReportTabularMonthly_invalidAggregationOrder )
{
	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"Output:Table:Monthly,",
		"Space Gains Annual Report, !- Name",
		"2, !-  Digits After Decimal",
		"Exterior Lights Electric Energy, !- Variable or Meter 1 Name",
		"SumOrAverageDuringHoursShown, !- Aggregation Type for Variable or Meter 1",
		"Exterior Lights Electric Power, !- Variable or Meter 2 Name",
		"Maximum, !- Aggregation Type for Variable or Meter 2",
		"Exterior Lights Electric Power, !- Variable or Meter 2 Name",
		"Minimum; !- Aggregation Type for Variable or Meter 2",

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	Real64 extLitUse;

	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite1", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite2", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite3", _, "Electricity", "Exterior Lights", "General" );

	DataGlobals::DoWeathSim = true;
	DataGlobals::TimeStepZone = 0.25;

	GetInputTabularMonthly( );
	EXPECT_EQ( MonthlyInputCount, 1 );
	InitializeTabularMonthly( );

	EXPECT_TRUE(isInvalidAggregationOrder());

}


TEST( OutputReportTabularTest, CollectPeakZoneConditions_test )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, CollectPeakZoneConditions_test" );

	Psychrometrics::InitializePsychRoutines();

	CompLoadTablesType compLoad;
	int timeOfMax = 10;
	int zoneIndex = 1;
	bool isCooling = true;

	Zone.allocate( 1 );
	Zone( 1 ).Multiplier = 1;
	Zone( 1 ).ListMultiplier = 1;
	Zone( 1 ).FloorArea = 12.;

	CoolPeakDateHrMin.allocate( 1 );
	CoolPeakDateHrMin( 1 ) = "06/30 13:15:00";

	CalcFinalZoneSizing.allocate( 1 );
	CalcFinalZoneSizing( 1 ).CoolOutTempSeq.allocate( 10 );
	CalcFinalZoneSizing( 1 ).CoolOutTempSeq( 10 ) = 38.;
	CalcFinalZoneSizing( 1 ).CoolOutHumRatSeq.allocate( 10 );
	CalcFinalZoneSizing( 1 ).CoolOutHumRatSeq( 10 ) = 0.01459;
	CalcFinalZoneSizing( 1 ).CoolZoneTempSeq.allocate( 10 );
	CalcFinalZoneSizing( 1 ).CoolZoneTempSeq( 10 ) = 24.;
	CalcFinalZoneSizing( 1 ).CoolZoneHumRatSeq.allocate( 10 );
	CalcFinalZoneSizing( 1 ).CoolZoneHumRatSeq( 10 ) = 0.00979;
	CalcFinalZoneSizing( 1 ).DesCoolLoad = 500.;
	CalcFinalZoneSizing( 1 ).ZnCoolDgnSAMethod = SupplyAirTemperature;
	CalcFinalZoneSizing( 1 ).CoolDesTemp = 13.;
	CalcFinalZoneSizing( 1 ).DesCoolVolFlow = 3.3;

	FinalZoneSizing.allocate( 1 );
	FinalZoneSizing( 1 ).DesCoolLoad = 600.;

	CollectPeakZoneConditions( compLoad, timeOfMax, zoneIndex, isCooling );

	EXPECT_EQ( compLoad.peakDateHrMin, "06/30 13:15:00" );
	EXPECT_EQ( compLoad.outsideDryBulb, 38. );
	EXPECT_EQ( compLoad.outsideHumRatio, 0.01459 );
	EXPECT_EQ( compLoad.zoneDryBulb, 24. );
	EXPECT_EQ( compLoad.zoneHumRatio, 0.00979 );
	EXPECT_EQ( compLoad.peakDesSensLoad, 500. );
	EXPECT_EQ( compLoad.designPeakLoad, 600. );
	EXPECT_EQ( compLoad.supAirTemp, 13. );
	EXPECT_EQ( compLoad.mainFanAirFlow, 3.3 );
	EXPECT_NEAR( compLoad.airflowPerFlrArea, 3.3 / 12., 0.0001 );
	EXPECT_NEAR( compLoad.totCapPerArea, 600. / 12., 0.0001 );
	EXPECT_NEAR( compLoad.airflowPerTotCap, 3.3 / 600., 0.0001 );
	EXPECT_NEAR( compLoad.areaPerTotCap, 12. / 600., 0.0001 );

}

TEST( OutputReportTabularTest, CollectPeakAirLoopConditions_test )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, CollectPeakAirLoopConditions_test" );

	CompLoadTablesType compLoad;
	int airLoopIndex = 1;
	bool isCooling = true;

	CalcSysSizing.allocate( 1 );
	CalcSysSizing( 1 ).SensCoolCap = 500.;

	FinalSysSizing.allocate( 1 );
	FinalSysSizing( 1 ).CoolSupTemp = 13.;
	FinalSysSizing( 1 ).MixTempAtCoolPeak = 18.;
	FinalSysSizing( 1 ).SensCoolCap = 600.;
	FinalSysSizing( 1 ).DesMainVolFlow = 3.3;
	FinalSysSizing( 1 ).DesOutAirVolFlow = 0.12;

	CollectPeakAirLoopConditions( compLoad, airLoopIndex, isCooling );

	EXPECT_EQ( compLoad.supAirTemp, 13. );
	EXPECT_EQ( compLoad.mixAirTemp, 18. );
	EXPECT_EQ( compLoad.designPeakLoad, 600. );
	EXPECT_EQ( compLoad.peakDesSensLoad, 500. );
	EXPECT_EQ( compLoad.mainFanAirFlow, 3.3 );
	EXPECT_EQ( compLoad.outsideAirFlow, 0.12 );
	EXPECT_EQ( compLoad.diffDesignPeak, 100. );

	isCooling = false;

	CalcSysSizing( 1 ).HeatCap = 800.;

	FinalSysSizing.allocate( 1 );
	FinalSysSizing( 1 ).HeatSupTemp = 45.;
	FinalSysSizing( 1 ).HeatMixTemp = 37.;
	FinalSysSizing( 1 ).HeatCap = 900.;
	FinalSysSizing( 1 ).DesMainVolFlow = 3.3;
	FinalSysSizing( 1 ).DesOutAirVolFlow = 0.12;


	CollectPeakAirLoopConditions( compLoad, airLoopIndex, isCooling );

	EXPECT_EQ( compLoad.supAirTemp, 45. );
	EXPECT_EQ( compLoad.mixAirTemp, 37. );
	EXPECT_EQ( compLoad.designPeakLoad, -900. );
	EXPECT_EQ( compLoad.peakDesSensLoad, -800. );
	EXPECT_EQ( compLoad.mainFanAirFlow, 3.3 );
	EXPECT_EQ( compLoad.outsideAirFlow, 0.12 );
	EXPECT_EQ( compLoad.diffDesignPeak, -100. );

}

TEST( OutputReportTabularTest, ComputeEngineeringChecks_test )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, ComputeEngineeringChecks_test" );

	CompLoadTablesType compLoad;

	compLoad.outsideAirFlow = 0.12;
	compLoad.mainFanAirFlow = 0.50;
	compLoad.floorArea = 13.;
	compLoad.designPeakLoad = 800;

	ComputeEngineeringChecks( compLoad );

	EXPECT_EQ( compLoad.outsideAirRatio, 0.12/0.50 );
	EXPECT_EQ( compLoad.airflowPerFlrArea, 0.50/13. );
	EXPECT_EQ( compLoad.totCapPerArea, 800./13. );
	EXPECT_EQ( compLoad.airflowPerTotCap, 0.50/800. );
	EXPECT_EQ( compLoad.areaPerTotCap, 13./800. );

}

TEST( OutputReportTabularTest, GetZoneComponentAreas_test )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, GetZoneComponentAreas_test" );

	Array1D< ZompComponentAreasType > areas;
	areas.allocate(1);

	Zone.allocate( 1 );
	NumOfZones = 1;
	Zone( 1 ).FloorArea = 12.;

	Surface.allocate(13);

	Surface( 1 ).GrossArea = 5.;  //extWall
	Surface( 1 ).Class = SurfaceClass_Wall;
	Surface( 1 ).ExtBoundCond = ExternalEnvironment;
	Surface( 1 ).Zone = 1;
	Surface( 1 ).HeatTransSurf = true;

	Surface( 2 ).GrossArea = 6.; //grdCntWall
	Surface( 2 ).Class = SurfaceClass_Wall;
	Surface( 2 ).ExtBoundCond = GroundFCfactorMethod;
	Surface( 2 ).Zone = 1;
	Surface( 2 ).HeatTransSurf = true;

	Surface( 3 ).GrossArea = 7.; //intZoneWall
	Surface( 3 ).Class = SurfaceClass_Wall;
	Surface( 3 ).ExtBoundCond = 2;
	Surface( 3 ).Zone = 1;
	Surface( 3 ).HeatTransSurf = true;

	Surface( 4 ).GrossArea = 8.; //roof
	Surface( 4 ).Class = SurfaceClass_Roof;
	Surface( 4 ).ExtBoundCond = ExternalEnvironment;
	Surface( 4 ).Zone = 1;
	Surface( 4 ).HeatTransSurf = true;

	Surface( 5 ).GrossArea = 9.; //ceiling
	Surface( 5 ).Class = SurfaceClass_Roof;
	Surface( 5 ).ExtBoundCond = 5;
	Surface( 5 ).Zone = 1;
	Surface( 5 ).HeatTransSurf = true;

	Surface( 6 ).GrossArea = 10.; //extFloor
	Surface( 6 ).Class = SurfaceClass_Floor;
	Surface( 6 ).ExtBoundCond = ExternalEnvironment;
	Surface( 6 ).Zone = 1;
	Surface( 6 ).HeatTransSurf = true;

	Surface( 7 ).GrossArea = 11.; //grndCntFloor
	Surface( 7 ).Class = SurfaceClass_Floor;
	Surface( 7 ).ExtBoundCond = Ground;
	Surface( 7 ).Zone = 1;
	Surface( 7 ).HeatTransSurf = true;

	Surface( 8 ).GrossArea = 12.; //intZoneFloor
	Surface( 8 ).Class = SurfaceClass_Floor;
	Surface( 8 ).ExtBoundCond = 3;
	Surface( 8 ).Zone = 1;
	Surface( 8 ).HeatTransSurf = true;

	Surface( 9 ).GrossArea = 13.; //fenestration
	Surface( 9 ).Class = SurfaceClass_Window;
	Surface( 9 ).ExtBoundCond = ExternalEnvironment;
	Surface( 9 ).Zone = 1;
	Surface( 9 ).HeatTransSurf = true;

	Surface( 10 ).GrossArea = 14.; //door
	Surface( 10 ).Class = SurfaceClass_Door;
	Surface( 10 ).ExtBoundCond = ExternalEnvironment;
	Surface( 10 ).Zone = 1;
	Surface( 10 ).HeatTransSurf = true;

	Surface( 11 ).GrossArea = 15.; //door (again)
	Surface( 11 ).Class = SurfaceClass_GlassDoor;
	Surface( 11 ).ExtBoundCond = ExternalEnvironment;
	Surface( 11 ).Zone = 1;
	Surface( 11 ).HeatTransSurf = true;

	Surface( 12 ).GrossArea = 16.; //fenestration (again)
	Surface( 12 ).Class = SurfaceClass_TDD_Dome;
	Surface( 12 ).ExtBoundCond = ExternalEnvironment;
	Surface( 12 ).Zone = 1;
	Surface( 12 ).HeatTransSurf = true;

	Surface( 13 ).GrossArea = 17.; //grndCntFloor (again)
	Surface( 13 ).Class = SurfaceClass_Floor;
	Surface( 13 ).ExtBoundCond = KivaFoundation;
	Surface( 13 ).Zone = 1;
	Surface( 13 ).HeatTransSurf = true;

	GetZoneComponentAreas( areas );

	EXPECT_EQ( 12., areas( 1 ).floor );
	EXPECT_EQ( 8., areas( 1 ).roof );
	EXPECT_EQ( 9., areas( 1 ).ceiling );
	EXPECT_EQ( 5., areas( 1 ).extWall );
	EXPECT_EQ( 7., areas( 1 ).intZoneWall );
	EXPECT_EQ( 6., areas( 1 ).grndCntWall );
	EXPECT_EQ( 10., areas( 1 ).extFloor );
	EXPECT_EQ( 12., areas( 1 ).intZoneFloor );
	EXPECT_EQ( 28., areas( 1 ).grndCntFloor );
	EXPECT_EQ( 29., areas( 1 ).fenestration );
	EXPECT_EQ( 29., areas( 1 ).door );

}

TEST( OutputReportTabularTest, CombineLoadCompResults_test )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, CombineLoadCompResults_test" );

	CompLoadTablesType compLoadTotal;
	compLoadTotal.cells.allocate( 10, 30 );
	compLoadTotal.cells = 0.;
	compLoadTotal.cellUsed.allocate( 10, 30 );
	compLoadTotal.cellUsed = false;

	CompLoadTablesType compLoadPartial;
	compLoadPartial.cells.allocate( 10, 30 );
	compLoadPartial.cells = 0.;
	compLoadPartial.cellUsed.allocate( 10, 30 );
	compLoadPartial.cellUsed = false;

	Real64 multiplier = 3.;

	compLoadPartial.cells( 1, 1) = 1.1;
	compLoadPartial.cells( 4, 25 ) = 1.2;
	compLoadPartial.cellUsed( 3, 17 ) = true;
	compLoadPartial.outsideWebBulb = 17.;
	compLoadPartial.diffDesignPeak = 11.;

	CombineLoadCompResults( compLoadTotal, compLoadPartial, multiplier );

	EXPECT_EQ( 1.1 * 3., compLoadTotal.cells( 1, 1 ) );
	EXPECT_EQ( 1.2 * 3., compLoadTotal.cells( 4, 25 ) );
	EXPECT_EQ( true, compLoadTotal.cellUsed( 3, 17 ) );
	EXPECT_EQ( 17., compLoadTotal.outsideWebBulb );
	EXPECT_EQ( 33., compLoadTotal.diffDesignPeak );

}

TEST( OutputReportTabularTest, AddTotalRowsForLoadSummary_test )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, AddTotalRowsForLoadSummary_test" );

	CompLoadTablesType compLoad;
	compLoad.cells.allocate( cPerArea, rGrdTot );
	compLoad.cells = 0.;
	compLoad.cellUsed.allocate( cPerArea, rGrdTot );
	compLoad.cellUsed = true;

	compLoad.cells( cSensInst, rLights ) = 3.;
	compLoad.cells( cSensInst, rRefrig ) = 4.;
	compLoad.cells( cLatent, rLights ) = 10.;
	compLoad.cells( cLatent, rRefrig ) = 20.;

	compLoad.cells( cArea, rLights ) = 5.;
	compLoad.cells( cArea, rRefrig ) = 5.;

	AddTotalRowsForLoadSummary( compLoad );

	EXPECT_EQ( 3. + 4., compLoad.cells( cSensInst, rGrdTot ) );
	EXPECT_EQ( 10 + 20., compLoad.cells( cLatent, rGrdTot ) );
	EXPECT_EQ( 3. + 10., compLoad.cells( cTotal, rLights ) );
	EXPECT_EQ( 4 + 20., compLoad.cells( cTotal, rRefrig ) );

	EXPECT_EQ( 37., compLoad.cells( cTotal, rGrdTot ) );

	EXPECT_EQ( 100. * 13. / 37., compLoad.cells( cPerc, rLights ) );
	EXPECT_EQ( 100. * 24. / 37., compLoad.cells( cPerc, rRefrig ) );

	EXPECT_EQ( 13. / 5., compLoad.cells( cPerArea, rLights ) );
	EXPECT_EQ( 24. / 5., compLoad.cells( cPerArea, rRefrig ) );

}

TEST( OutputReportTabularTest, LoadSummaryUnitConversion_test )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, LoadSummaryUnitConversion_test" );

	CompLoadTablesType compLoad;
	compLoad.cells.allocate( cPerArea, rGrdTot );
	compLoad.cells = 0.;
	compLoad.cellUsed.allocate( cPerArea, rGrdTot );
	compLoad.cellUsed = true;

	compLoad.cells( cSensInst, rLights ) = 3.;
	compLoad.cells( cLatent, rLights ) = 10.;

	compLoad.cells( cArea, rLights ) = 5.;

	compLoad.outsideDryBulb = 20.;
	compLoad.mainFanAirFlow = 0.7;
	compLoad.airflowPerTotCap = 0.2;
	compLoad.totCapPerArea = 0.15;

	unitsStyle = unitsStyleInchPound;
	Real64 powerConversion = getSpecificUnitMultiplier( "W", "Btu/h" );
	Real64 areaConversion = getSpecificUnitMultiplier( "m2", "ft2" );
	Real64 airFlowConversion = getSpecificUnitMultiplier( "m3/s", "ft3/min" );
	Real64 airFlowPerAreaConversion = getSpecificUnitMultiplier( "m3/s-m2", "ft3/min-ft2" );
	int tempConvIndx = getSpecificUnitIndex( "C", "F" );

	LoadSummaryUnitConversion( compLoad );

	EXPECT_EQ( 3. * powerConversion, compLoad.cells( cSensInst, rLights ) );
	EXPECT_EQ( 10. * powerConversion, compLoad.cells( cLatent, rLights ) );
	EXPECT_EQ( 5. * areaConversion, compLoad.cells( cArea, rLights ) );
	EXPECT_EQ( 5. * areaConversion, compLoad.cells( cArea, rLights ) );

	EXPECT_EQ( ConvertIP( tempConvIndx, 20. ), compLoad.outsideDryBulb );
	EXPECT_EQ( 0.7 * airFlowConversion, compLoad.mainFanAirFlow );
	EXPECT_EQ( 0.2 * airFlowPerAreaConversion / powerConversion, compLoad.airflowPerTotCap );
	EXPECT_EQ( 0.15 * powerConversion / areaConversion, compLoad.totCapPerArea );

}


TEST( OutputReportTabularTest, CreateListOfZonesForAirLoop_test )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, CreateListOfZonesForAirLoop_test" );

	CompLoadTablesType compLoad;
	Array1D_int zoneToAirLoop;

	NumOfZones = 15;
	compLoad.zoneIndices.allocate( NumOfZones );
	compLoad.zoneIndices = 0;

	zoneToAirLoop.allocate( NumOfZones );
	zoneToAirLoop( 1 ) = 3;
	zoneToAirLoop( 2 ) = 2;
	zoneToAirLoop( 3 ) = 1;
	zoneToAirLoop( 4 ) = 1;
	zoneToAirLoop( 5 ) = 2;
	zoneToAirLoop( 6 ) = 3;
	zoneToAirLoop( 7 ) = 1;
	zoneToAirLoop( 8 ) = 1;
	zoneToAirLoop( 9 ) = 2;
	zoneToAirLoop( 10 ) = 2;
	zoneToAirLoop( 11 ) = 1;
	zoneToAirLoop( 12 ) = 1;
	zoneToAirLoop( 13 ) = 3;
	zoneToAirLoop( 14 ) = 3;
	zoneToAirLoop( 15 ) = 1;

	CreateListOfZonesForAirLoop( compLoad, zoneToAirLoop, 1 );
	EXPECT_EQ( 3, compLoad.zoneIndices( 1 ) );
	EXPECT_EQ( 4, compLoad.zoneIndices( 2 ) );
	EXPECT_EQ( 7, compLoad.zoneIndices( 3 ) );
	EXPECT_EQ( 8, compLoad.zoneIndices( 4 ) );
	EXPECT_EQ( 11, compLoad.zoneIndices( 5 ) );
	EXPECT_EQ( 12, compLoad.zoneIndices( 6 ) );
	EXPECT_EQ( 15, compLoad.zoneIndices( 7 ) );
	EXPECT_EQ( 0, compLoad.zoneIndices( 8 ) );

	compLoad.zoneIndices = 0;
	CreateListOfZonesForAirLoop( compLoad, zoneToAirLoop, 2 );
	EXPECT_EQ( 2, compLoad.zoneIndices( 1 ) );
	EXPECT_EQ( 5, compLoad.zoneIndices( 2 ) );
	EXPECT_EQ( 9, compLoad.zoneIndices( 3 ) );
	EXPECT_EQ( 10, compLoad.zoneIndices( 4 ) );
	EXPECT_EQ( 0, compLoad.zoneIndices( 5 ) );

	compLoad.zoneIndices = 0;
	CreateListOfZonesForAirLoop( compLoad, zoneToAirLoop, 3 );
	EXPECT_EQ( 1, compLoad.zoneIndices( 1 ) );
	EXPECT_EQ( 6, compLoad.zoneIndices( 2 ) );
	EXPECT_EQ( 13, compLoad.zoneIndices( 3 ) );
	EXPECT_EQ( 14, compLoad.zoneIndices( 4 ) );
	EXPECT_EQ( 0, compLoad.zoneIndices( 5 ) );

}



