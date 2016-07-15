// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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

TEST_F( EnergyPlusFixture, AirloopHVAC_ZoneSumTest )
{
	// AUTHOR: R. Raustad, FSEC
	// DATE WRITTEN: Sep 2015

	std::string const idf_objects = delimited_string( {

		" Version,8.3;",
		" Output:Diagnostics, DisplayExtraWarnings;",
		" Timestep, 4;",
		" BUILDING, AirloopHVAC_ZoneSumTest, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
		" SimulationControl, YES, YES, NO, YES, NO;",

		"  Site:Location,",
		"    Miami Intl Ap FL USA TMY3 WMO=722020E,    !- Name",
		"    25.82,                 !- Latitude {deg}",
		"    -80.30,                !- Longitude {deg}",
		"    -5.00,                 !- Time Zone {hr}",
		"    11;                    !- Elevation {m}",

		"SizingPeriod:DesignDay,",
		" Miami Intl Ap Ann Clg .4% Condns DB/MCWB, !- Name",
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

		"Output:Table:SummaryReports,",
		"  AllSummaryAndSizingPeriod; !- Report 1 Name",

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

		"ZoneGroup,",
		" Zone Group,               !- Name",
		" Zone List,                !- Zone List Name",
		" 10;                       !- Zone List Multiplier",

		"ZoneList,",
		" Zone List,                !- Name",
		" Spacex10;                 !- Zone 1 Name",

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
		" Space DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name",
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
		" Spacex10 DSOA Design OA Spec, !- Design Specification Outdoor Air Object Name",
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
		" ActivityLevelSched;       !- Activity Level Schedule Name",

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
		" ActivityLevelSched;       !- Activity Level Schedule Name",

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

		"Schedule:Compact,",
		" OnSched,                  !- Name",
		" Fraction,                 !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0;        !- Field 26",

		"ScheduleTypeLimits,",
		" Fraction,                 !- Name",
		" 0.0,                      !- Lower Limit Value",
		" 1.0,                      !- Upper Limit Value",
		" CONTINUOUS;               !- Numeric Type",

		"Construction,",
		" INT-WALL-1,               !- Name",
		" GP02,                     !- Outside Layer",
		" AL21,                     !- Layer 2",
		" GP02;                     !- Layer 3",

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

		"Material:AirGap,",
		" AL21,                     !- Name",
		" 0.1570000;                !- Thermal Resistance{ m2 - K / W }",

		"Construction,",
		"FLOOR-SLAB-1,              !- Name",
		"CC03,                      !- Outside Layer",
		"CP01;                      !- Layer 2",

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

		"Material:NoMass,",
		" CP01,                     !- Name",
		" Rough,                    !- Roughness",
		" 0.3670000,                !- Thermal Resistance{ m2 - K / W }",
		" 0.9000000,                !- Thermal Absorptance",
		" 0.7500000,                !- Solar Absorptance",
		" 0.7500000;                !- Visible Absorptance",

		"Construction,",
		" CLNG-1,                   !- Name",
		" MAT-CLNG-1;               !- Outside Layer",

		"Material:NoMass,",
		" MAT-CLNG-1,               !- Name",
		" Rough,                    !- Roughness",
		" 0.652259290,              !- Thermal Resistance{ m2 - K / W }",
		" 0.65,                     !- Thermal Absorptance",
		" 0.65,                     !- Solar Absorptance",
		" 0.65;                     !- Visible Absorptance",

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

		"ZoneControl:Thermostat,",
		" Space Thermostat,         !- Name",
		" Space,                    !- Zone or ZoneList Name",
		" Dual Zone Control Type Sched,  !- Control Type Schedule Name",
		" ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
		" Space DualSPSched;        !- Control 1 Name",

		"ZoneControl:Thermostat,",
		" Spacex10 Thermostat,      !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
		" Dual Zone Control Type Sched,  !- Control Type Schedule Name",
		" ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
		" Space DualSPSched;        !- Control 1 Name",

		"Schedule:Compact,",
		" Dual Zone Control Type Sched,  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00,4;           !- Field 3",

		"ThermostatSetpoint:DualSetpoint,",
		" Space DualSPSched,        !- Name",
		" HTGSETP_SCH,              !- Heating Setpoint Temperature Schedule Name",
		" CLGSETP_SCH;              !- Cooling Setpoint Temperature Schedule Name",

		"Schedule:Compact,",
		" CLGSETP_SCH,              !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 19",
		" Until: 24:00,22.1;        !- Field 20",

		"Schedule:Compact,",
		" HTGSETP_SCH,              !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 22",
		" Until: 24:00, 21.9;       !- Field 23",

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

		"Sizing:System,",
		" DOAS,                     !- AirLoop Name",
		" VentilationRequirement,   !- Type of Load to Size On",
		" autosize,                 !- Design Outdoor Air Flow Rate {m3/s}",
		" 1.0,                      !- Central Heating Maximum System Air Flow Ratio",
		" 2,                        !- Preheat Design Temperature {C}",
		" 0.008,                    !- Preheat Design Humidity Ratio {kgWater/kgDryAir}",
		" 11,                       !- Precool Design Temperature {C}",
		" 0.008,                    !- Precool Design Humidity Ratio {kgWater/kgDryAir}",
		" 16,                       !- Central Cooling Design Supply Air Temperature {C}",
		" 12.2,                     !- Central Heating Design Supply Air Temperature {C}",
		" NonCoincident,            !- Type of Zone Sum to Use",
		" Yes,                      !- 100% Outdoor Air in Cooling",
		" Yes,                      !- 100% Outdoor Air in Heating",
		" 0.0103,                   !- Central Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		" 0.003,                    !- Central Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		" DesignDay,                !- Cooling Supply Air Flow Rate Method",
		" 0,                        !- Cooling Supply Air Flow Rate {m3/s}",
		" ,                         !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
		" ,                         !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
		" ,                         !- Cooling Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}",
		" DesignDay,                !- Heating Supply Air Flow Rate Method",
		" 0,                        !- Heating Supply Air Flow Rate {m3/s}",
		" ,                         !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
		" ,                         !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
		" ,                         !- Heating Fraction of Autosized Cooling Supply Air Flow Rate",
		" ,                         !- Heating Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}",
		" ZoneSum,                  !- System Outdoor Air Method",
		" 1.0,                      !- Zone Maximum Outdoor Air Fraction {dimensionless}",
		" CoolingDesignCapacity,    !- Cooling Design Capacity Method",
		" autosize,                 !- Cooling Design Capacity {W}",
		" ,                         !- Cooling Design Capacity Per Floor Area {W/m2}",
		" ,                         !- Fraction of Autosized Cooling Design Capacity",
		" HeatingDesignCapacity,    !- Heating Design Capacity Method",
		" autosize,                 !- Heating Design Capacity {W}",
		" ,                         !- Heating Design Capacity Per Floor Area {W/m2}",
		" ,                         !- Fraction of Autosized Heating Design Capacity",
		" OnOff;                    !- Central Cooling Capacity Control Method",


		"AirLoopHVAC,",
		"  DOAS,                    !- Name",
		"  ,                        !- Controller List Name",
		"  DOAS Availability Managers,  !- Availability Manager List Name",
		"  autosize,                !- Design Supply Air Flow Rate {m3/s}",
		"  DOAS Branches,           !- Branch List Name",
		"  ,                        !- Connector List Name",
		"  DOAS Air Loop Inlet,     !- Supply Side Inlet Node Name",
		"  DOAS Return Air Outlet,  !- Demand Side Outlet Node Name",
		"  DOAS Supply Path Inlet,  !- Demand Side Inlet Node Names",
		"  DOAS Supply Fan Outlet;  !- Supply Side Outlet Node Names",

		"BranchList,",
		"  DOAS Branches,           !- Name",
		"  DOAS Main Branch;        !- Branch 1 Name",

		"Branch,",
		"  DOAS Main Branch,        !- Name",
		"  autosize,                !- Maximum Flow Rate {m3/s}",
		"  ,                        !- Pressure Drop Curve Name",
		"  AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
		"  DOAS OA System,          !- Component 1 Name",
		"  DOAS Air Loop Inlet,     !- Component 1 Inlet Node Name",
		"  DOAS Mixed Air Outlet,   !- Component 1 Outlet Node Name",
		"  Passive,                 !- Component 1 Branch Control Type",
		"  CoilSystem:Cooling:DX,   !- Component 2 Object Type",
		"  DOAS Cooling Coil,       !- Component 2 Name",
		"  DOAS Mixed Air Outlet,   !- Component 2 Inlet Node Name",
		"  DOAS Cooling Coil Outlet,!- Component 2 Outlet Node Name",
		"  Passive,                 !- Component 2 Branch Control Type",
		"  Coil:Heating:Gas,        !- Component 2 Object Type",
		"  DOAS Heating Coil,       !- Component 2 Name",
		"  DOAS Cooling Coil Outlet,  !- Component 2 Inlet Node Name",
		"  DOAS Heating Coil Outlet,!- Component 2 Outlet Node Name",
		"  Passive,                 !- Component 2 Branch Control Type",
		"  Fan:VariableVolume,      !- Component 3 Object Type",
		"  DOAS Supply Fan,         !- Component 3 Name",
		"  DOAS Heating Coil Outlet,!- Component 3 Inlet Node Name",
		"  DOAS Supply Fan Outlet,  !- Component 3 Outlet Node Name",
		"  Active;                  !- Component 3 Branch Control Type",

		"AirLoopHVAC:SupplyPath,",
		"  DOAS Supply Path,        !- Name",
		"  DOAS Supply Path Inlet,  !- Supply Air Path Inlet Node Name",
		"  AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
		"  DOAS Zone Splitter;      !- Component 1 Name",

		"AirLoopHVAC:ZoneSplitter,",
		"  DOAS Zone Splitter,      !- Name",
		"  DOAS Supply Path Inlet,  !- Inlet Node Name",
		"  Space ATU In Node,  !- Outlet 1 Node Name",
		"  Spacex10 ATU In Node;  !- Outlet 27 Node Name",

		"AirLoopHVAC:ReturnPath,",
		"  DOAS Return Path,        !- Name",
		"  DOAS Return Air Outlet,  !- Return Air Path Outlet Node Name",
		"  AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
		"  DOAS Zone Mixer;         !- Component 1 Name",

		"AirLoopHVAC:ZoneMixer,",
		"  DOAS Zone Mixer,         !- Name",
		"  DOAS Return Air Outlet,  !- Outlet Node Name",
		"  Space Ret Node,          !- Inlet 1 Node Name",
		"  Spacex10 Ret Node;       !- Inlet 27 Node Name",

		"AvailabilityManagerAssignmentList,",
		"  DOAS Availability Managers,  !- Name",
		"  AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
		"  DOAS Availability;       !- Availability Manager 1 Name",

		"AvailabilityManager:Scheduled,",
		"  DOAS Availability,       !- Name",
		"  AvailSched;              !- Schedule Name",

		"NodeList,",
		"  DOAS Cooling Setpoint Nodes,  !- Name",
		"  DOAS Cooling Coil Outlet, !- Node 1 Name",
		"  DOAS Heating Coil Outlet; !- Node 1 Name",

		"Schedule:Compact,",
		"  Always 22,               !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 1",
		"  For: AllDays,            !- Field 2",
		"  Until: 24:00,22;         !- Field 3",

		"SetpointManager:Scheduled,",
		"  DOAS Cooling Supply Air Temp Manager,  !- Name",
		"  Temperature,             !- Control Variable",
		"  Always 22,               !- Schedule Name",
		"  DOAS Supply Fan Outlet;  !- Setpoint Node or NodeList Name",

		"SetpointManager:MixedAir,",
		"  DOAS Cooling Coil Air Temp Manager,  !- Name",
		"  Temperature,             !- Control Variable",
		"  DOAS Supply Fan Outlet,  !- Reference Setpoint Node Name",
		"  DOAS Cooling Coil Outlet,!- Fan Inlet Node Name",
		"  DOAS Supply Fan Outlet,  !- Fan Outlet Node Name",
		"  DOAS Cooling Setpoint Nodes;  !- Setpoint Node or NodeList Name",

		"CoilSystem:Cooling:DX,",
		"  DOAS Cooling Coil,       !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  DOAS Mixed Air Outlet,   !- DX Cooling Coil System Inlet Node Name",
		"  DOAS Cooling Coil Outlet,  !- DX Cooling Coil System Outlet Node Name",
		"  DOAS Cooling Coil Outlet,  !- DX Cooling Coil System Sensor Node Name",
		"  Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
		"  DOAS DX Cooling Coil;    !- Cooling Coil Name",

		"Coil:Cooling:DX:SingleSpeed,",
		"	DOAS DX Cooling Coil,   !- Name",
		" 	AvailSched,            !- Availability Schedule Name",
		"	autosize,              !- Gross Rated Total Cooling Capacity { W }",
		"	autosize,              !- Gross Rated Sensible Heat Ratio",
		"	4.40,                  !- Gross Rated Cooling COP { W / W }",
		"	autosize,              !- Rated Air Flow Rate { m3 / s }",
		"	,                      !- Rated Evaporator Fan Power Per Volume Flow Rate { W / ( m3 / s ) }",
		"	DOAS Mixed Air Outlet, !- Air Inlet Node Name",
		"	DOAS Cooling Coil Outlet,    !- Air Outlet Node Name",
		"	Biquadratic,           !- Total Cooling Capacity Function of Temperature Curve Name",
		"	Cubic,                 !- Total Cooling Capacity Function of Flow Fraction Curve Name",
		"	Biquadratic,           !- Energy Input Ratio Function of Temperature Curve Name",
		"	Cubic,                 !- Energy Input Ratio Function of Flow Fraction Curve Name",
		"	Cubic,                 !- Part Load Fraction Correlation Curve Name",
		"	0.0,                   !- Nominal Time for Condensate Removal to Begin",
		"	0.0,                   !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
		"	0.0,                   !- Maximum Cycling Rate",
		"	0.0,                   !- Latent Capacity Time Constant",
		"	Cooling Coil Condenser Inlet, !- Condenser Air Inlet Node Name",
		"	EvaporativelyCooled,   !- Condenser Type",
		"	0.0,                   !- Evaporative Condenser Effectiveness",
		"	,                      !- Evaporative Condenser Air Flow Rate",
		"	autosize,              !- Evaporative Condenser Pump Rated Power Consumption",
		"	0.0,                   !- Crankcase Heater Capacity",
		"	10.0;                  !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",

		"Coil:Heating:Gas,",
		"  DOAS Heating Coil,       !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  0.8,                     !- Gas Burner Efficiency",
		"  autosize,                !- Nominal Capacity {W}",
		"  DOAS Cooling Coil Outlet,  !- Air Inlet Node Name",
		"  DOAS Heating Coil Outlet,  !- Air Outlet Node Name",
		"  DOAS Heating Coil Outlet;  !- Temperature Setpoint Node Name",

		"Fan:VariableVolume,",
		"  DOAS Supply Fan,         !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  0.7,                     !- Fan Total Efficiency",
		"  1000,                    !- Pressure Rise {Pa}",
		"  autosize,                !- Maximum Flow Rate {m3/s}",
		"  Fraction,                !- Fan Power Minimum Flow Rate Input Method",
		"  0.0,                     !- Fan Power Minimum Flow Fraction",
		"  ,                        !- Fan Power Minimum Air Flow Rate {m3/s}",
		"  0.9,                     !- Motor Efficiency",
		"  1,                       !- Motor In Airstream Fraction",
		"  0.0015302446,            !- Fan Power Coefficient 1",
		"  0.0052080574,            !- Fan Power Coefficient 2",
		"  1.1086242,               !- Fan Power Coefficient 3",
		"  -0.11635563,             !- Fan Power Coefficient 4",
		"  0,                       !- Fan Power Coefficient 5",
		"  DOAS Heating Coil Outlet,!- Air Inlet Node Name",
		"  DOAS Supply Fan Outlet;  !- Air Outlet Node Name",

		"OutdoorAir:NodeList,",
		"  DOAS Outdoor Air Inlet,  !- Node or NodeList Name 1",
		"  Cooling Coil Condenser Inlet;  !- Node or NodeList Name 2",

		"AirLoopHVAC:OutdoorAirSystem,",
		"  DOAS OA System,          !- Name",
		"  DOAS OA System Controllers,  !- Controller List Name",
		"  DOAS OA System Equipment,!- Outdoor Air Equipment List Name",
		"  DOAS Availability Managers;  !- Availability Manager List Name",

		"AirLoopHVAC:ControllerList,",
		"  DOAS OA System Controllers,  !- Name",
		"  Controller:OutdoorAir,   !- Controller 1 Object Type",
		"  DOAS OA Controller;      !- Controller 1 Name",

		"AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
		"  DOAS OA System Equipment,!- Name",
		"  OutdoorAir:Mixer,        !- Component 1 Object Type",
		"  DOAS OA Mixing Box;      !- Component 1 Name",

		"OutdoorAir:Mixer,",
		"  DOAS OA Mixing Box,      !- Name",
		"  DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
		"  DOAS Outdoor Air Inlet,  !- Outdoor Air Stream Node Name",
		"  DOAS Relief Air Outlet,  !- Relief Air Stream Node Name",
		"  DOAS Air Loop Inlet;     !- Return Air Stream Node Name",

		"Controller:OutdoorAir,",
		"  DOAS OA Controller,      !- Name",
		"  DOAS Relief Air Outlet,  !- Relief Air Outlet Node Name",
		"  DOAS Air Loop Inlet,     !- Return Air Node Name",
		"  DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
		"  DOAS Outdoor Air Inlet,  !- Actuator Node Name",
		"  0,                       !- Minimum Outdoor Air Flow Rate {m3/s}",
		"  autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
		"  NoEconomizer,            !- Economizer Control Type",
		"  MinimumFlowWithBypass,   !- Economizer Control Action Type",
		"  ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
		"  ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
		"  ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
		"  ,                        !- Electronic Enthalpy Limit Curve Name",
		"  12.2,                    !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
		"  NoLockout,               !- Lockout Type",
		"  FixedMinimum,            !- Minimum Limit Type",
		"  ,                        !- Minimum Outdoor Air Schedule Name",
		"  ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
		"  ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
		"  DCVObject;               !- Mechanical Ventilation Controller Name",

		"Controller:MechanicalVentilation,",
		"  DCVObject,               !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  Yes,                     !- Demand Controlled Ventilation",
		"  VentilationRateProcedure,!- System Outdoor Air Method",
		"  ,                        !- Zone Maximum Outdoor Air Fraction {dimensionless}",
		"  Space,  !- Zone 1 Name",
		"  Space DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name 1",
		"  Space DSOA Design ADE Spec, !- Design Specification Zone Air Distribution Object Name 1",
		"  Spacex10,      !- Zone 2 Name",
		"  Spacex10 DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name 2",
		"  Space DSOA Design ADE Spec;  !- Design Specification Zone Air Distribution Object Name 2",

		"DesignSpecification:OutdoorAir,",
		"  Space DSOA Design OA Spec,  !- Name",
		"  sum,                     !- Outdoor Air Method",
		"  0.0,                     !- Outdoor Air Flow per Person {m3/s-person}",
		"  0.0009,                  !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
		"  0;                       !- Outdoor Air Flow per Zone {m3/s}",

		"DesignSpecification:OutdoorAir,",
		"  Spacex10 DSOA Design OA Spec,  !- Name",
		"  sum,                     !- Outdoor Air Method",
		"  0.008,                   !- Outdoor Air Flow per Person {m3/s-person}",
		"  0.0009,                  !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
		"  0;                       !- Outdoor Air Flow per Zone {m3/s}",

		"DesignSpecification:ZoneAirDistribution,",
		"  Space DSOA Design ADE Spec,  !- Name",
		"  1,                       !- Zone Air Distribution Effectiveness in Cooling Mode {dimensionless}",
		"  1;                       !- Zone Air Distribution Effectiveness in Heating Mode {dimensionless}",

		"ZoneHVAC:EquipmentConnections,",
		" Space,                    !- Zone Name",
		" Space Eq,                 !- Zone Conditioning Equipment List Name",
		" Space In Node,            !- Zone Air Inlet Node or NodeList Name",
		" ,                         !- Zone Air Exhaust Node or NodeList Name",
		" Space Node,               !- Zone Air Node Name",
		" Space Ret Node;           !- Zone Return Air Node Name",

		"ZoneHVAC:EquipmentList,",
		" Space Eq,                 !- Name",
		" ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
		" Space ATU,                   !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

		"ZoneHVAC:AirDistributionUnit,",
		"  Space ATU,  !- Name",
		"  Space In Node,  !- Air Distribution Unit Outlet Node Name",
		"  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
		"  Space Air Terminal;  !- Air Terminal Name",

		"AirTerminal:SingleDuct:VAV:NoReheat,",
		"  Space Air Terminal,      !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  Space In Node,           !- Air Outlet Node Name",
		"  Space ATU In Node,       !- Air Inlet Node Name",
		"  autosize,                !- Maximum Air Flow Rate {m3/s}",
		"  Constant,                !- Zone Minimum Air Flow Input Method",
		"  0.0,                     !- Constant Minimum Air Flow Fraction",
		"  ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
		"  ,                        !- Minimum Air Flow Fraction Schedule Name",
		"  ;                        !- Design Specification Outdoor Air Object Name",

		"ZoneHVAC:EquipmentConnections,",
		" Spacex10,                 !- Zone Name",
		" Spacex10 Eq,              !- Zone Conditioning Equipment List Name",
		" Spacex10 In Node,         !- Zone Air Inlet Node or NodeList Name",
		" ,                         !- Zone Air Exhaust Node or NodeList Name",
		" Spacex10 Node,            !- Zone Air Node Name",
		" Spacex10 Ret Node;        !- Zone Return Air Node Name",

		"ZoneHVAC:EquipmentList,",
		" Spacex10 Eq,              !- Name",
		" ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
		" Spacex10 ATU,                !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

		"ZoneHVAC:AirDistributionUnit,",
		"  Spacex10 ATU,  !- Name",
		"  Spacex10 In Node,  !- Air Distribution Unit Outlet Node Name",
		"  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
		"  Spacex10 Air Terminal;  !- Air Terminal Name",

		"AirTerminal:SingleDuct:VAV:NoReheat,",
		"  Spacex10 Air Terminal,   !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  Spacex10 In Node,        !- Air Outlet Node Name",
		"  Spacex10 ATU In Node,    !- Air Inlet Node Name",
		"  autosize,                !- Maximum Air Flow Rate {m3/s}",
		"  Constant,                !- Zone Minimum Air Flow Input Method",
		"  0.0,                     !- Constant Minimum Air Flow Fraction",
		"  ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
		"  ,                        !- Minimum Air Flow Fraction Schedule Name",
		"  ;                        !- Design Specification Outdoor Air Object Name",

		"ScheduleTypeLimits,",
		"  Any Number;              !- Name",

		"Schedule:Compact,",
		"  AvailSched,              !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 3",
		"  For: AllDays,            !- Field 4",
		"  Until: 24:00,1.0;        !- Field 5",

		"Schedule:Compact,",
		"  EquipSched,              !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 3",
		"  For: WinterDesignDay,    !- Field 4",
		"  Until: 24:00,0.0,        !- Field 5",
		"  For: AllOtherDays,       !- Field 4",
		"  Until: 24:00,1.0;        !- Field 5",

		"Schedule:Compact,",
		"  ActivityLevelSched,      !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 3",
		"  For: AllDays,            !- Field 4",
		"  Until: 24:00,120.0;      !- Field 5",

		"Schedule:Compact,",
		"  FanOpModeSchedule,       !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 1",
		"  For: AllDays,            !- Field 2",
		"  Until: 24:00,1.0;        !- Field 7",

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
		"  5,                       !- Minimum Value of y",
		"  40,                      !- Maximum Value of y",
		"  ,                        !- Minimum Curve Output",
		"  ,                        !- Maximum Curve Output",
		"  Temperature,             !- Input Unit Type for X",
		"  Temperature,             !- Input Unit Type for Y",
		"  Dimensionless;           !- Output Unit Type",

		"Curve:Cubic,",
		"  Cubic,                   !- Name",
		"  1.0,                     !- Coefficient1 Constant",
		"  0.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0,                       !- Coefficient4 x**3",
		"  5,                       !- Minimum Value of x",
		"  40,                      !- Maximum Value of x",
		"  ,                        !- Minimum Curve Output",
		"  ,                        !- Maximum Curve Output",
		"  Temperature,             !- Input Unit Type for X",
		"  Temperature;             !- Output Unit Type",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	OutputProcessor::TimeValue.allocate( 2 );
	DataGlobals::DDOnlySimulation = true;

	ManageSimulation(); // run the design day over the warmup period (24 hrs, 25 days)

	EXPECT_EQ( 10.0, ( Zone( 2 ).Volume * Zone( 2 ).Multiplier * Zone( 2 ).ListMultiplier ) / ( Zone( 1 ).Volume * Zone( 1 ).Multiplier * Zone( 1 ).ListMultiplier ) );

	DataGlobals::DoWeathSim = true; // flag to trick tabular reports to scan meters
	DataGlobals::KindOfSim = DataGlobals::ksRunPeriodWeather; // fake a weather run since a weather file can't be used (could it?)
	UpdateTabularReports( OutputReportTabular::stepTypeHVAC );

	EXPECT_NEAR( 1.86168, DataSizing::FinalSysSizing( 1 ).DesOutAirVolFlow, 0.0001 );

}

TEST_F( EnergyPlusFixture, AirloopHVAC_VentilationRateProcedure )
{
	// AUTHOR: R. Raustad, FSEC
	// DATE WRITTEN: Sep 2015

	std::string const idf_objects = delimited_string( {
		" Version,8.3;",
		" Output:Diagnostics, DisplayExtraWarnings;",
		" Timestep, 4;",
		" BUILDING, AirloopHVAC_VentilationRateProcedure, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
		" SimulationControl, YES, YES, NO, YES, NO;",

		"  Site:Location,",
		"    Miami Intl Ap FL USA TMY3 WMO=722020E,    !- Name",
		"    25.82,                 !- Latitude {deg}",
		"    -80.30,                !- Longitude {deg}",
		"    -5.00,                 !- Time Zone {hr}",
		"    11;                    !- Elevation {m}",

		"SizingPeriod:DesignDay,",
		" Miami Intl Ap Ann Clg .4% Condns DB/MCWB, !- Name",
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

		"Output:Table:SummaryReports,",
		"  AllSummaryAndSizingPeriod; !- Report 1 Name",

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

		"ZoneGroup,",
		" Zone Group,               !- Name",
		" Zone List,                !- Zone List Name",
		" 10;                       !- Zone List Multiplier",

		"ZoneList,",
		" Zone List,                !- Name",
		" Spacex10;                 !- Zone 1 Name",

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
		" Space DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name",
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
		" Spacex10 DSOA Design OA Spec, !- Design Specification Outdoor Air Object Name",
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
		" ActivityLevelSched;       !- Activity Level Schedule Name",

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
		" ActivityLevelSched;       !- Activity Level Schedule Name",

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

		"Schedule:Compact,",
		" OnSched,                  !- Name",
		" Fraction,                 !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0;        !- Field 26",

		"ScheduleTypeLimits,",
		" Fraction,                 !- Name",
		" 0.0,                      !- Lower Limit Value",
		" 1.0,                      !- Upper Limit Value",
		" CONTINUOUS;               !- Numeric Type",

		"Construction,",
		" INT-WALL-1,               !- Name",
		" GP02,                     !- Outside Layer",
		" AL21,                     !- Layer 2",
		" GP02;                     !- Layer 3",

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

		"Material:AirGap,",
		" AL21,                     !- Name",
		" 0.1570000;                !- Thermal Resistance{ m2 - K / W }",

		"Construction,",
		"FLOOR-SLAB-1,              !- Name",
		"CC03,                      !- Outside Layer",
		"CP01;                      !- Layer 2",

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

		"Material:NoMass,",
		" CP01,                     !- Name",
		" Rough,                    !- Roughness",
		" 0.3670000,                !- Thermal Resistance{ m2 - K / W }",
		" 0.9000000,                !- Thermal Absorptance",
		" 0.7500000,                !- Solar Absorptance",
		" 0.7500000;                !- Visible Absorptance",

		"Construction,",
		" CLNG-1,                   !- Name",
		" MAT-CLNG-1;               !- Outside Layer",

		"Material:NoMass,",
		" MAT-CLNG-1,               !- Name",
		" Rough,                    !- Roughness",
		" 0.652259290,              !- Thermal Resistance{ m2 - K / W }",
		" 0.65,                     !- Thermal Absorptance",
		" 0.65,                     !- Solar Absorptance",
		" 0.65;                     !- Visible Absorptance",

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

		"ZoneControl:Thermostat,",
		" Space Thermostat,         !- Name",
		" Space,                    !- Zone or ZoneList Name",
		" Dual Zone Control Type Sched,  !- Control Type Schedule Name",
		" ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
		" Space DualSPSched;        !- Control 1 Name",

		"ZoneControl:Thermostat,",
		" Spacex10 Thermostat,      !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
		" Dual Zone Control Type Sched,  !- Control Type Schedule Name",
		" ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
		" Space DualSPSched;        !- Control 1 Name",

		"Schedule:Compact,",
		" Dual Zone Control Type Sched,  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00,4;           !- Field 3",

		"ThermostatSetpoint:DualSetpoint,",
		" Space DualSPSched,        !- Name",
		" HTGSETP_SCH,              !- Heating Setpoint Temperature Schedule Name",
		" CLGSETP_SCH;              !- Cooling Setpoint Temperature Schedule Name",

		"Schedule:Compact,",
		" CLGSETP_SCH,              !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 19",
		" Until: 24:00,22.1;        !- Field 20",

		"Schedule:Compact,",
		" HTGSETP_SCH,              !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 22",
		" Until: 24:00, 21.9;       !- Field 23",

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

		"Sizing:System,",
		" DOAS,                     !- AirLoop Name",
		" VentilationRequirement,   !- Type of Load to Size On",
		" autosize,                 !- Design Outdoor Air Flow Rate {m3/s}",
		" 1.0,                      !- Central Heating Maximum System Air Flow Ratio",
		" 2,                        !- Preheat Design Temperature {C}",
		" 0.008,                    !- Preheat Design Humidity Ratio {kgWater/kgDryAir}",
		" 11,                       !- Precool Design Temperature {C}",
		" 0.008,                    !- Precool Design Humidity Ratio {kgWater/kgDryAir}",
		" 16,                       !- Central Cooling Design Supply Air Temperature {C}",
		" 12.2,                     !- Central Heating Design Supply Air Temperature {C}",
		" NonCoincident,            !- Type of Zone Sum to Use",
		" Yes,                      !- 100% Outdoor Air in Cooling",
		" Yes,                      !- 100% Outdoor Air in Heating",
		" 0.0103,                   !- Central Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		" 0.003,                    !- Central Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		" DesignDay,                !- Cooling Supply Air Flow Rate Method",
		" 0,                        !- Cooling Supply Air Flow Rate {m3/s}",
		" ,                         !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
		" ,                         !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
		" ,                         !- Cooling Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}",
		" DesignDay,                !- Heating Supply Air Flow Rate Method",
		" 0,                        !- Heating Supply Air Flow Rate {m3/s}",
		" ,                         !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
		" ,                         !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
		" ,                         !- Heating Fraction of Autosized Cooling Supply Air Flow Rate",
		" ,                         !- Heating Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}",
		" VentilationRateProcedure, !- System Outdoor Air Method",
		" 1.0,                      !- Zone Maximum Outdoor Air Fraction {dimensionless}",
		" CoolingDesignCapacity,    !- Cooling Design Capacity Method",
		" autosize,                 !- Cooling Design Capacity {W}",
		" ,                         !- Cooling Design Capacity Per Floor Area {W/m2}",
		" ,                         !- Fraction of Autosized Cooling Design Capacity",
		" HeatingDesignCapacity,    !- Heating Design Capacity Method",
		" autosize,                 !- Heating Design Capacity {W}",
		" ,                         !- Heating Design Capacity Per Floor Area {W/m2}",
		" ,                         !- Fraction of Autosized Heating Design Capacity",
		" OnOff;                    !- Central Cooling Capacity Control Method",


		"AirLoopHVAC,",
		"  DOAS,                    !- Name",
		"  ,                        !- Controller List Name",
		"  DOAS Availability Managers,  !- Availability Manager List Name",
		"  autosize,                !- Design Supply Air Flow Rate {m3/s}",
		"  DOAS Branches,           !- Branch List Name",
		"  ,                        !- Connector List Name",
		"  DOAS Air Loop Inlet,     !- Supply Side Inlet Node Name",
		"  DOAS Return Air Outlet,  !- Demand Side Outlet Node Name",
		"  DOAS Supply Path Inlet,  !- Demand Side Inlet Node Names",
		"  DOAS Supply Fan Outlet;  !- Supply Side Outlet Node Names",

		"BranchList,",
		"  DOAS Branches,           !- Name",
		"  DOAS Main Branch;        !- Branch 1 Name",

		"Branch,",
		"  DOAS Main Branch,        !- Name",
		"  autosize,                !- Maximum Flow Rate {m3/s}",
		"  ,                        !- Pressure Drop Curve Name",
		"  AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
		"  DOAS OA System,          !- Component 1 Name",
		"  DOAS Air Loop Inlet,     !- Component 1 Inlet Node Name",
		"  DOAS Mixed Air Outlet,   !- Component 1 Outlet Node Name",
		"  Passive,                 !- Component 1 Branch Control Type",
		"  CoilSystem:Cooling:DX,   !- Component 2 Object Type",
		"  DOAS Cooling Coil,       !- Component 2 Name",
		"  DOAS Mixed Air Outlet,   !- Component 2 Inlet Node Name",
		"  DOAS Cooling Coil Outlet,!- Component 2 Outlet Node Name",
		"  Passive,                 !- Component 2 Branch Control Type",
		"  Coil:Heating:Gas,        !- Component 2 Object Type",
		"  DOAS Heating Coil,       !- Component 2 Name",
		"  DOAS Cooling Coil Outlet,  !- Component 2 Inlet Node Name",
		"  DOAS Heating Coil Outlet,!- Component 2 Outlet Node Name",
		"  Passive,                 !- Component 2 Branch Control Type",
		"  Fan:VariableVolume,      !- Component 3 Object Type",
		"  DOAS Supply Fan,         !- Component 3 Name",
		"  DOAS Heating Coil Outlet,!- Component 3 Inlet Node Name",
		"  DOAS Supply Fan Outlet,  !- Component 3 Outlet Node Name",
		"  Active;                  !- Component 3 Branch Control Type",

		"AirLoopHVAC:SupplyPath,",
		"  DOAS Supply Path,        !- Name",
		"  DOAS Supply Path Inlet,  !- Supply Air Path Inlet Node Name",
		"  AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
		"  DOAS Zone Splitter;      !- Component 1 Name",

		"AirLoopHVAC:ZoneSplitter,",
		"  DOAS Zone Splitter,      !- Name",
		"  DOAS Supply Path Inlet,  !- Inlet Node Name",
		"  Space ATU In Node,  !- Outlet 1 Node Name",
		"  Spacex10 ATU In Node;  !- Outlet 27 Node Name",

		"AirLoopHVAC:ReturnPath,",
		"  DOAS Return Path,        !- Name",
		"  DOAS Return Air Outlet,  !- Return Air Path Outlet Node Name",
		"  AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
		"  DOAS Zone Mixer;         !- Component 1 Name",

		"AirLoopHVAC:ZoneMixer,",
		"  DOAS Zone Mixer,         !- Name",
		"  DOAS Return Air Outlet,  !- Outlet Node Name",
		"  Space Ret Node,          !- Inlet 1 Node Name",
		"  Spacex10 Ret Node;       !- Inlet 27 Node Name",

		"AvailabilityManagerAssignmentList,",
		"  DOAS Availability Managers,  !- Name",
		"  AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
		"  DOAS Availability;       !- Availability Manager 1 Name",

		"AvailabilityManager:Scheduled,",
		"  DOAS Availability,       !- Name",
		"  AvailSched;              !- Schedule Name",

		"NodeList,",
		"  DOAS Cooling Setpoint Nodes,  !- Name",
		"  DOAS Cooling Coil Outlet, !- Node 1 Name",
		"  DOAS Heating Coil Outlet; !- Node 1 Name",

		"Schedule:Compact,",
		"  Always 22,               !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 1",
		"  For: AllDays,            !- Field 2",
		"  Until: 24:00,22;         !- Field 3",

		"SetpointManager:Scheduled,",
		"  DOAS Cooling Supply Air Temp Manager,  !- Name",
		"  Temperature,             !- Control Variable",
		"  Always 22,               !- Schedule Name",
		"  DOAS Supply Fan Outlet;  !- Setpoint Node or NodeList Name",

		"SetpointManager:MixedAir,",
		"  DOAS Cooling Coil Air Temp Manager,  !- Name",
		"  Temperature,             !- Control Variable",
		"  DOAS Supply Fan Outlet,  !- Reference Setpoint Node Name",
		"  DOAS Cooling Coil Outlet,!- Fan Inlet Node Name",
		"  DOAS Supply Fan Outlet,  !- Fan Outlet Node Name",
		"  DOAS Cooling Setpoint Nodes;  !- Setpoint Node or NodeList Name",

		"CoilSystem:Cooling:DX,",
		"  DOAS Cooling Coil,       !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  DOAS Mixed Air Outlet,   !- DX Cooling Coil System Inlet Node Name",
		"  DOAS Cooling Coil Outlet,  !- DX Cooling Coil System Outlet Node Name",
		"  DOAS Cooling Coil Outlet,  !- DX Cooling Coil System Sensor Node Name",
		"  Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
		"  DOAS DX Cooling Coil;    !- Cooling Coil Name",

		"Coil:Cooling:DX:SingleSpeed,",
		"	DOAS DX Cooling Coil,   !- Name",
		" 	AvailSched,            !- Availability Schedule Name",
		"	autosize,              !- Gross Rated Total Cooling Capacity { W }",
		"	autosize,              !- Gross Rated Sensible Heat Ratio",
		"	4.40,                  !- Gross Rated Cooling COP { W / W }",
		"	autosize,              !- Rated Air Flow Rate { m3 / s }",
		"	,                      !- Rated Evaporator Fan Power Per Volume Flow Rate { W / ( m3 / s ) }",
		"	DOAS Mixed Air Outlet, !- Air Inlet Node Name",
		"	DOAS Cooling Coil Outlet,    !- Air Outlet Node Name",
		"	Biquadratic,           !- Total Cooling Capacity Function of Temperature Curve Name",
		"	Cubic,                 !- Total Cooling Capacity Function of Flow Fraction Curve Name",
		"	Biquadratic,           !- Energy Input Ratio Function of Temperature Curve Name",
		"	Cubic,                 !- Energy Input Ratio Function of Flow Fraction Curve Name",
		"	Cubic,                 !- Part Load Fraction Correlation Curve Name",
		"	0.0,                   !- Nominal Time for Condensate Removal to Begin",
		"	0.0,                   !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
		"	0.0,                   !- Maximum Cycling Rate",
		"	0.0,                   !- Latent Capacity Time Constant",
		"	Cooling Coil Condenser Inlet, !- Condenser Air Inlet Node Name",
		"	EvaporativelyCooled,   !- Condenser Type",
		"	0.0,                   !- Evaporative Condenser Effectiveness",
		"	,                      !- Evaporative Condenser Air Flow Rate",
		"	autosize,              !- Evaporative Condenser Pump Rated Power Consumption",
		"	0.0,                   !- Crankcase Heater Capacity",
		"	10.0;                  !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",

		"Coil:Heating:Gas,",
		"  DOAS Heating Coil,       !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  0.8,                     !- Gas Burner Efficiency",
		"  autosize,                !- Nominal Capacity {W}",
		"  DOAS Cooling Coil Outlet,  !- Air Inlet Node Name",
		"  DOAS Heating Coil Outlet,  !- Air Outlet Node Name",
		"  DOAS Heating Coil Outlet;  !- Temperature Setpoint Node Name",

		"Fan:VariableVolume,",
		"  DOAS Supply Fan,         !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  0.7,                     !- Fan Total Efficiency",
		"  1000,                    !- Pressure Rise {Pa}",
		"  autosize,                !- Maximum Flow Rate {m3/s}",
		"  Fraction,                !- Fan Power Minimum Flow Rate Input Method",
		"  0.0,                     !- Fan Power Minimum Flow Fraction",
		"  ,                        !- Fan Power Minimum Air Flow Rate {m3/s}",
		"  0.9,                     !- Motor Efficiency",
		"  1,                       !- Motor In Airstream Fraction",
		"  0.0015302446,            !- Fan Power Coefficient 1",
		"  0.0052080574,            !- Fan Power Coefficient 2",
		"  1.1086242,               !- Fan Power Coefficient 3",
		"  -0.11635563,             !- Fan Power Coefficient 4",
		"  0,                       !- Fan Power Coefficient 5",
		"  DOAS Heating Coil Outlet,!- Air Inlet Node Name",
		"  DOAS Supply Fan Outlet;  !- Air Outlet Node Name",

		"OutdoorAir:NodeList,",
		"  DOAS Outdoor Air Inlet,  !- Node or NodeList Name 1",
		"  Cooling Coil Condenser Inlet;  !- Node or NodeList Name 2",

		"AirLoopHVAC:OutdoorAirSystem,",
		"  DOAS OA System,          !- Name",
		"  DOAS OA System Controllers,  !- Controller List Name",
		"  DOAS OA System Equipment,!- Outdoor Air Equipment List Name",
		"  DOAS Availability Managers;  !- Availability Manager List Name",

		"AirLoopHVAC:ControllerList,",
		"  DOAS OA System Controllers,  !- Name",
		"  Controller:OutdoorAir,   !- Controller 1 Object Type",
		"  DOAS OA Controller;      !- Controller 1 Name",

		"AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
		"  DOAS OA System Equipment,!- Name",
		"  OutdoorAir:Mixer,        !- Component 1 Object Type",
		"  DOAS OA Mixing Box;      !- Component 1 Name",

		"OutdoorAir:Mixer,",
		"  DOAS OA Mixing Box,      !- Name",
		"  DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
		"  DOAS Outdoor Air Inlet,  !- Outdoor Air Stream Node Name",
		"  DOAS Relief Air Outlet,  !- Relief Air Stream Node Name",
		"  DOAS Air Loop Inlet;     !- Return Air Stream Node Name",

		"Controller:OutdoorAir,",
		"  DOAS OA Controller,      !- Name",
		"  DOAS Relief Air Outlet,  !- Relief Air Outlet Node Name",
		"  DOAS Air Loop Inlet,     !- Return Air Node Name",
		"  DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
		"  DOAS Outdoor Air Inlet,  !- Actuator Node Name",
		"  0,                       !- Minimum Outdoor Air Flow Rate {m3/s}",
		"  autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
		"  NoEconomizer,            !- Economizer Control Type",
		"  MinimumFlowWithBypass,   !- Economizer Control Action Type",
		"  ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
		"  ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
		"  ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
		"  ,                        !- Electronic Enthalpy Limit Curve Name",
		"  12.2,                    !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
		"  NoLockout,               !- Lockout Type",
		"  FixedMinimum,            !- Minimum Limit Type",
		"  ,                        !- Minimum Outdoor Air Schedule Name",
		"  ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
		"  ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
		"  DCVObject;               !- Mechanical Ventilation Controller Name",

		"Controller:MechanicalVentilation,",
		"  DCVObject,               !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  Yes,                     !- Demand Controlled Ventilation",
		"  VentilationRateProcedure,!- System Outdoor Air Method",
		"  ,                        !- Zone Maximum Outdoor Air Fraction {dimensionless}",
		"  Space,  !- Zone 1 Name",
		"  Space DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name 1",
		"  Space DSOA Design ADE Spec, !- Design Specification Zone Air Distribution Object Name 1",
		"  Spacex10,      !- Zone 2 Name",
		"  Spacex10 DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name 2",
		"  Space DSOA Design ADE Spec;  !- Design Specification Zone Air Distribution Object Name 2",

		"DesignSpecification:OutdoorAir,",
		"  Space DSOA Design OA Spec,  !- Name",
		"  sum,                     !- Outdoor Air Method",
		"  0.0,                     !- Outdoor Air Flow per Person {m3/s-person}",
		"  0.0009,                  !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
		"  0;                       !- Outdoor Air Flow per Zone {m3/s}",

		"DesignSpecification:OutdoorAir,",
		"  Spacex10 DSOA Design OA Spec,  !- Name",
		"  sum,                     !- Outdoor Air Method",
		"  0.008,                   !- Outdoor Air Flow per Person {m3/s-person}",
		"  0.0009,                  !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
		"  0;                       !- Outdoor Air Flow per Zone {m3/s}",

		"DesignSpecification:ZoneAirDistribution,",
		"  Space DSOA Design ADE Spec,  !- Name",
		"  1,                       !- Zone Air Distribution Effectiveness in Cooling Mode {dimensionless}",
		"  1;                       !- Zone Air Distribution Effectiveness in Heating Mode {dimensionless}",

		"ZoneHVAC:EquipmentConnections,",
		" Space,                    !- Zone Name",
		" Space Eq,                 !- Zone Conditioning Equipment List Name",
		" Space In Node,            !- Zone Air Inlet Node or NodeList Name",
		" ,                         !- Zone Air Exhaust Node or NodeList Name",
		" Space Node,               !- Zone Air Node Name",
		" Space Ret Node;           !- Zone Return Air Node Name",

		"ZoneHVAC:EquipmentList,",
		" Space Eq,                 !- Name",
		" ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
		" Space ATU,                   !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

		"ZoneHVAC:AirDistributionUnit,",
		"  Space ATU,  !- Name",
		"  Space In Node,  !- Air Distribution Unit Outlet Node Name",
		"  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
		"  Space Air Terminal;  !- Air Terminal Name",

		"AirTerminal:SingleDuct:VAV:NoReheat,",
		"  Space Air Terminal,      !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  Space In Node,           !- Air Outlet Node Name",
		"  Space ATU In Node,       !- Air Inlet Node Name",
		"  autosize,                !- Maximum Air Flow Rate {m3/s}",
		"  Constant,                !- Zone Minimum Air Flow Input Method",
		"  0.0,                     !- Constant Minimum Air Flow Fraction",
		"  ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
		"  ,                        !- Minimum Air Flow Fraction Schedule Name",
		"  ;                        !- Design Specification Outdoor Air Object Name",

		"ZoneHVAC:EquipmentConnections,",
		" Spacex10,                 !- Zone Name",
		" Spacex10 Eq,              !- Zone Conditioning Equipment List Name",
		" Spacex10 In Node,         !- Zone Air Inlet Node or NodeList Name",
		" ,                         !- Zone Air Exhaust Node or NodeList Name",
		" Spacex10 Node,            !- Zone Air Node Name",
		" Spacex10 Ret Node;        !- Zone Return Air Node Name",

		"ZoneHVAC:EquipmentList,",
		" Spacex10 Eq,              !- Name",
		" ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
		" Spacex10 ATU,                !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

		"ZoneHVAC:AirDistributionUnit,",
		"  Spacex10 ATU,  !- Name",
		"  Spacex10 In Node,  !- Air Distribution Unit Outlet Node Name",
		"  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
		"  Spacex10 Air Terminal;  !- Air Terminal Name",

		"AirTerminal:SingleDuct:VAV:NoReheat,",
		"  Spacex10 Air Terminal,   !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  Spacex10 In Node,        !- Air Outlet Node Name",
		"  Spacex10 ATU In Node,    !- Air Inlet Node Name",
		"  autosize,                !- Maximum Air Flow Rate {m3/s}",
		"  Constant,                !- Zone Minimum Air Flow Input Method",
		"  0.0,                     !- Constant Minimum Air Flow Fraction",
		"  ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
		"  ,                        !- Minimum Air Flow Fraction Schedule Name",
		"  ;                        !- Design Specification Outdoor Air Object Name",

		"ScheduleTypeLimits,",
		"  Any Number;              !- Name",

		"Schedule:Compact,",
		"  AvailSched,              !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 3",
		"  For: AllDays,            !- Field 4",
		"  Until: 24:00,1.0;        !- Field 5",

		"Schedule:Compact,",
		"  EquipSched,              !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 3",
		"  For: WinterDesignDay,    !- Field 4",
		"  Until: 24:00,0.0,        !- Field 5",
		"  For: AllOtherDays,       !- Field 4",
		"  Until: 24:00,1.0;        !- Field 5",

		"Schedule:Compact,",
		"  ActivityLevelSched,      !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 3",
		"  For: AllDays,            !- Field 4",
		"  Until: 24:00,120.0;      !- Field 5",

		"Schedule:Compact,",
		"  FanOpModeSchedule,       !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 1",
		"  For: AllDays,            !- Field 2",
		"  Until: 24:00,1.0;        !- Field 7",

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
		"  5,                       !- Minimum Value of y",
		"  40,                      !- Maximum Value of y",
		"  ,                        !- Minimum Curve Output",
		"  ,                        !- Maximum Curve Output",
		"  Temperature,             !- Input Unit Type for X",
		"  Temperature,             !- Input Unit Type for Y",
		"  Dimensionless;           !- Output Unit Type",

		"Curve:Cubic,",
		"  Cubic,                   !- Name",
		"  1.0,                     !- Coefficient1 Constant",
		"  0.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0,                       !- Coefficient4 x**3",
		"  5,                       !- Minimum Value of x",
		"  40,                      !- Maximum Value of x",
		"  ,                        !- Minimum Curve Output",
		"  ,                        !- Maximum Curve Output",
		"  Temperature,             !- Input Unit Type for X",
		"  Temperature;             !- Output Unit Type",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	OutputProcessor::TimeValue.allocate( 2 );
	DataGlobals::DDOnlySimulation = true;

	ManageSimulation(); // run the design day over the warmup period (24 hrs, 25 days)

	EXPECT_EQ( 10.0, ( Zone( 2 ).Volume * Zone( 2 ).Multiplier * Zone( 2 ).ListMultiplier ) / ( Zone( 1 ).Volume * Zone( 1 ).Multiplier * Zone( 1 ).ListMultiplier ) );

	DataGlobals::DoWeathSim = true; // flag to trick tabular reports to scan meters
	DataGlobals::KindOfSim = DataGlobals::ksRunPeriodWeather; // fake a weather run since a weather file can't be used (could it?)
	UpdateTabularReports( OutputReportTabular::stepTypeHVAC );

	EXPECT_NEAR( 1.86168, DataSizing::FinalSysSizing( 1 ).DesOutAirVolFlow, 0.0001 );

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


TEST_F( EnergyPlusFixture, FinAndOverhangCount )
{
	// based on 4ZoneWithShading_Simple_2.idf
	std::string const idf_objects = delimited_string( {

		"  Version,8.5;                                                                                     ",
		"                                                                                                   ",
		" Output:Diagnostics, DisplayExtraWarnings;",
		"  Timestep,4;                                                                                      ",
		"                                                                                                   ",
		"  Building,                                                                                        ",
		"    BUILDING #1,             !- Name                                                               ",
		"    0.0000000E+00,           !- North Axis {deg}                                                   ",
		"    Suburbs,                 !- Terrain                                                            ",
		"    3.9999999E-02,           !- Loads Convergence Tolerance Value                                  ",
		"    0.4000000,               !- Temperature Convergence Tolerance Value {deltaC}                   ",
		"    FullExterior,            !- Solar Distribution                                                 ",
		"    25,                      !- Maximum Number of Warmup Days                                      ",
		"    6;                       !- Minimum Number of Warmup Days                                      ",
		"                                                                                                   ",
		"  SurfaceConvectionAlgorithm:Inside,Simple;                                                        ",
		"                                                                                                   ",
		"  SurfaceConvectionAlgorithm:Outside,SimpleCombined;                                               ",
		"                                                                                                   ",
		"  HeatBalanceAlgorithm,ConductionTransferFunction;                                                 ",
		"                                                                                                   ",
		"  SimulationControl,                                                                               ",
		"    No,                     !- Do Zone Sizing Calculation                                         ",
		"    No,                      !- Do System Sizing Calculation                                       ",
		"    No,                      !- Do Plant Sizing Calculation                                        ",
		"    Yes,                     !- Run Simulation for Sizing Periods                                  ",
		"    No;                      !- Run Simulation for Weather File Run Periods                        ",
		"                                                                                                   ",
		"  Site:Location,                                                                                   ",
		"    Chicago Ohare Intl Ap IL USA WMO=725300,  !- Name                                              ",
		"    41.99,                   !- Latitude {deg}                                                     ",
		"    -87.91,                  !- Longitude {deg}                                                    ",
		"    -6.00,                   !- Time Zone {hr}                                                     ",
		"    205.00;                  !- Elevation {m}                                                      ",
		"                                                                                                   ",
		"  SizingPeriod:DesignDay,                                                                          ",
		"    Chicago Ohare Intl Ap Ann Htg 99% Condns DB,  !- Name                                          ",
		"    1,                       !- Month                                                              ",
		"    21,                      !- Day of Month                                                       ",
		"    WinterDesignDay,         !- Day Type                                                           ",
		"    -16.6,                   !- Maximum Dry-Bulb Temperature {C}                                   ",
		"    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}                          ",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Type                           ",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name              ",
		"    Wetbulb,                 !- Humidity Condition Type                                            ",
		"    -16.6,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}                        ",
		"    ,                        !- Humidity Condition Day Schedule Name                               ",
		"    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}              ",
		"    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}                                ",
		"    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}                          ",
		"    98886.,                  !- Barometric Pressure {Pa}                                           ",
		"    4.9,                     !- Wind Speed {m/s}                                                   ",
		"    270,                     !- Wind Direction {deg}                                               ",
		"    No,                      !- Rain Indicator                                                     ",
		"    No,                      !- Snow Indicator                                                     ",
		"    No,                      !- Daylight Saving Time Indicator                                     ",
		"    ASHRAEClearSky,          !- Solar Model Indicator                                              ",
		"    ,                        !- Beam Solar Day Schedule Name                                       ",
		"    ,                        !- Diffuse Solar Day Schedule Name                                    ",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensio",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimen",
		"    0.00;                    !- Sky Clearness                                                      ",
		"                                                                                                   ",
		"  SizingPeriod:DesignDay,                                                                          ",
		"    Chicago Ohare Intl Ap Ann Clg 1% Condns DB=>MWB,  !- Name                                      ",
		"    7,                       !- Month                                                              ",
		"    21,                      !- Day of Month                                                       ",
		"    SummerDesignDay,         !- Day Type                                                           ",
		"    31.6,                    !- Maximum Dry-Bulb Temperature {C}                                   ",
		"    10.5,                    !- Daily Dry-Bulb Temperature Range {deltaC}                          ",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Type                           ",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name              ",
		"    Wetbulb,                 !- Humidity Condition Type                                            ",
		"    23,                      !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}                        ",
		"    ,                        !- Humidity Condition Day Schedule Name                               ",
		"    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}              ",
		"    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}                                ",
		"    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}                          ",
		"    98886.,                  !- Barometric Pressure {Pa}                                           ",
		"    5.2,                     !- Wind Speed {m/s}                                                   ",
		"    230,                     !- Wind Direction {deg}                                               ",
		"    No,                      !- Rain Indicator                                                     ",
		"    No,                      !- Snow Indicator                                                     ",
		"    No,                      !- Daylight Saving Time Indicator                                     ",
		"    ASHRAEClearSky,          !- Solar Model Indicator                                              ",
		"    ,                        !- Beam Solar Day Schedule Name                                       ",
		"    ,                        !- Diffuse Solar Day Schedule Name                                    ",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensio",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimen",
		"    1.00;                    !- Sky Clearness                                                      ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    A2 - 4 IN DENSE FACE BRICK,  !- Name                                                           ",
		"    Rough,                   !- Roughness                                                          ",
		"    0.1014984,               !- Thickness {m}                                                      ",
		"    1.245296,                !- Conductivity {W/m-K}                                               ",
		"    2082.400,                !- Density {kg/m3}                                                    ",
		"    920.4800,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.9300000,               !- Solar Absorptance                                                  ",
		"    0.9300000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    B3 - 2 IN INSULATION,    !- Name                                                               ",
		"    VeryRough,               !- Roughness                                                          ",
		"    5.0901599E-02,           !- Thickness {m}                                                      ",
		"    4.3239430E-02,           !- Conductivity {W/m-K}                                               ",
		"    32.03693,                !- Density {kg/m3}                                                    ",
		"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.5000000,               !- Solar Absorptance                                                  ",
		"    0.5000000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    C2 - 4 IN LW CONCRETE BLOCK,  !- Name                                                          ",
		"    MediumRough,             !- Roughness                                                          ",
		"    0.1014984,               !- Thickness {m}                                                      ",
		"    0.3805070,               !- Conductivity {W/m-K}                                               ",
		"    608.7016,                !- Density {kg/m3}                                                    ",
		"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.6500000,               !- Solar Absorptance                                                  ",
		"    0.6500000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name                                                   ",
		"    Smooth,                  !- Roughness                                                          ",
		"    1.9050000E-02,           !- Thickness {m}                                                      ",
		"    0.7264224,               !- Conductivity {W/m-K}                                               ",
		"    1601.846,                !- Density {kg/m3}                                                    ",
		"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.9200000,               !- Solar Absorptance                                                  ",
		"    0.9200000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"   ! (R=.0472,TRANS=.80,VERY SMOOTH,GLASS), from 6mm clear                                         ",
		"                                                                                                   ",
		"  WindowMaterial:Glazing,                                                                          ",
		"    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name                                                         ",
		"    SpectralAverage,         !- Optical Data Type                                                  ",
		"    ,                        !- Window Glass Spectral Data Set Name                                ",
		"    6.0000001E-03,           !- Thickness {m}                                                      ",
		"    0.7750000,               !- Solar Transmittance at Normal Incidence                            ",
		"    7.1000002E-02,           !- Front Side Solar Reflectance at Normal Incidence                   ",
		"    7.1000002E-02,           !- Back Side Solar Reflectance at Normal Incidence                    ",
		"    0.8810000,               !- Visible Transmittance at Normal Incidence                          ",
		"    7.9999998E-02,           !- Front Side Visible Reflectance at Normal Incidence                 ",
		"    7.9999998E-02,           !- Back Side Visible Reflectance at Normal Incidence                  ",
		"    0.0000000E+00,           !- Infrared Transmittance at Normal Incidence                         ",
		"    0.8400000,               !- Front Side Infrared Hemispherical Emissivity                       ",
		"    0.8400000,               !- Back Side Infrared Hemispherical Emissivity                        ",
		"    0.9000000;               !- Conductivity {W/m-K}                                               ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    WOOD - HARDWOOD 1 / 8 IN,!- Name                                                               ",
		"    MediumSmooth,            !- Roughness                                                          ",
		"    3.1699201E-03,           !- Thickness {m}                                                      ",
		"    0.1591211,               !- Conductivity {W/m-K}                                               ",
		"    720.8308,                !- Density {kg/m3}                                                    ",
		"    1255.200,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.7800000,               !- Solar Absorptance                                                  ",
		"    0.7800000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material:AirGap,                                                                                 ",
		"    B1 - AIRSPACE RESISTANCE,!- Name                                                               ",
		"    0.1603675;               !- Thermal Resistance {m2-K/W}                                        ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    C7 - 8 IN LW CONCRETE BLOCK,  !- Name                                                          ",
		"    Rough,                   !- Roughness                                                          ",
		"    0.2033016,               !- Thickness {m}                                                      ",
		"    0.5707605,               !- Conductivity {W/m-K}                                               ",
		"    608.7016,                !- Density {kg/m3}                                                    ",
		"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.6500000,               !- Solar Absorptance                                                  ",
		"    0.6500000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    E5 - ACOUSTIC TILE,      !- Name                                                               ",
		"    MediumSmooth,            !- Roughness                                                          ",
		"    1.9050000E-02,           !- Thickness {m}                                                      ",
		"    6.0535200E-02,           !- Conductivity {W/m-K}                                               ",
		"    480.5539,                !- Density {kg/m3}                                                    ",
		"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.3200000,               !- Solar Absorptance                                                  ",
		"    0.3200000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material:AirGap,                                                                                 ",
		"    E4 - CEILING AIRSPACE,   !- Name                                                               ",
		"    0.1762280;               !- Thermal Resistance {m2-K/W}                                        ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    C5 - 4 IN HW CONCRETE,   !- Name                                                               ",
		"    MediumRough,             !- Roughness                                                          ",
		"    0.1014984,               !- Thickness {m}                                                      ",
		"    1.729577,                !- Conductivity {W/m-K}                                               ",
		"    2242.585,                !- Density {kg/m3}                                                    ",
		"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.6500000,               !- Solar Absorptance                                                  ",
		"    0.6500000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    C10 - 8 IN HW CONCRETE,  !- Name                                                               ",
		"    MediumRough,             !- Roughness                                                          ",
		"    0.2033016,               !- Thickness {m}                                                      ",
		"    1.729577,                !- Conductivity {W/m-K}                                               ",
		"    2242.585,                !- Density {kg/m3}                                                    ",
		"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.6500000,               !- Solar Absorptance                                                  ",
		"    0.6500000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    E2 - 1 / 2 IN SLAG OR STONE,  !- Name                                                          ",
		"    Rough,                   !- Roughness                                                          ",
		"    1.2710161E-02,           !- Thickness {m}                                                      ",
		"    1.435549,                !- Conductivity {W/m-K}                                               ",
		"    881.0155,                !- Density {kg/m3}                                                    ",
		"    1673.600,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.5500000,               !- Solar Absorptance                                                  ",
		"    0.5500000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name                                                      ",
		"    Rough,                   !- Roughness                                                          ",
		"    9.5402403E-03,           !- Thickness {m}                                                      ",
		"    0.1902535,               !- Conductivity {W/m-K}                                               ",
		"    1121.292,                !- Density {kg/m3}                                                    ",
		"    1673.600,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.7500000,               !- Solar Absorptance                                                  ",
		"    0.7500000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    B6 - 2 IN DENSE INSULATION,  !- Name                                                           ",
		"    VeryRough,               !- Roughness                                                          ",
		"    5.0901599E-02,           !- Thickness {m}                                                      ",
		"    4.3239430E-02,           !- Conductivity {W/m-K}                                               ",
		"    91.30524,                !- Density {kg/m3}                                                    ",
		"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.5000000,               !- Solar Absorptance                                                  ",
		"    0.5000000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Material,                                                                                        ",
		"    C12 - 2 IN HW CONCRETE,  !- Name                                                               ",
		"    MediumRough,             !- Roughness                                                          ",
		"    5.0901599E-02,           !- Thickness {m}                                                      ",
		"    1.729577,                !- Conductivity {W/m-K}                                               ",
		"    2242.585,                !- Density {kg/m3}                                                    ",
		"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
		"    0.9000000,               !- Thermal Absorptance                                                ",
		"    0.6500000,               !- Solar Absorptance                                                  ",
		"    0.6500000;               !- Visible Absorptance                                                ",
		"                                                                                                   ",
		"  Construction,                                                                                    ",
		"    EXTERIOR,                !- Name                                                               ",
		"    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer                                                  ",
		"    B3 - 2 IN INSULATION,    !- Layer 2                                                            ",
		"    C2 - 4 IN LW CONCRETE BLOCK,  !- Layer 3                                                       ",
		"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4                                                ",
		"                                                                                                   ",
		"  Construction,                                                                                    ",
		"    SINGLE PANE HW WINDOW,   !- Name                                                               ",
		"    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer                                                ",
		"                                                                                                   ",
		"  Construction,                                                                                    ",
		"    HOLLOW WOOD DOOR,        !- Name                                                               ",
		"    WOOD - HARDWOOD 1 / 8 IN,!- Outside Layer                                                      ",
		"    B1 - AIRSPACE RESISTANCE,!- Layer 2                                                            ",
		"    WOOD - HARDWOOD 1 / 8 IN;!- Layer 3                                                            ",
		"                                                                                                   ",
		"  Construction,                                                                                    ",
		"    INTERIOR,                !- Name                                                               ",
		"    C7 - 8 IN LW CONCRETE BLOCK;  !- Outside Layer                                                 ",
		"                                                                                                   ",
		"  Construction,                                                                                    ",
		"    FLOOR,                   !- Name                                                               ",
		"    E5 - ACOUSTIC TILE,      !- Outside Layer                                                      ",
		"    E4 - CEILING AIRSPACE,   !- Layer 2                                                            ",
		"    C5 - 4 IN HW CONCRETE;   !- Layer 3                                                            ",
		"                                                                                                   ",
		"  Construction,                                                                                    ",
		"    CEILING34,               !- Name                                                               ",
		"    C10 - 8 IN HW CONCRETE;  !- Outside Layer                                                      ",
		"                                                                                                   ",
		"  Construction,                                                                                    ",
		"    FLOOR34,                 !- Name                                                               ",
		"    C10 - 8 IN HW CONCRETE;  !- Outside Layer                                                      ",
		"                                                                                                   ",
		"  Construction,                                                                                    ",
		"    ROOF31,                  !- Name                                                               ",
		"    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer                                                 ",
		"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2                                                   ",
		"    B6 - 2 IN DENSE INSULATION,  !- Layer 3                                                        ",
		"    C12 - 2 IN HW CONCRETE;  !- Layer 4                                                            ",
		"                                                                                                   ",
		"  ScheduleTypeLimits,                                                                              ",
		"    Any Number;              !- Name                                                               ",
		"                                                                                                   ",
		"  ScheduleTypeLimits,                                                                              ",
		"    Fraction,                !- Name                                                               ",
		"    0.0,                     !- Lower Limit Value                                                  ",
		"    1.0,                     !- Upper Limit Value                                                  ",
		"    CONTINUOUS;              !- Numeric Type                                                       ",
		"                                                                                                   ",
		"  Site:GroundTemperature:BuildingSurface,20,20,20,20,20,20,20,20,20,20,20,20;                      ",
		"                                                                                                   ",
		"  Zone,                                                                                            ",
		"    ZONE 1,                  !- Name                                                               ",
		"    0,                       !- Direction of Relative North {deg}                                  ",
		"    0,                       !- X Origin {m}                                                       ",
		"    0,                       !- Y Origin {m}                                                       ",
		"    -10,                     !- Z Origin {m}                                                       ",
		"    1,                       !- Type                                                               ",
		"    1,                       !- Multiplier                                                         ",
		"    AutoCalculate,           !- Ceiling Height {m}                                                 ",
		"    AutoCalculate;           !- Volume {m3}                                                        ",
		"                                                                                                   ",
		"  Zone,                                                                                            ",
		"    ZONE 2,                  !- Name                                                               ",
		"    0,                       !- Direction of Relative North {deg}                                  ",
		"    20,                      !- X Origin {m}                                                       ",
		"    0,                       !- Y Origin {m}                                                       ",
		"    -10,                     !- Z Origin {m}                                                       ",
		"    1,                       !- Type                                                               ",
		"    1,                       !- Multiplier                                                         ",
		"    AutoCalculate,           !- Ceiling Height {m}                                                 ",
		"    AutoCalculate;           !- Volume {m3}                                                        ",
		"                                                                                                   ",
		"  Zone,                                                                                            ",
		"    ZONE 3,                  !- Name                                                               ",
		"    0,                       !- Direction of Relative North {deg}                                  ",
		"    0,                       !- X Origin {m}                                                       ",
		"    0,                       !- Y Origin {m}                                                       ",
		"    0,                       !- Z Origin {m}                                                       ",
		"    1,                       !- Type                                                               ",
		"    1,                       !- Multiplier                                                         ",
		"    AutoCalculate,           !- Ceiling Height {m}                                                 ",
		"    AutoCalculate;           !- Volume {m3}                                                        ",
		"                                                                                                   ",
		"  Zone,                                                                                            ",
		"    ZONE 4,                  !- Name                                                               ",
		"    0,                       !- Direction of Relative North {deg}                                  ",
		"    20,                      !- X Origin {m}                                                       ",
		"    0,                       !- Y Origin {m}                                                       ",
		"    0,                       !- Z Origin {m}                                                       ",
		"    1,                       !- Type                                                               ",
		"    1,                       !- Multiplier                                                         ",
		"    AutoCalculate,           !- Ceiling Height {m}                                                 ",
		"    AutoCalculate;           !- Volume {m3}                                                        ",
		"                                                                                                   ",
		"  GlobalGeometryRules,                                                                             ",
		"    UpperLeftCorner,         !- Starting Vertex Position                                           ",
		"    CounterClockWise,        !- Vertex Entry Direction                                             ",
		"    Relative,                !- Coordinate System                                                  ",
		"    ,                        !- Daylighting Reference Point Coordinate System                      ",
		"    Relative;                !- Rectangular Surface Coordinate System                              ",
		"                                                                                                   ",
		"  Shading:Building,                                                                                ",
		"    Bushes-East,             !- Name                                                               ",
		"    90,                      !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    45,                      !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    50,                      !- Length {m}                                                         ",
		"    1;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Shading:Site,                                                                                    ",
		"    Bushes-North,            !- Name                                                               ",
		"    0,                       !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    45,                      !- Starting X Coordinate {m}                                          ",
		"    50,                      !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    50,                      !- Length {m}                                                         ",
		"    1;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Shading:Site,                                                                                    ",
		"    Bushes-West,             !- Name                                                               ",
		"    270,                     !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    -5,                      !- Starting X Coordinate {m}                                          ",
		"    50,                      !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    50,                      !- Length {m}                                                         ",
		"    1;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Exterior,                                                                                   ",
		"    Zn001:Wall001,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 1,                  !- Zone Name                                                          ",
		"    180,                     !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Window,                                                                                          ",
		"    Zn001:Wall001:Win001,    !- Name                                                               ",
		"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
		"    Zn001:Wall001,           !- Building Surface Name                                              ",
		"    ,                        !- Shading Control Name                                               ",
		"    ,                        !- Frame and Divider Name                                             ",
		"    1,                       !- Multiplier                                                         ",
		"    4,                       !- Starting X Coordinate {m}                                          ",
		"    3,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  GlazedDoor,                                                                                      ",
		"    Zn001:Wall001:Door001,   !- Name                                                               ",
		"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
		"    Zn001:Wall001,           !- Building Surface Name                                              ",
		"    ,                        !- Shading Control Name                                               ",
		"    ,                        !- Frame and Divider Name                                             ",
		"    1,                       !- Multiplier                                                         ",
		"    14,                      !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Shading:Overhang:Projection,                                                                     ",
		"    Zn001:Wall001:Win001:Shade001,  !- Name                                                        ",
		"    Zn001:Wall001:Win001,    !- Window or Door Name                                                ",
		"    .7,                      !- Height above Window or Door {m}                                    ",
		"    90,                      !- Tilt Angle from Window/Door {deg}                                  ",
		"    .2,                      !- Left extension from Window/Door Width {m}                          ",
		"    .2,                      !- Right extension from Window/Door Width {m}                         ",
		"    .6;                      !- Depth as Fraction of Window/Door Height {dimensionless}            ",
		"                                                                                                   ",
		"  Shading:Overhang,                                                                                ",
		"    Zn001:Wall001:Door001:Shade001,  !- Name                                                       ",
		"    Zn001:Wall001:Door001,   !- Window or Door Name                                                ",
		"    .6,                      !- Height above Window or Door {m}                                    ",
		"    90,                      !- Tilt Angle from Window/Door {deg}                                  ",
		"    0,                       !- Left extension from Window/Door Width {m}                          ",
		"    0,                       !- Right extension from Window/Door Width {m}                         ",
		"    3;                       !- Depth {m}                                                          ",
		"                                                                                                   ",
		"  Shading:Fin:Projection,                                                                          ",
		"    Zn001:Wall001:Shade003,  !- Name                                                               ",
		"    Zn001:Wall001:Win001,    !- Window or Door Name                                                ",
		"    .1,                      !- Left Extension from Window/Door {m}                                ",
		"    .1,                      !- Left Distance Above Top of Window {m}                              ",
		"    .1,                      !- Left Distance Below Bottom of Window {m}                           ",
		"    90,                      !- Left Tilt Angle from Window/Door {deg}                             ",
		"    .6,                      !- Left Depth as Fraction of Window/Door Width {dimensionless}        ",
		"    .1,                      !- Right Extension from Window/Door {m}                               ",
		"    .1,                      !- Right Distance Above Top of Window {m}                             ",
		"    .1,                      !- Right Distance Below Bottom of Window {m}                          ",
		"    90,                      !- Right Tilt Angle from Window/Door {deg}                            ",
		"    .6;                      !- Right Depth as Fraction of Window/Door Width {dimensionless}       ",
		"                                                                                                   ",
		"  Wall:Underground,                                                                                ",
		"    Zn001:Wall002,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 1,                  !- Zone Name                                                          ",
		"    0,                       !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    20,                      !- Starting X Coordinate {m}                                          ",
		"    20,                      !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Underground,                                                                                ",
		"    Zn001:Wall003,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 1,                  !- Zone Name                                                          ",
		"    270,                     !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    20,                      !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Door,                                                                                            ",
		"    Zn001:Wall001:Door002,   !- Name                                                               ",
		"    HOLLOW WOOD DOOR,        !- Construction Name                                                  ",
		"    Zn001:Wall003,           !- Building Surface Name                                              ",
		"    1,                       !- Multiplier                                                         ",
		"    14,                      !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Adiabatic,                                                                                  ",
		"    Zn001:Wall004,           !- Name                                                               ",
		"    INTERIOR,                !- Construction Name                                                  ",
		"    ZONE 1,                  !- Zone Name                                                          ",
		"    90,                      !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    20,                      !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Floor:GroundContact,                                                                             ",
		"    Zn001:Flr001,            !- Name                                                               ",
		"    FLOOR,                   !- Construction Name                                                  ",
		"    ZONE 1,                  !- Zone Name                                                          ",
		"    90,                      !- Azimuth Angle {deg}                                                ",
		"    180,                     !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    20;                      !- Width {m}                                                          ",
		"                                                                                                   ",
		"  Ceiling:Adiabatic,                                                                               ",
		"    Zn001:Roof001,           !- Name                                                               ",
		"    CEILING34,               !- Construction Name                                                  ",
		"    ZONE 1,                  !- Zone Name                                                          ",
		"    180,                     !- Azimuth Angle {deg}                                                ",
		"    0,                       !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    10,                      !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    20;                      !- Width {m}                                                          ",
		"                                                                                                   ",
		"  Wall:Exterior,                                                                                   ",
		"    Zn002:Wall001,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 2,                  !- Zone Name                                                          ",
		"    180,                     !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Window,                                                                                          ",
		"    Zn002:Wall001:Win001,    !- Name                                                               ",
		"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
		"    Zn002:Wall001,           !- Building Surface Name                                              ",
		"    ,                        !- Shading Control Name                                               ",
		"    ,                        !- Frame and Divider Name                                             ",
		"    1,                       !- Multiplier                                                         ",
		"    4,                       !- Starting X Coordinate {m}                                          ",
		"    3,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Shading:Overhang,                                                                                ",
		"    Zn002:Wall001:Win001:Shade001,  !- Name                                                        ",
		"    Zn002:Wall001:Win001,    !- Window or Door Name                                                ",
		"    .7,                      !- Height above Window or Door {m}                                    ",
		"    90,                      !- Tilt Angle from Window/Door {deg}                                  ",
		"    0,                       !- Left extension from Window/Door Width {m}                          ",
		"    0,                       !- Right extension from Window/Door Width {m}                         ",
		"    3;                       !- Depth {m}                                                          ",
		"                                                                                                   ",
		"  Window,                                                                                          ",
		"    Zn002:Wall001:Win002,    !- Name                                                               ",
		"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
		"    Zn002:Wall001,           !- Building Surface Name                                              ",
		"    ,                        !- Shading Control Name                                               ",
		"    ,                        !- Frame and Divider Name                                             ",
		"    1,                       !- Multiplier                                                         ",
		"    10,                      !- Starting X Coordinate {m}                                          ",
		"    3,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Underground,                                                                                ",
		"    Zn002:Wall002,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 2,                  !- Zone Name                                                          ",
		"    90,                      !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    20,                      !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Underground,                                                                                ",
		"    Zn002:Wall003,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 2,                  !- Zone Name                                                          ",
		"    0,                       !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    20,                      !- Starting X Coordinate {m}                                          ",
		"    20,                      !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Adiabatic,                                                                                  ",
		"    Zn002:Wall004,           !- Name                                                               ",
		"    INTERIOR,                !- Construction Name                                                  ",
		"    ZONE 2,                  !- Zone Name                                                          ",
		"    270,                     !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    20,                      !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Floor:GroundContact,                                                                             ",
		"    Zn002:Flr001,            !- Name                                                               ",
		"    FLOOR,                   !- Construction Name                                                  ",
		"    ZONE 2,                  !- Zone Name                                                          ",
		"    90,                      !- Azimuth Angle {deg}                                                ",
		"    180,                     !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    20;                      !- Width {m}                                                          ",
		"                                                                                                   ",
		"  Ceiling:Adiabatic,                                                                               ",
		"    Zn002:Roof001,           !- Name                                                               ",
		"    CEILING34,               !- Construction Name                                                  ",
		"    ZONE 2,                  !- Zone Name                                                          ",
		"    180,                     !- Azimuth Angle {deg}                                                ",
		"    0,                       !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    10,                      !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    20;                      !- Width {m}                                                          ",
		"                                                                                                   ",
		"  Wall:Exterior,                                                                                   ",
		"    Zn003:Wall001,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 3,                  !- Zone Name                                                          ",
		"    180,                     !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Window,                                                                                          ",
		"    Zn003:Wall001:Win001,    !- Name                                                               ",
		"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
		"    Zn003:Wall001,           !- Building Surface Name                                              ",
		"    ,                        !- Shading Control Name                                               ",
		"    ,                        !- Frame and Divider Name                                             ",
		"    1,                       !- Multiplier                                                         ",
		"    8,                       !- Starting X Coordinate {m}                                          ",
		"    3,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Exterior,                                                                                   ",
		"    Zn003:Wall002,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 3,                  !- Zone Name                                                          ",
		"    0,                       !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    20,                      !- Starting X Coordinate {m}                                          ",
		"    20,                      !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Exterior,                                                                                   ",
		"    Zn003:Wall003,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 3,                  !- Zone Name                                                          ",
		"    270,                     !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    20,                      !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Interzone,                                                                                  ",
		"    Zn003:Wall004,           !- Name                                                               ",
		"    INTERIOR,                !- Construction Name                                                  ",
		"    ZONE 3,                  !- Zone Name                                                          ",
		"    ZONE 4,                  !- Outside Boundary Condition Object                                  ",
		"    90,                      !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    20,                      !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  GlazedDoor:Interzone,                                                                            ",
		"    Zn003:Wall004:Door002,   !- Name                                                               ",
		"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
		"    Zn003:Wall004,           !- Building Surface Name                                              ",
		"    ZONE 4,                  !- Outside Boundary Condition Object                                  ",
		"    1,                       !- Multiplier                                                         ",
		"    10,                      !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Window:Interzone,                                                                                ",
		"    Zn003:Wall004:Win001,    !- Name                                                               ",
		"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
		"    Zn003:Wall004,           !- Building Surface Name                                              ",
		"    ZONE 4,                  !- Outside Boundary Condition Object                                  ",
		"    1,                       !- Multiplier                                                         ",
		"    4,                       !- Starting X Coordinate {m}                                          ",
		"    3,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Door:Interzone,                                                                                  ",
		"    Zn003:Wall004:Door001,   !- Name                                                               ",
		"    HOLLOW WOOD DOOR,        !- Construction Name                                                  ",
		"    Zn003:Wall004,           !- Building Surface Name                                              ",
		"    ZONE 4,                  !- Outside Boundary Condition Object                                  ",
		"    1,                       !- Multiplier                                                         ",
		"    10,                      !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Floor:Interzone,                                                                                 ",
		"    Zn003:Flr001,            !- Name                                                               ",
		"    FLOOR34,                 !- Construction Name                                                  ",
		"    ZONE 3,                  !- Zone Name                                                          ",
		"    Zn001:Roof001,           !- Outside Boundary Condition Object                                  ",
		"    90,                      !- Azimuth Angle {deg}                                                ",
		"    180,                     !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    20;                      !- Width {m}                                                          ",
		"                                                                                                   ",
		"  Roof,                                                                                            ",
		"    Zn003:Roof001,           !- Name                                                               ",
		"    ROOF31,                  !- Construction Name                                                  ",
		"    ZONE 3,                  !- Zone Name                                                          ",
		"    180,                     !- Azimuth Angle {deg}                                                ",
		"    0,                       !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    10,                      !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    20;                      !- Width {m}                                                          ",
		"                                                                                                   ",
		"  Wall:Exterior,                                                                                   ",
		"    Zn004:Wall001,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 4,                  !- Zone Name                                                          ",
		"    180,                     !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Window,                                                                                          ",
		"    Zn004:Wall001:Win001,    !- Name                                                               ",
		"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
		"    Zn004:Wall001,           !- Building Surface Name                                              ",
		"    ,                        !- Shading Control Name                                               ",
		"    ,                        !- Frame and Divider Name                                             ",
		"    1,                       !- Multiplier                                                         ",
		"    8,                       !- Starting X Coordinate {m}                                          ",
		"    3,                       !- Starting Z Coordinate {m}                                          ",
		"    3,                       !- Length {m}                                                         ",
		"    5;                       !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Exterior,                                                                                   ",
		"    Zn004:Wall002,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 4,                  !- Zone Name                                                          ",
		"    90,                      !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    20,                      !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Wall:Exterior,                                                                                   ",
		"    Zn004:Wall003,           !- Name                                                               ",
		"    EXTERIOR,                !- Construction Name                                                  ",
		"    ZONE 4,                  !- Zone Name                                                          ",
		"    0,                       !- Azimuth Angle {deg}                                                ",
		"    90,                      !- Tilt Angle {deg}                                                   ",
		"    20,                      !- Starting X Coordinate {m}                                          ",
		"    20,                      !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    10;                      !- Height {m}                                                         ",
		"                                                                                                   ",
		"  Floor:Interzone,                                                                                 ",
		"    Zn004:Flr001,            !- Name                                                               ",
		"    FLOOR34,                 !- Construction Name                                                  ",
		"    ZONE 4,                  !- Zone Name                                                          ",
		"    Zn002:Roof001,           !- Outside Boundary Condition Object                                  ",
		"    90,                      !- Azimuth Angle {deg}                                                ",
		"    180,                     !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    0,                       !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    20;                      !- Width {m}                                                          ",
		"                                                                                                   ",
		"  Roof,                                                                                            ",
		"    Zn004:Roof001,           !- Name                                                               ",
		"    ROOF31,                  !- Construction Name                                                  ",
		"    ZONE 4,                  !- Zone Name                                                          ",
		"    180,                     !- Azimuth Angle {deg}                                                ",
		"    0,                       !- Tilt Angle {deg}                                                   ",
		"    0,                       !- Starting X Coordinate {m}                                          ",
		"    0,                       !- Starting Y Coordinate {m}                                          ",
		"    10,                      !- Starting Z Coordinate {m}                                          ",
		"    20,                      !- Length {m}                                                         ",
		"    20;                      !- Width {m}                                                          ",
		"                                                                                                   ",
		"  ScheduleTypeLimits,                                                                              ",
		"    Temperature,             !- Name                                                               ",
		"    -100,                    !- Lower Limit Value                                                  ",
		"    200,                     !- Upper Limit Value                                                  ",
		"    CONTINUOUS;              !- Numeric Type                                                       ",
		"                                                                                                   ",
		"  ScheduleTypeLimits,                                                                              ",
		"    ControlType,             !- Name                                                               ",
		"    0,                       !- Lower Limit Value                                                  ",
		"    4,                       !- Upper Limit Value                                                  ",
		"    DISCRETE;                !- Numeric Type                                                       ",
		"                                                                                                   ",
		"  SCHEDULE:COMPACT,                                                                                ",
		"    ZONE CONTROL TYPE SCHEDULE,  !- Name                                                           ",
		"    CONTROLTYPE,             !- Schedule Type Limits Name                                          ",
		"    Through: 12/31,          !- Field 1                                                            ",
		"    For: AllDays,            !- Field 2                                                            ",
		"    Until: 24:00,4;          !- Field 3                                                            ",
		"                                                                                                   ",
		"  SCHEDULE:COMPACT,                                                                                ",
		"    ZONE HEATING SETPOINTS,  !- Name                                                               ",
		"    TEMPERATURE,             !- Schedule Type Limits Name                                          ",
		"    Through: 12/31,          !- Field 1                                                            ",
		"    For: Weekdays WinterDesignDay, !- Field 2                                                      ",
		"    Until: 7:00,15.60,       !- Field 3                                                            ",
		"    Until: 17:00,19.40,      !- Field 5                                                            ",
		"    Until: 24:00,15.60,      !- Field 7                                                            ",
		"    For: AllOtherDays,       !- Field 9                                                            ",
		"    Until: 24:00,15.60;      !- Field 10                                                           ",
		"                                                                                                   ",
		"  SCHEDULE:COMPACT,                                                                                ",
		"    ZONE COOLING SETPOINTS,  !- Name                                                               ",
		"    TEMPERATURE,             !- Schedule Type Limits Name                                          ",
		"    Through: 12/31,          !- Field 1                                                            ",
		"    For: Weekdays SummerDesignDay, !- Field 2                                                      ",
		"    Until: 7:00,100.00,      !- Field 3                                                            ",
		"    Until: 17:00,26.10,      !- Field 5                                                            ",
		"    Until: 24:00,100.00,     !- Field 7                                                            ",
		"    For: AllOtherDays,       !- Field 9                                                            ",
		"    Until: 24:00,100.00;     !- Field 10                                                           ",
		"                                                                                                   ",
		"  ZoneControl:Thermostat,                                                                          ",
		"    ZONE 1 CONTROLS,         !- Name                                                               ",
		"    ZONE 1,                  !- Zone or ZoneList Name                                              ",
		"    Zone Control Type Schedule,  !- Control Type Schedule Name                                     ",
		"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type                                     ",
		"    ZONE 1 SETPOINTS;        !- Control 1 Name                                                     ",
		"                                                                                                   ",
		"  ThermostatSetpoint:DualSetpoint,                                                                 ",
		"    ZONE 1 SETPOINTS,        !- Name                                                               ",
		"    ZONE Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name                         ",
		"    ZONE Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name                         ",
		"                                                                                                   ",
		"  ZoneHVAC:EquipmentConnections,                                                                   ",
		"    ZONE 1,                  !- Zone Name                                                          ",
		"    ZONE 1 EQUIPMENT,        !- Zone Conditioning Equipment List Name                              ",
		"    ZONE 1 INLETS,           !- Zone Air Inlet Node or NodeList Name                               ",
		"    ,                        !- Zone Air Exhaust Node or NodeList Name                             ",
		"    ZONE 1 NODE,             !- Zone Air Node Name                                                 ",
		"    ZONE 1 OUTLET;           !- Zone Return Air Node Name                                          ",
		"                                                                                                   ",
		"  ZoneHVAC:EquipmentList,                                                                          ",
		"    ZONE 1 EQUIPMENT,        !- Name                                                               ",
		"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                                 ",
		"    ZONE 1 Ideal Loads,      !- Zone Equipment 1 Name                                              ",
		"    1,                       !- Zone Equipment 1 Cooling Sequence                                  ",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence                       ",
		"                                                                                                   ",
		"  ZoneHVAC:IdealLoadsAirSystem,                                                                    ",
		"    ZONE 1 Ideal Loads,      !- Name                                                               ",
		"    ,                        !- Availability Schedule Name                                         ",
		"    ZONE 1 INLETS,           !- Zone Supply Air Node Name                                          ",
		"    ,                        !- Zone Exhaust Air Node Name                                         ",
		"    50,                      !- Maximum Heating Supply Air Temperature {C}                         ",
		"    13,                      !- Minimum Cooling Supply Air Temperature {C}                         ",
		"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
		"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
		"    NoLimit,                 !- Heating Limit                                                      ",
		"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}                               ",
		"    ,                        !- Maximum Sensible Heating Capacity {W}                              ",
		"    NoLimit,                 !- Cooling Limit                                                      ",
		"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}                               ",
		"    ,                        !- Maximum Total Cooling Capacity {W}                                 ",
		"    ,                        !- Heating Availability Schedule Name                                 ",
		"    ,                        !- Cooling Availability Schedule Name                                 ",
		"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                                 ",
		"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}                        ",
		"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                                   ",
		"    ,                        !- Design Specification Outdoor Air Object Name                       ",
		"    ,                        !- Outdoor Air Inlet Node Name                                        ",
		"    ,                        !- Demand Controlled Ventilation Type                                 ",
		"    ,                        !- Outdoor Air Economizer Type                                        ",
		"    ,                        !- Heat Recovery Type                                                 ",
		"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}               ",
		"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}                 ",
		"                                                                                                   ",
		"  NodeList,                                                                                        ",
		"    ZONE 1 INLETS,           !- Name                                                               ",
		"    ZONE 1 INLET;            !- Node 1 Name                                                        ",
		"                                                                                                   ",
		"  ZoneControl:Thermostat,                                                                          ",
		"    ZONE 2 CONTROLS,         !- Name                                                               ",
		"    ZONE 2,                  !- Zone or ZoneList Name                                              ",
		"    Zone Control Type Schedule,  !- Control Type Schedule Name                                     ",
		"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type                                     ",
		"    ZONE 2 SETPOINTS;        !- Control 1 Name                                                     ",
		"                                                                                                   ",
		"  ThermostatSetpoint:DualSetpoint,                                                                 ",
		"    ZONE 2 SETPOINTS,        !- Name                                                               ",
		"    ZONE Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name                         ",
		"    ZONE Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name                         ",
		"                                                                                                   ",
		"  ZoneHVAC:EquipmentConnections,                                                                   ",
		"    ZONE 2,                  !- Zone Name                                                          ",
		"    ZONE 2 EQUIPMENT,        !- Zone Conditioning Equipment List Name                              ",
		"    ZONE 2 INLETS,           !- Zone Air Inlet Node or NodeList Name                               ",
		"    ,                        !- Zone Air Exhaust Node or NodeList Name                             ",
		"    ZONE 2 NODE,             !- Zone Air Node Name                                                 ",
		"    ZONE 2 OUTLET;           !- Zone Return Air Node Name                                          ",
		"                                                                                                   ",
		"  ZoneHVAC:EquipmentList,                                                                          ",
		"    ZONE 2 EQUIPMENT,        !- Name                                                               ",
		"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                                 ",
		"    ZONE 2 Ideal Loads,      !- Zone Equipment 1 Name                                              ",
		"    1,                       !- Zone Equipment 1 Cooling Sequence                                  ",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence                       ",
		"                                                                                                   ",
		"  ZoneHVAC:IdealLoadsAirSystem,                                                                    ",
		"    ZONE 2 Ideal Loads,      !- Name                                                               ",
		"    ,                        !- Availability Schedule Name                                         ",
		"    ZONE 2 INLETS,           !- Zone Supply Air Node Name                                          ",
		"    ,                        !- Zone Exhaust Air Node Name                                         ",
		"    50,                      !- Maximum Heating Supply Air Temperature {C}                         ",
		"    13,                      !- Minimum Cooling Supply Air Temperature {C}                         ",
		"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
		"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
		"    NoLimit,                 !- Heating Limit                                                      ",
		"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}                               ",
		"    ,                        !- Maximum Sensible Heating Capacity {W}                              ",
		"    NoLimit,                 !- Cooling Limit                                                      ",
		"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}                               ",
		"    ,                        !- Maximum Total Cooling Capacity {W}                                 ",
		"    ,                        !- Heating Availability Schedule Name                                 ",
		"    ,                        !- Cooling Availability Schedule Name                                 ",
		"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                                 ",
		"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}                        ",
		"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                                   ",
		"    ,                        !- Design Specification Outdoor Air Object Name                       ",
		"    ,                        !- Outdoor Air Inlet Node Name                                        ",
		"    ,                        !- Demand Controlled Ventilation Type                                 ",
		"    ,                        !- Outdoor Air Economizer Type                                        ",
		"    ,                        !- Heat Recovery Type                                                 ",
		"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}               ",
		"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}                 ",
		"                                                                                                   ",
		"  NodeList,                                                                                        ",
		"    ZONE 2 INLETS,           !- Name                                                               ",
		"    ZONE 2 INLET;            !- Node 1 Name                                                        ",
		"                                                                                                   ",
		"  ZoneControl:Thermostat,                                                                          ",
		"    ZONE 3 CONTROLS,         !- Name                                                               ",
		"    ZONE 3,                  !- Zone or ZoneList Name                                              ",
		"    Zone Control Type Schedule,  !- Control Type Schedule Name                                     ",
		"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type                                     ",
		"    ZONE 3 SETPOINTS;        !- Control 1 Name                                                     ",
		"                                                                                                   ",
		"  ThermostatSetpoint:DualSetpoint,                                                                 ",
		"    ZONE 3 SETPOINTS,        !- Name                                                               ",
		"    ZONE Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name                         ",
		"    ZONE Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name                         ",
		"                                                                                                   ",
		"  ZoneHVAC:EquipmentConnections,                                                                   ",
		"    ZONE 3,                  !- Zone Name                                                          ",
		"    ZONE 3 EQUIPMENT,        !- Zone Conditioning Equipment List Name                              ",
		"    ZONE 3 INLETS,           !- Zone Air Inlet Node or NodeList Name                               ",
		"    ,                        !- Zone Air Exhaust Node or NodeList Name                             ",
		"    ZONE 3 NODE,             !- Zone Air Node Name                                                 ",
		"    ZONE 3 OUTLET;           !- Zone Return Air Node Name                                          ",
		"                                                                                                   ",
		"  ZoneHVAC:EquipmentList,                                                                          ",
		"    ZONE 3 EQUIPMENT,        !- Name                                                               ",
		"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                                 ",
		"    ZONE 3 Ideal Loads,      !- Zone Equipment 1 Name                                              ",
		"    1,                       !- Zone Equipment 1 Cooling Sequence                                  ",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence                       ",
		"                                                                                                   ",
		"  ZoneHVAC:IdealLoadsAirSystem,                                                                    ",
		"    ZONE 3 Ideal Loads,      !- Name                                                               ",
		"    ,                        !- Availability Schedule Name                                         ",
		"    ZONE 3 INLETS,           !- Zone Supply Air Node Name                                          ",
		"    ,                        !- Zone Exhaust Air Node Name                                         ",
		"    50,                      !- Maximum Heating Supply Air Temperature {C}                         ",
		"    13,                      !- Minimum Cooling Supply Air Temperature {C}                         ",
		"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
		"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
		"    NoLimit,                 !- Heating Limit                                                      ",
		"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}                               ",
		"    ,                        !- Maximum Sensible Heating Capacity {W}                              ",
		"    NoLimit,                 !- Cooling Limit                                                      ",
		"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}                               ",
		"    ,                        !- Maximum Total Cooling Capacity {W}                                 ",
		"    ,                        !- Heating Availability Schedule Name                                 ",
		"    ,                        !- Cooling Availability Schedule Name                                 ",
		"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                                 ",
		"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}                        ",
		"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                                   ",
		"    ,                        !- Design Specification Outdoor Air Object Name                       ",
		"    ,                        !- Outdoor Air Inlet Node Name                                        ",
		"    ,                        !- Demand Controlled Ventilation Type                                 ",
		"    ,                        !- Outdoor Air Economizer Type                                        ",
		"    ,                        !- Heat Recovery Type                                                 ",
		"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}               ",
		"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}                 ",
		"                                                                                                   ",
		"  NodeList,                                                                                        ",
		"    ZONE 3 INLETS,           !- Name                                                               ",
		"    ZONE 3 INLET;            !- Node 1 Name                                                        ",
		"                                                                                                   ",
		"  ZoneControl:Thermostat,                                                                          ",
		"    ZONE 4 CONTROLS,         !- Name                                                               ",
		"    ZONE 4,                  !- Zone or ZoneList Name                                              ",
		"    Zone Control Type Schedule,  !- Control Type Schedule Name                                     ",
		"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type                                     ",
		"    ZONE 4 SETPOINTS;        !- Control 1 Name                                                     ",
		"                                                                                                   ",
		"  ThermostatSetpoint:DualSetpoint,                                                                 ",
		"    ZONE 4 SETPOINTS,        !- Name                                                               ",
		"    ZONE Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name                         ",
		"    ZONE Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name                         ",
		"                                                                                                   ",
		"  ZoneHVAC:EquipmentConnections,                                                                   ",
		"    ZONE 4,                  !- Zone Name                                                          ",
		"    ZONE 4 EQUIPMENT,        !- Zone Conditioning Equipment List Name                              ",
		"    ZONE 4 INLETS,           !- Zone Air Inlet Node or NodeList Name                               ",
		"    ,                        !- Zone Air Exhaust Node or NodeList Name                             ",
		"    ZONE 4 NODE,             !- Zone Air Node Name                                                 ",
		"    ZONE 4 OUTLET;           !- Zone Return Air Node Name                                          ",
		"                                                                                                   ",
		"  ZoneHVAC:EquipmentList,                                                                          ",
		"    ZONE 4 EQUIPMENT,        !- Name                                                               ",
		"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                                 ",
		"    ZONE 4 Ideal Loads,      !- Zone Equipment 1 Name                                              ",
		"    1,                       !- Zone Equipment 1 Cooling Sequence                                  ",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence                       ",
		"                                                                                                   ",
		"  ZoneHVAC:IdealLoadsAirSystem,                                                                    ",
		"    ZONE 4 Ideal Loads,      !- Name                                                               ",
		"    ,                        !- Availability Schedule Name                                         ",
		"    ZONE 4 INLETS,           !- Zone Supply Air Node Name                                          ",
		"    ,                        !- Zone Exhaust Air Node Name                                         ",
		"    50,                      !- Maximum Heating Supply Air Temperature {C}                         ",
		"    13,                      !- Minimum Cooling Supply Air Temperature {C}                         ",
		"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
		"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
		"    NoLimit,                 !- Heating Limit                                                      ",
		"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}                               ",
		"    ,                        !- Maximum Sensible Heating Capacity {W}                              ",
		"    NoLimit,                 !- Cooling Limit                                                      ",
		"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}                               ",
		"    ,                        !- Maximum Total Cooling Capacity {W}                                 ",
		"    ,                        !- Heating Availability Schedule Name                                 ",
		"    ,                        !- Cooling Availability Schedule Name                                 ",
		"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                                 ",
		"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}                        ",
		"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                                   ",
		"    ,                        !- Design Specification Outdoor Air Object Name                       ",
		"    ,                        !- Outdoor Air Inlet Node Name                                        ",
		"    ,                        !- Demand Controlled Ventilation Type                                 ",
		"    ,                        !- Outdoor Air Economizer Type                                        ",
		"    ,                        !- Heat Recovery Type                                                 ",
		"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}               ",
		"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}                 ",
		"                                                                                                   ",
		"  NodeList,                                                                                        ",
		"    ZONE 4 INLETS,           !- Name                                                               ",
		"    ZONE 4 INLET;            !- Node 1 Name                                                        ",
		"                                                                                                   ",
		"OutputControl:Table:Style,                                                                         ",
		"    HTML,                    !- Column Separator                                                   ",
		"    JtoKWH;                  !- Unit Conversion                                                    ",
		"                                                                                                   ",
		"Output:Table:SummaryReports,                                                                       ",
		"    AllSummary;              !- Report 1 Name                                                      ",
		"                                                                                                   "
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	OutputProcessor::TimeValue.allocate( 2 );
	DataGlobals::DDOnlySimulation = true;

	ManageSimulation();
//	compare_err_stream( "" );

	EXPECT_EQ( "3", RetrievePreDefTableEntry( pdchSurfCntTot, "Overhang" ) );
	EXPECT_EQ( "3", RetrievePreDefTableEntry( pdchSurfCntExt, "Overhang" ) );

	EXPECT_EQ( "1", RetrievePreDefTableEntry( pdchSurfCntTot, "Fin" ) );
	EXPECT_EQ( "1", RetrievePreDefTableEntry( pdchSurfCntExt, "Fin" ) );


}

TEST_F( EnergyPlusFixture, TubularDaylightDiffuserCount )
{
	// based on DaylightingDeviceTubular.idf
	std::string const idf_objects = delimited_string( {
		"  Version,8.5;                                                                            ",
		"                                                                                          ",
		"  Building,                                                                               ",
		"    Tubular Daylighting Device Example,  !- Name                                          ",
		"    0.0,                     !- North Axis {deg}                                          ",
		"    Suburbs,                 !- Terrain                                                   ",
		"    0.04,                    !- Loads Convergence Tolerance Value                         ",
		"    0.04,                    !- Temperature Convergence Tolerance Value {deltaC}          ",
		"    FullInteriorAndExterior, !- Solar Distribution                                        ",
		"    25,                      !- Maximum Number of Warmup Days                             ",
		"    6;                       !- Minimum Number of Warmup Days                             ",
		"                                                                                          ",
		"  Timestep,6;                                                                             ",
		"                                                                                          ",
		"  ShadowCalculation,                                                                      ",
		"    AverageOverDaysInFrequency,  !- Calculation Method                                    ",
		"    20;                      !- Calculation Frequency                                     ",
		"                                                                                          ",
		"  HeatBalanceAlgorithm,ConductionTransferFunction;                                        ",
		"                                                                                          ",
		"  SurfaceConvectionAlgorithm:Inside,TARP;                                                 ",
		"                                                                                          ",
		"  SurfaceConvectionAlgorithm:Outside,DOE-2;                                               ",
		"                                                                                          ",
		"  GlobalGeometryRules,                                                                    ",
		"    UpperLeftCorner,         !- Starting Vertex Position                                  ",
		"    CounterClockWise,        !- Vertex Entry Direction                                    ",
		"    Relative;                !- Coordinate System                                         ",
		"                                                                                          ",
		"  Site:Location,                                                                          ",
		"    CHICAGO_IL_USA_WMO_725300,  !- Name                                                   ",
		"    42.00,                   !- Latitude {deg}                                            ",
		"    -87.88,                  !- Longitude {deg}                                           ",
		"    -6.00,                   !- Time Zone {hr}                                            ",
		"    190.00;                  !- Elevation {m}                                             ",
		"                                                                                          ",
		"  SizingPeriod:DesignDay,                                                                 ",
		"    CHICAGO Ann Htg 99% Condns DB,  !- Name                                               ",
		"    1,                       !- Month                                                     ",
		"    21,                      !- Day of Month                                              ",
		"    WinterDesignDay,         !- Day Type                                                  ",
		"    -17.3,                   !- Maximum Dry-Bulb Temperature {C}                          ",
		"    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}                 ",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Type                  ",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name     ",
		"    Wetbulb,                 !- Humidity Condition Type                                   ",
		"    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}               ",
		"    ,                        !- Humidity Condition Day Schedule Name                      ",
		"    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}     ",
		"    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}                       ",
		"    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}                 ",
		"    99063.,                  !- Barometric Pressure {Pa}                                  ",
		"    4.9,                     !- Wind Speed {m/s}                                          ",
		"    270,                     !- Wind Direction {deg}                                      ",
		"    No,                      !- Rain Indicator                                            ",
		"    No,                      !- Snow Indicator                                            ",
		"    No,                      !- Daylight Saving Time Indicator                            ",
		"    ASHRAEClearSky,          !- Solar Model Indicator                                     ",
		"    ,                        !- Beam Solar Day Schedule Name                              ",
		"    ,                        !- Diffuse Solar Day Schedule Name                           ",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) ",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (tau",
		"    0.00;                    !- Sky Clearness                                             ",
		"                                                                                          ",
		"  SizingPeriod:DesignDay,                                                                 ",
		"    CHICAGO Ann Clg 1% Condns DB=>MWB,  !- Name                                           ",
		"    7,                       !- Month                                                     ",
		"    21,                      !- Day of Month                                              ",
		"    SummerDesignDay,         !- Day Type                                                  ",
		"    31.5,                    !- Maximum Dry-Bulb Temperature {C}                          ",
		"    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}                 ",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Type                  ",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name     ",
		"    Wetbulb,                 !- Humidity Condition Type                                   ",
		"    23,                      !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}               ",
		"    ,                        !- Humidity Condition Day Schedule Name                      ",
		"    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}     ",
		"    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}                       ",
		"    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}                 ",
		"    99063.,                  !- Barometric Pressure {Pa}                                  ",
		"    5.3,                     !- Wind Speed {m/s}                                          ",
		"    230,                     !- Wind Direction {deg}                                      ",
		"    No,                      !- Rain Indicator                                            ",
		"    No,                      !- Snow Indicator                                            ",
		"    No,                      !- Daylight Saving Time Indicator                            ",
		"    ASHRAEClearSky,          !- Solar Model Indicator                                     ",
		"    ,                        !- Beam Solar Day Schedule Name                              ",
		"    ,                        !- Diffuse Solar Day Schedule Name                           ",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) ",
		"    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (tau",
		"    1.00;                    !- Sky Clearness                                             ",
		"                                                                                          ",
		"  RunPeriod,                                                                              ",
		"    ,                        !- Name                                                      ",
		"    1,                       !- Begin Month                                               ",
		"    1,                       !- Begin Day of Month                                        ",
		"    12,                      !- End Month                                                 ",
		"    31,                      !- End Day of Month                                          ",
		"    Tuesday,                 !- Day of Week for Start Day                                 ",
		"    Yes,                     !- Use Weather File Holidays and Special Days                ",
		"    Yes,                     !- Use Weather File Daylight Saving Period                   ",
		"    No,                      !- Apply Weekend Holiday Rule                                ",
		"    Yes,                     !- Use Weather File Rain Indicators                          ",
		"    Yes;                     !- Use Weather File Snow Indicators                          ",
		"                                                                                          ",
		"  SimulationControl,                                                                      ",
		"    NO,                      !- Do Zone Sizing Calculation                                ",
		"    NO,                      !- Do System Sizing Calculation                              ",
		"    NO,                      !- Do Plant Sizing Calculation                               ",
		"    YES,                     !- Run Simulation for Sizing Periods                         ",
		"    NO;                      !- Run Simulation for Weather File Run Periods               ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    A1 - 1 IN STUCCO,        !- Name                                                      ",
		"    Smooth,                  !- Roughness                                                 ",
		"    2.5389841E-02,           !- Thickness {m}                                             ",
		"    0.6918309,               !- Conductivity {W/m-K}                                      ",
		"    1858.142,                !- Density {kg/m3}                                           ",
		"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.92,                    !- Solar Absorptance                                         ",
		"    0.92;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    C4 - 4 IN COMMON BRICK,  !- Name                                                      ",
		"    Rough,                   !- Roughness                                                 ",
		"    0.1014984,               !- Thickness {m}                                             ",
		"    0.7264224,               !- Conductivity {W/m-K}                                      ",
		"    1922.216,                !- Density {kg/m3}                                           ",
		"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.76,                    !- Solar Absorptance                                         ",
		"    0.76;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name                                          ",
		"    Smooth,                  !- Roughness                                                 ",
		"    1.9050000E-02,           !- Thickness {m}                                             ",
		"    0.7264224,               !- Conductivity {W/m-K}                                      ",
		"    1601.846,                !- Density {kg/m3}                                           ",
		"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.92,                    !- Solar Absorptance                                         ",
		"    0.92;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    C6 - 8 IN CLAY TILE,     !- Name                                                      ",
		"    Smooth,                  !- Roughness                                                 ",
		"    0.2033016,               !- Thickness {m}                                             ",
		"    0.5707605,               !- Conductivity {W/m-K}                                      ",
		"    1121.292,                !- Density {kg/m3}                                           ",
		"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.82,                    !- Solar Absorptance                                         ",
		"    0.82;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    C10 - 8 IN HW CONCRETE,  !- Name                                                      ",
		"    MediumRough,             !- Roughness                                                 ",
		"    0.2033016,               !- Thickness {m}                                             ",
		"    1.729577,                !- Conductivity {W/m-K}                                      ",
		"    2242.585,                !- Density {kg/m3}                                           ",
		"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.65,                    !- Solar Absorptance                                         ",
		"    0.65;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    E2 - 1 / 2 IN SLAG OR STONE,  !- Name                                                 ",
		"    Rough,                   !- Roughness                                                 ",
		"    1.2710161E-02,           !- Thickness {m}                                             ",
		"    1.435549,                !- Conductivity {W/m-K}                                      ",
		"    881.0155,                !- Density {kg/m3}                                           ",
		"    1673.6,                  !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.55,                    !- Solar Absorptance                                         ",
		"    0.55;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name                                             ",
		"    Rough,                   !- Roughness                                                 ",
		"    9.5402403E-03,           !- Thickness {m}                                             ",
		"    0.1902535,               !- Conductivity {W/m-K}                                      ",
		"    1121.292,                !- Density {kg/m3}                                           ",
		"    1673.6,                  !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.75,                    !- Solar Absorptance                                         ",
		"    0.75;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    B5 - 1 IN DENSE INSULATION,  !- Name                                                  ",
		"    VeryRough,               !- Roughness                                                 ",
		"    2.5389841E-02,           !- Thickness {m}                                             ",
		"    4.3239430E-02,           !- Conductivity {W/m-K}                                      ",
		"    91.30524,                !- Density {kg/m3}                                           ",
		"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.50,                    !- Solar Absorptance                                         ",
		"    0.50;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    C12 - 2 IN HW CONCRETE,  !- Name                                                      ",
		"    MediumRough,             !- Roughness                                                 ",
		"    5.0901599E-02,           !- Thickness {m}                                             ",
		"    1.729577,                !- Conductivity {W/m-K}                                      ",
		"    2242.585,                !- Density {kg/m3}                                           ",
		"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.65,                    !- Solar Absorptance                                         ",
		"    0.65;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    ROOFING - ASPHALT SHINGLES,  !- Name                                                  ",
		"    VeryRough,               !- Roughness                                                 ",
		"    3.1999999E-03,           !- Thickness {m}                                             ",
		"    2.9999999E-02,           !- Conductivity {W/m-K}                                      ",
		"    1121.29,                 !- Density {kg/m3}                                           ",
		"    830.0,                   !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.70,                    !- Solar Absorptance                                         ",
		"    0.70;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    BB46 - 5 / 8 IN PLYWOOD, !- Name                                                      ",
		"    Smooth,                  !- Roughness                                                 ",
		"    9.9999998E-03,           !- Thickness {m}                                             ",
		"    0.110,                   !- Conductivity {W/m-K}                                      ",
		"    544.62,                  !- Density {kg/m3}                                           ",
		"    1210.0,                  !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.70,                    !- Solar Absorptance                                         ",
		"    0.70;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    INS - GLASS FIBER BONDED 3 IN,  !- Name                                               ",
		"    VeryRough,               !- Roughness                                                 ",
		"    7.000E-02,               !- Thickness {m}                                             ",
		"    2.9999999E-02,           !- Conductivity {W/m-K}                                      ",
		"    96.11,                   !- Density {kg/m3}                                           ",
		"    790.0,                   !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.50,                    !- Solar Absorptance                                         ",
		"    0.50;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  WindowMaterial:Glazing,                                                                 ",
		"    Clear Acrylic Plastic,   !- Name                                                      ",
		"    SpectralAverage,         !- Optical Data Type                                         ",
		"    ,                        !- Window Glass Spectral Data Set Name                       ",
		"    0.003,                   !- Thickness {m}                                             ",
		"    0.92,                    !- Solar Transmittance at Normal Incidence                   ",
		"    0.05,                    !- Front Side Solar Reflectance at Normal Incidence          ",
		"    0.05,                    !- Back Side Solar Reflectance at Normal Incidence           ",
		"    0.92,                    !- Visible Transmittance at Normal Incidence                 ",
		"    0.05,                    !- Front Side Visible Reflectance at Normal Incidence        ",
		"    0.05,                    !- Back Side Visible Reflectance at Normal Incidence         ",
		"    0.00,                    !- Infrared Transmittance at Normal Incidence                ",
		"    0.90,                    !- Front Side Infrared Hemispherical Emissivity              ",
		"    0.90,                    !- Back Side Infrared Hemispherical Emissivity               ",
		"    0.90;                    !- Conductivity {W/m-K}                                      ",
		"                                                                                          ",
		"  WindowMaterial:Glazing,                                                                 ",
		"    Diffusing Acrylic Plastic,  !- Name                                                   ",
		"    SpectralAverage,         !- Optical Data Type                                         ",
		"    ,                        !- Window Glass Spectral Data Set Name                       ",
		"    0.0022,                  !- Thickness {m}                                             ",
		"    0.90,                    !- Solar Transmittance at Normal Incidence                   ",
		"    0.08,                    !- Front Side Solar Reflectance at Normal Incidence          ",
		"    0.08,                    !- Back Side Solar Reflectance at Normal Incidence           ",
		"    0.90,                    !- Visible Transmittance at Normal Incidence                 ",
		"    0.08,                    !- Front Side Visible Reflectance at Normal Incidence        ",
		"    0.08,                    !- Back Side Visible Reflectance at Normal Incidence         ",
		"    0.00,                    !- Infrared Transmittance at Normal Incidence                ",
		"    0.90,                    !- Front Side Infrared Hemispherical Emissivity              ",
		"    0.90,                    !- Back Side Infrared Hemispherical Emissivity               ",
		"    0.90;                    !- Conductivity {W/m-K}                                      ",
		"                                                                                          ",
		"  Material,                                                                               ",
		"    Very High Reflectivity Surface,  !- Name                                              ",
		"    Smooth,                  !- Roughness                                                 ",
		"    0.0005,                  !- Thickness {m}                                             ",
		"    237,                     !- Conductivity {W/m-K}                                      ",
		"    2702,                    !- Density {kg/m3}                                           ",
		"    903,                     !- Specific Heat {J/kg-K}                                    ",
		"    0.90,                    !- Thermal Absorptance                                       ",
		"    0.05,                    !- Solar Absorptance                                         ",
		"    0.05;                    !- Visible Absorptance                                       ",
		"                                                                                          ",
		"  Construction,                                                                           ",
		"    EXTWALL80,               !- Name                                                      ",
		"    A1 - 1 IN STUCCO,        !- Outside Layer                                             ",
		"    C4 - 4 IN COMMON BRICK,  !- Layer 2                                                   ",
		"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3                                       ",
		"                                                                                          ",
		"  Construction,                                                                           ",
		"    FLOOR SLAB 8 IN,         !- Name                                                      ",
		"    C10 - 8 IN HW CONCRETE;  !- Outside Layer                                             ",
		"                                                                                          ",
		"  Construction,                                                                           ",
		"    ROOF,                    !- Name                                                      ",
		"    ROOFING - ASPHALT SHINGLES,  !- Outside Layer                                         ",
		"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2                                          ",
		"    BB46 - 5 / 8 IN PLYWOOD; !- Layer 3                                                   ",
		"                                                                                          ",
		"  Construction,                                                                           ",
		"    CEILING IN ZONE,         !- Name                                                      ",
		"    INS - GLASS FIBER BONDED 3 IN,  !- Outside Layer                                      ",
		"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 2                                       ",
		"                                                                                          ",
		"  Construction,                                                                           ",
		"    CEILING IN ATTIC,        !- Name                                                      ",
		"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer                                 ",
		"    INS - GLASS FIBER BONDED 3 IN;  !- Layer 2                                            ",
		"                                                                                          ",
		"  Construction,                                                                           ",
		"    TDD Pipe,                !- Name                                                      ",
		"    Very High Reflectivity Surface;  !- Outside Layer                                     ",
		"                                                                                          ",
		"  Construction,                                                                           ",
		"    TDD Dome,                !- Name                                                      ",
		"    Clear Acrylic Plastic;   !- Outside Layer                                             ",
		"                                                                                          ",
		"  Construction,                                                                           ",
		"    TDD Diffuser,            !- Name                                                      ",
		"    Diffusing Acrylic Plastic;  !- Outside Layer                                          ",
		"                                                                                          ",
		"  Zone,                                                                                   ",
		"    Daylit Zone,             !- Name                                                      ",
		"    0.0,                     !- Direction of Relative North {deg}                         ",
		"    0.0,                     !- X Origin {m}                                              ",
		"    0.0,                     !- Y Origin {m}                                              ",
		"    0.0,                     !- Z Origin {m}                                              ",
		"    1,                       !- Type                                                      ",
		"    1,                       !- Multiplier                                                ",
		"    autocalculate,           !- Ceiling Height {m}                                        ",
		"    autocalculate;           !- Volume {m3}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit South Wall,       !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Daylit Zone,             !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit West Wall,        !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Daylit Zone,             !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    0.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit North Wall,       !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Daylit Zone,             !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    0.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit East Wall,        !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Daylit Zone,             !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit Floor,            !- Name                                                      ",
		"    Floor,                   !- Surface Type                                              ",
		"    FLOOR SLAB 8 IN,         !- Construction Name                                         ",
		"    Daylit Zone,             !- Zone Name                                                 ",
		"    Surface,                 !- Outside Boundary Condition                                ",
		"    Daylit Floor,            !- Outside Boundary Condition Object                         ",
		"    NoSun,                   !- Sun Exposure                                              ",
		"    NoWind,                  !- Wind Exposure                                             ",
		"    1.0,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    5.0,0.0,0.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit Ceiling,          !- Name                                                      ",
		"    Roof,                    !- Surface Type                                              ",
		"    CEILING IN ZONE,         !- Construction Name                                         ",
		"    Daylit Zone,             !- Zone Name                                                 ",
		"    Surface,                 !- Outside Boundary Condition                                ",
		"    Daylit Attic Floor,      !- Outside Boundary Condition Object                         ",
		"    NoSun,                   !- Sun Exposure                                              ",
		"    NoWind,                  !- Wind Exposure                                             ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  Zone,                                                                                   ",
		"    Daylit Attic Zone,       !- Name                                                      ",
		"    0.0,                     !- Direction of Relative North {deg}                         ",
		"    0.0,                     !- X Origin {m}                                              ",
		"    0.0,                     !- Y Origin {m}                                              ",
		"    0.0,                     !- Z Origin {m}                                              ",
		"    1,                       !- Type                                                      ",
		"    1,                       !- Multiplier                                                ",
		"    autocalculate,           !- Ceiling Height {m}                                        ",
		"    autocalculate;           !- Volume {m3}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit Attic South Wall, !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Daylit Attic Zone,       !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    5.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit Attic West Wall,  !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Daylit Attic Zone,       !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    0.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit Attic North Wall, !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Daylit Attic Zone,       !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    5.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    0.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit Attic East Wall,  !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Daylit Attic Zone,       !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit Attic Floor,      !- Name                                                      ",
		"    Floor,                   !- Surface Type                                              ",
		"    CEILING IN ATTIC,        !- Construction Name                                         ",
		"    Daylit Attic Zone,       !- Zone Name                                                 ",
		"    Surface,                 !- Outside Boundary Condition                                ",
		"    Daylit Ceiling,          !- Outside Boundary Condition Object                         ",
		"    NoSun,                   !- Sun Exposure                                              ",
		"    NoWind,                  !- Wind Exposure                                             ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Daylit Attic Roof,       !- Name                                                      ",
		"    Roof,                    !- Surface Type                                              ",
		"    ROOF,                    !- Construction Name                                         ",
		"    Daylit Attic Zone,       !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  Zone,                                                                                   ",
		"    Standard Zone,           !- Name                                                      ",
		"    0.0,                     !- Direction of Relative North {deg}                         ",
		"    50.0,                    !- X Origin {m}                                              ",
		"    50.0,                    !- Y Origin {m}                                              ",
		"    0.0,                     !- Z Origin {m}                                              ",
		"    1,                       !- Type                                                      ",
		"    1,                       !- Multiplier                                                ",
		"    autocalculate,           !- Ceiling Height {m}                                        ",
		"    autocalculate;           !- Volume {m3}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard South Wall,     !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Standard Zone,           !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard West Wall,      !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Standard Zone,           !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    0.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard North Wall,     !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Standard Zone,           !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    0.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard East Wall,      !- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Standard Zone,           !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard Floor,          !- Name                                                      ",
		"    Floor,                   !- Surface Type                                              ",
		"    FLOOR SLAB 8 IN,         !- Construction Name                                         ",
		"    Standard Zone,           !- Zone Name                                                 ",
		"    Surface,                 !- Outside Boundary Condition                                ",
		"    Standard Floor,          !- Outside Boundary Condition Object                         ",
		"    NoSun,                   !- Sun Exposure                                              ",
		"    NoWind,                  !- Wind Exposure                                             ",
		"    1.0,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    5.0,0.0,0.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard Ceiling,        !- Name                                                      ",
		"    Roof,                    !- Surface Type                                              ",
		"    CEILING IN ZONE,         !- Construction Name                                         ",
		"    Standard Zone,           !- Zone Name                                                 ",
		"    Surface,                 !- Outside Boundary Condition                                ",
		"    Standard Attic Floor,    !- Outside Boundary Condition Object                         ",
		"    NoSun,                   !- Sun Exposure                                              ",
		"    NoWind,                  !- Wind Exposure                                             ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  Zone,                                                                                   ",
		"    Standard Attic Zone,     !- Name                                                      ",
		"    0.0,                     !- Direction of Relative North {deg}                         ",
		"    50.0,                    !- X Origin {m}                                              ",
		"    50.0,                    !- Y Origin {m}                                              ",
		"    0.0,                     !- Z Origin {m}                                              ",
		"    1,                       !- Type                                                      ",
		"    1,                       !- Multiplier                                                ",
		"    autocalculate,           !- Ceiling Height {m}                                        ",
		"    autocalculate;           !- Volume {m3}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard Attic South Wall,  !- Name                                                   ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Standard Attic Zone,     !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    5.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard Attic West Wall,!- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Standard Attic Zone,     !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    0.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard Attic North Wall,  !- Name                                                   ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Standard Attic Zone,     !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    5.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    0.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard Attic East Wall,!- Name                                                      ",
		"    Wall,                    !- Surface Type                                              ",
		"    EXTWALL80,               !- Construction Name                                         ",
		"    Standard Attic Zone,     !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.5,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard Attic Floor,    !- Name                                                      ",
		"    Floor,                   !- Surface Type                                              ",
		"    CEILING IN ATTIC,        !- Construction Name                                         ",
		"    Standard Attic Zone,     !- Zone Name                                                 ",
		"    Surface,                 !- Outside Boundary Condition                                ",
		"    Standard Ceiling,        !- Outside Boundary Condition Object                         ",
		"    NoSun,                   !- Sun Exposure                                              ",
		"    NoWind,                  !- Wind Exposure                                             ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
		"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
		"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
		"    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
		"                                                                                          ",
		"  BuildingSurface:Detailed,                                                               ",
		"    Standard Attic Roof,     !- Name                                                      ",
		"    Roof,                    !- Surface Type                                              ",
		"    ROOF,                    !- Construction Name                                         ",
		"    Standard Attic Zone,     !- Zone Name                                                 ",
		"    Outdoors,                !- Outside Boundary Condition                                ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    SunExposed,              !- Sun Exposure                                              ",
		"    WindExposed,             !- Wind Exposure                                             ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    4,                       !- Number of Vertices                                        ",
		"    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
		"    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
		"    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
		"    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
		"                                                                                          ",
		"  Lights,                                                                                 ",
		"    Daylit Zone Lights 1,    !- Name                                                      ",
		"    Daylit Zone,             !- Zone or ZoneList Name                                     ",
		"    AlwaysOnSchedule,        !- Schedule Name                                             ",
		"    LightingLevel,           !- Design Level Calculation Method                           ",
		"    800,                     !- Lighting Level {W}                                        ",
		"    ,                        !- Watts per Zone Floor Area {W/m2}                          ",
		"    ,                        !- Watts per Person {W/person}                               ",
		"    0,                       !- Return Air Fraction                                       ",
		"    0.40,                    !- Fraction Radiant                                          ",
		"    0.20,                    !- Fraction Visible                                          ",
		"    1.0,                     !- Fraction Replaceable                                      ",
		"    GeneralLights;           !- End-Use Subcategory                                       ",
		"                                                                                          ",
		"  Lights,                                                                                 ",
		"    Standard Zone Lights 1,  !- Name                                                      ",
		"    Standard Zone,           !- Zone or ZoneList Name                                     ",
		"    AlwaysOnSchedule,        !- Schedule Name                                             ",
		"    LightingLevel,           !- Design Level Calculation Method                           ",
		"    800,                     !- Lighting Level {W}                                        ",
		"    ,                        !- Watts per Zone Floor Area {W/m2}                          ",
		"    ,                        !- Watts per Person {W/person}                               ",
		"    0,                       !- Return Air Fraction                                       ",
		"    0.40,                    !- Fraction Radiant                                          ",
		"    0.20,                    !- Fraction Visible                                          ",
		"    1.0,                     !- Fraction Replaceable                                      ",
		"    GeneralLights;           !- End-Use Subcategory                                       ",
		"                                                                                          ",
		"  DaylightingDevice:Tubular,                                                              ",
		"    Pipe1,                   !- Name                                                      ",
		"    Dome1,                   !- Dome Name                                                 ",
		"    Diffuser1,               !- Diffuser Name                                             ",
		"    TDD Pipe,                !- Construction Name                                         ",
		"    0.3556,                  !- Diameter {m}                                              ",
		"    1.4,                     !- Total Length {m}                                          ",
		"    0.28,                    !- Effective Thermal Resistance {m2-K/W}                     ",
		"    Daylit Attic Zone,       !- Transition Zone 1 Name                                    ",
		"    1.1;                     !- Transition Zone 1 Length {m}                              ",
		"                                                                                          ",
		"  FenestrationSurface:Detailed,                                                           ",
		"    Dome1,                   !- Name                                                      ",
		"    TubularDaylightDome,     !- Surface Type                                              ",
		"    TDD Dome,                !- Construction Name                                         ",
		"    Daylit Attic Roof,       !- Building Surface Name                                     ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    ,                        !- Shading Control Name                                      ",
		"    ,                        !- Frame and Divider Name                                    ",
		"    1.0,                     !- Multiplier                                                ",
		"    4,                       !- Number of Vertices                                        ",
		"    2.3425,3.209,3.64,  !- X,Y,Z ==> Vertex 1 {m}                                         ",
		"    2.3425,2.906,3.58,  !- X,Y,Z ==> Vertex 2 {m}                                         ",
		"    2.6575,2.906,3.58,  !- X,Y,Z ==> Vertex 3 {m}                                         ",
		"    2.6575,3.209,3.64;  !- X,Y,Z ==> Vertex 4 {m}                                         ",
		"                                                                                          ",
		"  FenestrationSurface:Detailed,                                                           ",
		"    Diffuser1,               !- Name                                                      ",
		"    TubularDaylightDiffuser, !- Surface Type                                              ",
		"    TDD Diffuser,            !- Construction Name                                         ",
		"    Daylit Ceiling,          !- Building Surface Name                                     ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    ,                        !- Shading Control Name                                      ",
		"    ,                        !- Frame and Divider Name                                    ",
		"    1.0,                     !- Multiplier                                                ",
		"    4,                       !- Number of Vertices                                        ",
		"    2.3425,3.1575,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                         ",
		"    2.3425,2.8425,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                         ",
		"    2.6575,2.8425,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                         ",
		"    2.6575,3.1575,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                         ",
		"                                                                                          ",
		"  DaylightingDevice:Tubular,                                                              ",
		"    Pipe2,                   !- Name                                                      ",
		"    Dome2,                   !- Dome Name                                                 ",
		"    Diffuser2,               !- Diffuser Name                                             ",
		"    TDD Pipe,                !- Construction Name                                         ",
		"    0.3556,                  !- Diameter {m}                                              ",
		"    2.2,                     !- Total Length {m}                                          ",
		"    0.28,                    !- Effective Thermal Resistance {m2-K/W}                     ",
		"    Daylit Attic Zone,       !- Transition Zone 1 Name                                    ",
		"    1.9;                     !- Transition Zone 1 Length {m}                              ",
		"                                                                                          ",
		"  FenestrationSurface:Detailed,                                                           ",
		"    Dome2,                   !- Name                                                      ",
		"    TubularDaylightDome,     !- Surface Type                                              ",
		"    TDD Dome,                !- Construction Name                                         ",
		"    Daylit Attic Roof,       !- Building Surface Name                                     ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    ,                        !- Shading Control Name                                      ",
		"    ,                        !- Frame and Divider Name                                    ",
		"    1.0,                     !- Multiplier                                                ",
		"    4,                       !- Number of Vertices                                        ",
		"    2.3425,7.209134615385,4.441826923077,  !- X,Y,Z ==> Vertex 1 {m}                      ",
		"    2.3425,6.90625,4.38125,  !- X,Y,Z ==> Vertex 2 {m}                                    ",
		"    2.6575,6.90625,4.38125,  !- X,Y,Z ==> Vertex 3 {m}                                    ",
		"    2.6575,7.209134615385,4.441826923077;  !- X,Y,Z ==> Vertex 4 {m}                      ",
		"                                                                                          ",
		"  FenestrationSurface:Detailed,                                                           ",
		"    Diffuser2,               !- Name                                                      ",
		"    TubularDaylightDiffuser, !- Surface Type                                              ",
		"    TDD Diffuser,            !- Construction Name                                         ",
		"    Daylit Ceiling,          !- Building Surface Name                                     ",
		"    ,                        !- Outside Boundary Condition Object                         ",
		"    0.0,                     !- View Factor to Ground                                     ",
		"    ,                        !- Shading Control Name                                      ",
		"    ,                        !- Frame and Divider Name                                    ",
		"    1.0,                     !- Multiplier                                                ",
		"    4,                       !- Number of Vertices                                        ",
		"    2.3425,7.1575,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                         ",
		"    2.3425,6.8425,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                         ",
		"    2.6575,6.8425,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                         ",
		"    2.6575,7.1575,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                         ",
		"                                                                                          ",
		"  Daylighting:Controls,                                                                   ",
		"    Daylit Zone,             !- Zone Name                                                 ",
		"    2,                       !- Total Daylighting Reference Points                        ",
		"    2.5,                     !- X-Coordinate of First Reference Point {m}                 ",
		"    5.0,                     !- Y-Coordinate of First Reference Point {m}                 ",
		"    0.8,                     !- Z-Coordinate of First Reference Point {m}                 ",
		"    2.5,                     !- X-Coordinate of Second Reference Point {m}                ",
		"    3.0,                     !- Y-Coordinate of Second Reference Point {m}                ",
		"    0.8,                     !- Z-Coordinate of Second Reference Point {m}                ",
		"    1.0,                     !- Fraction of Zone Controlled by First Reference Point      ",
		"    0.0,                     !- Fraction of Zone Controlled by Second Reference Point     ",
		"    550,                     !- Illuminance Setpoint at First Reference Point {lux}       ",
		"    0,                       !- Illuminance Setpoint at Second Reference Point {lux}      ",
		"    1,                       !- Lighting Control Type                                     ",
		"    0.0,                     !- Glare Calculation Azimuth Angle of View Direction Clockwis",
		"    20.0,                    !- Maximum Allowable Discomfort Glare Index                  ",
		"    0.3,                     !- Minimum Input Power Fraction for Continuous Dimming Contro",
		"    0.2,                     !- Minimum Light Output Fraction for Continuous Dimming Contr",
		"    0,                       !- Number of Stepped Control Steps                           ",
		"    1.0;                     !- Probability Lighting will be Reset When Needed in Manual S",
		"                                                                                          ",
		"  OutputControl:IlluminanceMap:Style,                                                     ",
		"    Comma;                   !- Column Separator                                          ",
		"                                                                                          ",
		"  Output:IlluminanceMap,                                                                  ",
		"    Daylit Map,              !- Name                                                      ",
		"    Daylit Zone,             !- Zone Name                                                 ",
		"    0.8,                     !- Z height {m}                                              ",
		"    0.1,                     !- X Minimum Coordinate {m}                                  ",
		"    4.9,                     !- X Maximum Coordinate {m}                                  ",
		"    10,                      !- Number of X Grid Points                                   ",
		"    0.1,                     !- Y Minimum Coordinate {m}                                  ",
		"    9.9,                     !- Y Maximum Coordinate {m}                                  ",
		"    10;                      !- Number of Y Grid Points                                   ",
		"                                                                                          ",
		"  ScheduleTypeLimits,                                                                     ",
		"    On/Off,                  !- Name                                                      ",
		"    0,                       !- Lower Limit Value                                         ",
		"    1,                       !- Upper Limit Value                                         ",
		"    DISCRETE;                !- Numeric Type                                              ",
		"                                                                                          ",
		"  Schedule:Compact,                                                                       ",
		"    AlwaysOnSchedule,        !- Name                                                      ",
		"    On/Off,                  !- Schedule Type Limits Name                                 ",
		"    THROUGH: 12/31,          !- Field 1                                                   ",
		"    FOR: AllDays,            !- Field 2                                                   ",
		"    UNTIL: 24:00,1;          !- Field 3                                                   ",
		"                                                                                          ",
		"  ScheduleTypeLimits,                                                                     ",
		"    Control Type,            !- Name                                                      ",
		"    0,                       !- Lower Limit Value                                         ",
		"    4,                       !- Upper Limit Value                                         ",
		"    DISCRETE;                !- Numeric Type                                              ",
		"                                                                                          ",
		"  Schedule:Compact,                                                                       ",
		"    Zone Control Type Sch,   !- Name                                                      ",
		"    Control Type,            !- Schedule Type Limits Name                                 ",
		"    THROUGH: 12/31,          !- Field 1                                                   ",
		"    FOR: AllDays,            !- Field 2                                                   ",
		"    UNTIL: 24:00,3;          !- Field 3                                                   ",
		"                                                                                          ",
		"  ScheduleTypeLimits,                                                                     ",
		"    Temperature,             !- Name                                                      ",
		"    -60,                     !- Lower Limit Value                                         ",
		"    200,                     !- Upper Limit Value                                         ",
		"    CONTINUOUS,              !- Numeric Type                                              ",
		"    Temperature;             !- Unit Type                                                 ",
		"                                                                                          ",
		"  Schedule:Compact,                                                                       ",
		"    Zone Temp Sch,           !- Name                                                      ",
		"    Temperature,             !- Schedule Type Limits Name                                 ",
		"    THROUGH: 12/31,          !- Field 1                                                   ",
		"    FOR: AllDays,            !- Field 2                                                   ",
		"    UNTIL: 24:00,21;         !- Field 3                                                   ",
		"                                                                                          ",
		"  NodeList,                                                                               ",
		"    Daylit Zone Inlets,      !- Name                                                      ",
		"    Daylit Zone Supply Node; !- Node 1 Name                                               ",
		"                                                                                          ",
		"  ZoneHVAC:EquipmentConnections,                                                          ",
		"    Daylit Zone,             !- Zone Name                                                 ",
		"    Daylit Zone Equipment,   !- Zone Conditioning Equipment List Name                     ",
		"    Daylit Zone Inlets,      !- Zone Air Inlet Node or NodeList Name                      ",
		"    ,                        !- Zone Air Exhaust Node or NodeList Name                    ",
		"    Daylit Zone Air Node,    !- Zone Air Node Name                                        ",
		"    Daylit Zone Return Node; !- Zone Return Air Node Name                                 ",
		"                                                                                          ",
		"  ThermostatSetpoint:SingleHeatingOrCooling,                                              ",
		"    Heating Cooling Setpoint,!- Name                                                      ",
		"    Zone Temp Sch;           !- Setpoint Temperature Schedule Name                        ",
		"                                                                                          ",
		"  ZoneControl:Thermostat,                                                                 ",
		"    Daylit Zone Thermostat,  !- Name                                                      ",
		"    Daylit Zone,             !- Zone or ZoneList Name                                     ",
		"    Zone Control Type Sch,   !- Control Type Schedule Name                                ",
		"    ThermostatSetpoint:SingleHeatingOrCooling,  !- Control 1 Object Type                  ",
		"    Heating Cooling Setpoint;!- Control 1 Name                                            ",
		"                                                                                          ",
		"  ZoneHVAC:EquipmentList,                                                                 ",
		"    Daylit Zone Equipment,   !- Name                                                      ",
		"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                        ",
		"    Daylit Zone Air,         !- Zone Equipment 1 Name                                     ",
		"    1,                       !- Zone Equipment 1 Cooling Sequence                         ",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence              ",
		"                                                                                          ",
		"  ZoneHVAC:IdealLoadsAirSystem,                                                           ",
		"    Daylit Zone Air,         !- Name                                                      ",
		"    ,                        !- Availability Schedule Name                                ",
		"    Daylit Zone Supply Node, !- Zone Supply Air Node Name                                 ",
		"    ,                        !- Zone Exhaust Air Node Name                                ",
		"    50,                      !- Maximum Heating Supply Air Temperature {C}                ",
		"    13,                      !- Minimum Cooling Supply Air Temperature {C}                ",
		"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAi",
		"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAi",
		"    NoLimit,                 !- Heating Limit                                             ",
		"    AUTOSIZE,                !- Maximum Heating Air Flow Rate {m3/s}                      ",
		"    ,                        !- Maximum Sensible Heating Capacity {W}                     ",
		"    NoLimit,                 !- Cooling Limit                                             ",
		"    AUTOSIZE,                !- Maximum Cooling Air Flow Rate {m3/s}                      ",
		"    ,                        !- Maximum Total Cooling Capacity {W}                        ",
		"    ,                        !- Heating Availability Schedule Name                        ",
		"    ,                        !- Cooling Availability Schedule Name                        ",
		"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                        ",
		"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}               ",
		"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                          ",
		"    ,                        !- Design Specification Outdoor Air Object Name              ",
		"    ,                        !- Outdoor Air Inlet Node Name                               ",
		"    ,                        !- Demand Controlled Ventilation Type                        ",
		"    ,                        !- Outdoor Air Economizer Type                               ",
		"    ,                        !- Heat Recovery Type                                        ",
		"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}      ",
		"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}        ",
		"                                                                                          ",
		"  NodeList,                                                                               ",
		"    Standard Zone Inlets,    !- Name                                                      ",
		"    Standard Zone Supply Node;  !- Node 1 Name                                            ",
		"                                                                                          ",
		"  ZoneHVAC:EquipmentConnections,                                                          ",
		"    Standard Zone,           !- Zone Name                                                 ",
		"    Standard Zone Equipment, !- Zone Conditioning Equipment List Name                     ",
		"    Standard Zone Inlets,    !- Zone Air Inlet Node or NodeList Name                      ",
		"    ,                        !- Zone Air Exhaust Node or NodeList Name                    ",
		"    Standard Zone Air Node,  !- Zone Air Node Name                                        ",
		"    Standard Zone Return Node;  !- Zone Return Air Node Name                              ",
		"                                                                                          ",
		"  ThermostatSetpoint:SingleHeatingOrCooling,                                              ",
		"    Heating Cooling Setpoint,!- Name                                                      ",
		"    Zone Temp Sch;           !- Setpoint Temperature Schedule Name                        ",
		"                                                                                          ",
		"  ZoneControl:Thermostat,                                                                 ",
		"    Standard Zone Thermostat,!- Name                                                      ",
		"    Standard Zone,           !- Zone or ZoneList Name                                     ",
		"    Zone Control Type Sch,   !- Control Type Schedule Name                                ",
		"    ThermostatSetpoint:SingleHeatingOrCooling,  !- Control 1 Object Type                  ",
		"    Heating Cooling Setpoint;!- Control 1 Name                                            ",
		"                                                                                          ",
		"  ZoneHVAC:EquipmentList,                                                                 ",
		"    Standard Zone Equipment, !- Name                                                      ",
		"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                        ",
		"    Standard Zone Air,       !- Zone Equipment 1 Name                                     ",
		"    1,                       !- Zone Equipment 1 Cooling Sequence                         ",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence              ",
		"                                                                                          ",
		"  ZoneHVAC:IdealLoadsAirSystem,                                                           ",
		"    Standard Zone Air,       !- Name                                                      ",
		"    ,                        !- Availability Schedule Name                                ",
		"    Standard Zone Supply Node,  !- Zone Supply Air Node Name                              ",
		"    ,                        !- Zone Exhaust Air Node Name                                ",
		"    50,                      !- Maximum Heating Supply Air Temperature {C}                ",
		"    13,                      !- Minimum Cooling Supply Air Temperature {C}                ",
		"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAi",
		"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAi",
		"    NoLimit,                 !- Heating Limit                                             ",
		"    AUTOSIZE,                !- Maximum Heating Air Flow Rate {m3/s}                      ",
		"    ,                        !- Maximum Sensible Heating Capacity {W}                     ",
		"    NoLimit,                 !- Cooling Limit                                             ",
		"    AUTOSIZE,                !- Maximum Cooling Air Flow Rate {m3/s}                      ",
		"    ,                        !- Maximum Total Cooling Capacity {W}                        ",
		"    ,                        !- Heating Availability Schedule Name                        ",
		"    ,                        !- Cooling Availability Schedule Name                        ",
		"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                        ",
		"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}               ",
		"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                          ",
		"    ,                        !- Design Specification Outdoor Air Object Name              ",
		"    ,                        !- Outdoor Air Inlet Node Name                               ",
		"    ,                        !- Demand Controlled Ventilation Type                        ",
		"    ,                        !- Outdoor Air Economizer Type                               ",
		"    ,                        !- Heat Recovery Type                                        ",
		"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}      ",
		"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}        ",
		"                                                                                          ",
		"  Output:VariableDictionary,Regular;                                                      ",
		"                                                                                          ",
		"  Output:Surfaces:Drawing,DXF;                                                            ",
		"                                                                                          ",
		"  Output:Constructions,Constructions;                                                     ",
		"                                                                                          ",
		"  Output:Surfaces:List,Detailswithvertices;                                               ",
		"                                                                                          ",
		"  OutputControl:Table:Style,                                                              ",
		"    HTML;                    !- Column Separator                                          ",
		"                                                                                          ",
		"  Output:Table:SummaryReports,                                                            ",
		"    AllSummary;              !- Report 1 Name                                             ",
		"                                                                                          ",
		"  Output:SQLite,                                                                          ",
		"    SimpleAndTabular;        !- Option Type                                               ",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	OutputProcessor::TimeValue.allocate( 2 );
	DataGlobals::DDOnlySimulation = true;

	ManageSimulation();
	//compare_err_stream( "" );

	EXPECT_EQ( "2", RetrievePreDefTableEntry( pdchSurfCntTot, "Tubular Daylighting Device Diffuser" ) );
	EXPECT_EQ( "2", RetrievePreDefTableEntry( pdchSurfCntExt, "Tubular Daylighting Device Diffuser" ) );

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
