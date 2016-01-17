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

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataGlobalConstants;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::HeatBalanceManager;
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
	ShowMessage( "Begin Test: EnergyPlusFixture, OutputReportTabularTest_AllocateLoadComponentArraysTest" );

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
	EXPECT_EQ( peopleDelaySeq.size(), 1920u );

	// lightInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( lightInstantSeq.size(), 1920u );

	// lightRetAirSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( lightRetAirSeq.size(), 1920u );

	// lightLWRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( lightLWRadSeq.size(), 1920u );

	// lightSWRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
	EXPECT_EQ( lightSWRadSeq.size(), 3360u );

	// lightDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( lightDelaySeq.size(), 1920u );

	// equipInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( equipInstantSeq.size(), 1920u );

	// equipLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( equipLatentSeq.size(), 1920u );

	// equipRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( equipRadSeq.size(), 1920u );

	// equipDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( equipDelaySeq.size(), 1920u );

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
	EXPECT_EQ( hvacLossDelaySeq.size(), 1920u );

	// powerGenInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( powerGenInstantSeq.size(), 1920u );

	// powerGenRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( powerGenRadSeq.size(), 1920u );

	// powerGenDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
	EXPECT_EQ( powerGenDelaySeq.size(), 1920u );

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
	EXPECT_EQ( feneSolarDelaySeq.size(), 1920u );

	// surfDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
	EXPECT_EQ( surfDelaySeq.size(), 3360u );

}

TEST_F( EnergyPlusFixture, OutputReportTabular_ZoneMultiplierTest )
{
	// AUTHOR: R. Raustad, FSEC
	// DATE WRITTEN: Sep 2015

	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		" ",
		" Output:Diagnostics, DisplayExtraWarnings;",
		"  Timestep, 4;",
		" ",
		"BUILDING, Bldg2, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
		" ",
		"SimulationControl, YES, NO, NO, YES, NO;",
		" ",
		"  Site:Location,",
		"    Miami Intl Ap FL USA TMY3 WMO=722020E,    !- Name",
		"    25.82,                   !- Latitude {deg}",
		"    -80.30,                  !- Longitude {deg}",
		"    -5.00,                   !- Time Zone {hr}",
		"    11;                      !- Elevation {m}",
		" ",
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
		" ",
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
		"  2.0,                     !- Ceiling Height {m}",
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
		"  2.0,                     !- Ceiling Height {m}",
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

	OutputProcessor::TimeValue.allocate( 2 ); // ** HELP ** where the heck does this get allocated ??

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

	//expect reported occupancy to be based on multipliers
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).NumOccAccum / DataHeatBalance::ZonePreDefRep( 1 ).NumOccAccum ), 0.00001 );

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

	GetInputTabularPredefined();

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
