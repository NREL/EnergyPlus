// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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

#include <tuple>

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include "Fixtures/SQLiteFixture.hh"

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/CondenserLoopTowers.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/PollutionModule.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/WeatherManager.hh>

using namespace EnergyPlus;
using namespace DataGlobalConstants;
using namespace DataEnvironment;
using namespace DataHeatBalance;
using namespace DataSizing;
using namespace DataSurfaces;
using namespace OutputProcessor;
using namespace OutputReportPredefined;
using namespace OutputReportTabular;
using namespace OutputProcessor;
using namespace SimulationManager;

TEST_F(EnergyPlusFixture, OutputReportTabularTest_ConfirmSetUnitsStyleFromString)
{

    EXPECT_EQ(OutputReportTabular::iUnitsStyle::None, SetUnitsStyleFromString("None"));
    EXPECT_EQ(OutputReportTabular::iUnitsStyle::JtoKWH, SetUnitsStyleFromString("JTOKWH"));
    EXPECT_EQ(OutputReportTabular::iUnitsStyle::JtoMJ, SetUnitsStyleFromString("JTOMJ"));
    EXPECT_EQ(OutputReportTabular::iUnitsStyle::JtoGJ, SetUnitsStyleFromString("JTOGJ"));
    EXPECT_EQ(OutputReportTabular::iUnitsStyle::InchPound, SetUnitsStyleFromString("INCHPOUND"));
    EXPECT_EQ(OutputReportTabular::iUnitsStyle::NotFound, SetUnitsStyleFromString("qqq"));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_Basic)
{
    state->dataOutRptTab->OutputTableBinned.allocate(10);
    EXPECT_TRUE(warningAboutKeyNotFound(*state, 0, 1, "moduleName"));
    EXPECT_FALSE(warningAboutKeyNotFound(*state, 100, 1, "moduleName"));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_RealToStr)
{
    EXPECT_EQ("       0.001", RealToStr(0.0011, 3));
    EXPECT_NE("       0.001", RealToStr(0.0019, 3));

    EXPECT_EQ("          1.", RealToStr(1.23456789, 0));
    EXPECT_EQ("         1.2", RealToStr(1.23456789, 1));
    EXPECT_EQ("        1.23", RealToStr(1.23456789, 2));
    EXPECT_EQ("       1.235", RealToStr(1.23456789, 3));
    EXPECT_EQ("      1.2346", RealToStr(1.23456789, 4));
    EXPECT_EQ("     1.23457", RealToStr(1.23456789, 5));
    EXPECT_EQ("    1.234568", RealToStr(1.23456789, 6));
    EXPECT_EQ("   1.2345679", RealToStr(1.23456789, 7));
    EXPECT_EQ("  1.23456789", RealToStr(1.23456789, 8));

    EXPECT_EQ("    1.234000", RealToStr(1.234, 6));
    EXPECT_EQ("   1.2340000", RealToStr(1.234, 7));
    EXPECT_EQ("  1.23400000", RealToStr(1.234, 8));

    EXPECT_EQ("     123457.", RealToStr(123456.789, 0));
    EXPECT_EQ("    123456.8", RealToStr(123456.789, 1));
    EXPECT_EQ("   123456.79", RealToStr(123456.789, 2));
    EXPECT_EQ("  123456.789", RealToStr(123456.789, 3));
    EXPECT_EQ(" 123456.7890", RealToStr(123456.789, 4));

    EXPECT_EQ("0.123457E+06", RealToStr(123456.789, 5));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_isNumber)
{
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

TEST_F(EnergyPlusFixture, OutputReportTabularTest_digitsAferDecimal)
{
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

TEST_F(EnergyPlusFixture, OutputReportTabularTest_splitCommaString)
{
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

TEST_F(EnergyPlusFixture, OutputReportTabularTest_unitsFromHeading)
{
    std::string unitString;
    int indexUnitConv;
    std::string curUnits;
    Real64 curConversionFactor;
    Real64 curConversionOffset;

    SetupUnitConversions(*state);
    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::InchPound;

    unitString = "";
    EXPECT_EQ(97, unitsFromHeading(*state, unitString));
    EXPECT_EQ("", unitString);
    unitString = "Zone Floor Area {m2}";
    EXPECT_EQ(46, unitsFromHeading(*state, unitString));
    EXPECT_EQ("Zone Floor Area {ft2}", unitString);
    unitString = "Fictional field {nonsense}";
    EXPECT_EQ(0, unitsFromHeading(*state, unitString));
    EXPECT_EQ("Fictional field {nonsense}", unitString);

    // Check a few report column headings too
    unitString = "Standard Rated Net Cooling Capacity [W]";
    indexUnitConv = unitsFromHeading(*state, unitString);
    GetUnitConversion(*state, indexUnitConv, curConversionFactor, curConversionOffset, curUnits);
    // We expect W to convert to tons because it's cooling
    EXPECT_EQ(70, indexUnitConv);
    EXPECT_EQ("ton", curUnits);
    EXPECT_EQ(0.0002843333, curConversionFactor);
    EXPECT_EQ(0.0, curConversionOffset);

    unitString = "Rated Net Cooling Capacity Test A [W]";
    indexUnitConv = unitsFromHeading(*state, unitString);
    GetUnitConversion(*state, indexUnitConv, curConversionFactor, curConversionOffset, curUnits);
    // We expect W to convert to tons because it's cooling
    EXPECT_EQ(70, indexUnitConv);
    EXPECT_EQ("ton", curUnits);
    EXPECT_EQ(0.0002843333, curConversionFactor);
    EXPECT_EQ(0.0, curConversionOffset);
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_ConfirmResourceWarning)
{
    EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Electricity [kWh]",
              ResourceWarningMessage("Electricity [kWh]"));
    EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Natural Gas [kWh]",
              ResourceWarningMessage("Natural Gas [kWh]"));
    EXPECT_EQ(
        "In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Additional Fuel [kWh]",
        ResourceWarningMessage("Additional Fuel [kWh]"));
    EXPECT_EQ(
        "In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: District Cooling [kBtu]",
        ResourceWarningMessage("District Cooling [kBtu]"));
    EXPECT_EQ(
        "In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: District Heating [kBtu]",
        ResourceWarningMessage("District Heating [kBtu]"));
    EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Water [GJ]",
              ResourceWarningMessage("Water [GJ]"));
    EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Electricity [GJ]",
              ResourceWarningMessage("Electricity [GJ]"));
    EXPECT_NE("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Gas [kWh]",
              ResourceWarningMessage("Electricity [kWh]"));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_ConfirmWaterConversion)
{
    EXPECT_EQ(15, WaterConversionFunct(75, 5));
    EXPECT_EQ(1, WaterConversionFunct(1, 1));
    EXPECT_EQ(13.756, WaterConversionFunct(481.46, 35));
    EXPECT_EQ(-2, WaterConversionFunct(-12, 6));
    EXPECT_NE(15, WaterConversionFunct(135, 5));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_GetUnitConversion)
{
    int indexUnitConv;
    std::string curUnits;
    Real64 curConversionFactor;
    Real64 curConversionOffset;
    std::string varNameWithUnits;

    SetupUnitConversions(*state);

    varNameWithUnits = "ZONE AIR SYSTEM SENSIBLE COOLING RATE[W]";
    LookupSItoIP(*state, varNameWithUnits, indexUnitConv, curUnits);
    GetUnitConversion(*state, indexUnitConv, curConversionFactor, curConversionOffset, curUnits);
    EXPECT_EQ(70, indexUnitConv);
    EXPECT_EQ("ton", curUnits);
    EXPECT_EQ(0.0002843333, curConversionFactor);
    EXPECT_EQ(0.0, curConversionOffset);

    varNameWithUnits = "SITE OUTDOOR AIR DRYBULB TEMPERATURE[C]";
    LookupSItoIP(*state, varNameWithUnits, indexUnitConv, curUnits);
    GetUnitConversion(*state, indexUnitConv, curConversionFactor, curConversionOffset, curUnits);
    EXPECT_EQ(11, indexUnitConv);
    EXPECT_EQ("F", curUnits);
    EXPECT_EQ(1.8, curConversionFactor);
    EXPECT_EQ(32., curConversionOffset);

    varNameWithUnits = "ZONE ELECTRIC EQUIPMENT ELECTRICITY ENERGY[J]";
    LookupSItoIP(*state, varNameWithUnits, indexUnitConv, curUnits);
    GetUnitConversion(*state, indexUnitConv, curConversionFactor, curConversionOffset, curUnits);
    EXPECT_EQ(20, indexUnitConv);
    EXPECT_EQ("kWh", curUnits);
    EXPECT_EQ(0.000000277778, curConversionFactor);
    EXPECT_EQ(0.0, curConversionOffset);

    varNameWithUnits = "ZONE COOLING SETPOINT NOT MET TIME[hr]";
    LookupSItoIP(*state, varNameWithUnits, indexUnitConv, curUnits);
    GetUnitConversion(*state, indexUnitConv, curConversionFactor, curConversionOffset, curUnits);
    EXPECT_EQ(17, indexUnitConv);
    EXPECT_EQ("hr", curUnits);
    EXPECT_EQ(1.0, curConversionFactor);
    EXPECT_EQ(0.0, curConversionOffset);

    varNameWithUnits = "ZONE LIGHTS TOTAL HEATING ENERGY[Invalid/Undefined]";
    LookupSItoIP(*state, varNameWithUnits, indexUnitConv, curUnits);
    GetUnitConversion(*state, indexUnitConv, curConversionFactor, curConversionOffset, curUnits);
    EXPECT_EQ(96, indexUnitConv);
    EXPECT_EQ("Invalid/Undefined", curUnits);
    EXPECT_EQ(1.0, curConversionFactor);
    EXPECT_EQ(0.0, curConversionOffset);

    varNameWithUnits = "FICTIONAL VARIABLE[qqq]";
    LookupSItoIP(*state, varNameWithUnits, indexUnitConv, curUnits);
    GetUnitConversion(*state, indexUnitConv, curConversionFactor, curConversionOffset, curUnits);
    EXPECT_EQ(0, indexUnitConv);
    EXPECT_EQ("", curUnits);
    EXPECT_EQ(1.0, curConversionFactor);
    EXPECT_EQ(0.0, curConversionOffset);

    varNameWithUnits = "ZONE PEOPLE OCCUPANT COUNT[]";
    LookupSItoIP(*state, varNameWithUnits, indexUnitConv, curUnits);
    GetUnitConversion(*state, indexUnitConv, curConversionFactor, curConversionOffset, curUnits);
    EXPECT_EQ(97, indexUnitConv);
    EXPECT_EQ("", curUnits);
    EXPECT_EQ(1.0, curConversionFactor);
    EXPECT_EQ(0.0, curConversionOffset);

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
        "[W]",
        "[person/m2]",
    };

    for (auto u : units) {
        LookupSItoIP(*state, u, indexUnitConv, curUnits);
        EXPECT_NE(indexUnitConv, 0);
    }
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_LookupJtokWH)
{
    int indexUnitConv;
    std::string curUnits;
    std::string varNameWithUnits;

    SetupUnitConversions(*state);

    varNameWithUnits = "ZONE AIR SYSTEM SENSIBLE COOLING RATE[W]";
    LookupJtokWH(*state, varNameWithUnits, indexUnitConv, curUnits);
    EXPECT_EQ(0, indexUnitConv);
    EXPECT_EQ("ZONE AIR SYSTEM SENSIBLE COOLING RATE[W]", curUnits);

    varNameWithUnits = "Electricity Energy Use [GJ]";
    LookupJtokWH(*state, varNameWithUnits, indexUnitConv, curUnits);
    EXPECT_EQ(86, indexUnitConv);
    EXPECT_EQ("Electricity Energy Use [kWh]", curUnits);

    varNameWithUnits = "Electricity [MJ/m2]";
    LookupJtokWH(*state, varNameWithUnits, indexUnitConv, curUnits);
    EXPECT_EQ(95, indexUnitConv);
    EXPECT_EQ("Electricity [kWh/m2]", curUnits);
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_GetColumnUsingTabs)
{
    {
        std::string inString = " Col1 \t Col2 \t Col3 ";
        EXPECT_EQ(" Col1 ", GetColumnUsingTabs(inString, 1));
        EXPECT_EQ(" Col2 ", GetColumnUsingTabs(inString, 2));
        EXPECT_EQ(" Col3 ", GetColumnUsingTabs(inString, 3));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 4));
    }

    {
        std::string inString = "Col1\tCol2\tCol3";
        EXPECT_EQ("Col1", GetColumnUsingTabs(inString, 1));
        EXPECT_EQ("Col2", GetColumnUsingTabs(inString, 2));
        EXPECT_EQ("Col3", GetColumnUsingTabs(inString, 3));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 4));
    }

    {
        std::string inString = "Col1\tCol2\tCol3\t";
        EXPECT_EQ("Col1", GetColumnUsingTabs(inString, 1));
        EXPECT_EQ("Col2", GetColumnUsingTabs(inString, 2));
        EXPECT_EQ("Col3", GetColumnUsingTabs(inString, 3));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 4));
    }

    {
        std::string inString = "";
        EXPECT_EQ("", GetColumnUsingTabs(inString, 1));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 2));
    }

    {
        std::string inString = " ";
        EXPECT_EQ(" ", GetColumnUsingTabs(inString, 1));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 2));
    }

    {
        std::string inString = "\t";
        EXPECT_EQ("", GetColumnUsingTabs(inString, 1));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 2));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 3));
    }

    {
        std::string inString = " \t ";
        EXPECT_EQ(" ", GetColumnUsingTabs(inString, 1));
        EXPECT_EQ(" ", GetColumnUsingTabs(inString, 2));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 3));
    }

    {
        std::string inString = "\tCol1\tCol2\tCol3\t";
        EXPECT_EQ("", GetColumnUsingTabs(inString, 1));
        EXPECT_EQ("Col1", GetColumnUsingTabs(inString, 2));
        EXPECT_EQ("Col2", GetColumnUsingTabs(inString, 3));
        EXPECT_EQ("Col3", GetColumnUsingTabs(inString, 4));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 5));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 6));
    }

    {
        std::string inString = "Col1\t\tCol2\tCol3\t";
        EXPECT_EQ("Col1", GetColumnUsingTabs(inString, 1));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 2));
        EXPECT_EQ("Col2", GetColumnUsingTabs(inString, 3));
        EXPECT_EQ("Col3", GetColumnUsingTabs(inString, 4));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 5));
        EXPECT_EQ("", GetColumnUsingTabs(inString, 6));
    }
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_AllocateLoadComponentArraysTest)
{
    state->dataEnvrn->TotDesDays = 2;
    state->dataEnvrn->TotRunDesPersDays = 3;
    state->dataGlobal->NumOfZones = 4;
    state->dataSurface->TotSurfaces = 7;
    state->dataGlobal->NumOfTimeStepInHour = 4;

    AllocateLoadComponentArrays(*state);

    // radiantPulseTimestep.allocate( { 0, TotDesDays + TotRunDesPersDays }, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->radiantPulseTimestep.size(), 24u);

    // radiantPulseReceived.allocate( { 0, TotDesDays + TotRunDesPersDays }, TotSurfaces );
    EXPECT_EQ(state->dataOutRptTab->radiantPulseReceived.size(), 42u);

    // loadConvectedNormal.allocate( TotDesDays + TotRunDesPersDays, { 0, NumOfTimeStepInHour * 24 }, TotSurfaces );
    EXPECT_EQ(state->dataOutRptTab->loadConvectedNormal.size(), 3395u);

    // loadConvectedWithPulse.allocate( TotDesDays + TotRunDesPersDays, { 0, NumOfTimeStepInHour * 24 }, TotSurfaces );
    EXPECT_EQ(state->dataOutRptTab->loadConvectedWithPulse.size(), 3395u);

    // netSurfRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
    EXPECT_EQ(state->dataOutRptTab->netSurfRadSeq.size(), 3360u);

    // decayCurveCool.allocate( NumOfTimeStepInHour * 24, TotSurfaces );
    EXPECT_EQ(state->dataOutRptTab->decayCurveCool.size(), 672u);

    // decayCurveHeat.allocate( NumOfTimeStepInHour * 24, TotSurfaces );
    EXPECT_EQ(state->dataOutRptTab->decayCurveHeat.size(), 672u);

    // ITABSFseq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
    EXPECT_EQ(state->dataOutRptTab->ITABSFseq.size(), 3360u);

    // TMULTseq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->TMULTseq.size(), 1920u);

    // peopleInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->peopleInstantSeq.size(), 1920u);

    // peopleLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->peopleLatentSeq.size(), 1920u);

    // peopleRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->peopleRadSeq.size(), 1920u);

    // peopleDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    // EXPECT_EQ( peopleDelaySeq.size(), 1920u );

    // lightInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->lightInstantSeq.size(), 1920u);

    // lightRetAirSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->lightRetAirSeq.size(), 1920u);

    // lightLWRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->lightLWRadSeq.size(), 1920u);

    // lightSWRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
    EXPECT_EQ(state->dataOutRptTab->lightSWRadSeq.size(), 3360u);

    // lightDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    // EXPECT_EQ( lightDelaySeq.size(), 1920u );

    // equipInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->equipInstantSeq.size(), 1920u);

    // equipLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->equipLatentSeq.size(), 1920u);

    // equipRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->equipRadSeq.size(), 1920u);

    // equipDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    // EXPECT_EQ( equipDelaySeq.size(), 1920u );

    // refrigInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->refrigInstantSeq.size(), 1920u);

    // refrigRetAirSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->refrigRetAirSeq.size(), 1920u);

    // refrigLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->refrigLatentSeq.size(), 1920u);

    // waterUseInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->waterUseInstantSeq.size(), 1920u);

    // waterUseLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->waterUseLatentSeq.size(), 1920u);

    // hvacLossInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->hvacLossInstantSeq.size(), 1920u);

    // hvacLossRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->hvacLossRadSeq.size(), 1920u);

    // hvacLossDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    // EXPECT_EQ( hvacLossDelaySeq.size(), 1920u );

    // powerGenInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->powerGenInstantSeq.size(), 1920u);

    // powerGenRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->powerGenRadSeq.size(), 1920u);

    // powerGenDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    // EXPECT_EQ( powerGenDelaySeq.size(), 1920u );

    // infilInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->infilInstantSeq.size(), 1920u);

    // infilLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->infilLatentSeq.size(), 1920u);

    // zoneVentInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->zoneVentInstantSeq.size(), 1920u);

    // zoneVentLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->zoneVentLatentSeq.size(), 1920u);

    // interZoneMixInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->interZoneMixInstantSeq.size(), 1920u);

    // interZoneMixLatentSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->interZoneMixLatentSeq.size(), 1920u);

    // feneCondInstantSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    EXPECT_EQ(state->dataOutRptTab->feneCondInstantSeq.size(), 1920u);

    // feneSolarRadSeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
    EXPECT_EQ(state->dataOutRptTab->feneSolarRadSeq.size(), 3360u);

    // feneSolarDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, NumOfZones );
    // EXPECT_EQ( feneSolarDelaySeq.size(), 1920u );

    // surfDelaySeq.allocate( TotDesDays + TotRunDesPersDays, NumOfTimeStepInHour * 24, TotSurfaces );
    // EXPECT_EQ( surfDelaySeq.size(), 3360u );
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_ConfirmConvertToEscaped)
{
    EXPECT_EQ("", ConvertToEscaped(""));
    EXPECT_EQ(" ", ConvertToEscaped(" "));
    EXPECT_EQ("String with &gt; in it", ConvertToEscaped("String with > in it"));
    EXPECT_EQ("String with &lt; in it", ConvertToEscaped("String with < in it"));
    EXPECT_EQ("String with &amp; in it", ConvertToEscaped("String with & in it"));
    EXPECT_EQ("String with &quot; in it", ConvertToEscaped("String with \" in it"));
    EXPECT_EQ("String with &apos; in it", ConvertToEscaped("String with \' in it"));
    EXPECT_EQ("String with &quot; in it", ConvertToEscaped(R"(String with \" in it)"));
    EXPECT_EQ("String with &apos; in it", ConvertToEscaped(R"(String with \' in it)"));
    EXPECT_EQ("String with &deg; in it", ConvertToEscaped(std::string("String with ") + char(176) + std::string(" in it")));
    EXPECT_EQ("String with &deg; in it", ConvertToEscaped("String with \u00B0 in it"));
    EXPECT_EQ("String with &deg; in it", ConvertToEscaped("String with \xB0 in it"));
    EXPECT_EQ("String with &deg; in it", ConvertToEscaped("String with \xC2\xB0 in it"));
    EXPECT_EQ("String with \xC2 in it", ConvertToEscaped("String with \xC2 in it"));
    EXPECT_EQ("String with \xC2\xB1 in it", ConvertToEscaped("String with \xC2\xB1 in it"));
    EXPECT_EQ("String with &deg; in it", ConvertToEscaped(R"(String with \u00B0 in it)"));
    EXPECT_EQ("String with &deg; in it", ConvertToEscaped(R"(String with \xB0 in it)"));
    EXPECT_ANY_THROW(ConvertToEscaped(R"(String with \u in it)"));
    EXPECT_ANY_THROW(ConvertToEscaped(R"(String with \x in it)"));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_ConvertUnicodeToUTF8)
{
    {
        std::string test;
        test += static_cast<char>(0);
        EXPECT_EQ(test, ConvertUnicodeToUTF8(std::stoul("0x0000", nullptr, 16)));
    }
    EXPECT_EQ("\x7F", ConvertUnicodeToUTF8(std::stoul("0x7F", nullptr, 16)));
    EXPECT_EQ("\xC2\xB0", ConvertUnicodeToUTF8(std::stoul("0xB0", nullptr, 16)));
    EXPECT_EQ("\xC2\xB0", ConvertUnicodeToUTF8(std::stoul("0x00B0", nullptr, 16)));
    EXPECT_EQ("\xEF\xBF\xBF", ConvertUnicodeToUTF8(std::stoul("0xFFFF", nullptr, 16)));
    EXPECT_EQ("\xF4\x8F\xBF\xBF", ConvertUnicodeToUTF8(std::stoul("0x10FFFF", nullptr, 16)));
    EXPECT_EQ("", ConvertUnicodeToUTF8(std::stoul("0x110000", nullptr, 16)));
    EXPECT_EQ("", ConvertUnicodeToUTF8(std::stoul("0x1FFFFF", nullptr, 16)));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_GetUnitSubStringTest)
{
    EXPECT_EQ("", GetUnitSubString(""));
    EXPECT_EQ("", GetUnitSubString(" "));
    EXPECT_EQ("J/KG", GetUnitSubString("[J/KG]"));
    EXPECT_EQ("M3/S-PERSON", GetUnitSubString(" [M3/S-PERSON]")); // leading space
    EXPECT_EQ("W/M2-K", GetUnitSubString("[W/M2-K] "));           // trailing space
    EXPECT_EQ("MJ/m2", GetUnitSubString(" [MJ/m2] "));            // leading and trailing space
    EXPECT_EQ("PA", GetUnitSubString("This is a column header with units [PA] "));
    EXPECT_EQ("W", GetUnitSubString("This is a column header with units [W] "));
    EXPECT_EQ("K/M", GetUnitSubString("This is a column header with units [K/M] and trailing text."));
}

TEST_F(EnergyPlusFixture, OutputReportTabular_ZoneMultiplierTest)
{
    // AUTHOR: R. Raustad, FSEC
    // DATE WRITTEN: Sep 2015

    std::string const idf_objects = delimited_string({

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
        " SequentialLoad,           !- Load Distribution Scheme",
        " ZoneHVAC:WindowAirConditioner, !- Zone Equipment 1 Object Type",
        " WindAC,                   !- Zone Equipment 1 Name",
        " 1,                        !- Zone Equipment 1 Cooling Sequence",
        " 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",
        " ",
        "ZoneHVAC:EquipmentList,",
        " Spacex10 Eq,              !- Name",
        " SequentialLoad,           !- Load Distribution Scheme",
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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ManageSimulation(*state); // run the design day over the warmup period (24 hrs, 25 days)

    EXPECT_EQ(10.0,
              (state->dataHeatBal->Zone(2).Volume * state->dataHeatBal->Zone(2).Multiplier * state->dataHeatBal->Zone(2).ListMultiplier) /
                  (state->dataHeatBal->Zone(1).Volume * state->dataHeatBal->Zone(1).Multiplier * state->dataHeatBal->Zone(1).ListMultiplier));
    // leaving a little wiggle room on these
    EXPECT_NEAR(10.0, (state->dataDXCoils->DXCoil(2).RatedTotCap(1) / state->dataDXCoils->DXCoil(1).RatedTotCap(1)), 0.00001);
    EXPECT_NEAR(10.0, (state->dataDXCoils->DXCoil(2).RatedAirVolFlowRate(1) / state->dataDXCoils->DXCoil(1).RatedAirVolFlowRate(1)), 0.00001);
    EXPECT_NEAR(10.0,
                (state->dataZoneEnergyDemand->ZoneSysEnergyDemand(2).TotalOutputRequired /
                 state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).TotalOutputRequired),
                0.00001);

    state->dataGlobal->DoWeathSim = true; // flag to trick tabular reports to scan meters
    state->dataGlobal->KindOfSim =
        DataGlobalConstants::KindOfSim::RunPeriodWeather; // fake a weather run since a weather file can't be used (could it?)
    UpdateTabularReports(*state, OutputProcessor::TimeStepType::TimeStepSystem);

    // zone equipment should report single zone magnitude, multipliers do not apply, should be > 0 or what's the point
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleRadGain, state->dataHeatBal->ZnRpt(2).PeopleRadGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleConGain, state->dataHeatBal->ZnRpt(2).PeopleConGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleSenGain, state->dataHeatBal->ZnRpt(2).PeopleSenGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleNumOcc, state->dataHeatBal->ZnRpt(2).PeopleNumOcc);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleLatGain, state->dataHeatBal->ZnRpt(2).PeopleLatGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleTotGain, state->dataHeatBal->ZnRpt(2).PeopleTotGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleRadGainRate, state->dataHeatBal->ZnRpt(2).PeopleRadGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleConGainRate, state->dataHeatBal->ZnRpt(2).PeopleConGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleSenGainRate, state->dataHeatBal->ZnRpt(2).PeopleSenGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleLatGainRate, state->dataHeatBal->ZnRpt(2).PeopleLatGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).PeopleTotGainRate, state->dataHeatBal->ZnRpt(2).PeopleTotGainRate);

    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsPower, state->dataHeatBal->ZnRpt(2).LtsPower);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsElecConsump, state->dataHeatBal->ZnRpt(2).LtsElecConsump);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsRadGain, state->dataHeatBal->ZnRpt(2).LtsRadGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsVisGain, state->dataHeatBal->ZnRpt(2).LtsVisGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsConGain, state->dataHeatBal->ZnRpt(2).LtsConGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsRetAirGain, state->dataHeatBal->ZnRpt(2).LtsRetAirGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsTotGain, state->dataHeatBal->ZnRpt(2).LtsTotGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsRadGainRate, state->dataHeatBal->ZnRpt(2).LtsRadGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsVisGainRate, state->dataHeatBal->ZnRpt(2).LtsVisGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsConGainRate, state->dataHeatBal->ZnRpt(2).LtsConGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsRetAirGainRate, state->dataHeatBal->ZnRpt(2).LtsRetAirGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsTotGainRate, state->dataHeatBal->ZnRpt(2).LtsTotGainRate);

    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecPower, state->dataHeatBal->ZnRpt(2).ElecPower);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecConsump, state->dataHeatBal->ZnRpt(2).ElecConsump);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecRadGain, state->dataHeatBal->ZnRpt(2).ElecRadGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecConGain, state->dataHeatBal->ZnRpt(2).ElecConGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecLatGain, state->dataHeatBal->ZnRpt(2).ElecLatGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecLost, state->dataHeatBal->ZnRpt(2).ElecLost);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecTotGain, state->dataHeatBal->ZnRpt(2).ElecTotGain);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecRadGainRate, state->dataHeatBal->ZnRpt(2).ElecRadGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecConGainRate, state->dataHeatBal->ZnRpt(2).ElecConGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecLatGainRate, state->dataHeatBal->ZnRpt(2).ElecLatGainRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecLostRate, state->dataHeatBal->ZnRpt(2).ElecLostRate);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecTotGainRate, state->dataHeatBal->ZnRpt(2).ElecTotGainRate);

    // expect occupancy time data to be equal
    EXPECT_EQ(state->dataHeatBal->ZonePreDefRep(1).NumOccAccumTime, state->dataHeatBal->ZonePreDefRep(2).NumOccAccumTime);
    EXPECT_EQ(state->dataHeatBal->ZonePreDefRep(1).TotTimeOcc, state->dataHeatBal->ZonePreDefRep(2).TotTimeOcc);

    // occupancy is reported on a zone basis without multipliers (until this changes, expect results to be equal)
    EXPECT_EQ(state->dataHeatBal->ZonePreDefRep(1).NumOccAccum, state->dataHeatBal->ZonePreDefRep(2).NumOccAccum);

    // expect energy to report according to multipliers
    EXPECT_NEAR(10.0, (state->dataHeatBal->ZonePreDefRep(2).MechVentVolTotalOcc / state->dataHeatBal->ZonePreDefRep(1).MechVentVolTotalOcc), 0.00001);
    EXPECT_NEAR(10.0, (state->dataHeatBal->ZonePreDefRep(2).MechVentVolMin / state->dataHeatBal->ZonePreDefRep(1).MechVentVolMin), 0.00001);
    EXPECT_NEAR(10.0, (state->dataHeatBal->ZonePreDefRep(2).SHGSAnZoneEqCl / state->dataHeatBal->ZonePreDefRep(1).SHGSAnZoneEqCl), 0.00001);
    EXPECT_NEAR(10.0, (state->dataHeatBal->ZonePreDefRep(2).SHGSAnPeoplAdd / state->dataHeatBal->ZonePreDefRep(1).SHGSAnPeoplAdd), 0.00001);
    EXPECT_NEAR(10.0, (state->dataHeatBal->ZonePreDefRep(2).SHGSAnLiteAdd / state->dataHeatBal->ZonePreDefRep(1).SHGSAnLiteAdd), 0.00001);
    EXPECT_NEAR(10.0, (state->dataHeatBal->ZonePreDefRep(2).SHGSAnEquipAdd / state->dataHeatBal->ZonePreDefRep(1).SHGSAnEquipAdd), 0.00001);
    EXPECT_NEAR(10.0, (state->dataHeatBal->ZonePreDefRep(2).SHGSAnOtherRem / state->dataHeatBal->ZonePreDefRep(1).SHGSAnOtherRem), 0.00001);
    EXPECT_NEAR(10.0, (state->dataHeatBal->ZonePreDefRep(2).clPeak / state->dataHeatBal->ZonePreDefRep(1).clPeak), 0.00001);
}

TEST_F(EnergyPlusFixture, AirloopHVAC_ZoneSumTest)
{
    // AUTHOR: R. Raustad, FSEC
    // DATE WRITTEN: Sep 2015

    std::string const idf_objects = delimited_string({

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
        "  ,                        !- Pressure Drop Curve Name",
        "  AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
        "  DOAS OA System,          !- Component 1 Name",
        "  DOAS Air Loop Inlet,     !- Component 1 Inlet Node Name",
        "  DOAS Mixed Air Outlet,   !- Component 1 Outlet Node Name",
        "  CoilSystem:Cooling:DX,   !- Component 2 Object Type",
        "  DOAS Cooling Coil,       !- Component 2 Name",
        "  DOAS Mixed Air Outlet,   !- Component 2 Inlet Node Name",
        "  DOAS Cooling Coil Outlet,!- Component 2 Outlet Node Name",
        "  Coil:Heating:Fuel,        !- Component 2 Object Type",
        "  DOAS Heating Coil,       !- Component 2 Name",
        "  DOAS Cooling Coil Outlet,  !- Component 2 Inlet Node Name",
        "  DOAS Heating Coil Outlet,!- Component 2 Outlet Node Name",
        "  Fan:VariableVolume,      !- Component 3 Object Type",
        "  DOAS Supply Fan,         !- Component 3 Name",
        "  DOAS Heating Coil Outlet,!- Component 3 Inlet Node Name",
        "  DOAS Supply Fan Outlet;  !- Component 3 Outlet Node Name",

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
        "	,                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
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

        "Coil:Heating:Fuel,",
        "  DOAS Heating Coil,       !- Name",
        "  AvailSched,              !- Availability Schedule Name",
        "  NaturalGas,              !- Fuel Type",
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
        " SequentialLoad,           !- Load Distribution Scheme",
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
        " SequentialLoad,           !- Load Distribution Scheme",
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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ManageSimulation(*state); // run the design day over the warmup period (24 hrs, 25 days)

    EXPECT_EQ(10.0,
              (state->dataHeatBal->Zone(2).Volume * state->dataHeatBal->Zone(2).Multiplier * state->dataHeatBal->Zone(2).ListMultiplier) /
                  (state->dataHeatBal->Zone(1).Volume * state->dataHeatBal->Zone(1).Multiplier * state->dataHeatBal->Zone(1).ListMultiplier));

    state->dataGlobal->DoWeathSim = true; // flag to trick tabular reports to scan meters
    state->dataGlobal->KindOfSim =
        DataGlobalConstants::KindOfSim::RunPeriodWeather; // fake a weather run since a weather file can't be used (could it?)
    UpdateTabularReports(*state, OutputProcessor::TimeStepType::TimeStepSystem);

    EXPECT_NEAR(1.86168, state->dataSize->FinalSysSizing(1).DesOutAirVolFlow, 0.0001);
}

// TEST_F( EnergyPlusFixture, AirloopHVAC_VentilationRateProcedure )
//{
//// AUTHOR: R. Raustad, FSEC
//// DATE WRITTEN: Sep 2015

// std::string const idf_objects = delimited_string( {
//" Output:Diagnostics, DisplayExtraWarnings;",
//" Timestep, 4;",
//" BUILDING, AirloopHVAC_VentilationRateProcedure, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
//" SimulationControl, YES, YES, NO, YES, NO;",

//"  Site:Location,",
//"    Miami Intl Ap FL USA TMY3 WMO=722020E,    !- Name",
//"    25.82,                 !- Latitude {deg}",
//"    -80.30,                !- Longitude {deg}",
//"    -5.00,                 !- Time Zone {hr}",
//"    11;                    !- Elevation {m}",

//"SizingPeriod:DesignDay,",
//" Miami Intl Ap Ann Clg .4% Condns DB/MCWB, !- Name",
//" 7,                        !- Month",
//" 21,                       !- Day of Month",
//" SummerDesignDay,          !- Day Type",
//" 31.7,                     !- Maximum Dry - Bulb Temperature{ C }",
//" 10.0,                      !- Daily Dry - Bulb Temperature Range{ deltaC }",
//" ,                         !- Dry - Bulb Temperature Range Modifier Type",
//" ,                         !- Dry - Bulb Temperature Range Modifier Day Schedule Name",
//" Wetbulb,                  !- Humidity Condition Type",
//" 22.7,                     !- Wetbulb or DewPoint at Maximum Dry - Bulb{ C }",
//" ,                         !- Humidity Condition Day Schedule Name",
//" ,                         !- Humidity Ratio at Maximum Dry - Bulb{ kgWater / kgDryAir }",
//" ,                         !- Enthalpy at Maximum Dry - Bulb{ J / kg }",
//" ,                         !- Daily Wet - Bulb Temperature Range{ deltaC }",
//" 101217.,                  !- Barometric Pressure{ Pa }",
//" 3.8,                      !- Wind Speed{ m / s }",
//" 340,                      !- Wind Direction{ deg }",
//" No,                       !- Rain Indicator",
//" No,                       !- Snow Indicator",
//" No,                       !- Daylight Saving Time Indicator",
//" ASHRAEClearSky,           !- Solar Model Indicator",
//" ,                         !- Beam Solar Day Schedule Name",
//" ,                         !- Diffuse Solar Day Schedule Name",
//" ,                         !- ASHRAE Clear Sky Optical Depth for Beam Irradiance( taub ) { dimensionless }",
//" ,                         !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance( taud ) { dimensionless }",
//" 1.00;                     !- Sky Clearness",

//"SizingPeriod:DesignDay,",
//" Miami Intl Ap Ann Htg 99.6% Condns DB, !- Name",
//" 1,                        !- Month",
//" 21,                       !- Day of Month",
//" WinterDesignDay,          !- Day Type",
//" 8.7,                      !- Maximum Dry - Bulb Temperature{ C }",
//" 0.0,                      !- Daily Dry - Bulb Temperature Range{ deltaC }",
//" ,                         !- Dry - Bulb Temperature Range Modifier Type",
//" ,                         !- Dry - Bulb Temperature Range Modifier Day Schedule Name",
//" Wetbulb,                  !- Humidity Condition Type",
//" 8.7,                      !- Wetbulb or DewPoint at Maximum Dry - Bulb{ C }",
//" ,                         !- Humidity Condition Day Schedule Name",
//" ,                         !- Humidity Ratio at Maximum Dry - Bulb{ kgWater / kgDryAir }",
//" ,                         !- Enthalpy at Maximum Dry - Bulb{ J / kg }",
//" ,                         !- Daily Wet - Bulb Temperature Range{ deltaC }",
//" 101217.,                  !- Barometric Pressure{ Pa }",
//" 3.8,                      !- Wind Speed{ m / s }",
//" 340,                      !- Wind Direction{ deg }",
//" No,                       !- Rain Indicator",
//" No,                       !- Snow Indicator",
//" No,                       !- Daylight Saving Time Indicator",
//" ASHRAEClearSky,           !- Solar Model Indicator",
//" ,                         !- Beam Solar Day Schedule Name",
//" ,                         !- Diffuse Solar Day Schedule Name",
//" ,                         !- ASHRAE Clear Sky Optical Depth for Beam Irradiance( taub ) { dimensionless }",
//" ,                         !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance( taud ) { dimensionless }",
//" 0.00;                     !- Sky Clearness",

//"OutputControl:Table:Style,",
//"  HTML;                    !- Column Separator",

//"Output:Table:SummaryReports,",
//"  AllSummaryAndSizingPeriod; !- Report 1 Name",

//"Zone,",
//"  Space,                   !- Name",
//"  0.0000,                  !- Direction of Relative North {deg}",
//"  0.0000,                  !- X Origin {m}",
//"  0.0000,                  !- Y Origin {m}",
//"  0.0000,                  !- Z Origin {m}",
//"  1,                       !- Type",
//"  1,                       !- Multiplier",
//"  2.4,                     !- Ceiling Height {m}",
//"  ,                        !- Volume {m3}",
//"  autocalculate,           !- Floor Area {m2}",
//"  ,                        !- Zone Inside Convection Algorithm",
//"  ,                        !- Zone Outside Convection Algorithm",
//"  Yes;                     !- Part of Total Floor Area",

//"ZoneGroup,",
//" Zone Group,               !- Name",
//" Zone List,                !- Zone List Name",
//" 10;                       !- Zone List Multiplier",

//"ZoneList,",
//" Zone List,                !- Name",
//" Spacex10;                 !- Zone 1 Name",

//"Zone,",
//"  Spacex10,                !- Name",
//"  0.0000,                  !- Direction of Relative North {deg}",
//"  0.0000,                  !- X Origin {m}",
//"  0.0000,                  !- Y Origin {m}",
//"  0.0000,                  !- Z Origin {m}",
//"  1,                       !- Type",
//"  1,                       !- Multiplier",
//"  2.4,                     !- Ceiling Height {m}",
//"  ,                        !- Volume {m3}",
//"  autocalculate,           !- Floor Area {m2}",
//"  ,                        !- Zone Inside Convection Algorithm",
//"  ,                        !- Zone Outside Convection Algorithm",
//"  Yes;                     !- Part of Total Floor Area",

//"Sizing:Zone,",
//" Space,                    !- Zone or ZoneList Name",
//" SupplyAirTemperature,     !- Zone Cooling Design Supply Air Temperature Input Method",
//" 12.,                      !- Zone Cooling Design Supply Air Temperature{ C }",
//" ,                         !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
//" SupplyAirTemperature,     !- Zone Heating Design Supply Air Temperature Input Method",
//" 50.,                      !- Zone Heating Design Supply Air Temperature{ C }",
//" ,                         !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
//" 0.008,                    !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
//" 0.008,                    !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
//" Space DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name",
//" 0.0,                      !- Zone Heating Sizing Factor",
//" 0.0,                      !- Zone Cooling Sizing Factor",
//" DesignDay,                !- Cooling Design Air Flow Method",
//" 0,                        !- Cooling Design Air Flow Rate{ m3 / s }",
//" ,                         !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
//" ,                         !- Cooling Minimum Air Flow{ m3 / s }",
//" ,                         !- Cooling Minimum Air Flow Fraction",
//" DesignDay,                !- Heating Design Air Flow Method",
//" 0,                        !- Heating Design Air Flow Rate{ m3 / s }",
//" ,                         !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
//" ,                         !- Heating Maximum Air Flow{ m3 / s }",
//" ;                         !- Heating Maximum Air Flow Fraction",

//"Sizing:Zone,",
//" Spacex10,                 !- Zone or ZoneList Name",
//" SupplyAirTemperature,     !- Zone Cooling Design Supply Air Temperature Input Method",
//" 12.,                      !- Zone Cooling Design Supply Air Temperature{ C }",
//" ,                         !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
//" SupplyAirTemperature,     !- Zone Heating Design Supply Air Temperature Input Method",
//" 50.,                      !- Zone Heating Design Supply Air Temperature{ C }",
//" ,                         !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
//" 0.008,                    !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
//" 0.008,                    !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
//" Spacex10 DSOA Design OA Spec, !- Design Specification Outdoor Air Object Name",
//" 0.0,                      !- Zone Heating Sizing Factor",
//" 0.0,                      !- Zone Cooling Sizing Factor",
//" DesignDay,                !- Cooling Design Air Flow Method",
//" 0,                        !- Cooling Design Air Flow Rate{ m3 / s }",
//" ,                         !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
//" ,                         !- Cooling Minimum Air Flow{ m3 / s }",
//" ,                         !- Cooling Minimum Air Flow Fraction",
//" DesignDay,                !- Heating Design Air Flow Method",
//" 0,                        !- Heating Design Air Flow Rate{ m3 / s }",
//" ,                         !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
//" ,                         !- Heating Maximum Air Flow{ m3 / s }",
//" ;                         !- Heating Maximum Air Flow Fraction",
//" ",

//"People,",
//" Space People,             !- Name",
//" Space,                    !- Zone or ZoneList Name",
//" OnSched,                  !- Number of People Schedule Name",
//" people,                   !- Number of People Calculation Method",
//" 11,                       !- Number of People",
//" ,                         !- People per Zone Floor Area{ person / m2 }",
//" ,                         !- Zone Floor Area per Person{ m2 / person }",
//" 0.3,                      !- Fraction Radiant",
//" AutoCalculate,            !- Sensible Heat Fraction",
//" ActivityLevelSched;       !- Activity Level Schedule Name",

//"People,",
//" Spacex10 People,          !- Name",
//" Spacex10,                 !- Zone or ZoneList Name",
//" OnSched,                  !- Number of People Schedule Name",
//" people,                   !- Number of People Calculation Method",
//" 11,                       !- Number of People",
//" ,                         !- People per Zone Floor Area{ person / m2 }",
//" ,                         !- Zone Floor Area per Person{ m2 / person }",
//" 0.3,                      !- Fraction Radiant",
//" AutoCalculate,            !- Sensible Heat Fraction",
//" ActivityLevelSched;       !- Activity Level Schedule Name",

//"Lights,",
//" Space Lights,             !- Name",
//" Space,                    !- Zone or ZoneList Name",
//" OnSched,                  !- Schedule Name",
//" Watts/Area,               !- Design Level Calculation Method",
//" ,                         !- Lighting Level{ W }",
//" 10.0,                     !- Watts per Zone Floor Area{ W / m2 }",
//" ,                         !- Watts per Person{ W / person }",
//" 0.1,                      !- Return Air Fraction",
//" 0.59,                     !- Fraction Radiant",
//" 0.2,                      !- Fraction Visible",
//" 0,                        !- Fraction Replaceable",
//" GeneralLights;            !- End - Use Subcategory",

//"Lights,",
//" Space Lights x10,         !- Name",
//" Spacex10,                 !- Zone or ZoneList Name",
//" OnSched,                  !- Schedule Name",
//" Watts/Area,               !- Design Level Calculation Method",
//" ,                         !- Lighting Level{ W }",
//" 10.0,                     !- Watts per Zone Floor Area{ W / m2 }",
//" ,                         !- Watts per Person{ W / person }",
//" 0.1,                      !- Return Air Fraction",
//" 0.59,                     !- Fraction Radiant",
//" 0.2,                      !- Fraction Visible",
//" 0,                        !- Fraction Replaceable",
//" GeneralLights;            !- End - Use Subcategory",

//"ElectricEquipment,",
//" Space ElecEq,             !- Name",
//" Space,                    !- Zone or ZoneList Name",
//" OnSched,                  !- Schedule Name",
//" Watts/Area,               !- Design Level Calculation Method",
//" ,                         !- Design Level{ W }",
//" 20.0,                     !- Watts per Zone Floor Area{ W / m2 }",
//" ,                         !- Watts per Person{ W / person }",
//" 0.1,                      !- Fraction Latent",
//" 0.3,                      !- Fraction Radiant",
//" 0.1;                      !- Fraction Lost",

//"ElectricEquipment,",
//" Space ElecEq x10,         !- Name",
//" Spacex10,                 !- Zone or ZoneList Name",
//" OnSched,                  !- Schedule Name",
//" Watts/Area,               !- Design Level Calculation Method",
//" ,                         !- Design Level{ W }",
//" 20.0,                     !- Watts per Zone Floor Area{ W / m2 }",
//" ,                         !- Watts per Person{ W / person }",
//" 0.1,                      !- Fraction Latent",
//" 0.3,                      !- Fraction Radiant",
//" 0.1;                      !- Fraction Lost",

//"Schedule:Compact,",
//" OnSched,                  !- Name",
//" Fraction,                 !- Schedule Type Limits Name",
//" Through: 12/31,           !- Field 1",
//" For: AllDays,             !- Field 2",
//" Until: 24:00, 1.0;        !- Field 26",

//"ScheduleTypeLimits,",
//" Fraction,                 !- Name",
//" 0.0,                      !- Lower Limit Value",
//" 1.0,                      !- Upper Limit Value",
//" CONTINUOUS;               !- Numeric Type",

//"Construction,",
//" INT-WALL-1,               !- Name",
//" GP02,                     !- Outside Layer",
//" AL21,                     !- Layer 2",
//" GP02;                     !- Layer 3",

//"Material,",
//" GP02,                     !- Name",
//" MediumSmooth,             !- Roughness",
//" 1.5900001E-02,            !- Thickness{ m }",
//" 0.1600000,                !- Conductivity{ W / m - K }",
//" 801.0000,                 !- Density{ kg / m3 }",
//" 837.0000,                 !- Specific Heat{ J / kg - K }",
//" 0.9000000,                !- Thermal Absorptance",
//" 0.7500000,                !- Solar Absorptance",
//" 0.7500000;                !- Visible Absorptance",

//"Material:AirGap,",
//" AL21,                     !- Name",
//" 0.1570000;                !- Thermal Resistance{ m2 - K / W }",

//"Construction,",
//"FLOOR-SLAB-1,              !- Name",
//"CC03,                      !- Outside Layer",
//"CP01;                      !- Layer 2",

//"Material,",
//" CC03,                     !- Name",
//" MediumRough,              !- Roughness",
//" 0.1016000,                !- Thickness{ m }",
//" 1.310000,                 !- Conductivity{ W / m - K }",
//" 2243.000,                 !- Density{ kg / m3 }",
//" 837.0000,                 !- Specific Heat{ J / kg - K }",
//" 0.9000000,                !- Thermal Absorptance",
//" 0.6500000,                !- Solar Absorptance",
//" 0.6500000;                !- Visible Absorptance",

//"Material:NoMass,",
//" CP01,                     !- Name",
//" Rough,                    !- Roughness",
//" 0.3670000,                !- Thermal Resistance{ m2 - K / W }",
//" 0.9000000,                !- Thermal Absorptance",
//" 0.7500000,                !- Solar Absorptance",
//" 0.7500000;                !- Visible Absorptance",

//"Construction,",
//" CLNG-1,                   !- Name",
//" MAT-CLNG-1;               !- Outside Layer",

//"Material:NoMass,",
//" MAT-CLNG-1,               !- Name",
//" Rough,                    !- Roughness",
//" 0.652259290,              !- Thermal Resistance{ m2 - K / W }",
//" 0.65,                     !- Thermal Absorptance",
//" 0.65,                     !- Solar Absorptance",
//" 0.65;                     !- Visible Absorptance",

//"BuildingSurface:Detailed,",
//" FRONT-1,                  !- Name",
//" WALL,                     !- Surface Type",
//" INT-WALL-1,               !- Construction Name",
//" Space,                    !- Zone Name",
//" Outdoors,                 !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" SunExposed,               !- Sun Exposure",
//" WindExposed,              !- Wind Exposure",
//" 0.50000,                  !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
//" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
//" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
//" 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" C1-1,                     !- Name",
//" CEILING,                  !- Surface Type",
//" CLNG-1,                   !- Construction Name",
//" Space,                    !- Zone Name",
//" Outdoors,                 !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
//" 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 2 {m}",
//" 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 3 {m}",
//" 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" F1-1,                     !- Name",
//" FLOOR,                    !- Surface Type",
//" FLOOR-SLAB-1,             !- Construction Name",
//" Space,                    !- Zone Name",
//" Ground,                   !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 1 {m}",
//" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
//" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
//" 3.7, 3.7, 0.0;            !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" SB12,                     !- Name",
//" WALL,                     !- Surface Type",
//" INT-WALL-1,               !- Construction Name",
//" Space,                    !- Zone Name",
//" Adiabatic,                !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
//" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
//" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
//" 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" SB14,                     !- Name",
//" WALL,                     !- Surface Type",
//" INT-WALL-1,               !- Construction Name",
//" Space,                    !- Zone Name",
//" Adiabatic,                !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
//" 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
//" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
//" 0.0, 0.0, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" SB15,                     !- Name",
//" WALL,                     !- Surface Type",
//" INT-WALL-1,               !- Construction Name",
//" Space,                    !- Zone Name",
//" Adiabatic,                !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 26.8, 3.7, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
//" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
//" 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
//" 3.7, 3.7, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

//"ZoneControl:Thermostat,",
//" Space Thermostat,         !- Name",
//" Space,                    !- Zone or ZoneList Name",
//" Dual Zone Control Type Sched,  !- Control Type Schedule Name",
//" ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
//" Space DualSPSched;        !- Control 1 Name",

//"ZoneControl:Thermostat,",
//" Spacex10 Thermostat,      !- Name",
//" Spacex10,                 !- Zone or ZoneList Name",
//" Dual Zone Control Type Sched,  !- Control Type Schedule Name",
//" ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
//" Space DualSPSched;        !- Control 1 Name",

//"Schedule:Compact,",
//" Dual Zone Control Type Sched,  !- Name",
//" Any Number,               !- Schedule Type Limits Name",
//" Through: 12/31,           !- Field 1",
//" For: AllDays,             !- Field 2",
//" Until: 24:00,4;           !- Field 3",

//"ThermostatSetpoint:DualSetpoint,",
//" Space DualSPSched,        !- Name",
//" HTGSETP_SCH,              !- Heating Setpoint Temperature Schedule Name",
//" CLGSETP_SCH;              !- Cooling Setpoint Temperature Schedule Name",

//"Schedule:Compact,",
//" CLGSETP_SCH,              !- Name",
//" Any Number,               !- Schedule Type Limits Name",
//" Through: 12/31,           !- Field 1",
//" For: AllDays,             !- Field 19",
//" Until: 24:00,22.1;        !- Field 20",

//"Schedule:Compact,",
//" HTGSETP_SCH,              !- Name",
//" Any Number,               !- Schedule Type Limits Name",
//" Through: 12/31,           !- Field 1",
//" For: AllDays,             !- Field 22",
//" Until: 24:00, 21.9;       !- Field 23",

//"BuildingSurface:Detailed,",
//" FRONT-1x10,               !- Name",
//" WALL,                     !- Surface Type",
//" INT-WALL-1,               !- Construction Name",
//" Spacex10,                 !- Zone Name",
//" Outdoors,                 !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" SunExposed,               !- Sun Exposure",
//" WindExposed,              !- Wind Exposure",
//" 0.50000,                  !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
//" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
//" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
//" 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" C1-1x10,                  !- Name",
//" CEILING,                  !- Surface Type",
//" CLNG-1,                   !- Construction Name",
//" Spacex10,                 !- Zone Name",
//" Outdoors,                 !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
//" 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 2 {m}",
//" 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 3 {m}",
//" 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" F1-1x10,                  !- Name",
//" FLOOR,                    !- Surface Type",
//" FLOOR-SLAB-1,             !- Construction Name",
//" Spacex10,                 !- Zone Name",
//" Ground,                   !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 1 {m}",
//" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
//" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
//" 3.7, 3.7, 0.0;            !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" SB12x10,                  !- Name",
//" WALL,                     !- Surface Type",
//" INT-WALL-1,               !- Construction Name",
//" Spacex10,                 !- Zone Name",
//" Adiabatic,                !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
//" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
//" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
//" 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" SB14x10,                  !- Name",
//" WALL,                     !- Surface Type",
//" INT-WALL-1,               !- Construction Name",
//" Spacex10,                 !- Zone Name",
//" Adiabatic,                !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
//" 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
//" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
//" 0.0, 0.0, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

//"BuildingSurface:Detailed,",
//" SB15x10,                  !- Name",
//" WALL,                     !- Surface Type",
//" INT-WALL-1,               !- Construction Name",
//" Spacex10,                 !- Zone Name",
//" Adiabatic,                !- Outside Boundary Condition",
//" ,                         !- Outside Boundary Condition Object",
//" NoSun,                    !- Sun Exposure",
//" NoWind,                   !- Wind Exposure",
//" 0.0,                      !- View Factor to Ground",
//" 4,                        !- Number of Vertices",
//" 26.8, 3.7, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
//" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
//" 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
//" 3.7, 3.7, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

//"Sizing:System,",
//" DOAS,                     !- AirLoop Name",
//" VentilationRequirement,   !- Type of Load to Size On",
//" autosize,                 !- Design Outdoor Air Flow Rate {m3/s}",
//" 1.0,                      !- Central Heating Maximum System Air Flow Ratio",
//" 2,                        !- Preheat Design Temperature {C}",
//" 0.008,                    !- Preheat Design Humidity Ratio {kgWater/kgDryAir}",
//" 11,                       !- Precool Design Temperature {C}",
//" 0.008,                    !- Precool Design Humidity Ratio {kgWater/kgDryAir}",
//" 16,                       !- Central Cooling Design Supply Air Temperature {C}",
//" 12.2,                     !- Central Heating Design Supply Air Temperature {C}",
//" NonCoincident,            !- Type of Zone Sum to Use",
//" Yes,                      !- 100% Outdoor Air in Cooling",
//" Yes,                      !- 100% Outdoor Air in Heating",
//" 0.0103,                   !- Central Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
//" 0.003,                    !- Central Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
//" DesignDay,                !- Cooling Supply Air Flow Rate Method",
//" 0,                        !- Cooling Supply Air Flow Rate {m3/s}",
//" ,                         !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
//" ,                         !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
//" ,                         !- Cooling Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}",
//" DesignDay,                !- Heating Supply Air Flow Rate Method",
//" 0,                        !- Heating Supply Air Flow Rate {m3/s}",
//" ,                         !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
//" ,                         !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
//" ,                         !- Heating Fraction of Autosized Cooling Supply Air Flow Rate",
//" ,                         !- Heating Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}",
//" VentilationRateProcedure, !- System Outdoor Air Method",
//" 1.0,                      !- Zone Maximum Outdoor Air Fraction {dimensionless}",
//" CoolingDesignCapacity,    !- Cooling Design Capacity Method",
//" autosize,                 !- Cooling Design Capacity {W}",
//" ,                         !- Cooling Design Capacity Per Floor Area {W/m2}",
//" ,                         !- Fraction of Autosized Cooling Design Capacity",
//" HeatingDesignCapacity,    !- Heating Design Capacity Method",
//" autosize,                 !- Heating Design Capacity {W}",
//" ,                         !- Heating Design Capacity Per Floor Area {W/m2}",
//" ,                         !- Fraction of Autosized Heating Design Capacity",
//" OnOff;                    !- Central Cooling Capacity Control Method",

//"AirLoopHVAC,",
//"  DOAS,                    !- Name",
//"  ,                        !- Controller List Name",
//"  DOAS Availability Managers,  !- Availability Manager List Name",
//"  autosize,                !- Design Supply Air Flow Rate {m3/s}",
//"  DOAS Branches,           !- Branch List Name",
//"  ,                        !- Connector List Name",
//"  DOAS Air Loop Inlet,     !- Supply Side Inlet Node Name",
//"  DOAS Return Air Outlet,  !- Demand Side Outlet Node Name",
//"  DOAS Supply Path Inlet,  !- Demand Side Inlet Node Names",
//"  DOAS Supply Fan Outlet;  !- Supply Side Outlet Node Names",

//"BranchList,",
//"  DOAS Branches,           !- Name",
//"  DOAS Main Branch;        !- Branch 1 Name",

//"Branch,",
//"  DOAS Main Branch,        !- Name",
//"  ,                        !- Pressure Drop Curve Name",
//"  AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
//"  DOAS OA System,          !- Component 1 Name",
//"  DOAS Air Loop Inlet,     !- Component 1 Inlet Node Name",
//"  DOAS Mixed Air Outlet,   !- Component 1 Outlet Node Name",
//"  CoilSystem:Cooling:DX,   !- Component 2 Object Type",
//"  DOAS Cooling Coil,       !- Component 2 Name",
//"  DOAS Mixed Air Outlet,   !- Component 2 Inlet Node Name",
//"  DOAS Cooling Coil Outlet,!- Component 2 Outlet Node Name",
//"  Coil:Heating:Fuel,        !- Component 2 Object Type",
//"  DOAS Heating Coil,       !- Component 2 Name",
//"  DOAS Cooling Coil Outlet,  !- Component 2 Inlet Node Name",
//"  DOAS Heating Coil Outlet,!- Component 2 Outlet Node Name",
//"  Fan:VariableVolume,      !- Component 3 Object Type",
//"  DOAS Supply Fan,         !- Component 3 Name",
//"  DOAS Heating Coil Outlet,!- Component 3 Inlet Node Name",
//"  DOAS Supply Fan Outlet;  !- Component 3 Outlet Node Name",

//"AirLoopHVAC:SupplyPath,",
//"  DOAS Supply Path,        !- Name",
//"  DOAS Supply Path Inlet,  !- Supply Air Path Inlet Node Name",
//"  AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
//"  DOAS Zone Splitter;      !- Component 1 Name",

//"AirLoopHVAC:ZoneSplitter,",
//"  DOAS Zone Splitter,      !- Name",
//"  DOAS Supply Path Inlet,  !- Inlet Node Name",
//"  Space ATU In Node,  !- Outlet 1 Node Name",
//"  Spacex10 ATU In Node;  !- Outlet 27 Node Name",

//"AirLoopHVAC:ReturnPath,",
//"  DOAS Return Path,        !- Name",
//"  DOAS Return Air Outlet,  !- Return Air Path Outlet Node Name",
//"  AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
//"  DOAS Zone Mixer;         !- Component 1 Name",

//"AirLoopHVAC:ZoneMixer,",
//"  DOAS Zone Mixer,         !- Name",
//"  DOAS Return Air Outlet,  !- Outlet Node Name",
//"  Space Ret Node,          !- Inlet 1 Node Name",
//"  Spacex10 Ret Node;       !- Inlet 27 Node Name",

//"AvailabilityManagerAssignmentList,",
//"  DOAS Availability Managers,  !- Name",
//"  AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
//"  DOAS Availability;       !- Availability Manager 1 Name",

//"AvailabilityManager:Scheduled,",
//"  DOAS Availability,       !- Name",
//"  AvailSched;              !- Schedule Name",

//"NodeList,",
//"  DOAS Cooling Setpoint Nodes,  !- Name",
//"  DOAS Cooling Coil Outlet, !- Node 1 Name",
//"  DOAS Heating Coil Outlet; !- Node 1 Name",

//"Schedule:Compact,",
//"  Always 22,               !- Name",
//"  Any Number,              !- Schedule Type Limits Name",
//"  Through: 12/31,          !- Field 1",
//"  For: AllDays,            !- Field 2",
//"  Until: 24:00,22;         !- Field 3",

//"SetpointManager:Scheduled,",
//"  DOAS Cooling Supply Air Temp Manager,  !- Name",
//"  Temperature,             !- Control Variable",
//"  Always 22,               !- Schedule Name",
//"  DOAS Supply Fan Outlet;  !- Setpoint Node or NodeList Name",

//"SetpointManager:MixedAir,",
//"  DOAS Cooling Coil Air Temp Manager,  !- Name",
//"  Temperature,             !- Control Variable",
//"  DOAS Supply Fan Outlet,  !- Reference Setpoint Node Name",
//"  DOAS Cooling Coil Outlet,!- Fan Inlet Node Name",
//"  DOAS Supply Fan Outlet,  !- Fan Outlet Node Name",
//"  DOAS Cooling Setpoint Nodes;  !- Setpoint Node or NodeList Name",

//"CoilSystem:Cooling:DX,",
//"  DOAS Cooling Coil,       !- Name",
//"  AvailSched,              !- Availability Schedule Name",
//"  DOAS Mixed Air Outlet,   !- DX Cooling Coil System Inlet Node Name",
//"  DOAS Cooling Coil Outlet,  !- DX Cooling Coil System Outlet Node Name",
//"  DOAS Cooling Coil Outlet,  !- DX Cooling Coil System Sensor Node Name",
//"  Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
//"  DOAS DX Cooling Coil;    !- Cooling Coil Name",

//"Coil:Cooling:DX:SingleSpeed,",
//"	DOAS DX Cooling Coil,   !- Name",
//" 	AvailSched,            !- Availability Schedule Name",
//"	autosize,              !- Gross Rated Total Cooling Capacity { W }",
//"	autosize,              !- Gross Rated Sensible Heat Ratio",
//"	4.40,                  !- Gross Rated Cooling COP { W / W }",
//"	autosize,              !- Rated Air Flow Rate { m3 / s }",
//"	,                      !- Rated Evaporator Fan Power Per Volume Flow Rate { W / ( m3 / s ) }",
//"	DOAS Mixed Air Outlet, !- Air Inlet Node Name",
//"	DOAS Cooling Coil Outlet,    !- Air Outlet Node Name",
//"	Biquadratic,           !- Total Cooling Capacity Function of Temperature Curve Name",
//"	Cubic,                 !- Total Cooling Capacity Function of Flow Fraction Curve Name",
//"	Biquadratic,           !- Energy Input Ratio Function of Temperature Curve Name",
//"	Cubic,                 !- Energy Input Ratio Function of Flow Fraction Curve Name",
//"	Cubic,                 !- Part Load Fraction Correlation Curve Name",
//"	,                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
//"	0.0,                   !- Nominal Time for Condensate Removal to Begin",
//"	0.0,                   !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
//"	0.0,                   !- Maximum Cycling Rate",
//"	0.0,                   !- Latent Capacity Time Constant",
//"	Cooling Coil Condenser Inlet, !- Condenser Air Inlet Node Name",
//"	EvaporativelyCooled,   !- Condenser Type",
//"	0.0,                   !- Evaporative Condenser Effectiveness",
//"	,                      !- Evaporative Condenser Air Flow Rate",
//"	autosize,              !- Evaporative Condenser Pump Rated Power Consumption",
//"	0.0,                   !- Crankcase Heater Capacity",
//"	10.0;                  !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",

//"Coil:Heating:Fuel,",
//"  DOAS Heating Coil,       !- Name",
//"  AvailSched,              !- Availability Schedule Name",
//"  Gas,                     !- Fuel Type",
//"  0.8,                     !- Gas Burner Efficiency",
//"  autosize,                !- Nominal Capacity {W}",
//"  DOAS Cooling Coil Outlet,  !- Air Inlet Node Name",
//"  DOAS Heating Coil Outlet,  !- Air Outlet Node Name",
//"  DOAS Heating Coil Outlet;  !- Temperature Setpoint Node Name",

//"Fan:VariableVolume,",
//"  DOAS Supply Fan,         !- Name",
//"  AvailSched,              !- Availability Schedule Name",
//"  0.7,                     !- Fan Total Efficiency",
//"  1000,                    !- Pressure Rise {Pa}",
//"  autosize,                !- Maximum Flow Rate {m3/s}",
//"  Fraction,                !- Fan Power Minimum Flow Rate Input Method",
//"  0.0,                     !- Fan Power Minimum Flow Fraction",
//"  ,                        !- Fan Power Minimum Air Flow Rate {m3/s}",
//"  0.9,                     !- Motor Efficiency",
//"  1,                       !- Motor In Airstream Fraction",
//"  0.0015302446,            !- Fan Power Coefficient 1",
//"  0.0052080574,            !- Fan Power Coefficient 2",
//"  1.1086242,               !- Fan Power Coefficient 3",
//"  -0.11635563,             !- Fan Power Coefficient 4",
//"  0,                       !- Fan Power Coefficient 5",
//"  DOAS Heating Coil Outlet,!- Air Inlet Node Name",
//"  DOAS Supply Fan Outlet;  !- Air Outlet Node Name",

//"OutdoorAir:NodeList,",
//"  DOAS Outdoor Air Inlet,  !- Node or NodeList Name 1",
//"  Cooling Coil Condenser Inlet;  !- Node or NodeList Name 2",

//"AirLoopHVAC:OutdoorAirSystem,",
//"  DOAS OA System,          !- Name",
//"  DOAS OA System Controllers,  !- Controller List Name",
//"  DOAS OA System Equipment,!- Outdoor Air Equipment List Name",
//"  DOAS Availability Managers;  !- Availability Manager List Name",

//"AirLoopHVAC:ControllerList,",
//"  DOAS OA System Controllers,  !- Name",
//"  Controller:OutdoorAir,   !- Controller 1 Object Type",
//"  DOAS OA Controller;      !- Controller 1 Name",

//"AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
//"  DOAS OA System Equipment,!- Name",
//"  OutdoorAir:Mixer,        !- Component 1 Object Type",
//"  DOAS OA Mixing Box;      !- Component 1 Name",

//"OutdoorAir:Mixer,",
//"  DOAS OA Mixing Box,      !- Name",
//"  DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
//"  DOAS Outdoor Air Inlet,  !- Outdoor Air Stream Node Name",
//"  DOAS Relief Air Outlet,  !- Relief Air Stream Node Name",
//"  DOAS Air Loop Inlet;     !- Return Air Stream Node Name",

//"Controller:OutdoorAir,",
//"  DOAS OA Controller,      !- Name",
//"  DOAS Relief Air Outlet,  !- Relief Air Outlet Node Name",
//"  DOAS Air Loop Inlet,     !- Return Air Node Name",
//"  DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
//"  DOAS Outdoor Air Inlet,  !- Actuator Node Name",
//"  0,                       !- Minimum Outdoor Air Flow Rate {m3/s}",
//"  autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
//"  NoEconomizer,            !- Economizer Control Type",
//"  MinimumFlowWithBypass,   !- Economizer Control Action Type",
//"  ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
//"  ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
//"  ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
//"  ,                        !- Electronic Enthalpy Limit Curve Name",
//"  12.2,                    !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
//"  NoLockout,               !- Lockout Type",
//"  FixedMinimum,            !- Minimum Limit Type",
//"  ,                        !- Minimum Outdoor Air Schedule Name",
//"  ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
//"  ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
//"  DCVObject;               !- Mechanical Ventilation Controller Name",

//"Controller:MechanicalVentilation,",
//"  DCVObject,               !- Name",
//"  AvailSched,              !- Availability Schedule Name",
//"  Yes,                     !- Demand Controlled Ventilation",
//"  VentilationRateProcedure,!- System Outdoor Air Method",
//"  ,                        !- Zone Maximum Outdoor Air Fraction {dimensionless}",
//"  Space,  !- Zone 1 Name",
//"  Space DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name 1",
//"  Space DSOA Design ADE Spec, !- Design Specification Zone Air Distribution Object Name 1",
//"  Spacex10,      !- Zone 2 Name",
//"  Spacex10 DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name 2",
//"  Space DSOA Design ADE Spec;  !- Design Specification Zone Air Distribution Object Name 2",

//"DesignSpecification:OutdoorAir,",
//"  Space DSOA Design OA Spec,  !- Name",
//"  sum,                     !- Outdoor Air Method",
//"  0.0,                     !- Outdoor Air Flow per Person {m3/s-person}",
//"  0.0009,                  !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
//"  0;                       !- Outdoor Air Flow per Zone {m3/s}",

//"DesignSpecification:OutdoorAir,",
//"  Spacex10 DSOA Design OA Spec,  !- Name",
//"  sum,                     !- Outdoor Air Method",
//"  0.008,                   !- Outdoor Air Flow per Person {m3/s-person}",
//"  0.0009,                  !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
//"  0;                       !- Outdoor Air Flow per Zone {m3/s}",

//"DesignSpecification:ZoneAirDistribution,",
//"  Space DSOA Design ADE Spec,  !- Name",
//"  1,                       !- Zone Air Distribution Effectiveness in Cooling Mode {dimensionless}",
//"  1;                       !- Zone Air Distribution Effectiveness in Heating Mode {dimensionless}",

//"ZoneHVAC:EquipmentConnections,",
//" Space,                    !- Zone Name",
//" Space Eq,                 !- Zone Conditioning Equipment List Name",
//" Space In Node,            !- Zone Air Inlet Node or NodeList Name",
//" ,                         !- Zone Air Exhaust Node or NodeList Name",
//" Space Node,               !- Zone Air Node Name",
//" Space Ret Node;           !- Zone Return Air Node Name",

//"ZoneHVAC:EquipmentList,",
//" Space Eq,                 !- Name",
//" ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
//" Space ATU,                   !- Zone Equipment 1 Name",
//" 1,                        !- Zone Equipment 1 Cooling Sequence",
//" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

//"ZoneHVAC:AirDistributionUnit,",
//"  Space ATU,  !- Name",
//"  Space In Node,  !- Air Distribution Unit Outlet Node Name",
//"  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
//"  Space Air Terminal;  !- Air Terminal Name",

//"AirTerminal:SingleDuct:VAV:NoReheat,",
//"  Space Air Terminal,      !- Name",
//"  AvailSched,              !- Availability Schedule Name",
//"  Space In Node,           !- Air Outlet Node Name",
//"  Space ATU In Node,       !- Air Inlet Node Name",
//"  autosize,                !- Maximum Air Flow Rate {m3/s}",
//"  Constant,                !- Zone Minimum Air Flow Input Method",
//"  0.0,                     !- Constant Minimum Air Flow Fraction",
//"  ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
//"  ,                        !- Minimum Air Flow Fraction Schedule Name",
//"  ;                        !- Design Specification Outdoor Air Object Name",

//"ZoneHVAC:EquipmentConnections,",
//" Spacex10,                 !- Zone Name",
//" Spacex10 Eq,              !- Zone Conditioning Equipment List Name",
//" Spacex10 In Node,         !- Zone Air Inlet Node or NodeList Name",
//" ,                         !- Zone Air Exhaust Node or NodeList Name",
//" Spacex10 Node,            !- Zone Air Node Name",
//" Spacex10 Ret Node;        !- Zone Return Air Node Name",

//"ZoneHVAC:EquipmentList,",
//" Spacex10 Eq,              !- Name",
//" ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
//" Spacex10 ATU,                !- Zone Equipment 1 Name",
//" 1,                        !- Zone Equipment 1 Cooling Sequence",
//" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

//"ZoneHVAC:AirDistributionUnit,",
//"  Spacex10 ATU,  !- Name",
//"  Spacex10 In Node,  !- Air Distribution Unit Outlet Node Name",
//"  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
//"  Spacex10 Air Terminal;  !- Air Terminal Name",

//"AirTerminal:SingleDuct:VAV:NoReheat,",
//"  Spacex10 Air Terminal,   !- Name",
//"  AvailSched,              !- Availability Schedule Name",
//"  Spacex10 In Node,        !- Air Outlet Node Name",
//"  Spacex10 ATU In Node,    !- Air Inlet Node Name",
//"  autosize,                !- Maximum Air Flow Rate {m3/s}",
//"  Constant,                !- Zone Minimum Air Flow Input Method",
//"  0.0,                     !- Constant Minimum Air Flow Fraction",
//"  ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
//"  ,                        !- Minimum Air Flow Fraction Schedule Name",
//"  ;                        !- Design Specification Outdoor Air Object Name",

//"ScheduleTypeLimits,",
//"  Any Number;              !- Name",

//"Schedule:Compact,",
//"  AvailSched,              !- Name",
//"  Any Number,              !- Schedule Type Limits Name",
//"  Through: 12/31,          !- Field 3",
//"  For: AllDays,            !- Field 4",
//"  Until: 24:00,1.0;        !- Field 5",

//"Schedule:Compact,",
//"  EquipSched,              !- Name",
//"  Any Number,              !- Schedule Type Limits Name",
//"  Through: 12/31,          !- Field 3",
//"  For: WinterDesignDay,    !- Field 4",
//"  Until: 24:00,0.0,        !- Field 5",
//"  For: AllOtherDays,       !- Field 4",
//"  Until: 24:00,1.0;        !- Field 5",

//"Schedule:Compact,",
//"  ActivityLevelSched,      !- Name",
//"  Any Number,              !- Schedule Type Limits Name",
//"  Through: 12/31,          !- Field 3",
//"  For: AllDays,            !- Field 4",
//"  Until: 24:00,120.0;      !- Field 5",

//"Schedule:Compact,",
//"  FanOpModeSchedule,       !- Name",
//"  Any Number,              !- Schedule Type Limits Name",
//"  Through: 12/31,          !- Field 1",
//"  For: AllDays,            !- Field 2",
//"  Until: 24:00,1.0;        !- Field 7",

//"Curve:Biquadratic,",
//"  Biquadratic,             !- Name",
//"  1.0,                     !- Coefficient1 Constant",
//"  0.0,                     !- Coefficient2 x",
//"  0.0,                     !- Coefficient3 x**2",
//"  0.0,                     !- Coefficient4 y",
//"  0.0,                     !- Coefficient5 y**2",
//"  0.0,                     !- Coefficient6 x*y",
//"  5,                       !- Minimum Value of x",
//"  40,                      !- Maximum Value of x",
//"  5,                       !- Minimum Value of y",
//"  40,                      !- Maximum Value of y",
//"  ,                        !- Minimum Curve Output",
//"  ,                        !- Maximum Curve Output",
//"  Temperature,             !- Input Unit Type for X",
//"  Temperature,             !- Input Unit Type for Y",
//"  Dimensionless;           !- Output Unit Type",

//"Curve:Cubic,",
//"  Cubic,                   !- Name",
//"  1.0,                     !- Coefficient1 Constant",
//"  0.0,                     !- Coefficient2 x",
//"  0.0,                     !- Coefficient3 x**2",
//"  0,                       !- Coefficient4 x**3",
//"  5,                       !- Minimum Value of x",
//"  40,                      !- Maximum Value of x",
//"  ,                        !- Minimum Curve Output",
//"  ,                        !- Maximum Curve Output",
//"  Temperature,             !- Input Unit Type for X",
//"  Temperature;             !- Output Unit Type",
//} );

// ASSERT_TRUE( process_idf( idf_objects ) );

// OutputProcessor::TimeValue.allocate(2);
////state->dataGlobal->DDOnlySimulation = true;

// ManageSimulation(); // run the design day over the warmup period (24 hrs, 25 days)

// EXPECT_EQ( 10.0, ( Zone( 2 ).Volume * Zone( 2 ).Multiplier * Zone( 2 ).ListMultiplier ) / ( Zone( 1 ).Volume * Zone( 1 ).Multiplier * Zone( 1
// ).ListMultiplier ) );

// state->dataGlobal->DoWeathSim = true; // flag to trick tabular reports to scan meters
// DataGlobals::KindOfSim = DataGlobalConstants::KindOfSim::RunPeriodWeather; // fake a weather run since a weather file can't be used (could it?)
// UpdateTabularReports( OutputProcessor::TimeStepType::TimeStepSystem );

// EXPECT_NEAR( 1.86168, DataSizing::FinalSysSizing( 1 ).DesOutAirVolFlow, 0.0001 );

//}

TEST_F(EnergyPlusFixture, OutputReportTabularMonthly_ResetMonthlyGathering)
{
    std::string const idf_objects = delimited_string({
        "Output:Table:Monthly,",
        "Space Gains Annual Report, !- Name",
        "2, !-  Digits After Decimal",
        "Exterior Lights Electricity Energy, !- Variable or Meter 1 Name",
        "SumOrAverage, !- Aggregation Type for Variable or Meter 1",
        "Exterior Lights Electricity Rate, !- Variable or Meter 2 Name",
        "Maximum, !- Aggregation Type for Variable or Meter 2",
        "Exterior Lights Electricity Rate, !- Variable or Meter 2 Name",
        "Minimum; !- Aggregation Type for Variable or Meter 2",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    Real64 extLitUse;

    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite1",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite2",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite3",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");

    state->dataGlobal->DoWeathSim = true;
    state->dataGlobal->TimeStepZone = 0.25;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * 60.0;

    GetInputTabularMonthly(*state);
    EXPECT_EQ(state->dataOutRptTab->MonthlyInputCount, 1);
    InitializeTabularMonthly(*state);

    extLitUse = 1.01;

    state->dataEnvrn->Month = 12;

    GatherMonthlyResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_EQ(extLitUse * 1, state->dataOutRptTab->MonthlyColumns(1).reslt(12));

    GatherMonthlyResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_EQ(extLitUse * 2, state->dataOutRptTab->MonthlyColumns(1).reslt(12));

    GatherMonthlyResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_EQ(extLitUse * 3, state->dataOutRptTab->MonthlyColumns(1).reslt(12));

    ResetMonthlyGathering(*state);

    EXPECT_EQ(0., state->dataOutRptTab->MonthlyColumns(1).reslt(12));

    GatherMonthlyResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_EQ(extLitUse * 1, state->dataOutRptTab->MonthlyColumns(1).reslt(12));
}

TEST_F(EnergyPlusFixture, OutputReportTabular_ConfirmResetBEPSGathering)
{

    Real64 extLitUse;

    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite1",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite2",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite3",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");

    state->dataGlobal->DoWeathSim = true;
    state->dataGlobal->TimeStepZone = 1.0;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * 60.0;
    state->dataOutRptTab->displayTabularBEPS = true;
    // OutputProcessor::TimeValue.allocate(2);

    auto timeStep = 1.0;

    SetupTimePointers(*state, "Zone", timeStep);
    SetupTimePointers(*state, "HVAC", timeStep);

    *state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::TimeStepZone).TimeStep = 60;
    *state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::TimeStepSystem).TimeStep = 60;

    GetInputOutputTableSummaryReports(*state);

    extLitUse = 1.01;

    state->dataEnvrn->Month = 12;

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_EQ(extLitUse * 3,
              state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)));

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_EQ(extLitUse * 6,
              state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)));

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_EQ(extLitUse * 9,
              state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)));

    ResetBEPSGathering(*state);

    EXPECT_EQ(0., state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)));

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_EQ(extLitUse * 3,
              state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)));
}

TEST_F(EnergyPlusFixture, OutputReportTabular_GatherPeakDemandForTimestep)
{
    // Glazer - Sep 2017

    state->dataOutRptTab->displayDemandEndUse = true;
    state->dataOutRptTab->displayLEEDSummary = true;
    state->dataGlobal->TimeStepZoneSec = 900.0;

    int resourceNum = 1;
    int totalMeterNum = 2;
    int endUseMeterNum = 3;
    int subEndUseMeterNum = 4;

    int endUseNum = 1;
    int subEndUseNum = 1;

    state->dataOutRptTab->meterNumEndUseSubBEPS.allocate(10, 10, 10);
    state->dataOutRptTab->gatherDemandEndUseSub.allocate(10, 10, 10);
    state->dataOutRptTab->gatherDemandIndEndUseSub.allocate(10, 10, 10);
    state->dataOutputProcessor->EnergyMeters.allocate(100);

    state->dataOutputProcessor->EndUseCategory.allocate(endUseNum);
    state->dataOutputProcessor->EndUseCategory(endUseNum).NumSubcategories = 1;

    state->dataOutRptTab->meterNumTotalsBEPS(resourceNum) = totalMeterNum; // create a test meter number
    state->dataOutRptTab->gatherDemandTotal(resourceNum) = 0.;

    state->dataOutRptTab->meterNumEndUseBEPS(resourceNum, endUseNum) = endUseMeterNum;
    state->dataOutRptTab->gatherDemandEndUse(resourceNum, endUseNum) = 0.;
    state->dataOutRptTab->gatherDemandIndEndUse(resourceNum, endUseNum) = 0.;

    state->dataOutRptTab->meterNumEndUseSubBEPS(subEndUseNum, endUseNum, resourceNum) = subEndUseMeterNum;
    state->dataOutRptTab->gatherDemandEndUseSub(subEndUseNum, endUseNum, resourceNum) = 0.;
    state->dataOutRptTab->gatherDemandIndEndUseSub(subEndUseNum, endUseNum, resourceNum) = 0.;

    // first "timestep"

    state->dataOutputProcessor->EnergyMeters(totalMeterNum).CurTSValue =
        123.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the total meter
    state->dataOutputProcessor->EnergyMeters(endUseMeterNum).CurTSValue =
        47.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the end use meter
    state->dataOutputProcessor->EnergyMeters(subEndUseMeterNum).CurTSValue =
        28.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the sub end use meter

    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);

    EXPECT_EQ(123., state->dataOutRptTab->gatherDemandTotal(resourceNum));

    EXPECT_EQ(47., state->dataOutRptTab->gatherDemandEndUse(resourceNum, endUseNum));
    EXPECT_EQ(47., state->dataOutRptTab->gatherDemandIndEndUse(resourceNum, endUseNum));

    EXPECT_EQ(28., state->dataOutRptTab->gatherDemandEndUseSub(subEndUseNum, endUseNum, resourceNum));
    EXPECT_EQ(28., state->dataOutRptTab->gatherDemandIndEndUseSub(subEndUseNum, endUseNum, resourceNum));

    // next "timestep" total higher

    state->dataOutputProcessor->EnergyMeters(totalMeterNum).CurTSValue =
        133.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the total meter
    state->dataOutputProcessor->EnergyMeters(endUseMeterNum).CurTSValue =
        57.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the end use meter
    state->dataOutputProcessor->EnergyMeters(subEndUseMeterNum).CurTSValue =
        38.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the sub end use meter

    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);

    EXPECT_EQ(133., state->dataOutRptTab->gatherDemandTotal(resourceNum));

    EXPECT_EQ(57., state->dataOutRptTab->gatherDemandEndUse(resourceNum, endUseNum));
    EXPECT_EQ(57., state->dataOutRptTab->gatherDemandIndEndUse(resourceNum, endUseNum));

    EXPECT_EQ(38., state->dataOutRptTab->gatherDemandEndUseSub(subEndUseNum, endUseNum, resourceNum));
    EXPECT_EQ(38., state->dataOutRptTab->gatherDemandIndEndUseSub(subEndUseNum, endUseNum, resourceNum));

    // next "timestep" total lower but end use higher and sub end use higher

    state->dataOutputProcessor->EnergyMeters(totalMeterNum).CurTSValue =
        103.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the total meter
    state->dataOutputProcessor->EnergyMeters(endUseMeterNum).CurTSValue =
        61.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the end use meter
    state->dataOutputProcessor->EnergyMeters(subEndUseMeterNum).CurTSValue =
        42.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the sub end use meter

    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);

    EXPECT_EQ(133., state->dataOutRptTab->gatherDemandTotal(resourceNum));

    EXPECT_EQ(57., state->dataOutRptTab->gatherDemandEndUse(resourceNum, endUseNum));
    EXPECT_EQ(61., state->dataOutRptTab->gatherDemandIndEndUse(resourceNum, endUseNum));

    EXPECT_EQ(38., state->dataOutRptTab->gatherDemandEndUseSub(subEndUseNum, endUseNum, resourceNum));
    EXPECT_EQ(42., state->dataOutRptTab->gatherDemandIndEndUseSub(subEndUseNum, endUseNum, resourceNum));

    // next "timestep" total higher but end use lower and sub end use lower

    state->dataOutputProcessor->EnergyMeters(totalMeterNum).CurTSValue =
        143.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the total meter
    state->dataOutputProcessor->EnergyMeters(endUseMeterNum).CurTSValue =
        59.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the end use meter
    state->dataOutputProcessor->EnergyMeters(subEndUseMeterNum).CurTSValue =
        39.0 * state->dataGlobal->TimeStepZoneSec; // create the current value for the sub end use meter

    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);

    EXPECT_EQ(143., state->dataOutRptTab->gatherDemandTotal(resourceNum));

    EXPECT_EQ(59., state->dataOutRptTab->gatherDemandEndUse(resourceNum, endUseNum));
    EXPECT_EQ(61., state->dataOutRptTab->gatherDemandIndEndUse(resourceNum, endUseNum));

    EXPECT_EQ(39., state->dataOutRptTab->gatherDemandEndUseSub(subEndUseNum, endUseNum, resourceNum));
    EXPECT_EQ(42., state->dataOutRptTab->gatherDemandIndEndUseSub(subEndUseNum, endUseNum, resourceNum));

    state->dataOutRptTab->meterNumEndUseSubBEPS.deallocate();
    state->dataOutRptTab->gatherDemandEndUseSub.deallocate();
    state->dataOutRptTab->gatherDemandIndEndUseSub.deallocate();
}

TEST_F(EnergyPlusFixture, OutputReportTabular_GatherHeatEmissionReport)
{

    state->dataOutRptTab->displayHeatEmissionsSummary = true;
    state->dataGlobal->DoWeathSim = true;
    state->dataHVACGlobal->TimeStepSys = 10.0;
    state->dataEnvrn->OutHumRat = 0.005;
    state->dataEnvrn->OutDryBulbTemp = 25.0;

    state->dataMixedAir->NumOAControllers = 2;
    state->dataMixedAir->OAController.allocate(2);
    state->dataMixedAir->OAController(1).RelTotalLossRate = 1.0;
    state->dataMixedAir->OAController(2).RelTotalLossRate = 1.0;
    state->dataCondenserLoopTowers->NumSimpleTowers = 1;
    state->dataCondenserLoopTowers->towers.allocate(1);
    state->dataCondenserLoopTowers->towers(1).Qactual = 1.0;
    state->dataCondenserLoopTowers->towers(1).FanEnergy = 50.0;

    Real64 TimeStepSysSec = state->dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    Real64 reliefEnergy = 2.0 * TimeStepSysSec;
    Real64 condenserReject = 1.0 * TimeStepSysSec + 50.0;

    GatherHeatEmissionReport(*state, OutputProcessor::TimeStepType::TimeStepSystem);

    EXPECT_EQ(reliefEnergy, state->dataHeatBal->SysTotalHVACReliefHeatLoss);
    EXPECT_EQ(reliefEnergy * DataGlobalConstants::convertJtoGJ, state->dataHeatBal->BuildingPreDefRep.emiHVACRelief);
    EXPECT_EQ(condenserReject, state->dataHeatBal->SysTotalHVACRejectHeatLoss);
    EXPECT_EQ(condenserReject * DataGlobalConstants::convertJtoGJ, state->dataHeatBal->BuildingPreDefRep.emiHVACReject);

    state->dataDXCoils->NumDXCoils = 2;
    state->dataDXCoils->DXCoil.allocate(2);
    state->dataDXCoils->DXCoil(1).DXCoilType_Num = DataHVACGlobals::CoilDX_MultiSpeedCooling;
    state->dataDXCoils->DXCoil(1).CondenserType(1) = DataHVACGlobals::AirCooled;
    state->dataDXCoils->DXCoil(1).FuelType = "NaturalGas";
    state->dataDXCoils->DXCoil(1).ElecCoolingConsumption = 100.0;
    state->dataDXCoils->DXCoil(1).TotalCoolingEnergy = 100.0;
    state->dataDXCoils->DXCoil(1).MSFuelWasteHeat = 1.0;
    state->dataDXCoils->DXCoil(1).DefrostConsumption = 0.0;
    state->dataDXCoils->DXCoil(1).CrankcaseHeaterConsumption = 0.0;
    state->dataDXCoils->DXCoil(2).DXCoilType_Num = DataHVACGlobals::CoilDX_HeatingEmpirical;
    state->dataDXCoils->DXCoil(2).ElecHeatingConsumption = 50.0;
    state->dataDXCoils->DXCoil(2).TotalHeatingEnergy = 40.0;
    state->dataDXCoils->DXCoil(2).DefrostConsumption = 0.0;
    state->dataDXCoils->DXCoil(2).FuelConsumed = 0.0;
    state->dataDXCoils->DXCoil(2).CrankcaseHeaterConsumption = 0.0;

    Real64 coilReject = 1.0 * TimeStepSysSec + 200.0 + 10.0;
    GatherHeatEmissionReport(*state, OutputProcessor::TimeStepType::TimeStepSystem);
    EXPECT_EQ(reliefEnergy, state->dataHeatBal->SysTotalHVACReliefHeatLoss);
    EXPECT_EQ(2 * reliefEnergy * DataGlobalConstants::convertJtoGJ, state->dataHeatBal->BuildingPreDefRep.emiHVACRelief);
    EXPECT_EQ(condenserReject + coilReject, state->dataHeatBal->SysTotalHVACRejectHeatLoss);
    EXPECT_EQ(2 * condenserReject * DataGlobalConstants::convertJtoGJ + coilReject * DataGlobalConstants::convertJtoGJ,
              state->dataHeatBal->BuildingPreDefRep.emiHVACReject);
}

TEST_F(EnergyPlusFixture, OutputTableTimeBins_GetInput)
{
    std::string const idf_objects = delimited_string({"Output:Table:TimeBins,",
                                                      "System1, !- Key Value",
                                                      "Some Temperature Variable, !- Variable Name",
                                                      "0.00, !- Interval Start",
                                                      "0.20, !- Interval Size",
                                                      "5,                       !- Interval Count",
                                                      "Always1; !- Schedule Name"});
    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->DoWeathSim = true;

    GetInputTabularTimeBins(*state);

    EXPECT_EQ(state->dataOutRptTab->OutputTableBinned.size(), 1u);
    EXPECT_EQ(state->dataOutRptTab->OutputTableBinned(1).keyValue, "SYSTEM1");
    EXPECT_EQ(state->dataOutRptTab->OutputTableBinned(1).varOrMeter, "SOME TEMPERATURE VARIABLE");
    EXPECT_EQ(state->dataOutRptTab->OutputTableBinned(1).intervalStart, 0.0);
    EXPECT_EQ(state->dataOutRptTab->OutputTableBinned(1).intervalSize, 0.20);
    EXPECT_EQ(state->dataOutRptTab->OutputTableBinned(1).intervalCount, 5);
    EXPECT_EQ(state->dataOutRptTab->OutputTableBinned(1).ScheduleName, "ALWAYS1");
}

// TEST_F( EnergyPlusFixture, FinAndOverhangCount )
//{
//// based on 4ZoneWithShading_Simple_2.idf
// std::string const idf_objects = delimited_string( {

//"                                                                                                   ",
//" Output:Diagnostics, DisplayExtraWarnings;",
//"  Timestep,4;                                                                                      ",
//"                                                                                                   ",
//"  Building,                                                                                        ",
//"    BUILDING #1,             !- Name                                                               ",
//"    0.0000000E+00,           !- North Axis {deg}                                                   ",
//"    Suburbs,                 !- Terrain                                                            ",
//"    3.9999999E-02,           !- Loads Convergence Tolerance Value                                  ",
//"    0.4000000,               !- Temperature Convergence Tolerance Value {deltaC}                   ",
//"    FullExterior,            !- Solar Distribution                                                 ",
//"    25,                      !- Maximum Number of Warmup Days                                      ",
//"    6;                       !- Minimum Number of Warmup Days                                      ",
//"                                                                                                   ",
//"  SurfaceConvectionAlgorithm:Inside,Simple;                                                        ",
//"                                                                                                   ",
//"  SurfaceConvectionAlgorithm:Outside,SimpleCombined;                                               ",
//"                                                                                                   ",
//"  HeatBalanceAlgorithm,ConductionTransferFunction;                                                 ",
//"                                                                                                   ",
//"  SimulationControl,                                                                               ",
//"    No,                     !- Do Zone Sizing Calculation                                         ",
//"    No,                      !- Do System Sizing Calculation                                       ",
//"    No,                      !- Do Plant Sizing Calculation                                        ",
//"    Yes,                     !- Run Simulation for Sizing Periods                                  ",
//"    No;                      !- Run Simulation for Weather File Run Periods                        ",
//"                                                                                                   ",
//"  Site:Location,                                                                                   ",
//"    Chicago Ohare Intl Ap IL USA WMO=725300,  !- Name                                              ",
//"    41.99,                   !- Latitude {deg}                                                     ",
//"    -87.91,                  !- Longitude {deg}                                                    ",
//"    -6.00,                   !- Time Zone {hr}                                                     ",
//"    205.00;                  !- Elevation {m}                                                      ",
//"                                                                                                   ",
//"  SizingPeriod:DesignDay,                                                                          ",
//"    Chicago Ohare Intl Ap Ann Htg 99% Condns DB,  !- Name                                          ",
//"    1,                       !- Month                                                              ",
//"    21,                      !- Day of Month                                                       ",
//"    WinterDesignDay,         !- Day Type                                                           ",
//"    -16.6,                   !- Maximum Dry-Bulb Temperature {C}                                   ",
//"    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}                          ",
//"    ,                        !- Dry-Bulb Temperature Range Modifier Type                           ",
//"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name              ",
//"    Wetbulb,                 !- Humidity Condition Type                                            ",
//"    -16.6,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}                        ",
//"    ,                        !- Humidity Condition Day Schedule Name                               ",
//"    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}              ",
//"    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}                                ",
//"    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}                          ",
//"    98886.,                  !- Barometric Pressure {Pa}                                           ",
//"    4.9,                     !- Wind Speed {m/s}                                                   ",
//"    270,                     !- Wind Direction {deg}                                               ",
//"    No,                      !- Rain Indicator                                                     ",
//"    No,                      !- Snow Indicator                                                     ",
//"    No,                      !- Daylight Saving Time Indicator                                     ",
//"    ASHRAEClearSky,          !- Solar Model Indicator                                              ",
//"    ,                        !- Beam Solar Day Schedule Name                                       ",
//"    ,                        !- Diffuse Solar Day Schedule Name                                    ",
//"    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensio",
//"    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimen",
//"    0.00;                    !- Sky Clearness                                                      ",
//"                                                                                                   ",
//"  SizingPeriod:DesignDay,                                                                          ",
//"    Chicago Ohare Intl Ap Ann Clg 1% Condns DB=>MWB,  !- Name                                      ",
//"    7,                       !- Month                                                              ",
//"    21,                      !- Day of Month                                                       ",
//"    SummerDesignDay,         !- Day Type                                                           ",
//"    31.6,                    !- Maximum Dry-Bulb Temperature {C}                                   ",
//"    10.5,                    !- Daily Dry-Bulb Temperature Range {deltaC}                          ",
//"    ,                        !- Dry-Bulb Temperature Range Modifier Type                           ",
//"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name              ",
//"    Wetbulb,                 !- Humidity Condition Type                                            ",
//"    23,                      !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}                        ",
//"    ,                        !- Humidity Condition Day Schedule Name                               ",
//"    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}              ",
//"    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}                                ",
//"    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}                          ",
//"    98886.,                  !- Barometric Pressure {Pa}                                           ",
//"    5.2,                     !- Wind Speed {m/s}                                                   ",
//"    230,                     !- Wind Direction {deg}                                               ",
//"    No,                      !- Rain Indicator                                                     ",
//"    No,                      !- Snow Indicator                                                     ",
//"    No,                      !- Daylight Saving Time Indicator                                     ",
//"    ASHRAEClearSky,          !- Solar Model Indicator                                              ",
//"    ,                        !- Beam Solar Day Schedule Name                                       ",
//"    ,                        !- Diffuse Solar Day Schedule Name                                    ",
//"    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensio",
//"    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimen",
//"    1.00;                    !- Sky Clearness                                                      ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    A2 - 4 IN DENSE FACE BRICK,  !- Name                                                           ",
//"    Rough,                   !- Roughness                                                          ",
//"    0.1014984,               !- Thickness {m}                                                      ",
//"    1.245296,                !- Conductivity {W/m-K}                                               ",
//"    2082.400,                !- Density {kg/m3}                                                    ",
//"    920.4800,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.9300000,               !- Solar Absorptance                                                  ",
//"    0.9300000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    B3 - 2 IN INSULATION,    !- Name                                                               ",
//"    VeryRough,               !- Roughness                                                          ",
//"    5.0901599E-02,           !- Thickness {m}                                                      ",
//"    4.3239430E-02,           !- Conductivity {W/m-K}                                               ",
//"    32.03693,                !- Density {kg/m3}                                                    ",
//"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.5000000,               !- Solar Absorptance                                                  ",
//"    0.5000000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    C2 - 4 IN LW CONCRETE BLOCK,  !- Name                                                          ",
//"    MediumRough,             !- Roughness                                                          ",
//"    0.1014984,               !- Thickness {m}                                                      ",
//"    0.3805070,               !- Conductivity {W/m-K}                                               ",
//"    608.7016,                !- Density {kg/m3}                                                    ",
//"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.6500000,               !- Solar Absorptance                                                  ",
//"    0.6500000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name                                                   ",
//"    Smooth,                  !- Roughness                                                          ",
//"    1.9050000E-02,           !- Thickness {m}                                                      ",
//"    0.7264224,               !- Conductivity {W/m-K}                                               ",
//"    1601.846,                !- Density {kg/m3}                                                    ",
//"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.9200000,               !- Solar Absorptance                                                  ",
//"    0.9200000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"   ! (R=.0472,TRANS=.80,VERY SMOOTH,GLASS), from 6mm clear                                         ",
//"                                                                                                   ",
//"  WindowMaterial:Glazing,                                                                          ",
//"    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name                                                         ",
//"    SpectralAverage,         !- Optical Data Type                                                  ",
//"    ,                        !- Window Glass Spectral Data Set Name                                ",
//"    6.0000001E-03,           !- Thickness {m}                                                      ",
//"    0.7750000,               !- Solar Transmittance at Normal Incidence                            ",
//"    7.1000002E-02,           !- Front Side Solar Reflectance at Normal Incidence                   ",
//"    7.1000002E-02,           !- Back Side Solar Reflectance at Normal Incidence                    ",
//"    0.8810000,               !- Visible Transmittance at Normal Incidence                          ",
//"    7.9999998E-02,           !- Front Side Visible Reflectance at Normal Incidence                 ",
//"    7.9999998E-02,           !- Back Side Visible Reflectance at Normal Incidence                  ",
//"    0.0000000E+00,           !- Infrared Transmittance at Normal Incidence                         ",
//"    0.8400000,               !- Front Side Infrared Hemispherical Emissivity                       ",
//"    0.8400000,               !- Back Side Infrared Hemispherical Emissivity                        ",
//"    0.9000000;               !- Conductivity {W/m-K}                                               ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    WOOD - HARDWOOD 1 / 8 IN,!- Name                                                               ",
//"    MediumSmooth,            !- Roughness                                                          ",
//"    3.1699201E-03,           !- Thickness {m}                                                      ",
//"    0.1591211,               !- Conductivity {W/m-K}                                               ",
//"    720.8308,                !- Density {kg/m3}                                                    ",
//"    1255.200,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.7800000,               !- Solar Absorptance                                                  ",
//"    0.7800000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material:AirGap,                                                                                 ",
//"    B1 - AIRSPACE RESISTANCE,!- Name                                                               ",
//"    0.1603675;               !- Thermal Resistance {m2-K/W}                                        ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    C7 - 8 IN LW CONCRETE BLOCK,  !- Name                                                          ",
//"    Rough,                   !- Roughness                                                          ",
//"    0.2033016,               !- Thickness {m}                                                      ",
//"    0.5707605,               !- Conductivity {W/m-K}                                               ",
//"    608.7016,                !- Density {kg/m3}                                                    ",
//"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.6500000,               !- Solar Absorptance                                                  ",
//"    0.6500000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    E5 - ACOUSTIC TILE,      !- Name                                                               ",
//"    MediumSmooth,            !- Roughness                                                          ",
//"    1.9050000E-02,           !- Thickness {m}                                                      ",
//"    6.0535200E-02,           !- Conductivity {W/m-K}                                               ",
//"    480.5539,                !- Density {kg/m3}                                                    ",
//"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.3200000,               !- Solar Absorptance                                                  ",
//"    0.3200000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material:AirGap,                                                                                 ",
//"    E4 - CEILING AIRSPACE,   !- Name                                                               ",
//"    0.1762280;               !- Thermal Resistance {m2-K/W}                                        ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    C5 - 4 IN HW CONCRETE,   !- Name                                                               ",
//"    MediumRough,             !- Roughness                                                          ",
//"    0.1014984,               !- Thickness {m}                                                      ",
//"    1.729577,                !- Conductivity {W/m-K}                                               ",
//"    2242.585,                !- Density {kg/m3}                                                    ",
//"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.6500000,               !- Solar Absorptance                                                  ",
//"    0.6500000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    C10 - 8 IN HW CONCRETE,  !- Name                                                               ",
//"    MediumRough,             !- Roughness                                                          ",
//"    0.2033016,               !- Thickness {m}                                                      ",
//"    1.729577,                !- Conductivity {W/m-K}                                               ",
//"    2242.585,                !- Density {kg/m3}                                                    ",
//"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.6500000,               !- Solar Absorptance                                                  ",
//"    0.6500000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    E2 - 1 / 2 IN SLAG OR STONE,  !- Name                                                          ",
//"    Rough,                   !- Roughness                                                          ",
//"    1.2710161E-02,           !- Thickness {m}                                                      ",
//"    1.435549,                !- Conductivity {W/m-K}                                               ",
//"    881.0155,                !- Density {kg/m3}                                                    ",
//"    1673.600,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.5500000,               !- Solar Absorptance                                                  ",
//"    0.5500000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name                                                      ",
//"    Rough,                   !- Roughness                                                          ",
//"    9.5402403E-03,           !- Thickness {m}                                                      ",
//"    0.1902535,               !- Conductivity {W/m-K}                                               ",
//"    1121.292,                !- Density {kg/m3}                                                    ",
//"    1673.600,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.7500000,               !- Solar Absorptance                                                  ",
//"    0.7500000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    B6 - 2 IN DENSE INSULATION,  !- Name                                                           ",
//"    VeryRough,               !- Roughness                                                          ",
//"    5.0901599E-02,           !- Thickness {m}                                                      ",
//"    4.3239430E-02,           !- Conductivity {W/m-K}                                               ",
//"    91.30524,                !- Density {kg/m3}                                                    ",
//"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.5000000,               !- Solar Absorptance                                                  ",
//"    0.5000000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Material,                                                                                        ",
//"    C12 - 2 IN HW CONCRETE,  !- Name                                                               ",
//"    MediumRough,             !- Roughness                                                          ",
//"    5.0901599E-02,           !- Thickness {m}                                                      ",
//"    1.729577,                !- Conductivity {W/m-K}                                               ",
//"    2242.585,                !- Density {kg/m3}                                                    ",
//"    836.8000,                !- Specific Heat {J/kg-K}                                             ",
//"    0.9000000,               !- Thermal Absorptance                                                ",
//"    0.6500000,               !- Solar Absorptance                                                  ",
//"    0.6500000;               !- Visible Absorptance                                                ",
//"                                                                                                   ",
//"  Construction,                                                                                    ",
//"    EXTERIOR,                !- Name                                                               ",
//"    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer                                                  ",
//"    B3 - 2 IN INSULATION,    !- Layer 2                                                            ",
//"    C2 - 4 IN LW CONCRETE BLOCK,  !- Layer 3                                                       ",
//"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4                                                ",
//"                                                                                                   ",
//"  Construction,                                                                                    ",
//"    SINGLE PANE HW WINDOW,   !- Name                                                               ",
//"    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer                                                ",
//"                                                                                                   ",
//"  Construction,                                                                                    ",
//"    HOLLOW WOOD DOOR,        !- Name                                                               ",
//"    WOOD - HARDWOOD 1 / 8 IN,!- Outside Layer                                                      ",
//"    B1 - AIRSPACE RESISTANCE,!- Layer 2                                                            ",
//"    WOOD - HARDWOOD 1 / 8 IN;!- Layer 3                                                            ",
//"                                                                                                   ",
//"  Construction,                                                                                    ",
//"    INTERIOR,                !- Name                                                               ",
//"    C7 - 8 IN LW CONCRETE BLOCK;  !- Outside Layer                                                 ",
//"                                                                                                   ",
//"  Construction,                                                                                    ",
//"    FLOOR,                   !- Name                                                               ",
//"    E5 - ACOUSTIC TILE,      !- Outside Layer                                                      ",
//"    E4 - CEILING AIRSPACE,   !- Layer 2                                                            ",
//"    C5 - 4 IN HW CONCRETE;   !- Layer 3                                                            ",
//"                                                                                                   ",
//"  Construction,                                                                                    ",
//"    CEILING34,               !- Name                                                               ",
//"    C10 - 8 IN HW CONCRETE;  !- Outside Layer                                                      ",
//"                                                                                                   ",
//"  Construction,                                                                                    ",
//"    FLOOR34,                 !- Name                                                               ",
//"    C10 - 8 IN HW CONCRETE;  !- Outside Layer                                                      ",
//"                                                                                                   ",
//"  Construction,                                                                                    ",
//"    ROOF31,                  !- Name                                                               ",
//"    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer                                                 ",
//"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2                                                   ",
//"    B6 - 2 IN DENSE INSULATION,  !- Layer 3                                                        ",
//"    C12 - 2 IN HW CONCRETE;  !- Layer 4                                                            ",
//"                                                                                                   ",
//"  ScheduleTypeLimits,                                                                              ",
//"    Any Number;              !- Name                                                               ",
//"                                                                                                   ",
//"  ScheduleTypeLimits,                                                                              ",
//"    Fraction,                !- Name                                                               ",
//"    0.0,                     !- Lower Limit Value                                                  ",
//"    1.0,                     !- Upper Limit Value                                                  ",
//"    CONTINUOUS;              !- Numeric Type                                                       ",
//"                                                                                                   ",
//"  Site:GroundTemperature:BuildingSurface,20,20,20,20,20,20,20,20,20,20,20,20;                      ",
//"                                                                                                   ",
//"  Zone,                                                                                            ",
//"    ZONE 1,                  !- Name                                                               ",
//"    0,                       !- Direction of Relative North {deg}                                  ",
//"    0,                       !- X Origin {m}                                                       ",
//"    0,                       !- Y Origin {m}                                                       ",
//"    -10,                     !- Z Origin {m}                                                       ",
//"    1,                       !- Type                                                               ",
//"    1,                       !- Multiplier                                                         ",
//"    AutoCalculate,           !- Ceiling Height {m}                                                 ",
//"    AutoCalculate;           !- Volume {m3}                                                        ",
//"                                                                                                   ",
//"  Zone,                                                                                            ",
//"    ZONE 2,                  !- Name                                                               ",
//"    0,                       !- Direction of Relative North {deg}                                  ",
//"    20,                      !- X Origin {m}                                                       ",
//"    0,                       !- Y Origin {m}                                                       ",
//"    -10,                     !- Z Origin {m}                                                       ",
//"    1,                       !- Type                                                               ",
//"    1,                       !- Multiplier                                                         ",
//"    AutoCalculate,           !- Ceiling Height {m}                                                 ",
//"    AutoCalculate;           !- Volume {m3}                                                        ",
//"                                                                                                   ",
//"  Zone,                                                                                            ",
//"    ZONE 3,                  !- Name                                                               ",
//"    0,                       !- Direction of Relative North {deg}                                  ",
//"    0,                       !- X Origin {m}                                                       ",
//"    0,                       !- Y Origin {m}                                                       ",
//"    0,                       !- Z Origin {m}                                                       ",
//"    1,                       !- Type                                                               ",
//"    1,                       !- Multiplier                                                         ",
//"    AutoCalculate,           !- Ceiling Height {m}                                                 ",
//"    AutoCalculate;           !- Volume {m3}                                                        ",
//"                                                                                                   ",
//"  Zone,                                                                                            ",
//"    ZONE 4,                  !- Name                                                               ",
//"    0,                       !- Direction of Relative North {deg}                                  ",
//"    20,                      !- X Origin {m}                                                       ",
//"    0,                       !- Y Origin {m}                                                       ",
//"    0,                       !- Z Origin {m}                                                       ",
//"    1,                       !- Type                                                               ",
//"    1,                       !- Multiplier                                                         ",
//"    AutoCalculate,           !- Ceiling Height {m}                                                 ",
//"    AutoCalculate;           !- Volume {m3}                                                        ",
//"                                                                                                   ",
//"  GlobalGeometryRules,                                                                             ",
//"    UpperLeftCorner,         !- Starting Vertex Position                                           ",
//"    CounterClockWise,        !- Vertex Entry Direction                                             ",
//"    Relative,                !- Coordinate System                                                  ",
//"    ,                        !- Daylighting Reference Point Coordinate System                      ",
//"    Relative;                !- Rectangular Surface Coordinate System                              ",
//"                                                                                                   ",
//"  Shading:Building,                                                                                ",
//"    Bushes-East,             !- Name                                                               ",
//"    90,                      !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    45,                      !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    50,                      !- Length {m}                                                         ",
//"    1;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Shading:Site,                                                                                    ",
//"    Bushes-North,            !- Name                                                               ",
//"    0,                       !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    45,                      !- Starting X Coordinate {m}                                          ",
//"    50,                      !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    50,                      !- Length {m}                                                         ",
//"    1;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Shading:Site,                                                                                    ",
//"    Bushes-West,             !- Name                                                               ",
//"    270,                     !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    -5,                      !- Starting X Coordinate {m}                                          ",
//"    50,                      !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    50,                      !- Length {m}                                                         ",
//"    1;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Exterior,                                                                                   ",
//"    Zn001:Wall001,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 1,                  !- Zone Name                                                          ",
//"    180,                     !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Window,                                                                                          ",
//"    Zn001:Wall001:Win001,    !- Name                                                               ",
//"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
//"    Zn001:Wall001,           !- Building Surface Name                                              ",
//"    ,                        !- Frame and Divider Name                                             ",
//"    1,                       !- Multiplier                                                         ",
//"    4,                       !- Starting X Coordinate {m}                                          ",
//"    3,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  GlazedDoor,                                                                                      ",
//"    Zn001:Wall001:Door001,   !- Name                                                               ",
//"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
//"    Zn001:Wall001,           !- Building Surface Name                                              ",
//"    ,                        !- Frame and Divider Name                                             ",
//"    1,                       !- Multiplier                                                         ",
//"    14,                      !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Shading:Overhang:Projection,                                                                     ",
//"    Zn001:Wall001:Win001:Shade001,  !- Name                                                        ",
//"    Zn001:Wall001:Win001,    !- Window or Door Name                                                ",
//"    .7,                      !- Height above Window or Door {m}                                    ",
//"    90,                      !- Tilt Angle from Window/Door {deg}                                  ",
//"    .2,                      !- Left extension from Window/Door Width {m}                          ",
//"    .2,                      !- Right extension from Window/Door Width {m}                         ",
//"    .6;                      !- Depth as Fraction of Window/Door Height {dimensionless}            ",
//"                                                                                                   ",
//"  Shading:Overhang,                                                                                ",
//"    Zn001:Wall001:Door001:Shade001,  !- Name                                                       ",
//"    Zn001:Wall001:Door001,   !- Window or Door Name                                                ",
//"    .6,                      !- Height above Window or Door {m}                                    ",
//"    90,                      !- Tilt Angle from Window/Door {deg}                                  ",
//"    0,                       !- Left extension from Window/Door Width {m}                          ",
//"    0,                       !- Right extension from Window/Door Width {m}                         ",
//"    3;                       !- Depth {m}                                                          ",
//"                                                                                                   ",
//"  Shading:Fin:Projection,                                                                          ",
//"    Zn001:Wall001:Shade003,  !- Name                                                               ",
//"    Zn001:Wall001:Win001,    !- Window or Door Name                                                ",
//"    .1,                      !- Left Extension from Window/Door {m}                                ",
//"    .1,                      !- Left Distance Above Top of Window {m}                              ",
//"    .1,                      !- Left Distance Below Bottom of Window {m}                           ",
//"    90,                      !- Left Tilt Angle from Window/Door {deg}                             ",
//"    .6,                      !- Left Depth as Fraction of Window/Door Width {dimensionless}        ",
//"    .1,                      !- Right Extension from Window/Door {m}                               ",
//"    .1,                      !- Right Distance Above Top of Window {m}                             ",
//"    .1,                      !- Right Distance Below Bottom of Window {m}                          ",
//"    90,                      !- Right Tilt Angle from Window/Door {deg}                            ",
//"    .6;                      !- Right Depth as Fraction of Window/Door Width {dimensionless}       ",
//"                                                                                                   ",
//"  Wall:Underground,                                                                                ",
//"    Zn001:Wall002,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 1,                  !- Zone Name                                                          ",
//"    0,                       !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    20,                      !- Starting X Coordinate {m}                                          ",
//"    20,                      !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Underground,                                                                                ",
//"    Zn001:Wall003,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 1,                  !- Zone Name                                                          ",
//"    270,                     !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    20,                      !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Door,                                                                                            ",
//"    Zn001:Wall001:Door002,   !- Name                                                               ",
//"    HOLLOW WOOD DOOR,        !- Construction Name                                                  ",
//"    Zn001:Wall003,           !- Building Surface Name                                              ",
//"    1,                       !- Multiplier                                                         ",
//"    14,                      !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Adiabatic,                                                                                  ",
//"    Zn001:Wall004,           !- Name                                                               ",
//"    INTERIOR,                !- Construction Name                                                  ",
//"    ZONE 1,                  !- Zone Name                                                          ",
//"    90,                      !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    20,                      !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Floor:GroundContact,                                                                             ",
//"    Zn001:Flr001,            !- Name                                                               ",
//"    FLOOR,                   !- Construction Name                                                  ",
//"    ZONE 1,                  !- Zone Name                                                          ",
//"    90,                      !- Azimuth Angle {deg}                                                ",
//"    180,                     !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    20;                      !- Width {m}                                                          ",
//"                                                                                                   ",
//"  Ceiling:Adiabatic,                                                                               ",
//"    Zn001:Roof001,           !- Name                                                               ",
//"    CEILING34,               !- Construction Name                                                  ",
//"    ZONE 1,                  !- Zone Name                                                          ",
//"    180,                     !- Azimuth Angle {deg}                                                ",
//"    0,                       !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    10,                      !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    20;                      !- Width {m}                                                          ",
//"                                                                                                   ",
//"  Wall:Exterior,                                                                                   ",
//"    Zn002:Wall001,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 2,                  !- Zone Name                                                          ",
//"    180,                     !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Window,                                                                                          ",
//"    Zn002:Wall001:Win001,    !- Name                                                               ",
//"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
//"    Zn002:Wall001,           !- Building Surface Name                                              ",
//"    ,                        !- Frame and Divider Name                                             ",
//"    1,                       !- Multiplier                                                         ",
//"    4,                       !- Starting X Coordinate {m}                                          ",
//"    3,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Shading:Overhang,                                                                                ",
//"    Zn002:Wall001:Win001:Shade001,  !- Name                                                        ",
//"    Zn002:Wall001:Win001,    !- Window or Door Name                                                ",
//"    .7,                      !- Height above Window or Door {m}                                    ",
//"    90,                      !- Tilt Angle from Window/Door {deg}                                  ",
//"    0,                       !- Left extension from Window/Door Width {m}                          ",
//"    0,                       !- Right extension from Window/Door Width {m}                         ",
//"    3;                       !- Depth {m}                                                          ",
//"                                                                                                   ",
//"  Window,                                                                                          ",
//"    Zn002:Wall001:Win002,    !- Name                                                               ",
//"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
//"    Zn002:Wall001,           !- Building Surface Name                                              ",
//"    ,                        !- Frame and Divider Name                                             ",
//"    1,                       !- Multiplier                                                         ",
//"    10,                      !- Starting X Coordinate {m}                                          ",
//"    3,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Underground,                                                                                ",
//"    Zn002:Wall002,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 2,                  !- Zone Name                                                          ",
//"    90,                      !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    20,                      !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Underground,                                                                                ",
//"    Zn002:Wall003,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 2,                  !- Zone Name                                                          ",
//"    0,                       !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    20,                      !- Starting X Coordinate {m}                                          ",
//"    20,                      !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Adiabatic,                                                                                  ",
//"    Zn002:Wall004,           !- Name                                                               ",
//"    INTERIOR,                !- Construction Name                                                  ",
//"    ZONE 2,                  !- Zone Name                                                          ",
//"    270,                     !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    20,                      !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Floor:GroundContact,                                                                             ",
//"    Zn002:Flr001,            !- Name                                                               ",
//"    FLOOR,                   !- Construction Name                                                  ",
//"    ZONE 2,                  !- Zone Name                                                          ",
//"    90,                      !- Azimuth Angle {deg}                                                ",
//"    180,                     !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    20;                      !- Width {m}                                                          ",
//"                                                                                                   ",
//"  Ceiling:Adiabatic,                                                                               ",
//"    Zn002:Roof001,           !- Name                                                               ",
//"    CEILING34,               !- Construction Name                                                  ",
//"    ZONE 2,                  !- Zone Name                                                          ",
//"    180,                     !- Azimuth Angle {deg}                                                ",
//"    0,                       !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    10,                      !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    20;                      !- Width {m}                                                          ",
//"                                                                                                   ",
//"  Wall:Exterior,                                                                                   ",
//"    Zn003:Wall001,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 3,                  !- Zone Name                                                          ",
//"    180,                     !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Window,                                                                                          ",
//"    Zn003:Wall001:Win001,    !- Name                                                               ",
//"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
//"    Zn003:Wall001,           !- Building Surface Name                                              ",
//"    ,                        !- Frame and Divider Name                                             ",
//"    1,                       !- Multiplier                                                         ",
//"    8,                       !- Starting X Coordinate {m}                                          ",
//"    3,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Exterior,                                                                                   ",
//"    Zn003:Wall002,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 3,                  !- Zone Name                                                          ",
//"    0,                       !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    20,                      !- Starting X Coordinate {m}                                          ",
//"    20,                      !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Exterior,                                                                                   ",
//"    Zn003:Wall003,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 3,                  !- Zone Name                                                          ",
//"    270,                     !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    20,                      !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Interzone,                                                                                  ",
//"    Zn003:Wall004,           !- Name                                                               ",
//"    INTERIOR,                !- Construction Name                                                  ",
//"    ZONE 3,                  !- Zone Name                                                          ",
//"    ZONE 4,                  !- Outside Boundary Condition Object                                  ",
//"    90,                      !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    20,                      !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  GlazedDoor:Interzone,                                                                            ",
//"    Zn003:Wall004:Door002,   !- Name                                                               ",
//"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
//"    Zn003:Wall004,           !- Building Surface Name                                              ",
//"    ZONE 4,                  !- Outside Boundary Condition Object                                  ",
//"    1,                       !- Multiplier                                                         ",
//"    10,                      !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Window:Interzone,                                                                                ",
//"    Zn003:Wall004:Win001,    !- Name                                                               ",
//"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
//"    Zn003:Wall004,           !- Building Surface Name                                              ",
//"    ZONE 4,                  !- Outside Boundary Condition Object                                  ",
//"    1,                       !- Multiplier                                                         ",
//"    4,                       !- Starting X Coordinate {m}                                          ",
//"    3,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Door:Interzone,                                                                                  ",
//"    Zn003:Wall004:Door001,   !- Name                                                               ",
//"    HOLLOW WOOD DOOR,        !- Construction Name                                                  ",
//"    Zn003:Wall004,           !- Building Surface Name                                              ",
//"    ZONE 4,                  !- Outside Boundary Condition Object                                  ",
//"    1,                       !- Multiplier                                                         ",
//"    10,                      !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Floor:Interzone,                                                                                 ",
//"    Zn003:Flr001,            !- Name                                                               ",
//"    FLOOR34,                 !- Construction Name                                                  ",
//"    ZONE 3,                  !- Zone Name                                                          ",
//"    Zn001:Roof001,           !- Outside Boundary Condition Object                                  ",
//"    90,                      !- Azimuth Angle {deg}                                                ",
//"    180,                     !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    20;                      !- Width {m}                                                          ",
//"                                                                                                   ",
//"  Roof,                                                                                            ",
//"    Zn003:Roof001,           !- Name                                                               ",
//"    ROOF31,                  !- Construction Name                                                  ",
//"    ZONE 3,                  !- Zone Name                                                          ",
//"    180,                     !- Azimuth Angle {deg}                                                ",
//"    0,                       !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    10,                      !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    20;                      !- Width {m}                                                          ",
//"                                                                                                   ",
//"  Wall:Exterior,                                                                                   ",
//"    Zn004:Wall001,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 4,                  !- Zone Name                                                          ",
//"    180,                     !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Window,                                                                                          ",
//"    Zn004:Wall001:Win001,    !- Name                                                               ",
//"    SINGLE PANE HW WINDOW,   !- Construction Name                                                  ",
//"    Zn004:Wall001,           !- Building Surface Name                                              ",
//"    ,                        !- Frame and Divider Name                                             ",
//"    1,                       !- Multiplier                                                         ",
//"    8,                       !- Starting X Coordinate {m}                                          ",
//"    3,                       !- Starting Z Coordinate {m}                                          ",
//"    3,                       !- Length {m}                                                         ",
//"    5;                       !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Exterior,                                                                                   ",
//"    Zn004:Wall002,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 4,                  !- Zone Name                                                          ",
//"    90,                      !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    20,                      !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Wall:Exterior,                                                                                   ",
//"    Zn004:Wall003,           !- Name                                                               ",
//"    EXTERIOR,                !- Construction Name                                                  ",
//"    ZONE 4,                  !- Zone Name                                                          ",
//"    0,                       !- Azimuth Angle {deg}                                                ",
//"    90,                      !- Tilt Angle {deg}                                                   ",
//"    20,                      !- Starting X Coordinate {m}                                          ",
//"    20,                      !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    10;                      !- Height {m}                                                         ",
//"                                                                                                   ",
//"  Floor:Interzone,                                                                                 ",
//"    Zn004:Flr001,            !- Name                                                               ",
//"    FLOOR34,                 !- Construction Name                                                  ",
//"    ZONE 4,                  !- Zone Name                                                          ",
//"    Zn002:Roof001,           !- Outside Boundary Condition Object                                  ",
//"    90,                      !- Azimuth Angle {deg}                                                ",
//"    180,                     !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    0,                       !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    20;                      !- Width {m}                                                          ",
//"                                                                                                   ",
//"  Roof,                                                                                            ",
//"    Zn004:Roof001,           !- Name                                                               ",
//"    ROOF31,                  !- Construction Name                                                  ",
//"    ZONE 4,                  !- Zone Name                                                          ",
//"    180,                     !- Azimuth Angle {deg}                                                ",
//"    0,                       !- Tilt Angle {deg}                                                   ",
//"    0,                       !- Starting X Coordinate {m}                                          ",
//"    0,                       !- Starting Y Coordinate {m}                                          ",
//"    10,                      !- Starting Z Coordinate {m}                                          ",
//"    20,                      !- Length {m}                                                         ",
//"    20;                      !- Width {m}                                                          ",
//"                                                                                                   ",
//"  ScheduleTypeLimits,                                                                              ",
//"    Temperature,             !- Name                                                               ",
//"    -100,                    !- Lower Limit Value                                                  ",
//"    200,                     !- Upper Limit Value                                                  ",
//"    CONTINUOUS;              !- Numeric Type                                                       ",
//"                                                                                                   ",
//"  ScheduleTypeLimits,                                                                              ",
//"    ControlType,             !- Name                                                               ",
//"    0,                       !- Lower Limit Value                                                  ",
//"    4,                       !- Upper Limit Value                                                  ",
//"    DISCRETE;                !- Numeric Type                                                       ",
//"                                                                                                   ",
//"  SCHEDULE:COMPACT,                                                                                ",
//"    ZONE CONTROL TYPE SCHEDULE,  !- Name                                                           ",
//"    CONTROLTYPE,             !- Schedule Type Limits Name                                          ",
//"    Through: 12/31,          !- Field 1                                                            ",
//"    For: AllDays,            !- Field 2                                                            ",
//"    Until: 24:00,4;          !- Field 3                                                            ",
//"                                                                                                   ",
//"  SCHEDULE:COMPACT,                                                                                ",
//"    ZONE HEATING SETPOINTS,  !- Name                                                               ",
//"    TEMPERATURE,             !- Schedule Type Limits Name                                          ",
//"    Through: 12/31,          !- Field 1                                                            ",
//"    For: Weekdays WinterDesignDay, !- Field 2                                                      ",
//"    Until: 7:00,15.60,       !- Field 3                                                            ",
//"    Until: 17:00,19.40,      !- Field 5                                                            ",
//"    Until: 24:00,15.60,      !- Field 7                                                            ",
//"    For: AllOtherDays,       !- Field 9                                                            ",
//"    Until: 24:00,15.60;      !- Field 10                                                           ",
//"                                                                                                   ",
//"  SCHEDULE:COMPACT,                                                                                ",
//"    ZONE COOLING SETPOINTS,  !- Name                                                               ",
//"    TEMPERATURE,             !- Schedule Type Limits Name                                          ",
//"    Through: 12/31,          !- Field 1                                                            ",
//"    For: Weekdays SummerDesignDay, !- Field 2                                                      ",
//"    Until: 7:00,100.00,      !- Field 3                                                            ",
//"    Until: 17:00,26.10,      !- Field 5                                                            ",
//"    Until: 24:00,100.00,     !- Field 7                                                            ",
//"    For: AllOtherDays,       !- Field 9                                                            ",
//"    Until: 24:00,100.00;     !- Field 10                                                           ",
//"                                                                                                   ",
//"  ZoneControl:Thermostat,                                                                          ",
//"    ZONE 1 CONTROLS,         !- Name                                                               ",
//"    ZONE 1,                  !- Zone or ZoneList Name                                              ",
//"    Zone Control Type Schedule,  !- Control Type Schedule Name                                     ",
//"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type                                     ",
//"    ZONE 1 SETPOINTS;        !- Control 1 Name                                                     ",
//"                                                                                                   ",
//"  ThermostatSetpoint:DualSetpoint,                                                                 ",
//"    ZONE 1 SETPOINTS,        !- Name                                                               ",
//"    ZONE Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name                         ",
//"    ZONE Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name                         ",
//"                                                                                                   ",
//"  ZoneHVAC:EquipmentConnections,                                                                   ",
//"    ZONE 1,                  !- Zone Name                                                          ",
//"    ZONE 1 EQUIPMENT,        !- Zone Conditioning Equipment List Name                              ",
//"    ZONE 1 INLETS,           !- Zone Air Inlet Node or NodeList Name                               ",
//"    ,                        !- Zone Air Exhaust Node or NodeList Name                             ",
//"    ZONE 1 NODE,             !- Zone Air Node Name                                                 ",
//"    ZONE 1 OUTLET;           !- Zone Return Air Node Name                                          ",
//"                                                                                                   ",
//"  ZoneHVAC:EquipmentList,                                                                          ",
//"    ZONE 1 EQUIPMENT,        !- Name                                                               ",
//"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                                 ",
//"    ZONE 1 Ideal Loads,      !- Zone Equipment 1 Name                                              ",
//"    1,                       !- Zone Equipment 1 Cooling Sequence                                  ",
//"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence                       ",
//"                                                                                                   ",
//"  ZoneHVAC:IdealLoadsAirSystem,                                                                    ",
//"    ZONE 1 Ideal Loads,      !- Name                                                               ",
//"    ,                        !- Availability Schedule Name                                         ",
//"    ZONE 1 INLETS,           !- Zone Supply Air Node Name                                          ",
//"    ,                        !- Zone Exhaust Air Node Name                                         ",
//"    ,                        !- System Inlet Air Node Name                                         ",
//"    50,                      !- Maximum Heating Supply Air Temperature {C}                         ",
//"    13,                      !- Minimum Cooling Supply Air Temperature {C}                         ",
//"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
//"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
//"    NoLimit,                 !- Heating Limit                                                      ",
//"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}                               ",
//"    ,                        !- Maximum Sensible Heating Capacity {W}                              ",
//"    NoLimit,                 !- Cooling Limit                                                      ",
//"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}                               ",
//"    ,                        !- Maximum Total Cooling Capacity {W}                                 ",
//"    ,                        !- Heating Availability Schedule Name                                 ",
//"    ,                        !- Cooling Availability Schedule Name                                 ",
//"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                                 ",
//"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}                        ",
//"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                                   ",
//"    ,                        !- Design Specification Outdoor Air Object Name                       ",
//"    ,                        !- Outdoor Air Inlet Node Name                                        ",
//"    ,                        !- Demand Controlled Ventilation Type                                 ",
//"    ,                        !- Outdoor Air Economizer Type                                        ",
//"    ,                        !- Heat Recovery Type                                                 ",
//"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}               ",
//"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}                 ",
//"                                                                                                   ",
//"  NodeList,                                                                                        ",
//"    ZONE 1 INLETS,           !- Name                                                               ",
//"    ZONE 1 INLET;            !- Node 1 Name                                                        ",
//"                                                                                                   ",
//"  ZoneControl:Thermostat,                                                                          ",
//"    ZONE 2 CONTROLS,         !- Name                                                               ",
//"    ZONE 2,                  !- Zone or ZoneList Name                                              ",
//"    Zone Control Type Schedule,  !- Control Type Schedule Name                                     ",
//"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type                                     ",
//"    ZONE 2 SETPOINTS;        !- Control 1 Name                                                     ",
//"                                                                                                   ",
//"  ThermostatSetpoint:DualSetpoint,                                                                 ",
//"    ZONE 2 SETPOINTS,        !- Name                                                               ",
//"    ZONE Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name                         ",
//"    ZONE Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name                         ",
//"                                                                                                   ",
//"  ZoneHVAC:EquipmentConnections,                                                                   ",
//"    ZONE 2,                  !- Zone Name                                                          ",
//"    ZONE 2 EQUIPMENT,        !- Zone Conditioning Equipment List Name                              ",
//"    ZONE 2 INLETS,           !- Zone Air Inlet Node or NodeList Name                               ",
//"    ,                        !- Zone Air Exhaust Node or NodeList Name                             ",
//"    ZONE 2 NODE,             !- Zone Air Node Name                                                 ",
//"    ZONE 2 OUTLET;           !- Zone Return Air Node Name                                          ",
//"                                                                                                   ",
//"  ZoneHVAC:EquipmentList,                                                                          ",
//"    ZONE 2 EQUIPMENT,        !- Name                                                               ",
//"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                                 ",
//"    ZONE 2 Ideal Loads,      !- Zone Equipment 1 Name                                              ",
//"    1,                       !- Zone Equipment 1 Cooling Sequence                                  ",
//"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence                       ",
//"                                                                                                   ",
//"  ZoneHVAC:IdealLoadsAirSystem,                                                                    ",
//"    ZONE 2 Ideal Loads,      !- Name                                                               ",
//"    ,                        !- Availability Schedule Name                                         ",
//"    ZONE 2 INLETS,           !- Zone Supply Air Node Name                                          ",
//"    ,                        !- Zone Exhaust Air Node Name                                         ",
//"    ,                        !- System Inlet Air Node Name                                         ",
//"    50,                      !- Maximum Heating Supply Air Temperature {C}                         ",
//"    13,                      !- Minimum Cooling Supply Air Temperature {C}                         ",
//"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
//"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
//"    NoLimit,                 !- Heating Limit                                                      ",
//"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}                               ",
//"    ,                        !- Maximum Sensible Heating Capacity {W}                              ",
//"    NoLimit,                 !- Cooling Limit                                                      ",
//"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}                               ",
//"    ,                        !- Maximum Total Cooling Capacity {W}                                 ",
//"    ,                        !- Heating Availability Schedule Name                                 ",
//"    ,                        !- Cooling Availability Schedule Name                                 ",
//"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                                 ",
//"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}                        ",
//"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                                   ",
//"    ,                        !- Design Specification Outdoor Air Object Name                       ",
//"    ,                        !- Outdoor Air Inlet Node Name                                        ",
//"    ,                        !- Demand Controlled Ventilation Type                                 ",
//"    ,                        !- Outdoor Air Economizer Type                                        ",
//"    ,                        !- Heat Recovery Type                                                 ",
//"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}               ",
//"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}                 ",
//"                                                                                                   ",
//"  NodeList,                                                                                        ",
//"    ZONE 2 INLETS,           !- Name                                                               ",
//"    ZONE 2 INLET;            !- Node 1 Name                                                        ",
//"                                                                                                   ",
//"  ZoneControl:Thermostat,                                                                          ",
//"    ZONE 3 CONTROLS,         !- Name                                                               ",
//"    ZONE 3,                  !- Zone or ZoneList Name                                              ",
//"    Zone Control Type Schedule,  !- Control Type Schedule Name                                     ",
//"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type                                     ",
//"    ZONE 3 SETPOINTS;        !- Control 1 Name                                                     ",
//"                                                                                                   ",
//"  ThermostatSetpoint:DualSetpoint,                                                                 ",
//"    ZONE 3 SETPOINTS,        !- Name                                                               ",
//"    ZONE Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name                         ",
//"    ZONE Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name                         ",
//"                                                                                                   ",
//"  ZoneHVAC:EquipmentConnections,                                                                   ",
//"    ZONE 3,                  !- Zone Name                                                          ",
//"    ZONE 3 EQUIPMENT,        !- Zone Conditioning Equipment List Name                              ",
//"    ZONE 3 INLETS,           !- Zone Air Inlet Node or NodeList Name                               ",
//"    ,                        !- Zone Air Exhaust Node or NodeList Name                             ",
//"    ZONE 3 NODE,             !- Zone Air Node Name                                                 ",
//"    ZONE 3 OUTLET;           !- Zone Return Air Node Name                                          ",
//"                                                                                                   ",
//"  ZoneHVAC:EquipmentList,                                                                          ",
//"    ZONE 3 EQUIPMENT,        !- Name                                                               ",
//"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                                 ",
//"    ZONE 3 Ideal Loads,      !- Zone Equipment 1 Name                                              ",
//"    1,                       !- Zone Equipment 1 Cooling Sequence                                  ",
//"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence                       ",
//"                                                                                                   ",
//"  ZoneHVAC:IdealLoadsAirSystem,                                                                    ",
//"    ZONE 3 Ideal Loads,      !- Name                                                               ",
//"    ,                        !- Availability Schedule Name                                         ",
//"    ZONE 3 INLETS,           !- Zone Supply Air Node Name                                          ",
//"    ,                        !- Zone Exhaust Air Node Name                                         ",
//"    ,                        !- System Inlet Air Node Name                                         ",
//"    50,                      !- Maximum Heating Supply Air Temperature {C}                         ",
//"    13,                      !- Minimum Cooling Supply Air Temperature {C}                         ",
//"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
//"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
//"    NoLimit,                 !- Heating Limit                                                      ",
//"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}                               ",
//"    ,                        !- Maximum Sensible Heating Capacity {W}                              ",
//"    NoLimit,                 !- Cooling Limit                                                      ",
//"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}                               ",
//"    ,                        !- Maximum Total Cooling Capacity {W}                                 ",
//"    ,                        !- Heating Availability Schedule Name                                 ",
//"    ,                        !- Cooling Availability Schedule Name                                 ",
//"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                                 ",
//"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}                        ",
//"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                                   ",
//"    ,                        !- Design Specification Outdoor Air Object Name                       ",
//"    ,                        !- Outdoor Air Inlet Node Name                                        ",
//"    ,                        !- Demand Controlled Ventilation Type                                 ",
//"    ,                        !- Outdoor Air Economizer Type                                        ",
//"    ,                        !- Heat Recovery Type                                                 ",
//"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}               ",
//"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}                 ",
//"                                                                                                   ",
//"  NodeList,                                                                                        ",
//"    ZONE 3 INLETS,           !- Name                                                               ",
//"    ZONE 3 INLET;            !- Node 1 Name                                                        ",
//"                                                                                                   ",
//"  ZoneControl:Thermostat,                                                                          ",
//"    ZONE 4 CONTROLS,         !- Name                                                               ",
//"    ZONE 4,                  !- Zone or ZoneList Name                                              ",
//"    Zone Control Type Schedule,  !- Control Type Schedule Name                                     ",
//"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type                                     ",
//"    ZONE 4 SETPOINTS;        !- Control 1 Name                                                     ",
//"                                                                                                   ",
//"  ThermostatSetpoint:DualSetpoint,                                                                 ",
//"    ZONE 4 SETPOINTS,        !- Name                                                               ",
//"    ZONE Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name                         ",
//"    ZONE Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name                         ",
//"                                                                                                   ",
//"  ZoneHVAC:EquipmentConnections,                                                                   ",
//"    ZONE 4,                  !- Zone Name                                                          ",
//"    ZONE 4 EQUIPMENT,        !- Zone Conditioning Equipment List Name                              ",
//"    ZONE 4 INLETS,           !- Zone Air Inlet Node or NodeList Name                               ",
//"    ,                        !- Zone Air Exhaust Node or NodeList Name                             ",
//"    ZONE 4 NODE,             !- Zone Air Node Name                                                 ",
//"    ZONE 4 OUTLET;           !- Zone Return Air Node Name                                          ",
//"                                                                                                   ",
//"  ZoneHVAC:EquipmentList,                                                                          ",
//"    ZONE 4 EQUIPMENT,        !- Name                                                               ",
//"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                                 ",
//"    ZONE 4 Ideal Loads,      !- Zone Equipment 1 Name                                              ",
//"    1,                       !- Zone Equipment 1 Cooling Sequence                                  ",
//"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence                       ",
//"                                                                                                   ",
//"  ZoneHVAC:IdealLoadsAirSystem,                                                                    ",
//"    ZONE 4 Ideal Loads,      !- Name                                                               ",
//"    ,                        !- Availability Schedule Name                                         ",
//"    ZONE 4 INLETS,           !- Zone Supply Air Node Name                                          ",
//"    ,                        !- Zone Exhaust Air Node Name                                         ",
//"    ,                        !- System Inlet Air Node Name                                         ",
//"    50,                      !- Maximum Heating Supply Air Temperature {C}                         ",
//"    13,                      !- Minimum Cooling Supply Air Temperature {C}                         ",
//"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
//"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}       ",
//"    NoLimit,                 !- Heating Limit                                                      ",
//"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}                               ",
//"    ,                        !- Maximum Sensible Heating Capacity {W}                              ",
//"    NoLimit,                 !- Cooling Limit                                                      ",
//"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}                               ",
//"    ,                        !- Maximum Total Cooling Capacity {W}                                 ",
//"    ,                        !- Heating Availability Schedule Name                                 ",
//"    ,                        !- Cooling Availability Schedule Name                                 ",
//"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                                 ",
//"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}                        ",
//"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                                   ",
//"    ,                        !- Design Specification Outdoor Air Object Name                       ",
//"    ,                        !- Outdoor Air Inlet Node Name                                        ",
//"    ,                        !- Demand Controlled Ventilation Type                                 ",
//"    ,                        !- Outdoor Air Economizer Type                                        ",
//"    ,                        !- Heat Recovery Type                                                 ",
//"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}               ",
//"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}                 ",
//"                                                                                                   ",
//"  NodeList,                                                                                        ",
//"    ZONE 4 INLETS,           !- Name                                                               ",
//"    ZONE 4 INLET;            !- Node 1 Name                                                        ",
//"                                                                                                   ",
//"OutputControl:Table:Style,                                                                         ",
//"    HTML,                    !- Column Separator                                                   ",
//"    JtoKWH;                  !- Unit Conversion                                                    ",
//"                                                                                                   ",
//"Output:Table:SummaryReports,                                                                       ",
//"    AllSummary;              !- Report 1 Name                                                      ",
//"                                                                                                   "
//} );

// ASSERT_TRUE( process_idf( idf_objects ) );

// OutputProcessor::TimeValue.allocate(2);
////state->dataGlobal->DDOnlySimulation = true;

// ManageSimulation();
////	compare_err_stream( "" );

// EXPECT_EQ( "3", RetrievePreDefTableEntry(*state,  pdchSurfCntTot, "Overhang" ) );
// EXPECT_EQ( "3", RetrievePreDefTableEntry(*state,  pdchSurfCntExt, "Overhang" ) );

// EXPECT_EQ( "1", RetrievePreDefTableEntry(*state,  pdchSurfCntTot, "Fin" ) );
// EXPECT_EQ( "1", RetrievePreDefTableEntry(*state,  pdchSurfCntExt, "Fin" ) );

//}

// TEST_F( EnergyPlusFixture, TubularDaylightDiffuserCount )
//{
//// based on DaylightingDeviceTubular.idf
// std::string const idf_objects = delimited_string( {
//"                                                                                          ",
//"  Building,                                                                               ",
//"    Tubular Daylighting Device Example,  !- Name                                          ",
//"    0.0,                     !- North Axis {deg}                                          ",
//"    Suburbs,                 !- Terrain                                                   ",
//"    0.04,                    !- Loads Convergence Tolerance Value                         ",
//"    0.04,                    !- Temperature Convergence Tolerance Value {deltaC}          ",
//"    FullInteriorAndExterior, !- Solar Distribution                                        ",
//"    25,                      !- Maximum Number of Warmup Days                             ",
//"    6;                       !- Minimum Number of Warmup Days                             ",
//"                                                                                          ",
//"  Timestep,6;                                                                             ",
//"                                                                                          ",
//" ShadowCalculation,",
//"    PolygonClipping,         !- Shading Calculation Method",
//"    Periodic,                !- Shading Calculation Update Frequency Method",
//"    20,                      !- Shading Calculation Update Frequency",
//"    15000;                   !- Maximum Figures in Shadow Overlap Calculations",
//"                                                                                          ",
//"  HeatBalanceAlgorithm,ConductionTransferFunction;                                        ",
//"                                                                                          ",
//"  SurfaceConvectionAlgorithm:Inside,TARP;                                                 ",
//"                                                                                          ",
//"  SurfaceConvectionAlgorithm:Outside,DOE-2;                                               ",
//"                                                                                          ",
//"  GlobalGeometryRules,                                                                    ",
//"    UpperLeftCorner,         !- Starting Vertex Position                                  ",
//"    CounterClockWise,        !- Vertex Entry Direction                                    ",
//"    Relative;                !- Coordinate System                                         ",
//"                                                                                          ",
//"  Site:Location,                                                                          ",
//"    CHICAGO_IL_USA_WMO_725300,  !- Name                                                   ",
//"    42.00,                   !- Latitude {deg}                                            ",
//"    -87.88,                  !- Longitude {deg}                                           ",
//"    -6.00,                   !- Time Zone {hr}                                            ",
//"    190.00;                  !- Elevation {m}                                             ",
//"                                                                                          ",
//"  SizingPeriod:DesignDay,                                                                 ",
//"    CHICAGO Ann Htg 99% Condns DB,  !- Name                                               ",
//"    1,                       !- Month                                                     ",
//"    21,                      !- Day of Month                                              ",
//"    WinterDesignDay,         !- Day Type                                                  ",
//"    -17.3,                   !- Maximum Dry-Bulb Temperature {C}                          ",
//"    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}                 ",
//"    ,                        !- Dry-Bulb Temperature Range Modifier Type                  ",
//"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name     ",
//"    Wetbulb,                 !- Humidity Condition Type                                   ",
//"    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}               ",
//"    ,                        !- Humidity Condition Day Schedule Name                      ",
//"    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}     ",
//"    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}                       ",
//"    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}                 ",
//"    99063.,                  !- Barometric Pressure {Pa}                                  ",
//"    4.9,                     !- Wind Speed {m/s}                                          ",
//"    270,                     !- Wind Direction {deg}                                      ",
//"    No,                      !- Rain Indicator                                            ",
//"    No,                      !- Snow Indicator                                            ",
//"    No,                      !- Daylight Saving Time Indicator                            ",
//"    ASHRAEClearSky,          !- Solar Model Indicator                                     ",
//"    ,                        !- Beam Solar Day Schedule Name                              ",
//"    ,                        !- Diffuse Solar Day Schedule Name                           ",
//"    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) ",
//"    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (tau",
//"    0.00;                    !- Sky Clearness                                             ",
//"                                                                                          ",
//"  SizingPeriod:DesignDay,                                                                 ",
//"    CHICAGO Ann Clg 1% Condns DB=>MWB,  !- Name                                           ",
//"    7,                       !- Month                                                     ",
//"    21,                      !- Day of Month                                              ",
//"    SummerDesignDay,         !- Day Type                                                  ",
//"    31.5,                    !- Maximum Dry-Bulb Temperature {C}                          ",
//"    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}                 ",
//"    ,                        !- Dry-Bulb Temperature Range Modifier Type                  ",
//"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name     ",
//"    Wetbulb,                 !- Humidity Condition Type                                   ",
//"    23,                      !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}               ",
//"    ,                        !- Humidity Condition Day Schedule Name                      ",
//"    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}     ",
//"    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}                       ",
//"    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}                 ",
//"    99063.,                  !- Barometric Pressure {Pa}                                  ",
//"    5.3,                     !- Wind Speed {m/s}                                          ",
//"    230,                     !- Wind Direction {deg}                                      ",
//"    No,                      !- Rain Indicator                                            ",
//"    No,                      !- Snow Indicator                                            ",
//"    No,                      !- Daylight Saving Time Indicator                            ",
//"    ASHRAEClearSky,          !- Solar Model Indicator                                     ",
//"    ,                        !- Beam Solar Day Schedule Name                              ",
//"    ,                        !- Diffuse Solar Day Schedule Name                           ",
//"    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) ",
//"    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (tau",
//"    1.00;                    !- Sky Clearness                                             ",
//"                                                                                          ",
//"  SimulationControl,                                                                      ",
//"    NO,                      !- Do Zone Sizing Calculation                                ",
//"    NO,                      !- Do System Sizing Calculation                              ",
//"    NO,                      !- Do Plant Sizing Calculation                               ",
//"    YES,                     !- Run Simulation for Sizing Periods                         ",
//"    NO;                      !- Run Simulation for Weather File Run Periods               ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    A1 - 1 IN STUCCO,        !- Name                                                      ",
//"    Smooth,                  !- Roughness                                                 ",
//"    2.5389841E-02,           !- Thickness {m}                                             ",
//"    0.6918309,               !- Conductivity {W/m-K}                                      ",
//"    1858.142,                !- Density {kg/m3}                                           ",
//"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.92,                    !- Solar Absorptance                                         ",
//"    0.92;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    C4 - 4 IN COMMON BRICK,  !- Name                                                      ",
//"    Rough,                   !- Roughness                                                 ",
//"    0.1014984,               !- Thickness {m}                                             ",
//"    0.7264224,               !- Conductivity {W/m-K}                                      ",
//"    1922.216,                !- Density {kg/m3}                                           ",
//"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.76,                    !- Solar Absorptance                                         ",
//"    0.76;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name                                          ",
//"    Smooth,                  !- Roughness                                                 ",
//"    1.9050000E-02,           !- Thickness {m}                                             ",
//"    0.7264224,               !- Conductivity {W/m-K}                                      ",
//"    1601.846,                !- Density {kg/m3}                                           ",
//"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.92,                    !- Solar Absorptance                                         ",
//"    0.92;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    C6 - 8 IN CLAY TILE,     !- Name                                                      ",
//"    Smooth,                  !- Roughness                                                 ",
//"    0.2033016,               !- Thickness {m}                                             ",
//"    0.5707605,               !- Conductivity {W/m-K}                                      ",
//"    1121.292,                !- Density {kg/m3}                                           ",
//"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.82,                    !- Solar Absorptance                                         ",
//"    0.82;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    C10 - 8 IN HW CONCRETE,  !- Name                                                      ",
//"    MediumRough,             !- Roughness                                                 ",
//"    0.2033016,               !- Thickness {m}                                             ",
//"    1.729577,                !- Conductivity {W/m-K}                                      ",
//"    2242.585,                !- Density {kg/m3}                                           ",
//"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.65,                    !- Solar Absorptance                                         ",
//"    0.65;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    E2 - 1 / 2 IN SLAG OR STONE,  !- Name                                                 ",
//"    Rough,                   !- Roughness                                                 ",
//"    1.2710161E-02,           !- Thickness {m}                                             ",
//"    1.435549,                !- Conductivity {W/m-K}                                      ",
//"    881.0155,                !- Density {kg/m3}                                           ",
//"    1673.6,                  !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.55,                    !- Solar Absorptance                                         ",
//"    0.55;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name                                             ",
//"    Rough,                   !- Roughness                                                 ",
//"    9.5402403E-03,           !- Thickness {m}                                             ",
//"    0.1902535,               !- Conductivity {W/m-K}                                      ",
//"    1121.292,                !- Density {kg/m3}                                           ",
//"    1673.6,                  !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.75,                    !- Solar Absorptance                                         ",
//"    0.75;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    B5 - 1 IN DENSE INSULATION,  !- Name                                                  ",
//"    VeryRough,               !- Roughness                                                 ",
//"    2.5389841E-02,           !- Thickness {m}                                             ",
//"    4.3239430E-02,           !- Conductivity {W/m-K}                                      ",
//"    91.30524,                !- Density {kg/m3}                                           ",
//"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.50,                    !- Solar Absorptance                                         ",
//"    0.50;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    C12 - 2 IN HW CONCRETE,  !- Name                                                      ",
//"    MediumRough,             !- Roughness                                                 ",
//"    5.0901599E-02,           !- Thickness {m}                                             ",
//"    1.729577,                !- Conductivity {W/m-K}                                      ",
//"    2242.585,                !- Density {kg/m3}                                           ",
//"    836.8,                   !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.65,                    !- Solar Absorptance                                         ",
//"    0.65;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    ROOFING - ASPHALT SHINGLES,  !- Name                                                  ",
//"    VeryRough,               !- Roughness                                                 ",
//"    3.1999999E-03,           !- Thickness {m}                                             ",
//"    2.9999999E-02,           !- Conductivity {W/m-K}                                      ",
//"    1121.29,                 !- Density {kg/m3}                                           ",
//"    830.0,                   !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.70,                    !- Solar Absorptance                                         ",
//"    0.70;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    BB46 - 5 / 8 IN PLYWOOD, !- Name                                                      ",
//"    Smooth,                  !- Roughness                                                 ",
//"    9.9999998E-03,           !- Thickness {m}                                             ",
//"    0.110,                   !- Conductivity {W/m-K}                                      ",
//"    544.62,                  !- Density {kg/m3}                                           ",
//"    1210.0,                  !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.70,                    !- Solar Absorptance                                         ",
//"    0.70;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    INS - GLASS FIBER BONDED 3 IN,  !- Name                                               ",
//"    VeryRough,               !- Roughness                                                 ",
//"    7.000E-02,               !- Thickness {m}                                             ",
//"    2.9999999E-02,           !- Conductivity {W/m-K}                                      ",
//"    96.11,                   !- Density {kg/m3}                                           ",
//"    790.0,                   !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.50,                    !- Solar Absorptance                                         ",
//"    0.50;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  WindowMaterial:Glazing,                                                                 ",
//"    Clear Acrylic Plastic,   !- Name                                                      ",
//"    SpectralAverage,         !- Optical Data Type                                         ",
//"    ,                        !- Window Glass Spectral Data Set Name                       ",
//"    0.003,                   !- Thickness {m}                                             ",
//"    0.92,                    !- Solar Transmittance at Normal Incidence                   ",
//"    0.05,                    !- Front Side Solar Reflectance at Normal Incidence          ",
//"    0.05,                    !- Back Side Solar Reflectance at Normal Incidence           ",
//"    0.92,                    !- Visible Transmittance at Normal Incidence                 ",
//"    0.05,                    !- Front Side Visible Reflectance at Normal Incidence        ",
//"    0.05,                    !- Back Side Visible Reflectance at Normal Incidence         ",
//"    0.00,                    !- Infrared Transmittance at Normal Incidence                ",
//"    0.90,                    !- Front Side Infrared Hemispherical Emissivity              ",
//"    0.90,                    !- Back Side Infrared Hemispherical Emissivity               ",
//"    0.90;                    !- Conductivity {W/m-K}                                      ",
//"                                                                                          ",
//"  WindowMaterial:Glazing,                                                                 ",
//"    Diffusing Acrylic Plastic,  !- Name                                                   ",
//"    SpectralAverage,         !- Optical Data Type                                         ",
//"    ,                        !- Window Glass Spectral Data Set Name                       ",
//"    0.0022,                  !- Thickness {m}                                             ",
//"    0.90,                    !- Solar Transmittance at Normal Incidence                   ",
//"    0.08,                    !- Front Side Solar Reflectance at Normal Incidence          ",
//"    0.08,                    !- Back Side Solar Reflectance at Normal Incidence           ",
//"    0.90,                    !- Visible Transmittance at Normal Incidence                 ",
//"    0.08,                    !- Front Side Visible Reflectance at Normal Incidence        ",
//"    0.08,                    !- Back Side Visible Reflectance at Normal Incidence         ",
//"    0.00,                    !- Infrared Transmittance at Normal Incidence                ",
//"    0.90,                    !- Front Side Infrared Hemispherical Emissivity              ",
//"    0.90,                    !- Back Side Infrared Hemispherical Emissivity               ",
//"    0.90;                    !- Conductivity {W/m-K}                                      ",
//"                                                                                          ",
//"  Material,                                                                               ",
//"    Very High Reflectivity Surface,  !- Name                                              ",
//"    Smooth,                  !- Roughness                                                 ",
//"    0.0005,                  !- Thickness {m}                                             ",
//"    237,                     !- Conductivity {W/m-K}                                      ",
//"    2702,                    !- Density {kg/m3}                                           ",
//"    903,                     !- Specific Heat {J/kg-K}                                    ",
//"    0.90,                    !- Thermal Absorptance                                       ",
//"    0.05,                    !- Solar Absorptance                                         ",
//"    0.05;                    !- Visible Absorptance                                       ",
//"                                                                                          ",
//"  Construction,                                                                           ",
//"    EXTWALL80,               !- Name                                                      ",
//"    A1 - 1 IN STUCCO,        !- Outside Layer                                             ",
//"    C4 - 4 IN COMMON BRICK,  !- Layer 2                                                   ",
//"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3                                       ",
//"                                                                                          ",
//"  Construction,                                                                           ",
//"    FLOOR SLAB 8 IN,         !- Name                                                      ",
//"    C10 - 8 IN HW CONCRETE;  !- Outside Layer                                             ",
//"                                                                                          ",
//"  Construction,                                                                           ",
//"    ROOF,                    !- Name                                                      ",
//"    ROOFING - ASPHALT SHINGLES,  !- Outside Layer                                         ",
//"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2                                          ",
//"    BB46 - 5 / 8 IN PLYWOOD; !- Layer 3                                                   ",
//"                                                                                          ",
//"  Construction,                                                                           ",
//"    CEILING IN ZONE,         !- Name                                                      ",
//"    INS - GLASS FIBER BONDED 3 IN,  !- Outside Layer                                      ",
//"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 2                                       ",
//"                                                                                          ",
//"  Construction,                                                                           ",
//"    CEILING IN ATTIC,        !- Name                                                      ",
//"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer                                 ",
//"    INS - GLASS FIBER BONDED 3 IN;  !- Layer 2                                            ",
//"                                                                                          ",
//"  Construction,                                                                           ",
//"    TDD Pipe,                !- Name                                                      ",
//"    Very High Reflectivity Surface;  !- Outside Layer                                     ",
//"                                                                                          ",
//"  Construction,                                                                           ",
//"    TDD Dome,                !- Name                                                      ",
//"    Clear Acrylic Plastic;   !- Outside Layer                                             ",
//"                                                                                          ",
//"  Construction,                                                                           ",
//"    TDD Diffuser,            !- Name                                                      ",
//"    Diffusing Acrylic Plastic;  !- Outside Layer                                          ",
//"                                                                                          ",
//"  Zone,                                                                                   ",
//"    Daylit Zone,             !- Name                                                      ",
//"    0.0,                     !- Direction of Relative North {deg}                         ",
//"    0.0,                     !- X Origin {m}                                              ",
//"    0.0,                     !- Y Origin {m}                                              ",
//"    0.0,                     !- Z Origin {m}                                              ",
//"    1,                       !- Type                                                      ",
//"    1,                       !- Multiplier                                                ",
//"    autocalculate,           !- Ceiling Height {m}                                        ",
//"    autocalculate;           !- Volume {m3}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit South Wall,       !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Daylit Zone,             !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit West Wall,        !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Daylit Zone,             !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    0.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit North Wall,       !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Daylit Zone,             !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    0.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit East Wall,        !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Daylit Zone,             !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit Floor,            !- Name                                                      ",
//"    Floor,                   !- Surface Type                                              ",
//"    FLOOR SLAB 8 IN,         !- Construction Name                                         ",
//"    Daylit Zone,             !- Zone Name                                                 ",
//"    Surface,                 !- Outside Boundary Condition                                ",
//"    Daylit Floor,            !- Outside Boundary Condition Object                         ",
//"    NoSun,                   !- Sun Exposure                                              ",
//"    NoWind,                  !- Wind Exposure                                             ",
//"    1.0,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    5.0,0.0,0.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit Ceiling,          !- Name                                                      ",
//"    Roof,                    !- Surface Type                                              ",
//"    CEILING IN ZONE,         !- Construction Name                                         ",
//"    Daylit Zone,             !- Zone Name                                                 ",
//"    Surface,                 !- Outside Boundary Condition                                ",
//"    Daylit Attic Floor,      !- Outside Boundary Condition Object                         ",
//"    NoSun,                   !- Sun Exposure                                              ",
//"    NoWind,                  !- Wind Exposure                                             ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  Zone,                                                                                   ",
//"    Daylit Attic Zone,       !- Name                                                      ",
//"    0.0,                     !- Direction of Relative North {deg}                         ",
//"    0.0,                     !- X Origin {m}                                              ",
//"    0.0,                     !- Y Origin {m}                                              ",
//"    0.0,                     !- Z Origin {m}                                              ",
//"    1,                       !- Type                                                      ",
//"    1,                       !- Multiplier                                                ",
//"    autocalculate,           !- Ceiling Height {m}                                        ",
//"    autocalculate;           !- Volume {m3}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit Attic South Wall, !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Daylit Attic Zone,       !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    5.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit Attic West Wall,  !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Daylit Attic Zone,       !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    0.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit Attic North Wall, !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Daylit Attic Zone,       !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    5.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    0.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit Attic East Wall,  !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Daylit Attic Zone,       !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit Attic Floor,      !- Name                                                      ",
//"    Floor,                   !- Surface Type                                              ",
//"    CEILING IN ATTIC,        !- Construction Name                                         ",
//"    Daylit Attic Zone,       !- Zone Name                                                 ",
//"    Surface,                 !- Outside Boundary Condition                                ",
//"    Daylit Ceiling,          !- Outside Boundary Condition Object                         ",
//"    NoSun,                   !- Sun Exposure                                              ",
//"    NoWind,                  !- Wind Exposure                                             ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Daylit Attic Roof,       !- Name                                                      ",
//"    Roof,                    !- Surface Type                                              ",
//"    ROOF,                    !- Construction Name                                         ",
//"    Daylit Attic Zone,       !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  Zone,                                                                                   ",
//"    Standard Zone,           !- Name                                                      ",
//"    0.0,                     !- Direction of Relative North {deg}                         ",
//"    50.0,                    !- X Origin {m}                                              ",
//"    50.0,                    !- Y Origin {m}                                              ",
//"    0.0,                     !- Z Origin {m}                                              ",
//"    1,                       !- Type                                                      ",
//"    1,                       !- Multiplier                                                ",
//"    autocalculate,           !- Ceiling Height {m}                                        ",
//"    autocalculate;           !- Volume {m3}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard South Wall,     !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Standard Zone,           !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard West Wall,      !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Standard Zone,           !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    0.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard North Wall,     !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Standard Zone,           !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    0.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard East Wall,      !- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Standard Zone,           !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard Floor,          !- Name                                                      ",
//"    Floor,                   !- Surface Type                                              ",
//"    FLOOR SLAB 8 IN,         !- Construction Name                                         ",
//"    Standard Zone,           !- Zone Name                                                 ",
//"    Surface,                 !- Outside Boundary Condition                                ",
//"    Standard Floor,          !- Outside Boundary Condition Object                         ",
//"    NoSun,                   !- Sun Exposure                                              ",
//"    NoWind,                  !- Wind Exposure                                             ",
//"    1.0,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    5.0,0.0,0.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard Ceiling,        !- Name                                                      ",
//"    Roof,                    !- Surface Type                                              ",
//"    CEILING IN ZONE,         !- Construction Name                                         ",
//"    Standard Zone,           !- Zone Name                                                 ",
//"    Surface,                 !- Outside Boundary Condition                                ",
//"    Standard Attic Floor,    !- Outside Boundary Condition Object                         ",
//"    NoSun,                   !- Sun Exposure                                              ",
//"    NoWind,                  !- Wind Exposure                                             ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  Zone,                                                                                   ",
//"    Standard Attic Zone,     !- Name                                                      ",
//"    0.0,                     !- Direction of Relative North {deg}                         ",
//"    50.0,                    !- X Origin {m}                                              ",
//"    50.0,                    !- Y Origin {m}                                              ",
//"    0.0,                     !- Z Origin {m}                                              ",
//"    1,                       !- Type                                                      ",
//"    1,                       !- Multiplier                                                ",
//"    autocalculate,           !- Ceiling Height {m}                                        ",
//"    autocalculate;           !- Volume {m3}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard Attic South Wall,  !- Name                                                   ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Standard Attic Zone,     !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    5.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard Attic West Wall,!- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Standard Attic Zone,     !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    0.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard Attic North Wall,  !- Name                                                   ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Standard Attic Zone,     !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    5.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    0.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard Attic East Wall,!- Name                                                      ",
//"    Wall,                    !- Surface Type                                              ",
//"    EXTWALL80,               !- Construction Name                                         ",
//"    Standard Attic Zone,     !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.5,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard Attic Floor,    !- Name                                                      ",
//"    Floor,                   !- Surface Type                                              ",
//"    CEILING IN ATTIC,        !- Construction Name                                         ",
//"    Standard Attic Zone,     !- Zone Name                                                 ",
//"    Surface,                 !- Outside Boundary Condition                                ",
//"    Standard Ceiling,        !- Outside Boundary Condition Object                         ",
//"    NoSun,                   !- Sun Exposure                                              ",
//"    NoWind,                  !- Wind Exposure                                             ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
//"    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
//"    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
//"    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
//"                                                                                          ",
//"  BuildingSurface:Detailed,                                                               ",
//"    Standard Attic Roof,     !- Name                                                      ",
//"    Roof,                    !- Surface Type                                              ",
//"    ROOF,                    !- Construction Name                                         ",
//"    Standard Attic Zone,     !- Zone Name                                                 ",
//"    Outdoors,                !- Outside Boundary Condition                                ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    SunExposed,              !- Sun Exposure                                              ",
//"    WindExposed,             !- Wind Exposure                                             ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    4,                       !- Number of Vertices                                        ",
//"    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
//"    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
//"    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
//"    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
//"                                                                                          ",
//"  Lights,                                                                                 ",
//"    Daylit Zone Lights 1,    !- Name                                                      ",
//"    Daylit Zone,             !- Zone or ZoneList Name                                     ",
//"    AlwaysOnSchedule,        !- Schedule Name                                             ",
//"    LightingLevel,           !- Design Level Calculation Method                           ",
//"    800,                     !- Lighting Level {W}                                        ",
//"    ,                        !- Watts per Zone Floor Area {W/m2}                          ",
//"    ,                        !- Watts per Person {W/person}                               ",
//"    0,                       !- Return Air Fraction                                       ",
//"    0.40,                    !- Fraction Radiant                                          ",
//"    0.20,                    !- Fraction Visible                                          ",
//"    1.0,                     !- Fraction Replaceable                                      ",
//"    GeneralLights;           !- End-Use Subcategory                                       ",
//"                                                                                          ",
//"  Lights,                                                                                 ",
//"    Standard Zone Lights 1,  !- Name                                                      ",
//"    Standard Zone,           !- Zone or ZoneList Name                                     ",
//"    AlwaysOnSchedule,        !- Schedule Name                                             ",
//"    LightingLevel,           !- Design Level Calculation Method                           ",
//"    800,                     !- Lighting Level {W}                                        ",
//"    ,                        !- Watts per Zone Floor Area {W/m2}                          ",
//"    ,                        !- Watts per Person {W/person}                               ",
//"    0,                       !- Return Air Fraction                                       ",
//"    0.40,                    !- Fraction Radiant                                          ",
//"    0.20,                    !- Fraction Visible                                          ",
//"    1.0,                     !- Fraction Replaceable                                      ",
//"    GeneralLights;           !- End-Use Subcategory                                       ",
//"                                                                                          ",
//"  DaylightingDevice:Tubular,                                                              ",
//"    Pipe1,                   !- Name                                                      ",
//"    Dome1,                   !- Dome Name                                                 ",
//"    Diffuser1,               !- Diffuser Name                                             ",
//"    TDD Pipe,                !- Construction Name                                         ",
//"    0.3556,                  !- Diameter {m}                                              ",
//"    1.4,                     !- Total Length {m}                                          ",
//"    0.28,                    !- Effective Thermal Resistance {m2-K/W}                     ",
//"    Daylit Attic Zone,       !- Transition Zone 1 Name                                    ",
//"    1.1;                     !- Transition Zone 1 Length {m}                              ",
//"                                                                                          ",
//"  FenestrationSurface:Detailed,                                                           ",
//"    Dome1,                   !- Name                                                      ",
//"    TubularDaylightDome,     !- Surface Type                                              ",
//"    TDD Dome,                !- Construction Name                                         ",
//"    Daylit Attic Roof,       !- Building Surface Name                                     ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    ,                        !- Frame and Divider Name                                    ",
//"    1.0,                     !- Multiplier                                                ",
//"    4,                       !- Number of Vertices                                        ",
//"    2.3425,3.209,3.64,  !- X,Y,Z ==> Vertex 1 {m}                                         ",
//"    2.3425,2.906,3.58,  !- X,Y,Z ==> Vertex 2 {m}                                         ",
//"    2.6575,2.906,3.58,  !- X,Y,Z ==> Vertex 3 {m}                                         ",
//"    2.6575,3.209,3.64;  !- X,Y,Z ==> Vertex 4 {m}                                         ",
//"                                                                                          ",
//"  FenestrationSurface:Detailed,                                                           ",
//"    Diffuser1,               !- Name                                                      ",
//"    TubularDaylightDiffuser, !- Surface Type                                              ",
//"    TDD Diffuser,            !- Construction Name                                         ",
//"    Daylit Ceiling,          !- Building Surface Name                                     ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    ,                        !- Frame and Divider Name                                    ",
//"    1.0,                     !- Multiplier                                                ",
//"    4,                       !- Number of Vertices                                        ",
//"    2.3425,3.1575,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                         ",
//"    2.3425,2.8425,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                         ",
//"    2.6575,2.8425,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                         ",
//"    2.6575,3.1575,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                         ",
//"                                                                                          ",
//"  DaylightingDevice:Tubular,                                                              ",
//"    Pipe2,                   !- Name                                                      ",
//"    Dome2,                   !- Dome Name                                                 ",
//"    Diffuser2,               !- Diffuser Name                                             ",
//"    TDD Pipe,                !- Construction Name                                         ",
//"    0.3556,                  !- Diameter {m}                                              ",
//"    2.2,                     !- Total Length {m}                                          ",
//"    0.28,                    !- Effective Thermal Resistance {m2-K/W}                     ",
//"    Daylit Attic Zone,       !- Transition Zone 1 Name                                    ",
//"    1.9;                     !- Transition Zone 1 Length {m}                              ",
//"                                                                                          ",
//"  FenestrationSurface:Detailed,                                                           ",
//"    Dome2,                   !- Name                                                      ",
//"    TubularDaylightDome,     !- Surface Type                                              ",
//"    TDD Dome,                !- Construction Name                                         ",
//"    Daylit Attic Roof,       !- Building Surface Name                                     ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    ,                        !- Frame and Divider Name                                    ",
//"    1.0,                     !- Multiplier                                                ",
//"    4,                       !- Number of Vertices                                        ",
//"    2.3425,7.209134615385,4.441826923077,  !- X,Y,Z ==> Vertex 1 {m}                      ",
//"    2.3425,6.90625,4.38125,  !- X,Y,Z ==> Vertex 2 {m}                                    ",
//"    2.6575,6.90625,4.38125,  !- X,Y,Z ==> Vertex 3 {m}                                    ",
//"    2.6575,7.209134615385,4.441826923077;  !- X,Y,Z ==> Vertex 4 {m}                      ",
//"                                                                                          ",
//"  FenestrationSurface:Detailed,                                                           ",
//"    Diffuser2,               !- Name                                                      ",
//"    TubularDaylightDiffuser, !- Surface Type                                              ",
//"    TDD Diffuser,            !- Construction Name                                         ",
//"    Daylit Ceiling,          !- Building Surface Name                                     ",
//"    ,                        !- Outside Boundary Condition Object                         ",
//"    0.0,                     !- View Factor to Ground                                     ",
//"    ,                        !- Frame and Divider Name                                    ",
//"    1.0,                     !- Multiplier                                                ",
//"    4,                       !- Number of Vertices                                        ",
//"    2.3425,7.1575,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                         ",
//"    2.3425,6.8425,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                         ",
//"    2.6575,6.8425,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                         ",
//"    2.6575,7.1575,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                         ",
//"                                                                                          ",
//"  Daylighting:Controls,                                                                                                  ",
//"    Daylit Zone_DaylCtrl,    !- Name                                                                                     ",
//"    Daylit Zone,             !- Zone Name                                                                                ",
//"    SplitFlux,               !- Daylighting Method                                                                       ",
//"    ,                        !- Availability Schedule Name                                                               ",
//"    Continuous,              !- Lighting Control Type                                                                    ",
//"    0.3,                     !- Minimum Input Power Fraction for Continuous or ContinuousOff Dimming Control             ",
//"    0.2,                     !- Minimum Light Output Fraction for Continuous or ContinuousOff Dimming Control            ",
//"    ,                        !- Number of Stepped Control Steps                                                          ",
//"    1.0,                     !- Probability Lighting will be Reset When Needed in Manual Stepped Control                 ",
//"    Daylit Zone_DaylRefPt1,  !- Glare Calculation Daylighting Reference Point Name                                       ",
//"    0.0,                     !- Glare Calculation Azimuth Angle of View Direction Clockwise from Zone y-Axis {deg}       ",
//"    20.0,                    !- Maximum Allowable Discomfort Glare Index                                                 ",
//"    ,                        !- DElight Gridding Resolution {m2}                                                         ",
//"    Daylit Zone_DaylRefPt1,  !- Daylighting Reference Point 1 Name                                                       ",
//"    1.0,                     !- Fraction of Zone Controlled by Reference Point 1                                         ",
//"    550,                     !- Illuminance Setpoint at Reference Point 1 {lux}                                          ",
//"    Daylit Zone_DaylRefPt2,  !- Daylighting Reference Point 2 Name                                                       ",
//"    0.0,                     !- Fraction of Zone Controlled by Reference Point 2                                         ",
//"    0;                       !- Illuminance Setpoint at Reference Point 2 {lux}                                          ",
//"                                                                                                                         ",
//"  Daylighting:ReferencePoint,                                                                                            ",
//"    Daylit Zone_DaylRefPt1,  !- Name                                                                                     ",
//"    Daylit Zone,             !- Zone Name                                                                                ",
//"    2.5,                     !- X-Coordinate of Reference Point {m}                                                      ",
//"    5.0,                     !- Y-Coordinate of Reference Point {m}                                                      ",
//"    0.8;                     !- Z-Coordinate of Reference Point {m}                                                      ",
//"                                                                                                                         ",
//"  Daylighting:ReferencePoint,                                                                                            ",
//"    Daylit Zone_DaylRefPt2,  !- Name                                                                                     ",
//"    Daylit Zone,             !- Zone Name                                                                                ",
//"    2.5,                     !- X-Coordinate of Reference Point {m}                                                      ",
//"    3.0,                     !- Y-Coordinate of Reference Point {m}                                                      ",
//"    0.8;                     !- Z-Coordinate of Reference Point {m}                                                      ",
//"                                                                                          ",
//"  OutputControl:IlluminanceMap:Style,                                                     ",
//"    Comma;                   !- Column Separator                                          ",
//"                                                                                          ",
//"  Output:IlluminanceMap,                                                                  ",
//"    Daylit Map,              !- Name                                                      ",
//"    Daylit Zone,             !- Zone Name                                                 ",
//"    0.8,                     !- Z height {m}                                              ",
//"    0.1,                     !- X Minimum Coordinate {m}                                  ",
//"    4.9,                     !- X Maximum Coordinate {m}                                  ",
//"    10,                      !- Number of X Grid Points                                   ",
//"    0.1,                     !- Y Minimum Coordinate {m}                                  ",
//"    9.9,                     !- Y Maximum Coordinate {m}                                  ",
//"    10;                      !- Number of Y Grid Points                                   ",
//"                                                                                          ",
//"  ScheduleTypeLimits,                                                                     ",
//"    On/Off,                  !- Name                                                      ",
//"    0,                       !- Lower Limit Value                                         ",
//"    1,                       !- Upper Limit Value                                         ",
//"    DISCRETE;                !- Numeric Type                                              ",
//"                                                                                          ",
//"  Schedule:Compact,                                                                       ",
//"    AlwaysOnSchedule,        !- Name                                                      ",
//"    On/Off,                  !- Schedule Type Limits Name                                 ",
//"    THROUGH: 12/31,          !- Field 1                                                   ",
//"    FOR: AllDays,            !- Field 2                                                   ",
//"    UNTIL: 24:00,1;          !- Field 3                                                   ",
//"                                                                                          ",
//"  ScheduleTypeLimits,                                                                     ",
//"    Control Type,            !- Name                                                      ",
//"    0,                       !- Lower Limit Value                                         ",
//"    4,                       !- Upper Limit Value                                         ",
//"    DISCRETE;                !- Numeric Type                                              ",
//"                                                                                          ",
//"  Schedule:Compact,                                                                       ",
//"    Zone Control Type Sch,   !- Name                                                      ",
//"    Control Type,            !- Schedule Type Limits Name                                 ",
//"    THROUGH: 12/31,          !- Field 1                                                   ",
//"    FOR: AllDays,            !- Field 2                                                   ",
//"    UNTIL: 24:00,3;          !- Field 3                                                   ",
//"                                                                                          ",
//"  ScheduleTypeLimits,                                                                     ",
//"    Temperature,             !- Name                                                      ",
//"    -60,                     !- Lower Limit Value                                         ",
//"    200,                     !- Upper Limit Value                                         ",
//"    CONTINUOUS,              !- Numeric Type                                              ",
//"    Temperature;             !- Unit Type                                                 ",
//"                                                                                          ",
//"  Schedule:Compact,                                                                       ",
//"    Zone Temp Sch,           !- Name                                                      ",
//"    Temperature,             !- Schedule Type Limits Name                                 ",
//"    THROUGH: 12/31,          !- Field 1                                                   ",
//"    FOR: AllDays,            !- Field 2                                                   ",
//"    UNTIL: 24:00,21;         !- Field 3                                                   ",
//"                                                                                          ",
//"  NodeList,                                                                               ",
//"    Daylit Zone Inlets,      !- Name                                                      ",
//"    Daylit Zone Supply Node; !- Node 1 Name                                               ",
//"                                                                                          ",
//"  ZoneHVAC:EquipmentConnections,                                                          ",
//"    Daylit Zone,             !- Zone Name                                                 ",
//"    Daylit Zone Equipment,   !- Zone Conditioning Equipment List Name                     ",
//"    Daylit Zone Inlets,      !- Zone Air Inlet Node or NodeList Name                      ",
//"    ,                        !- Zone Air Exhaust Node or NodeList Name                    ",
//"    Daylit Zone Air Node,    !- Zone Air Node Name                                        ",
//"    Daylit Zone Return Node; !- Zone Return Air Node Name                                 ",
//"                                                                                          ",
//"  ThermostatSetpoint:SingleHeatingOrCooling,                                              ",
//"    Heating Cooling Setpoint,!- Name                                                      ",
//"    Zone Temp Sch;           !- Setpoint Temperature Schedule Name                        ",
//"                                                                                          ",
//"  ZoneControl:Thermostat,                                                                 ",
//"    Daylit Zone Thermostat,  !- Name                                                      ",
//"    Daylit Zone,             !- Zone or ZoneList Name                                     ",
//"    Zone Control Type Sch,   !- Control Type Schedule Name                                ",
//"    ThermostatSetpoint:SingleHeatingOrCooling,  !- Control 1 Object Type                  ",
//"    Heating Cooling Setpoint;!- Control 1 Name                                            ",
//"                                                                                          ",
//"  ZoneHVAC:EquipmentList,                                                                 ",
//"    Daylit Zone Equipment,   !- Name                                                      ",
//"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                        ",
//"    Daylit Zone Air,         !- Zone Equipment 1 Name                                     ",
//"    1,                       !- Zone Equipment 1 Cooling Sequence                         ",
//"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence              ",
//"                                                                                          ",
//"  ZoneHVAC:IdealLoadsAirSystem,                                                           ",
//"    Daylit Zone Air,         !- Name                                                      ",
//"    ,                        !- Availability Schedule Name                                ",
//"    Daylit Zone Supply Node, !- Zone Supply Air Node Name                                 ",
//"    ,                        !- Zone Exhaust Air Node Name                                ",
//"    ,                        !- System Inlet Air Node Name                                         ",
//"    50,                      !- Maximum Heating Supply Air Temperature {C}                ",
//"    13,                      !- Minimum Cooling Supply Air Temperature {C}                ",
//"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAi",
//"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAi",
//"    NoLimit,                 !- Heating Limit                                             ",
//"    AUTOSIZE,                !- Maximum Heating Air Flow Rate {m3/s}                      ",
//"    ,                        !- Maximum Sensible Heating Capacity {W}                     ",
//"    NoLimit,                 !- Cooling Limit                                             ",
//"    AUTOSIZE,                !- Maximum Cooling Air Flow Rate {m3/s}                      ",
//"    ,                        !- Maximum Total Cooling Capacity {W}                        ",
//"    ,                        !- Heating Availability Schedule Name                        ",
//"    ,                        !- Cooling Availability Schedule Name                        ",
//"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                        ",
//"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}               ",
//"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                          ",
//"    ,                        !- Design Specification Outdoor Air Object Name              ",
//"    ,                        !- Outdoor Air Inlet Node Name                               ",
//"    ,                        !- Demand Controlled Ventilation Type                        ",
//"    ,                        !- Outdoor Air Economizer Type                               ",
//"    ,                        !- Heat Recovery Type                                        ",
//"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}      ",
//"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}        ",
//"                                                                                          ",
//"  NodeList,                                                                               ",
//"    Standard Zone Inlets,    !- Name                                                      ",
//"    Standard Zone Supply Node;  !- Node 1 Name                                            ",
//"                                                                                          ",
//"  ZoneHVAC:EquipmentConnections,                                                          ",
//"    Standard Zone,           !- Zone Name                                                 ",
//"    Standard Zone Equipment, !- Zone Conditioning Equipment List Name                     ",
//"    Standard Zone Inlets,    !- Zone Air Inlet Node or NodeList Name                      ",
//"    ,                        !- Zone Air Exhaust Node or NodeList Name                    ",
//"    Standard Zone Air Node,  !- Zone Air Node Name                                        ",
//"    Standard Zone Return Node;  !- Zone Return Air Node Name                              ",
//"                                                                                          ",
//"  ThermostatSetpoint:SingleHeatingOrCooling,                                              ",
//"    Heating Cooling Setpoint,!- Name                                                      ",
//"    Zone Temp Sch;           !- Setpoint Temperature Schedule Name                        ",
//"                                                                                          ",
//"  ZoneControl:Thermostat,                                                                 ",
//"    Standard Zone Thermostat,!- Name                                                      ",
//"    Standard Zone,           !- Zone or ZoneList Name                                     ",
//"    Zone Control Type Sch,   !- Control Type Schedule Name                                ",
//"    ThermostatSetpoint:SingleHeatingOrCooling,  !- Control 1 Object Type                  ",
//"    Heating Cooling Setpoint;!- Control 1 Name                                            ",
//"                                                                                          ",
//"  ZoneHVAC:EquipmentList,                                                                 ",
//"    Standard Zone Equipment, !- Name                                                      ",
//"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type                        ",
//"    Standard Zone Air,       !- Zone Equipment 1 Name                                     ",
//"    1,                       !- Zone Equipment 1 Cooling Sequence                         ",
//"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence              ",
//"                                                                                          ",
//"  ZoneHVAC:IdealLoadsAirSystem,                                                           ",
//"    Standard Zone Air,       !- Name                                                      ",
//"    ,                        !- Availability Schedule Name                                ",
//"    Standard Zone Supply Node,  !- Zone Supply Air Node Name                              ",
//"    ,                        !- Zone Exhaust Air Node Name                                ",
//"    ,                        !- System Inlet Air Node Name                                         ",
//"    50,                      !- Maximum Heating Supply Air Temperature {C}                ",
//"    13,                      !- Minimum Cooling Supply Air Temperature {C}                ",
//"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAi",
//"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAi",
//"    NoLimit,                 !- Heating Limit                                             ",
//"    AUTOSIZE,                !- Maximum Heating Air Flow Rate {m3/s}                      ",
//"    ,                        !- Maximum Sensible Heating Capacity {W}                     ",
//"    NoLimit,                 !- Cooling Limit                                             ",
//"    AUTOSIZE,                !- Maximum Cooling Air Flow Rate {m3/s}                      ",
//"    ,                        !- Maximum Total Cooling Capacity {W}                        ",
//"    ,                        !- Heating Availability Schedule Name                        ",
//"    ,                        !- Cooling Availability Schedule Name                        ",
//"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type                        ",
//"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}               ",
//"    ConstantSupplyHumidityRatio,  !- Humidification Control Type                          ",
//"    ,                        !- Design Specification Outdoor Air Object Name              ",
//"    ,                        !- Outdoor Air Inlet Node Name                               ",
//"    ,                        !- Demand Controlled Ventilation Type                        ",
//"    ,                        !- Outdoor Air Economizer Type                               ",
//"    ,                        !- Heat Recovery Type                                        ",
//"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}      ",
//"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}        ",
//"                                                                                          ",
//"  Output:VariableDictionary,Regular;                                                      ",
//"                                                                                          ",
//"  Output:Surfaces:Drawing,DXF;                                                            ",
//"                                                                                          ",
//"  Output:Constructions,Constructions;                                                     ",
//"                                                                                          ",
//"  Output:Surfaces:List,Detailswithvertices;                                               ",
//"                                                                                          ",
//"  OutputControl:Table:Style,                                                              ",
//"    HTML;                    !- Column Separator                                          ",
//"                                                                                          ",
//"  Output:Table:SummaryReports,                                                            ",
//"    AllSummary;              !- Report 1 Name                                             ",
//"                                                                                          ",
//"  Output:SQLite,                                                                          ",
//"    SimpleAndTabular;        !- Option Type                                               ",
//} );

// ASSERT_TRUE( process_idf( idf_objects ) );

// OutputProcessor::TimeValue.allocate(2);
////state->dataGlobal->DDOnlySimulation = true;

// ManageSimulation();
////compare_err_stream( "" );

// EXPECT_EQ( "2", RetrievePreDefTableEntry(*state,  pdchSurfCntTot, "Tubular Daylighting Device Diffuser" ) );
// EXPECT_EQ( "2", RetrievePreDefTableEntry(*state,  pdchSurfCntExt, "Tubular Daylighting Device Diffuser" ) );

//}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_PredefinedTableRowMatchingTest)
{

    SetPredefinedTables(*state);

    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchLeedPerfElEneUse, "Exterior Lighting", 1000., 2);
    EXPECT_EQ("1000.00", RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchLeedPerfElEneUse, "Exterior Lighting"));
    EXPECT_EQ("NOT FOUND", RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchLeedPerfElEneUse, "EXTERIOR LIGHTING"));

    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchLeedPerfElEneUse, "EXTERIOR LIGHTING", 2000., 2);
    EXPECT_EQ("1000.00", RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchLeedPerfElEneUse, "Exterior Lighting"));
    EXPECT_EQ("2000.00", RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchLeedPerfElEneUse, "EXTERIOR LIGHTING"));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_GetUnitSubstring_Test)
{
    EXPECT_EQ("", GetUnitSubString(""));
    EXPECT_EQ("", GetUnitSubString(" "));
    EXPECT_EQ("", GetUnitSubString("String with no units"));
    EXPECT_EQ("feet", GetUnitSubString("[feet]"));
    EXPECT_EQ("meters", GetUnitSubString("String with  unit string at end [meters]"));
    EXPECT_EQ("newtons", GetUnitSubString("[newtons] String with unit string at beginning"));
    EXPECT_EQ("m", GetUnitSubString("String with unit string at end [m]"));
    EXPECT_EQ("N", GetUnitSubString("[N] String with unit string at beginning"));
    EXPECT_EQ("", GetUnitSubString("[]"));
    EXPECT_EQ("", GetUnitSubString("String with empty unit string at end []"));
    EXPECT_EQ("", GetUnitSubString("[] String with empty unit string at beginning"));
}

TEST_F(SQLiteFixture, WriteVeriSumTableAreasTest)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->displayTabularVeriSum = true;
    state->dataEnvrn->Latitude = 12.3;
    state->dataEnvrn->Longitude = 45.6;

    state->dataSurface->TotSurfaces = 4;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    // walls
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(1).Azimuth = 0.;
    state->dataSurface->Surface(1).GrossArea = 200.; // 20 x 10
    state->dataSurface->Surface(1).FrameDivider = 0;
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Zone = 1;

    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(2).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(2).Azimuth = 90.;
    state->dataSurface->Surface(2).GrossArea = 300.; // 30 x 10
    state->dataSurface->Surface(2).FrameDivider = 0;
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Zone = 1;

    // windows
    state->dataSurface->Surface(3).Class = SurfaceClass::Window;
    state->dataSurface->Surface(3).HeatTransSurf = true;
    state->dataSurface->Surface(3).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(3).Azimuth = 0.;
    state->dataSurface->Surface(3).GrossArea = 40.;
    state->dataSurface->Surface(3).Height = 5;
    state->dataSurface->Surface(3).Width = 8;
    state->dataSurface->Surface(3).FrameDivider = 1;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Zone = 1;

    state->dataSurface->Surface(4).Class = SurfaceClass::Window;
    state->dataSurface->Surface(4).HeatTransSurf = true;
    state->dataSurface->Surface(4).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(4).Azimuth = 90.;
    state->dataSurface->Surface(4).GrossArea = 60.;
    state->dataSurface->Surface(4).Height = 6;
    state->dataSurface->Surface(4).Width = 10;
    state->dataSurface->Surface(4).FrameDivider = 2;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Zone = 1;

    // frames
    state->dataHeatBal->TotFrameDivider = 2;
    state->dataSurface->FrameDivider.allocate(state->dataHeatBal->TotFrameDivider);
    state->dataSurface->FrameDivider(1).FrameWidth = 0.3;
    state->dataSurface->FrameDivider(2).FrameWidth = 0.2;

    // zone
    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 1;
    state->dataHeatBal->Zone(1).Multiplier = 1.;
    state->dataHeatBal->Zone(1).ListMultiplier = 1.;
    state->dataHeatBal->Zone(1).FloorArea = 600.; // 20 x 30
    state->dataHeatBal->Zone(1).Volume = 6000.;   // 20 x 30 x 10
    state->dataHeatBal->Zone(1).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(1).ExtGrossWallArea = 500.;
    state->dataHeatBal->Zone(1).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(1).ExtWindowArea = state->dataSurface->Surface(3).GrossArea + state->dataSurface->Surface(4).GrossArea;

    WriteVeriSumTable(*state);

    auto tabularData = queryResult("SELECT * FROM TabularData;", "TabularData");
    auto strings = queryResult("SELECT * FROM Strings;", "Strings");
    auto stringTypes = queryResult("SELECT * FROM StringTypes;", "StringTypes");
    state->dataSQLiteProcedures->sqlite->sqliteCommit();

    EXPECT_EQ(123ul, tabularData.size());
    // tabularDataIndex, reportNameIndex, reportForStringIndex, tableNameIndex, rowLabelIndex, columnLabelIndex, unitsIndex, simulationIndex, rowId,
    // columnId, value
    EXPECT_EQ("       12.30", tabularData[3][10]);
    EXPECT_EQ("       45.60", tabularData[4][10]);
    // envelope - window-wall ratio subtable
    // north
    EXPECT_EQ("      200.00", tabularData[15][10]);
    EXPECT_EQ("      200.00", tabularData[16][10]);
    EXPECT_EQ("       48.16", tabularData[17][10]);
    EXPECT_EQ("       24.08", tabularData[18][10]);
    EXPECT_EQ("       24.08", tabularData[19][10]);
    // east
    EXPECT_EQ("      300.00", tabularData[20][10]);
    EXPECT_EQ("      300.00", tabularData[21][10]);
    EXPECT_EQ("       66.56", tabularData[22][10]);
    EXPECT_EQ("       22.19", tabularData[23][10]);
    EXPECT_EQ("       22.19", tabularData[24][10]);
    // Performance - zone summary table
    EXPECT_EQ("      600.00", tabularData[63][10]);  // area
    EXPECT_EQ("Yes", tabularData[68][10]);           // conditioned
    EXPECT_EQ("Yes", tabularData[73][10]);           // part of total floor area
    EXPECT_EQ("     6000.00", tabularData[78][10]);  // volume
    EXPECT_EQ("        1.00", tabularData[83][10]);  // multiplier
    EXPECT_EQ("      500.00", tabularData[88][10]);  // above ground gross floor area
    EXPECT_EQ("      100.00", tabularData[98][10]);  // window glass area
    EXPECT_EQ("      114.72", tabularData[103][10]); // window opening area
}

// Test for #6350 and #6469
TEST_F(SQLiteFixture, WriteVeriSumTable_TestNotPartOfTotal)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->displayTabularVeriSum = true;
    state->dataEnvrn->Latitude = 12.3;
    state->dataEnvrn->Longitude = 45.6;

    state->dataSurface->TotSurfaces = 4;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    // walls
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(1).Azimuth = 0.;
    state->dataSurface->Surface(1).GrossArea = 200.; // 20 x 10
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Zone = 1;

    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(2).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(2).Azimuth = 90.;
    state->dataSurface->Surface(2).GrossArea = 300.; // 30 x 10
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Zone = 2;

    // windows
    state->dataSurface->Surface(3).Class = SurfaceClass::Window;
    state->dataSurface->Surface(3).HeatTransSurf = true;
    state->dataSurface->Surface(3).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(3).Azimuth = 0.;
    state->dataSurface->Surface(3).GrossArea = 40.;
    state->dataSurface->Surface(3).Height = 5;
    state->dataSurface->Surface(3).Width = 8;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Zone = 1;

    state->dataSurface->Surface(4).Class = SurfaceClass::Window;
    state->dataSurface->Surface(4).HeatTransSurf = true;
    state->dataSurface->Surface(4).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(4).Azimuth = 90.;
    state->dataSurface->Surface(4).GrossArea = 60.;
    state->dataSurface->Surface(4).Height = 6;
    state->dataSurface->Surface(4).Width = 10;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Zone = 2;

    // Loads
    state->dataHeatBal->TotLights = 3;
    state->dataHeatBal->Lights.allocate(state->dataHeatBal->TotLights);

    state->dataHeatBal->TotPeople = 3;
    state->dataHeatBal->People.allocate(state->dataHeatBal->TotPeople);

    state->dataHeatBal->TotElecEquip = 3;
    state->dataHeatBal->ZoneElectric.allocate(state->dataHeatBal->TotElecEquip);

    state->dataHeatBal->Lights(1).ZonePtr = 1;
    state->dataHeatBal->Lights(1).DesignLevel = 1000.0;
    state->dataHeatBal->Lights(2).ZonePtr = 2;
    state->dataHeatBal->Lights(2).DesignLevel = 100.0;
    state->dataHeatBal->Lights(3).ZonePtr = 3;
    state->dataHeatBal->Lights(3).DesignLevel = 10.0;

    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->People(1).NumberOfPeople = 10.0;
    state->dataHeatBal->People(2).ZonePtr = 2;
    state->dataHeatBal->People(2).NumberOfPeople = 5.0;
    state->dataHeatBal->People(3).ZonePtr = 3;
    state->dataHeatBal->People(3).NumberOfPeople = 1.0;

    state->dataHeatBal->ZoneElectric(1).ZonePtr = 1;
    state->dataHeatBal->ZoneElectric(1).DesignLevel = 500.0;
    state->dataHeatBal->ZoneElectric(2).ZonePtr = 2;
    state->dataHeatBal->ZoneElectric(2).DesignLevel = 50.0;
    state->dataHeatBal->ZoneElectric(3).ZonePtr = 3;
    state->dataHeatBal->ZoneElectric(3).DesignLevel = 5.0;

    // zone
    state->dataGlobal->NumOfZones = 3;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "PartofTot Conditioned Zone";
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 1; // Conditioned
    state->dataHeatBal->Zone(1).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(1).Multiplier = 1.;
    state->dataHeatBal->Zone(1).ListMultiplier = 1.;
    // 10x10x2
    state->dataHeatBal->Zone(1).FloorArea = 1000.;
    state->dataHeatBal->Zone(1).Volume = 2000.;
    state->dataHeatBal->Zone(1).ExtGrossWallArea = 800.;
    state->dataHeatBal->Zone(1).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(1).ExtWindowArea = state->dataSurface->Surface(3).GrossArea + state->dataSurface->Surface(4).GrossArea;

    state->dataHeatBal->Zone(2).Name = "PartofTot Unconditioned Zone";
    state->dataHeatBal->Zone(2).SystemZoneNodeNumber = 0; // Unconditioned
    state->dataHeatBal->Zone(2).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(2).Multiplier = 1.;
    state->dataHeatBal->Zone(2).ListMultiplier = 1.;
    // 10x10x2
    state->dataHeatBal->Zone(2).FloorArea = 100.;
    state->dataHeatBal->Zone(2).Volume = 200.;
    state->dataHeatBal->Zone(2).ExtGrossWallArea = 80.;
    state->dataHeatBal->Zone(2).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(2).ExtWindowArea = 0.0;

    state->dataHeatBal->Zone(3).Name = "NOT PartofTot Conditioned Zone";
    state->dataHeatBal->Zone(3).SystemZoneNodeNumber = 1; // Conditioned
    state->dataHeatBal->Zone(3).isPartOfTotalArea = false;
    state->dataHeatBal->Zone(3).Multiplier = 1.;
    state->dataHeatBal->Zone(3).ListMultiplier = 1.;
    // 10x10x2
    state->dataHeatBal->Zone(3).FloorArea = 10.;
    state->dataHeatBal->Zone(3).Volume = 20.;
    state->dataHeatBal->Zone(3).ExtGrossWallArea = 8.;
    state->dataHeatBal->Zone(3).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(3).ExtWindowArea = 0.0;

    WriteVeriSumTable(*state);

    /***********************************************************************************************************************************************
     *                                                              Check Yes/No flag                                                              *
     ***********************************************************************************************************************************************/

    // RowName, ColumnName, value
    std::vector<std::tuple<std::string, std::string, std::string>> results_strings({
        {state->dataHeatBal->Zone(1).Name, "Conditioned (Y/N)", "Yes"},
        {state->dataHeatBal->Zone(1).Name, "Part of Total Floor Area (Y/N)", "Yes"},

        {state->dataHeatBal->Zone(2).Name, "Conditioned (Y/N)", "No"},
        {state->dataHeatBal->Zone(2).Name, "Part of Total Floor Area (Y/N)", "Yes"},

        {state->dataHeatBal->Zone(3).Name, "Conditioned (Y/N)", "Yes"},
        {state->dataHeatBal->Zone(3).Name, "Part of Total Floor Area (Y/N)", "No"},
    });

    // Would have used bind_text in sqlite3 with a single prepared statement, but m_db is protected in SQLiteProcedures
    std::string rowName;
    std::string columnName;

    for (auto v : results_strings) {

        rowName = std::get<0>(v);
        columnName = std::get<1>(v);

        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE ReportName = 'InputVerificationandResultsSummary'"
                          "  AND TableName = 'Zone Summary'"
                          "  AND RowName = '" +
                          rowName + "'" + "  AND ColumnName = '" + columnName + "'");

        std::string flag = queryResult(query, "TabularDataWithStrings")[0][0];
        // Not needed, we're just querying, not inside a transaction
        // state->dataSQLiteProcedures->sqlite->sqliteCommit();

        // Add informative message if failed
        EXPECT_EQ(std::get<2>(v), flag) << "Failed for RowName=" << rowName << "; ColumnName=" << columnName;
    }

    /***********************************************************************************************************************************************
     *                                                       Check each zone and total rows                                                        *
     ***********************************************************************************************************************************************/

    // RowName, ColumnName, value
    std::vector<std::tuple<std::string, std::string, Real64>> results({
        {state->dataHeatBal->Zone(1).Name, "Area", state->dataHeatBal->Zone(1).FloorArea},
        {state->dataHeatBal->Zone(2).Name, "Area", state->dataHeatBal->Zone(2).FloorArea},
        {state->dataHeatBal->Zone(3).Name, "Area", state->dataHeatBal->Zone(3).FloorArea},
        {"Total", "Area", state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea},
        {"Conditioned Total", "Area", state->dataHeatBal->Zone(1).FloorArea},
        {"Unconditioned Total", "Area", state->dataHeatBal->Zone(2).FloorArea},
        {"Not Part of Total", "Area", state->dataHeatBal->Zone(3).FloorArea},

        {state->dataHeatBal->Zone(1).Name, "Volume", state->dataHeatBal->Zone(1).Volume},
        {state->dataHeatBal->Zone(2).Name, "Volume", state->dataHeatBal->Zone(2).Volume},
        {state->dataHeatBal->Zone(3).Name, "Volume", state->dataHeatBal->Zone(3).Volume},
        {"Total", "Volume", state->dataHeatBal->Zone(1).Volume + state->dataHeatBal->Zone(2).Volume},
        {"Conditioned Total", "Volume", state->dataHeatBal->Zone(1).Volume},
        {"Unconditioned Total", "Volume", state->dataHeatBal->Zone(2).Volume},
        {"Not Part of Total", "Volume", state->dataHeatBal->Zone(3).Volume},

        {state->dataHeatBal->Zone(1).Name, "Lighting", state->dataHeatBal->Lights(1).DesignLevel / state->dataHeatBal->Zone(1).FloorArea},
        {state->dataHeatBal->Zone(2).Name, "Lighting", state->dataHeatBal->Lights(2).DesignLevel / state->dataHeatBal->Zone(2).FloorArea},
        {state->dataHeatBal->Zone(3).Name, "Lighting", state->dataHeatBal->Lights(3).DesignLevel / state->dataHeatBal->Zone(3).FloorArea},
        {"Total",
         "Lighting",
         (state->dataHeatBal->Lights(1).DesignLevel + state->dataHeatBal->Lights(2).DesignLevel) /
             (state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea)},
        {"Conditioned Total", "Lighting", state->dataHeatBal->Lights(1).DesignLevel / state->dataHeatBal->Zone(1).FloorArea},
        {"Unconditioned Total", "Lighting", state->dataHeatBal->Lights(2).DesignLevel / state->dataHeatBal->Zone(2).FloorArea},
        {"Not Part of Total", "Lighting", state->dataHeatBal->Lights(3).DesignLevel / state->dataHeatBal->Zone(3).FloorArea},

        // People/m^2
        {state->dataHeatBal->Zone(1).Name, "People", state->dataHeatBal->Zone(1).FloorArea / state->dataHeatBal->People(1).NumberOfPeople},
        {state->dataHeatBal->Zone(2).Name, "People", state->dataHeatBal->Zone(2).FloorArea / state->dataHeatBal->People(2).NumberOfPeople},
        {state->dataHeatBal->Zone(3).Name, "People", state->dataHeatBal->Zone(3).FloorArea / state->dataHeatBal->People(3).NumberOfPeople},
        {"Total",
         "People",
         (state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea) /
             (state->dataHeatBal->People(1).NumberOfPeople + state->dataHeatBal->People(2).NumberOfPeople)},
        {"Conditioned Total", "People", state->dataHeatBal->Zone(1).FloorArea / state->dataHeatBal->People(1).NumberOfPeople},
        {"Unconditioned Total", "People", state->dataHeatBal->Zone(2).FloorArea / state->dataHeatBal->People(2).NumberOfPeople},
        {"Not Part of Total", "People", state->dataHeatBal->Zone(3).FloorArea / state->dataHeatBal->People(3).NumberOfPeople},

        {state->dataHeatBal->Zone(1).Name,
         "Plug and Process",
         state->dataHeatBal->ZoneElectric(1).DesignLevel / state->dataHeatBal->Zone(1).FloorArea},
        {state->dataHeatBal->Zone(2).Name,
         "Plug and Process",
         state->dataHeatBal->ZoneElectric(2).DesignLevel / state->dataHeatBal->Zone(2).FloorArea},
        {state->dataHeatBal->Zone(3).Name,
         "Plug and Process",
         state->dataHeatBal->ZoneElectric(3).DesignLevel / state->dataHeatBal->Zone(3).FloorArea},
        {"Total",
         "Plug and Process",
         (state->dataHeatBal->ZoneElectric(1).DesignLevel + state->dataHeatBal->ZoneElectric(2).DesignLevel) /
             (state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea)},
        {"Conditioned Total", "Plug and Process", state->dataHeatBal->ZoneElectric(1).DesignLevel / state->dataHeatBal->Zone(1).FloorArea},
        {"Unconditioned Total", "Plug and Process", state->dataHeatBal->ZoneElectric(2).DesignLevel / state->dataHeatBal->Zone(2).FloorArea},
        {"Not Part of Total", "Plug and Process", state->dataHeatBal->ZoneElectric(3).DesignLevel / state->dataHeatBal->Zone(3).FloorArea},
    });

    // Would have used bind_text in sqlite3 with a single prepared statement, but m_db is protected in SQLiteProcedures
    for (auto v : results) {

        rowName = std::get<0>(v);
        columnName = std::get<1>(v);

        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE ReportName = 'InputVerificationandResultsSummary'"
                          "  AND TableName = 'Zone Summary'"
                          "  AND RowName = '" +
                          rowName + "'" + "  AND ColumnName = '" + columnName + "'");

        Real64 return_val = execAndReturnFirstDouble(query);
        // state->dataSQLiteProcedures->sqlite->sqliteCommit();

        // Add informative message if failed
        EXPECT_NEAR(std::get<2>(v), return_val, 0.01) << "Failed for RowName=" << rowName << "; ColumnName=" << columnName;
    }
}

TEST_F(EnergyPlusFixture, OutputReportTabularMonthly_invalidAggregationOrder)
{
    std::string const idf_objects = delimited_string({
        "Output:Table:Monthly,",
        "Space Gains Annual Report, !- Name",
        "2, !-  Digits After Decimal",
        "Exterior Lights Electricity Energy, !- Variable or Meter 1 Name",
        "SumOrAverageDuringHoursShown, !- Aggregation Type for Variable or Meter 1",
        "Exterior Lights Electricity Rate, !- Variable or Meter 2 Name",
        "Maximum, !- Aggregation Type for Variable or Meter 2",
        "Exterior Lights Electricity Rate, !- Variable or Meter 2 Name",
        "Minimum; !- Aggregation Type for Variable or Meter 2",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    Real64 extLitUse;

    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite1",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite2",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite3",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");

    state->dataGlobal->DoWeathSim = true;
    state->dataGlobal->TimeStepZone = 0.25;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * 60.0;

    GetInputTabularMonthly(*state);
    EXPECT_EQ(state->dataOutRptTab->MonthlyInputCount, 1);
    InitializeTabularMonthly(*state);

    EXPECT_TRUE(isInvalidAggregationOrder(*state));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_CollectPeakZoneConditions_test)
{
    Psychrometrics::InitializePsychRoutines(*state);
    createCoilSelectionReportObj(*state);

    CompLoadTablesType compLoad;
    int timeOfMax = 63;
    int zoneIndex = 1;
    bool isCooling = true;

    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Multiplier = 1;
    state->dataHeatBal->Zone(1).ListMultiplier = 1;
    state->dataHeatBal->Zone(1).FloorArea = 12.;

    state->dataWeatherManager->DesDayInput.allocate(1);
    state->dataWeatherManager->DesDayInput(1).Month = 5;
    state->dataWeatherManager->DesDayInput(1).DayOfMonth = 21;

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;

    state->dataSize->CoolPeakDateHrMin.allocate(1);

    state->dataSize->CalcFinalZoneSizing.allocate(1);
    state->dataSize->CalcFinalZoneSizing(1).CoolOutTempSeq.allocate(96);
    state->dataSize->CalcFinalZoneSizing(1).CoolOutTempSeq(63) = 38.;
    state->dataSize->CalcFinalZoneSizing(1).CoolOutHumRatSeq.allocate(96);
    state->dataSize->CalcFinalZoneSizing(1).CoolOutHumRatSeq(63) = 0.01459;
    state->dataSize->CalcFinalZoneSizing(1).CoolZoneTempSeq.allocate(96);
    state->dataSize->CalcFinalZoneSizing(1).CoolZoneTempSeq(63) = 24.;
    state->dataSize->CalcFinalZoneSizing(1).CoolZoneHumRatSeq.allocate(96);
    state->dataSize->CalcFinalZoneSizing(1).CoolZoneHumRatSeq(63) = 0.00979;
    state->dataSize->CalcFinalZoneSizing(1).DesCoolLoad = 500.;
    state->dataSize->CalcFinalZoneSizing(1).ZnCoolDgnSAMethod = SupplyAirTemperature;
    state->dataSize->CalcFinalZoneSizing(1).CoolDesTemp = 13.;
    state->dataSize->CalcFinalZoneSizing(1).DesCoolVolFlow = 3.3;

    state->dataSize->FinalZoneSizing.allocate(1);
    state->dataSize->FinalZoneSizing(1).DesCoolLoad = 600.;

    CollectPeakZoneConditions(*state, compLoad, 1, timeOfMax, zoneIndex, isCooling);

    EXPECT_EQ(compLoad.peakDateHrMin, "5/21 15:45:00");
    EXPECT_EQ(compLoad.outsideDryBulb, 38.);
    EXPECT_EQ(compLoad.outsideHumRatio, 0.01459);
    EXPECT_EQ(compLoad.zoneDryBulb, 24.);
    EXPECT_EQ(compLoad.zoneHumRatio, 0.00979);
    EXPECT_EQ(compLoad.peakDesSensLoad, 500.);
    EXPECT_EQ(compLoad.designPeakLoad, 600.);
    EXPECT_EQ(compLoad.supAirTemp, 13.);
    EXPECT_EQ(compLoad.mainFanAirFlow, 3.3);
    EXPECT_NEAR(compLoad.airflowPerFlrArea, 3.3 / 12., 0.0001);
    EXPECT_NEAR(compLoad.totCapPerArea, 600. / 12., 0.0001);
    EXPECT_NEAR(compLoad.airflowPerTotCap, 3.3 / 600., 0.0001);
    EXPECT_NEAR(compLoad.areaPerTotCap, 12. / 600., 0.0001);
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_ComputeEngineeringChecks_test)
{
    CompLoadTablesType compLoad;

    compLoad.outsideAirFlow = 0.12;
    compLoad.mainFanAirFlow = 0.50;
    compLoad.floorArea = 13.;
    compLoad.designPeakLoad = 800;

    ComputeEngineeringChecks(compLoad);

    EXPECT_EQ(compLoad.outsideAirRatio, 0.12 / 0.50);
    EXPECT_EQ(compLoad.airflowPerFlrArea, 0.50 / 13.);
    EXPECT_EQ(compLoad.totCapPerArea, 800. / 13.);
    EXPECT_EQ(compLoad.airflowPerTotCap, 0.50 / 800.);
    EXPECT_EQ(compLoad.areaPerTotCap, 13. / 800.);
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_GetZoneComponentAreas_test)
{
    Array1D<ZompComponentAreasType> areas;
    areas.allocate(1);

    state->dataHeatBal->Zone.allocate(1);
    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone(1).FloorArea = 12.;

    state->dataSurface->Surface.allocate(13);

    state->dataSurface->Surface(1).GrossArea = 5.; // extWall
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(1).Zone = 1;
    state->dataSurface->Surface(1).HeatTransSurf = true;

    state->dataSurface->Surface(2).GrossArea = 6.; // grdCntWall
    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).ExtBoundCond = GroundFCfactorMethod;
    state->dataSurface->Surface(2).Zone = 1;
    state->dataSurface->Surface(2).HeatTransSurf = true;

    state->dataSurface->Surface(3).GrossArea = 7.; // intZoneWall
    state->dataSurface->Surface(3).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).ExtBoundCond = 2;
    state->dataSurface->Surface(3).Zone = 1;
    state->dataSurface->Surface(3).HeatTransSurf = true;

    state->dataSurface->Surface(4).GrossArea = 8.; // roof
    state->dataSurface->Surface(4).Class = SurfaceClass::Roof;
    state->dataSurface->Surface(4).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(4).Zone = 1;
    state->dataSurface->Surface(4).HeatTransSurf = true;

    state->dataSurface->Surface(5).GrossArea = 9.; // ceiling
    state->dataSurface->Surface(5).Class = SurfaceClass::Roof;
    state->dataSurface->Surface(5).ExtBoundCond = 5;
    state->dataSurface->Surface(5).Zone = 1;
    state->dataSurface->Surface(5).HeatTransSurf = true;

    state->dataSurface->Surface(6).GrossArea = 10.; // extFloor
    state->dataSurface->Surface(6).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(6).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(6).Zone = 1;
    state->dataSurface->Surface(6).HeatTransSurf = true;

    state->dataSurface->Surface(7).GrossArea = 11.; // grndCntFloor
    state->dataSurface->Surface(7).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(7).ExtBoundCond = Ground;
    state->dataSurface->Surface(7).Zone = 1;
    state->dataSurface->Surface(7).HeatTransSurf = true;

    state->dataSurface->Surface(8).GrossArea = 12.; // intZoneFloor
    state->dataSurface->Surface(8).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(8).ExtBoundCond = 3;
    state->dataSurface->Surface(8).Zone = 1;
    state->dataSurface->Surface(8).HeatTransSurf = true;

    state->dataSurface->Surface(9).GrossArea = 13.; // fenestration
    state->dataSurface->Surface(9).Class = SurfaceClass::Window;
    state->dataSurface->Surface(9).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(9).Zone = 1;
    state->dataSurface->Surface(9).HeatTransSurf = true;

    state->dataSurface->Surface(10).GrossArea = 14.; // door
    state->dataSurface->Surface(10).Class = SurfaceClass::Door;
    state->dataSurface->Surface(10).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(10).Zone = 1;
    state->dataSurface->Surface(10).HeatTransSurf = true;

    state->dataSurface->Surface(11).GrossArea = 15.; // door (again)
    state->dataSurface->Surface(11).Class = SurfaceClass::GlassDoor;
    state->dataSurface->Surface(11).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(11).Zone = 1;
    state->dataSurface->Surface(11).HeatTransSurf = true;

    state->dataSurface->Surface(12).GrossArea = 16.; // fenestration (again)
    state->dataSurface->Surface(12).Class = SurfaceClass::TDD_Dome;
    state->dataSurface->Surface(12).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(12).Zone = 1;
    state->dataSurface->Surface(12).HeatTransSurf = true;

    state->dataSurface->Surface(13).GrossArea = 17.; // grndCntFloor (again)
    state->dataSurface->Surface(13).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(13).ExtBoundCond = KivaFoundation;
    state->dataSurface->Surface(13).Zone = 1;
    state->dataSurface->Surface(13).HeatTransSurf = true;

    GetZoneComponentAreas(*state, areas);

    EXPECT_EQ(12., areas(1).floor);
    EXPECT_EQ(8., areas(1).roof);
    EXPECT_EQ(9., areas(1).ceiling);
    EXPECT_EQ(5., areas(1).extWall);
    EXPECT_EQ(7., areas(1).intZoneWall);
    EXPECT_EQ(6., areas(1).grndCntWall);
    EXPECT_EQ(10., areas(1).extFloor);
    EXPECT_EQ(12., areas(1).intZoneFloor);
    EXPECT_EQ(28., areas(1).grndCntFloor);
    EXPECT_EQ(29., areas(1).fenestration);
    EXPECT_EQ(29., areas(1).door);
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_CombineLoadCompResults_test)
{
    CompLoadTablesType compLoadTotal;
    // printf("3");
    compLoadTotal.cells.allocate(10, 30);
    compLoadTotal.cells = 0.;
    compLoadTotal.cellUsed.allocate(10, 30);
    compLoadTotal.cellUsed = false;

    // printf("4");
    CompLoadTablesType compLoadPartial;
    compLoadPartial.cells.allocate(10, 30);
    compLoadPartial.cells = 0.;
    compLoadPartial.cellUsed.allocate(10, 30);
    compLoadPartial.cellUsed = false;

    Real64 multiplier = 3.;

    compLoadPartial.cells(1, 1) = 1.1;
    compLoadPartial.cells(4, 25) = 1.2;
    compLoadPartial.cellUsed(3, 17) = true;
    compLoadPartial.outsideWetBulb = 17.;
    compLoadPartial.diffDesignPeak = 11.;

    // printf("5");
    CombineLoadCompResults(compLoadTotal, compLoadPartial, multiplier);
    // printf("6");

    EXPECT_EQ(1.1 * 3., compLoadTotal.cells(1, 1));
    EXPECT_EQ(1.2 * 3., compLoadTotal.cells(4, 25));
    EXPECT_EQ(true, compLoadTotal.cellUsed(3, 17));
    EXPECT_EQ(17., compLoadTotal.outsideWetBulb);
    EXPECT_EQ(33., compLoadTotal.diffDesignPeak);
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_AddTotalRowsForLoadSummary_test)
{
    CompLoadTablesType compLoad;
    compLoad.cells.allocate(cPerArea, rGrdTot);
    compLoad.cells = 0.;
    compLoad.cellUsed.allocate(cPerArea, rGrdTot);
    compLoad.cellUsed = true;

    compLoad.cells(cSensInst, rLights) = 3.;
    compLoad.cells(cSensInst, rRefrig) = 4.;
    compLoad.cells(cLatent, rLights) = 10.;
    compLoad.cells(cLatent, rRefrig) = 20.;

    compLoad.cells(cArea, rLights) = 5.;
    compLoad.cells(cArea, rRefrig) = 5.;

    AddTotalRowsForLoadSummary(compLoad);

    EXPECT_EQ(3. + 4., compLoad.cells(cSensInst, rGrdTot));
    EXPECT_EQ(10 + 20., compLoad.cells(cLatent, rGrdTot));
    EXPECT_EQ(3. + 10., compLoad.cells(cTotal, rLights));
    EXPECT_EQ(4 + 20., compLoad.cells(cTotal, rRefrig));

    EXPECT_EQ(37., compLoad.cells(cTotal, rGrdTot));

    EXPECT_EQ(100. * 13. / 37., compLoad.cells(cPerc, rLights));
    EXPECT_EQ(100. * 24. / 37., compLoad.cells(cPerc, rRefrig));

    EXPECT_EQ(13. / 5., compLoad.cells(cPerArea, rLights));
    EXPECT_EQ(24. / 5., compLoad.cells(cPerArea, rRefrig));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_LoadSummaryUnitConversion_test)
{
    CompLoadTablesType compLoad;
    compLoad.cells.allocate(cPerArea, rGrdTot);
    compLoad.cells = 0.;
    compLoad.cellUsed.allocate(cPerArea, rGrdTot);
    compLoad.cellUsed = true;

    compLoad.cells(cSensInst, rLights) = 3.;
    compLoad.cells(cLatent, rLights) = 10.;

    compLoad.cells(cArea, rLights) = 5.;

    compLoad.outsideDryBulb = 20.;
    compLoad.mainFanAirFlow = 0.7;
    compLoad.airflowPerTotCap = 0.2;
    compLoad.totCapPerArea = 0.15;

    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::InchPound;
    Real64 powerConversion = getSpecificUnitMultiplier(*state, "W", "Btu/h");
    Real64 areaConversion = getSpecificUnitMultiplier(*state, "m2", "ft2");
    Real64 airFlowConversion = getSpecificUnitMultiplier(*state, "m3/s", "ft3/min");
    Real64 airFlowPerAreaConversion = getSpecificUnitMultiplier(*state, "m3/s-m2", "ft3/min-ft2");
    int tempConvIndx = getSpecificUnitIndex(*state, "C", "F");

    LoadSummaryUnitConversion(*state, compLoad);

    EXPECT_EQ(3. * powerConversion, compLoad.cells(cSensInst, rLights));
    EXPECT_EQ(10. * powerConversion, compLoad.cells(cLatent, rLights));
    EXPECT_EQ(5. * areaConversion, compLoad.cells(cArea, rLights));
    EXPECT_EQ(5. * areaConversion, compLoad.cells(cArea, rLights));

    EXPECT_EQ(ConvertIP(*state, tempConvIndx, 20.), compLoad.outsideDryBulb);
    EXPECT_EQ(0.7 * airFlowConversion, compLoad.mainFanAirFlow);
    EXPECT_EQ(0.2 * airFlowPerAreaConversion / powerConversion, compLoad.airflowPerTotCap);
    EXPECT_EQ(0.15 * powerConversion / areaConversion, compLoad.totCapPerArea);
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_CreateListOfZonesForAirLoop_test)
{
    CompLoadTablesType compLoad;
    Array1D_int zoneToAirLoop;

    state->dataGlobal->NumOfZones = 15;
    compLoad.zoneIndices.allocate(state->dataGlobal->NumOfZones);
    compLoad.zoneIndices = 0;

    zoneToAirLoop.allocate(state->dataGlobal->NumOfZones);
    zoneToAirLoop(1) = 3;
    zoneToAirLoop(2) = 2;
    zoneToAirLoop(3) = 1;
    zoneToAirLoop(4) = 1;
    zoneToAirLoop(5) = 2;
    zoneToAirLoop(6) = 3;
    zoneToAirLoop(7) = 1;
    zoneToAirLoop(8) = 1;
    zoneToAirLoop(9) = 2;
    zoneToAirLoop(10) = 2;
    zoneToAirLoop(11) = 1;
    zoneToAirLoop(12) = 1;
    zoneToAirLoop(13) = 3;
    zoneToAirLoop(14) = 3;
    zoneToAirLoop(15) = 1;

    CreateListOfZonesForAirLoop(*state, compLoad, zoneToAirLoop, 1);
    EXPECT_EQ(3, compLoad.zoneIndices(1));
    EXPECT_EQ(4, compLoad.zoneIndices(2));
    EXPECT_EQ(7, compLoad.zoneIndices(3));
    EXPECT_EQ(8, compLoad.zoneIndices(4));
    EXPECT_EQ(11, compLoad.zoneIndices(5));
    EXPECT_EQ(12, compLoad.zoneIndices(6));
    EXPECT_EQ(15, compLoad.zoneIndices(7));
    EXPECT_EQ(0, compLoad.zoneIndices(8));

    compLoad.zoneIndices = 0;
    CreateListOfZonesForAirLoop(*state, compLoad, zoneToAirLoop, 2);
    EXPECT_EQ(2, compLoad.zoneIndices(1));
    EXPECT_EQ(5, compLoad.zoneIndices(2));
    EXPECT_EQ(9, compLoad.zoneIndices(3));
    EXPECT_EQ(10, compLoad.zoneIndices(4));
    EXPECT_EQ(0, compLoad.zoneIndices(5));

    compLoad.zoneIndices = 0;
    CreateListOfZonesForAirLoop(*state, compLoad, zoneToAirLoop, 3);
    EXPECT_EQ(1, compLoad.zoneIndices(1));
    EXPECT_EQ(6, compLoad.zoneIndices(2));
    EXPECT_EQ(13, compLoad.zoneIndices(3));
    EXPECT_EQ(14, compLoad.zoneIndices(4));
    EXPECT_EQ(0, compLoad.zoneIndices(5));
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_GetDelaySequencesTwice_test)
{

    int coolDesSelected = 1;
    int iZone = 1;
    state->dataEnvrn->TotDesDays = 2;
    state->dataEnvrn->TotRunDesPersDays = 3;
    state->dataGlobal->NumOfTimeStepInHour = 4;

    state->dataGlobal->NumOfZones = 4;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);

    state->dataHeatBal->Zone(iZone).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(iZone).HTSurfaceLast = 1;
    state->dataHeatBal->Zone(iZone).RadiantEnclosureNum = 1;

    state->dataSurface->TotSurfaces = 4;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).Class = SurfaceClass::Window;

    Array1D<Real64> peopleDelaySeq;
    peopleDelaySeq.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    peopleDelaySeq = 0.;

    Array1D<Real64> peopleDelaySeqCool;
    peopleDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    peopleDelaySeqCool = 0.;

    Array1D<Real64> equipDelaySeqCool;
    equipDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    equipDelaySeqCool = 0.;

    Array1D<Real64> hvacLossDelaySeqCool;
    hvacLossDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    hvacLossDelaySeqCool = 0.;

    Array1D<Real64> powerGenDelaySeqCool;
    powerGenDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    powerGenDelaySeqCool = 0.;

    Array1D<Real64> lightDelaySeqCool;
    lightDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    lightDelaySeqCool = 0.;

    Array1D<Real64> feneSolarDelaySeqCool;
    feneSolarDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    feneSolarDelaySeqCool = 0.;

    Array3D<Real64> feneCondInstantSeq;
    feneCondInstantSeq.allocate(state->dataEnvrn->TotDesDays + state->dataEnvrn->TotRunDesPersDays,
                                state->dataGlobal->NumOfTimeStepInHour * 24,
                                state->dataGlobal->NumOfZones);
    feneCondInstantSeq = 0.0;

    Array2D<Real64> surfDelaySeqCool;
    surfDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24, state->dataSurface->TotSurfaces);
    surfDelaySeqCool = 0.0;

    AllocateLoadComponentArrays(*state);

    feneCondInstantSeq(coolDesSelected, 1, 1) = 0.88;

    state->dataOutRptTab->netSurfRadSeq(coolDesSelected, 1, 1) = 0.05;

    GetDelaySequences(*state,
                      coolDesSelected,
                      true,
                      iZone,
                      peopleDelaySeqCool,
                      equipDelaySeqCool,
                      hvacLossDelaySeqCool,
                      powerGenDelaySeqCool,
                      lightDelaySeqCool,
                      feneSolarDelaySeqCool,
                      feneCondInstantSeq,
                      surfDelaySeqCool);

    EXPECT_EQ(0.83, feneCondInstantSeq(coolDesSelected, 1, 1)); // the first time the subtraction operation should have occurred

    GetDelaySequences(*state,
                      coolDesSelected,
                      true,
                      iZone,
                      peopleDelaySeqCool,
                      equipDelaySeqCool,
                      hvacLossDelaySeqCool,
                      powerGenDelaySeqCool,
                      lightDelaySeqCool,
                      feneSolarDelaySeqCool,
                      feneCondInstantSeq,
                      surfDelaySeqCool);

    EXPECT_EQ(0.83,
              feneCondInstantSeq(
                  coolDesSelected,
                  1,
                  1)); // the second time the subtraction should not have happened since it is only adjusted once so the value should be the same.
}

TEST_F(SQLiteFixture, OutputReportTabular_WriteLoadComponentSummaryTables_AirLoop_ZeroDesignDay)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataHVACGlobal->NumPrimaryAirSys = 1;
    state->dataSize->SysSizPeakDDNum.allocate(state->dataHVACGlobal->NumPrimaryAirSys);
    state->dataSize->FinalSysSizing.allocate(state->dataHVACGlobal->NumPrimaryAirSys);
    state->dataSize->CalcSysSizing.allocate(state->dataHVACGlobal->NumPrimaryAirSys);
    int numDesDays = 2;
    state->dataAirLoop->AirToZoneNodeInfo.allocate(state->dataHVACGlobal->NumPrimaryAirSys);
    state->dataGlobal->NumOfZones = 0;
    state->dataOutRptTab->displayAirLoopComponentLoadSummary = true;
    state->dataGlobal->CompLoadReportIsReq = true;
    state->dataSize->SysSizPeakDDNum(state->dataHVACGlobal->NumPrimaryAirSys).TimeStepAtTotCoolPk.allocate(numDesDays);

    state->dataSize->SysSizPeakDDNum(state->dataHVACGlobal->NumPrimaryAirSys).TotCoolPeakDD = 0; // set to zero to indicate no design day chosen
    state->dataSize->SysSizPeakDDNum(state->dataHVACGlobal->NumPrimaryAirSys).HeatPeakDD = 0;    // set to zero to indicate no design day chosen

    WriteLoadComponentSummaryTables(*state);

    auto tabularData = queryResult("SELECT * FROM TabularData;", "TabularData");
    auto strings = queryResult("SELECT * FROM Strings;", "Strings");
    auto stringTypes = queryResult("SELECT * FROM StringTypes;", "StringTypes");
    state->dataSQLiteProcedures->sqlite->sqliteCommit();

    EXPECT_EQ(460ul, tabularData.size());
    EXPECT_EQ(77ul, strings.size());
    EXPECT_EQ("AirLoop Component Load Summary", strings[0][2]); // just make sure that the output table was generated and did not crash
}

// Test for https://github.com/NREL/EnergyPlus/issues/7346
// We ensure that if the Airloop peak matches the zone peak, we don't do the IP conversion twice
TEST_F(SQLiteFixture, OutputReportTabular_WriteLoadComponentSummaryTables_AirLoop_IPConversion)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    OutputReportTabular::SetupUnitConversions(*state);
    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::InchPound;
    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::InchPound;

    // We ask for the air loop component load summary since that's the one we test
    // We also ask for the zone component load summary because that's necessary to "copy" the load and trigger a potential double conversion
    state->dataOutRptTab->displayAirLoopComponentLoadSummary = true;
    state->dataOutRptTab->displayZoneComponentLoadSummary = true;
    state->dataOutRptTab->displayFacilityComponentLoadSummary = true;
    state->dataGlobal->CompLoadReportIsReq = true;

    Psychrometrics::InitializePsychRoutines(*state);
    createCoilSelectionReportObj(*state);

    // Two design days
    int numDesDays = 2;
    state->dataEnvrn->TotDesDays = numDesDays;
    state->dataEnvrn->TotRunDesPersDays = 0;
    state->dataWeatherManager->DesDayInput.allocate(2);
    state->dataWeatherManager->DesDayInput(1).Month = 7;
    state->dataWeatherManager->DesDayInput(1).DayOfMonth = 21;
    state->dataWeatherManager->DesDayInput(2).Month = 1;
    state->dataWeatherManager->DesDayInput(2).DayOfMonth = 21;

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;
    int numTimeStepInDay = 96;

    // One Zone
    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).Multiplier = 1;
    state->dataHeatBal->Zone(1).ListMultiplier = 1;
    state->dataHeatBal->Zone(1).FloorArea = 100.;
    // Trick E+ into not iterating on Surfaces
    state->dataHeatBal->Zone(1).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).HTSurfaceLast = 0;

    // Cool Peak on 1st DD at 16:00 and Heat Peak on 2nd DD at 1:00
    state->dataSize->CalcFinalZoneSizing.allocate(state->dataGlobal->NumOfZones);

    int coolDDNum = 1;
    int coolTimeOfMax = 64;
    state->dataSize->CalcFinalZoneSizing(1).CoolDDNum = coolDDNum;
    state->dataSize->CalcFinalZoneSizing(1).TimeStepNumAtCoolMax = coolTimeOfMax;
    state->dataSize->CoolPeakDateHrMin.allocate(1);

    state->dataSize->CalcFinalZoneSizing(1).CoolOutTempSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcFinalZoneSizing(1).CoolOutTempSeq(coolTimeOfMax) = 38.;
    state->dataSize->CalcFinalZoneSizing(1).CoolOutHumRatSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcFinalZoneSizing(1).CoolOutHumRatSeq(coolTimeOfMax) = 0.01459;
    state->dataSize->CalcFinalZoneSizing(1).CoolZoneTempSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcFinalZoneSizing(1).CoolZoneTempSeq(coolTimeOfMax) = 24.;
    state->dataSize->CalcFinalZoneSizing(1).CoolZoneHumRatSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcFinalZoneSizing(1).CoolZoneHumRatSeq(coolTimeOfMax) = 0.00979;
    state->dataSize->CalcFinalZoneSizing(1).DesCoolLoad = 500.;
    state->dataSize->CalcFinalZoneSizing(1).ZnCoolDgnSAMethod = SupplyAirTemperature;
    state->dataSize->CalcFinalZoneSizing(1).CoolDesTemp = 13.;
    state->dataSize->CalcFinalZoneSizing(1).DesCoolVolFlow = 3.3;

    int heatDDNum = 2;
    int heatTimeOfMax = 4;
    state->dataSize->CalcFinalZoneSizing(1).HeatDDNum = heatDDNum;
    state->dataSize->CalcFinalZoneSizing(1).TimeStepNumAtHeatMax = heatTimeOfMax;
    state->dataSize->HeatPeakDateHrMin.allocate(1);

    state->dataSize->CalcFinalZoneSizing(1).HeatOutTempSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcFinalZoneSizing(1).HeatOutTempSeq(heatTimeOfMax) = -17.4;
    state->dataSize->CalcFinalZoneSizing(1).HeatOutHumRatSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcFinalZoneSizing(1).HeatOutHumRatSeq(heatTimeOfMax) = 0.01459;

    state->dataSize->FinalZoneSizing.allocate(1);
    state->dataSize->FinalZoneSizing(1).DesCoolLoad = 600.;

    // One airloop, that serves this zone.
    state->dataHVACGlobal->NumPrimaryAirSys = 1;
    state->dataSize->SysSizPeakDDNum.allocate(state->dataHVACGlobal->NumPrimaryAirSys);
    state->dataSize->FinalSysSizing.allocate(state->dataHVACGlobal->NumPrimaryAirSys);
    state->dataSize->CalcSysSizing.allocate(state->dataHVACGlobal->NumPrimaryAirSys);

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).IsControlled = true;

    state->dataSize->CalcFinalZoneSizing(1).HeatZoneTempSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcFinalZoneSizing(1).HeatZoneTempSeq(heatTimeOfMax) = 20.;
    state->dataSize->CalcFinalZoneSizing(1).HeatZoneHumRatSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcFinalZoneSizing(1).HeatZoneHumRatSeq(heatTimeOfMax) = 0.00979;
    state->dataSize->CalcFinalZoneSizing(1).DesHeatLoad = 750.;
    state->dataSize->CalcFinalZoneSizing(1).ZnHeatDgnSAMethod = SupplyAirTemperature;
    state->dataSize->CalcFinalZoneSizing(1).HeatDesTemp = 35.;
    state->dataSize->CalcFinalZoneSizing(1).DesHeatVolFlow = 3.3;

    state->dataSize->CalcZoneSizing.allocate(numDesDays, state->dataGlobal->NumOfZones);
    state->dataSize->CalcZoneSizing(1, 1).DOASHeatAddSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcZoneSizing(1, 1).DOASLatAddSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcZoneSizing(2, 1).DOASHeatAddSeq.allocate(numTimeStepInDay);
    state->dataSize->CalcZoneSizing(2, 1).DOASLatAddSeq.allocate(numTimeStepInDay);

    state->dataAirLoop->AirToZoneNodeInfo.allocate(state->dataHVACGlobal->NumPrimaryAirSys);
    state->dataAirLoop->AirToZoneNodeInfo(1).NumZonesCooled = 1;
    state->dataAirLoop->AirToZoneNodeInfo(1).CoolCtrlZoneNums.allocate(1);
    state->dataAirLoop->AirToZoneNodeInfo(1).CoolCtrlZoneNums(1) = 1;
    state->dataAirLoop->AirToZoneNodeInfo(1).NumZonesHeated = 1;
    state->dataAirLoop->AirToZoneNodeInfo(1).HeatCtrlZoneNums.allocate(1);
    state->dataAirLoop->AirToZoneNodeInfo(1).HeatCtrlZoneNums(1) = 1;

    // same Design Days peak and timestep peak as the zone it serves. This is the critical part of the test
    state->dataSize->FinalSysSizing(1).CoolingPeakLoadType = DataSizing::TotalCoolingLoad;

    state->dataSize->SysSizPeakDDNum(state->dataHVACGlobal->NumPrimaryAirSys).TotCoolPeakDD = coolDDNum;
    state->dataSize->SysSizPeakDDNum(state->dataHVACGlobal->NumPrimaryAirSys).HeatPeakDD = heatDDNum;
    state->dataSize->SysSizPeakDDNum(state->dataHVACGlobal->NumPrimaryAirSys).TimeStepAtTotCoolPk.allocate(numDesDays);
    state->dataSize->SysSizPeakDDNum(state->dataHVACGlobal->NumPrimaryAirSys).TimeStepAtTotCoolPk(1) = coolTimeOfMax;
    state->dataSize->SysSizPeakDDNum(state->dataHVACGlobal->NumPrimaryAirSys).TimeStepAtHeatPk.allocate(numDesDays);
    state->dataSize->SysSizPeakDDNum(state->dataHVACGlobal->NumPrimaryAirSys).TimeStepAtHeatPk(2) = heatTimeOfMax;

    // Set up Facility to peak like the zone too
    state->dataSize->CalcFinalFacilitySizing.CoolDDNum = coolDDNum;
    state->dataSize->CalcFinalFacilitySizing.TimeStepNumAtCoolMax = coolTimeOfMax;
    state->dataSize->CalcFinalFacilitySizing.HeatDDNum = heatDDNum;
    state->dataSize->CalcFinalFacilitySizing.TimeStepNumAtHeatMax = heatTimeOfMax;

    AllocateLoadComponentArrays(*state);
    WriteLoadComponentSummaryTables(*state);

    // TableName, ReportName, value
    std::vector<std::tuple<std::string, std::string, std::string>> results_strings({
        // -17.4C gives 0.68F
        {"Heating Peak Conditions", "Zone Component Load Summary", "        0.68"},
        {"Heating Peak Conditions", "AirLoop Component Load Summary", "        0.68"},
        {"Heating Peak Conditions", "Facility Component Load Summary", "        0.68"},

        // 38C gives 100.4 F
        {"Cooling Peak Conditions", "Zone Component Load Summary", "      100.40"},
        {"Cooling Peak Conditions", "AirLoop Component Load Summary", "      100.40"},
        {"Cooling Peak Conditions", "Facility Component Load Summary", "      100.40"},

    });

    // Would have used bind_text in sqlite3 with a single prepared statement, but m_db is protected in SQLiteProcedures
    std::string tableName;
    std::string reportName;

    for (auto v : results_strings) {

        tableName = std::get<0>(v);
        reportName = std::get<1>(v);

        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE ReportName = '" +
                          reportName +
                          "'"
                          "  AND TableName = '" +
                          tableName +
                          "'"
                          "  AND RowName = 'Outside Dry Bulb Temperature';");

        std::string oa_db = queryResult(query, "TabularDataWithStrings")[0][0];
        // Not needed, we're just querying, not inside a transaction
        // state->dataSQLiteProcedures->sqlite->sqliteCommit();

        // Add informative message if failed
        EXPECT_EQ(std::get<2>(v), oa_db) << "Failed for TableName=" << tableName << "; ReportName=" << reportName;
    }

    // https://github.com/NREL/EnergyPlus/pull/7741
    std::string query_2("SELECT Value From TabularDataWithStrings"
                        "  WHERE TableName = 'Engineering Checks for Cooling'"
                        "  AND RowName = 'Outside Air Fraction';");

    auto result = queryResult(query_2, "TabularDataWithStrings")[0][0];
    EXPECT_EQ(result, "0.0000");
}

TEST_F(EnergyPlusFixture, OutputReportTabularMonthly_hasSizingPeriodsDays_SizingPeriodDesignDay)
{
    std::string const idf_objects = delimited_string({
        "SizingPeriod:DesignDay,",
        "  CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
        "  1,                       !- Month",
        "  21,                      !- Day of Month",
        "  WinterDesignDay,         !- Day Type",
        "  -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
        "  0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
        "  ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "  ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "  Wetbulb,                 !- Humidity Condition Type",
        "  -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "  ,                        !- Humidity Condition Day Schedule Name",
        "  ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "  ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "  ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "  99063.,                  !- Barometric Pressure {Pa}",
        "  4.9,                     !- Wind Speed {m/s}",
        "  270,                     !- Wind Direction {deg}",
        "  No,                      !- Rain Indicator",
        "  No,                      !- Snow Indicator",
        "  No,                      !- Daylight Saving Time Indicator",
        "  ASHRAEClearSky,          !- Solar Model Indicator",
        "  ,                        !- Beam Solar Day Schedule Name",
        "  ,                        !- Diffuse Solar Day Schedule Name",
        "  ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "  ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "  0.0;                     !- Sky Clearness",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    EXPECT_TRUE(hasSizingPeriodsDays(*state));
}

TEST_F(EnergyPlusFixture, OutputReportTabularMonthly_hasSizingPeriodsDays_SizingPeriodWeatherFileDays)
{
    std::string const idf_objects = delimited_string({
        "SizingPeriod:WeatherFileDays,",
        "  Summer including Extreme Summer days,  !- Name",
        "  7,                       !- Begin Month",
        "  18,                      !- Begin Day of Month",
        "  7,                       !- End Month",
        "  25,                      !- End Day of Month",
        "  SummerDesignDay,         !- Day of Week for Start Day",
        "  No,                      !- Use Weather File Daylight Saving Period",
        "  No;                      !- Use Weather File Rain and Snow Indicators",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    EXPECT_TRUE(hasSizingPeriodsDays(*state));
}

TEST_F(EnergyPlusFixture, OutputReportTabularMonthly_hasSizingPeriodsDays_SizingPeriodWeatherFileConditionType)
{
    std::string const idf_objects = delimited_string({
        "SizingPeriod:WeatherFileConditionType,",
        "  Hot,                     !- Name",
        "  SummerExtreme,           !- Period Selection",
        "  Monday,                  !- Day of Week for Start Day",
        "  Yes,                     !- Use Weather File Daylight Saving Period",
        "  Yes;                     !- Use Weather File Rain and Snow Indicators",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    // this test should show a false since this type of sizing period is not compatible with the component loads calculations
    EXPECT_FALSE(hasSizingPeriodsDays(*state));
}

// This tests aims to ensure that the needed Output:Variables for the Predefined Monthly table
// are indeeed set up, and that as a result the numTables is good.
// https://github.com/NREL/EnergyPlus/issues/7019
TEST_F(EnergyPlusFixture, OutputReportTabularMonthlyPredefined_FindNeededOutputVars)
{

    std::string const idf_objects = delimited_string({
        "Output:Table:SummaryReports,",
        " SetpointsNotMetWithTemperaturesMonthly; !- Report 1 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    EXPECT_EQ(1, state->dataInputProcessing->inputProcessor->getNumObjectsFound(*state, "Output:Table:SummaryReports"));
    EXPECT_EQ(0, state->dataInputProcessing->inputProcessor->getNumObjectsFound(*state, "Output:Variable"));

    // InputProcessor::addVariablesForMonthlyReport should have requested 5 variables
    // for the SetpointsNotMetWithTemperatureMonthly report
    EXPECT_EQ(state->dataOutput->OutputVariablesForSimulation.size(), 5u);

    // The Variables needed for the report are initialized (=SetupOutputVariable)
    // Inside ThermalComfort::InitThermalComfort();
    // Except Zone Mean Air Temperature, which is in HeatBalanceAirManager::GetSimpleAirModelInputs()
    // Instead of calling these
    // Fake the setup of the OutputVariables needed for two different zones
    Real64 zoneTemp;
    Real64 timeNotMet;
    std::vector<std::string> ZoneNames({"Zone1", "Zone2"});

    for (int i = 0; i < 2; ++i) {
        SetupOutputVariable(*state, "Zone Mean Air Temperature", OutputProcessor::Unit::C, zoneTemp, "Zone", "Average", ZoneNames[i]);

        SetupOutputVariable(*state, "Zone Heating Setpoint Not Met Time", OutputProcessor::Unit::hr, timeNotMet, "Zone", "Sum", ZoneNames[i]);
        SetupOutputVariable(
            *state, "Zone Heating Setpoint Not Met While Occupied Time", OutputProcessor::Unit::hr, timeNotMet, "Zone", "Sum", ZoneNames[i]);
        SetupOutputVariable(*state, "Zone Cooling Setpoint Not Met Time", OutputProcessor::Unit::hr, timeNotMet, "Zone", "Sum", ZoneNames[i]);
        SetupOutputVariable(
            *state, "Zone Cooling Setpoint Not Met While Occupied Time", OutputProcessor::Unit::hr, timeNotMet, "Zone", "Sum", ZoneNames[i]);
    }

    // We do need to trick it into thinking it's a weather simulation, otherwise the monthly reports aren't reported
    state->dataGlobal->DoWeathSim = true; // flag to trick tabular reports to scan meters

    OutputProcessor::GetReportVariableInput(*state);
    OutputReportTabular::GetInputOutputTableSummaryReports(*state);
    OutputReportTabular::InitializeTabularMonthly(*state);

    // We check that the Predefined Table is actually set to show
    EXPECT_EQ("SetpointsNotMetWithTemperaturesMonthly", state->dataOutRptTab->namedMonthly(31).title);
    EXPECT_TRUE(state->dataOutRptTab->namedMonthly(31).show);

    // Check that it's the only one that's shown
    for (int i = 1; i <= OutputReportTabular::numNamedMonthly; ++i) {
        if (i != 31) {
            EXPECT_FALSE(state->dataOutRptTab->namedMonthly(i).show);
        }
    }

    // Variables aren't going to be output to SQL/ESO anyways
    EXPECT_EQ(state->dataOutputProcessor->NumOfReqVariables, 0);

    EXPECT_EQ(state->dataOutRptTab->MonthlyInputCount, 1);
    // If everything worked, we should have 2 tables, one for each zone.
    // Previously, KeyCount was 0  because it couldn't find the variable in the OutputVariablesForSimulation
    // and so the numTables was zero
    EXPECT_EQ(state->dataOutRptTab->MonthlyInput(1).numTables, 2);
}
// https://github.com/NREL/EnergyPlus/issues/6442
TEST_F(SQLiteFixture, OutputReportTabularTest_PredefinedTableDXConversion)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->WriteTabularFiles = true;

    SetupUnitConversions(*state);
    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::InchPound;
    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::InchPound;

    SetPredefinedTables(*state);
    std::string CompName = "My DX Coil with 10000W cooling";

    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilType, CompName, "Coil:Cooling:DX:SingleSpeed");
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilNetCapSIA, CompName, 10000., 1);
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilNetCapSIB, CompName, 12000., 1);
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilNetCapSIC, CompName, 14000., 1);
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilNetCapSID, CompName, 16000., 1);
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilElecPowerA, CompName, 3300., 1);
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilElecPowerB, CompName, 4300., 1);
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilElecPowerC, CompName, 5300., 1);
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilElecPowerD, CompName, 6300., 1);

    EXPECT_EQ("10000.0", RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilNetCapSIA, CompName));

    // We enable the report we care about, making sure it's the right one
    EXPECT_EQ("EquipmentSummary", state->dataOutRptPredefined->reportName(5).name);
    state->dataOutRptPredefined->reportName(5).show = true;

    WritePredefinedTables(*state);
    state->dataSQLiteProcedures->sqlite->sqliteCommit();
    state->dataSQLiteProcedures->sqlite->initializeIndexes();

    auto units = queryResult("Select Units From TabularDataWithStrings "
                             "WHERE ReportName = \"EquipmentSummary\" "
                             "  AND ColumnName = \"Rated Net Cooling Capacity Test A\"",
                             "TabularDataWithStrings");
    auto values = queryResult("Select Value From TabularDataWithStrings "
                              "WHERE ReportName = \"EquipmentSummary\" "
                              "  AND ColumnName = \"Rated Net Cooling Capacity Test A\"",
                              "TabularDataWithStrings");
    state->dataSQLiteProcedures->sqlite->sqliteCommit();

    EXPECT_EQ(1u, units.size());
    // Because the table has 8 cols
    EXPECT_EQ(8u, units[0].size());

    EXPECT_EQ("ton", units[0][0]);

    EXPECT_EQ(1u, values.size());
    // 10000 W equavals 2.843 tons, rounded to 1 decimal gives 2.8
    std::string s = values[0][0];
    // Trim the string, it has leading spaces
    s.erase(std::remove_if(s.begin(), s.end(), ::isspace), s.end());

    EXPECT_EQ("2.8", s);
}
// https://github.com/NREL/EnergyPlus/issues/7565
TEST_F(SQLiteFixture, OutputReportTabularTest_PredefinedTableCoilHumRat)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->WriteTabularFiles = true;

    SetupUnitConversions(*state);
    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::InchPound;
    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::InchPound;

    SetPredefinedTables(*state);
    std::string CompName = "My DX Coil";

    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilType, CompName, "Coil:Cooling:DX:SingleSpeed");
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdch2CoilLvgHumRatIdealPeak, CompName, 0.006, 8);
    // CoilSizingDetails
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchCoilLvgHumRatIdealPeak, CompName, 0.006, 8);

    // We enable the reports we care about, making sure we have the right ones
    EXPECT_EQ("HVACSizingSummary", state->dataOutRptPredefined->reportName(6).name);
    state->dataOutRptPredefined->reportName(6).show = true;
    EXPECT_EQ("CoilSizingDetails", state->dataOutRptPredefined->reportName(7).name);
    state->dataOutRptPredefined->reportName(7).show = true;

    WritePredefinedTables(*state);
    state->dataSQLiteProcedures->sqlite->sqliteCommit();
    state->dataSQLiteProcedures->sqlite->initializeIndexes();

    for (const std::string reportName : {"HVACSizingSummary", "CoilSizingDetails"}) {

        auto result = queryResult("SELECT Value, Units From TabularDataWithStrings "
                                  "WHERE ReportName = \"" +
                                      reportName +
                                      "\""
                                      "  AND ColumnName = \"Coil Leaving Air Humidity Ratio at Ideal Loads Peak\"",
                                  "TabularDataWithStrings");

        state->dataSQLiteProcedures->sqlite->sqliteCommit();

        EXPECT_EQ(1u, result.size());
        // Because the table has 8 cols
        EXPECT_EQ(8u, result[0].size());

        // 0.006 is a ratio, so unitconv = 1
        std::string s = result[0][0];
        // Trim the string, it has leading spaces
        s.erase(std::remove_if(s.begin(), s.end(), ::isspace), s.end());

        EXPECT_EQ("0.00600000", s);

        EXPECT_EQ("lbWater/lbDryAir", result[0][1]);
    }
}

// Test for #7046
// Ensures that we get consistency between the displayed Azimuth and its cardinal classification
TEST_F(EnergyPlusFixture, AzimuthToCardinal)
{
    // >= 45 and < 135 => E
    // >= 135 and < 225 => S
    // >= 225 and < 315 => W
    // >= 315 and < 45 => N
    std::vector<std::pair<double, std::string>> expectedAzimuthToCards{
        {45.0, "E"},
        {45.01, "E"},
        {134.991, "E"},   // Rounds to 134.99, so classified as E
        {134.99978, "S"}, // Gets rounded to 135.00, so classified as S
        {135.0, "S"},
        {135.001, "S"},
        {136, "S"},
        {314.991, "W"},
        {314.9999, "N"},
        {315.0, "N"},
        {315.00001, "N"},
    };

    int nTests = expectedAzimuthToCards.size();

    // Allocate some needed arrays
    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBal->Zone(1).ListMultiplier = 1;
    state->dataConstruction->Construct.allocate(1);
    state->dataConstruction->Construct(1).Name = "A Construction";
    // Avoid triggering CalcNominalWindowCond
    state->dataConstruction->Construct(1).SummerSHGC = 0.70;
    state->dataConstruction->Construct(1).VisTransNorm = 0.80;

    state->dataHeatBal->NominalU.allocate(1);
    state->dataHeatBal->NominalU(1) = 0.2;

    // Create one wall and one window with each azimuth from expectedAzimuthToCards
    // Azimuth & Cardinal entries happen in two separate blocks,
    // so test both to increase coverage and make sure both are correct

    state->dataSurface->TotSurfaces = 2 * nTests;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataSurface->SurfaceWindow.allocate(state->dataSurface->TotSurfaces);
    SurfaceGeometry::AllocateSurfaceWindows(*state, state->dataSurface->TotSurfaces);

    for (int i = 1; i <= nTests * 2; ++i) {

        state->dataSurface->Surface(i).HeatTransSurf = true;
        state->dataSurface->Surface(i).ExtBoundCond = ExternalEnvironment;
        state->dataSurface->Surface(i).GrossArea = 200.;
        state->dataSurface->Surface(i).Tilt = 90.;
        state->dataSurface->Surface(i).Zone = 1;
        state->dataSurface->Surface(i).Construction = 1;
        state->dataSurface->AllSurfaceListReportOrder.push_back(i);

        // Actual interesting stuff
        int entryIndex = (i - 1) / 2;
        double azimuth = expectedAzimuthToCards[entryIndex].first;
        state->dataSurface->Surface(i).Azimuth = azimuth;

        if (i % 2 == 1) {
            // It's a wall
            state->dataSurface->Surface(i).Class = DataSurfaces::SurfaceClass::Wall;
            state->dataSurface->Surface(i).Name = format("ExtWall_{}_{}", i, entryIndex);
        } else {
            // It's a window
            state->dataSurface->Surface(i).Class = DataSurfaces::SurfaceClass::Window;
            state->dataSurface->Surface(i).Name = format("ExtWindow_{}_{}", i, entryIndex);
            // Window references the previous wall
            state->dataSurface->Surface(i).BaseSurf = i - 1;
        }
    }

    // Setup pre def tables
    OutputReportPredefined::SetPredefinedTables(*state);

    // Call the routine that fills up the table we care about
    HeatBalanceSurfaceManager::GatherForPredefinedReport(*state);

    // Looking for Report 'EnvelopeSummary' (pdrEnvelope)
    // SubTable 'Opaque Exterior' (pdstOpaque)
    // Entry 'Azimuth [deg]' (pdchOpAzimuth)
    // Entry 'Cardinal Direction' (pdchOpDir)

    // Note: Unused because we don't need SQL
    //// We enable the report we care about, making sure it's the right one
    // EXPECT_EQ("EnvelopeSummary", OutputReportPredefined::reportName(2).name);
    // OutputReportPredefined::reportName(2).show = true;
    //// Write the Predef Tables
    // OutputReportTabular::WritePredefinedTables();

    // Integer to find surfaces
    int i = 1;
    for (const auto &expectedAzimuthToCard : expectedAzimuthToCards) {
        double oriAzimuth = expectedAzimuthToCard.first;
        std::string cardinalDir = expectedAzimuthToCard.second;

        // Internal: Just to ensure that we gets the same one with round
        EXPECT_EQ(format("{:.2R}", round(oriAzimuth * 100.0) / 100.0), format("{:.2R}", oriAzimuth));

        /****************************************************************************
         *                            Wall (odd entries)                             *
         *****************************************************************************/

        // Internal: Dumb check to ensure we didn't mess up in the indexation
        EXPECT_EQ(oriAzimuth, state->dataSurface->Surface(i).Azimuth) << "Surface Name = " << state->dataSurface->Surface(i).Name;

        // Check that the azimuth entry is the rounded version indeed
        EXPECT_EQ(
            OutputReportPredefined::RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchOpAzimuth, state->dataSurface->Surface(i).Name),
            format("{:.2R}", expectedAzimuthToCard.first))
            << "Surface Name = " << state->dataSurface->Surface(i).Name;
        // Check that we do get the expected cardinal direction
        EXPECT_EQ(
            OutputReportPredefined::RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchOpDir, state->dataSurface->Surface(i).Name),
            cardinalDir)
            << "Azimuth was " << expectedAzimuthToCard.first << "for Surface '" << state->dataSurface->Surface(i).Name << "'.";

        /****************************************************************************
         *                           Window (even entries)                           *
         *****************************************************************************/

        EXPECT_EQ(oriAzimuth, state->dataSurface->Surface(i + 1).Azimuth) << "Surface Name = " << state->dataSurface->Surface(i + 1).Name;

        // Check that the azimuth entry is the rounded version indeed
        EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                      *state, state->dataOutRptPredefined->pdchFenAzimuth, state->dataSurface->Surface(i + 1).Name),
                  format("{:.2R}", expectedAzimuthToCard.first))
            << "Surface Name = " << state->dataSurface->Surface(i + 1).Name;
        // Check that we do get the expected cardinal direction
        EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                      *state, state->dataOutRptPredefined->pdchFenDir, state->dataSurface->Surface(i + 1).Name),
                  cardinalDir)
            << "Azimuth was " << expectedAzimuthToCard.first << "for Surface '" << state->dataSurface->Surface(i + 1).Name << "'.";

        // Increment twice
        i = i + 2;
    }
}

// Test for interior surface report
TEST_F(EnergyPlusFixture, InteriorSurfaceEnvelopeSummaryReport)
{
    // Allocate some needed arrays
    state->dataHeatBal->Zone.allocate(2);
    state->dataHeatBal->Zone(1).ListMultiplier = 1;
    state->dataHeatBal->Zone(2).ListMultiplier = 1;
    state->dataConstruction->Construct.allocate(1);
    state->dataConstruction->Construct(1).Name = "A Construction";
    state->dataConstruction->Construct(1).OutsideAbsorpSolar = 0.4;

    state->dataHeatBal->NominalU.allocate(1);
    state->dataHeatBal->NominalU(1) = 0.2;

    state->dataSurface->TotSurfaces = 4;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    // first surface, interzonal walls and doors
    for (int i = 1; i <= state->dataSurface->TotSurfaces; i++) {
        state->dataSurface->Surface(i).HeatTransSurf = true;
        state->dataSurface->Surface(i).Azimuth = 180.;
        state->dataSurface->Surface(i).Tilt = 90.;
        state->dataSurface->Surface(i).Construction = 1;
        // odd number - wall, even number - door
        if (i % 2 == 1) {
            state->dataSurface->Surface(i).Name = "Interzonal_Wall_" + fmt::to_string((i + 1) / 2);
            state->dataSurface->Surface(i).GrossArea = 200.;
            state->dataSurface->Surface(i).Class = DataSurfaces::SurfaceClass::Wall;
            state->dataSurface->AllSurfaceListReportOrder.push_back(i);
        } else {
            state->dataSurface->Surface(i).Name = "Interzonal_Door_" + fmt::to_string((i + 1) / 2);
            state->dataSurface->Surface(i).BaseSurfName = state->dataSurface->Surface(i - 1).Name;
            state->dataSurface->Surface(i).BaseSurf = i - 1;
            state->dataSurface->Surface(i).GrossArea = 50.;
            state->dataSurface->Surface(i).Class = DataSurfaces::SurfaceClass::Door;
            state->dataSurface->AllSurfaceListReportOrder.push_back(i);
        }
        if ((i + 1) / 2 == 1) {
            // first pair of wall and door
            state->dataSurface->Surface(i).Zone = 1;
            state->dataSurface->Surface(i).ExtBoundCond = i + 2;
        } else {
            // second pair of wall and door
            state->dataSurface->Surface(i).Zone = 2;
            state->dataSurface->Surface(i).ExtBoundCond = i - 2;
        }
    }
    state->dataSurface->Surface(1).ExtBoundCondName = "Interzonal_Wall_2";
    state->dataSurface->Surface(2).ExtBoundCondName = "Interzonal_Door_2";
    state->dataSurface->Surface(3).ExtBoundCondName = "Interzonal_Wall_1";
    state->dataSurface->Surface(4).ExtBoundCondName = "Interzonal_Door_1";

    // Setup pre def tables
    OutputReportPredefined::SetPredefinedTables(*state);

    // Call the routine that fills up the table we care about
    HeatBalanceSurfaceManager::GatherForPredefinedReport(*state);

    // Looking for Report 'EnvelopeSummary' (pdrEnvelope)
    // SubTable 'Opaque Interior' (pdstIntOpaque)

    for (int i = 1; i <= state->dataSurface->TotSurfaces; i++) {
        if (i % 2 == 1) {

            /****************************************************************************
             *                            Wall (odd entries)                             *
             *****************************************************************************/

            // Check the wall gross area
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntOpGrArea, state->dataSurface->Surface(i).Name),
                      "200.00");
            // Check the wall net area
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntOpNetArea, state->dataSurface->Surface(i).Name),
                      "150.00");
            // Check the wall u value
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntOpUfactNoFilm, state->dataSurface->Surface(i).Name),
                      "0.200");
            // Check the wall construction
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntOpCons, state->dataSurface->Surface(i).Name),
                      "A Construction");
            // Check the wall reflectance
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntOpRefl, state->dataSurface->Surface(i).Name),
                      "0.60");
            // Check the tilt
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntOpTilt, state->dataSurface->Surface(i).Name),
                      "90.00");
            // Check the azimuth
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntOpAzimuth, state->dataSurface->Surface(i).Name),
                      "180.00");
            // Check cardinal direction
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntOpDir, state->dataSurface->Surface(i).Name),
                      "S");
        } else {

            /****************************************************************************
             *                           door (even entries)                           *
             *****************************************************************************/

            // Check the door gross area
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntDrGrArea, state->dataSurface->Surface(i).Name),
                      "50.00");
            // Check the wall u value
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntDrUfactNoFilm, state->dataSurface->Surface(i).Name),
                      "0.200");
            // Check the door construction
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntDrCons, state->dataSurface->Surface(i).Name),
                      "A Construction");
            // Check the door parant surface name
            EXPECT_EQ(OutputReportPredefined::RetrievePreDefTableEntry(
                          *state, state->dataOutRptPredefined->pdchIntDrParent, state->dataSurface->Surface(i).Name),
                      state->dataSurface->Surface(i - 1).Name);
        }
    }
}

TEST_F(SQLiteFixture, WriteSourceEnergyEndUseSummary_TestPerArea)
{

    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->displaySourceEnergyEndUseSummary = true;

    // DetermineBuildingFloorArea

    state->dataEnvrn->Latitude = 12.3;
    state->dataEnvrn->Longitude = 45.6;

    state->dataSurface->TotSurfaces = 4;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    // walls
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(1).Azimuth = 0.;
    state->dataSurface->Surface(1).GrossArea = 200.; // 20 x 10
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Zone = 1;

    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(2).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(2).Azimuth = 90.;
    state->dataSurface->Surface(2).GrossArea = 300.; // 30 x 10
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Zone = 2;

    // windows
    state->dataSurface->Surface(3).Class = SurfaceClass::Window;
    state->dataSurface->Surface(3).HeatTransSurf = true;
    state->dataSurface->Surface(3).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(3).Azimuth = 0.;
    state->dataSurface->Surface(3).GrossArea = 40.;
    state->dataSurface->Surface(3).Height = 5;
    state->dataSurface->Surface(3).Width = 8;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Zone = 1;

    state->dataSurface->Surface(4).Class = SurfaceClass::Window;
    state->dataSurface->Surface(4).HeatTransSurf = true;
    state->dataSurface->Surface(4).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(4).Azimuth = 90.;
    state->dataSurface->Surface(4).GrossArea = 60.;
    state->dataSurface->Surface(4).Height = 6;
    state->dataSurface->Surface(4).Width = 10;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Zone = 2;

    // Loads
    state->dataHeatBal->TotLights = 3;
    state->dataHeatBal->Lights.allocate(state->dataHeatBal->TotLights);

    state->dataHeatBal->TotPeople = 3;
    state->dataHeatBal->People.allocate(state->dataHeatBal->TotPeople);

    state->dataHeatBal->TotElecEquip = 3;
    state->dataHeatBal->ZoneElectric.allocate(state->dataHeatBal->TotElecEquip);

    state->dataHeatBal->Lights(1).ZonePtr = 1;
    state->dataHeatBal->Lights(1).DesignLevel = 1000.0;
    state->dataHeatBal->Lights(2).ZonePtr = 2;
    state->dataHeatBal->Lights(2).DesignLevel = 100.0;
    state->dataHeatBal->Lights(3).ZonePtr = 3;
    state->dataHeatBal->Lights(3).DesignLevel = 10.0;

    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->People(1).NumberOfPeople = 10.0;
    state->dataHeatBal->People(2).ZonePtr = 2;
    state->dataHeatBal->People(2).NumberOfPeople = 5.0;
    state->dataHeatBal->People(3).ZonePtr = 3;
    state->dataHeatBal->People(3).NumberOfPeople = 1.0;

    state->dataHeatBal->ZoneElectric(1).ZonePtr = 1;
    state->dataHeatBal->ZoneElectric(1).DesignLevel = 500.0;
    state->dataHeatBal->ZoneElectric(2).ZonePtr = 2;
    state->dataHeatBal->ZoneElectric(2).DesignLevel = 50.0;
    state->dataHeatBal->ZoneElectric(3).ZonePtr = 3;
    state->dataHeatBal->ZoneElectric(3).DesignLevel = 5.0;

    // zone
    state->dataGlobal->NumOfZones = 3;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "PartofTot Conditioned Zone";
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 1; // Conditioned
    state->dataHeatBal->Zone(1).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(1).Multiplier = 1.;
    state->dataHeatBal->Zone(1).ListMultiplier = 1.;
    state->dataHeatBal->Zone(1).FloorArea = 1000.;
    state->dataHeatBal->Zone(1).Volume = 2000.;
    state->dataHeatBal->Zone(1).ExtGrossWallArea = 800.;
    state->dataHeatBal->Zone(1).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(1).ExtWindowArea = state->dataSurface->Surface(3).GrossArea + state->dataSurface->Surface(4).GrossArea;

    state->dataHeatBal->Zone(2).Name = "PartofTot Unconditioned Zone";
    state->dataHeatBal->Zone(2).SystemZoneNodeNumber = 0; // Unconditioned
    state->dataHeatBal->Zone(2).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(2).Multiplier = 1.;
    state->dataHeatBal->Zone(2).ListMultiplier = 1.;
    state->dataHeatBal->Zone(2).FloorArea = 100.;
    state->dataHeatBal->Zone(2).Volume = 200.;
    state->dataHeatBal->Zone(2).ExtGrossWallArea = 80.;
    state->dataHeatBal->Zone(2).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(2).ExtWindowArea = 0.0;

    state->dataHeatBal->Zone(3).Name = "NOT PartofTot Conditioned Zone";
    state->dataHeatBal->Zone(3).SystemZoneNodeNumber = 1; // Conditioned
    state->dataHeatBal->Zone(3).isPartOfTotalArea = false;
    state->dataHeatBal->Zone(3).Multiplier = 1.;
    state->dataHeatBal->Zone(3).ListMultiplier = 1.;
    state->dataHeatBal->Zone(3).FloorArea = 10.;
    state->dataHeatBal->Zone(3).Volume = 20.;
    state->dataHeatBal->Zone(3).ExtGrossWallArea = 8.;
    state->dataHeatBal->Zone(3).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(3).ExtWindowArea = 0.0;

    // Gross takes all that are PartOfTot
    Real64 expectedBuildingGrossFloorArea = state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea;
    // Conditioned takes only PartOfTot AND COnditioned
    Real64 expectedBuildingConditionedFloorArea = state->dataHeatBal->Zone(1).FloorArea;

    // Assume that we only have electricity with a value of 3.6e6 * 1e4 J =10.000 kWh.
    // And that this only comes for a single end use state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::Heating)=1
    state->dataOutRptTab->gatherEndUseBySourceBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::Heating)) = 3.6e10;
    state->dataOutRptTab->gatherTotalsBySourceBEPS(1) = 3.6e10;
    Real64 eleckWh = 1e4;

    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::JtoKWH;
    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::JtoKWH;

    // Now we're ready to call the actual function of interest
    OutputReportTabular::WriteSourceEnergyEndUseSummary(*state);

    // Before we test the reporting itself, we check that DetermineBuildingFloorArea (called from WriteSourceEnergyEndUseSummary)
    // actually did what we expected
    EXPECT_EQ(expectedBuildingGrossFloorArea, state->dataOutRptTab->buildingGrossFloorArea);
    EXPECT_EQ(expectedBuildingConditionedFloorArea, state->dataOutRptTab->buildingConditionedFloorArea);

    // Now we test the reporting itself:
    // We consistently test in the same report (three different tables) and at the same column for fuel = Elec
    const std::string reportName = "SourceEnergyEndUseComponentsSummary";
    const std::string columnName = "Source Electricity";

    // We test for Heating and Total, since they should be the same
    std::vector<std::string> testRowNames = {"Heating", "Total Source Energy End Use Components"};

    // TableName, value
    std::vector<std::tuple<std::string, Real64>> results({
        {"Source Energy End Use Components Summary", eleckWh},
        {"Source Energy End Use Component Per Conditioned Floor Area", 10000.0 / expectedBuildingConditionedFloorArea},
        {"Source Energy End Use Components Per Total Floor Area", 10000.0 / expectedBuildingGrossFloorArea},
    });

    for (auto &v : results) {

        std::string tableName = std::get<0>(v);
        Real64 expectedValue = std::get<1>(v);

        for (auto &rowName : testRowNames) {
            std::string query("SELECT Value From TabularDataWithStrings"
                              "  WHERE ReportName = '" +
                              reportName +
                              "'"
                              "  AND TableName = '" +
                              tableName +
                              "'"
                              "  AND RowName = '" +
                              rowName + "'" + "  AND ColumnName = '" + columnName + "'");

            Real64 return_val = execAndReturnFirstDouble(query);

            // Add informative message if failed
            EXPECT_NEAR(expectedValue, return_val, 0.01) << "Failed for TableName=" << tableName << "; RowName=" << rowName;
        }
    }

    state->dataSQLiteProcedures->sqlite->sqliteCommit();
}

TEST_F(SQLiteFixture, OutputReportTabular_EndUseBySubcategorySQL)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->displayTabularBEPS = true;
    state->dataOutRptTab->displayDemandEndUse = true;
    state->dataOutRptTab->displayLEEDSummary = true;

    state->dataOutRptTab->WriteTabularFiles = true;

    SetupUnitConversions(*state);
    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::JtoKWH;
    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::JtoKWH;

    // Needed to avoid crash (from ElectricPowerServiceManager.hh)
    createFacilityElectricPowerServiceObject(*state);

    SetPredefinedTables(*state);

    Real64 extLitUse = 1e8;
    Real64 CoalHeating = 2e8;
    Real64 GasolineHeating = 3e8;
    Real64 PropaneHeating = 4e8;

    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite1",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite2",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "AnotherEndUseSubCat");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite3",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(
        *state, "Heating Coal Energy", OutputProcessor::Unit::J, CoalHeating, "Zone", "Sum", "Lite4", _, "Coal", "Heating", "General");
    SetupOutputVariable(
        *state, "Heating Gasoline Energy", OutputProcessor::Unit::J, GasolineHeating, "Zone", "Sum", "Lite5", _, "Gasoline", "Heating", "General");
    SetupOutputVariable(
        *state, "Heating Propane Energy", OutputProcessor::Unit::J, PropaneHeating, "Zone", "Sum", "Lite6", _, "Propane", "Heating", "General");
    state->dataGlobal->DoWeathSim = true;
    state->dataGlobal->TimeStepZone = 1.0;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * 60.0;
    state->dataOutRptTab->displayTabularBEPS = true;
    // OutputProcessor::TimeValue.allocate(2);

    auto timeStep = 1.0;

    SetupTimePointers(*state, "Zone", timeStep);
    SetupTimePointers(*state, "HVAC", timeStep);

    *state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::TimeStepZone).TimeStep = 60;
    *state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::TimeStepSystem).TimeStep = 60;

    GetInputOutputTableSummaryReports(*state);

    state->dataEnvrn->Month = 12;

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_NEAR(extLitUse * 3,
                state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)),
                1.);
    // General
    EXPECT_NEAR(extLitUse * 2,
                state->dataOutRptTab->gatherEndUseSubBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);
    // AnotherEndUseSubCat
    EXPECT_NEAR(extLitUse * 1,
                state->dataOutRptTab->gatherEndUseSubBEPS(2, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_NEAR(extLitUse * 6,
                state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)),
                1.);
    // General
    EXPECT_NEAR(extLitUse * 4,
                state->dataOutRptTab->gatherEndUseSubBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);
    // AnotherEndUseSubCat
    EXPECT_NEAR(extLitUse * 2,
                state->dataOutRptTab->gatherEndUseSubBEPS(2, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_NEAR(extLitUse * 9,
                state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)),
                1.);
    // General
    EXPECT_NEAR(extLitUse * 6,
                state->dataOutRptTab->gatherEndUseSubBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);
    // AnotherEndUseSubCat
    EXPECT_NEAR(extLitUse * 3,
                state->dataOutRptTab->gatherEndUseSubBEPS(2, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);

    OutputReportTabular::WriteBEPSTable(*state);
    OutputReportTabular::WriteDemandEndUseSummary(*state);

    state->dataSQLiteProcedures->sqlite->sqliteCommit();

    // We test for Heating and Total, since they should be the same
    std::vector<std::string> testReportNames = {"AnnualBuildingUtilityPerformanceSummary", "DemandEndUseComponentsSummary"};
    std::vector<std::string> endUseSubCategoryNames = {"General", "AnotherEndUseSubCat"};

    std::string endUseName = "Exterior Lighting";
    std::string endUseSubCategoryName = "AnotherEndUseSubCat";
    std::string rowName = endUseName + ":" + endUseSubCategoryName;
    std::string columnName = "Electricity";

    for (auto &endUseSubCategoryName : endUseSubCategoryNames) {
        for (auto &reportName : testReportNames) {

            std::string query("SELECT Value From TabularDataWithStrings"
                              "  WHERE TableName = 'End Uses By Subcategory'"
                              "  AND ColumnName = 'Electricity'"
                              "  AND ReportName = '" +
                              reportName +
                              "'"
                              "  AND RowName = '" +
                              endUseName + ":" + endUseSubCategoryName + "'"); // Now Like 'Exterior Lighting:General'

            auto result = queryResult(query, "TabularDataWithStrings");

            ASSERT_EQ(1ul, result.size()) << "Query crashed for reportName=" << reportName;
        }
    }

    for (auto &reportName : testReportNames) {

        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ColumnName = 'Electricity'"
                          "  AND ReportName = '" +
                          reportName +
                          "'"
                          "  AND RowName = '" +
                          endUseName + "'");

        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(1ul, result.size()) << "Query crashed for reportName=" << reportName;
    }

    // Specifically get the electricity usage for End Use = Exterior Lighting, and End Use Subcat = AnotherEndUseSubCat,
    // and make sure it's the right number that's returned
    std::string query("SELECT Value From TabularDataWithStrings"
                      "  WHERE TableName = 'End Uses By Subcategory'"
                      "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                      "  AND ColumnName = 'Electricity'"
                      "  AND RowName = 'Exterior Lighting:AnotherEndUseSubCat'");
    Real64 return_val = execAndReturnFirstDouble(query);

    EXPECT_NEAR(extLitUse * 3 / 3.6e6, return_val, 0.01) << "Failed for query: " << query;

    // Get all Interior Lighting End Uses (all subcats) for Electricity
    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses By Subcategory'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND ColumnName = 'Electricity'"
                          "  AND RowName LIKE 'Exterior Lighting:%'");
        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(2u, result.size()) << "Failed for query: " << query;
    }

    // Get all subcat usage for all fuels (13)
    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses By Subcategory'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND RowName = 'Exterior Lighting:AnotherEndUseSubCat'");
        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(13u, result.size()) << "Failed for query: " << query;
    }

    // Specifically get the each fuel (Coal, Gasoline, and Propane) usage for End Use = Heating,
    // and make sure it's the right number that's returned

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND ColumnName = 'Coal'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");
        Real64 return_val1 = execAndReturnFirstDouble(query);

        ASSERT_EQ(1u, result.size()) << "Failed for query: " << query;
        EXPECT_NEAR(CoalHeating * 3 / 3.6e6, return_val1, 0.01) << "Failed for query: " << query;
    }

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND ColumnName = 'Gasoline'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");
        Real64 return_val2 = execAndReturnFirstDouble(query);

        ASSERT_EQ(1u, result.size()) << "Failed for query: " << query;
        EXPECT_NEAR(GasolineHeating * 3 / 3.6e6, return_val2, 0.01) << "Failed for query: " << query;
    }

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND ColumnName = 'Propane'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");
        Real64 return_val3 = execAndReturnFirstDouble(query);

        ASSERT_EQ(1u, result.size()) << "Failed for query: " << query;
        EXPECT_NEAR(PropaneHeating * 3 / 3.6e6, return_val3, 0.01) << "Failed for query: " << query;
    }

    // Check the heating category has the result size of 13 (including all disaggregated additional fuels) in both reports)

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(13u, result.size()) << "Failed for query: " << query;
    }

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'DemandEndUseComponentsSummary'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(13u, result.size()) << "Failed for query: " << query;
    }
}

TEST_F(EnergyPlusFixture, StatFileCharacterMatching)
{
    StatLineType lineTypeReturn = StatLineType::Initialized;
    bool desCondLinePassed = false;
    bool htgDesignLinePassed = false;
    bool clgDesignLinePassed = false;
    bool isKoppen = false;
    std::string coolingLineGoodDegrees = "    - 2874 annual (standard) cooling degree-days (10°C baseline)";
    parseStatLine(coolingLineGoodDegrees, lineTypeReturn, desCondLinePassed, htgDesignLinePassed, clgDesignLinePassed, isKoppen);
    EXPECT_EQ((int)StatLineType::stdCDDLine, (int)lineTypeReturn);

    lineTypeReturn = StatLineType::Initialized;
    desCondLinePassed = false;
    htgDesignLinePassed = false;
    clgDesignLinePassed = false;
    isKoppen = false;
    std::string coolingLineBadDegrees = "    - 2874 annual (standard) cooling degree-days (10_BADDEGREESYMBOL_C baseline)";
    parseStatLine(coolingLineGoodDegrees, lineTypeReturn, desCondLinePassed, htgDesignLinePassed, clgDesignLinePassed, isKoppen);
    EXPECT_EQ((int)StatLineType::stdCDDLine, (int)lineTypeReturn);

    lineTypeReturn = StatLineType::Initialized;
    desCondLinePassed = false;
    htgDesignLinePassed = false;
    clgDesignLinePassed = false;
    isKoppen = false;
    std::string koppenLineWithDots = " - Climate type \"Cfa\" (Köppen classification)**";
    parseStatLine(koppenLineWithDots, lineTypeReturn, desCondLinePassed, htgDesignLinePassed, clgDesignLinePassed, isKoppen);
    EXPECT_EQ((int)StatLineType::KoppenLine, (int)lineTypeReturn);

    lineTypeReturn = StatLineType::Initialized;
    desCondLinePassed = false;
    htgDesignLinePassed = false;
    clgDesignLinePassed = false;
    isKoppen = false;
    std::string koppenLineNoDots = " - Climate type \"Cfa\" (Koppen classification)**";
    parseStatLine(koppenLineNoDots, lineTypeReturn, desCondLinePassed, htgDesignLinePassed, clgDesignLinePassed, isKoppen);
    EXPECT_EQ((int)StatLineType::KoppenLine, (int)lineTypeReturn);
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_GetDelaySequencesSurfaceOrder_test)
{

    int coolDesSelected = 1;
    int iZone = 1;
    state->dataEnvrn->TotDesDays = 2;
    state->dataEnvrn->TotRunDesPersDays = 3;
    state->dataGlobal->NumOfTimeStepInHour = 4;

    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);

    state->dataHeatBal->Zone(iZone).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(iZone).HTSurfaceLast = 4;
    state->dataHeatBal->Zone(iZone).RadiantEnclosureNum = 1;
    int radEnclosureNum = 1;

    state->dataSurface->TotSurfaces = 4;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    Array1D<Real64> peopleDelaySeq;
    peopleDelaySeq.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    peopleDelaySeq = 0.;

    Array1D<Real64> peopleDelaySeqCool;
    peopleDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    peopleDelaySeqCool = 0.;

    Array1D<Real64> equipDelaySeqCool;
    equipDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    equipDelaySeqCool = 0.;

    Array1D<Real64> hvacLossDelaySeqCool;
    hvacLossDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    hvacLossDelaySeqCool = 0.;

    Array1D<Real64> powerGenDelaySeqCool;
    powerGenDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    powerGenDelaySeqCool = 0.;

    Array1D<Real64> lightDelaySeqCool;
    lightDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    lightDelaySeqCool = 0.;

    Array1D<Real64> feneSolarDelaySeqCool;
    feneSolarDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24);
    feneSolarDelaySeqCool = 0.;

    Array3D<Real64> feneCondInstantSeq;
    feneCondInstantSeq.allocate(state->dataEnvrn->TotDesDays + state->dataEnvrn->TotRunDesPersDays,
                                state->dataGlobal->NumOfTimeStepInHour * 24,
                                state->dataGlobal->NumOfZones);
    feneCondInstantSeq = 0.0;

    Array2D<Real64> surfDelaySeqCool;
    surfDelaySeqCool.allocate(state->dataGlobal->NumOfTimeStepInHour * 24, state->dataSurface->TotSurfaces);
    surfDelaySeqCool = 0.0;

    AllocateLoadComponentArrays(*state);

    // Set surface values
    std::vector<Real64> surfBaseValue{100.0, 200.0, 300.0, 400.0};
    state->dataSurface->Surface(1).Area = 10;
    state->dataSurface->Surface(2).Area = 20;
    state->dataSurface->Surface(3).Area = 30;
    state->dataSurface->Surface(4).Area = 40;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(3).HeatTransSurf = true;
    state->dataSurface->Surface(4).HeatTransSurf = false;
    state->dataSurface->Surface(1).Class = SurfaceClass::Window;
    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(3).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(4).Class = SurfaceClass::Shading;

    for (int jSurf = 1; jSurf <= 4; ++jSurf) {
        for (int step = 1; step <= 10; ++step) {
            state->dataOutRptTab->TMULTseq(coolDesSelected, step, radEnclosureNum) = 0.1 * step;
            state->dataOutRptTab->ITABSFseq(coolDesSelected, step, jSurf) = 0.2 * step * surfBaseValue[jSurf - 1];
            state->dataOutRptTab->decayCurveCool(step, jSurf) = 0.3 * step * surfBaseValue[jSurf - 1];
            state->dataOutRptTab->peopleRadSeq(coolDesSelected, step, iZone) = 0.4 * step;
            state->dataOutRptTab->equipRadSeq(coolDesSelected, step, iZone) = 0.5 * step;
            state->dataOutRptTab->hvacLossRadSeq(coolDesSelected, step, iZone) = 0.6 * step;
            state->dataOutRptTab->powerGenRadSeq(coolDesSelected, step, iZone) = 0.7 * step;
            state->dataOutRptTab->lightLWRadSeq(coolDesSelected, step, iZone) = 0.8 * step;
        }
    }

    GetDelaySequences(*state,
                      coolDesSelected,
                      true,
                      iZone,
                      peopleDelaySeqCool,
                      equipDelaySeqCool,
                      hvacLossDelaySeqCool,
                      powerGenDelaySeqCool,
                      lightDelaySeqCool,
                      feneSolarDelaySeqCool,
                      feneCondInstantSeq,
                      surfDelaySeqCool);

    // Save some results from first pass
    Real64 peopleDelaySeqCool1 = peopleDelaySeqCool(1);
    Real64 equipDelaySeqCool1 = equipDelaySeqCool(2);
    Real64 hvacLossDelaySeqCool1 = hvacLossDelaySeqCool(3);
    Real64 powerGenDelaySeqCool1 = powerGenDelaySeqCool(4);
    Real64 lightDelaySeqCool1 = lightDelaySeqCool(5);
    Real64 feneSolarDelaySeqCool1 = feneSolarDelaySeqCool(6);

    // Rearrange surface values
    surfBaseValue = {300.0, 100.0, 400.0, 200.0};
    state->dataSurface->Surface(2).Area = 10;
    state->dataSurface->Surface(4).Area = 20;
    state->dataSurface->Surface(1).Area = 30;
    state->dataSurface->Surface(3).Area = 40;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(4).HeatTransSurf = true;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(3).HeatTransSurf = false;
    state->dataSurface->Surface(2).Class = SurfaceClass::Window;
    state->dataSurface->Surface(4).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).Class = SurfaceClass::Floor;
    state->dataSurface->Surface(3).Class = SurfaceClass::Shading;

    for (int jSurf = 1; jSurf <= 4; ++jSurf) {
        for (int step = 1; step <= 10; ++step) {
            state->dataOutRptTab->TMULTseq(coolDesSelected, step, radEnclosureNum) = 0.1 * step;
            state->dataOutRptTab->ITABSFseq(coolDesSelected, step, jSurf) = 0.2 * step * surfBaseValue[jSurf - 1];
            state->dataOutRptTab->decayCurveCool(step, jSurf) = 0.3 * step * surfBaseValue[jSurf - 1];
            state->dataOutRptTab->peopleRadSeq(coolDesSelected, step, iZone) = 0.4 * step;
            state->dataOutRptTab->equipRadSeq(coolDesSelected, step, iZone) = 0.5 * step;
            state->dataOutRptTab->hvacLossRadSeq(coolDesSelected, step, iZone) = 0.6 * step;
            state->dataOutRptTab->powerGenRadSeq(coolDesSelected, step, iZone) = 0.7 * step;
            state->dataOutRptTab->lightLWRadSeq(coolDesSelected, step, iZone) = 0.8 * step;
        }
    }

    GetDelaySequences(*state,
                      coolDesSelected,
                      true,
                      iZone,
                      peopleDelaySeqCool,
                      equipDelaySeqCool,
                      hvacLossDelaySeqCool,
                      powerGenDelaySeqCool,
                      lightDelaySeqCool,
                      feneSolarDelaySeqCool,
                      feneCondInstantSeq,
                      surfDelaySeqCool);

    // Save some results from second pass
    Real64 peopleDelaySeqCool2 = peopleDelaySeqCool(1);
    Real64 equipDelaySeqCool2 = equipDelaySeqCool(2);
    Real64 hvacLossDelaySeqCool2 = hvacLossDelaySeqCool(3);
    Real64 powerGenDelaySeqCool2 = powerGenDelaySeqCool(4);
    Real64 lightDelaySeqCool2 = lightDelaySeqCool(5);
    Real64 feneSolarDelaySeqCool2 = feneSolarDelaySeqCool(6);

    EXPECT_EQ(peopleDelaySeqCool1, peopleDelaySeqCool2);
    EXPECT_EQ(equipDelaySeqCool1, equipDelaySeqCool2);
    EXPECT_EQ(hvacLossDelaySeqCool1, hvacLossDelaySeqCool2);
    EXPECT_EQ(powerGenDelaySeqCool1, powerGenDelaySeqCool2);
    EXPECT_EQ(lightDelaySeqCool1, lightDelaySeqCool2);
    EXPECT_EQ(feneSolarDelaySeqCool1, feneSolarDelaySeqCool2);
}

TEST_F(EnergyPlusFixture, OutputReportTabularTest_ConfirmConversionFactors)
{

    Real64 curSourceFactor;
    bool fuelFactorUsed;
    bool fFScheduleUsed;
    int ffScheduleIndex;

    PollutionModule::GetFuelFactorInfo(*state, "Steam", fuelFactorUsed, curSourceFactor, fFScheduleUsed, ffScheduleIndex);

    EXPECT_EQ(curSourceFactor, 1.2);
}

TEST_F(EnergyPlusFixture, OutputReportTabular_GatherHeatGainReport)
{
    state->dataGlobal->DoWeathSim = true;

    state->dataOutRptPredefined->pdrSensibleGain = 1;
    state->dataOutRptPredefined->reportName.allocate(1);
    state->dataOutRptPredefined->reportName(state->dataOutRptPredefined->pdrSensibleGain).show = true;

    state->dataHVACGlobal->TimeStepSys = 10.0;
    state->dataGlobal->TimeStepZone = 20.0;
    state->dataDefineEquipment->NumAirDistUnits = 1;

    state->dataHeatBal->ZonePreDefRep.allocate(1);
    state->dataDefineEquipment->AirDistUnit.allocate(1);
    state->dataDefineEquipment->AirDistUnit(1).ZoneNum = 1;
    state->dataDefineEquipment->AirDistUnit(1).HeatGain = 1000.0;
    state->dataDefineEquipment->AirDistUnit(1).CoolGain = 2000.0;
    state->dataDefineEquipment->AirDistUnit(1).HeatRate = 3.0;
    state->dataDefineEquipment->AirDistUnit(1).CoolRate = 4.0;

    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Multiplier = 1;
    state->dataHeatBal->Zone(1).ListMultiplier = 1;

    state->dataHeatBal->ZnRpt.allocate(1);
    state->dataHeatBal->ZnAirRpt.allocate(1);

    state->dataHeatBal->ZoneWinHeatGainRepEnergy.allocate(1);
    state->dataHeatBal->ZoneWinHeatLossRepEnergy.allocate(1);

    GatherHeatGainReport(*state, OutputProcessor::TimeStepType::TimeStepSystem);

    EXPECT_EQ(1.0 * (state->dataHVACGlobal->TimeStepSys) * DataGlobalConstants::SecInHour, state->dataHeatBal->ZonePreDefRep(1).SHGSAnZoneEqHt);
    EXPECT_EQ(0.0 * (state->dataHVACGlobal->TimeStepSys) * DataGlobalConstants::SecInHour, state->dataHeatBal->ZonePreDefRep(1).SHGSAnZoneEqCl);
    EXPECT_EQ(1000.0, state->dataHeatBal->ZonePreDefRep(1).SHGSAnHvacATUHt);
    EXPECT_EQ(-2000.0, state->dataHeatBal->ZonePreDefRep(1).SHGSAnHvacATUCl);
}

TEST_F(EnergyPlusFixture, OutputReportTabularMonthly_8317_ValidateOutputTableMonthly)
{
    // Test for #8317 - Output:Table:Monthly does not warn about invalid variable or meter name
    std::string const idf_objects = delimited_string({

        "Schedule:Compact,",
        " OnSched,                  !- Name",
        " Fraction,                 !- Schedule Type Limits Name",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00, 1.0;        !- Field 3",

        "ScheduleTypeLimits,",
        " Fraction,                 !- Name",
        " 0.0,                      !- Lower Limit Value",
        " 1.0,                      !- Upper Limit Value",
        " CONTINUOUS;               !- Numeric Type",

        "Output:Table:Monthly,",
        "  My Report,                        !- Name",
        "  2,                                !- Digits After Decimal",
        "  Heating:Gas,                      !- Variable or Meter 1 Name", // A bad meter name
        "  SumOrAverage,                     !- Aggregation Type for Variable or Meter 1",
        "  Exterior Lights Electric Power,   !- Variable or Meter 2 Name", // A bad variable name
        "  Maximum,                          !- Aggregation Type for Variable or Meter 2",
        "  AlwaysOn,                         !- Variable or Meter 3 Name", // A bad name (eg: Schedule)
        "  Maximum,                          !- Aggregation Type for Variable or Meter 3",
        "  Exterior Lights Electricity Rate, !- Variable or Meter 4 Name", // A good variable name
        "  Minimum,                          !- Aggregation Type for Variable or Meter 4",
        "  OnSched,                          !- Variable or Meter 5 Name", // A good schedule name
        "  Minimum,                          !- Aggregation Type for Variable or Meter 5",
        "  Heating:NaturalGas,               !- Variable or Meter 6 Name", // A good meter name
        "  SumOrAverage;                     !- Aggregation Type for Variable or Meter 6",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    Real64 extLitUse;

    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite1",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Rate",
                        OutputProcessor::Unit::W,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite2",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");

    state->dataGlobal->DoWeathSim = true;
    state->dataGlobal->TimeStepZone = 0.25;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * 60.0;

    InitializeOutput(*state);

    int meter_array_ptr = -1;
    bool errors_found = false;

    std::string resourceType("NaturalGas");
    std::string endUse("Heating");
    std::string endUseSub("");
    std::string group("");
    std::string const zoneName("");

    AttachMeters(*state, OutputProcessor::Unit::J, resourceType, endUse, endUseSub, group, zoneName, 1, meter_array_ptr, errors_found);

    EXPECT_FALSE(errors_found);
    EXPECT_EQ(3, meter_array_ptr); // 2 meters were setup via SetupOutputVariable already

    OutputReportTabular::GetInputTabularMonthly(*state);
    OutputReportTabular::InitializeTabularMonthly(*state);

    std::string expected_error = delimited_string({
        "   ** Warning ** In Output:Table:Monthly 'MY REPORT' invalid Variable or Meter Name 'HEATING:GAS'",
        "   ** Warning ** In Output:Table:Monthly 'MY REPORT' invalid Variable or Meter Name 'EXTERIOR LIGHTS ELECTRIC POWER'",
        "   ** Warning ** In Output:Table:Monthly 'MY REPORT' invalid Variable or Meter Name 'ALWAYSON'",
    });

    compare_err_stream(expected_error);
}

TEST_F(EnergyPlusFixture, ORT_DualUnits_Process_Regular_Case_1)
{
    // Test units to ensure proper Output:SQLite unit conversion handling

    // Test the regular scenario (No missing or default values) Case 1: UseoutputControlTableStyle
    std::string const idf_objects =
        delimited_string({"Output:SQLite,", "SimpleAndTabular, !-Option Type", "UseOutputControlTableStyle; !-Tabular Unit Conversion"});

    ASSERT_TRUE(process_idf(idf_objects));

    state->files.outputControl.sqlite = true;
    state->dataStrGlobals->outputSqlFilePath = "eplussqlite1.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout1.sql";

    state->dataSQLiteProcedures->sqlite = EnergyPlus::CreateSQLiteDatabase(*state);

    EXPECT_EQ(state->dataOutRptTab->unitsStyle_SQLite, iUnitsStyle::NotFound);
    EXPECT_NE(state->dataSQLiteProcedures->sqlite, nullptr);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeOutputToSQLite(), true);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeTabularDataToSQLite(), true);

    state->dataStrGlobals->outputSqlFilePath = "eplussqlite.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout.sql";
}

TEST_F(EnergyPlusFixture, ORT_DualUnits_Process_Regular_Case_2)
{
    // Test the regular scenario (No missing or default values) Case 2: InchPound
    std::string const idf_objects = delimited_string({"Output:SQLite,", "SimpleAndTabular, !-Option Type", "InchPound; !-Tabular Unit Conversion"});

    ASSERT_TRUE(process_idf(idf_objects));

    state->files.outputControl.sqlite = true;

    state->dataStrGlobals->outputSqlFilePath = "eplussqlite2.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout2.sql";
    state->dataSQLiteProcedures->sqlite = EnergyPlus::CreateSQLiteDatabase(*state);

    EXPECT_EQ(state->dataOutRptTab->unitsStyle_SQLite, iUnitsStyle::InchPound);
    EXPECT_NE(state->dataSQLiteProcedures->sqlite, nullptr);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeOutputToSQLite(), true);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeTabularDataToSQLite(), true);

    state->dataStrGlobals->outputSqlFilePath = "eplussqlite.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout.sql";
}

TEST_F(EnergyPlusFixture, ORT_DualUnits_Process_Regular_Case_3)
{
    // Test the regular scenario (No missing or default values) Case 3: None
    std::string const idf_objects = delimited_string({"Output:SQLite,", "SimpleAndTabular, !-Option Type", "None; !-Tabular Unit Conversion"});

    ASSERT_TRUE(process_idf(idf_objects));

    state->files.outputControl.sqlite = true;

    state->dataStrGlobals->outputSqlFilePath = "eplussqlite3.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout3.sql";
    state->dataSQLiteProcedures->sqlite = EnergyPlus::CreateSQLiteDatabase(*state);

    EXPECT_EQ(state->dataOutRptTab->unitsStyle_SQLite, iUnitsStyle::None);
    EXPECT_NE(state->dataSQLiteProcedures->sqlite, nullptr);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeOutputToSQLite(), true);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeTabularDataToSQLite(), true);

    state->dataStrGlobals->outputSqlFilePath = "eplussqlite.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout.sql";
}

TEST_F(EnergyPlusFixture, ORT_DualUnits_Process_Missing_Case_1)
{
    // Test the missing scenario (has missing or default fields) Case 1: Default empty input
    std::string const idf_objects = delimited_string({"Output:SQLite,", "SimpleAndTabular, !-Option Type", "; !-Tabular Unit Conversion"});

    ASSERT_TRUE(process_idf(idf_objects));

    state->files.outputControl.sqlite = true;

    state->dataStrGlobals->outputSqlFilePath = "eplussqlite4.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout4.sql";

    state->dataSQLiteProcedures->sqlite = EnergyPlus::CreateSQLiteDatabase(*state);

    EXPECT_EQ(state->dataOutRptTab->unitsStyle_SQLite, iUnitsStyle::NotFound);
    EXPECT_NE(state->dataSQLiteProcedures->sqlite, nullptr);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeOutputToSQLite(), true);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeTabularDataToSQLite(), true);

    state->dataStrGlobals->outputSqlFilePath = "eplussqlite.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout.sql";
}

TEST_F(EnergyPlusFixture, ORT_DualUnits_Process_Missing_Case_2)
{
    // Test the missing scenario (has missing or default fields) Case 2: Missing A2 field at all
    // This will allow a backward compatiability: even an earlier version format can be correctly handeled.
    std::string const idf_objects = delimited_string({"Output:SQLite,", "SimpleAndTabular; !-Option Type"});

    ASSERT_TRUE(process_idf(idf_objects));

    state->files.outputControl.sqlite = true;

    state->dataStrGlobals->outputSqlFilePath = "eplussqlite5.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout5.sql";

    state->dataSQLiteProcedures->sqlite = EnergyPlus::CreateSQLiteDatabase(*state);

    EXPECT_EQ(state->dataOutRptTab->unitsStyle_SQLite, iUnitsStyle::NotFound);
    EXPECT_NE(state->dataSQLiteProcedures->sqlite, nullptr);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeOutputToSQLite(), true);
    EXPECT_EQ(state->dataSQLiteProcedures->sqlite->writeTabularDataToSQLite(), true);

    state->dataStrGlobals->outputSqlFilePath = "eplussqlite.err";
    state->dataStrGlobals->outputSqliteErrFilePath = "eplusout.sql";
}

TEST_F(SQLiteFixture, ORT_DualUnits_Heat_Emission)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->displayHeatEmissionsSummary = true;

    // DetermineBuildingFloorArea
    state->dataHeatBal->BuildingPreDefRep.emiEnvelopConv = 1000.00;    // * energyconversion, 2);
    state->dataHeatBal->BuildingPreDefRep.emiZoneExfiltration = 50.00; // * energyconversion, 2);
    state->dataHeatBal->BuildingPreDefRep.emiZoneExhaust = 200.00;     // * energyconversion, 2);
    state->dataHeatBal->BuildingPreDefRep.emiHVACRelief = 500.00;      // * energyconversion, 2);
    state->dataHeatBal->BuildingPreDefRep.emiHVACReject = 1750.00;     // * energyconversion, 2);
    state->dataHeatBal->BuildingPreDefRep.emiTotHeat = 4000.00;        // * energyconversion, 2);

    // Test Combination 0: GJ
    state->dataOutRptTab->unitsStyle = iUnitsStyle::JtoGJ;
    state->dataOutRptTab->unitsStyle_SQLite = iUnitsStyle::JtoGJ;
    Real64 energyconversion = 1.0;

    WriteHeatEmissionTable(*state);

    // Now test the reporting:
    const std::string reportName = "AnnualHeatEmissionsReport";
    const std::string tableName = "Annual Heat Emissions Summary";
    std::string unitsName = "GJ";

    // Test the row of heat emissions
    std::vector<std::string> testRowNames = {"Heat Emissions"};

    // TableName, value
    std::vector<std::tuple<std::string, Real64>> results0({
        {"Envelope Convection", state->dataHeatBal->BuildingPreDefRep.emiEnvelopConv * energyconversion},
        {"Zone Exfiltration", state->dataHeatBal->BuildingPreDefRep.emiZoneExfiltration * energyconversion},
        {"Zone Exhaust Air", state->dataHeatBal->BuildingPreDefRep.emiZoneExhaust * energyconversion},
        {"HVAC Relief Air", state->dataHeatBal->BuildingPreDefRep.emiHVACRelief * energyconversion},
        {"HVAC Reject Heat", state->dataHeatBal->BuildingPreDefRep.emiHVACReject * energyconversion},
        {"Total", state->dataHeatBal->BuildingPreDefRep.emiTotHeat * energyconversion},

    });

    for (auto &v : results0) {

        std::string columnName = std::get<0>(v);
        Real64 expectedValue = std::get<1>(v);

        for (auto &rowName : testRowNames) {
            std::string query("SELECT Value From TabularDataWithStrings"
                              "  WHERE ReportName = '" +
                              reportName +
                              "'"
                              "  AND TableName = '" +
                              tableName +
                              "'"
                              "  AND RowName = '" +
                              rowName +
                              "'"
                              "  AND ColumnName = '" +
                              columnName +
                              "'"
                              "  AND Units = '" +
                              unitsName + "'");

            Real64 return_val = execAndReturnFirstDouble(query);

            // Add informative message if failed
            EXPECT_NEAR(expectedValue, return_val, 0.01) << "Failed for TableName=" << tableName << "; RowName=" << rowName;
        }
    }

    // Test Combination 1: None
    state->dataOutRptTab->unitsStyle = iUnitsStyle::None;
    state->dataOutRptTab->unitsStyle_SQLite = iUnitsStyle::None;
    energyconversion = 1.0;

    WriteHeatEmissionTable(*state);

    // Now test the reporting:
    // const std::string reportName = "AnnualHeatEmissionsReport";
    // const std::string tableName = "Annual Heat Emissions Summary";
    unitsName = "GJ";

    // Test the row of heat emissions
    // std::vector<std::string> testRowNames = {"Heat Emissions"};

    // TableName, value
    std::vector<std::tuple<std::string, Real64>> results1({
        {"Envelope Convection", state->dataHeatBal->BuildingPreDefRep.emiEnvelopConv * energyconversion},
        {"Zone Exfiltration", state->dataHeatBal->BuildingPreDefRep.emiZoneExfiltration * energyconversion},
        {"Zone Exhaust Air", state->dataHeatBal->BuildingPreDefRep.emiZoneExhaust * energyconversion},
        {"HVAC Relief Air", state->dataHeatBal->BuildingPreDefRep.emiHVACRelief * energyconversion},
        {"HVAC Reject Heat", state->dataHeatBal->BuildingPreDefRep.emiHVACReject * energyconversion},
        {"Total", state->dataHeatBal->BuildingPreDefRep.emiTotHeat * energyconversion},

    });

    for (auto &v : results1) {

        std::string columnName = std::get<0>(v);
        Real64 expectedValue = std::get<1>(v);

        for (auto &rowName : testRowNames) {
            std::string query("SELECT Value From TabularDataWithStrings"
                              "  WHERE ReportName = '" +
                              reportName +
                              "'"
                              "  AND TableName = '" +
                              tableName +
                              "'"
                              "  AND RowName = '" +
                              rowName +
                              "'"
                              "  AND ColumnName = '" +
                              columnName +
                              "'"
                              "  AND Units = '" +
                              unitsName + "'");

            Real64 return_val = execAndReturnFirstDouble(query);

            // Add informative message if failed
            EXPECT_NEAR(expectedValue, return_val, 0.01) << "Failed for TableName=" << tableName << "; RowName=" << rowName;
        }
    }

    // Test Combination 2:
    state->dataOutRptTab->unitsStyle = iUnitsStyle::JtoKWH;
    state->dataOutRptTab->unitsStyle_SQLite = iUnitsStyle::InchPound;

    // Test 2.5:
    // Actually here is an additonal test unit for the getSpecificUnitDivider:
    SetupUnitConversions(*state);
    Real64 rconv = getSpecificUnitDivider(*state, "GJ", "kBtu");
    energyconversion = 1.0 / rconv; // 948.45
    EXPECT_NEAR(energyconversion, 948.0, 0.5);

    WriteHeatEmissionTable(*state);

    // Now test the reporting:
    // const std::string reportName = "AnnualHeatEmissionsReport";
    // const std::string tableName = "Annual Heat Emissions Summary";
    unitsName = "kBtu";

    // Test the row of heat emissions
    // std::vector<std::string> testRowNames = {"Heat Emissions"};

    // TableName, value
    std::vector<std::tuple<std::string, Real64>> results2({
        {"Envelope Convection", state->dataHeatBal->BuildingPreDefRep.emiEnvelopConv * energyconversion},
        {"Zone Exfiltration", state->dataHeatBal->BuildingPreDefRep.emiZoneExfiltration * energyconversion},
        {"Zone Exhaust Air", state->dataHeatBal->BuildingPreDefRep.emiZoneExhaust * energyconversion},
        {"HVAC Relief Air", state->dataHeatBal->BuildingPreDefRep.emiHVACRelief * energyconversion},
        {"HVAC Reject Heat", state->dataHeatBal->BuildingPreDefRep.emiHVACReject * energyconversion},
        {"Total", state->dataHeatBal->BuildingPreDefRep.emiTotHeat * energyconversion},

    });

    for (auto &v : results2) {

        std::string columnName = std::get<0>(v);
        Real64 expectedValue = std::get<1>(v);

        for (auto &rowName : testRowNames) {
            std::string query("SELECT Value From TabularDataWithStrings"
                              "  WHERE ReportName = '" +
                              reportName +
                              "'"
                              "  AND TableName = '" +
                              tableName +
                              "'"
                              "  AND RowName = '" +
                              rowName +
                              "'"
                              "  AND ColumnName = '" +
                              columnName +
                              "'"
                              "  AND Units = '" +
                              unitsName + "'");

            Real64 return_val = execAndReturnFirstDouble(query);

            // Add informative message if failed
            EXPECT_NEAR(expectedValue, return_val, 0.01) << "Failed for TableName=" << tableName << "; RowName=" << rowName;
        }
    }

    state->dataSQLiteProcedures->sqlite->sqliteCommit();
}

TEST_F(SQLiteFixture, WriteSourceEnergyEndUseSummary_DualUnits)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->displaySourceEnergyEndUseSummary = true;

    // DetermineBuildingFloorArea
    state->dataEnvrn->Latitude = 12.3;
    state->dataEnvrn->Longitude = 45.6;

    state->dataSurface->TotSurfaces = 4;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    // walls
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(1).Azimuth = 0.;
    state->dataSurface->Surface(1).GrossArea = 200.; // 20 x 10
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Zone = 1;

    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(2).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(2).Azimuth = 90.;
    state->dataSurface->Surface(2).GrossArea = 300.; // 30 x 10
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Zone = 2;

    // windows
    state->dataSurface->Surface(3).Class = SurfaceClass::Window;
    state->dataSurface->Surface(3).HeatTransSurf = true;
    state->dataSurface->Surface(3).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(3).Azimuth = 0.;
    state->dataSurface->Surface(3).GrossArea = 40.;
    state->dataSurface->Surface(3).Height = 5;
    state->dataSurface->Surface(3).Width = 8;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Zone = 1;

    state->dataSurface->Surface(4).Class = SurfaceClass::Window;
    state->dataSurface->Surface(4).HeatTransSurf = true;
    state->dataSurface->Surface(4).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(4).Azimuth = 90.;
    state->dataSurface->Surface(4).GrossArea = 60.;
    state->dataSurface->Surface(4).Height = 6;
    state->dataSurface->Surface(4).Width = 10;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Zone = 2;

    // Loads
    state->dataHeatBal->TotLights = 3;
    state->dataHeatBal->Lights.allocate(state->dataHeatBal->TotLights);

    state->dataHeatBal->TotPeople = 3;
    state->dataHeatBal->People.allocate(state->dataHeatBal->TotPeople);

    state->dataHeatBal->TotElecEquip = 3;
    state->dataHeatBal->ZoneElectric.allocate(state->dataHeatBal->TotElecEquip);

    state->dataHeatBal->Lights(1).ZonePtr = 1;
    state->dataHeatBal->Lights(1).DesignLevel = 1000.0;
    state->dataHeatBal->Lights(2).ZonePtr = 2;
    state->dataHeatBal->Lights(2).DesignLevel = 100.0;
    state->dataHeatBal->Lights(3).ZonePtr = 3;
    state->dataHeatBal->Lights(3).DesignLevel = 10.0;

    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->People(1).NumberOfPeople = 10.0;
    state->dataHeatBal->People(2).ZonePtr = 2;
    state->dataHeatBal->People(2).NumberOfPeople = 5.0;
    state->dataHeatBal->People(3).ZonePtr = 3;
    state->dataHeatBal->People(3).NumberOfPeople = 1.0;

    state->dataHeatBal->ZoneElectric(1).ZonePtr = 1;
    state->dataHeatBal->ZoneElectric(1).DesignLevel = 500.0;
    state->dataHeatBal->ZoneElectric(2).ZonePtr = 2;
    state->dataHeatBal->ZoneElectric(2).DesignLevel = 50.0;
    state->dataHeatBal->ZoneElectric(3).ZonePtr = 3;
    state->dataHeatBal->ZoneElectric(3).DesignLevel = 5.0;

    // zone
    state->dataGlobal->NumOfZones = 3;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "PartofTot Conditioned Zone";
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 1; // Conditioned
    state->dataHeatBal->Zone(1).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(1).Multiplier = 1.;
    state->dataHeatBal->Zone(1).ListMultiplier = 1.;
    state->dataHeatBal->Zone(1).FloorArea = 1000.;
    state->dataHeatBal->Zone(1).Volume = 2000.;
    state->dataHeatBal->Zone(1).ExtGrossWallArea = 800.;
    state->dataHeatBal->Zone(1).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(1).ExtWindowArea = state->dataSurface->Surface(3).GrossArea + state->dataSurface->Surface(4).GrossArea;

    state->dataHeatBal->Zone(2).Name = "PartofTot Unconditioned Zone";
    state->dataHeatBal->Zone(2).SystemZoneNodeNumber = 0; // Unconditioned
    state->dataHeatBal->Zone(2).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(2).Multiplier = 1.;
    state->dataHeatBal->Zone(2).ListMultiplier = 1.;
    state->dataHeatBal->Zone(2).FloorArea = 100.;
    state->dataHeatBal->Zone(2).Volume = 200.;
    state->dataHeatBal->Zone(2).ExtGrossWallArea = 80.;
    state->dataHeatBal->Zone(2).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(2).ExtWindowArea = 0.0;

    state->dataHeatBal->Zone(3).Name = "NOT PartofTot Conditioned Zone";
    state->dataHeatBal->Zone(3).SystemZoneNodeNumber = 1; // Conditioned
    state->dataHeatBal->Zone(3).isPartOfTotalArea = false;
    state->dataHeatBal->Zone(3).Multiplier = 1.;
    state->dataHeatBal->Zone(3).ListMultiplier = 1.;
    state->dataHeatBal->Zone(3).FloorArea = 10.;
    state->dataHeatBal->Zone(3).Volume = 20.;
    state->dataHeatBal->Zone(3).ExtGrossWallArea = 8.;
    state->dataHeatBal->Zone(3).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(3).ExtWindowArea = 0.0;

    // Gross takes all that are PartOfTot
    Real64 expectedBuildingGrossFloorArea = state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea;
    // Conditioned takes only PartOfTot AND COnditioned
    Real64 expectedBuildingConditionedFloorArea = state->dataHeatBal->Zone(1).FloorArea;

    // Assume that we only have electricity with a value of 3.6e6 * 1e4 J =10.000 kWh.
    // And that this only comes for a single end use state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::Heating)=1
    state->dataOutRptTab->gatherEndUseBySourceBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::Heating)) = 3.6e10;
    state->dataOutRptTab->gatherTotalsBySourceBEPS(1) = 3.6e10;
    Real64 eleckWh = 1e4;

    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::JtoKWH;
    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::InchPound;

    SetupUnitConversions(*state);
    Real64 largeConv = getSpecificUnitDivider(*state, "J", "kBtu");
    largeConv /= (3.6e6);
    Real64 areaConv = getSpecificUnitDivider(*state, "m2", "ft2");

    // Now we're ready to call the actual function of interest
    OutputReportTabular::WriteSourceEnergyEndUseSummary(*state);

    // Before we test the reporting itself, we check that DetermineBuildingFloorArea (called from WriteSourceEnergyEndUseSummary)
    // actually did what we expected
    EXPECT_EQ(expectedBuildingGrossFloorArea, state->dataOutRptTab->buildingGrossFloorArea);
    EXPECT_EQ(expectedBuildingConditionedFloorArea, state->dataOutRptTab->buildingConditionedFloorArea);

    // expectedBuildingGrossFloorArea /= areaConv;
    // expectedBuildingConditionedFloorArea /= areaConv;

    // eleckWh /= largeConv;

    // Now we test the reporting itself:
    // We consistently test in the same report (three different tables) and at the same column for fuel = Elec
    const std::string reportName = "SourceEnergyEndUseComponentsSummary";
    const std::string columnName = "Source Electricity";

    // We test for Heating and Total, since they should be the same
    std::vector<std::string> testRowNames = {"Heating", "Total Source Energy End Use Components"};

    // TableName, value
    std::vector<std::tuple<std::string, Real64>> results({
        {"Source Energy End Use Components Summary", eleckWh / largeConv},
        {"Source Energy End Use Component Per Conditioned Floor Area", 10000.0 / largeConv / (expectedBuildingConditionedFloorArea / areaConv)},
        {"Source Energy End Use Components Per Total Floor Area", 10000.0 / largeConv / (expectedBuildingGrossFloorArea / areaConv)},
    });

    for (auto &v : results) {

        std::string tableName = std::get<0>(v);
        Real64 expectedValue = std::get<1>(v);

        for (auto &rowName : testRowNames) {
            std::string query("SELECT Value From TabularDataWithStrings"
                              "  WHERE ReportName = '" +
                              reportName +
                              "'"
                              "  AND TableName = '" +
                              tableName +
                              "'"
                              "  AND RowName = '" +
                              rowName + "'" + "  AND ColumnName = '" + columnName + "'");

            Real64 return_val = execAndReturnFirstDouble(query);

            // Add informative message if failed
            EXPECT_NEAR(expectedValue, return_val, 0.01) << "Failed for TableName=" << tableName << "; RowName=" << rowName;
        }
    }

    state->dataSQLiteProcedures->sqlite->sqliteCommit();
}

TEST_F(EnergyPlusFixture, ORT_LoadSummaryUnitConversion_OverLoad_DualUnits)
{
    CompLoadTablesType compLoad;
    compLoad.cells.allocate(cPerArea, rGrdTot);
    compLoad.cells = 0.;
    compLoad.cellUsed.allocate(cPerArea, rGrdTot);
    compLoad.cellUsed = true;

    compLoad.cells(cSensInst, rLights) = 3.;
    compLoad.cells(cLatent, rLights) = 10.;

    compLoad.cells(cArea, rLights) = 5.;

    compLoad.outsideDryBulb = 20.;
    compLoad.mainFanAirFlow = 0.7;
    compLoad.airflowPerTotCap = 0.2;
    compLoad.totCapPerArea = 0.15;

    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::InchPound;
    Real64 powerConversion = getSpecificUnitMultiplier(*state, "W", "Btu/h");
    Real64 areaConversion = getSpecificUnitMultiplier(*state, "m2", "ft2");
    Real64 airFlowConversion = getSpecificUnitMultiplier(*state, "m3/s", "ft3/min");
    Real64 airFlowPerAreaConversion = getSpecificUnitMultiplier(*state, "m3/s-m2", "ft3/min-ft2");
    int tempConvIndx = getSpecificUnitIndex(*state, "C", "F");

    // LoadSummaryUnitConversion(*state, compLoad);
    LoadSummaryUnitConversion(*state, compLoad, state->dataOutRptTab->unitsStyle_SQLite);

    EXPECT_EQ(3. * powerConversion, compLoad.cells(cSensInst, rLights));
    EXPECT_EQ(10. * powerConversion, compLoad.cells(cLatent, rLights));
    EXPECT_EQ(5. * areaConversion, compLoad.cells(cArea, rLights));
    EXPECT_EQ(5. * areaConversion, compLoad.cells(cArea, rLights));

    EXPECT_EQ(ConvertIP(*state, tempConvIndx, 20.), compLoad.outsideDryBulb);
    EXPECT_EQ(0.7 * airFlowConversion, compLoad.mainFanAirFlow);
    EXPECT_EQ(0.2 * airFlowPerAreaConversion / powerConversion, compLoad.airflowPerTotCap);
    EXPECT_EQ(0.15 * powerConversion / areaConversion, compLoad.totCapPerArea);
}

TEST_F(SQLiteFixture, WriteVeriSumTableAreasTest_DualUnits)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->displayTabularVeriSum = true;
    state->dataEnvrn->Latitude = 12.3;
    state->dataEnvrn->Longitude = 45.6;

    state->dataSurface->TotSurfaces = 4;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    // walls
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(1).Azimuth = 0.;
    state->dataSurface->Surface(1).GrossArea = 200.; // 20 x 10
    state->dataSurface->Surface(1).FrameDivider = 0;
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Zone = 1;

    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(2).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(2).Azimuth = 90.;
    state->dataSurface->Surface(2).GrossArea = 300.; // 30 x 10
    state->dataSurface->Surface(2).FrameDivider = 0;
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Zone = 1;

    // windows
    state->dataSurface->Surface(3).Class = SurfaceClass::Window;
    state->dataSurface->Surface(3).HeatTransSurf = true;
    state->dataSurface->Surface(3).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(3).Azimuth = 0.;
    state->dataSurface->Surface(3).GrossArea = 40.;
    state->dataSurface->Surface(3).Height = 5;
    state->dataSurface->Surface(3).Width = 8;
    state->dataSurface->Surface(3).FrameDivider = 1;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Zone = 1;

    state->dataSurface->Surface(4).Class = SurfaceClass::Window;
    state->dataSurface->Surface(4).HeatTransSurf = true;
    state->dataSurface->Surface(4).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(4).Azimuth = 90.;
    state->dataSurface->Surface(4).GrossArea = 60.;
    state->dataSurface->Surface(4).Height = 6;
    state->dataSurface->Surface(4).Width = 10;
    state->dataSurface->Surface(4).FrameDivider = 2;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Zone = 1;

    // frames
    state->dataHeatBal->TotFrameDivider = 2;
    state->dataSurface->FrameDivider.allocate(state->dataHeatBal->TotFrameDivider);
    state->dataSurface->FrameDivider(1).FrameWidth = 0.3;
    state->dataSurface->FrameDivider(2).FrameWidth = 0.2;

    // zone
    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 1;
    state->dataHeatBal->Zone(1).Multiplier = 1.;
    state->dataHeatBal->Zone(1).ListMultiplier = 1.;
    state->dataHeatBal->Zone(1).FloorArea = 600.; // 20 x 30
    state->dataHeatBal->Zone(1).Volume = 6000.;   // 20 x 30 x 10
    state->dataHeatBal->Zone(1).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(1).ExtGrossWallArea = 500.;
    state->dataHeatBal->Zone(1).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(1).ExtWindowArea = state->dataSurface->Surface(3).GrossArea + state->dataSurface->Surface(4).GrossArea;

    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::None;
    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::InchPound;

    SetupUnitConversions(*state);
    Real64 areaConv = getSpecificUnitDivider(*state, "m2", "ft2");
    Real64 volConv = getSpecificUnitDivider(*state, "m3", "ft3");

    WriteVeriSumTable(*state);

    auto tabularData = queryResult("SELECT * FROM TabularData;", "TabularData");
    auto strings = queryResult("SELECT * FROM Strings;", "Strings");
    auto stringTypes = queryResult("SELECT * FROM StringTypes;", "StringTypes");
    state->dataSQLiteProcedures->sqlite->sqliteCommit();

    EXPECT_EQ(123ul, tabularData.size());
    // tabularDataIndex, reportNameIndex, reportForStringIndex, tableNameIndex, rowLabelIndex, columnLabelIndex, unitsIndex, simulationIndex, rowId,
    // columnId, value
    EXPECT_EQ("       12.30", tabularData[3][10]);
    EXPECT_EQ("       45.60", tabularData[4][10]);
    // envelope - window-wall ratio subtable
    // north
    EXPECT_NEAR(200.00 / areaConv, std::stod(tabularData[15][10]), 0.01);
    EXPECT_NEAR(200.00 / areaConv, std::stod(tabularData[16][10]), 0.01);
    EXPECT_NEAR(48.16 / areaConv, std::stod(tabularData[17][10]), 0.01);
    EXPECT_EQ("       24.08", tabularData[18][10]);
    EXPECT_EQ("       24.08", tabularData[19][10]);
    // east
    EXPECT_NEAR(300.00 / areaConv, std::stod(tabularData[20][10]), 0.01);
    EXPECT_NEAR(300.00 / areaConv, std::stod(tabularData[21][10]), 0.01);
    EXPECT_NEAR(66.56 / areaConv, std::stod(tabularData[22][10]), 0.01);
    EXPECT_EQ("       22.19", tabularData[23][10]);
    EXPECT_EQ("       22.19", tabularData[24][10]);
    // Performance - zone summary table
    EXPECT_NEAR(600.00 / areaConv, std::stod(tabularData[63][10]), 0.01);
    EXPECT_EQ("Yes", tabularData[68][10]); // conditioned
    EXPECT_EQ("Yes", tabularData[73][10]); // part of total floor area
    EXPECT_NEAR(6000.00 / volConv, std::stod(tabularData[78][10]), 0.01);
    EXPECT_EQ("        1.00", tabularData[83][10]); // multiplier
    EXPECT_NEAR(500.00 / areaConv, std::stod(tabularData[88][10]), 0.01);
    EXPECT_NEAR(100.00 / areaConv, std::stod(tabularData[98][10]), 0.01);
    EXPECT_NEAR(114.72 / areaConv, std::stod(tabularData[103][10]), 0.01);
}

// Dual Unit test: Borrowed from Test for #6350 and #6469
TEST_F(SQLiteFixture, WriteVeriSumTable_TestNotPartOfTotal_DualUnits)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->displayTabularVeriSum = true;
    state->dataEnvrn->Latitude = 12.3;
    state->dataEnvrn->Longitude = 45.6;

    state->dataSurface->TotSurfaces = 4;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);

    // walls
    state->dataSurface->Surface(1).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(1).Azimuth = 0.;
    state->dataSurface->Surface(1).GrossArea = 200.; // 20 x 10
    state->dataSurface->Surface(1).Tilt = 90.;
    state->dataSurface->Surface(1).Zone = 1;

    state->dataSurface->Surface(2).Class = SurfaceClass::Wall;
    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(2).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(2).Azimuth = 90.;
    state->dataSurface->Surface(2).GrossArea = 300.; // 30 x 10
    state->dataSurface->Surface(2).Tilt = 90.;
    state->dataSurface->Surface(2).Zone = 2;

    // windows
    state->dataSurface->Surface(3).Class = SurfaceClass::Window;
    state->dataSurface->Surface(3).HeatTransSurf = true;
    state->dataSurface->Surface(3).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(3).Azimuth = 0.;
    state->dataSurface->Surface(3).GrossArea = 40.;
    state->dataSurface->Surface(3).Height = 5;
    state->dataSurface->Surface(3).Width = 8;
    state->dataSurface->Surface(3).Tilt = 90.;
    state->dataSurface->Surface(3).Zone = 1;

    state->dataSurface->Surface(4).Class = SurfaceClass::Window;
    state->dataSurface->Surface(4).HeatTransSurf = true;
    state->dataSurface->Surface(4).ExtBoundCond = ExternalEnvironment;
    state->dataSurface->Surface(4).Azimuth = 90.;
    state->dataSurface->Surface(4).GrossArea = 60.;
    state->dataSurface->Surface(4).Height = 6;
    state->dataSurface->Surface(4).Width = 10;
    state->dataSurface->Surface(4).Tilt = 90.;
    state->dataSurface->Surface(4).Zone = 2;

    // Loads
    state->dataHeatBal->TotLights = 3;
    state->dataHeatBal->Lights.allocate(state->dataHeatBal->TotLights);

    state->dataHeatBal->TotPeople = 3;
    state->dataHeatBal->People.allocate(state->dataHeatBal->TotPeople);

    state->dataHeatBal->TotElecEquip = 3;
    state->dataHeatBal->ZoneElectric.allocate(state->dataHeatBal->TotElecEquip);

    state->dataHeatBal->Lights(1).ZonePtr = 1;
    state->dataHeatBal->Lights(1).DesignLevel = 1000.0;
    state->dataHeatBal->Lights(2).ZonePtr = 2;
    state->dataHeatBal->Lights(2).DesignLevel = 100.0;
    state->dataHeatBal->Lights(3).ZonePtr = 3;
    state->dataHeatBal->Lights(3).DesignLevel = 10.0;

    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->People(1).NumberOfPeople = 10.0;
    state->dataHeatBal->People(2).ZonePtr = 2;
    state->dataHeatBal->People(2).NumberOfPeople = 5.0;
    state->dataHeatBal->People(3).ZonePtr = 3;
    state->dataHeatBal->People(3).NumberOfPeople = 1.0;

    state->dataHeatBal->ZoneElectric(1).ZonePtr = 1;
    state->dataHeatBal->ZoneElectric(1).DesignLevel = 500.0;
    state->dataHeatBal->ZoneElectric(2).ZonePtr = 2;
    state->dataHeatBal->ZoneElectric(2).DesignLevel = 50.0;
    state->dataHeatBal->ZoneElectric(3).ZonePtr = 3;
    state->dataHeatBal->ZoneElectric(3).DesignLevel = 5.0;

    // zone
    state->dataGlobal->NumOfZones = 3;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "PartofTot Conditioned Zone";
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 1; // Conditioned
    state->dataHeatBal->Zone(1).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(1).Multiplier = 1.;
    state->dataHeatBal->Zone(1).ListMultiplier = 1.;
    // 10x10x2
    state->dataHeatBal->Zone(1).FloorArea = 1000.;
    state->dataHeatBal->Zone(1).Volume = 2000.;
    state->dataHeatBal->Zone(1).ExtGrossWallArea = 800.;
    state->dataHeatBal->Zone(1).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(1).ExtWindowArea = state->dataSurface->Surface(3).GrossArea + state->dataSurface->Surface(4).GrossArea;

    state->dataHeatBal->Zone(2).Name = "PartofTot Unconditioned Zone";
    state->dataHeatBal->Zone(2).SystemZoneNodeNumber = 0; // Unconditioned
    state->dataHeatBal->Zone(2).isPartOfTotalArea = true;
    state->dataHeatBal->Zone(2).Multiplier = 1.;
    state->dataHeatBal->Zone(2).ListMultiplier = 1.;
    // 10x10x2
    state->dataHeatBal->Zone(2).FloorArea = 100.;
    state->dataHeatBal->Zone(2).Volume = 200.;
    state->dataHeatBal->Zone(2).ExtGrossWallArea = 80.;
    state->dataHeatBal->Zone(2).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(2).ExtWindowArea = 0.0;

    state->dataHeatBal->Zone(3).Name = "NOT PartofTot Conditioned Zone";
    state->dataHeatBal->Zone(3).SystemZoneNodeNumber = 1; // Conditioned
    state->dataHeatBal->Zone(3).isPartOfTotalArea = false;
    state->dataHeatBal->Zone(3).Multiplier = 1.;
    state->dataHeatBal->Zone(3).ListMultiplier = 1.;
    // 10x10x2
    state->dataHeatBal->Zone(3).FloorArea = 10.;
    state->dataHeatBal->Zone(3).Volume = 20.;
    state->dataHeatBal->Zone(3).ExtGrossWallArea = 8.;
    state->dataHeatBal->Zone(3).ExteriorTotalGroundSurfArea = 0;
    state->dataHeatBal->Zone(3).ExtWindowArea = 0.0;

    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::None;
    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::InchPound;

    SetupUnitConversions(*state);
    Real64 areaConv = getSpecificUnitDivider(*state, "m2", "ft2");
    Real64 volConv = getSpecificUnitDivider(*state, "m3", "ft3");

    std::string SIunit = "[W/m2]";
    int unitConvIndex;
    std::string Wm2_unitName;
    Real64 Wm2_unitConv;
    LookupSItoIP(*state, SIunit, unitConvIndex, Wm2_unitName);
    Wm2_unitConv = ConvertIP(*state, unitConvIndex, 1.0);

    WriteVeriSumTable(*state);

    /***********************************************************************************************************************************************
     *                                                              Check Yes/No flag                                                              *
     ***********************************************************************************************************************************************/

    // RowName, ColumnName, value
    std::vector<std::tuple<std::string, std::string, std::string>> results_strings({
        {state->dataHeatBal->Zone(1).Name, "Conditioned (Y/N)", "Yes"},
        {state->dataHeatBal->Zone(1).Name, "Part of Total Floor Area (Y/N)", "Yes"},

        {state->dataHeatBal->Zone(2).Name, "Conditioned (Y/N)", "No"},
        {state->dataHeatBal->Zone(2).Name, "Part of Total Floor Area (Y/N)", "Yes"},

        {state->dataHeatBal->Zone(3).Name, "Conditioned (Y/N)", "Yes"},
        {state->dataHeatBal->Zone(3).Name, "Part of Total Floor Area (Y/N)", "No"},
    });

    // Would have used bind_text in sqlite3 with a single prepared statement, but m_db is protected in SQLiteProcedures
    std::string rowName;
    std::string columnName;

    for (auto v : results_strings) {

        rowName = std::get<0>(v);
        columnName = std::get<1>(v);

        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE ReportName = 'InputVerificationandResultsSummary'"
                          "  AND TableName = 'Zone Summary'"
                          "  AND RowName = '" +
                          rowName + "'" + "  AND ColumnName = '" + columnName + "'");

        std::string flag = queryResult(query, "TabularDataWithStrings")[0][0];
        // Not needed, we're just querying, not inside a transaction
        // state->dataSQLiteProcedures->sqlite->sqliteCommit();

        // Add informative message if failed
        EXPECT_EQ(std::get<2>(v), flag) << "Failed for RowName=" << rowName << "; ColumnName=" << columnName;
    }

    /***********************************************************************************************************************************************
     *                                                       Check each zone and total rows                                                        *
     ***********************************************************************************************************************************************/

    // RowName, ColumnName, value
    std::vector<std::tuple<std::string, std::string, Real64>> results({
        {state->dataHeatBal->Zone(1).Name, "Area", state->dataHeatBal->Zone(1).FloorArea / areaConv},
        {state->dataHeatBal->Zone(2).Name, "Area", state->dataHeatBal->Zone(2).FloorArea / areaConv},
        {state->dataHeatBal->Zone(3).Name, "Area", state->dataHeatBal->Zone(3).FloorArea / areaConv},
        {"Total", "Area", (state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea) / areaConv},
        {"Conditioned Total", "Area", state->dataHeatBal->Zone(1).FloorArea / areaConv},
        {"Unconditioned Total", "Area", state->dataHeatBal->Zone(2).FloorArea / areaConv},
        {"Not Part of Total", "Area", state->dataHeatBal->Zone(3).FloorArea / areaConv},

        {state->dataHeatBal->Zone(1).Name, "Volume", state->dataHeatBal->Zone(1).Volume / volConv},
        {state->dataHeatBal->Zone(2).Name, "Volume", state->dataHeatBal->Zone(2).Volume / volConv},
        {state->dataHeatBal->Zone(3).Name, "Volume", state->dataHeatBal->Zone(3).Volume / volConv},
        {"Total", "Volume", (state->dataHeatBal->Zone(1).Volume + state->dataHeatBal->Zone(2).Volume) / volConv},
        {"Conditioned Total", "Volume", state->dataHeatBal->Zone(1).Volume / volConv},
        {"Unconditioned Total", "Volume", state->dataHeatBal->Zone(2).Volume / volConv},
        {"Not Part of Total", "Volume", state->dataHeatBal->Zone(3).Volume / volConv},

        {state->dataHeatBal->Zone(1).Name,
         "Lighting",
         state->dataHeatBal->Lights(1).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(1).FloorArea},
        {state->dataHeatBal->Zone(2).Name,
         "Lighting",
         state->dataHeatBal->Lights(2).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(2).FloorArea},
        {state->dataHeatBal->Zone(3).Name,
         "Lighting",
         state->dataHeatBal->Lights(3).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(3).FloorArea},
        {"Total",
         "Lighting",
         (state->dataHeatBal->Lights(1).DesignLevel + state->dataHeatBal->Lights(2).DesignLevel) * Wm2_unitConv /
             (state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea)},
        {"Conditioned Total", "Lighting", state->dataHeatBal->Lights(1).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(1).FloorArea},
        {"Unconditioned Total", "Lighting", state->dataHeatBal->Lights(2).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(2).FloorArea},
        {"Not Part of Total", "Lighting", state->dataHeatBal->Lights(3).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(3).FloorArea},

        // People/m^2
        {state->dataHeatBal->Zone(1).Name, "People", state->dataHeatBal->Zone(1).FloorArea / areaConv / state->dataHeatBal->People(1).NumberOfPeople},
        {state->dataHeatBal->Zone(2).Name, "People", state->dataHeatBal->Zone(2).FloorArea / areaConv / state->dataHeatBal->People(2).NumberOfPeople},
        {state->dataHeatBal->Zone(3).Name, "People", state->dataHeatBal->Zone(3).FloorArea / areaConv / state->dataHeatBal->People(3).NumberOfPeople},
        {"Total",
         "People",
         (state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea) / areaConv /
             (state->dataHeatBal->People(1).NumberOfPeople + state->dataHeatBal->People(2).NumberOfPeople)},
        {"Conditioned Total", "People", state->dataHeatBal->Zone(1).FloorArea / areaConv / state->dataHeatBal->People(1).NumberOfPeople},
        {"Unconditioned Total", "People", state->dataHeatBal->Zone(2).FloorArea / areaConv / state->dataHeatBal->People(2).NumberOfPeople},
        {"Not Part of Total", "People", state->dataHeatBal->Zone(3).FloorArea / areaConv / state->dataHeatBal->People(3).NumberOfPeople},

        {state->dataHeatBal->Zone(1).Name,
         "Plug and Process",
         state->dataHeatBal->ZoneElectric(1).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(1).FloorArea},
        {state->dataHeatBal->Zone(2).Name,
         "Plug and Process",
         state->dataHeatBal->ZoneElectric(2).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(2).FloorArea},
        {state->dataHeatBal->Zone(3).Name,
         "Plug and Process",
         state->dataHeatBal->ZoneElectric(3).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(3).FloorArea},
        {"Total",
         "Plug and Process",
         (state->dataHeatBal->ZoneElectric(1).DesignLevel + state->dataHeatBal->ZoneElectric(2).DesignLevel) * Wm2_unitConv /
             (state->dataHeatBal->Zone(1).FloorArea + state->dataHeatBal->Zone(2).FloorArea)},
        {"Conditioned Total",
         "Plug and Process",
         state->dataHeatBal->ZoneElectric(1).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(1).FloorArea},
        {"Unconditioned Total",
         "Plug and Process",
         state->dataHeatBal->ZoneElectric(2).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(2).FloorArea},
        {"Not Part of Total",
         "Plug and Process",
         state->dataHeatBal->ZoneElectric(3).DesignLevel * Wm2_unitConv / state->dataHeatBal->Zone(3).FloorArea},
    });

    // Would have used bind_text in sqlite3 with a single prepared statement, but m_db is protected in SQLiteProcedures
    for (auto v : results) {

        rowName = std::get<0>(v);
        columnName = std::get<1>(v);

        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE ReportName = 'InputVerificationandResultsSummary'"
                          "  AND TableName = 'Zone Summary'"
                          "  AND RowName = '" +
                          rowName + "'" + "  AND ColumnName = '" + columnName + "'");

        Real64 return_val = execAndReturnFirstDouble(query);
        // state->dataSQLiteProcedures->sqlite->sqliteCommit();

        // Add informative message if failed
        EXPECT_NEAR(std::get<2>(v), return_val, 0.01) << "Failed for RowName=" << rowName << "; ColumnName=" << columnName;
    }
}

TEST_F(SQLiteFixture, ORT_EndUseBySubcategorySQL_DualUnits)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    state->dataOutRptTab->displayTabularBEPS = true;
    state->dataOutRptTab->displayDemandEndUse = true;
    state->dataOutRptTab->displayLEEDSummary = true;

    state->dataOutRptTab->WriteTabularFiles = true;

    SetupUnitConversions(*state);
    state->dataOutRptTab->unitsStyle = OutputReportTabular::iUnitsStyle::JtoKWH;
    state->dataOutRptTab->unitsStyle_SQLite = OutputReportTabular::iUnitsStyle::InchPound;
    Real64 enerConv = getSpecificUnitDivider(*state, "GJ", "kBtu"); // 948.45
    EXPECT_NEAR(1.0 / enerConv, 948.0, 0.5);

    // Needed to avoid crash (from ElectricPowerServiceManager.hh)
    createFacilityElectricPowerServiceObject(*state);

    SetPredefinedTables(*state);

    Real64 extLitUse = 1e8;
    Real64 CoalHeating = 2e8;
    Real64 GasolineHeating = 3e8;
    Real64 PropaneHeating = 4e8;

    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite1",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite2",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "AnotherEndUseSubCat");
    SetupOutputVariable(*state,
                        "Exterior Lights Electricity Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        "Zone",
                        "Sum",
                        "Lite3",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(
        *state, "Heating Coal Energy", OutputProcessor::Unit::J, CoalHeating, "Zone", "Sum", "Lite4", _, "Coal", "Heating", "General");
    SetupOutputVariable(
        *state, "Heating Gasoline Energy", OutputProcessor::Unit::J, GasolineHeating, "Zone", "Sum", "Lite5", _, "Gasoline", "Heating", "General");
    SetupOutputVariable(
        *state, "Heating Propane Energy", OutputProcessor::Unit::J, PropaneHeating, "Zone", "Sum", "Lite6", _, "Propane", "Heating", "General");
    state->dataGlobal->DoWeathSim = true;
    state->dataGlobal->TimeStepZone = 1.0;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * 60.0;
    state->dataOutRptTab->displayTabularBEPS = true;
    // OutputProcessor::TimeValue.allocate(2);

    auto timeStep = 1.0;

    SetupTimePointers(*state, "Zone", timeStep);
    SetupTimePointers(*state, "HVAC", timeStep);

    *state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::TimeStepZone).TimeStep = 60;
    *state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::TimeStepSystem).TimeStep = 60;

    GetInputOutputTableSummaryReports(*state);

    state->dataEnvrn->Month = 12;

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_NEAR(extLitUse * 3,
                state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)),
                1.);
    // General
    EXPECT_NEAR(extLitUse * 2,
                state->dataOutRptTab->gatherEndUseSubBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);
    // AnotherEndUseSubCat
    EXPECT_NEAR(extLitUse * 1,
                state->dataOutRptTab->gatherEndUseSubBEPS(2, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_NEAR(extLitUse * 6,
                state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)),
                1.);
    // General
    EXPECT_NEAR(extLitUse * 4,
                state->dataOutRptTab->gatherEndUseSubBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);
    // AnotherEndUseSubCat
    EXPECT_NEAR(extLitUse * 2,
                state->dataOutRptTab->gatherEndUseSubBEPS(2, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);

    UpdateMeterReporting(*state);
    UpdateDataandReport(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherBEPSResultsForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    GatherPeakDemandForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);
    EXPECT_NEAR(extLitUse * 9,
                state->dataOutRptTab->gatherEndUseBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)),
                1.);
    // General
    EXPECT_NEAR(extLitUse * 6,
                state->dataOutRptTab->gatherEndUseSubBEPS(1, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);
    // AnotherEndUseSubCat
    EXPECT_NEAR(extLitUse * 3,
                state->dataOutRptTab->gatherEndUseSubBEPS(2, state->dataGlobalConst->iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights), 1),
                1.);

    OutputReportTabular::WriteBEPSTable(*state);
    OutputReportTabular::WriteDemandEndUseSummary(*state);

    state->dataSQLiteProcedures->sqlite->sqliteCommit();

    // We test for Heating and Total, since they should be the same
    std::vector<std::string> testReportNames = {"AnnualBuildingUtilityPerformanceSummary", "DemandEndUseComponentsSummary"};
    std::vector<std::string> endUseSubCategoryNames = {"General", "AnotherEndUseSubCat"};

    std::string endUseName = "Exterior Lighting";
    std::string endUseSubCategoryName = "AnotherEndUseSubCat";
    std::string rowName = endUseName + ":" + endUseSubCategoryName;
    std::string columnName = "Electricity";

    for (auto &endUseSubCategoryName : endUseSubCategoryNames) {
        for (auto &reportName : testReportNames) {

            std::string query("SELECT Value From TabularDataWithStrings"
                              "  WHERE TableName = 'End Uses By Subcategory'"
                              "  AND ColumnName = 'Electricity'"
                              "  AND ReportName = '" +
                              reportName +
                              "'"
                              "  AND RowName = '" +
                              endUseName + ":" + endUseSubCategoryName + "'"); // Now Like 'Exterior Lighting:General'

            auto result = queryResult(query, "TabularDataWithStrings");

            ASSERT_EQ(1ul, result.size()) << "Query crashed for reportName=" << reportName;
        }
    }

    for (auto &reportName : testReportNames) {

        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ColumnName = 'Electricity'"
                          "  AND ReportName = '" +
                          reportName +
                          "'"
                          "  AND RowName = '" +
                          endUseName + "'");

        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(1ul, result.size()) << "Query crashed for reportName=" << reportName;
    }

    // Specifically get the electricity usage for End Use = Exterior Lighting, and End Use Subcat = AnotherEndUseSubCat,
    // and make sure it's the right number that's returned
    std::string query("SELECT Value From TabularDataWithStrings"
                      "  WHERE TableName = 'End Uses By Subcategory'"
                      "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                      "  AND ColumnName = 'Electricity'"
                      "  AND RowName = 'Exterior Lighting:AnotherEndUseSubCat'");
    Real64 return_val = execAndReturnFirstDouble(query);

    // EXPECT_NEAR(extLitUse * 3 / 3.6e6, return_val, 0.01) << "Failed for query: " << query;
    Real64 expected_value = extLitUse * 3.0 / 1.0e9 / enerConv;
    EXPECT_NEAR(expected_value, return_val, 0.01) << "Failed for query: " << query;

    // Get all Interior Lighting End Uses (all subcats) for Electricity
    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses By Subcategory'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND ColumnName = 'Electricity'"
                          "  AND RowName LIKE 'Exterior Lighting:%'");
        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(2u, result.size()) << "Failed for query: " << query;
    }

    // Get all subcat usage for all fuels (13)
    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses By Subcategory'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND RowName = 'Exterior Lighting:AnotherEndUseSubCat'");
        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(13u, result.size()) << "Failed for query: " << query;
    }

    // Specifically get the each fuel (Coal, Gasoline, and Propane) usage for End Use = Heating,
    // and make sure it's the right number that's returned

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND ColumnName = 'Coal'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");
        Real64 return_val1 = execAndReturnFirstDouble(query);

        ASSERT_EQ(1u, result.size()) << "Failed for query: " << query;
        // EXPECT_NEAR(CoalHeating * 3 / 3.6e6, return_val1, 0.01) << "Failed for query: " << query;
        Real64 expected_coalHt = CoalHeating * 3 / 1.0e9 / enerConv;
        EXPECT_NEAR(expected_coalHt, return_val1, 0.01) << "Failed for query: " << query;
    }

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND ColumnName = 'Gasoline'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");
        Real64 return_val2 = execAndReturnFirstDouble(query);

        ASSERT_EQ(1u, result.size()) << "Failed for query: " << query;
        // EXPECT_NEAR(GasolineHeating * 3 / 3.6e6, return_val2, 0.01) << "Failed for query: " << query;
        EXPECT_NEAR(GasolineHeating * 3 / 1.0e9 / enerConv, return_val2, 0.01) << "Failed for query: " << query;
    }

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND ColumnName = 'Propane'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");
        Real64 return_val3 = execAndReturnFirstDouble(query);

        ASSERT_EQ(1u, result.size()) << "Failed for query: " << query;
        // EXPECT_NEAR(PropaneHeating * 3 / 3.6e6, return_val3, 0.01) << "Failed for query: " << query;
        EXPECT_NEAR(PropaneHeating * 3 / 1.0e9 / enerConv, return_val3, 0.01) << "Failed for query: " << query;
    }

    // Check the heating category has the result size of 13 (including all disaggregated additional fuels) in both reports)

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'AnnualBuildingUtilityPerformanceSummary'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(13u, result.size()) << "Failed for query: " << query;
    }

    {
        std::string query("SELECT Value From TabularDataWithStrings"
                          "  WHERE TableName = 'End Uses'"
                          "  AND ReportName = 'DemandEndUseComponentsSummary'"
                          "  AND RowName = 'Heating'");
        auto result = queryResult(query, "TabularDataWithStrings");

        ASSERT_EQ(13u, result.size()) << "Failed for query: " << query;
    }
}

TEST_F(SQLiteFixture, OutputReportTabularTest_EscapeHTML)
{
    // Test for #8542 - Ensures strings are escaped before going to HTML
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    auto &ort(state->dataOutRptTab);
    ort->numStyles = 1;
    ort->TableStyle(1) = OutputReportTabular::iTableStyle::HTML;
    ort->del(1) = DataStringGlobals::CharSpace; // space - this is not used much for HTML output

    ort->WriteTabularFiles = true;

    SetupUnitConversions(*state);
    ort->unitsStyle = OutputReportTabular::iUnitsStyle::JtoKWH;

    SetPredefinedTables(*state);
    std::string CompName = "My Coil <coil is DX>";

    PreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilType, CompName, "Coil:Cooling:DX:SingleSpeed");
    // This would normally be called with numerics such as CompName, 0.006, 8, but I don't really care
    PreDefTableEntry(*state, state->dataOutRptPredefined->pdch2CoilLvgHumRatIdealPeak, CompName, "My Design Day where it's >= 8\u00B0");
    PreDefTableEntry(*state,
                     state->dataOutRptPredefined->pdst2CoilSummaryCoilSelection,
                     CompName,
                     "My Design Day where it's >= 8\u00B0"); // this is >= 8 degree sign

    // We enable the reports we care about, making sure we have the right ones
    EXPECT_EQ("HVACSizingSummary", state->dataOutRptPredefined->reportName(6).name);
    state->dataOutRptPredefined->reportName(6).show = true;

    OutputReportTabular::OpenOutputTabularFile(*state);

    WritePredefinedTables(*state);

    OutputReportTabular::CloseOutputTabularFile(*state);

    std::vector<std::string> lines = read_lines_in_file(state->dataStrGlobals->outputTblHtmFilePath);

    // Lambda helper to locate a line in the html file, and compare that line with the expected html after trimming
    auto compare_html_output = [this, &lines](const std::string &lookup, const std::string &expectedHTMLString) {
        std::string found_cell;
        for (const auto &line : lines) {
            if (line.find(lookup) != std::string::npos) {
                found_cell = line;
                break;
            }
        }
        EXPECT_FALSE(found_cell.empty()) << "Did not find the lookup string '" << lookup << "' string in the html output at '"
                                         << state->dataStrGlobals->outputTblHtmFilePath << "'..." << '\n'
                                         << delimited_string(lines);

        // Trim leading and trailing spaces
        found_cell.erase(0, found_cell.find_first_not_of(' ')); // ltrim
        found_cell.erase(found_cell.find_last_not_of(' ') + 1); // rtrim

        EXPECT_EQ(expectedHTMLString, found_cell) << found_cell;
    };

    compare_html_output("My Coil", "<td align=\"right\">My Coil &lt;coil is DX&gt;</td>");

    // Note that I DO NOT expect `'` to be escaped by `&apos;` like it would in xml. Technically HTML4 doesn't support that, though most browsers
    // would anyways. Also, escaping single and double quotes is only needed inside attributes
    compare_html_output("My Design Day", "<td align=\"right\">My Design Day where it's &gt;= 8&deg;</td>");

    // We ensure that SQL doesn't have the same escape
    for (const std::string reportName : {"HVACSizingSummary"}) {

        auto result = queryResult("SELECT RowName, Value From TabularDataWithStrings "
                                  "WHERE ReportName = \"" +
                                      reportName +
                                      "\""
                                      "  AND ColumnName = \"Coil Leaving Air Humidity Ratio at Ideal Loads Peak\"",
                                  "TabularDataWithStrings");

        state->dataSQLiteProcedures->sqlite->sqliteCommit();

        EXPECT_EQ(1u, result.size());
        // Because the table has 8 cols
        EXPECT_EQ(8u, result[0].size());

        // 0.006 is a ratio, so unitconv = 1
        std::string s = result[0][0];
        // Trim the string, it has leading spaces
        // s.erase(std::remove_if(s.begin(), s.end(), ::isspace), s.end());

        EXPECT_EQ("My Coil <coil is DX>", s);

        EXPECT_EQ("My Design Day where it's >= 8\u00B0", result[0][1]);
    }

    // Clean up
    FileSystem::removeFile(state->dataStrGlobals->outputTblHtmFilePath);
}
