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

// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::DataSizing;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, GetOARequirementsTest_DSOA1)
{
    bool ErrorsFound(false); // If errors detected in input
    int OAIndex(0);          // Zone number
    int NumAlphas(2);
    int NumNumbers(4);

    std::string CurrentModuleObject = "DesignSpecification:OutdoorAir";
    int NumOARequirements = 6;
    state->dataSize->OARequirements.allocate(NumOARequirements);

    Array1D_string Alphas;         // Alpha input items for object
    Array1D_string cAlphaFields;   // Alpha field names
    Array1D_string cNumericFields; // Numeric field names
    Array1D<Real64> Numbers;       // Numeric input items for object
    Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.

    Alphas.allocate(NumAlphas);
    cAlphaFields.allocate(NumAlphas);
    cNumericFields.allocate(NumNumbers);
    Numbers.dimension(NumNumbers, 0.0);
    lAlphaBlanks.dimension(NumAlphas, true);
    lNumericBlanks.dimension(NumNumbers, true);

    // Flow/Area
    OAIndex = 1;
    Alphas(1) = "Test DSOA 1"; // Name
    Alphas(2) = "Flow/Area";   // Outdoor Air Method
    Numbers(1) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
    Numbers(2) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
    Numbers(3) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
    Numbers(4) = 0.4;          // Outdoor Air Flow Air Changes per Hour

    ErrorsFound = false;
    ProcessInputOARequirements(*state,
                               CurrentModuleObject,
                               OAIndex,
                               Alphas,
                               NumAlphas,
                               Numbers,
                               NumNumbers,
                               lNumericBlanks,
                               lAlphaBlanks,
                               cAlphaFields,
                               cNumericFields,
                               ErrorsFound);

    EXPECT_FALSE(ErrorsFound);

    EXPECT_EQ(OAFlowPerArea, state->dataSize->OARequirements(OAIndex).OAFlowMethod);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowPerPerson);
    EXPECT_EQ(0.2, state->dataSize->OARequirements(OAIndex).OAFlowPerArea);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowPerZone);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowACH);

    // Flow/Person
    OAIndex = 2;
    Alphas(1) = "Test DSOA 2"; // Name
    Alphas(2) = "Flow/Person"; // Outdoor Air Method
    Numbers(1) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
    Numbers(2) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
    Numbers(3) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
    Numbers(4) = 0.4;          // Outdoor Air Flow Air Changes per Hour

    ErrorsFound = false;
    ProcessInputOARequirements(*state,
                               CurrentModuleObject,
                               OAIndex,
                               Alphas,
                               NumAlphas,
                               Numbers,
                               NumNumbers,
                               lNumericBlanks,
                               lAlphaBlanks,
                               cAlphaFields,
                               cNumericFields,
                               ErrorsFound);

    EXPECT_FALSE(ErrorsFound);

    EXPECT_EQ(OAFlowPPer, state->dataSize->OARequirements(OAIndex).OAFlowMethod);
    EXPECT_EQ(0.1, state->dataSize->OARequirements(OAIndex).OAFlowPerPerson);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowPerArea);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowPerZone);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowACH);

    // Flow/Zone
    OAIndex = 3;
    Alphas(1) = "Test DSOA 3"; // Name
    Alphas(2) = "Flow/Zone";   // Outdoor Air Method
    Numbers(1) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
    Numbers(2) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
    Numbers(3) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
    Numbers(4) = 0.4;          // Outdoor Air Flow Air Changes per Hour

    ErrorsFound = false;
    ProcessInputOARequirements(*state,
                               CurrentModuleObject,
                               OAIndex,
                               Alphas,
                               NumAlphas,
                               Numbers,
                               NumNumbers,
                               lNumericBlanks,
                               lAlphaBlanks,
                               cAlphaFields,
                               cNumericFields,
                               ErrorsFound);

    EXPECT_FALSE(ErrorsFound);

    EXPECT_EQ(OAFlow, state->dataSize->OARequirements(OAIndex).OAFlowMethod);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowPerPerson);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowPerArea);
    EXPECT_EQ(0.3, state->dataSize->OARequirements(OAIndex).OAFlowPerZone);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowACH);

    // Flow/Zone
    OAIndex = 4;
    Alphas(1) = "Test DSOA 4";     // Name
    Alphas(2) = "AirChanges/Hour"; // Outdoor Air Method
    Numbers(1) = 0.1;              // Outdoor Air Flow per Person{ m3 / s - person }
    Numbers(2) = 0.2;              // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
    Numbers(3) = 0.3;              // Outdoor Air Flow per Zone{ m3 / s }
    Numbers(4) = 0.4;              // Outdoor Air Flow Air Changes per Hour

    ErrorsFound = false;
    ProcessInputOARequirements(*state,
                               CurrentModuleObject,
                               OAIndex,
                               Alphas,
                               NumAlphas,
                               Numbers,
                               NumNumbers,
                               lNumericBlanks,
                               lAlphaBlanks,
                               cAlphaFields,
                               cNumericFields,
                               ErrorsFound);

    EXPECT_FALSE(ErrorsFound);

    EXPECT_EQ(OAFlowACH, state->dataSize->OARequirements(OAIndex).OAFlowMethod);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowPerPerson);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowPerArea);
    EXPECT_EQ(0.0, state->dataSize->OARequirements(OAIndex).OAFlowPerZone);
    EXPECT_EQ(0.4, state->dataSize->OARequirements(OAIndex).OAFlowACH);

    // Sum
    OAIndex = 5;
    Alphas(1) = "Test DSOA 5"; // Name
    Alphas(2) = "Sum";         // Outdoor Air Method
    Numbers(1) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
    Numbers(2) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
    Numbers(3) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
    Numbers(4) = 0.4;          // Outdoor Air Flow Air Changes per Hour

    ErrorsFound = false;
    ProcessInputOARequirements(*state,
                               CurrentModuleObject,
                               OAIndex,
                               Alphas,
                               NumAlphas,
                               Numbers,
                               NumNumbers,
                               lNumericBlanks,
                               lAlphaBlanks,
                               cAlphaFields,
                               cNumericFields,
                               ErrorsFound);

    EXPECT_FALSE(ErrorsFound);

    EXPECT_EQ(OAFlowSum, state->dataSize->OARequirements(OAIndex).OAFlowMethod);
    EXPECT_EQ(0.1, state->dataSize->OARequirements(OAIndex).OAFlowPerPerson);
    EXPECT_EQ(0.2, state->dataSize->OARequirements(OAIndex).OAFlowPerArea);
    EXPECT_EQ(0.3, state->dataSize->OARequirements(OAIndex).OAFlowPerZone);
    EXPECT_EQ(0.4, state->dataSize->OARequirements(OAIndex).OAFlowACH);

    // Maximum
    OAIndex = 6;
    Alphas(1) = "Test DSOA 6"; // Name
    Alphas(2) = "Maximum";     // Outdoor Air Method
    Numbers(1) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
    Numbers(2) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
    Numbers(3) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
    Numbers(4) = 0.4;          // Outdoor Air Flow Air Changes per Hour

    ErrorsFound = false;
    ProcessInputOARequirements(*state,
                               CurrentModuleObject,
                               OAIndex,
                               Alphas,
                               NumAlphas,
                               Numbers,
                               NumNumbers,
                               lNumericBlanks,
                               lAlphaBlanks,
                               cAlphaFields,
                               cNumericFields,
                               ErrorsFound);

    EXPECT_FALSE(ErrorsFound);

    EXPECT_EQ(OAFlowMax, state->dataSize->OARequirements(OAIndex).OAFlowMethod);
    EXPECT_EQ(0.1, state->dataSize->OARequirements(OAIndex).OAFlowPerPerson);
    EXPECT_EQ(0.2, state->dataSize->OARequirements(OAIndex).OAFlowPerArea);
    EXPECT_EQ(0.3, state->dataSize->OARequirements(OAIndex).OAFlowPerZone);
    EXPECT_EQ(0.4, state->dataSize->OARequirements(OAIndex).OAFlowACH);

    // Clean up
    state->dataSize->OARequirements.deallocate();
    Alphas.deallocate();
    cAlphaFields.deallocate();
    cNumericFields.deallocate();
}

TEST_F(EnergyPlusFixture, SizingManagerTest_TimeIndexToHrMinString_test)
{
    state->dataGlobal->MinutesPerTimeStep = 15;

    EXPECT_EQ("00:00:00", TimeIndexToHrMinString(*state, 0));
    EXPECT_EQ("00:15:00", TimeIndexToHrMinString(*state, 1));
    EXPECT_EQ("01:45:00", TimeIndexToHrMinString(*state, 7));
    EXPECT_EQ("07:45:00", TimeIndexToHrMinString(*state, 31));
    EXPECT_EQ("19:45:00", TimeIndexToHrMinString(*state, 79));
    EXPECT_EQ("24:00:00", TimeIndexToHrMinString(*state, 96));

    state->dataGlobal->MinutesPerTimeStep = 3;

    EXPECT_EQ("00:00:00", TimeIndexToHrMinString(*state, 0));
    EXPECT_EQ("00:03:00", TimeIndexToHrMinString(*state, 1));
    EXPECT_EQ("00:21:00", TimeIndexToHrMinString(*state, 7));
    EXPECT_EQ("01:33:00", TimeIndexToHrMinString(*state, 31));
    EXPECT_EQ("03:57:00", TimeIndexToHrMinString(*state, 79));
    EXPECT_EQ("04:48:00", TimeIndexToHrMinString(*state, 96));
    EXPECT_EQ("16:39:00", TimeIndexToHrMinString(*state, 333));
    EXPECT_EQ("24:00:00", TimeIndexToHrMinString(*state, 480));
}

TEST_F(EnergyPlusFixture, SizingManager_DOASControlStrategyDefaultSpecificationTest)
{
    // checks DOAS Control Strategy default setpoint values test
    std::string const idf_objects = delimited_string({

        " Zone,",
        "	SPACE1-1,      !- Name",
        "	0,             !- Direction of Relative North { deg }",
        "	0,             !- X Origin { m }",
        "	0,             !- Y Origin { m }",
        "	0,             !- Z Origin { m }",
        "	1,             !- Type",
        "	1,             !- Multiplier",
        "	3.0,           !- Ceiling Height {m}",
        "	240.0;         !- Volume {m3}",

        " Sizing:Zone,",
        "	SPACE1-1,             !- Zone or ZoneList Name",
        "	SupplyAirTemperature, !- Zone Cooling Design Supply Air Temperature Input Method",
        "	14.,                  !- Zone Cooling Design Supply Air Temperature { C }",
        "	,                     !- Zone Cooling Design Supply Air Temperature Difference { deltaC }",
        "	SupplyAirTemperature, !- Zone Heating Design Supply Air Temperature Input Method",
        "	50.,                  !- Zone Heating Design Supply Air Temperature { C }",
        "	,                     !- Zone Heating Design Supply Air Temperature Difference { deltaC }",
        "	0.009,                !- Zone Cooling Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	0.004,                !- Zone Heating Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	SZ DSOA SPACE1-1,     !- Design Specification Outdoor Air Object Name",
        "	0.0,                  !- Zone Heating Sizing Factor",
        "	0.0,                  !- Zone Cooling Sizing Factor",
        "	DesignDayWithLimit,   !- Cooling Design Air Flow Method",
        "	,                     !- Cooling Design Air Flow Rate { m3/s }",
        "	,                     !- Cooling Minimum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	,                     !- Cooling Minimum Air Flow { m3/s }",
        "	,                     !- Cooling Minimum Air Flow Fraction",
        "	DesignDay,            !- Heating Design Air Flow Method",
        "	,                     !- Heating Design Air Flow Rate { m3/s }",
        "	,                     !- Heating Maximum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	,                     !- Heating Maximum Air Flow { m3/s }",
        "	,                     !- Heating Maximum Air Flow Fraction",
        "	,                     !- Design Specification Zone Air Distribution Object Name",
        "   Yes,                  !- Account for Dedicated Outside Air System",
        "   NeutralSupplyAir,     !- Dedicated Outside Air System Control Strategy",
        "   ,                     !- Dedicated Outside Air Low Setpoint for Design",
        "   ;                     !- Dedicated Outside Air High Setpoint for Design",

        " DesignSpecification:OutdoorAir,",
        "	SZ DSOA SPACE1-1,     !- Name",
        "	sum,                  !- Outdoor Air Method",
        "	0.00236,              !- Outdoor Air Flow per Person { m3/s-person }",
        "	0.000305,             !- Outdoor Air Flow per Zone Floor Area { m3/s-m2 }",
        "	0.0;                  !- Outdoor Air Flow per Zone { m3/s }",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    SizingManager::GetOARequirements(*state);
    SizingManager::GetZoneSizingInput(*state);
    ASSERT_EQ(1, state->dataSize->NumZoneSizingInput);
    ASSERT_EQ(DOANeutralSup, state->dataSize->ZoneSizingInput(1).DOASControlStrategy);
    ASSERT_EQ(DataSizing::AutoSize, state->dataSize->ZoneSizingInput(1).DOASLowSetpoint);
    ASSERT_EQ(DataSizing::AutoSize, state->dataSize->ZoneSizingInput(1).DOASHighSetpoint);
    // set default DOAS control strategy setpoint values
    ZoneEquipmentManager::AutoCalcDOASControlStrategy(*state);
    // check default low and high set point values
    ASSERT_EQ(21.1, state->dataSize->ZoneSizingInput(1).DOASLowSetpoint);
    ASSERT_EQ(23.9, state->dataSize->ZoneSizingInput(1).DOASHighSetpoint);
}

TEST_F(EnergyPlusFixture, SizingManager_DOASControlStrategyDefaultSpecificationTest2)
{
    // checks DOAS Control Strategy default setpoint values test
    std::string const idf_objects = delimited_string({

        " Zone,",
        "	SPACE1-1,      !- Name",
        "	0,             !- Direction of Relative North { deg }",
        "	0,             !- X Origin { m }",
        "	0,             !- Y Origin { m }",
        "	0,             !- Z Origin { m }",
        "	1,             !- Type",
        "	1,             !- Multiplier",
        "	3.0,           !- Ceiling Height {m}",
        "	240.0;         !- Volume {m3}",

        " Sizing:Zone,",
        "	SPACE1-1,             !- Zone or ZoneList Name",
        "	SupplyAirTemperature, !- Zone Cooling Design Supply Air Temperature Input Method",
        "	14.,                  !- Zone Cooling Design Supply Air Temperature { C }",
        "	,                     !- Zone Cooling Design Supply Air Temperature Difference { deltaC }",
        "	SupplyAirTemperature, !- Zone Heating Design Supply Air Temperature Input Method",
        "	50.,                  !- Zone Heating Design Supply Air Temperature { C }",
        "	,                     !- Zone Heating Design Supply Air Temperature Difference { deltaC }",
        "	0.009,                !- Zone Cooling Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	0.004,                !- Zone Heating Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	SZ DSOA SPACE1-1,     !- Design Specification Outdoor Air Object Name",
        "	0.0,                  !- Zone Heating Sizing Factor",
        "	0.0,                  !- Zone Cooling Sizing Factor",
        "	DesignDayWithLimit,   !- Cooling Design Air Flow Method",
        "	,                     !- Cooling Design Air Flow Rate { m3/s }",
        "	,                     !- Cooling Minimum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	,                     !- Cooling Minimum Air Flow { m3/s }",
        "	,                     !- Cooling Minimum Air Flow Fraction",
        "	DesignDay,            !- Heating Design Air Flow Method",
        "	,                     !- Heating Design Air Flow Rate { m3/s }",
        "	,                     !- Heating Maximum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	,                     !- Heating Maximum Air Flow { m3/s }",
        "	,                     !- Heating Maximum Air Flow Fraction",
        "	,                     !- Design Specification Zone Air Distribution Object Name",
        "   Yes,                  !- Account for Dedicated Outside Air System",
        "   NeutralSupplyAir;     !- Dedicated Outside Air System Control Strategy",

        " DesignSpecification:OutdoorAir,",
        "	SZ DSOA SPACE1-1,     !- Name",
        "	sum,                  !- Outdoor Air Method",
        "	0.00236,              !- Outdoor Air Flow per Person { m3/s-person }",
        "	0.000305,             !- Outdoor Air Flow per Zone Floor Area { m3/s-m2 }",
        "	0.0;                  !- Outdoor Air Flow per Zone { m3/s }",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    SizingManager::GetOARequirements(*state);
    SizingManager::GetZoneSizingInput(*state);
    ASSERT_EQ(1, state->dataSize->NumZoneSizingInput);
    ASSERT_EQ(DOANeutralSup, state->dataSize->ZoneSizingInput(1).DOASControlStrategy);
    ASSERT_EQ(DataSizing::AutoSize, state->dataSize->ZoneSizingInput(1).DOASLowSetpoint);
    ASSERT_EQ(DataSizing::AutoSize, state->dataSize->ZoneSizingInput(1).DOASHighSetpoint);
    // set default DOAS control strategy setpoint values
    ZoneEquipmentManager::AutoCalcDOASControlStrategy(*state);
    // check default low and high set point values
    ASSERT_EQ(21.1, state->dataSize->ZoneSizingInput(1).DOASLowSetpoint);
    ASSERT_EQ(23.9, state->dataSize->ZoneSizingInput(1).DOASHighSetpoint);
}

TEST_F(EnergyPlusFixture, SizingManager_CalcdoLoadComponentPulseNowTest)
{

    bool Answer;
    bool WarmupFlag;
    bool PulseSizing;
    int HourNum;
    int TimeStepNum;

    // Tests for when to do a pulse test for the Load Component Output Report

    // Test 1a: Everything as it should be to set this to true-->result should be true
    PulseSizing = true;
    WarmupFlag = false;
    HourNum = 10;
    TimeStepNum = 1;
    state->dataGlobal->KindOfSim = EnergyPlus::DataGlobalConstants::KindOfSim::RunPeriodDesign;
    state->dataGlobal->DayOfSim = 2;
    Answer = CalcdoLoadComponentPulseNow(*state, PulseSizing, WarmupFlag, HourNum, TimeStepNum, state->dataGlobal->KindOfSim);
    ASSERT_TRUE(Answer);

    // Test 16: Everything as it should be to set this to true-->result should be true
    PulseSizing = true;
    WarmupFlag = false;
    HourNum = 10;
    TimeStepNum = 1;
    state->dataGlobal->KindOfSim = EnergyPlus::DataGlobalConstants::KindOfSim::DesignDay;
    state->dataGlobal->DayOfSim = 1;
    Answer = CalcdoLoadComponentPulseNow(*state, PulseSizing, WarmupFlag, HourNum, TimeStepNum, state->dataGlobal->KindOfSim);
    ASSERT_TRUE(Answer);

    // Test 2: PulseSizing is false-->result should be false
    PulseSizing = false;
    WarmupFlag = false;
    HourNum = 10;
    TimeStepNum = 1;
    state->dataGlobal->KindOfSim = EnergyPlus::DataGlobalConstants::KindOfSim::RunPeriodDesign;
    state->dataGlobal->DayOfSim = 1;
    Answer = CalcdoLoadComponentPulseNow(*state, PulseSizing, WarmupFlag, HourNum, TimeStepNum, state->dataGlobal->KindOfSim);
    ASSERT_FALSE(Answer);

    // Test 3: Warmup is true-->result should be false
    PulseSizing = false;
    WarmupFlag = true;
    HourNum = 10;
    TimeStepNum = 1;
    state->dataGlobal->KindOfSim = EnergyPlus::DataGlobalConstants::KindOfSim::RunPeriodDesign;
    state->dataGlobal->DayOfSim = 1;
    Answer = CalcdoLoadComponentPulseNow(*state, PulseSizing, WarmupFlag, HourNum, TimeStepNum, state->dataGlobal->KindOfSim);
    ASSERT_FALSE(Answer);

    // Test 4: HourNum not 10-->result should be false
    PulseSizing = true;
    WarmupFlag = false;
    HourNum = 7;
    TimeStepNum = 1;
    state->dataGlobal->KindOfSim = EnergyPlus::DataGlobalConstants::KindOfSim::RunPeriodDesign;
    state->dataGlobal->DayOfSim = 1;
    Answer = CalcdoLoadComponentPulseNow(*state, PulseSizing, WarmupFlag, HourNum, TimeStepNum, state->dataGlobal->KindOfSim);
    ASSERT_FALSE(Answer);

    // Test 5: TimeStepNum not 1-->result should be false
    PulseSizing = true;
    WarmupFlag = false;
    HourNum = 10;
    TimeStepNum = 2;
    state->dataGlobal->KindOfSim = EnergyPlus::DataGlobalConstants::KindOfSim::RunPeriodDesign;
    state->dataGlobal->DayOfSim = 1;
    Answer = CalcdoLoadComponentPulseNow(*state, PulseSizing, WarmupFlag, HourNum, TimeStepNum, state->dataGlobal->KindOfSim);
    ASSERT_FALSE(Answer);

    // Test 6: DayOfSim not 1 and KindSim not weather file period --> result should be false
    PulseSizing = true;
    WarmupFlag = false;
    HourNum = 10;
    TimeStepNum = 1;
    state->dataGlobal->KindOfSim = EnergyPlus::DataGlobalConstants::KindOfSim::DesignDay;
    state->dataGlobal->DayOfSim = 2;
    Answer = CalcdoLoadComponentPulseNow(*state, PulseSizing, WarmupFlag, HourNum, TimeStepNum, state->dataGlobal->KindOfSim);
    ASSERT_FALSE(Answer);

    // Test 7: everything set to make the answer false
    PulseSizing = false;
    WarmupFlag = true;
    HourNum = 2;
    TimeStepNum = 7;
    state->dataGlobal->KindOfSim = EnergyPlus::DataGlobalConstants::KindOfSim::DesignDay;
    state->dataGlobal->DayOfSim = 2;
    Answer = CalcdoLoadComponentPulseNow(*state, PulseSizing, WarmupFlag, HourNum, TimeStepNum, state->dataGlobal->KindOfSim);
    ASSERT_FALSE(Answer);
}
