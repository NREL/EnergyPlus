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

#include "AutosizingFixture.hh"
#include <gtest/gtest.h>

#include <EnergyPlus/Autosizing/HeatingAirflowUASizing.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>

namespace EnergyPlus {

TEST_F(AutoSizingFixture, HeatingAirflowUA_APIExampleUnitTest)
{
    // simple example unit test for API calls
    // C and Python files will exercise the API by making function calls into each library
    // see energyplusapi/Source Files/autosizing.c
    bool errorsFound = false;
    HeatingAirflowUASizer sizer;
    // initializeForSingleDuctZoneTerminal(Real64 const elevation, Real64 mainFlowRate)
    sizer.initializeForSingleDuctZoneTerminal(*state, 1650.0, 0.3); // Denver
    EXPECT_TRUE(sizer.zoneSizingRunDone);
    EXPECT_EQ(sizer.curZoneEqNum, 1);
    EXPECT_TRUE(sizer.termUnitSingDuct);
    EXPECT_EQ(sizer.curTermUnitSizingNum, 1);
    EXPECT_GT(int(sizer.termUnitSizing.size()), 0);
    EXPECT_EQ(sizer.termUnitSizing(1).AirVolFlow, 0.3);
    Real64 sizedValue = sizer.size(*state, DataSizing::AutoSize, errorsFound);
    EXPECT_NEAR(sizedValue, 0.29599, 0.00001); // converts volume input to mass flow rate at elevation
    EXPECT_FALSE(errorsFound);

    sizer.initializeForSingleDuctZoneTerminal(*state, 0.0, 0.3);
    sizedValue = sizer.size(*state, DataSizing::AutoSize, errorsFound);
    EXPECT_NEAR(sizedValue, 0.36129, 0.00001); // converts volume input to mass flow rate at elevation
}

TEST_F(AutoSizingFixture, HeatingAirflowUASizingGauntlet)
{
    // this global state is what would be set up by E+ currently
    state->dataEnvrn->StdRhoAir = 1.2;
    state->dataSize->ZoneEqSizing.allocate(1);
    static constexpr std::string_view routineName("HeatingAirflowUASizingGauntlet");

    // create the sizer and set up the flags to specify the sizing configuration
    HeatingAirflowUASizer sizer;
    Real64 inputValue = 5;
    bool errorsFound = false;
    bool printFlag = false;

    // uninitialized sizing type
    Real64 sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::ErrorType2, sizer.errorType));
    EXPECT_NEAR(0.0, sizedValue, 0.01); // unitialized sizing types always return 0
    errorsFound = false;

    // ZONE EQUIPMENT TESTING
    state->dataSize->CurZoneEqNum = 1;
    state->dataSize->CurTermUnitSizingNum = 1;
    state->dataSize->TermUnitSingDuct = true;

    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);

    // Test #1 - Zone Equipment, no autosizing
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    // reset eio stream
    has_eio_output(true);
    printFlag = true;

    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    std::string eiooutput =
        std::string("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n"
                    " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Heating Coil Airflow For UA, 5.00000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // reset eio stream
    has_eio_output(true);

    // now allocate sizing arrays for testing autosized field
    state->dataSize->TermUnitSizing.allocate(1);
    state->dataSize->TermUnitSizing(1).AirVolFlow = 0.0008;
    state->dataSize->FinalZoneSizing.allocate(1);
    state->dataSize->ZoneEqSizing.allocate(1);

    state->dataSize->ZoneSizingRunDone = true;

    // Test 2 - Zone Equipment, Single Duct TU, UA sizes to 0
    state->dataSize->TermUnitSingDuct = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    state->dataSize->ZoneSizingInput.allocate(1);
    state->dataSize->ZoneSizingInput(1).ZoneNum = 1;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0, sizedValue, 0.01);
    EXPECT_NEAR(0.0008, state->dataSize->TermUnitSizing(1).AirVolFlow, 0.0001);
    EXPECT_NEAR(1.2, state->dataEnvrn->StdRhoAir, 0.01);

    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Heating Coil Airflow For UA, 0.00000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 3 - Zone Equipment, Single Duct TU
    state->dataSize->TermUnitSizing(1).AirVolFlow = 5;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    EXPECT_NEAR(5.0, state->dataSize->TermUnitSizing(1).AirVolFlow, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 4 - Zone Equipment, Powered Induction TU
    state->dataSize->TermUnitSingDuct = false;
    state->dataSize->TermUnitPIU = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 5 - Zone Equipment, Induction TU
    state->dataSize->TermUnitPIU = false;
    state->dataSize->TermUnitIU = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 6 - Zone Equipment, Zone Eq Fan Coil
    state->dataSize->TermUnitIU = false;
    state->dataSize->ZoneEqFanCoil = true;
    state->dataSize->TermUnitSizing(1).AirVolFlow = 0.0;
    state->dataSize->FinalZoneSizing(1).DesHeatVolFlow = 5.0;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 7 - Zone Equipment, Other Equipment
    state->dataSize->ZoneEqFanCoil = false;
    // baseFlags.otherEqType = true; set in initialize function based on other flags
    state->dataSize->FinalZoneSizing(1).DesHeatVolFlow = 0.0;
    state->dataSize->ZoneEqSizing(1).AirVolFlow = 5.0;
    state->dataSize->ZoneEqSizing(1).SystemAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 8 - Zone Equipment, Other Equipment
    state->dataSize->ZoneEqSizing(1).AirVolFlow = 0.0;
    state->dataSize->ZoneEqSizing(1).HeatingAirVolFlow = 5.0;
    state->dataSize->ZoneEqSizing(1).SystemAirFlow = false;
    state->dataSize->ZoneEqSizing(1).HeatingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 9 - Zone Equipment, Other Equipment
    state->dataSize->ZoneEqSizing(1).HeatingAirVolFlow = 0.0;
    state->dataSize->ZoneEqSizing(1).HeatingAirFlow = false;
    state->dataSize->FinalZoneSizing(1).DesHeatMassFlow = 5.0;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // uses a mass flow rate for sizing
    sizer.autoSizedValue = 0.0;         // reset for next test

    // reset eio stream
    has_eio_output(true);
    eiooutput = "";

    // AIRLOOP EQUIPMENT TESTING - CurDuctType not set, no reporting
    // Test 10 - Airloop Equipment
    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->NumZoneSizingInput = 0;
    state->dataSize->CurTermUnitSizingNum = 0;
    // baseFlags.otherEqType = false; set in initialize function based on other flags
    state->dataSize->ZoneEqSizing.deallocate();
    state->dataSize->FinalZoneSizing.deallocate();

    state->dataSize->CurSysNum = 1;
    state->dataHVACGlobal->NumPrimaryAirSys = 1;
    state->dataSize->NumSysSizInput = 1;
    state->dataSize->SysSizingRunDone = false;
    // start with a hard-sized value as the user input, no system sizing arrays
    inputValue = 5.0;
    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 11 - Airloop Equipment - CurDuctType not set
    state->dataSize->CurSysNum = 1;
    state->dataHVACGlobal->NumPrimaryAirSys = 1;
    state->dataSize->NumSysSizInput = 1;
    state->dataSize->SysSizingRunDone = true;
    state->dataSize->FinalSysSizing.allocate(1);
    state->dataSize->SysSizInput.allocate(1);
    state->dataSize->SysSizInput(1).AirLoopNum = 1;

    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 5.0; // CurDuctType not set
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Heating Coil Airflow For UA, 6.00000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 12 - Airloop Equipment - CurDuctType = Main, SysAirMinFlowRat = 0
    state->dataSize->CurDuctType = DataHVACGlobals::Main;
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 5.0;
    state->dataSize->FinalSysSizing(1).SysAirMinFlowRat = 0.0;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 13 - Airloop Equipment - CurDuctType = Main, SysAirMinFlowRat = 0.5
    state->dataSize->FinalSysSizing(1).SysAirMinFlowRat = 0.5;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 14 - Airloop Equipment - CurDuctType = Cooling, SysAirMinFlowRat = 0
    state->dataSize->CurDuctType = DataHVACGlobals::Cooling;
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 0.0;
    state->dataSize->FinalSysSizing(1).DesCoolVolFlow = 5.0;
    state->dataSize->FinalSysSizing(1).SysAirMinFlowRat = 0.0;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 15 - Airloop Equipment - CurDuctType = Cooling, SysAirMinFlowRat = 0.5
    state->dataSize->FinalSysSizing(1).SysAirMinFlowRat = 0.5;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 16 - Airloop Equipment - CurDuctType = Heating, SysAirMinFlowRat doesn't matter
    state->dataSize->CurDuctType = DataHVACGlobals::Heating;
    state->dataSize->FinalSysSizing(1).DesCoolVolFlow = 0.0;
    state->dataSize->FinalSysSizing(1).DesHeatVolFlow = 5.0;
    state->dataSize->FinalSysSizing(1).SysAirMinFlowRat = 0.5;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // OUTDOOR AIR SYSTEM EQUIPMENT TESTING
    // Test 17 - Outdoor Air System Equipment, no DOAS air loop
    state->dataSize->FinalSysSizing(1).DesHeatVolFlow = 0.0;
    state->dataSize->FinalSysSizing(1).DesOutAirVolFlow = 5.0;
    state->dataSize->OASysEqSizing.allocate(1);
    state->dataAirLoop->OutsideAirSys.allocate(1);
    state->dataSize->CurOASysNum = 1;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 18 - Outdoor Air System Equipment with DOAS system
    state->dataSize->FinalSysSizing(1).DesOutAirVolFlow = 0.0;
    state->dataAirLoop->OutsideAirSys(1).AirLoopDOASNum = 0;
    state->dataAirLoopHVACDOAS->airloopDOAS.emplace_back();
    state->dataAirLoopHVACDOAS->airloopDOAS[0].SizingMassFlow = 5.0;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // uses a mass flow rate for sizing
    sizer.autoSizedValue = 0.0;         // reset for next test

    // reset eio stream
    has_eio_output(true);

    // Test 19 - Outdoor Air System Equipment with DOAS system, hard-sized air flow rate
    // start with an auto-sized value as the user input
    inputValue = 5.0;
    state->dataAirLoopHVACDOAS->airloopDOAS[0].SizingMassFlow = 3.0;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType)); // cumulative of previous calls
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Heating Coil Airflow For UA, 3.00000\n"
                            " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Heating Coil Airflow For UA, 5.00000\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}

} // namespace EnergyPlus
