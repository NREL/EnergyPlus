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

#include <EnergyPlus/Autosizing/HeatingWaterDesCoilLoadUsedForUASizing.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>

namespace EnergyPlus {

TEST_F(AutoSizingFixture, HeatingWaterDesCoilLoadUsedForUASizingGauntlet)
{
    // this global state is what would be set up by E+ currently
    state->dataEnvrn->StdRhoAir = 1.2;
    static std::string const routineName("HeatingWaterDesCoilLoadUsedForUASizingGauntlet");

    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);

    // create the sizer and set up the flags to specify the sizing configuration
    HeatingWaterDesCoilLoadUsedForUASizer sizer;
    Real64 inputValue = 5125.3;
    bool errorsFound = false;
    bool printFlag = false;

    // uninitialized sizing type
    Real64 sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(errorsFound);
    EXPECT_EQ(AutoSizingResultType::ErrorType2, sizer.errorType);
    EXPECT_NEAR(0.0, sizedValue, 0.01); // unitialized sizing types always return 0
    errorsFound = false;

    // ZONE EQUIPMENT TESTING
    DataSizing::CurZoneEqNum = 1;
    DataSizing::CurTermUnitSizingNum = 1;
    DataSizing::TermUnitSingDuct = true;

    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);

    // Test #1 - Zone Equipment, no autosizing (this sizer is not typically hard-sized)
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5125.3, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    // reset eio stream
    has_eio_output(true);
    printFlag = true;

    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5125.3, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    std::string eiooutput =
        std::string("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n"
                    " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Water Heating Design Coil Load for UA Sizing, 5125.30000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // now allocate sizing arrays for testing autosized field
    EnergyPlus::DataSizing::TermUnitSizing.allocate(1);
    EnergyPlus::DataSizing::TermUnitFinalZoneSizing.allocate(1);
    EnergyPlus::DataSizing::TermUnitFinalZoneSizing(1).DesHeatCoilInTempTU = 15.0;
    EnergyPlus::DataSizing::TermUnitFinalZoneSizing(1).ZoneTempAtHeatPeak = 20.0;
    EnergyPlus::DataSizing::TermUnitSizing(1).AirVolFlow = 0.0005;
    EnergyPlus::DataSizing::TermUnitSizing(1).ReheatAirFlowMult = 1.0;
    EnergyPlus::DataSizing::FinalZoneSizing.allocate(1);
    EnergyPlus::DataSizing::FinalZoneSizing(1).ZoneTempAtHeatPeak = 20.0;
    EnergyPlus::DataSizing::FinalZoneSizing(1).ZoneRetTempAtHeatPeak = 24.0;
    EnergyPlus::DataSizing::FinalZoneSizing(1).ZoneHumRatAtHeatPeak = 0.007;
    EnergyPlus::DataSizing::FinalZoneSizing(1).ZoneHumRatAtHeatPeak = 0.006;
    EnergyPlus::DataSizing::FinalZoneSizing(1).DesHeatMassFlow = 0.2;
    EnergyPlus::DataSizing::FinalZoneSizing(1).HeatDesTemp = 30.0;
    EnergyPlus::DataSizing::FinalZoneSizing(1).HeatDesHumRat = 0.004;
    EnergyPlus::DataSizing::FinalZoneSizing(1).OutTempAtHeatPeak = 5.0;
    EnergyPlus::DataSizing::FinalZoneSizing(1).OutHumRatAtHeatPeak = 0.002;
    EnergyPlus::DataSizing::ZoneSizingInput.allocate(1);
    EnergyPlus::DataSizing::ZoneSizingInput(1).ZoneNum = 1;

    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    EnergyPlus::DataSizing::ZoneEqSizing(1).MaxHWVolFlow = 0.0002;
    EnergyPlus::DataSizing::ZoneEqSizing(1).ATMixerHeatPriDryBulb = 28.0;
    EnergyPlus::DataSizing::ZoneEqSizing(1).ATMixerHeatPriHumRat = 0.0045;
    state->dataPlnt->PlantLoop.allocate(1);
    DataSizing::DataWaterLoopNum = 1;
    DataSizing::DataWaterCoilSizHeatDeltaT = 5.0;
    DataSizing::DataWaterFlowUsedForSizing = 0.0002;
    DataSizing::ZoneSizingRunDone = true;

    // Test 2 - Zone Equipment, Single Duct TU
    DataSizing::TermUnitSingDuct = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.zoneSizingInput.allocate(1);
    sizer.zoneSizingInput(1).ZoneNum = 1;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(4114.69, sizedValue, 0.01);
    EXPECT_NEAR(1.2, state->dataEnvrn->StdRhoAir, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 4 - Zone Equipment, Zone Eq Fan Coil
    DataSizing::TermUnitSingDuct = false;
    DataSizing::ZoneEqFanCoil = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(4114.69, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 5 - Zone Equipment, Zone Eq Fan Coil
    DataSizing::ZoneEqFanCoil = false;
    DataSizing::TermUnitIU = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(4114.69, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    DataSizing::ZoneEqSizing(1).OAVolFlow = 0.05;
    // Test 6 - Zone Equipment, Other Equipment
    DataSizing::TermUnitIU = false;
    // start with an auto-sized value as the user input, OA flow > 0
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2935.6, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 7 - Zone Equipment, Other Equipment
    DataSizing::ZoneEqSizing(1).ATMixerVolFlow = 0.03;
    // start with an auto-sized value as the user input, AT Mixer present
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1068.96, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::ZoneEqSizing(1).OAVolFlow = 0.0;
    DataSizing::ZoneEqSizing(1).ATMixerVolFlow = 0.0;

    // Test 8 - Zone Equipment, Other Equipment
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SystemAirFlow = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).AirVolFlow = 0.3;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3644.19, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 9 - Zone Equipment, Other Equipment
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SystemAirFlow = false;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).AirVolFlow = 0.0;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirFlow = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirVolFlow = 0.4;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(4858.92, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 10 - Zone Equipment, Other Equipment
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirFlow = false;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirVolFlow = 0.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2024.55, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // reset eio stream
    has_eio_output(true);

    // Test 11 - Zone Equipment, Other Equipment, heating sizing ratio = 0.5
    // start with a hard-sized value as the user input
    inputValue = 1500.0;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(1500.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Water Heating Design Coil Load for UA Sizing, 2024.55160\n"
                            " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Water Heating Design Coil Load for UA Sizing, 1500.00000\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // reset eio stream
    has_eio_output(true);
    eiooutput = "";

    // AIRLOOP EQUIPMENT TESTING
    // Test 12 - Airloop Equipment
    DataSizing::CurZoneEqNum = 0;
    DataSizing::NumZoneSizingInput = 0;
    DataSizing::CurTermUnitSizingNum = 0;
    EnergyPlus::DataSizing::ZoneEqSizing.deallocate();
    EnergyPlus::DataSizing::FinalZoneSizing.deallocate();

    DataSizing::CurSysNum = 1;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataSizing::NumSysSizInput = 1;
    DataSizing::SysSizingRunDone = false;
    // start with a hard-sized value as the user input, no system sizing arrays
    inputValue = 5000.0;
    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5000.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 13 - Airloop Equipment - sizing arrays set
    DataSizing::CurSysNum = 1;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataSizing::NumSysSizInput = 1;
    DataSizing::SysSizingRunDone = true;
    EnergyPlus::DataSizing::FinalSysSizing.allocate(1);
    EnergyPlus::DataSizing::FinalSysSizing(1).HeatOutTemp = 10.0;
    EnergyPlus::DataSizing::FinalSysSizing(1).HeatRetTemp = 24.0;
    EnergyPlus::DataSizing::FinalSysSizing(1).HeatSupTemp = 30.0;
    EnergyPlus::DataSizing::SysSizInput.allocate(1);
    EnergyPlus::DataSizing::SysSizInput(1).AirLoopNum = 1;
    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    DataSizing::DataAirFlowUsedForSizing = 0.6;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(14469.96, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Water Heating Design Coil Load for UA Sizing, 14469.96369\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 14 - Airloop Equipment, 50% OA, MinOA option
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).HeatOAOption = DataSizing::MinOA;
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesOutAirVolFlow = DataSizing::DataAirFlowUsedForSizing / 2.0;
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).PreheatTemp = 19.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(9405.48, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 15 - Airloop Equipment - desiccant regen heating coil
    DataSizing::DataDesicRegCoil = true;
    DataSizing::DataDesOutletAirTemp = 20.0;
    DataSizing::DataDesInletAirTemp = 10.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(7234.98, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 16 - Air Loop Equipment, DOAS air loop w/ heating coil
    state->dataAirSystemsData->PrimaryAirSystems(DataSizing::CurSysNum).NumOAHeatCoils = 1;
    DataSizing::DataDesicRegCoil = false;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6149.73, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // OUTDOOR AIR SYSTEM EQUIPMENT TESTING
    // Test 17 - Outdoor Air System Equipment, DOAS air loop
    EnergyPlus::DataSizing::OASysEqSizing.allocate(1);
    state->dataAirLoop->OutsideAirSys.allocate(1);
    DataSizing::CurOASysNum = 1;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6511.48, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 18 - Outdoor Air System Equipment with DOAS system
    state->dataAirLoop->OutsideAirSys(DataSizing::CurOASysNum).AirLoopDOASNum = 0;
    state->dataAirLoopHVACDOAS->airloopDOAS.emplace_back();
    state->dataAirLoopHVACDOAS->airloopDOAS[0].HeatOutTemp = 8.0;
    state->dataAirLoopHVACDOAS->airloopDOAS[0].PreheatTemp = 15.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5064.49, sizedValue, 0.01); // uses a mass flow rate for sizing
    sizer.autoSizedValue = 0.0;          // reset for next test

    // reset eio stream
    has_eio_output(true);

    // Test 19 - Outdoor Air System Equipment with DOAS system, hard-sized temperature
    inputValue = 7000.0;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType); // cumulative of previous calls
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(7000.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Water Heating Design Coil Load for UA Sizing, 5064.48729\n"
        " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Water Heating Design Coil Load for UA Sizing, 7000.00000\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}

} // namespace EnergyPlus
