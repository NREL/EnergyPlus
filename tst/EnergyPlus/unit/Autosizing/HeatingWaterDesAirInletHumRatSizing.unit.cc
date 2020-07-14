// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

#include <EnergyPlus/Autosizing/HeatingWaterDesAirInletHumRatSizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>

namespace EnergyPlus {

TEST_F(AutoSizingFixture, HeatingWaterDesAirInletHumRatSizingGauntlet)
{
    // this global state is what would be set up by E+ currently
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    DataEnvironment::StdRhoAir = 1.2;
    static std::string const routineName("HeatingWaterDesAirInletHumRatSizingGauntlet");

    // create the sizer and set up the flags to specify the sizing configuration
    HeatingWaterDesAirInletHumRatSizer sizer;
    Real64 inputValue = 0.009;
    bool errorsFound = false;
    bool printFlag = false;

    // uninitialized sizing type
    Real64 sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_TRUE(errorsFound);
    EXPECT_EQ(AutoSizingResultType::ErrorType2, sizer.errorType);
    EXPECT_NEAR(0.0, sizedValue, 0.01); // unitialized sizing types always return 0
    errorsFound = false;

    // ZONE EQUIPMENT TESTING
    DataSizing::CurZoneEqNum = 1;

    // reset eio stream
    has_eio_output(true);

    // Test #1 - Zone Equipment, no autosizing, TU type not set
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.009, sizedValue, 0.001); // hard-sized value
    sizer.autoSizedValue = 0.0;            // reset for next test

    std::string eiooutput = std::string("");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test #2 - Zone Equipment, no autosizing, TU type not set
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.009, sizedValue, 0.001); // hard-sized value
    sizer.autoSizedValue = 0.0;            // reset for next test

    eiooutput =
        std::string("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n"
                    " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 9.00000E-003\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // reset eio stream
    has_eio_output(true);

    // now allocate sizing arrays for testing autosized field
    EnergyPlus::DataSizing::FinalZoneSizing.allocate(1);
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    EnergyPlus::DataSizing::TermUnitSizing.allocate(1);
    EnergyPlus::DataSizing::TermUnitFinalZoneSizing.allocate(1);

    // Sizing Type Prerequisites:
    // TermUnitPIU, TermUnitSingDuct
    EnergyPlus::DataSizing::TermUnitFinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatCoilInHumRatTU = 0.008;
    // TermUnitPIU
    EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).ZoneHumRatAtHeatPeak = 0.007;
    // TermUnitIU
    EnergyPlus::DataSizing::TermUnitFinalZoneSizing(DataSizing::CurZoneEqNum).ZoneHumRatAtHeatPeak = 0.006;
    // all others
    EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatCoilInHumRat = 0.008;
    EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatMassFlow = 0.01;
    EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).OutHumRatAtHeatPeak = 0.004;

    EnergyPlus::DataSizing::ZoneSizingInput.allocate(1);
    EnergyPlus::DataSizing::ZoneSizingInput(DataSizing::CurZoneEqNum).ZoneNum = DataSizing::CurZoneEqNum;
    // set by parent
    EnergyPlus::DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).AirVolFlow = 0.021;
    EnergyPlus::DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirVolFlow = 0.015;
    EnergyPlus::DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).OAVolFlow = 0.003;

    DataSizing::ZoneSizingRunDone = true;
    DataSizing::CurTermUnitSizingNum = 1;

    // Test 3 - Zone Equipment, Single Duct TU
    DataSizing::TermUnitSingDuct = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.008, sizedValue, 0.0001);

    eiooutput =
        std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 8.00000E-003\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 4 - Zone Equipment, Powered Induction TU
    DataSizing::TermUnitSingDuct = false;
    DataSizing::TermUnitPIU = true;
    EnergyPlus::DataSizing::TermUnitSizing(1).MinFlowFrac = 0.0; // all zone air
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.007, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 5 - Zone Equipment, Powered Induction TU with MinFlowFrac at 0.3
    EnergyPlus::DataSizing::TermUnitSizing(1).MinFlowFrac = 0.3; // mix zone air and DesHeatCoilInHumRatTU
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    Real64 expectedValue = 0.7 * 0.007 + 0.3 * 0.008; // 70% zone + 30% DesHeatCoilInHumRatTU
    EXPECT_NEAR(expectedValue, sizedValue, 0.00001);
    EXPECT_NEAR(0.0073, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 6 - Zone Equipment, Induction Unit
    DataSizing::TermUnitPIU = false;
    DataSizing::TermUnitIU = true;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.006, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 7 - Zone Equipment, Zone Eq Fan Coil, mixture of zone and OA
    DataSizing::TermUnitIU = false;
    DataSizing::ZoneEqFanCoil = true;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0059, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 8 - Zone Equipment, Zone Eq Fan Coil, parent set system flow
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SystemAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.00657, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SystemAirFlow = false;

    // Test 9 - Zone Equipment, Zone Eq Fan Coil, parent set heating flow
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirVolFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.00592, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirVolFlow = false;

    // Test 10 - Zone Equipment w/ 10% OA
    DataSizing::ZoneEqSizing(1).OAVolFlow =
        EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatMassFlow / (10.0 * DataEnvironment::StdRhoAir);
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    expectedValue = 0.9 * 0.007 + 0.1 * 0.004; // 90% zone + 10% OA
    EXPECT_NEAR(expectedValue, sizedValue, 0.0001);
    EXPECT_NEAR(0.0067, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 11 - Zone Equipment w/ AT Mixer at 20% of design flow
    DataSizing::ZoneEqSizing(1).ATMixerHeatPriHumRat = 0.001;
    DataSizing::ZoneEqSizing(1).ATMixerVolFlow = 0.002 / DataEnvironment::StdRhoAir; // AT mass flow smaller than DesCoolMassFlow by factor of 5
    Real64 mixedHumRat2 = 0.8 * 0.007 + 0.2 * 0.001;                                 // 80% of ZoneHumRatAtCoolPeak, 20% of AT Mixer mass flow

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(mixedHumRat2, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 12 - Zone Equipment w/o AT Mixer or OA flow set by parent, yields zone hum rat
    DataSizing::ZoneEqSizing(1).ATMixerVolFlow = 0.0;
    DataSizing::ZoneEqSizing(1).OAVolFlow = 0.0;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.007, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    DataSizing::ZoneEqFanCoil = false;
    // reset eio stream
    has_eio_output(true);
    eiooutput = "";

    // AIRLOOP EQUIPMENT TESTING - CurDuctType not set, no reporting
    // Test 13 - Airloop Equipment
    // delete zone sizing info
    DataSizing::CurZoneEqNum = 0;
    DataSizing::NumZoneSizingInput = 0;
    EnergyPlus::DataSizing::ZoneEqSizing.deallocate();
    EnergyPlus::DataSizing::FinalZoneSizing.deallocate();

    DataSizing::CurSysNum = 1;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataSizing::NumSysSizInput = 1;
    DataSizing::SysSizingRunDone = false;
    // start with a hard-sized value as the user input, no system sizing arrays
    inputValue = 0.012;
    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.012, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;             // reset for next test
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 14 - Airloop Equipment - no OA coils
    DataSizing::CurSysNum = 1;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataSizing::NumSysSizInput = 1;
    DataSizing::SysSizingRunDone = true;
    EnergyPlus::DataSizing::FinalSysSizing.allocate(1);
    EnergyPlus::DataSizing::SysSizInput.allocate(1);
    EnergyPlus::DataSizing::SysSizInput(1).AirLoopNum = 1;

    EnergyPlus::DataSizing::FinalSysSizing(1).HeatRetHumRat = 0.012;
    EnergyPlus::DataSizing::FinalSysSizing(1).HeatOutHumRat = 0.006;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.006, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput =
        std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 6.00000E-003\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 15 - Airloop Equipment - 1 OA coil
    DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).NumOACoolCoils = 1;
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).RetHumRatAtCoolPeak = 0.015;
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).PrecoolHumRat = 0.01;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.006, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 16 - Airloop Equipment - 1 OA coil use mixture of outdoor and return hum rat since DataFlowUsedForSizing > 0
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesOutAirVolFlow = 0.01;
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).HeatOAOption = DataSizing::MinOA;
    EnergyPlus::DataSizing::DataFlowUsedForSizing = 0.1; // system volume flow
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0114, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // OUTDOOR AIR SYSTEM EQUIPMENT TESTING
    // Test 17 - Outdoor Air System Equipment, no DOAS air loop
    EnergyPlus::DataSizing::OASysEqSizing.allocate(1);
    EnergyPlus::DataAirLoop::OutsideAirSys.allocate(1);
    DataSizing::CurOASysNum = 1;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    Real64 outAirHumRat = EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).HeatOutHumRat;
    EXPECT_NEAR(outAirHumRat, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 18 - Outdoor Air System Equipment with DOAS system
    EnergyPlus::DataSizing::FinalSysSizing(1).DesOutAirVolFlow = 0.0;
    EnergyPlus::DataAirLoop::OutsideAirSys(1).AirLoopDOASNum = 0;
    state.dataAirLoopHVACDOAS.airloopDOAS.emplace_back();
    state.dataAirLoopHVACDOAS.airloopDOAS[0].HeatOutHumRat = 0.0036;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0036, sizedValue, 0.00001); // DOAS system hum rat
    sizer.autoSizedValue = 0.0;               // reset for next test

    // reset eio stream
    has_eio_output(true);

    // Test 19 - Outdoor Air System Equipment with DOAS system, hard-sized humidity ratio
    // start with an auto-sized value as the user input
    inputValue = 0.00665; // value not previously used

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType); // cumulative of previous calls
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(inputValue, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;                // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput =
        std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 3.60000E-003\n"
                    " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 6.65000E-003\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}

} // namespace EnergyPlus
