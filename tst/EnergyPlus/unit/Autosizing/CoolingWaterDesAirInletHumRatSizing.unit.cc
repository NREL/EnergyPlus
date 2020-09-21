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

#include <EnergyPlus/Autosizing/CoolingWaterDesAirInletHumRatSizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>

namespace EnergyPlus {

TEST_F(AutoSizingFixture, CoolingWaterDesAirInletHumRatSizingGauntlet)
{
    // this global state is what would be set up by E+ currently
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    DataEnvironment::StdRhoAir = 1.2;
    static std::string const routineName("CoolingWaterDesAirInletHumRatSizingGauntlet");

    // create the sizer and set up the flags to specify the sizing configuration
    CoolingWaterDesAirInletHumRatSizer sizer;
    Real64 inputValue = 0.009;
    bool errorsFound = false;
    bool printFlag = false;

    // uninitialized sizing type
    Real64 sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_TRUE(errorsFound);
    EXPECT_EQ(AutoSizingResultType::ErrorType2, sizer.errorType);
    EXPECT_NEAR(0.0, sizedValue, 0.01); // unitialized sizing types always return 0
    errorsFound = false;

    // ZONE EQUIPMENT TESTING
    DataSizing::CurZoneEqNum = 1;

    // reset eio stream
    has_eio_output(true);

    // Test #1 - Zone Equipment, no autosizing, TU type not set
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.009, sizedValue, 0.001); // hard-sized value
    sizer.autoSizedValue = 0.0;            // reset for next test

    std::string eiooutput = std::string("");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test #2 - Zone Equipment, no autosizing, TU type not set
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.009, sizedValue, 0.001); // hard-sized value
    sizer.autoSizedValue = 0.0;            // reset for next test

    eiooutput =
        std::string("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n"
                    " Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, User-Specified Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 9.00000E-003\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // reset eio stream
    has_eio_output(true);

    // now allocate sizing arrays for testing autosized field
    EnergyPlus::DataSizing::FinalZoneSizing.allocate(1);
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);

    // Sizing Type Prerequisites:
    // TermUnitIU, ZoneEqFanCoil w/ no OA
    EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).ZoneHumRatAtCoolPeak = 0.007;
    // All other TU types (TermUnitSingDuct, TermUnitPIU)
    EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInHumRat = 0.008;
    // ZoneEqFanCoil w/ OA
    EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolMassFlow = 0.01;
    EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).OutHumRatAtCoolPeak = 0.004;

    EnergyPlus::DataSizing::ZoneSizingInput.allocate(1);
    EnergyPlus::DataSizing::ZoneSizingInput(DataSizing::CurZoneEqNum).ZoneNum = DataSizing::CurZoneEqNum;

    DataSizing::ZoneSizingRunDone = true;

    // Test 3 - Zone Equipment, Single Duct TU
    DataSizing::TermUnitSingDuct = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.008, sizedValue, 0.0001);

    eiooutput =
        std::string(" Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, Design Size Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 8.00000E-003\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 4 - Zone Equipment, Powered Induction TU
    DataSizing::TermUnitSingDuct = false;
    DataSizing::TermUnitPIU = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.008, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 5 - Zone Equipment, Induction Unit
    DataSizing::TermUnitPIU = false;
    DataSizing::TermUnitIU = true;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.007, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 6 - Zone Equipment, Zone Eq Fan Coil
    DataSizing::TermUnitIU = false;
    DataSizing::ZoneEqFanCoil = true;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.007, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 7 - Zone Equipment w/ 10% OA
    DataSizing::ZoneEqSizing(1).OAVolFlow =
        EnergyPlus::DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolMassFlow / (10.0 * DataEnvironment::StdRhoAir);
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    Real64 mixedHumRat = 0.9 * 0.007 + 0.1 * 0.004; // 90% of ZoneHumRatAtCoolPeak, 10% of OutHumRatAtCoolPeak
    EXPECT_NEAR(mixedHumRat, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 8 - Zone Equipment w/ AT Mixer at 20% of design flow
    DataSizing::ZoneEqSizing(1).ATMixerCoolPriHumRat = 0.001;
    DataSizing::ZoneEqSizing(1).ATMixerVolFlow = 0.002 / DataEnvironment::StdRhoAir; // AT mass flow smaller than DesCoolMassFlow by factor of 5
    Real64 mixedHumRat2 = 0.8 * 0.007 + 0.2 * 0.001;                                 // 80% of ZoneHumRatAtCoolPeak, 20% of ATMixerCoolPriHumRat

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(mixedHumRat2, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    DataSizing::ZoneEqFanCoil = false;
    // reset eio stream
    has_eio_output(true);
    eiooutput = "";

    // AIRLOOP EQUIPMENT TESTING - CurDuctType not set, no reporting
    // Test 9 - Airloop Equipment
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
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.012, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;             // reset for next test
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 10 - Airloop Equipment - no OA coils
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataSizing::SysSizingRunDone = true;
    EnergyPlus::DataSizing::FinalSysSizing.allocate(1);
    EnergyPlus::DataSizing::SysSizInput.allocate(1);
    EnergyPlus::DataSizing::SysSizInput(1).AirLoopNum = 1;

    EnergyPlus::DataSizing::FinalSysSizing(1).MixHumRatAtCoolPeak = 0.0105;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0105, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput =
        std::string(" Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, Design Size Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 1.05000E-002\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 11 - Airloop Equipment - 1 OA coil, use PrecoolHumRat
    DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).NumOACoolCoils = 1;
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).RetHumRatAtCoolPeak = 0.015;
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).PrecoolHumRat = 0.01;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.010, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 12 - Airloop Equipment - 1 OA coil use mixture of outdoor and return hum rat since DataFlowUsedForSizing > 0
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesOutAirVolFlow = 0.01;
    EnergyPlus::DataSizing::DataFlowUsedForSizing = 0.1; // system volume flow
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0145, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // OUTDOOR AIR SYSTEM EQUIPMENT TESTING
    // Test 13 - Outdoor Air System Equipment, no DOAS air loop
    EnergyPlus::DataSizing::OASysEqSizing.allocate(1);
    state.dataAirLoop->OutsideAirSys.allocate(1);
    DataSizing::CurOASysNum = 1;
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).OutHumRatAtCoolPeak = 0.003;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    Real64 outAirHumRat = EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).OutHumRatAtCoolPeak;
    EXPECT_NEAR(outAirHumRat, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 14 - Outdoor Air System Equipment with DOAS system
    EnergyPlus::DataSizing::FinalSysSizing(1).DesOutAirVolFlow = 0.0;
    state.dataAirLoop->OutsideAirSys(1).AirLoopDOASNum = 0;
    state.dataAirLoopHVACDOAS->airloopDOAS.emplace_back();
    state.dataAirLoopHVACDOAS->airloopDOAS[0].SizingCoolOAHumRat = 0.0036;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0036, sizedValue, 0.00001); // DOAS system hum rat
    sizer.autoSizedValue = 0.0;               // reset for next test

    // reset eio stream
    has_eio_output(true);

    // Test 15 - Outdoor Air System Equipment with DOAS system, hard-sized humidity ratio
    // start with an auto-sized value as the user input
    inputValue = 0.00665; // value not previously used

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType); // cumulative of previous calls
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(inputValue, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;                // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput =
        std::string(" Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, Design Size Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 3.60000E-003\n"
                    " Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, User-Specified Design Inlet Air Humidity Ratio [kgWater/kgDryAir], 6.65000E-003\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}

} // namespace EnergyPlus
