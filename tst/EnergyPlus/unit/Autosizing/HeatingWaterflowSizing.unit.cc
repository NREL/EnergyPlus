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

#include <EnergyPlus/Autosizing/HeatingWaterflowSizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>

namespace EnergyPlus {

TEST_F(AutoSizingFixture, HeatingWaterflowSizingGauntlet)
{
    // this global state is what would be set up by E+ currently
    DataEnvironment::StdRhoAir = 1.2;
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    static std::string const routineName("HeatingWaterflowSizingGauntlet");

    // create the sizer and set up the flags to specify the sizing configuration
    HeatingWaterflowSizer sizer;
    Real64 inputValue = 5;
    bool errorsFound = false;
    bool printFlag = false;

    // uninitialized sizing type
    Real64 sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_TRUE(errorsFound);
    EXPECT_EQ(AutoSizingResultType::ErrorType2, sizer.errorType);
    EXPECT_NEAR(0.0, sizedValue, 0.01); // unitialized sizing types always return 0
    errorsFound = false;

    // test auto calculate
    DataSizing::DataFractionUsedForSizing = 0.0;
    DataSizing::DataConstantUsedForSizing = 1.0;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    // developer errror, DataFractionUsedForSizing = 0 and Constant > 0
    EXPECT_EQ(AutoSizingResultType::ErrorType1, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0, sizedValue, 0.01);
    EXPECT_NEAR(5.0, sizer.originalValue, 0.01);
    sizer.autoSizedValue = 0.0;         // reset for next test

    DataSizing::DataFractionUsedForSizing = 1.0;
    DataSizing::DataConstantUsedForSizing = 0.0;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0, sizedValue, 0.01); // auto calculates to 0
    EXPECT_NEAR(5.0, sizer.originalValue, 0.01); // developer errror, DataFractionUsedForSizing = 0 and Constant > 0
    sizer.autoSizedValue = 0.0;         // reset for next test

    DataSizing::DataFractionUsedForSizing = 1.0;
    DataSizing::DataConstantUsedForSizing = 1.0;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(1.0, sizedValue, 0.01); // auto calculates to 1
    sizer.autoSizedValue = 0.0;         // reset for next test

    // autosized input with AutoCalculate
    inputValue = DataSizing::AutoSize;
    DataSizing::DataFractionUsedForSizing = 1.0;
    DataSizing::DataConstantUsedForSizing = 2.0;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);  // autosized input
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2.0, sizedValue, 0.01); // autosizes to 1.0
    sizer.autoSizedValue = 0.0;         // reset for next test

    // reset AutoCalculate globals
    DataSizing::DataFractionUsedForSizing = 0.0;
    DataSizing::DataConstantUsedForSizing = 0.0;

    // ZONE EQUIPMENT TESTING
    inputValue = 5;
    DataSizing::CurZoneEqNum = 1;
    DataSizing::CurTermUnitSizingNum = 1;
    DataSizing::TermUnitSingDuct = true;
    DataSizing::TermUnitSizing.allocate(1);
    DataSizing::TermUnitSizing(1).MaxHWVolFlow = 0.005;

    // Test #1 - Zone Equipment, no autosizing
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    // reset eio stream
    has_eio_output(true);
    printFlag = true;

    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    std::string eiooutput =
        std::string("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n"
                    " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Maximum Water Flow Rate [m3/s], 5.00000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // reset eio stream
    has_eio_output(true);

    // now allocate sizing arrays for testing autosized field
    EnergyPlus::DataSizing::FinalZoneSizing.allocate(1);
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    EnergyPlus::DataSizing::FinalZoneSizing(1).DesHeatMassFlow = 0.3;
    EnergyPlus::DataSizing::FinalZoneSizing(1).HeatDesTemp = 30.0;
    EnergyPlus::DataSizing::FinalZoneSizing(1).HeatDesHumRat = 0.004;
    EnergyPlus::DataSizing::FinalZoneSizing(1).ZoneRetTempAtHeatPeak = 19.0;
    EnergyPlus::DataSizing::FinalZoneSizing(1).ZoneTempAtHeatPeak = 21.0;
    EnergyPlus::DataSizing::FinalZoneSizing(1).OutTempAtHeatPeak = 10.0;
    EnergyPlus::DataSizing::FinalZoneSizing(1).ZoneHumRatAtHeatPeak = 0.006;
    EnergyPlus::DataSizing::FinalZoneSizing(1).OutHumRatAtHeatPeak = 0.003;
    DataSizing::ZoneSizingRunDone = true;

    // Test 2 - Zone Equipment, Single Duct TU, UA sizes to 0
    DataSizing::TermUnitSingDuct = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    DataSizing::ZoneSizingInput.allocate(1);
    DataSizing::ZoneSizingInput(1).ZoneNum = 1;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.005, sizedValue, 0.01);
    EXPECT_NEAR(0.005, EnergyPlus::DataSizing::TermUnitSizing(1).MaxHWVolFlow, 0.0001);
    EXPECT_NEAR(1.2, DataEnvironment::StdRhoAir, 0.01);

    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Maximum Water Flow Rate [m3/s], 5.00000E-003\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 3 - Zone Equipment, Single Duct TU
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.005, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 4 - Zone Equipment, Powered Induction TU
    DataSizing::TermUnitSingDuct = false;
    DataSizing::TermUnitPIU = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.005, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 5 - Zone Equipment, Induction TU
    DataSizing::TermUnitPIU = false;
    DataSizing::TermUnitIU = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.005, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 6 - Zone Equipment, Zone Eq Fan Coil
    DataSizing::TermUnitIU = false;
    DataSizing::ZoneEqFanCoil = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.005, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 7 - Zone Equipment, Other Equipment
    DataSizing::ZoneEqFanCoil = false;
    DataSizing::ZoneEqUnitHeater = true;
    // baseFlags.otherEqType = true; set in initialize function based on other flags
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.005, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 8 - Zone Equipment, Other Equipment
    DataSizing::ZoneEqUnitHeater = false;
    DataSizing::DataWaterLoopNum = 1;
    DataSizing::DataWaterCoilSizHeatDeltaT = 10.0;
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).FluidName = "Water";
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.000066, sizedValue, 0.000001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 9 - Zone Equipment, Other Equipment
    EnergyPlus::DataSizing::ZoneEqSizing(1).AirVolFlow = 0.5;
    EnergyPlus::DataSizing::ZoneEqSizing(1).SystemAirFlow = true;
    EnergyPlus::DataSizing::FinalZoneSizing(1).DesHeatMassFlow = 0.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.000133, sizedValue, 0.000001); // uses a mass flow rate for sizing
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 10 - Zone Equipment, Other Equipment
    EnergyPlus::DataSizing::ZoneEqSizing(1).AirVolFlow = 0.0;
    EnergyPlus::DataSizing::ZoneEqSizing(1).SystemAirFlow = false;
    EnergyPlus::DataSizing::ZoneEqSizing(1).HeatingAirVolFlow = 0.25;
    EnergyPlus::DataSizing::ZoneEqSizing(1).HeatingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.000066, sizedValue, 0.000001); // uses a mass flow rate for sizing
    sizer.autoSizedValue = 0.0;         // reset for next test
    EnergyPlus::DataSizing::ZoneEqSizing(1).HeatingAirVolFlow = 0.0;
    EnergyPlus::DataSizing::ZoneEqSizing(1).HeatingAirFlow = false;

    // reset eio stream
    has_eio_output(true);
    eiooutput = "";

    // AIRLOOP EQUIPMENT TESTING - CurDuctType not set, no reporting
    // Test 11 - Airloop Equipment
    DataSizing::CurZoneEqNum = 0;
    DataSizing::NumZoneSizingInput = 0;
    DataSizing::CurTermUnitSizingNum = 0;
    // baseFlags.otherEqType = false; set in initialize function based on other flags
    EnergyPlus::DataSizing::ZoneEqSizing.deallocate();
    EnergyPlus::DataSizing::FinalZoneSizing.deallocate();

    DataSizing::CurSysNum = 1;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataSizing::NumSysSizInput = 1;
    DataSizing::SysSizingRunDone = false;
    // start with a hard-sized value as the user input, no system sizing arrays
    inputValue = 5.0;
    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 12 - Airloop Equipment - CurDuctType not set
    DataSizing::CurSysNum = 1;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataSizing::NumSysSizInput = 1;
    DataSizing::SysSizingRunDone = true;
    EnergyPlus::DataSizing::FinalSysSizing.allocate(1);
    EnergyPlus::DataSizing::SysSizInput.allocate(1);
    EnergyPlus::DataSizing::SysSizInput(1).AirLoopNum = 1;

    EnergyPlus::DataSizing::FinalSysSizing(1).DesMainVolFlow = 5.0; // CurDuctType not set
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    DataSizing::DataCapacityUsedForSizing = 5000.0;
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.000121, sizedValue, 0.000001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Maximum Water Flow Rate [m3/s], 1.21516E-004\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // reset eio stream
    has_eio_output(true);
    eiooutput = "";

    // OUTDOOR AIR SYSTEM EQUIPMENT TESTING
    // Test 13 - Outdoor Air System Equipment, no DOAS air loop
    DataSizing::CurOASysNum = 1;
    DataSizing::OASysEqSizing.allocate(1);
    // start with an auto-sized value as the user input
    inputValue = 0.0002;
    printFlag = true;
    errorsFound = false;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0002, sizedValue, 0.000001);
    sizer.autoSizedValue = 0.0; // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Maximum Water Flow Rate [m3/s], 1.21516E-004\n"
                            " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Maximum Water Flow Rate [m3/s], 2.00000E-004\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}

} // namespace EnergyPlus
