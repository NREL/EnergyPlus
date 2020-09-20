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

#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/SimAirServingZones.hh>

namespace EnergyPlus {

TEST_F(AutoSizingFixture, HeatingCapacitySizingGauntlet)
{
    // fan heat is needed for DOAS systems
    std::string const idf_objects = delimited_string({

        "  Fan:SystemModel,",
        "    MyFan,                       !- Name",
        "    ,                            !- Availability Schedule Name",
        "    TestFanAirInletNode,         !- Air Inlet Node Name",
        "    TestFanOutletNode,           !- Air Outlet Node Name",
        "    0.2,                         !- Design Maximum Air Flow Rate",
        "    Discrete ,                   !- Speed Control Method",
        "    0.0,                         !- Electric Power Minimum Flow Rate Fraction",
        "    100.0,                       !- Design Pressure Rise",
        "    0.9 ,                        !- Motor Efficiency",
        "    1.0 ,                        !- Motor In Air Stream Fraction",
        "    AUTOSIZE,                    !- Design Electric Power Consumption",
        "    TotalEfficiencyAndPressure,  !- Design Power Sizing Method",
        "    ,                            !- Electric Power Per Unit Flow Rate",
        "    ,                            !- Electric Power Per Unit Flow Rate Per Unit Pressure",
        "    0.50;                        !- Fan Total Efficiency",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    DataEnvironment::StdRhoAir = 1.2;
    // call simulate to trigger sizing call
    HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(state, "MyFan"));
    DataLoopNode::Node(1).Press = 101325.0;
    DataLoopNode::Node(1).Temp = 24.0;
    HVACFan::fanObjs[0]->simulate(state, _, _, _, _);

    // this global state is what would be set up by E+ currently
    static std::string const routineName("HeatingCapacitySizingGauntlet");
    DataSizing::ZoneEqSizing.allocate(1);

    // create the sizer and set up the flags to specify the sizing configuration
    HeatingCapacitySizer sizer;
    Real64 inputValue = 5125.3;
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
    DataSizing::CurTermUnitSizingNum = 1;
    DataSizing::TermUnitSingDuct = true;

    // Test #1 - Zone Equipment, no autosizing
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5125.3, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;            // reset for next test

    // reset eio stream
    has_eio_output(true);
    printFlag = true;

    DataSizing::ZoneSizingRunDone = true;
    DataSizing::ZoneSizingInput.allocate(1);
    DataSizing::ZoneSizingInput(1).ZoneNum = 1;

    DataSizing::ZoneEqSizing(1).DesignSizeFromParent = true;
    DataSizing::ZoneEqSizing(1).DesHeatingLoad = sizedValue;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5125.3, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;            // reset for next test
    DataSizing::ZoneEqSizing(1).DesignSizeFromParent = false;

    std::string eiooutput =
        std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Heating Capacity [W], 5125.30000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // now allocate sizing arrays for testing autosized field
    DataSizing::TermUnitFinalZoneSizing.allocate(1);
    DataSizing::TermUnitFinalZoneSizing(1).DesHeatCoilInTempTU = 15.0;
    DataSizing::TermUnitFinalZoneSizing(1).DesHeatCoilInHumRatTU = 0.007;
    DataSizing::TermUnitFinalZoneSizing(1).HeatDesTemp = 30.0;
    DataSizing::TermUnitFinalZoneSizing(1).HeatDesHumRat = 0.007;
    DataSizing::TermUnitFinalZoneSizing(1).ZoneTempAtHeatPeak = 20.0;
    DataSizing::TermUnitFinalZoneSizing(1).ZoneHumRatAtHeatPeak = 0.006;
    DataSizing::TermUnitSizing.allocate(1);
    DataSizing::TermUnitSizing(1).InducRat = 0.5;
    DataSizing::TermUnitSizing(1).AirVolFlow = 0.2;
    DataSizing::FinalZoneSizing.allocate(1);
    DataSizing::FinalZoneSizing(1).ZoneTempAtHeatPeak = 20.0;
    DataSizing::FinalZoneSizing(1).ZoneRetTempAtHeatPeak = 24.0;
    DataSizing::FinalZoneSizing(1).ZoneHumRatAtHeatPeak = 0.006;
    DataSizing::FinalZoneSizing(1).DesHeatMassFlow = 0.2;
    DataSizing::FinalZoneSizing(1).HeatDesTemp = 30.0;
    DataSizing::FinalZoneSizing(1).HeatDesHumRat = 0.004;
    DataSizing::FinalZoneSizing(1).OutTempAtHeatPeak = 5.0;
    DataSizing::FinalZoneSizing(1).OutHumRatAtHeatPeak = 0.002;

    DataSizing::ZoneEqSizing(1).ATMixerHeatPriDryBulb = 20.0;
    DataSizing::ZoneEqSizing(1).ATMixerHeatPriHumRat = 0.007;

    DataPlant::PlantLoop.allocate(1);
    DataSizing::DataWaterLoopNum = 1;
    DataSizing::DataWaterCoilSizHeatDeltaT = 5.0;

    // Test 2 - Zone Equipment, Single Duct TU
    DataSizing::TermUnitSingDuct = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.zoneSizingInput.allocate(1);
    sizer.zoneSizingInput(1).ZoneNum = 1;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3664.27, sizedValue, 0.01);
    EXPECT_NEAR(1.2, DataEnvironment::StdRhoAir, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 3 - Zone Equipment, Zone Eq Fan Coil
    DataSizing::TermUnitSingDuct = false;
    DataSizing::ZoneEqFanCoil = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2024.55, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 4 - Zone DX Equipment
    DataSizing::ZoneEqFanCoil = false;
    DataSizing::DataFlowUsedForSizing = DataSizing::FinalZoneSizing(1).DesHeatMassFlow / DataEnvironment::StdRhoAir;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_HeatingEmpirical), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2024.55, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 5 - Zone Equipment, Terminal Induction Unit
    DataSizing::TermUnitIU = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2442.84, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 6 - Zone Equipment
    DataSizing::TermUnitIU = false;
    DataSizing::ZoneEqSizing(1).OAVolFlow = 0.05;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_CoolingSingleSpeed), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2935.6, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 7 - Zone DX Equipment, inlet side AT Mixer
    DataSizing::ZoneEqSizing(1).ATMixerVolFlow = 0.03;
    DataSizing::ZoneEqDXCoil = true;
    // start with an auto-sized value as the user input, AT Mixer present
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_CoolingSingleSpeed), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1360.5, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::ZoneEqSizing(1).ATMixerVolFlow = 0.0;

    // Test 8 - Zone DX Equipment, no AT Mixer, local OA only
    DataSizing::ZoneEqSizing(1).ATMixerVolFlow = 0.0;
    // start with an auto-sized value as the user input, AT Mixer present
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_CoolingSingleSpeed), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2935.6, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 9 - Zone Equipment, powered induction unit
    DataSizing::TermUnitPIU = true;
    DataSizing::TermUnitSizing(1).MinFlowFrac = 0.3;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_CoolingSingleSpeed), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2809.27, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 10 - Zone Equipment, powered induction unit
    DataSizing::TermUnitSizing(1).InducesPlenumAir = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_CoolingSingleSpeed), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6229.26, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::ZoneEqSizing(1).OAVolFlow = 0.0;

    // Test 11 - Zone Equipment, powered induction unit
    DataSizing::DataCoolCoilCap = 4250.0; // overrides capacity
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_CoolingSingleSpeed), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(4250.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0;        // reset for next test
    DataSizing::DataCoolCoilCap = 0.0; // reset for next test

    DataSizing::ZoneEqSizing(1).HeatingCapacity = true;
    // Test 12 - Zone Equipment, parent set heating capacity
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_CoolingSingleSpeed), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5125.3, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::ZoneEqSizing(1).HeatingCapacity = false;

    // Test 13 - Zone Equipment, parent sets constant and fraction used for sizing
    DataSizing::DataConstantUsedForSizing = 2800.0;
    DataSizing::DataFractionUsedForSizing = 1.0;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_CoolingSingleSpeed), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2800.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::DataConstantUsedForSizing = 0.0;
    DataSizing::DataFractionUsedForSizing = 0.0;

    // Test 14 - Zone Equipment, EMS override
    DataSizing::DataEMSOverrideON = true;
    DataSizing::DataEMSOverride = 1500.0;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(
        this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_CoolingSingleSpeed), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1500.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::DataEMSOverrideON = false;
    DataSizing::DataEMSOverride = 0.0;

    // reset eio stream
    has_eio_output(true);

    // Test 15 - Zone Equipment, hard size
    // start with an auto-sized value as the user input
    inputValue = 5500.0;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5500.0, sizedValue, 0.01); // hard size value
    sizer.autoSizedValue = 0.0;            // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Heating Capacity [W], 6229.25822\n"
                            " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Heating Capacity [W], 5500.00000\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
    eiooutput = "";

    // AIRLOOP Equipment
    // Test 16 - Airloop Equipment, hard size, no sizing run
    DataSizing::CurZoneEqNum = 0;
    DataSizing::NumZoneSizingInput = 0;
    // baseFlags.otherEqType = false; set in initialize function based on other flags
    DataSizing::ZoneEqSizing.deallocate();
    DataSizing::FinalZoneSizing.deallocate();

    DataSizing::CurSysNum = 1;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataSizing::NumSysSizInput = 1;
    DataSizing::SysSizingRunDone = false;
    // start with a hard-sized value as the user input, no system sizing arrays
    inputValue = 2700.8;
    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(2700.8, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;              // reset for next test
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    DataSizing::SysSizingRunDone = true;
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirLoop::AirLoopControlInfo.allocate(1);
    DataSizing::UnitarySysEqSizing.allocate(1);
    DataSizing::SysSizInput.allocate(1);
    DataSizing::SysSizInput(1).AirLoopNum = 1;
    DataSizing::FinalSysSizing.allocate(1);
    DataSizing::FinalSysSizing(1).HeatRetTemp = 20.0;
    DataSizing::FinalSysSizing(1).HeatOutTemp = 5.0;
    DataSizing::FinalSysSizing(1).HeatSupTemp = 30.0;
    DataSizing::FinalSysSizing(1).HeatRetHumRat = 0.006;
    DataSizing::FinalSysSizing(1).HeatOutHumRat = 0.004;
    DataSizing::FinalSysSizing(1).PreheatTemp = 10.0;
    DataSizing::FinalSysSizing(1).PreheatHumRat = 0.005;
    DataSizing::FinalSysSizing(1).DesMainVolFlow = 0.3;
    DataSizing::FinalSysSizing(1).DesHeatVolFlow = 0.27;
    DataSizing::FinalSysSizing(1).DesCoolVolFlow = 0.24;

    // Test 17 - Airloop Equipment, no OA
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5024.3, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    Real64 unAdjustedSize = sizedValue;

    // Test 18 - Airloop Equipment, no OA, fraction of autosized heating capacity
    DataSizing::FinalSysSizing(1).HeatingCapMethod = DataSizing::FractionOfAutosizedHeatingCapacity;
    DataSizing::FinalSysSizing(1).FractionOfAutosizedHeatingCapacity = 0.5;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(unAdjustedSize * 0.5, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::FinalSysSizing(1).HeatingCapMethod = DataSizing::None;
    DataSizing::FinalSysSizing(1).FractionOfAutosizedHeatingCapacity = 0.0;

    // Test 19 - Airloop Equipment, with min OA
    DataSizing::FinalSysSizing(1).DesOutAirVolFlow = 0.02;
    DataSizing::FinalSysSizing(1).HeatOAOption = DataSizing::MinOA;
    DataAirSystems::PrimaryAirSystem(1).NumOAHeatCoils = 1;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2250.88, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 20 - Airloop Equipment, unitary system sets capacity
    DataSizing::UnitarySysEqSizing(1).HeatingCapacity = true;
    DataSizing::UnitarySysEqSizing(1).DesHeatingLoad = 4500.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(4500.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 21 - Airloop Equipment, desiccant heating coil
    DataSizing::UnitarySysEqSizing(1).HeatingCapacity = false;
    DataSizing::DataDesicRegCoil = true;
    DataSizing::DataDesOutletAirTemp = 32.0;
    DataSizing::DataDesInletAirTemp = 5.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5426.24, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::DataDesicRegCoil = false;

    // Test 22 - Airloop Equipment, unitary system sets air flow
    DataSizing::DataFlowUsedForSizing = 0.0;
    DataSizing::UnitarySysEqSizing(1).AirFlow = true;
    DataSizing::UnitarySysEqSizing(1).AirVolFlow = 0.15;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2049.91, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::UnitarySysEqSizing(1).AirFlow = false;

    // Test 23 - Airloop Equipment, unitary system sets heating air flow
    DataSizing::UnitarySysEqSizing(1).HeatingAirFlow = true;
    DataSizing::UnitarySysEqSizing(1).HeatingAirVolFlow = 0.12;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1688.16, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::UnitarySysEqSizing(1).HeatingAirFlow = false;

    // Test 24 - Airloop Equipment, CurDuctType = Main
    DataSizing::CurDuctType = DataHVACGlobals::Main;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3858.66, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 25 - Airloop Equipment, CurDuctType = Main, SysAirMinFlowRat > 0
    DataSizing::FinalSysSizing(1).SysAirMinFlowRat = 0.3;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1326.41, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 26 - Airloop Equipment, CurDuctType = Cooling
    DataSizing::CurDuctType = DataHVACGlobals::Cooling;
    DataSizing::FinalSysSizing(1).SysAirMinFlowRat = 0.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3135.16, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 27 - Airloop Equipment, CurDuctType = Main, SysAirMinFlowRat > 0
    DataSizing::FinalSysSizing(1).SysAirMinFlowRat = 0.3;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1109.36, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 28 - Airloop Equipment, CurDuctType = Heating
    DataSizing::CurDuctType = DataHVACGlobals::Heating;
    DataSizing::FinalSysSizing(1).SysAirMinFlowRat = 0.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3496.91, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 29 - Airloop Equipment, CurDuctType = Other
    DataSizing::CurDuctType = DataHVACGlobals::Other;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3858.66, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 30 - Airloop Equipment, CurDuctType = RAB
    DataSizing::CurDuctType = DataHVACGlobals::RAB;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3858.66, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 31 - Airloop Equipment, CurDuctType = Main, Unitary system does not set size for HW coils
    // even when AirLoopControlInfo(1).UnitarySysSimulating = true
    DataAirLoop::AirLoopControlInfo(1).UnitarySys = true;
    DataSizing::UnitaryHeatCap = 4790.0;
    DataSizing::CurDuctType = DataHVACGlobals::Main;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3858.66, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 32 - Airloop Equipment, CurDuctType = Main, Unitary system does set size for non-water heating coils
    DataAirLoop::AirLoopControlInfo(1).UnitarySysSimulating = true;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_HeatingEmpirical), "MyDXCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(4790.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 33 - Airloop Equipment, CurDuctType = Main, Unitary system has supp heating coil
    DataSizing::DataCoilIsSuppHeater = true;
    DataSizing::SuppHeatCap = 5325.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5325.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::DataCoilIsSuppHeater = false;
    DataSizing::SuppHeatCap = 0.0;

    // Test 34 - Airloop Equipment, CurDuctType = Main, Unitary system sets heat coil to same size as cool coil
    DataSizing::DataCoolCoilCap = 4325.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(4325.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::DataCoolCoilCap = 0.0;

    DataAirLoop::AirLoopControlInfo(1).UnitarySys = false;

    // Test 35 - Airloop Equipment, CurDuctType = Main, air loop uses scaled sizing
    DataSizing::FinalSysSizing(1).HeatingCapMethod = DataSizing::CapacityPerFloorArea;
    DataSizing::FinalSysSizing(1).HeatingTotalCapacity = 3325.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3325.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 36 - Airloop Equipment, CurDuctType = Main, air loop uses scaled sizing
    DataSizing::FinalSysSizing(1).HeatingCapMethod = DataSizing::HeatingDesignCapacity;
    DataSizing::FinalSysSizing(1).HeatingTotalCapacity = 2325.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2325.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::FinalSysSizing(1).HeatingCapMethod = DataSizing::None;
    DataSizing::FinalSysSizing(1).HeatingTotalCapacity = 0.0;

    // Test 37 - OA Equipment, OA Sys capacity sizing
    DataSizing::CurOASysNum = 1;
    DataAirLoop::OutsideAirSys.allocate(1);
    DataSizing::OASysEqSizing.allocate(1);
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(120.58, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 38 - OA Equipment, OA Sys air flow sizing
    DataSizing::OASysEqSizing(1).AirFlow = true;
    DataSizing::OASysEqSizing(1).AirVolFlow = 1.5;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(9043.73, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::OASysEqSizing(1).AirFlow = false;

    // Test 39 - OA Equipment, OA Sys heating air flow sizing
    DataSizing::OASysEqSizing(1).HeatingAirFlow = true;
    DataSizing::OASysEqSizing(1).HeatingAirVolFlow = 1.2;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(7234.98, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::OASysEqSizing(1).HeatingAirFlow = false;

    // Test 40 - OA Equipment, OA Sys unitary sets capacity
    DataSizing::OASysEqSizing(1).HeatingCapacity = true;
    DataSizing::OASysEqSizing(1).DesHeatingLoad = 4400.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(4400.0, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::OASysEqSizing(1).HeatingCapacity = false;
    DataSizing::OASysEqSizing(1).DesHeatingLoad = 0.0;

    // Test 41 - OA Equipment, desiccant heating coil
    DataSizing::DataDesicRegCoil = true;
    DataSizing::DataDesOutletAirTemp = 38.0;
    DataSizing::DataDesInletAirTemp = 5.0;
    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(795.85, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::DataDesicRegCoil = false;

    // Test 42 - OA Equipment, DOAS Air loop
    DataSizing::OASysEqSizing(1).HeatingAirFlow = false;
    DataAirLoop::OutsideAirSys.allocate(1);
    DataAirLoop::OutsideAirSys(1).AirLoopDOASNum = 0;
    state.dataAirLoopHVACDOAS->airloopDOAS.emplace_back();
    state.dataAirLoopHVACDOAS->airloopDOAS[0].SizingMassFlow = 1.1;
    state.dataAirLoopHVACDOAS->airloopDOAS[0].HeatOutTemp = 5.0;
    state.dataAirLoopHVACDOAS->airloopDOAS[0].PreheatTemp = 11.0;
    state.dataAirLoopHVACDOAS->airloopDOAS[0].m_FanIndex = 0;
    state.dataAirLoopHVACDOAS->airloopDOAS[0].FanBlowTroughFlag = true;
    state.dataAirLoopHVACDOAS->airloopDOAS[0].m_FanTypeNum = SimAirServingZones::Fan_System_Object;

    // start with an autosized value
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6448.73, sizedValue, 0.01); // capacity includes system fan heat
    sizer.autoSizedValue = 0.0;             // reset for next test

    // reset eio stream
    has_eio_output(true);

    // Test 43 - OA Equipment, DOAS Air loop
    // start with an autosized value
    inputValue = 4200.0;
    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(4200.0, sizedValue, 0.01); // hard sized capacity
    sizer.autoSizedValue = 0.0;            // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Heating Capacity [W], 6448.73336\n"
                            " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Heating Capacity [W], 4200.00000\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}

} // namespace EnergyPlus
