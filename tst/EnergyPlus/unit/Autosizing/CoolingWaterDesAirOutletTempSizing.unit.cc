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

#include <EnergyPlus/Autosizing/CoolingWaterDesAirOutletTempSizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataSizing.hh>

namespace EnergyPlus {

TEST_F(AutoSizingFixture, CoolingWaterDesAirOutletTempSizingGauntlet)
{
    // this global state is what would be set up by E+ currently
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    DataEnvironment::StdRhoAir = 1.2;
    static std::string const routineName("CoolingWaterDesAirInletTempSizingGauntlet");

    // create the sizer and set up the flags to specify the sizing configuration
    CoolingWaterDesAirOutletTempSizer sizer;
    Real64 inputValue = 13.7;
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
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(13.7, sizedValue, 0.001); // hard-sized value
    sizer.autoSizedValue = 0.0;            // reset for next test

    std::string eiooutput = std::string("");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test #2 - Zone Equipment, no autosizing, TU type not set
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(13.7, sizedValue, 0.001); // hard-sized value
    sizer.autoSizedValue = 0.0;            // reset for next test

    eiooutput =
        std::string("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n"
                    " Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, User-Specified Design Outlet Air Temperature [C], 13.70000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // reset eio stream
    has_eio_output(true);

    // now allocate sizing arrays for testing autosized field
    EnergyPlus::DataSizing::FinalZoneSizing.allocate(1);
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    EnergyPlus::DataPlant::PlantLoop.allocate(1);
    EnergyPlus::DataSizing::PlantSizData.allocate(1);
    EnergyPlus::DataSizing::PlantSizData(1).ExitTemp = 7.0;
    EnergyPlus::DataSizing::DataPltSizCoolNum = 1;

    // Sizing Type Prerequisites:
    EnergyPlus::DataSizing::FinalZoneSizing(1).CoolDesTemp = 12.88;
    EnergyPlus::DataSizing::ZoneSizingInput.allocate(1);
    EnergyPlus::DataSizing::ZoneSizingInput(DataSizing::CurZoneEqNum).ZoneNum = DataSizing::CurZoneEqNum;

    DataSizing::ZoneSizingRunDone = true;

    // Test 3 - Zone Equipment, Single Duct TU
    DataSizing::TermUnitSingDuct = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(12.88, sizedValue, 0.0001);

    eiooutput =
        std::string(" Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, Design Size Design Outlet Air Temperature [C], 12.88000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 4 - Zone Equipment, Powered Induction TU
    DataSizing::TermUnitSingDuct = false;
    DataSizing::TermUnitPIU = true;
    DataSizing::DataAirFlowUsedForSizing = 0.2;
    DataSizing::DataWaterFlowUsedForSizing = 0.0001;
    DataSizing::DataWaterCoilSizCoolDeltaT = 5.0;
    DataSizing::DataDesInletAirTemp = 23.0;
    DataSizing::DataDesInletAirHumRat = 0.007;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(12.88, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 5 - Zone Equipment, Induction Unit
    DataSizing::TermUnitPIU = false;
    DataSizing::TermUnitIU = true;
    DataSizing::DataWaterLoopNum = 1;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(14.408, sizedValue, 0.001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 6 - Zone Equipment, Induction Unit, add fan heat
    Fans::Fan.allocate(1);
    Fans::Fan(1).DeltaPress = 100.0;
    Fans::Fan(1).MotEff = 0.9;
    Fans::Fan(1).FanEff = 0.6;
    Fans::Fan(1).MotInAirFrac = 0.1;
    Fans::Fan(1).FanType_Num = DataHVACGlobals::FanType_SimpleConstVolume;
    DataSizing::DataFanIndex = 1;
    DataSizing::DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
    DataSizing::DataFanPlacement = DataSizing::zoneFanPlacement::zoneDrawThru;
    DataSizing::DataDesInletAirHumRat = 0.008;
    DataSizing::DataAirFlowUsedForSizing = 0.24;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(15.729, sizedValue, 0.001);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::DataFanIndex = -1;

    // Test 7 - Zone Equipment, Zone Eq Fan Coil, no fan heat
    DataSizing::TermUnitIU = false;
    DataSizing::ZoneEqFanCoil = true;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(12.88, sizedValue, 0.0001); // no fan heat since DataFanInext = -1
    sizer.autoSizedValue = 0.0; // reset for next test

    // reset eio stream
    has_eio_output(true);

    // AIRLOOP EQUIPMENT TESTING - CurDuctType not set, no reporting
    // Test 8 - Airloop Equipment
    // delete zone sizing info
    DataSizing::CurZoneEqNum = 0;
    DataSizing::NumZoneSizingInput = 0;
    EnergyPlus::DataSizing::ZoneEqSizing.deallocate();
    EnergyPlus::DataSizing::FinalZoneSizing.deallocate();

    DataSizing::CurSysNum = 1;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataSizing::NumSysSizInput = 1;
    DataSizing::SysSizingRunDone = false;
    // start with a hard-sized value as the user input, no system sizing arrays
    inputValue = 14.0;
    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(14.0, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;             // reset for next test
    eiooutput = std::string("");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 9 - Airloop Equipment - no OA coils, no fan heat
    DataSizing::SysSizingRunDone = true;
    EnergyPlus::DataSizing::FinalSysSizing.allocate(1);
    EnergyPlus::DataSizing::SysSizInput.allocate(1);
    EnergyPlus::DataSizing::SysSizInput(1).AirLoopNum = 1;

    EnergyPlus::DataSizing::FinalSysSizing(1).CoolSupTemp = 12.15;
    DataSizing::FinalSysSizing(DataSizing::CurSysNum).OutTempAtCoolPeak = 27.88;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(12.15, sizedValue, 0.01);
    sizer.autoSizedValue = 0.0; // reset for next test

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput =
        std::string(" Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, Design Size Design Outlet Air Temperature [C], 12.15000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 10 - Airloop Equipment - no OA coils, with fan heat
    DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).supFanLocation = DataAirSystems::fanPlacement::DrawThru;
    DataSizing::DataFanIndex = 1;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(12.026, sizedValue, 0.001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 10 - Airloop Equipment - 1 OA coil, use PrecoolHumRat
    DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).NumOACoolCoils = 1;
    EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).PrecoolTemp = 12.21;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(12.026, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 11 - Airloop Equipment - 1 OA coil, DataDesOutletAirTemp is set
    DataSizing::DataDesOutletAirTemp = 10.6;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(10.476, sizedValue, 0.001); // includes impact of fan heat
    EXPECT_LT(sizedValue, DataSizing::DataDesOutletAirTemp);
    sizer.autoSizedValue = 0.0; // reset for next test

    // OUTDOOR AIR SYSTEM EQUIPMENT TESTING
    // Test 12 - Outdoor Air System Equipment, no DOAS air loop
    EnergyPlus::DataSizing::OASysEqSizing.allocate(1);
    EnergyPlus::DataAirLoop::OutsideAirSys.allocate(1);
    DataSizing::CurOASysNum = 1;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    Real64 outAirTemp = EnergyPlus::DataSizing::FinalSysSizing(DataSizing::CurSysNum).PrecoolTemp;
    EXPECT_NEAR(outAirTemp, sizedValue, 0.00001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 13 - Outdoor Air System Equipment with DOAS system
    EnergyPlus::DataSizing::FinalSysSizing(1).DesOutAirVolFlow = 0.0;
    EnergyPlus::DataAirLoop::OutsideAirSys(1).AirLoopDOASNum = 0;
    state.dataAirLoopHVACDOAS.airloopDOAS.emplace_back();
    state.dataAirLoopHVACDOAS.airloopDOAS[0].PrecoolTemp = 11.44;

    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(11.44, sizedValue, 0.00001); // DOAS system hum rat
    sizer.autoSizedValue = 0.0;               // reset for next test

    // reset eio stream
    has_eio_output(true);

    // Test 14 - Outdoor Air System Equipment with DOAS system, hard-sized humidity ratio
    // start with an auto-sized value as the user input
    inputValue = 14.44; // value not previously used

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType); // cumulative of previous calls
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(inputValue, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;                // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput =
        std::string(" Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, Design Size Design Outlet Air Temperature [C], 11.44000\n"
                    " Component Sizing Information, Coil:Cooling:Water, MyWaterCoil, User-Specified Design Outlet Air Temperature [C], 14.44000\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}

} // namespace EnergyPlus
