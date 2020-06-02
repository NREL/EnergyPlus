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

#include <gtest/gtest.h>
#include "AutosizingFixture.hh"

#include <ObjexxFCL/Array1D.hh>
#include <EnergyPlus/Autosizing/HeatingAirflowUASizing.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus {

    TEST_F(AutoSizingFixture, HeatingAirflowUASizingGauntlet) {
        // this global state is what would be set up by E+ currently
        DataEnvironment::StdRhoAir = 1.0;
        Array1D<EnergyPlus::DataSizing::TermUnitSizingData> tmpTermUnitData;
        tmpTermUnitData.allocate(1);
        tmpTermUnitData(1).AirVolFlow = 5;
        // there is definitely a better way to do this...
        Array1D<EnergyPlus::DataSizing::ZoneSizingData> tmpFinalZoneSizing;
        Array1D<EnergyPlus::DataSizing::ZoneEqSizingData> tmpZoneEqSizing;
        tmpFinalZoneSizing.allocate(1);
        tmpZoneEqSizing.allocate(1);

        CommonFlags baseFlags;
        baseFlags.compType = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater);
        baseFlags.compName = "MyWaterCoil";
        baseFlags.zoneSizingRunDone = true;

        // set up the flags to specify the sizing configuration
        HeatingAirflowUASizerFlags flags;
        flags.curTermUnitSizingNum = 1;
        flags.curZoneEqNum = 1;
        baseFlags.curZoneEqNum = 1;

        // Test 1 - Single Duct TU
        flags.termUnitSingDuct = true;
        // start with an auto-sized value as the user input
        Real64 inputValue = EnergyPlus::DataSizing::AutoSize;
        // create the sizer and do sizing
        HeatingAirflowUASizer sizer;
        sizer.zoneSizingInput.allocate(1);
        sizer.zoneSizingInput(1).ZoneNum = 1;
        sizer.setParameters(baseFlags, flags, tmpTermUnitData, tmpFinalZoneSizing, tmpZoneEqSizing);
        AutoSizingResultType result = sizer.size(inputValue);
        EXPECT_EQ(AutoSizingResultType::NoError, result);
        EXPECT_TRUE(sizer.wasAutoSized);
        EXPECT_NEAR(5.0, sizer.autoSizedValue, 0.01);
        EXPECT_NEAR(5.0, tmpTermUnitData(1).AirVolFlow, 0.01);
        EXPECT_NEAR(1.0, DataEnvironment::StdRhoAir, 0.01);

        // Test 2 - Powered Induction TU
        flags.termUnitSingDuct = false;
        flags.termUnitPIU = true;
        // start with an auto-sized value as the user input
        inputValue = EnergyPlus::DataSizing::AutoSize;
        // do sizing
        sizer.wasAutoSized = false;
        sizer.setParameters(baseFlags, flags, tmpTermUnitData, tmpFinalZoneSizing, tmpZoneEqSizing);
        result = sizer.size(inputValue);
        EXPECT_EQ(AutoSizingResultType::NoError, result);
        EXPECT_TRUE(sizer.wasAutoSized);
        EXPECT_NEAR(5.0, sizer.autoSizedValue, 0.01);

        // Test 3 - Induction TU
        flags.termUnitPIU = false;
        flags.termUnitIU = true;
        // start with an auto-sized value as the user input
        inputValue = EnergyPlus::DataSizing::AutoSize;
        // do sizing
        sizer.wasAutoSized = false;
        sizer.setParameters(baseFlags, flags, tmpTermUnitData, tmpFinalZoneSizing, tmpZoneEqSizing);
        result = sizer.size(inputValue);
        EXPECT_EQ(AutoSizingResultType::NoError, result);
        EXPECT_TRUE(sizer.wasAutoSized);
        EXPECT_NEAR(5.0, sizer.autoSizedValue, 0.01);

        // Test 4 - Zone Eq Fan Coil
        flags.termUnitIU = false;
        flags.zoneEqFanCoil = true;
        tmpTermUnitData(1).AirVolFlow = 0.0;
        tmpFinalZoneSizing(1).DesHeatVolFlow = 5.0;
        // start with an auto-sized value as the user input
        inputValue = EnergyPlus::DataSizing::AutoSize;
        // do sizing
        sizer.wasAutoSized = false;
        sizer.setParameters(baseFlags, flags, tmpTermUnitData, tmpFinalZoneSizing, tmpZoneEqSizing);
        result = sizer.size(inputValue);
        EXPECT_EQ(AutoSizingResultType::NoError, result);
        EXPECT_TRUE(sizer.wasAutoSized);
        EXPECT_NEAR(5.0, sizer.autoSizedValue, 0.01);

        // Test 5 - Other Equipment
        flags.zoneEqFanCoil = false;
        flags.otherEqType = true;
        tmpFinalZoneSizing(1).DesHeatVolFlow = 0.0;
        tmpZoneEqSizing(1).AirVolFlow = 5.0;
        tmpZoneEqSizing(1).SystemAirFlow = true;
        // start with an auto-sized value as the user input
        inputValue = EnergyPlus::DataSizing::AutoSize;
        // do sizing
        sizer.wasAutoSized = false;
        sizer.setParameters(baseFlags, flags, tmpTermUnitData, tmpFinalZoneSizing, tmpZoneEqSizing);
        result = sizer.size(inputValue);
        EXPECT_EQ(AutoSizingResultType::NoError, result);
        EXPECT_TRUE(sizer.wasAutoSized);
        EXPECT_NEAR(5.0, sizer.autoSizedValue, 0.01);

        // Test 6 - Other Equipment
        tmpZoneEqSizing(1).AirVolFlow = 0.0;
        tmpZoneEqSizing(1).HeatingAirVolFlow = 5.0;
        tmpZoneEqSizing(1).SystemAirFlow = false;
        tmpZoneEqSizing(1).HeatingAirFlow = true;
        // start with an auto-sized value as the user input
        inputValue = EnergyPlus::DataSizing::AutoSize;
        // do sizing
        sizer.wasAutoSized = false;
        sizer.setParameters(baseFlags, flags, tmpTermUnitData, tmpFinalZoneSizing, tmpZoneEqSizing);
        result = sizer.size(inputValue);
        EXPECT_EQ(AutoSizingResultType::NoError, result);
        EXPECT_TRUE(sizer.wasAutoSized);
        EXPECT_NEAR(5.0, sizer.autoSizedValue, 0.01);

        // Test 7 - Other Equipment
        tmpZoneEqSizing(1).HeatingAirVolFlow = 0.0;
        tmpZoneEqSizing(1).HeatingAirFlow = false;
        tmpFinalZoneSizing(1).DesHeatMassFlow = 5.0;
        // start with an auto-sized value as the user input
        inputValue = EnergyPlus::DataSizing::AutoSize;
        // do sizing
        sizer.wasAutoSized = false;
        sizer.setParameters(baseFlags, flags, tmpTermUnitData, tmpFinalZoneSizing, tmpZoneEqSizing);
        result = sizer.size(inputValue);
        EXPECT_EQ(AutoSizingResultType::NoError, result);
        EXPECT_TRUE(sizer.wasAutoSized);
        EXPECT_NEAR(5.0, sizer.autoSizedValue, 0.01);
    }

}
