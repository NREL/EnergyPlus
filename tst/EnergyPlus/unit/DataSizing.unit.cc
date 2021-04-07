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

// EnergyPlus::DataSizing Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataSizing.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, DataSizingTest_resetHVACSizingGlobals)
{
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->CurZoneEqNum = 1;
    bool FirstPass = true;

    // set up Data globals to non-initial state to test that they are reset appropriately
    state->dataSize->DataTotCapCurveIndex = 1;
    state->dataSize->DataPltSizCoolNum = 1;
    state->dataSize->DataPltSizHeatNum = 1;
    state->dataSize->DataWaterLoopNum = 1;
    state->dataSize->DataCoilNum = 1;
    state->dataSize->DataFanOpMode = 1;
    state->dataSize->DataCoilIsSuppHeater = true;
    state->dataSize->DataIsDXCoil = true;
    state->dataSize->DataAutosizable = false;
    state->dataSize->DataEMSOverrideON = true;
    state->dataSize->DataScalableSizingON = true;
    state->dataSize->DataScalableCapSizingON = true;
    state->dataSize->DataSysScalableFlowSizingON = true;
    state->dataSize->DataSysScalableCapSizingON = true;
    state->dataSize->DataDesInletWaterTemp = 1.0;
    state->dataSize->DataDesInletAirHumRat = 1.0;
    state->dataSize->DataDesInletAirTemp = 1.0;
    state->dataSize->DataDesOutletAirTemp = 1.0;
    state->dataSize->DataDesOutletAirHumRat = 1.0;
    state->dataSize->DataCoolCoilCap = 1.0;
    state->dataSize->DataFlowUsedForSizing = 1.0;
    state->dataSize->DataAirFlowUsedForSizing = 1.0;
    state->dataSize->DataWaterFlowUsedForSizing = 1.0;
    state->dataSize->DataCapacityUsedForSizing = 1.0;
    state->dataSize->DataDesignCoilCapacity = 1.0;
    state->dataSize->DataHeatSizeRatio = 2.0;
    state->dataSize->DataEMSOverride = 1.0;
    state->dataSize->DataBypassFrac = 1.0;
    state->dataSize->DataFracOfAutosizedCoolingAirflow = 2.0;
    state->dataSize->DataFracOfAutosizedHeatingAirflow = 2.0;
    state->dataSize->DataFlowPerCoolingCapacity = 1.0;
    state->dataSize->DataFlowPerHeatingCapacity = 1.0;
    state->dataSize->DataFracOfAutosizedCoolingCapacity = 2.0;
    state->dataSize->DataFracOfAutosizedHeatingCapacity = 2.0;
    state->dataSize->DataAutosizedCoolingCapacity = 1.0;
    state->dataSize->DataAutosizedHeatingCapacity = 1.0;
    state->dataSize->DataConstantUsedForSizing = 1.0;
    state->dataSize->DataFractionUsedForSizing = 1.0;
    state->dataSize->DataNonZoneNonAirloopValue = 1.0;
    state->dataSize->DataZoneNumber = 1;
    state->dataSize->DataFanEnumType = 1;
    state->dataSize->DataFanIndex = 1;
    state->dataSize->DataWaterCoilSizCoolDeltaT = 1.0;
    state->dataSize->DataWaterCoilSizHeatDeltaT = 1.0;
    state->dataSize->DataNomCapInpMeth = true;

    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).AirFlow = true;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).CoolingAirFlow = true;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).HeatingAirFlow = true;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SystemAirFlow = true;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).Capacity = true;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).CoolingCapacity = true;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).HeatingCapacity = true;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).AirVolFlow = 1.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).MaxHWVolFlow = 1.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).MaxCWVolFlow = 1.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).OAVolFlow = 1.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).DesCoolingLoad = 1.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).DesHeatingLoad = 1.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).CoolingAirVolFlow = 1.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).HeatingAirVolFlow = 1.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SystemAirVolFlow = 1.0;

    // test a few to ensure not equal to initial state
    EXPECT_NE(state->dataSize->DataTotCapCurveIndex, 0);
    EXPECT_NE(state->dataSize->DataDesInletWaterTemp, 0.0);
    EXPECT_NE(state->dataSize->DataHeatSizeRatio, 1.0);
    EXPECT_NE(state->dataSize->DataFanEnumType, -1);
    EXPECT_TRUE(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).AirFlow);
    EXPECT_FALSE(state->dataSize->DataAutosizable);

    // function argument initialized to true at beginning of simulation
    EXPECT_TRUE(FirstPass);

    // call reset function
    DataSizing::resetHVACSizingGlobals(*state, state->dataSize->CurZoneEqNum, 0, FirstPass);

    // expect function argument to be false after calling resetHVACSizingGlobals
    EXPECT_FALSE(FirstPass);

    // expect these to be reset to the initial state as defined in DataSizing.cc and DataSizing.hh
    EXPECT_EQ(state->dataSize->DataTotCapCurveIndex, 0);
    EXPECT_EQ(state->dataSize->DataPltSizCoolNum, 0);
    EXPECT_EQ(state->dataSize->DataPltSizHeatNum, 0);
    EXPECT_EQ(state->dataSize->DataWaterLoopNum, 0);
    EXPECT_EQ(state->dataSize->DataCoilNum, 0);
    EXPECT_EQ(state->dataSize->DataFanOpMode, 0);
    EXPECT_FALSE(state->dataSize->DataCoilIsSuppHeater);
    EXPECT_FALSE(state->dataSize->DataIsDXCoil);
    EXPECT_TRUE(state->dataSize->DataAutosizable);
    EXPECT_FALSE(state->dataSize->DataEMSOverrideON);
    EXPECT_FALSE(state->dataSize->DataScalableSizingON);
    EXPECT_FALSE(state->dataSize->DataScalableCapSizingON);
    EXPECT_FALSE(state->dataSize->DataSysScalableFlowSizingON);
    EXPECT_FALSE(state->dataSize->DataSysScalableCapSizingON);
    EXPECT_EQ(state->dataSize->DataDesInletWaterTemp, 0.0);
    EXPECT_EQ(state->dataSize->DataDesInletAirHumRat, 0.0);
    EXPECT_EQ(state->dataSize->DataDesInletAirTemp, 0.0);
    EXPECT_EQ(state->dataSize->DataDesOutletAirTemp, 0.0);
    EXPECT_EQ(state->dataSize->DataDesOutletAirHumRat, 0.0);
    EXPECT_EQ(state->dataSize->DataCoolCoilCap, 0.0);
    EXPECT_EQ(state->dataSize->DataFlowUsedForSizing, 0.0);
    EXPECT_EQ(state->dataSize->DataAirFlowUsedForSizing, 0.0);
    EXPECT_EQ(state->dataSize->DataWaterFlowUsedForSizing, 0.0);
    EXPECT_EQ(state->dataSize->DataCapacityUsedForSizing, 0.0);
    EXPECT_EQ(state->dataSize->DataDesignCoilCapacity, 0.0);
    EXPECT_EQ(state->dataSize->DataHeatSizeRatio, 1.0);
    EXPECT_EQ(state->dataSize->DataEMSOverride, 0.0);
    EXPECT_EQ(state->dataSize->DataBypassFrac, 0.0);
    EXPECT_EQ(state->dataSize->DataFracOfAutosizedCoolingAirflow, 1.0);
    EXPECT_EQ(state->dataSize->DataFracOfAutosizedHeatingAirflow, 1.0);
    EXPECT_EQ(state->dataSize->DataFlowPerCoolingCapacity, 0.0);
    EXPECT_EQ(state->dataSize->DataFlowPerHeatingCapacity, 0.0);
    EXPECT_EQ(state->dataSize->DataFracOfAutosizedCoolingCapacity, 1.0);
    EXPECT_EQ(state->dataSize->DataFracOfAutosizedHeatingCapacity, 1.0);
    EXPECT_EQ(state->dataSize->DataAutosizedCoolingCapacity, 0.0);
    EXPECT_EQ(state->dataSize->DataAutosizedHeatingCapacity, 0.0);
    EXPECT_EQ(state->dataSize->DataConstantUsedForSizing, 0.0);
    EXPECT_EQ(state->dataSize->DataFractionUsedForSizing, 0.0);
    EXPECT_EQ(state->dataSize->DataNonZoneNonAirloopValue, 0.0);
    EXPECT_EQ(state->dataSize->DataZoneNumber, 0);
    EXPECT_EQ(state->dataSize->DataFanEnumType, -1);
    EXPECT_EQ(state->dataSize->DataFanIndex, -1);
    EXPECT_EQ(state->dataSize->DataWaterCoilSizCoolDeltaT, 0.0);
    EXPECT_EQ(state->dataSize->DataWaterCoilSizHeatDeltaT, 0.0);
    EXPECT_FALSE(state->dataSize->DataNomCapInpMeth);

    EXPECT_FALSE(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).AirFlow);
    EXPECT_FALSE(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).CoolingAirFlow);
    EXPECT_FALSE(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).HeatingAirFlow);
    EXPECT_FALSE(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SystemAirFlow);
    EXPECT_FALSE(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).Capacity);
    EXPECT_FALSE(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).CoolingCapacity);
    EXPECT_FALSE(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).HeatingCapacity);

    EXPECT_EQ(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).AirVolFlow, 0.0);
    EXPECT_EQ(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).MaxHWVolFlow, 0.0);
    EXPECT_EQ(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).MaxCWVolFlow, 0.0);
    EXPECT_EQ(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).OAVolFlow, 0.0);
    EXPECT_EQ(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).DesCoolingLoad, 0.0);
    EXPECT_EQ(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).DesHeatingLoad, 0.0);
    EXPECT_EQ(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).CoolingAirVolFlow, 0.0);
    EXPECT_EQ(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).HeatingAirVolFlow, 0.0);
    EXPECT_EQ(state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SystemAirVolFlow, 0.0);

    // Test clean return if CurZoneEqNum = 0
    FirstPass = true;
    state->dataSize->CurZoneEqNum = 0;
    DataSizing::resetHVACSizingGlobals(*state, state->dataSize->CurZoneEqNum, 0, FirstPass);
    EXPECT_FALSE(FirstPass);

    // Test clean return if ZoneEqSizing is not allocated
    state->dataSize->ZoneEqSizing.deallocate();
    state->dataSize->CurZoneEqNum = 1;
    FirstPass = true;
    // call reset function
    DataSizing::resetHVACSizingGlobals(*state, state->dataSize->CurZoneEqNum, 0, FirstPass);
    EXPECT_FALSE(FirstPass);
}
