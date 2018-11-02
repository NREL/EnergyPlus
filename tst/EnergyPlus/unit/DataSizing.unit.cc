// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataSizing.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, DataSizingTest_resetHVACSizingGlobals)
{
    DataSizing::ZoneEqSizing.allocate(1);
    DataSizing::CurZoneEqNum = 1;
    bool FirstPass = true;

    // set up Data globals to non-initial state to test that they are reset appropriately
    DataSizing::DataTotCapCurveIndex = 1;
    DataSizing::DataPltSizCoolNum = 1;
    DataSizing::DataPltSizHeatNum = 1;
    DataSizing::DataWaterLoopNum = 1;
    DataSizing::DataCoilNum = 1;
    DataSizing::DataFanOpMode = 1;
    DataSizing::DataCoilIsSuppHeater = true;
    DataSizing::DataIsDXCoil = true;
    DataSizing::DataAutosizable = false;
    DataSizing::DataEMSOverrideON = true;
    DataSizing::DataScalableSizingON = true;
    DataSizing::DataScalableCapSizingON = true;
    DataSizing::DataSysScalableFlowSizingON = true;
    DataSizing::DataSysScalableCapSizingON = true;
    DataSizing::DataDesInletWaterTemp = 1.0;
    DataSizing::DataDesInletAirHumRat = 1.0;
    DataSizing::DataDesInletAirTemp = 1.0;
    DataSizing::DataDesOutletAirTemp = 1.0;
    DataSizing::DataDesOutletAirHumRat = 1.0;
    DataSizing::DataCoolCoilCap = 1.0;
    DataSizing::DataFlowUsedForSizing = 1.0;
    DataSizing::DataAirFlowUsedForSizing = 1.0;
    DataSizing::DataWaterFlowUsedForSizing = 1.0;
    DataSizing::DataCapacityUsedForSizing = 1.0;
    DataSizing::DataDesignCoilCapacity = 1.0;
    DataSizing::DataHeatSizeRatio = 2.0;
    DataSizing::DataEMSOverride = 1.0;
    DataSizing::DataBypassFrac = 1.0;
    DataSizing::DataFracOfAutosizedCoolingAirflow = 2.0;
    DataSizing::DataFracOfAutosizedHeatingAirflow = 2.0;
    DataSizing::DataFlowPerCoolingCapacity = 1.0;
    DataSizing::DataFlowPerHeatingCapacity = 1.0;
    DataSizing::DataFracOfAutosizedCoolingCapacity = 2.0;
    DataSizing::DataFracOfAutosizedHeatingCapacity = 2.0;
    DataSizing::DataAutosizedCoolingCapacity = 1.0;
    DataSizing::DataAutosizedHeatingCapacity = 1.0;
    DataSizing::DataConstantUsedForSizing = 1.0;
    DataSizing::DataFractionUsedForSizing = 1.0;
    DataSizing::DataNonZoneNonAirloopValue = 1.0;
    DataSizing::DataZoneNumber = 1;
    DataSizing::DataFanEnumType = 1;
    DataSizing::DataFanIndex = 1;
    DataSizing::DataWaterCoilSizCoolDeltaT = 1.0;
    DataSizing::DataWaterCoilSizHeatDeltaT = 1.0;
    DataSizing::DataNomCapInpMeth = true;

    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).AirFlow = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingAirFlow = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirFlow = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SystemAirFlow = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).Capacity = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingCapacity = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingCapacity = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).AirVolFlow = 1.0;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).MaxHWVolFlow = 1.0;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).MaxCWVolFlow = 1.0;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).OAVolFlow = 1.0;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesCoolingLoad = 1.0;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesHeatingLoad = 1.0;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingAirVolFlow = 1.0;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirVolFlow = 1.0;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SystemAirVolFlow = 1.0;

    // test a few to ensure not equal to initial state
    EXPECT_NE(DataSizing::DataTotCapCurveIndex, 0);
    EXPECT_NE(DataSizing::DataDesInletWaterTemp, 0.0);
    EXPECT_NE(DataSizing::DataHeatSizeRatio, 1.0);
    EXPECT_NE(DataSizing::DataFanEnumType, -1);
    EXPECT_TRUE(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).AirFlow);
    EXPECT_FALSE(DataSizing::DataAutosizable);

    // function argument initialized to true at beginning of simulation
    EXPECT_TRUE(FirstPass);

    // call reset function
    DataSizing::resetHVACSizingGlobals(DataSizing::CurZoneEqNum, 0, FirstPass);

    // expect function argument to be false after calling resetHVACSizingGlobals
    EXPECT_FALSE(FirstPass);

    // expect these to be reset to the initial state as defined in DataSizing.cc and DataSizing.hh
    EXPECT_EQ(DataSizing::DataTotCapCurveIndex, 0);
    EXPECT_EQ(DataSizing::DataPltSizCoolNum, 0);
    EXPECT_EQ(DataSizing::DataPltSizHeatNum, 0);
    EXPECT_EQ(DataSizing::DataWaterLoopNum, 0);
    EXPECT_EQ(DataSizing::DataCoilNum, 0);
    EXPECT_EQ(DataSizing::DataFanOpMode, 0);
    EXPECT_FALSE(DataSizing::DataCoilIsSuppHeater);
    EXPECT_FALSE(DataSizing::DataIsDXCoil);
    EXPECT_TRUE(DataSizing::DataAutosizable);
    EXPECT_FALSE(DataSizing::DataEMSOverrideON);
    EXPECT_FALSE(DataSizing::DataScalableSizingON);
    EXPECT_FALSE(DataSizing::DataScalableCapSizingON);
    EXPECT_FALSE(DataSizing::DataSysScalableFlowSizingON);
    EXPECT_FALSE(DataSizing::DataSysScalableCapSizingON);
    EXPECT_EQ(DataSizing::DataDesInletWaterTemp, 0.0);
    EXPECT_EQ(DataSizing::DataDesInletAirHumRat, 0.0);
    EXPECT_EQ(DataSizing::DataDesInletAirTemp, 0.0);
    EXPECT_EQ(DataSizing::DataDesOutletAirTemp, 0.0);
    EXPECT_EQ(DataSizing::DataDesOutletAirHumRat, 0.0);
    EXPECT_EQ(DataSizing::DataCoolCoilCap, 0.0);
    EXPECT_EQ(DataSizing::DataFlowUsedForSizing, 0.0);
    EXPECT_EQ(DataSizing::DataAirFlowUsedForSizing, 0.0);
    EXPECT_EQ(DataSizing::DataWaterFlowUsedForSizing, 0.0);
    EXPECT_EQ(DataSizing::DataCapacityUsedForSizing, 0.0);
    EXPECT_EQ(DataSizing::DataDesignCoilCapacity, 0.0);
    EXPECT_EQ(DataSizing::DataHeatSizeRatio, 1.0);
    EXPECT_EQ(DataSizing::DataEMSOverride, 0.0);
    EXPECT_EQ(DataSizing::DataBypassFrac, 0.0);
    EXPECT_EQ(DataSizing::DataFracOfAutosizedCoolingAirflow, 1.0);
    EXPECT_EQ(DataSizing::DataFracOfAutosizedHeatingAirflow, 1.0);
    EXPECT_EQ(DataSizing::DataFlowPerCoolingCapacity, 0.0);
    EXPECT_EQ(DataSizing::DataFlowPerHeatingCapacity, 0.0);
    EXPECT_EQ(DataSizing::DataFracOfAutosizedCoolingCapacity, 1.0);
    EXPECT_EQ(DataSizing::DataFracOfAutosizedHeatingCapacity, 1.0);
    EXPECT_EQ(DataSizing::DataAutosizedCoolingCapacity, 0.0);
    EXPECT_EQ(DataSizing::DataAutosizedHeatingCapacity, 0.0);
    EXPECT_EQ(DataSizing::DataConstantUsedForSizing, 0.0);
    EXPECT_EQ(DataSizing::DataFractionUsedForSizing, 0.0);
    EXPECT_EQ(DataSizing::DataNonZoneNonAirloopValue, 0.0);
    EXPECT_EQ(DataSizing::DataZoneNumber, 0);
    EXPECT_EQ(DataSizing::DataFanEnumType, -1);
    EXPECT_EQ(DataSizing::DataFanIndex, -1);
    EXPECT_EQ(DataSizing::DataWaterCoilSizCoolDeltaT, 0.0);
    EXPECT_EQ(DataSizing::DataWaterCoilSizHeatDeltaT, 0.0);
    EXPECT_FALSE(DataSizing::DataNomCapInpMeth);

    EXPECT_FALSE(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).AirFlow);
    EXPECT_FALSE(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingAirFlow);
    EXPECT_FALSE(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirFlow);
    EXPECT_FALSE(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SystemAirFlow);
    EXPECT_FALSE(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).Capacity);
    EXPECT_FALSE(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingCapacity);
    EXPECT_FALSE(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingCapacity);

    EXPECT_EQ(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).AirVolFlow, 0.0);
    EXPECT_EQ(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).MaxHWVolFlow, 0.0);
    EXPECT_EQ(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).MaxCWVolFlow, 0.0);
    EXPECT_EQ(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).OAVolFlow, 0.0);
    EXPECT_EQ(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesCoolingLoad, 0.0);
    EXPECT_EQ(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesHeatingLoad, 0.0);
    EXPECT_EQ(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingAirVolFlow, 0.0);
    EXPECT_EQ(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirVolFlow, 0.0);
    EXPECT_EQ(DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SystemAirVolFlow, 0.0);

    // Test clean return if CurZoneEqNum = 0
    FirstPass = true;
    DataSizing::CurZoneEqNum = 0;
    DataSizing::resetHVACSizingGlobals(DataSizing::CurZoneEqNum, 0, FirstPass);
    EXPECT_FALSE(FirstPass);

    // Test clean return if ZoneEqSizing is not allocated
    DataSizing::ZoneEqSizing.deallocate();
    DataSizing::CurZoneEqNum = 1;
    FirstPass = true;
    // call reset function
    DataSizing::resetHVACSizingGlobals(DataSizing::CurZoneEqNum, 0, FirstPass);
    EXPECT_FALSE(FirstPass);
}
