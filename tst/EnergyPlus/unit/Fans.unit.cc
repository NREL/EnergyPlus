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

// EnergyPlus::Fans Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace DataGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::Fans;

TEST_F(EnergyPlusFixture, Fans_FanSizing)
{
    CurZoneEqNum = 0;
    CurSysNum = 0;
    CurOASysNum = 0;
    state.fans.NumFans = 1;
    Fan.allocate(state.fans.NumFans);
    FanNumericFields.allocate(state.fans.NumFans);
    FanNumericFields(state.fans.NumFans).FieldNames.allocate(3);

    int FanNum = 1;
    Fan(FanNum).FanName = "Test Fan";
    Fan(FanNum).FanType = "Fan:OnOff";
    Fan(FanNum).FanType_Num = FanType_SimpleOnOff;
    Fan(FanNum).MaxAirFlowRate = AutoSize;
    Fan(FanNum).DeltaPress = 500.0;
    Fan(FanNum).FanEff = 0.4; // Prevent divide by zero computing RatedPower

    DataEnvironment::StdRhoAir = 1.2;

    FanNumericFields(FanNum).FieldNames(3) = "Maximum Flow Rate";

    CurZoneEqNum = 0;
    CurSysNum = 0;
    CurOASysNum = 0;

    // DataNonZoneNonAirloopValue must be set when CurZoneEqNum and CurSysNum = 0
    DataNonZoneNonAirloopValue = 1.00635;
    SizeFan(state, FanNum);
    EXPECT_DOUBLE_EQ(1.00635, Fan(FanNum).MaxAirFlowRate);
    DataNonZoneNonAirloopValue = 0.0;
    EXPECT_NEAR(1.0371, Fan(FanNum).DesignPointFEI, 0.0001);
}

TEST_F(EnergyPlusFixture, Fans_ConstantVolume_EMSPressureRiseResetTest)
{

    state.fans.NumFans = 1;
    Fans::Fan.allocate(state.fans.NumFans);
    Fans::FanNumericFields.allocate(state.fans.NumFans);
    Fans::FanNumericFields(state.fans.NumFans).FieldNames.allocate(2);
    // set standard air density
    DataEnvironment::StdRhoAir = 1.0;
    // set fan model inputs
    int FanNum(1);
    FanNumericFields(FanNum).FieldNames(1) = "Fan Total Efficiency";
    FanNumericFields(FanNum).FieldNames(2) = "Pressure Rise";
    auto &thisFan(Fans::Fan(FanNum));
    thisFan.FanName = "Test Fan";
    thisFan.FanType = "Fan:ConstantVolume";
    thisFan.FanType_Num = DataHVACGlobals::FanType_SimpleConstVolume;
    thisFan.MaxAirFlowRate = AutoSize;
    thisFan.DeltaPress = 300.0;
    thisFan.FanEff = 1.0;
    thisFan.MotEff = 0.8;
    thisFan.MotInAirFrac = 1.0;
    thisFan.AvailSchedPtrNum = -1.0;
    thisFan.MaxAirFlowRate = 1.0;
    thisFan.MinAirMassFlowRate = 0.0;
    thisFan.MaxAirMassFlowRate = thisFan.MaxAirFlowRate;
    thisFan.InletAirMassFlowRate = thisFan.MaxAirMassFlowRate;
    thisFan.RhoAirStdInit = DataEnvironment::StdRhoAir;
    thisFan.EMSFanPressureOverrideOn = false;
    thisFan.EMSFanPressureValue = 0.0;
    state.fans.LocalTurnFansOn = true;
    state.fans.LocalTurnFansOff = false;
    // simulate the fan
    Fans::SimSimpleFan(state, state.fans, FanNum);
    // fan power = MassFlow * DeltaPress / (FanEff * RhoAir)
    Real64 Result_FanPower = max(0.0, thisFan.MaxAirMassFlowRate * thisFan.DeltaPress / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result_FanPower, thisFan.FanPower); // expects 300 W

    // negative fan pressure rise set using EMS
    thisFan.EMSFanPressureOverrideOn = true;
    thisFan.EMSFanPressureValue = -300.0;
    // simulate the fan with negative pressure rise
    // set using fans EMS actuator for Pressure Rise
    Fans::SimSimpleFan(state, state.fans, FanNum);
    Real64 Result2_FanPower = max(0.0, thisFan.MaxAirMassFlowRate * thisFan.EMSFanPressureValue / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result2_FanPower, thisFan.FanPower); // expects zero
}
TEST_F(EnergyPlusFixture, Fans_OnOff_EMSPressureRiseResetTest)
{

    state.fans.NumFans = 1;
    Fans::Fan.allocate(state.fans.NumFans);
    Fans::FanNumericFields.allocate(state.fans.NumFans);
    Fans::FanNumericFields(state.fans.NumFans).FieldNames.allocate(2);
    // set standard air density
    DataEnvironment::StdRhoAir = 1.0;
    // set fan model inputs
    int FanNum(1);
    FanNumericFields(FanNum).FieldNames(1) = "Fan Total Efficiency";
    FanNumericFields(FanNum).FieldNames(2) = "Pressure Rise";
    auto &thisFan(Fans::Fan(FanNum));
    thisFan.FanName = "Test Fan";
    thisFan.FanType = "Fan:OnOff";
    thisFan.FanType_Num = DataHVACGlobals::FanType_SimpleOnOff;
    thisFan.MaxAirFlowRate = AutoSize;
    thisFan.DeltaPress = 300.0;
    thisFan.FanEff = 1.0;
    thisFan.MotEff = 0.8;
    thisFan.MotInAirFrac = 1.0;
    thisFan.AvailSchedPtrNum = -1.0;
    thisFan.MaxAirFlowRate = 1.0;
    thisFan.MinAirMassFlowRate = 0.0;
    thisFan.MaxAirMassFlowRate = thisFan.MaxAirFlowRate;
    thisFan.InletAirMassFlowRate = thisFan.MaxAirMassFlowRate;
    thisFan.RhoAirStdInit = DataEnvironment::StdRhoAir;
    thisFan.EMSFanPressureOverrideOn = false;
    thisFan.EMSFanPressureValue = 0.0;
    state.fans.LocalTurnFansOn = true;
    state.fans.LocalTurnFansOff = false;
    // simulate the fan
    Fans::SimOnOffFan(state, state.fans, FanNum);
    // fan power = MassFlow * DeltaPress / (FanEff * RhoAir)
    Real64 Result_FanPower = max(0.0, thisFan.MaxAirMassFlowRate * thisFan.DeltaPress / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result_FanPower, thisFan.FanPower); // expects 300 W

    // negative fan pressure rise set using EMS
    thisFan.EMSFanPressureOverrideOn = true;
    thisFan.EMSFanPressureValue = -300.0;
    // simulate the fan with negative pressure rise
    // set using fans EMS actuator for Pressure Rise
    Fans::SimOnOffFan(state, state.fans, FanNum);
    Real64 Result2_FanPower = max(0.0, thisFan.MaxAirMassFlowRate * thisFan.EMSFanPressureValue / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result2_FanPower, thisFan.FanPower); // expects zero
}
TEST_F(EnergyPlusFixture, Fans_VariableVolume_EMSPressureRiseResetTest)
{

    state.fans.NumFans = 1;
    Fans::Fan.allocate(state.fans.NumFans);
    Fans::FanNumericFields.allocate(state.fans.NumFans);
    Fans::FanNumericFields(state.fans.NumFans).FieldNames.allocate(2);
    // set standard air density
    DataEnvironment::StdRhoAir = 1.0;
    // set fan model inputs
    int FanNum(1);
    FanNumericFields(FanNum).FieldNames(1) = "Fan Total Efficiency";
    FanNumericFields(FanNum).FieldNames(2) = "Pressure Rise";
    auto &thisFan(Fans::Fan(FanNum));
    thisFan.FanName = "Test Fan";
    thisFan.FanType = "Fan:VariableVolume";
    thisFan.FanType_Num = DataHVACGlobals::FanType_SimpleVAV;
    thisFan.MaxAirFlowRate = AutoSize;
    thisFan.DeltaPress = 300.0;
    thisFan.FanEff = 1.0;
    thisFan.MotEff = 0.8;
    thisFan.MotInAirFrac = 1.0;
    thisFan.AvailSchedPtrNum = -1.0;
    thisFan.MaxAirFlowRate = 1.0;
    thisFan.MinAirMassFlowRate = 0.0;
    thisFan.MaxAirMassFlowRate = thisFan.MaxAirFlowRate;
    thisFan.InletAirMassFlowRate = thisFan.MaxAirMassFlowRate;
    thisFan.RhoAirStdInit = DataEnvironment::StdRhoAir;
    // VAV Fan Power Coefficients
    thisFan.FanCoeff(1) = 0.06990146;
    thisFan.FanCoeff(2) = 1.39500612;
    thisFan.FanCoeff(3) = -3.35487336;
    thisFan.FanCoeff(4) = 2.89232315;
    thisFan.FanCoeff(5) = 0.000;
    thisFan.EMSFanPressureOverrideOn = false;
    thisFan.EMSFanPressureValue = 0.0;
    state.fans.LocalTurnFansOn = true;
    state.fans.LocalTurnFansOff = false;
    // simulate the fan
    Fans::SimVariableVolumeFan(state, state.fans, FanNum);
    // fan power = PartLoadFrac * MassFlow * DeltaPress / (FanEff * RhoAir)
    Real64 FlowRatio = 1.0;
    Real64 PartLoadFrac = thisFan.FanCoeff(1) + thisFan.FanCoeff(2) * FlowRatio + thisFan.FanCoeff(3) * FlowRatio * FlowRatio +
                          thisFan.FanCoeff(4) * FlowRatio * FlowRatio * FlowRatio;

    Real64 Result_FanPower = max(0.0, PartLoadFrac * thisFan.MaxAirMassFlowRate * thisFan.DeltaPress / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result_FanPower, thisFan.FanPower); // expects 300 W

    // negative fan pressure rise set using EMS
    thisFan.EMSFanPressureOverrideOn = true;
    thisFan.EMSFanPressureValue = -300.0;
    // simulate the fan with negative pressure rise
    // set using fans EMS actuator for Pressure Rise
    Fans::SimVariableVolumeFan(state, state.fans, FanNum);
    Real64 Result2_FanPower =
        max(0.0, PartLoadFrac * thisFan.MaxAirMassFlowRate * thisFan.EMSFanPressureValue / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result2_FanPower, thisFan.FanPower); // expects zero
}
