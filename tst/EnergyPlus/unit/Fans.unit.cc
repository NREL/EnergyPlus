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

// EnergyPlus::Fans Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Fans.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::Fans;

TEST_F(EnergyPlusFixture, Fans_FanSizing)
{
    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 0;
    state->dataSize->CurOASysNum = 0;
    state->dataFans->NumFans = 1;
    state->dataFans->Fan.allocate(state->dataFans->NumFans);
    state->dataFans->FanNumericFields.allocate(state->dataFans->NumFans);
    state->dataFans->FanNumericFields(state->dataFans->NumFans).FieldNames.allocate(3);

    int FanNum = 1;
    state->dataFans->Fan(FanNum).FanName = "Test Fan";
    state->dataFans->Fan(FanNum).FanType = "Fan:OnOff";
    state->dataFans->Fan(FanNum).FanType_Num = FanType_SimpleOnOff;
    state->dataFans->Fan(FanNum).MaxAirFlowRate = AutoSize;
    state->dataFans->Fan(FanNum).DeltaPress = 500.0;
    state->dataFans->Fan(FanNum).FanEff = 0.4; // Prevent divide by zero computing RatedPower

    state->dataEnvrn->StdRhoAir = 1.2;

    state->dataFans->FanNumericFields(FanNum).FieldNames(3) = "Maximum Flow Rate";

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 0;
    state->dataSize->CurOASysNum = 0;

    // DataNonZoneNonAirloopValue must be set when CurZoneEqNum and CurSysNum = 0
    state->dataSize->DataNonZoneNonAirloopValue = 1.00635;
    SizeFan(*state, FanNum);
    EXPECT_DOUBLE_EQ(1.00635, state->dataFans->Fan(FanNum).MaxAirFlowRate);
    state->dataSize->DataNonZoneNonAirloopValue = 0.0;
    EXPECT_NEAR(1.0371, state->dataFans->Fan(FanNum).DesignPointFEI, 0.0001);
}

TEST_F(EnergyPlusFixture, Fans_ConstantVolume_EMSPressureRiseResetTest)
{

    state->dataFans->NumFans = 1;
    state->dataFans->Fan.allocate(state->dataFans->NumFans);
    state->dataFans->FanNumericFields.allocate(state->dataFans->NumFans);
    state->dataFans->FanNumericFields(state->dataFans->NumFans).FieldNames.allocate(2);
    // set standard air density
    state->dataEnvrn->StdRhoAir = 1.0;
    // set fan model inputs
    int FanNum(1);
    state->dataFans->FanNumericFields(FanNum).FieldNames(1) = "Fan Total Efficiency";
    state->dataFans->FanNumericFields(FanNum).FieldNames(2) = "Pressure Rise";
    auto &thisFan(state->dataFans->Fan(FanNum));
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
    thisFan.RhoAirStdInit = state->dataEnvrn->StdRhoAir;
    thisFan.EMSFanPressureOverrideOn = false;
    thisFan.EMSFanPressureValue = 0.0;
    state->dataFans->LocalTurnFansOn = true;
    state->dataFans->LocalTurnFansOff = false;
    // simulate the fan
    Fans::SimSimpleFan(*state, FanNum);
    // fan power = MassFlow * DeltaPress / (FanEff * RhoAir)
    Real64 Result_FanPower = max(0.0, thisFan.MaxAirMassFlowRate * thisFan.DeltaPress / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result_FanPower, thisFan.FanPower); // expects 300 W

    // negative fan pressure rise set using EMS
    thisFan.EMSFanPressureOverrideOn = true;
    thisFan.EMSFanPressureValue = -300.0;
    // simulate the fan with negative pressure rise
    // set using fans EMS actuator for Pressure Rise
    Fans::SimSimpleFan(*state, FanNum);
    Real64 Result2_FanPower = max(0.0, thisFan.MaxAirMassFlowRate * thisFan.EMSFanPressureValue / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result2_FanPower, thisFan.FanPower); // expects zero
}
TEST_F(EnergyPlusFixture, Fans_OnOff_EMSPressureRiseResetTest)
{

    state->dataFans->NumFans = 1;
    state->dataFans->Fan.allocate(state->dataFans->NumFans);
    state->dataFans->FanNumericFields.allocate(state->dataFans->NumFans);
    state->dataFans->FanNumericFields(state->dataFans->NumFans).FieldNames.allocate(2);
    // set standard air density
    state->dataEnvrn->StdRhoAir = 1.0;
    // set fan model inputs
    int FanNum(1);
    state->dataFans->FanNumericFields(FanNum).FieldNames(1) = "Fan Total Efficiency";
    state->dataFans->FanNumericFields(FanNum).FieldNames(2) = "Pressure Rise";
    auto &thisFan(state->dataFans->Fan(FanNum));
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
    thisFan.RhoAirStdInit = state->dataEnvrn->StdRhoAir;
    thisFan.EMSFanPressureOverrideOn = false;
    thisFan.EMSFanPressureValue = 0.0;
    state->dataFans->LocalTurnFansOn = true;
    state->dataFans->LocalTurnFansOff = false;
    // simulate the fan
    Fans::SimOnOffFan(*state, FanNum);
    // fan power = MassFlow * DeltaPress / (FanEff * RhoAir)
    Real64 Result_FanPower = max(0.0, thisFan.MaxAirMassFlowRate * thisFan.DeltaPress / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result_FanPower, thisFan.FanPower); // expects 300 W

    // negative fan pressure rise set using EMS
    thisFan.EMSFanPressureOverrideOn = true;
    thisFan.EMSFanPressureValue = -300.0;
    // simulate the fan with negative pressure rise
    // set using fans EMS actuator for Pressure Rise
    Fans::SimOnOffFan(*state, FanNum);
    Real64 Result2_FanPower = max(0.0, thisFan.MaxAirMassFlowRate * thisFan.EMSFanPressureValue / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result2_FanPower, thisFan.FanPower); // expects zero
}
TEST_F(EnergyPlusFixture, Fans_VariableVolume_EMSPressureRiseResetTest)
{

    state->dataFans->NumFans = 1;
    state->dataFans->Fan.allocate(state->dataFans->NumFans);
    state->dataFans->FanNumericFields.allocate(state->dataFans->NumFans);
    state->dataFans->FanNumericFields(state->dataFans->NumFans).FieldNames.allocate(2);
    // set standard air density
    state->dataEnvrn->StdRhoAir = 1.0;
    // set fan model inputs
    int FanNum(1);
    state->dataFans->FanNumericFields(FanNum).FieldNames(1) = "Fan Total Efficiency";
    state->dataFans->FanNumericFields(FanNum).FieldNames(2) = "Pressure Rise";
    auto &thisFan(state->dataFans->Fan(FanNum));
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
    thisFan.RhoAirStdInit = state->dataEnvrn->StdRhoAir;
    // VAV Fan Power Coefficients
    thisFan.FanCoeff(1) = 0.06990146;
    thisFan.FanCoeff(2) = 1.39500612;
    thisFan.FanCoeff(3) = -3.35487336;
    thisFan.FanCoeff(4) = 2.89232315;
    thisFan.FanCoeff(5) = 0.000;
    thisFan.EMSFanPressureOverrideOn = false;
    thisFan.EMSFanPressureValue = 0.0;
    state->dataFans->LocalTurnFansOn = true;
    state->dataFans->LocalTurnFansOff = false;
    // simulate the fan
    Fans::SimVariableVolumeFan(*state, FanNum);
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
    Fans::SimVariableVolumeFan(*state, FanNum);
    Real64 Result2_FanPower =
        max(0.0, PartLoadFrac * thisFan.MaxAirMassFlowRate * thisFan.EMSFanPressureValue / (thisFan.FanEff * thisFan.RhoAirStdInit));
    EXPECT_DOUBLE_EQ(Result2_FanPower, thisFan.FanPower); // expects zero
}
