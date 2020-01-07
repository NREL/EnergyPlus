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

// EnergyPlus::Standalone ERV Unit Tests

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace HVACManager;

TEST_F(EnergyPlusFixture, CrossMixingReportTest)
{

    // Test for #5007
    int NumOfZones = 2;
    int NumOfCrossMixing = 1;

    DataHeatBalance::Zone.allocate(NumOfZones);
    DataHeatBalFanSys::MAT.allocate(NumOfZones);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(NumOfZones);
    DataHeatBalance::CrossMixing.allocate(NumOfCrossMixing);
    DataHeatBalance::ZnAirRpt.allocate(NumOfZones);
    DataZoneEquipment::CrossMixingReportFlag.allocate(NumOfCrossMixing);
    DataHeatBalFanSys::MCPI.allocate(NumOfZones);
    DataHeatBalFanSys::MCPV.allocate(NumOfZones);
    DataHeatBalFanSys::ZoneAirHumRatAvg.allocate(NumOfZones);

    DataGlobals::NumOfZones = NumOfZones;
    DataHeatBalance::TotCrossMixing = NumOfCrossMixing;
    DataZoneEquipment::CrossMixingReportFlag(1) = true;
    DataHVACGlobals::TimeStepSys = 1.0;
    DataHeatBalFanSys::MCPI = 0.0;
    DataHeatBalFanSys::MCPV = 0.0;
    DataEnvironment::OutBaroPress = 101325.0;
    DataHeatBalFanSys::MAT(1) = 22.0;
    DataHeatBalFanSys::MAT(2) = 25.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.001;
    DataHeatBalFanSys::ZoneAirHumRat(2) = 0.0011;
    DataHeatBalFanSys::ZoneAirHumRatAvg = DataHeatBalFanSys::ZoneAirHumRat;
    DataEnvironment::StdRhoAir = 1.20;

    DataHeatBalance::CrossMixing(1).ZonePtr = 1;
    DataHeatBalance::CrossMixing(1).FromZone = 2;
    DataHeatBalance::CrossMixing(1).DesiredAirFlowRate = 0.1;
    DataZoneEquipment::ZoneEquipConfig.allocate(NumOfZones);
    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(2).NumInletNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(2).NumExhaustNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(2).NumReturnNodes = 0;

    // Call HVACManager
    ReportAirHeatBalance();

    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixVolume, DataHeatBalance::ZnAirRpt(2).MixVolume, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixVdotCurDensity, DataHeatBalance::ZnAirRpt(2).MixVdotCurDensity, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixVdotStdDensity, DataHeatBalance::ZnAirRpt(2).MixVdotStdDensity, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixMass, DataHeatBalance::ZnAirRpt(2).MixMass, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixMdot, DataHeatBalance::ZnAirRpt(2).MixMdot, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixHeatLoss, DataHeatBalance::ZnAirRpt(2).MixHeatGain, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixHeatGain, DataHeatBalance::ZnAirRpt(2).MixHeatLoss, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixLatentLoss, DataHeatBalance::ZnAirRpt(2).MixLatentGain, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixLatentGain, DataHeatBalance::ZnAirRpt(2).MixLatentLoss, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixTotalLoss, DataHeatBalance::ZnAirRpt(2).MixTotalGain, 0.0001);
    EXPECT_NEAR(DataHeatBalance::ZnAirRpt(1).MixTotalGain, DataHeatBalance::ZnAirRpt(2).MixTotalLoss, 0.0001);

    // Cleanup
    DataHeatBalance::Zone.deallocate();
    DataHeatBalFanSys::MAT.deallocate();
    DataHeatBalFanSys::ZoneAirHumRat.deallocate();
    DataHeatBalance::CrossMixing.deallocate();
    DataHeatBalance::ZnAirRpt.deallocate();
    DataZoneEquipment::CrossMixingReportFlag.deallocate();
    DataHeatBalFanSys::MCPI.deallocate();
    DataHeatBalFanSys::MCPV.deallocate();
    DataHeatBalFanSys::ZoneAirHumRatAvg.deallocate();
}

TEST_F(EnergyPlusFixture, InfiltrationReportTest)
{

    int NumOfZones = 2;

    DataHeatBalance::Zone.allocate(NumOfZones);
    DataHeatBalFanSys::MAT.allocate(NumOfZones);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(NumOfZones);
    DataHeatBalance::ZnAirRpt.allocate(NumOfZones);
    DataHeatBalFanSys::MCPI.allocate(NumOfZones);
    DataHeatBalFanSys::MCPV.allocate(NumOfZones);
    DataHeatBalFanSys::ZoneAirHumRatAvg.allocate(NumOfZones);

    DataGlobals::NumOfZones = NumOfZones;
    DataHVACGlobals::TimeStepSys = 1.0;
    DataHeatBalFanSys::MCPI(1) = 1.0;
    DataHeatBalFanSys::MCPI(2) = 1.5;
    DataHeatBalFanSys::MCPV(1) = 2.0;
    DataHeatBalFanSys::MCPV(2) = 2.5;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutHumRat = 0.0005;
    DataHeatBalFanSys::MAT(1) = 22.0;
    DataHeatBalFanSys::MAT(2) = 25.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.001;
    DataHeatBalFanSys::ZoneAirHumRat(2) = 0.0011;
    DataHeatBalFanSys::ZoneAirHumRatAvg = DataHeatBalFanSys::ZoneAirHumRat;
    DataEnvironment::StdRhoAir = 1.20;
    DataHeatBalance::Zone(1).OutDryBulbTemp = 20.0;
    DataHeatBalance::Zone(2).OutDryBulbTemp = 20.0;
    DataZoneEquipment::ZoneEquipConfig.allocate(NumOfZones);
    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(2).NumInletNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(2).NumExhaustNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(2).NumReturnNodes = 0;
    // Call HVACManager
    ReportAirHeatBalance();

    EXPECT_NEAR(2.9971591, DataHeatBalance::ZnAirRpt(1).InfilVolumeCurDensity, 0.0001);
    EXPECT_NEAR(5.9943183, DataHeatBalance::ZnAirRpt(1).VentilVolumeCurDensity, 0.0001);
    EXPECT_NEAR(2.9827908, DataHeatBalance::ZnAirRpt(1).InfilVolumeStdDensity, 0.0001);
    EXPECT_NEAR(5.9655817, DataHeatBalance::ZnAirRpt(1).VentilVolumeStdDensity, 0.0001);
    EXPECT_NEAR(4.5421638, DataHeatBalance::ZnAirRpt(2).InfilVolumeCurDensity, 0.0001);
    EXPECT_NEAR(7.5702731, DataHeatBalance::ZnAirRpt(2).VentilVolumeCurDensity, 0.0001);
    EXPECT_NEAR(4.4741862, DataHeatBalance::ZnAirRpt(2).InfilVolumeStdDensity, 0.0001);
    EXPECT_NEAR(7.4569771, DataHeatBalance::ZnAirRpt(2).VentilVolumeStdDensity, 0.0001);
}

TEST_F(EnergyPlusFixture, ExfilAndExhaustReportTest)
{

    int NumOfZones = 2;

    DataHeatBalance::Zone.allocate(NumOfZones);
    DataHeatBalFanSys::MAT.allocate(NumOfZones);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(NumOfZones);
    DataHeatBalance::ZnAirRpt.allocate(NumOfZones);
    DataHeatBalFanSys::MCPI.allocate(NumOfZones);
    DataHeatBalFanSys::MCPV.allocate(NumOfZones);
    DataHeatBalFanSys::ZoneAirHumRatAvg.allocate(NumOfZones);

    DataGlobals::NumOfZones = NumOfZones;
    DataHVACGlobals::TimeStepSys = 1.0;
    DataHeatBalFanSys::MCPI(1) = 1.0;
    DataHeatBalFanSys::MCPI(2) = 1.5;
    DataHeatBalFanSys::MCPV(1) = 2.0;
    DataHeatBalFanSys::MCPV(2) = 2.5;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutHumRat = 0.0005;
    DataHeatBalFanSys::MAT(1) = 22.0;
    DataHeatBalFanSys::MAT(2) = 25.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.001;
    DataHeatBalFanSys::ZoneAirHumRat(2) = 0.0011;
    DataHeatBalFanSys::ZoneAirHumRatAvg = DataHeatBalFanSys::ZoneAirHumRat;
    DataEnvironment::StdRhoAir = 1.20;
    DataHeatBalance::Zone(1).OutDryBulbTemp = 20.0;
    DataHeatBalance::Zone(2).OutDryBulbTemp = 20.0;
    DataZoneEquipment::ZoneEquipConfig.allocate(NumOfZones);
    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(2).NumInletNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(2).NumExhaustNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(2).NumReturnNodes = 0;
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = 1;

    Fans::Fan.allocate(1);
    Fans::NumFans = 1;
    Fans::Fan(1).FanType_Num = DataHVACGlobals::FanType_ZoneExhaust;
    Fans::Fan(1).OutletAirMassFlowRate = 1.0;
    Fans::Fan(1).OutletAirTemp = 22.0;
    Fans::Fan(1).OutletAirEnthalpy = Psychrometrics::PsyHFnTdbW(Fans::Fan(1).OutletAirTemp, 0.0005);
    Fans::Fan(1).InletNodeNum = 1;

    DataLoopNode::Node.allocate(1);
    DataLoopNode::Node(1).MassFlowRate = 0.0;

    // Call HVACManager
    ReportAirHeatBalance();

    EXPECT_NEAR(9.7853391, DataHeatBalance::ZnAirRpt(1).ExfilTotalLoss, 0.0001);
    EXPECT_NEAR(26.056543, DataHeatBalance::ZnAirRpt(2).ExfilTotalLoss, 0.0001);
    EXPECT_NEAR(6.0, DataHeatBalance::ZnAirRpt(1).ExfilSensiLoss, 0.0001);
    EXPECT_NEAR(20.0, DataHeatBalance::ZnAirRpt(2).ExfilSensiLoss, 0.0001);
    EXPECT_NEAR(23377.40, DataHeatBalance::ZnAirRpt(1).ExhTotalLoss, 0.01);
    EXPECT_NEAR(0, DataHeatBalance::ZnAirRpt(2).ExhTotalLoss, 0.01);
    EXPECT_NEAR(35.841882 * 3600, DataHeatBalance::ZoneTotalExfiltrationHeatLoss, 0.01);
    EXPECT_NEAR(23377.39845 * 3600, DataHeatBalance::ZoneTotalExhaustHeatLoss, 0.01);
}

TEST_F(EnergyPlusFixture, AirloopFlowBalanceTest)
{

    DataGlobals::isPulseZoneSizing = false;
    DataHeatBalance::ZoneAirMassFlow.EnforceZoneMassBalance = false;
    DataGlobals::WarmupFlag = false;
    DataHVACGlobals::AirLoopsSimOnce = true;
    DataEnvironment::StdRhoAir = 1.0;

    DataHVACGlobals::NumPrimaryAirSys = 2;
    DataAirSystems::PrimaryAirSystem.allocate(DataHVACGlobals::NumPrimaryAirSys);
    DataAirSystems::PrimaryAirSystem(1).Name = "System 1";
    DataAirSystems::PrimaryAirSystem(2).Name = "System 2";
        DataAirLoop::AirLoopFlow.allocate(DataHVACGlobals::NumPrimaryAirSys);
    auto &thisAirLoopFlow1(DataAirLoop::AirLoopFlow(1));
    auto &thisAirLoopFlow2(DataAirLoop::AirLoopFlow(2));

    // Case 1 - No flow - no error
    thisAirLoopFlow1.SupFlow = 0.0;
    thisAirLoopFlow1.SysRetFlow = 0.0;
    thisAirLoopFlow1.OAFlow = 0.0;

    thisAirLoopFlow2.SupFlow = 0.0;
    thisAirLoopFlow2.SysRetFlow = 0.0;
    thisAirLoopFlow2.OAFlow = 0.0;

    HVACManager::CheckAirLoopFlowBalance();
    EXPECT_FALSE(has_err_output(true));

    //Case 2 - Both loops are balanced
    thisAirLoopFlow1.SupFlow = 2.0;
    thisAirLoopFlow1.SysRetFlow = 1.0;
    thisAirLoopFlow1.OAFlow = 1.0;

    thisAirLoopFlow2.SupFlow = 3.0;
    thisAirLoopFlow2.SysRetFlow = 3.0;
    thisAirLoopFlow2.OAFlow = 0.0;

    HVACManager::CheckAirLoopFlowBalance();
    EXPECT_FALSE(has_err_output(true));

    //Case 3 - Loop 1 is unbalanced
    thisAirLoopFlow1.SupFlow = 2.0;
    thisAirLoopFlow1.SysRetFlow = 1.0;
    thisAirLoopFlow1.OAFlow = 0.0;

    thisAirLoopFlow2.SupFlow = 3.0;
    thisAirLoopFlow2.SysRetFlow = 3.0;
    thisAirLoopFlow2.OAFlow = 0.0;

    HVACManager::CheckAirLoopFlowBalance();
    EXPECT_TRUE(has_err_output(false));
    std::string error_string = delimited_string({
        "   ** Severe  ** CheckAirLoopFlowBalance: AirLoopHVAC System 1 is unbalanced. Supply is > return plus outdoor air.",
        "   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00",
        "   **   ~~~   **   Flows [m3/s at standard density]: Supply=2.000000  Return=1.000000  Outdoor Air=0.000000",
        "   **   ~~~   **   Imbalance=1.000000",
        "   **   ~~~   **   This error will only be reported once per system." });
    EXPECT_TRUE(compare_err_stream(error_string, true));

    //Case 4 - Loop 2 is unbalanced
    thisAirLoopFlow1.SupFlow = 0.0;
    thisAirLoopFlow1.SysRetFlow = 0.0;
    thisAirLoopFlow1.OAFlow = 0.0;

    thisAirLoopFlow2.SupFlow = 3.0;
    thisAirLoopFlow2.SysRetFlow = 2.0;
    thisAirLoopFlow2.OAFlow = 0.99;

    HVACManager::CheckAirLoopFlowBalance();
    EXPECT_TRUE(has_err_output(false));
    error_string = delimited_string({
        "   ** Severe  ** CheckAirLoopFlowBalance: AirLoopHVAC System 2 is unbalanced. Supply is > return plus outdoor air.",
        "   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00",
        "   **   ~~~   **   Flows [m3/s at standard density]: Supply=3.000000  Return=2.000000  Outdoor Air=0.990000",
        "   **   ~~~   **   Imbalance=1.000000E-002",
        "   **   ~~~   **   This error will only be reported once per system." });
    EXPECT_TRUE(compare_err_stream(error_string, true));

}
