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

// EnergyPlus::SZVAV Model Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PackagedTerminalHeatPump.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/SZVAVModel.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/WaterCoils.hh>

using namespace EnergyPlus;
using namespace CurveManager;
using namespace DataBranchNodeConnections;
using namespace DataEnvironment;
using namespace DataHeatBalance;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::FanCoilUnits;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::PackagedTerminalHeatPump;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SZVAVModel;
using namespace EnergyPlus::WaterCoils;

TEST_F(EnergyPlusFixture, SZVAV_PTUnit_Testing)
{

    std::string const idf_objects = delimited_string({

        "  Schedule:Compact, OnSched, Fraction, Through: 12/31, For: AllDays, Until: 24:00, 1.0; ",
        "  ScheduleTypeLimits, Fraction, 0.0, 1.0, CONTINUOUS; ",
        "  Curve:Quadratic, PLF Curve, 0.85, 0.15, 0, 0, 1, 0.0, 1.0, Dimensionless, Dimensionless; ",
        "  Curve:Cubic, CubicCurve, 1.0, 0.0, 0.0, 0.0, 0.76, 1.09, , , Dimensionless, Dimensionless; ",
        "  Curve:Biquadratic, BiquadraticCurve, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10, 25.6, 7.2, 48.9, , , Temperature, Temperature, Dimensionless; ",

        " Fan:OnOff,",
        "   Test Fan,                      !- Name",
        "   OnSched,                       !- Availability Schedule Name",
        "   0.7,                           !- Fan Total Efficiency",
        "   13,                            !- Pressure Rise {Pa}",
        "   0.2,                           !- Maximum Flow Rate {m3/s}",
        "   0.9,                           !- Motor Efficiency",
        "   1.0,                           !- Motor In Airstream Fraction",
        "   Fan In Node,                   !- Air Inlet Node Name",
        "   Fan Out Node;                  !- Air Outlet Node Name",

        "Coil:Cooling:DX:SingleSpeed,",
        "	CoolingCoil,                   !- Name",
        " 	OnSched,                       !- Availability Schedule Name",
        "	4000.0,                        !- Gross Rated Total Cooling Capacity { W }",
        "	0.75,                          !- Gross Rated Sensible Heat Ratio",
        "	3.1,                           !- Gross Rated Cooling COP { W / W }",
        "	0.20,                          !- Rated Air Flow Rate { m3 / s }",
        "	,                              !- Rated Evaporator Fan Power Per Volume Flow Rate { W / ( m3 / s ) }",
        "	Fan Out Node,                  !- Air Inlet Node Name",
        "	Cooling Coil Out Node,         !- Air Outlet Node Name",
        "	BiquadraticCurve,              !- Total Cooling Capacity Function of Temperature Curve Name",
        "	CubicCurve,                    !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "	BiquadraticCurve,              !- Energy Input Ratio Function of Temperature Curve Name",
        "	CubicCurve,                    !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "	PLF Curve;                     !- Part Load Fraction Correlation Curve Name",

        "Coil:Heating:DX:SingleSpeed,",
        "   HeatingCoil,                   !- Name",
        "   OnSched,                       !- Availability Schedule Name",
        "   4000.0,                        !- Gross Rated Heating Capacity {W}",
        "   3.1,                           !- Gross Rated Heating COP {W/W}",
        "   0.20,                          !- Rated Air Flow Rate {m3/s}",
        "   ,                              !- Rated Supply Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   Cooling Coil Out Node,         !- Air Inlet Node Name",
        "   Heating Coil Out Node,         !- Air Outlet Node Name",
        "   BiquadraticCurve,              !- Heating Capacity Function of Temperature Curve Name",
        "   CubicCurve,                    !- Heating Capacity Function of Flow Fraction Curve Name",
        "   BiquadraticCurve,              !- Energy Input Ratio Function of Temperature Curve Name",
        "   CubicCurve,                    !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "   PLF Curve,                     !- Part Load Fraction Correlation Curve Name",
        "   ,                              !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "   2.0,                           !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "   ,                              !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "   5.0,                           !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "   200.0,                         !- Crankcase Heater Capacity {W}",
        "   0.0,                           !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "   Resistive,                     !- Defrost Strategy",
        "   TIMED,                         !- Defrost Control",
        "   0.166667,                      !- Defrost Time Period Fraction",
        "   1.0;                           !- Resistive Defrost Heater Capacity {W}",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    state->dataEnvrn->StdRhoAir = 1.0;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 0;
    state->dataSize->CurOASysNum = 0;
    state->dataLoopNodes->Node.allocate(10);

    PTUnitData thisUnit;
    thisUnit.AirInNode = 1;
    state->dataLoopNodes->Node(1).Temp = 15.0;
    state->dataLoopNodes->Node(1).HumRat = 0.08;
    thisUnit.AirOutNode = 4;
    state->dataLoopNodes->Node(4).Temp = 25.0;
    state->dataLoopNodes->Node(4).HumRat = 0.08;
    thisUnit.NodeNumOfControlledZone = 5;
    state->dataLoopNodes->Node(5).Temp = 21.0;
    state->dataLoopNodes->Node(5).HumRat = 0.08;
    thisUnit.ATMixerExists = false;
    thisUnit.MaxCoolCoilFluidFlow = 0.1;
    thisUnit.DesignMinOutletTemp = 15.0;
    thisUnit.MaxNoCoolHeatAirMassFlow = 0.1;
    thisUnit.MaxCoolAirMassFlow = 0.2;
    thisUnit.LowSpeedCoolFanRatio = 0.5;
    thisUnit.LowSpeedHeatFanRatio = 0.5;
    thisUnit.CoolCoilFluidInletNode = 0;
    thisUnit.CoolCoilFluidOutletNodeNum = 0;
    thisUnit.CoolCoilLoopNum = 0;
    thisUnit.CoolCoilLoopSide = 0;
    thisUnit.CoolCoilBranchNum = 0;
    thisUnit.CoolCoilCompNum = 0;
    thisUnit.CoolCoilInletNodeNum = 2;
    thisUnit.CoolCoilOutletNodeNum = 4;

    thisUnit.DesignMaxOutletTemp = 30.0;
    thisUnit.MaxHeatAirMassFlow = 0.2;
    thisUnit.HeatCoilFluidInletNode = 0;
    thisUnit.HeatCoilFluidOutletNodeNum = 0;
    thisUnit.HeatCoilLoopNum = 0;
    thisUnit.HeatCoilLoopSide = 0;
    thisUnit.HeatCoilBranchNum = 0;
    thisUnit.HeatCoilCompNum = 0;
    thisUnit.HeatCoilInletNodeNum = 4;
    thisUnit.HeatCoilOutletNodeNum = 3;

    thisUnit.ControlZoneNum = 1;

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneNode = 1;

    state->dataScheduleMgr->Schedule.allocate(1);
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1.0;

    state->dataPTHP->PTUnit.allocate(1);
    state->dataPTHP->PTUnit(1) = thisUnit;
    state->dataPTHP->PTUnit(1).simASHRAEModel = true;
    state->dataPTHP->PTUnit(1).MinOATCompressorCooling = -10.0;
    state->dataPTHP->PTUnit(1).UnitType_Num = PackagedTerminalHeatPump::iPTHPType::PTHPUnit;
    state->dataPTHP->PTUnit(1).FanName = "TEST FAN";
    state->dataPTHP->PTUnit(1).FanType = "Fan:OnOff";
    state->dataPTHP->PTUnit(1).DXCoolCoilName = "COOLINGCOIL";
    state->dataPTHP->PTUnit(1).DXHeatCoilName = "HEATINGCOIL";
    state->dataPTHP->PTUnit(1).DXCoolCoilIndexNum = 1;
    state->dataPTHP->PTUnit(1).DXHeatCoilIndexNum = 2;
    state->dataPTHP->PTUnit(1).SchedPtr = 1;
    state->dataPTHP->PTUnit(1).FanAvailSchedPtr = 1;
    state->dataPTHP->PTUnit(1).FanPlace = BlowThru;
    state->dataPTHP->PTUnit(1).OpMode = DataHVACGlobals::ContFanCycCoil; // ensure constant fan mode is used since Init is not called
    state->dataPTHP->CompOnMassFlow = thisUnit.MaxCoolAirMassFlow;
    state->dataPTHP->CompOffMassFlow = thisUnit.MaxNoCoolHeatAirMassFlow;

    state->dataBranchNodeConnections->NumCompSets = 2;
    state->dataBranchNodeConnections->CompSets.allocate(2);
    state->dataBranchNodeConnections->CompSets(1).CType = "Coil:Cooling:DX:SingleSpeed";
    state->dataBranchNodeConnections->CompSets(1).CName = "CoolingCoil";
    state->dataBranchNodeConnections->CompSets(1).ParentCType = "ZoneHVAC:PackagedTerminalHeatPump";
    state->dataBranchNodeConnections->CompSets(1).ParentCName = "AirSystem";
    state->dataBranchNodeConnections->CompSets(2).CType = "Coil:Heating:DX:SingleSpeed";
    state->dataBranchNodeConnections->CompSets(2).CName = "HeatingCoil";
    state->dataBranchNodeConnections->CompSets(2).ParentCType = "ZoneHVAC:PackagedTerminalHeatPump";
    state->dataBranchNodeConnections->CompSets(2).ParentCName = "AirSystem";

    state->dataEnvrn->OutDryBulbTemp = 5.0;
    OutputReportPredefined::SetPredefinedTables(*state);
    Psychrometrics::InitializePsychRoutines(*state);
    createCoilSelectionReportObj(*state);

    int UnitNum = 1;
    bool FirstHVACIteration = true;
    bool CoolingLoad = true;
    bool HeatingLoad = false;
    Real64 QZnReq = -200.0;
    Real64 OnOffAirFlowRatio = 1.0;
    bool HXUnitOn = false;
    int AirLoopNum = 0;
    Real64 PLR = 0.0;
    int CompressorOnFlag = 0;
    auto &SZVAVModel(state->dataPTHP->PTUnit(1));
    // first pass through will get objects and reset node data
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               UnitNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);

    // set unit inlet node conditions for cooling
    state->dataLoopNodes->Node(1).Temp = 24.0;
    state->dataLoopNodes->Node(1).HumRat = 0.011;
    state->dataLoopNodes->Node(1).Enthalpy = 52120.0;
    bool ErrorsFound = false;
    // set zone condition
    NodeInputManager::GetOnlySingleNode(*state,
                                        "ZoneNode",
                                        ErrorsFound,
                                        "PTUnit",
                                        "PTUnit",
                                        DataLoopNode::NodeFluidType::Air,
                                        DataLoopNode::NodeConnectionType::Inlet,
                                        NodeInputManager::compFluidStream::Primary,
                                        DataLoopNode::ObjectIsNotParent);

    state->dataLoopNodes->Node(5).Temp = 24.0;
    state->dataLoopNodes->Node(5).HumRat = 0.011;
    state->dataLoopNodes->Node(5).Enthalpy = 52120.0;

    // turn the availability schedule on
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1.0;
    state->dataPTHP->CoolingLoad = CoolingLoad;
    state->dataPTHP->HeatingLoad = HeatingLoad;
    state->dataGlobal->BeginEnvrnFlag = true;
    // set fan inlet max avail so fan doesn't shut down flow
    state->dataLoopNodes->Node(1).MassFlowRateMaxAvail = 0.2;
    state->dataEnvrn->StdRhoAir = 1.2; // fan used this to convert volume to mass flow rate
    state->dataEnvrn->OutBaroPress = 101325.0;
    // second pass through will run model

    // Region 1 of control, low air flow rate, modulate coil capacity
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               UnitNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);

    EXPECT_NEAR(state->dataLoopNodes->Node(1).MassFlowRate, thisUnit.MaxNoCoolHeatAirMassFlow, 0.00000001); // low speed air flow rate
    EXPECT_LT(state->dataLoopNodes->Node(4).Temp, state->dataLoopNodes->Node(1).Temp);                      // active cooling
    Real64 AirMassFlow = thisUnit.MaxNoCoolHeatAirMassFlow;
    Real64 MinHumRat = min(state->dataLoopNodes->Node(4).HumRat, state->dataLoopNodes->Node(1).HumRat);
    Real64 OutletTemp = state->dataLoopNodes->Node(4).Temp;
    Real64 InletTemp = state->dataLoopNodes->Node(1).Temp;
    Real64 LoadMet = AirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, -200.0, 0.0001);

    // Region 2 of control, modulate air flow rate, modulate coil capacity
    QZnReq = -1200.0;
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               UnitNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);

    EXPECT_GT(state->dataLoopNodes->Node(1).MassFlowRate, thisUnit.MaxNoCoolHeatAirMassFlow); // air flow higher than low speed
    EXPECT_LT(state->dataLoopNodes->Node(1).MassFlowRate, thisUnit.MaxCoolAirMassFlow);       // air flow lower than high speed
    EXPECT_LT(state->dataLoopNodes->Node(4).Temp, state->dataLoopNodes->Node(1).Temp);        // active cooling

    AirMassFlow = state->dataLoopNodes->Node(4).MassFlowRate;
    MinHumRat = min(state->dataLoopNodes->Node(4).HumRat, state->dataLoopNodes->Node(1).HumRat);
    OutletTemp = state->dataLoopNodes->Node(4).Temp;
    InletTemp = state->dataLoopNodes->Node(1).Temp;
    LoadMet = AirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, -1200.0, 0.0001);

    // Region 3 of control, high air flow rate, modulate coil capacity
    QZnReq = -2000.0;
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               UnitNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);

    EXPECT_NEAR(state->dataLoopNodes->Node(1).MassFlowRate, thisUnit.MaxCoolAirMassFlow, 0.00000001); // high speed air flow rate
    EXPECT_LT(state->dataLoopNodes->Node(4).Temp, state->dataLoopNodes->Node(1).Temp);                // active cooling

    MinHumRat = min(state->dataLoopNodes->Node(4).HumRat, state->dataLoopNodes->Node(1).HumRat);
    OutletTemp = state->dataLoopNodes->Node(4).Temp;
    InletTemp = state->dataLoopNodes->Node(1).Temp;
    LoadMet = thisUnit.MaxCoolAirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, -2000.0, 0.0001);

    CoolingLoad = false;
    HeatingLoad = true;
    state->dataPTHP->CoolingLoad = CoolingLoad;
    state->dataPTHP->HeatingLoad = HeatingLoad;

    // set unit inlet node conditions for heating
    state->dataLoopNodes->Node(1).Temp = 21.0;
    state->dataLoopNodes->Node(1).HumRat = 0.008;
    state->dataLoopNodes->Node(1).Enthalpy = 41431.0;
    state->dataLoopNodes->Node(5).Temp = 21.0;
    state->dataLoopNodes->Node(5).HumRat = 0.008;
    state->dataLoopNodes->Node(5).Enthalpy = 41431.0;

    // Region 1 of control, low air flow rate, modulate coil capacity
    QZnReq = 200.0;
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               UnitNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);

    EXPECT_NEAR(state->dataLoopNodes->Node(1).MassFlowRate, thisUnit.MaxNoCoolHeatAirMassFlow, 0.00000001); // high speed air flow rate
    EXPECT_GT(state->dataLoopNodes->Node(4).Temp, state->dataLoopNodes->Node(1).Temp);                      // active heating

    MinHumRat = min(state->dataLoopNodes->Node(4).HumRat, state->dataLoopNodes->Node(1).HumRat);
    OutletTemp = state->dataLoopNodes->Node(4).Temp;
    InletTemp = state->dataLoopNodes->Node(1).Temp;
    LoadMet =
        thisUnit.MaxNoCoolHeatAirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, 200.0, 0.0001);

    // Region 2 of control, modulate air flow rate, modulate coil capacity
    QZnReq = 1200.0;
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               UnitNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);

    EXPECT_GT(state->dataLoopNodes->Node(1).MassFlowRate, thisUnit.MaxNoCoolHeatAirMassFlow); // air flow higher than low speed
    EXPECT_LT(state->dataLoopNodes->Node(1).MassFlowRate, thisUnit.MaxHeatAirMassFlow);       // air flow lower than high speed
    EXPECT_GT(state->dataLoopNodes->Node(4).Temp, state->dataLoopNodes->Node(1).Temp);        // active heating

    AirMassFlow = state->dataLoopNodes->Node(4).MassFlowRate;
    MinHumRat = min(state->dataLoopNodes->Node(4).HumRat, state->dataLoopNodes->Node(1).HumRat);
    OutletTemp = state->dataLoopNodes->Node(4).Temp;
    InletTemp = state->dataLoopNodes->Node(1).Temp;
    LoadMet = AirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, 1200.0, 0.0001);

    // Region 3 of control, high air flow rate, modulate coil capacity
    QZnReq = 2000.0;
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               UnitNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);

    EXPECT_NEAR(state->dataLoopNodes->Node(1).MassFlowRate, thisUnit.MaxHeatAirMassFlow, 0.00000001); // high speed air flow rate
    EXPECT_GT(state->dataLoopNodes->Node(4).Temp, state->dataLoopNodes->Node(1).Temp);                // active heating

    MinHumRat = min(state->dataLoopNodes->Node(4).HumRat, state->dataLoopNodes->Node(1).HumRat);
    OutletTemp = state->dataLoopNodes->Node(4).Temp;
    InletTemp = state->dataLoopNodes->Node(1).Temp;
    LoadMet = thisUnit.MaxHeatAirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, 2000.0, 0.0001);
}

TEST_F(EnergyPlusFixture, SZVAV_FanCoilUnit_Testing)
{

    int FanCoilNum(1);
    int ZoneNum(1);
    int CompressorOnFlag(0);
    int AirLoopNum(0);
    bool FirstHVACIteration(false);
    bool ErrorsFound(false);
    bool CoolingLoad(false);
    bool HeatingLoad(false);
    bool HXUnitOn(false);
    Real64 QZnReq(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 QLatOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);
    Real64 OnOffAirFlowRatio(1.0);
    Real64 PLR(0.0);

    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.20;
    state->dataWaterCoils->GetWaterCoilsInputFlag = true;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    state->dataSize->CurZoneEqNum = 1;

    std::string const idf_objects = delimited_string({

        " Zone,",
        "  WEST ZONE,                 !- Name",
        "  0,                         !- Direction of Relative North { deg }",
        "  0,                         !- X Origin { m }",
        "  0,                         !- Y Origin { m }",
        "  0,                         !- Z Origin { m }",
        "  1,                         !- Type",
        "  1,                         !- Multiplier",
        "  autocalculate,             !- Ceiling Height { m }",
        "  autocalculate;             !- Volume { m3 }",

        " ZoneHVAC:EquipmentConnections,",
        "  WEST ZONE,                 !- Zone Name",
        "  ZoneEquipment,             !- Zone Conditioning Equipment List Name",
        "  FanCoilAirOutletNode,      !- Zone Air Inlet Node or NodeList Name",
        "  FanCoilAirInletNode,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone Air Node,             !- Zone Air Node Name",
        "  Zone Air Outlet Node;      !- Zone Return Air Node Name",

        " ZoneHVAC:EquipmentList,",
        "  ZoneEquipment,             !- Name",
        "  SequentialLoad,            !- Load Distribution Scheme",
        "  ZoneHVAC:FourPipeFanCoil,  !- Zone Equipment 1 Object Type",
        "  ZoneFanCoil,               !- Zone Equipment 1 Name",
        "  1,                         !- Zone Equipment 1 Cooling Sequence",
        "  1;                         !- Zone Equipment 1 Heating or No - Load Sequence",

        " ZoneHVAC:FourPipeFanCoil,",
        "  ZoneFanCoil,               !- Name",
        "  FanAndCoilAvailSched,      !- Availability Schedule Name",
        "  ASHRAE90VariableFan,       !- Capacity Control Method",
        "  0.5,                       !- Maximum Supply Air Flow Rate { m3 / s }",
        "  0.3,                       !- Low Speed Supply Air Flow Ratio",
        "  0.6,                       !- Medium Speed Supply Air Flow Ratio",
        "  0.0,                       !- Maximum Outdoor Air Flow Rate { m3 / s }",
        "  FanAndCoilAvailSched,      !- Outdoor Air Schedule Name",
        "  FanCoilAirInletNode,       !- Air Inlet Node Name",
        "  FanCoilAirOutletNode,      !- Air Outlet Node Name",
        "  OutdoorAir:Mixer,          !- Outdoor Air Mixer Object Type",
        "  FanCoilOAMixer,            !- Outdoor Air Mixer Name",
        "  Fan:OnOff,                 !- Supply Air Fan Object Type",
        "  FanCoilFan,                !- Supply Air Fan Name",
        "  Coil:Cooling:Water,        !- Cooling Coil Object Type",
        "  FanCoilCoolingCoil,        !- Cooling Coil Name",
        "  0.00014,                   !- Maximum Cold Water Flow Rate { m3 / s }",
        "  0.0,                       !- Minimum Cold Water Flow Rate { m3 / s }",
        "  0.001,                     !- Cooling Convergence Tolerance",
        "  Coil:Heating:Electric,     !- Heating Coil Object Type",
        "  FanCoilElecHeatingCoil,    !- Heating Coil Name",
        "  0.0,                       !- Maximum Hot Water Flow Rate { m3 / s }",
        "  0.0,                       !- Minimum Hot Water Flow Rate { m3 / s }",
        "  0.001,                     !- Heating Convergence Tolerance",
        "  ,                          !- Availability Manager List Name",
        "  ;                          !- Design Specification ZoneHVAC Sizing Object Name",

        " OutdoorAir:NodeList,",
        "  FanCoilOAInNode;           !- Node or NodeList Name 1",

        " OutdoorAir:Mixer,",
        "  FanCoilOAMixer,            !- Name",
        "  FanCoilOAMixerOutletNode,  !- Mixed Air Node Name",
        "  FanCoilOAInNode,           !- Outdoor Air Stream Node Name",
        "  FanCoilExhNode,            !- Relief Air Stream Node Name",
        "  FanCoilAirInletNode;       !- Return Air Stream Node Name",

        " Fan:OnOff,",
        "  FanCoilFan,                !- Name",
        "  FanAndCoilAvailSched,      !- Availability Schedule Name",
        "  0.5,                       !- Fan Total Efficiency",
        "  75,                        !- Design Pressure Rise {Pa}",
        "  0.5,                       !- Maximum Air Flow Rate {m3/s}",
        "  0.9,                       !- Motor Efficiency",
        "  1,                         !- Motor In Air Stream Fraction",
        "  FanCoilOAMixerOutletNode,  !- Air Inlet Node Name",
        "  FanCoilFanOutletNode;      !- Air Outlet Node Name",

        " Coil:Cooling:Water,",
        "  FanCoilCoolingCoil,        !- Name",
        "  FanAndCoilAvailSched,      !- Availability Schedule Namev",
        "  0.0002,                    !- Design Water Flow Rate { m3 / s }",
        "  0.5000,                    !- Design Air Flow Rate { m3 / s }",
        "  7.22,                      !- Design Inlet Water Temperature { Cv }",
        "  24.340,                    !- Design Inlet Air Temperature { C }",
        "  14.000,                    !- Design Outlet Air Temperature { C }",
        "  0.0095,                    !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "  0.0090,                    !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "  FanCoilChWInletNode,       !- Water Inlet Node Name",
        "  FanCoilChWOutletNode,      !- Water Outlet Node Name",
        "  FanCoilFanOutletNode,      !- Air Inlet Node Name",
        "  FanCoilCCOutletNode,       !- Air Outlet Node Name",
        "  SimpleAnalysis,            !- Type of Analysis",
        "  CrossFlow;                 !- Heat Exchanger Configuration",

        " Coil:Heating:Electric,",
        "  FanCoilElecHeatingCoil,    !- Name",
        "  FanAndCoilAvailSched,      !- Availability Schedule Name",
        "  1,                         !- Efficiency",
        "  10000.0,                    !- Nominal Capacity {W}",
        "  FanCoilCCOutletNode,       !- Air Inlet Node Name",
        "  FanCoilAirOutletNode;      !- Air Outlet Node Name",

        " Schedule:Constant,",
        "  FanAndCoilAvailSched,      !- Name",
        "  FRACTION,                  !- Schedule Type",
        "  1;                         !- TimeStep Value",

        " ScheduleTypeLimits,",
        "  Fraction,                  !- Name",
        "  0.0,                       !- Lower Limit Value",
        "  1.0,                       !- Upper Limit Value",
        "  CONTINUOUS;                !- Numeric Type",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    state->dataEnvrn->StdRhoAir = 1.0;

    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.20;
    state->dataWaterCoils->GetWaterCoilsInputFlag = true;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    state->dataSize->CurZoneEqNum = 1;
    InitializePsychRoutines(*state);
    GetZoneData(*state, ErrorsFound);
    EXPECT_EQ("WEST ZONE", state->dataHeatBal->Zone(1).Name);
    GetZoneEquipmentData(*state);
    ProcessScheduleInput(*state);
    state->dataScheduleMgr->ScheduleInputProcessed = true;
    GetFanCoilUnits(*state);
    auto &thisFanCoil(state->dataFanCoilUnits->FanCoil(1));
    EXPECT_EQ("ASHRAE90VARIABLEFAN", thisFanCoil.CapCtrlMeth);
    EXPECT_EQ("OUTDOORAIR:MIXER", thisFanCoil.OAMixType);
    EXPECT_EQ("FAN:ONOFF", thisFanCoil.FanType);
    EXPECT_EQ("COIL:COOLING:WATER", thisFanCoil.CCoilType);
    EXPECT_EQ("COIL:HEATING:ELECTRIC", thisFanCoil.HCoilType);
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;
    ColdWaterMassFlowRate = 1.0;
    thisFanCoil.OutAirMassFlow = 0.0;
    thisFanCoil.MaxAirMassFlow = MaxAirMassFlow;
    // outside air mixer
    auto &MixerOA(state->dataMixedAir->OAMixer(1));
    state->dataLoopNodes->Node(MixerOA.RetNode).MassFlowRate = AirMassFlow;
    state->dataLoopNodes->Node(MixerOA.RetNode).MassFlowRateMax = MaxAirMassFlow;
    state->dataLoopNodes->Node(MixerOA.RetNode).Temp = 20.0;
    state->dataLoopNodes->Node(MixerOA.RetNode).Enthalpy = 36000;
    state->dataLoopNodes->Node(MixerOA.RetNode).HumRat =
        PsyWFnTdbH(*state, state->dataLoopNodes->Node(MixerOA.RetNode).Temp, state->dataLoopNodes->Node(MixerOA.RetNode).Enthalpy);
    state->dataLoopNodes->Node(MixerOA.InletNode).Temp = 10.0;
    state->dataLoopNodes->Node(MixerOA.InletNode).Enthalpy = 18000;
    state->dataLoopNodes->Node(MixerOA.InletNode).HumRat =
        PsyWFnTdbH(*state, state->dataLoopNodes->Node(MixerOA.InletNode).Temp, state->dataLoopNodes->Node(MixerOA.InletNode).Enthalpy);
    // chilled water coil
    auto &CWCoil(state->dataWaterCoils->WaterCoil(1));
    CWCoil.UACoilTotal = 470.0;
    CWCoil.UACoilExternal = 611.0;
    CWCoil.UACoilInternal = 2010.0;
    CWCoil.TotCoilOutsideSurfArea = 50.0;
    state->dataLoopNodes->Node(CWCoil.AirInletNodeNum).MassFlowRate = AirMassFlow;
    state->dataLoopNodes->Node(CWCoil.AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    state->dataLoopNodes->Node(CWCoil.AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;
    CWCoil.InletWaterMassFlowRate = ColdWaterMassFlowRate;
    CWCoil.MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    state->dataLoopNodes->Node(CWCoil.WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    state->dataLoopNodes->Node(CWCoil.WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    state->dataLoopNodes->Node(CWCoil.WaterInletNodeNum).Temp = 6.0;
    CWCoil.WaterLoopNum = 1;
    CWCoil.WaterLoopSide = 1;
    CWCoil.WaterLoopBranchNum = 1;
    CWCoil.WaterLoopCompNum = 1;
    // electric heating coil
    auto &eHCoil(state->dataHeatingCoils->HeatingCoil(1));
    state->dataLoopNodes->Node(eHCoil.AirInletNodeNum).MassFlowRate = AirMassFlow;
    state->dataLoopNodes->Node(eHCoil.AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {
        auto &loop(state->dataPlnt->PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(state->dataPlnt->PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    // chilled water plant loop
    auto &CWLoop(state->dataPlnt->PlantLoop(1));
    CWLoop.Name = "ChilledWaterLoop";
    CWLoop.FluidName = "ChilledWater";
    CWLoop.FluidIndex = 1;
    CWLoop.FluidName = "WATER";
    CWLoop.LoopSide(1).Branch(1).Comp(1).Name = CWCoil.Name;
    CWLoop.LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterCooling;
    CWLoop.LoopSide(1).Branch(1).Comp(1).NodeNumIn = CWCoil.WaterInletNodeNum;
    CWLoop.LoopSide(1).Branch(1).Comp(1).NodeNumOut = CWCoil.WaterOutletNodeNum;

    state->dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state->dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;
    state->dataGlobal->DoingSizing = true;
    state->dataFans->LocalTurnFansOff = false;
    state->dataFans->LocalTurnFansOn = true;

    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    auto &zSysEDemand(state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1));

    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 21;
    state->dataGlobal->HourOfDay = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    UpdateScheduleValues(*state);
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->ZoneSizingRunDone = true;
    thisFanCoil.DesignHeatingCapacity = 10000.0;
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataGlobal->SysSizingCalc = true;
    FirstHVACIteration = true;
    zSysEDemand.RemainingOutputReqToCoolSP = 0.0;
    zSysEDemand.RemainingOutputReqToHeatSP = 0.0;
    zSysEDemand.RemainingOutputRequired = 0.0;
    QZnReq = 0.0;
    QUnitOut = 0.0;
    QLatOut = 0.0;
    // init
    InitFanCoilUnits(*state, FanCoilNum, ZoneNum, ZoneNum);
    Sim4PipeFanCoil(*state, FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    // heating mode tests
    CoolingLoad = false;
    HeatingLoad = true;
    OnOffAirFlowRatio = 1.0;
    HXUnitOn = false;
    AirLoopNum = 0;
    CompressorOnFlag = 0;
    FirstHVACIteration = true;
    auto &SZVAVModel(state->dataFanCoilUnits->FanCoil(FanCoilNum));

    // test 1: 1000 W heating load
    zSysEDemand.RemainingOutputReqToCoolSP = 1000.0;
    zSysEDemand.RemainingOutputReqToHeatSP = 1000.0;
    zSysEDemand.RemainingOutputRequired = 1000.0;
    QZnReq = 1000.0;
    QUnitOut = 0.0;
    QLatOut = 0.0;
    PLR = 0.0;
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               FanCoilNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);
    Calc4PipeFanCoil(*state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR);
    EXPECT_NEAR(PLR, 0.092, 0.001);
    EXPECT_NEAR(QUnitOut, 1000.0, 1.0);

    // test 2: 5000 W heating load
    zSysEDemand.RemainingOutputReqToCoolSP = 5000.0;
    zSysEDemand.RemainingOutputReqToHeatSP = 5000.0;
    zSysEDemand.RemainingOutputRequired = 5000.0;
    QZnReq = 5000.0;
    QUnitOut = 0.0;
    QLatOut = 0.0;
    PLR = 0.0;
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               FanCoilNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);
    Calc4PipeFanCoil(*state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR);
    EXPECT_NEAR(PLR, 0.492, 0.001);
    EXPECT_NEAR(QUnitOut, 5000.0, 1.0);

    // test 3: 9000 W heating load
    zSysEDemand.RemainingOutputReqToCoolSP = 9000.0;
    zSysEDemand.RemainingOutputReqToHeatSP = 9000.0;
    zSysEDemand.RemainingOutputRequired = 9000.0;
    QZnReq = 9000.0;
    QUnitOut = 0.0;
    QLatOut = 0.0;
    PLR = 0.0;
    SZVAVModel::calcSZVAVModel(*state,
                               SZVAVModel,
                               FanCoilNum,
                               FirstHVACIteration,
                               CoolingLoad,
                               HeatingLoad,
                               QZnReq,
                               OnOffAirFlowRatio,
                               HXUnitOn,
                               AirLoopNum,
                               PLR,
                               CompressorOnFlag);
    Calc4PipeFanCoil(*state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR);
    EXPECT_NEAR(PLR, 0.892, 0.001);
    EXPECT_NEAR(QUnitOut, 9000.0, 1.0);
}
