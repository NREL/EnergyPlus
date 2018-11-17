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

// EnergyPlus::SZVAV Model Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PackagedTerminalHeatPump.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/SZVAVModel.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace CurveManager;
using namespace DataBranchNodeConnections;
using namespace DataEnvironment;
using namespace DataHeatBalance;
using namespace DataGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DXCoils;
using namespace EnergyPlus::FanCoilUnits;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::PackagedTerminalHeatPump;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SZVAVModel;

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

    CurZoneEqNum = 0;
    CurSysNum = 0;
    CurOASysNum = 0;
    DataLoopNode::Node.allocate(10);

    PTUnitData thisUnit;
    thisUnit.AirInNode = 1;
    DataLoopNode::Node(1).Temp = 15.0;
    DataLoopNode::Node(1).HumRat = 0.08;
    thisUnit.AirOutNode = 4;
    DataLoopNode::Node(4).Temp = 25.0;
    DataLoopNode::Node(4).HumRat = 0.08;
    thisUnit.NodeNumOfControlledZone = 5;
    DataLoopNode::Node(5).Temp = 21.0;
    DataLoopNode::Node(5).HumRat = 0.08;
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

    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = 1;

    ScheduleManager::Schedule.allocate(1);
    Schedule(1).CurrentValue = 1.0;

    PTUnit.allocate(1);
    PTUnit(1) = thisUnit;
    PTUnit(1).simASHRAEModel = true;
    PTUnit(1).MinOATCompressorCooling = -10.0;
    PTUnit(1).UnitType_Num = PackagedTerminalHeatPump::PTHPUnit;
    PTUnit(1).FanName = "TEST FAN";
    PTUnit(1).FanType = "Fan:OnOff";
    PTUnit(1).DXCoolCoilName = "COOLINGCOIL";
    PTUnit(1).DXHeatCoilName = "HEATINGCOIL";
    PTUnit(1).DXCoolCoilIndexNum = 1;
    PTUnit(1).DXHeatCoilIndexNum = 2;
    PTUnit(1).SchedPtr = 1;
    PTUnit(1).FanAvailSchedPtr = 1;
    PTUnit(1).FanPlace = BlowThru;
    PTUnit(1).OpMode = DataHVACGlobals::ContFanCycCoil; // ensure constant fan mode is used since Init is not called
    PackagedTerminalHeatPump::CompOnMassFlow = thisUnit.MaxCoolAirMassFlow;
    PackagedTerminalHeatPump::CompOffMassFlow = thisUnit.MaxNoCoolHeatAirMassFlow;

    DataBranchNodeConnections::NumCompSets = 2;
    CompSets.allocate(2);
    CompSets(1).CType = "Coil:Cooling:DX:SingleSpeed";
    CompSets(1).CName = "CoolingCoil";
    CompSets(1).ParentCType = "ZoneHVAC:PackagedTerminalHeatPump";
    CompSets(1).ParentCName = "AirSystem";
    CompSets(2).CType = "Coil:Heating:DX:SingleSpeed";
    CompSets(2).CName = "HeatingCoil";
    CompSets(2).ParentCType = "ZoneHVAC:PackagedTerminalHeatPump";
    CompSets(2).ParentCName = "AirSystem";

    DataEnvironment::OutDryBulbTemp = 5.0;
    OutputReportPredefined::SetPredefinedTables();
    Psychrometrics::InitializePsychRoutines();
    createCoilSelectionReportObj();

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
    auto &SZVAVModel(PTUnit(1));
    // first pass through will get objects and reset node data
    SZVAVModel::calcSZVAVModel(
        SZVAVModel, UnitNum, FirstHVACIteration, CoolingLoad, HeatingLoad, QZnReq, OnOffAirFlowRatio, HXUnitOn, AirLoopNum, PLR, CompressorOnFlag);

    // set unit inlet node conditions for cooling
    DataLoopNode::Node(1).Temp = 24.0;
    DataLoopNode::Node(1).HumRat = 0.011;
    DataLoopNode::Node(1).Enthalpy = 52120.0;
    bool ErrorsFound = false;
    // set zone condition
    NodeInputManager::GetOnlySingleNode("ZoneNode",
                                        ErrorsFound,
                                        "PTUnit",
                                        "PTUnit",
                                        DataLoopNode::NodeType_Air,
                                        DataLoopNode::NodeConnectionType_Inlet,
                                        1,
                                        DataLoopNode::ObjectIsNotParent);

    DataLoopNode::Node(5).Temp = 24.0;
    DataLoopNode::Node(5).HumRat = 0.011;
    DataLoopNode::Node(5).Enthalpy = 52120.0;

    // turn the availability schedule on
    Schedule(1).CurrentValue = 1.0;
    PackagedTerminalHeatPump::CoolingLoad = CoolingLoad;
    PackagedTerminalHeatPump::HeatingLoad = HeatingLoad;
    DataGlobals::BeginEnvrnFlag = true;
    // set fan inlet max avail so fan doesn't shut down flow
    DataLoopNode::Node(1).MassFlowRateMaxAvail = 0.2;
    DataEnvironment::StdRhoAir = 1.2; // fan used this to convert volume to mass flow rate
    DataEnvironment::OutBaroPress = 101325.0;
    // second pass through will run model

    // Region 1 of control, low air flow rate, modulate coil capacity
    SZVAVModel::calcSZVAVModel(
        SZVAVModel, UnitNum, FirstHVACIteration, CoolingLoad, HeatingLoad, QZnReq, OnOffAirFlowRatio, HXUnitOn, AirLoopNum, PLR, CompressorOnFlag);

    EXPECT_NEAR(DataLoopNode::Node(1).MassFlowRate, thisUnit.MaxNoCoolHeatAirMassFlow, 0.00000001); // low speed air flow rate
    EXPECT_LT(DataLoopNode::Node(4).Temp, DataLoopNode::Node(1).Temp);                              // active cooling
    Real64 AirMassFlow = thisUnit.MaxNoCoolHeatAirMassFlow;
    Real64 MinHumRat = min(DataLoopNode::Node(4).HumRat, DataLoopNode::Node(1).HumRat);
    Real64 OutletTemp = DataLoopNode::Node(4).Temp;
    Real64 InletTemp = DataLoopNode::Node(1).Temp;
    Real64 LoadMet = AirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, -200.0, 0.0001);

    // Region 2 of control, modulate air flow rate, modulate coil capacity
    QZnReq = -1200.0;
    SZVAVModel::calcSZVAVModel(
        SZVAVModel, UnitNum, FirstHVACIteration, CoolingLoad, HeatingLoad, QZnReq, OnOffAirFlowRatio, HXUnitOn, AirLoopNum, PLR, CompressorOnFlag);

    EXPECT_GT(DataLoopNode::Node(1).MassFlowRate, thisUnit.MaxNoCoolHeatAirMassFlow); // air flow higher than low speed
    EXPECT_LT(DataLoopNode::Node(1).MassFlowRate, thisUnit.MaxCoolAirMassFlow);       // air flow lower than high speed
    EXPECT_LT(DataLoopNode::Node(4).Temp, DataLoopNode::Node(1).Temp);                // active cooling

    AirMassFlow = DataLoopNode::Node(4).MassFlowRate;
    MinHumRat = min(DataLoopNode::Node(4).HumRat, DataLoopNode::Node(1).HumRat);
    OutletTemp = DataLoopNode::Node(4).Temp;
    InletTemp = DataLoopNode::Node(1).Temp;
    LoadMet = AirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, -1200.0, 0.0001);

    // Region 3 of control, high air flow rate, modulate coil capacity
    QZnReq = -2000.0;
    SZVAVModel::calcSZVAVModel(
        SZVAVModel, UnitNum, FirstHVACIteration, CoolingLoad, HeatingLoad, QZnReq, OnOffAirFlowRatio, HXUnitOn, AirLoopNum, PLR, CompressorOnFlag);

    EXPECT_NEAR(DataLoopNode::Node(1).MassFlowRate, thisUnit.MaxCoolAirMassFlow, 0.00000001); // high speed air flow rate
    EXPECT_LT(DataLoopNode::Node(4).Temp, DataLoopNode::Node(1).Temp);                        // active cooling

    MinHumRat = min(DataLoopNode::Node(4).HumRat, DataLoopNode::Node(1).HumRat);
    OutletTemp = DataLoopNode::Node(4).Temp;
    InletTemp = DataLoopNode::Node(1).Temp;
    LoadMet = thisUnit.MaxCoolAirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, -2000.0, 0.0001);

    CoolingLoad = false;
    HeatingLoad = true;
    PackagedTerminalHeatPump::CoolingLoad = CoolingLoad;
    PackagedTerminalHeatPump::HeatingLoad = HeatingLoad;

    // set unit inlet node conditions for heating
    DataLoopNode::Node(1).Temp = 21.0;
    DataLoopNode::Node(1).HumRat = 0.008;
    DataLoopNode::Node(1).Enthalpy = 41431.0;
    DataLoopNode::Node(5).Temp = 21.0;
    DataLoopNode::Node(5).HumRat = 0.008;
    DataLoopNode::Node(5).Enthalpy = 41431.0;

    // Region 1 of control, low air flow rate, modulate coil capacity
    QZnReq = 200.0;
    SZVAVModel::calcSZVAVModel(
        SZVAVModel, UnitNum, FirstHVACIteration, CoolingLoad, HeatingLoad, QZnReq, OnOffAirFlowRatio, HXUnitOn, AirLoopNum, PLR, CompressorOnFlag);

    EXPECT_NEAR(DataLoopNode::Node(1).MassFlowRate, thisUnit.MaxNoCoolHeatAirMassFlow, 0.00000001); // high speed air flow rate
    EXPECT_GT(DataLoopNode::Node(4).Temp, DataLoopNode::Node(1).Temp);                              // active heating

    MinHumRat = min(DataLoopNode::Node(4).HumRat, DataLoopNode::Node(1).HumRat);
    OutletTemp = DataLoopNode::Node(4).Temp;
    InletTemp = DataLoopNode::Node(1).Temp;
    LoadMet =
        thisUnit.MaxNoCoolHeatAirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, 200.0, 0.0001);

    // Region 2 of control, modulate air flow rate, modulate coil capacity
    QZnReq = 1200.0;
    SZVAVModel::calcSZVAVModel(
        SZVAVModel, UnitNum, FirstHVACIteration, CoolingLoad, HeatingLoad, QZnReq, OnOffAirFlowRatio, HXUnitOn, AirLoopNum, PLR, CompressorOnFlag);

    EXPECT_GT(DataLoopNode::Node(1).MassFlowRate, thisUnit.MaxNoCoolHeatAirMassFlow); // air flow higher than low speed
    EXPECT_LT(DataLoopNode::Node(1).MassFlowRate, thisUnit.MaxHeatAirMassFlow);       // air flow lower than high speed
    EXPECT_GT(DataLoopNode::Node(4).Temp, DataLoopNode::Node(1).Temp);                // active heating

    AirMassFlow = DataLoopNode::Node(4).MassFlowRate;
    MinHumRat = min(DataLoopNode::Node(4).HumRat, DataLoopNode::Node(1).HumRat);
    OutletTemp = DataLoopNode::Node(4).Temp;
    InletTemp = DataLoopNode::Node(1).Temp;
    LoadMet = AirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, 1200.0, 0.0001);

    // Region 3 of control, high air flow rate, modulate coil capacity
    QZnReq = 2000.0;
    SZVAVModel::calcSZVAVModel(
        SZVAVModel, UnitNum, FirstHVACIteration, CoolingLoad, HeatingLoad, QZnReq, OnOffAirFlowRatio, HXUnitOn, AirLoopNum, PLR, CompressorOnFlag);

    EXPECT_NEAR(DataLoopNode::Node(1).MassFlowRate, thisUnit.MaxHeatAirMassFlow, 0.00000001); // high speed air flow rate
    EXPECT_GT(DataLoopNode::Node(4).Temp, DataLoopNode::Node(1).Temp);                        // active heating

    MinHumRat = min(DataLoopNode::Node(4).HumRat, DataLoopNode::Node(1).HumRat);
    OutletTemp = DataLoopNode::Node(4).Temp;
    InletTemp = DataLoopNode::Node(1).Temp;
    LoadMet = thisUnit.MaxHeatAirMassFlow * (Psychrometrics::PsyHFnTdbW(OutletTemp, MinHumRat) - Psychrometrics::PsyHFnTdbW(InletTemp, MinHumRat));
    EXPECT_NEAR(LoadMet, QZnReq, 0.0001);
    EXPECT_NEAR(LoadMet, 2000.0, 0.0001);
}
