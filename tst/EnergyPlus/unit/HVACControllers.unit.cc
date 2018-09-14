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

// EnergyPlus::HVACControllers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataAirLoop.hh>
#include <DataConvergParams.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/WaterCoils.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::HVACControllers;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SetPointManager;
using namespace EnergyPlus::WaterCoils;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HVACControllers_ResetHumidityRatioCtrlVarType)
{
    std::string const idf_objects = delimited_string({
        " Version,8.3;",
        " Coil:Cooling:Water,",
        "	Chilled Water Coil,	!- Name",
        "	AvailSched,			!- Availability Schedule Name",
        "	autosize,			!- Design Water Flow Rate { m3 / s }",
        "	autosize,			!- Design Air Flow Rate { m3 / s }",
        "	autosize,			!- Design Inlet Water Temperature { C }",
        "	autosize,			!- Design Inlet Air Temperature { C }",
        "	autosize,			!- Design Outlet Air Temperature { C }",
        "	autosize,			!- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	autosize,			!- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Water Inlet Node,	!- Water Inlet Node Name",
        "	Water Outlet Node,  !- Water Outlet Node Name",
        "	Air Inlet Node,		!- Air Inlet Node Name",
        "	Air Outlet Node,	!- Air Outlet Node Name",
        "	SimpleAnalysis,		!- Type of Analysis",
        "	CrossFlow;          !- Heat Exchanger Configuration",
        " Controller:WaterCoil,",
        "	CW Coil Controller, !- Name",
        "	HumidityRatio,		!- Control Variable",
        "	Reverse,			!- Action",
        "	FLOW,				!- Actuator Variable",
        "	Air Outlet Node,	!- Sensor Node Name",
        "	Water Inlet Node,	!- Actuator Node Name",
        "	autosize,			!- Controller Convergence Tolerance { deltaC }",
        "	autosize,			!- Maximum Actuated Flow { m3 / s }",
        "	0.0;				!- Minimum Actuated Flow { m3 / s }",
        " SetpointManager:Scheduled,",
        "	HumRatSPManager,	!- Name",
        "	HumidityRatio,		!- Control Variable",
        "	HumRatioSched,		!- Schedule Name",
        "	Air Outlet Node;	!- Setpoint Node or NodeList Name",
        " Schedule:Compact,",
        "   HumRatioSched,		!- Name",
        "	Any Number,			!- Schedule Type Limits Name",
        "	Through: 12/31,		!- Field 1",
        "	For: AllDays,		!- Field 2",
        "	Until: 24:00, 0.015; !- Field 3",
        " Schedule:Compact,",
        "   AvailSched,			!- Name",
        "	Fraction,			!- Schedule Type Limits Name",
        "	Through: 12/31,		!- Field 1",
        "	For: AllDays,		!- Field 2",
        "	Until: 24:00, 1.0;  !- Field 3",
        " AirLoopHVAC:ControllerList,",
        "	CW Coil Controller, !- Name",
        "	Controller:WaterCoil, !- Controller 1 Object Type",
        "	CW Coil Controller; !- Controller 1 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetSetPointManagerInputs();
    // check specified control variable type is "HumidityRatio"
    ASSERT_EQ(iCtrlVarType_HumRat, AllSetPtMgr(1).CtrlTypeMode);

    GetControllerInput();
    // check control variable type in AllSetPtMgr is reset to "MaximumHumidityRatio"
    ASSERT_EQ(iCtrlVarType_MaxHumRat, AllSetPtMgr(1).CtrlTypeMode);

    // ControllerProps always expects the control variable type to be "HumididtyRatio"
    ControllerProps(1).HumRatCntrlType = GetHumidityRatioVariableType(ControllerProps(1).SensedNode);
    ASSERT_EQ(iCtrlVarType_HumRat, ControllerProps(1).HumRatCntrlType);
}

TEST_F(EnergyPlusFixture, HVACControllers_TestTempAndHumidityRatioCtrlVarType)
{
    std::string const idf_objects = delimited_string({

        " Coil:Cooling:Water,",
        "	Chilled Water Coil,	!- Name",
        "	AvailSched,			!- Availability Schedule Name",
        "	0.01,				!- Design Water Flow Rate { m3 / s }",
        "	1.0,				!- Design Air Flow Rate { m3 / s }",
        "	7.2,				!- Design Inlet Water Temperature { C }",
        "	32.0,				!- Design Inlet Air Temperature { C }",
        "	12.0,				!- Design Outlet Air Temperature { C }",
        "	0.01,				!- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	0.07,				!- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Water Inlet Node,	!- Water Inlet Node Name",
        "	Water Outlet Node,  !- Water Outlet Node Name",
        "	Air Inlet Node,		!- Air Inlet Node Name",
        "	Air Outlet Node,	!- Air Outlet Node Name",
        "	SimpleAnalysis,		!- Type of Analysis",
        "	CrossFlow;          !- Heat Exchanger Configuration",
        " Controller:WaterCoil,",
        "	CW Coil Controller, !- Name",
        "	TemperatureAndHumidityRatio,		!- Control Variable",
        "	Reverse,			!- Action",
        "	FLOW,				!- Actuator Variable",
        "	Air Outlet Node,	!- Sensor Node Name",
        "	Water Inlet Node,	!- Actuator Node Name",
        "	0.001,				!- Controller Convergence Tolerance { deltaC }",
        "	0.01,				!- Maximum Actuated Flow { m3 / s }",
        "	0.0;				!- Minimum Actuated Flow { m3 / s }",
        " SetpointManager:Scheduled,",
        "	HumRatSPManager,	!- Name",
        "	MaximumHumidityRatio,  !- Control Variable",
        "	HumRatioSched,		!- Schedule Name",
        "	Air Outlet Node;	!- Setpoint Node or NodeList Name",
        " Schedule:Compact,",
        "   HumRatioSched,		!- Name",
        "	Any Number,			!- Schedule Type Limits Name",
        "	Through: 12/31,		!- Field 1",
        "	For: AllDays,		!- Field 2",
        "	Until: 24:00, 0.015; !- Field 3",
        " Schedule:Compact,",
        "   AvailSched,			!- Name",
        "	Fraction,			!- Schedule Type Limits Name",
        "	Through: 12/31,		!- Field 1",
        "	For: AllDays,		!- Field 2",
        "	Until: 24:00, 1.0;  !- Field 3",
        " AirLoopHVAC:ControllerList,",
        "	CW Coil Controller, !- Name",
        "	Controller:WaterCoil, !- Controller 1 Object Type",
        "	CW Coil Controller; !- Controller 1 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetSetPointManagerInputs();
    // check specified control variable type is "HumidityRatio"
    ASSERT_EQ(iCtrlVarType_MaxHumRat, AllSetPtMgr(1).CtrlTypeMode);

    GetControllerInput();
    // check control variable type in AllSetPtMgr is reset to "MaximumHumidityRatio"
    ASSERT_EQ(iCtrlVarType_MaxHumRat, AllSetPtMgr(1).CtrlTypeMode);

    // ControllerProps expects the control variable type to be "MaximumHumididtyRatio"
    ControllerProps(1).HumRatCntrlType = GetHumidityRatioVariableType(ControllerProps(1).SensedNode);
    ASSERT_EQ(iCtrlVarType_MaxHumRat, ControllerProps(1).HumRatCntrlType);

    // test index for air loop controllers
    // before controllers are simulated, AirLoopControllerIndex = 0
    ASSERT_EQ(0, ControllerProps(1).AirLoopControllerIndex);

    OutputReportPredefined::SetPredefinedTables();
    SimAirServingZones::GetAirLoopInputFlag = false;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataAirLoop::PriAirSysAvailMgr.allocate(1);
    DataAirLoop::AirLoopControlInfo.allocate(1);
    DataAirLoop::AirToZoneNodeInfo.allocate(1);
    DataAirLoop::AirToZoneNodeInfo(1).NumSupplyNodes = 1;
    DataAirLoop::AirToZoneNodeInfo(1).AirLoopSupplyNodeNum.allocate(1);
    DataAirLoop::AirToZoneNodeInfo(1).AirLoopSupplyNodeNum(1) = 1;
    DataAirLoop::AirToZoneNodeInfo(1).ZoneEquipSupplyNodeNum.allocate(1);
    DataAirLoop::AirToZoneNodeInfo(1).ZoneEquipSupplyNodeNum(1) = 4;
    DataConvergParams::AirLoopConvergence.allocate(1);
    DataConvergParams::AirLoopConvergence(1).HVACMassFlowNotConverged.allocate(2);
    DataConvergParams::AirLoopConvergence(1).HVACHumRatNotConverged.allocate(2);
    DataConvergParams::AirLoopConvergence(1).HVACTempNotConverged.allocate(2);
    DataConvergParams::AirLoopConvergence(1).HVACEnergyNotConverged.allocate(2);
    DataConvergParams::AirLoopConvergence(1).HVACEnthalpyNotConverged.allocate(2);
    DataConvergParams::AirLoopConvergence(1).HVACPressureNotConverged.allocate(2);
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).NumBranches = 1;
    DataAirSystems::PrimaryAirSystem(1).NumControllers = 1;
    DataAirSystems::PrimaryAirSystem(1).ControllerIndex.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).ControllerIndex(1) = 0;
    DataAirSystems::PrimaryAirSystem(1).ControllerName.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).ControllerName(1) = "CW COIL CONTROLLER";
    DataAirSystems::PrimaryAirSystem(1).ControlConverged.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).NodeNumIn = 4;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).NodeNumOut = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).TotalNodes = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).TotalComponents = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).NodeNum.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).NodeNum(1) = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).Name = "CHILLED WATER COIL";
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).CompType_Num = 5; // WaterCoil_Cooling
    DataPlant::PlantLoop.allocate(1);
    DataPlant::TotNumLoops = 1;
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(1).TotalBranches = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = 39;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 2;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 3;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = "CHILLED WATER COIL";
    bool SimZoneEquipment(false);
    SimAirServingZones::SimAirLoops(true, SimZoneEquipment);

    // after controllers are simulated, AirLoopControllerIndex = index to this controller on this air loop (e.g., n of num contollers on air loop)
    ASSERT_EQ(1, DataAirSystems::PrimaryAirSystem(1).NumControllers);
    ASSERT_EQ(1, DataAirSystems::PrimaryAirSystem(1).ControllerIndex(1));
    ASSERT_EQ(1, ControllerProps(1).AirLoopControllerIndex);
}

TEST_F(EnergyPlusFixture, HVACControllers_SchSetPointMgrsOrderTest)
{
    std::string const idf_objects = delimited_string({
        "  Version,8.6;",

        "  Coil:Cooling:Water,",
        "    Main Cooling Coil 1,     !- Name",
        "    CoolingCoilAvailSched,   !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    CCoil Water Inlet Node,  !- Water Inlet Node Name",
        "    CCoil Water Outlet Node, !- Water Outlet Node Name",
        "    Mixed Air Node 1,        !- Air Inlet Node Name",
        "    CCoil Air Outlet Node,   !- Air Outlet Node Name",
        "    SimpleAnalysis,          !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "  Schedule:Compact,",
        "   CoolingCoilAvailSched,	  !- Name",
        "	Fraction,			      !- Schedule Type Limits Name",
        "	Through: 12/31,		      !- Field 1",
        "	For: AllDays,		      !- Field 2",
        "	Until: 24:00, 1.0;        !- Field 3",

        "  Controller:WaterCoil,",
        "    Cooling Coil Contoller,  !- Name",
        "    HumidityRatio,           !- Control Variable",
        "    Reverse,                 !- Action",
        "    FLOW,                    !- Actuator Variable",
        "    CCoil Air Outlet Node,   !- Sensor Node Name",
        "    CCoil Water Inlet Node,  !- Actuator Node Name",
        "    autosize,                !- Controller Convergence Tolerance {deltaC}",
        "    autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0.0;                     !- Minimum Actuated Flow {m3/s}",

        "  SetpointManager:Scheduled,",
        "    CCoil Temp Setpoint Mgr, !- Name",
        "    Temperature,             !- Control Variable",
        "    Always 16,               !- Schedule Name",
        "    CCoil Air Outlet Node;   !- Setpoint Node or NodeList Name",

        "  Schedule:Compact,",
        "    Always 16,               !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,16;         !- Field 3",

        "  SetpointManager:Scheduled,",
        "    CCoil Hum Setpoint Mgr,  !- Name",
        "    MaximumHumidityRatio,    !- Control Variable",
        "    HumSetPt,                !- Schedule Name",
        "    CCoil Air Outlet Node;   !- Setpoint Node or NodeList Name",

        "  Schedule:Compact,",
        "    HumSetPt,                !- Name",
        "    AnyNumber,               !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00, 0.009;     !- Field 3",

        "  AirLoopHVAC:ControllerList,",
        "	CW Coil Controller,       !- Name",
        "	Controller:WaterCoil,     !- Controller 1 Object Type",
        "	Cooling Coil Contoller;   !- Controller 1 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetSetPointManagerInputs();
    // There are two setpoint managers and are schedule type
    ASSERT_EQ(2, NumSchSetPtMgrs); // 2 schedule set point managers
    ASSERT_EQ(2, NumAllSetPtMgrs); // 2 all set point managers
    // check specified control variable types
    ASSERT_EQ(iTemperature, AllSetPtMgr(1).CtrlTypeMode);           // is "Temperature"
    ASSERT_EQ(iCtrlVarType_MaxHumRat, AllSetPtMgr(2).CtrlTypeMode); // is "MaximumHumidityRatio"

    GetControllerInput();
    // check ControllerProps control variable is set to "MaximumHumidityRatio"
    ControllerProps(1).HumRatCntrlType = GetHumidityRatioVariableType(ControllerProps(1).SensedNode);
    ASSERT_EQ(iCtrlVarType_MaxHumRat, ControllerProps(1).HumRatCntrlType); // MaximumHumidityRatio
}

TEST_F(EnergyPlusFixture, HVACControllers_WaterCoilOnPrimaryLoopCheckTest)
{
    std::string const idf_objects = delimited_string({

        " Coil:Cooling:Water,",
        "	Chilled Water Coil,	!- Name",
        "	,        			!- Availability Schedule Name",
        "	0.01,				!- Design Water Flow Rate { m3 / s }",
        "	1.0,				!- Design Air Flow Rate { m3 / s }",
        "	7.2,				!- Design Inlet Water Temperature { C }",
        "	32.0,				!- Design Inlet Air Temperature { C }",
        "	12.0,				!- Design Outlet Air Temperature { C }",
        "	0.01,				!- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	0.07,				!- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Water Inlet Node,	!- Water Inlet Node Name",
        "	Water Outlet Node,  !- Water Outlet Node Name",
        "	Air Inlet Node,		!- Air Inlet Node Name",
        "	Air Outlet Node,	!- Air Outlet Node Name",
        "	SimpleAnalysis,		!- Type of Analysis",
        "	CrossFlow;          !- Heat Exchanger Configuration",

        " Controller:WaterCoil,",
        "	CW Coil Controller, !- Name",
        "	TemperatureAndHumidityRatio,!- Control Variable",
        "	Reverse,			!- Action",
        "	FLOW,				!- Actuator Variable",
        "	Air Outlet Node,	!- Sensor Node Name",
        "	Water Inlet Node,	!- Actuator Node Name",
        "	0.001,				!- Controller Convergence Tolerance { deltaC }",
        "	0.01,				!- Maximum Actuated Flow { m3 / s }",
        "	0.0;				!- Minimum Actuated Flow { m3 / s }",

        " AirLoopHVAC:ControllerList,",
        "	CW Coil Controller, !- Name",
        "	Controller:WaterCoil,   !- Controller 1 Object Type",
        "	CW Coil Controller; !- Controller 1 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetControllerInput();

    ASSERT_EQ(WaterCoil(1).Name, "CHILLED WATER COIL");
    ASSERT_EQ(WaterCoil(1).WaterCoilType_Num, WaterCoils::WaterCoil_Cooling);

    OutputReportPredefined::SetPredefinedTables();
    SimAirServingZones::GetAirLoopInputFlag = false;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).NumBranches = 1;
    DataAirSystems::PrimaryAirSystem(1).NumControllers = 1;
    DataAirSystems::PrimaryAirSystem(1).ControllerIndex.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).ControllerIndex(1) = 0;
    DataAirSystems::PrimaryAirSystem(1).ControllerName.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).ControllerName(1) = "CW COIL CONTROLLER";
    DataAirSystems::PrimaryAirSystem(1).ControlConverged.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).NodeNumIn = 4;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).NodeNumOut = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).TotalNodes = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).TotalComponents = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).NodeNum.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).NodeNum(1) = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).Name = WaterCoil(1).Name;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).CompType_Num = SimAirServingZones::WaterCoil_Cooling;

    bool WaterCoilOnAirLoop = true;
    std::string CompType = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater); //"Coil:Cooling:Water";
    std::string CompName = "CHILLED WATER COIL";
    int CoilTypeNum = SimAirServingZones::WaterCoil_Cooling;

    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilOnPrimaryAirLoopBranch(CoilTypeNum, CompName);
    EXPECT_TRUE(WaterCoilOnAirLoop);

    WaterCoilOnAirLoop = true;
    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilOnOASystem(CoilTypeNum, CompName);
    EXPECT_FALSE(WaterCoilOnAirLoop);

    WaterCoilOnAirLoop = true;
    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilSystemOnAirLoopOrOASystem(CoilTypeNum, CompName);
    EXPECT_FALSE(WaterCoilOnAirLoop);

    WaterCoilOnAirLoop = true;
    SimAirServingZones::CheckWaterCoilIsOnAirLoop(CoilTypeNum, CompType, CompName, WaterCoilOnAirLoop);
    EXPECT_TRUE(WaterCoilOnAirLoop);

    // now test a different water coil type
    CoilTypeNum = WaterCoils::WaterCoil_DetFlatFinCooling;
    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilOnPrimaryAirLoopBranch(CoilTypeNum, CompName);
    EXPECT_FALSE(WaterCoilOnAirLoop);
}

TEST_F(EnergyPlusFixture, HVACControllers_WaterCoilOnOutsideAirSystemCheckTest)
{
    std::string const idf_objects = delimited_string({
        " Version, 8.9;",

        "  AirLoopHVAC:ControllerList,",
        "    OA Sys 1 Controllers,    !- Name",
        "    Controller:WaterCoil,    !- Controller 1 Object Type",
        "    Preheat Coil Controller; !- Controller 1 Name",

        "  Coil:Heating:Water,",
        "    OA Preheat HW Coil,      !- Name",
        "    ,                        !- Availability Schedule Name",
        "    autosize,                !- U-Factor Times Area Value {W/K}",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    HWCoil Water InletNode,  !- Zone1UnitHeatHWInletNode,!- Water Inlet Node Name",
        "    HWCoil Water OutletNode, !- Water Outlet Node Name",
        "    Outside Air Inlet Node,  !- Air Inlet Node Name",
        "    HW Coil Air OutletNode,  !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                !- Rated Capacity {W}",
        "    82.2,                    !- Rated Inlet Water Temperature {C}",
        "    16.6,                    !- Rated Inlet Air Temperature {C}",
        "    71.1,                    !- Rated Outlet Water Temperature {C}",
        "    32.2,                    !- Rated Outlet Air Temperature {C}",
        "    ;                        !- Rated Ratio for Air and Water Convection",

        "  Controller:WaterCoil,",
        "    Preheat Coil Controller, !- Name",
        "    Temperature,             !- Control Variable",
        "    Normal,                  !- Action",
        "    Flow,                    !- Actuator Variable",
        "    HW Coil Air OutletNode,  !- Sensor Node Name",
        "    HWCoil Water InletNode,  !- Actuator Node Name",
        "    Autosize,                !- Controller Convergence Tolerance {deltaC}",
        "    Autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0;                       !- Minimum Actuated Flow {m3/s}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetControllerInput();

    ASSERT_EQ(WaterCoil(1).Name, "OA PREHEAT HW COIL");
    ASSERT_EQ(WaterCoil(1).WaterCoilType_Num, WaterCoils::WaterCoil_SimpleHeating);

    OutputReportPredefined::SetPredefinedTables();
    SimAirServingZones::GetAirLoopInputFlag = false;

    DataAirLoop::NumOASystems = 1;
    DataAirLoop::OutsideAirSys.allocate(1);
    DataAirLoop::OutsideAirSys(1).Name = "AIRLOOP OASYSTEM";
    DataAirLoop::OutsideAirSys(1).NumControllers = 1;
    DataAirLoop::OutsideAirSys(1).ControllerName.allocate(1);
    DataAirLoop::OutsideAirSys(1).ControllerName(1) = "OA CONTROLLER 1";
    DataAirLoop::OutsideAirSys(1).NumComponents = 2;
    DataAirLoop::OutsideAirSys(1).ComponentType.allocate(2);
    DataAirLoop::OutsideAirSys(1).ComponentType(1) = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater);
    DataAirLoop::OutsideAirSys(1).ComponentType(2) = "OutdoorAir:Mixer";
    DataAirLoop::OutsideAirSys(1).ComponentName.allocate(2);
    DataAirLoop::OutsideAirSys(1).ComponentName(1) = WaterCoil(1).Name;
    DataAirLoop::OutsideAirSys(1).ComponentName(2) = "OAMixer";
    DataAirLoop::OutsideAirSys(1).ComponentType_Num.allocate(2);
    DataAirLoop::OutsideAirSys(1).ComponentType_Num(1) = SimAirServingZones::WaterCoil_SimpleHeat;
    DataAirLoop::OutsideAirSys(1).ComponentType_Num(2) = SimAirServingZones::OAMixer_Num;

    OAMixer.allocate(1);
    OAMixer(1).Name = "OAMixer";
    OAMixer(1).InletNode = 2;

    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Name = "PrimaryAirLoop";
    DataAirSystems::PrimaryAirSystem(1).NumBranches = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).TotalComponents = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).Name = DataAirLoop::OutsideAirSys(1).Name;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";

    bool WaterCoilOnAirLoop = true;
    std::string CompType = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater);
    std::string CompName = WaterCoil(1).Name;
    int CoilTypeNum = SimAirServingZones::WaterCoil_SimpleHeat;

    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilOnPrimaryAirLoopBranch(CoilTypeNum, CompName);
    EXPECT_FALSE(WaterCoilOnAirLoop);

    WaterCoilOnAirLoop = false;
    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilOnOASystem(CoilTypeNum, CompName);
    EXPECT_TRUE(WaterCoilOnAirLoop);

    WaterCoilOnAirLoop = false;
    SimAirServingZones::CheckWaterCoilIsOnAirLoop(CoilTypeNum, CompType, CompName, WaterCoilOnAirLoop);
    EXPECT_TRUE(WaterCoilOnAirLoop);

    // test a different water coil type
    CoilTypeNum = SimAirServingZones::WaterCoil_DetailedCool;
    WaterCoilOnAirLoop = true;
    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilOnOASystem(CoilTypeNum, CompName);
    EXPECT_FALSE(WaterCoilOnAirLoop);
}
TEST_F(EnergyPlusFixture, HVACControllers_CoilSystemCoolingWaterOnOutsideAirSystemCheckTest)
{
    std::string const idf_objects = delimited_string({
        " Version, 8.9;",

        "  AirLoopHVAC:ControllerList,",
        "    OA System Controllers,   !- Name",
        "    Controller:WaterCoil,    !- Controller 1 Object Type",
        "    Detailed WaterCoil Cntrl;!- Controller 1 Name",

        "  Controller:WaterCoil,",
        "    Detailed WaterCoil Cntrl,!- Name",
        "    Temperature,             !- Control Variable",
        "    Reverse,                 !- Action",
        "    FLOW,                    !- Actuator Variable",
        "    Main Cooling Coil 1 Outlet Node,  !- Sensor Node Name",
        "    Main Cooling Coil 1 Water Inlet Node,  !- Actuator Node Name",
        "    0.002,                   !- Controller Convergence Tolerance {deltaC}",
        "    autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0.0;                     !- Minimum Actuated Flow {m3/s}",

        "  Coil:Cooling:Water:DetailedGeometry,",
        "    Detailed Pre Cooling Coil, !- Name",
        "    ,                        !- Availability Schedule Name",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    autosize,                !- Tube Outside Surface Area {m2}",
        "    autosize,                !- Total Tube Inside Area {m2}",
        "    autosize,                !- Fin Surface Area {m2}",
        "    autosize,                !- Minimum Airflow Area {m2}",
        "    autosize,                !- Coil Depth {m}",
        "    autosize,                !- Fin Diameter {m}",
        "    ,                        !- Fin Thickness {m}",
        "    ,                        !- Tube Inside Diameter {m}",
        "    ,                        !- Tube Outside Diameter {m}",
        "    ,                        !- Tube Thermal Conductivity {W/m-K}",
        "    ,                        !- Fin Thermal Conductivity {W/m-K}",
        "    ,                        !- Fin Spacing {m}",
        "    ,                        !- Tube Depth Spacing {m}",
        "    ,                        !- Number of Tube Rows",
        "    autosize,                !- Number of Tubes per Row",
        "    Main Cooling Coil 1 Water Inlet Node,  !- Water Inlet Node Name",
        "    Main Cooling Coil 1 Water Outlet Node,  !- Water Outlet Node Name",
        "    Main Cooling Coil 1 Inlet Node,  !- Air Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node;  !- Air Outlet Node Name",

        "  CoilSystem:Cooling:Water:HeatExchangerAssisted,",
        "    HXAssisting Cooling Coil,  !- Name",
        "    HeatExchanger:AirToAir:FlatPlate,  !- Heat Exchanger Object Type",
        "    HXAssisting Cooling Coil,  !- Heat Exchanger Name",
        "    Coil:Cooling:Water:DetailedGeometry,  !- Cooling Coil Object Type",
        "    Detailed Pre Cooling Coil; !- Cooling Coil Name",

        "  HeatExchanger:AirToAir:FlatPlate,",
        "    HXAssisting Cooling Coil,!- Name",
        "    ,                        !- Availability Schedule Name",
        "    CounterFlow,             !- Flow Arrangement Type",
        "    Yes,                     !- Economizer Lockout",
        "    1.0,                     !- Ratio of Supply to Secondary hA Values",
        "    1.32,                    !- Nominal Supply Air Flow Rate {m3/s}",
        "    24.0,                    !- Nominal Supply Air Inlet Temperature {C}",
        "    21.0,                    !- Nominal Supply Air Outlet Temperature {C}",
        "    1.32,                    !- Nominal Secondary Air Flow Rate {m3/s}",
        "    12.0,                    !- Nominal Secondary Air Inlet Temperature {C}",
        "    100.0,                   !- Nominal Electric Power {W}",
        "    Mixed Air Node 1,        !- Supply Air Inlet Node Name",
        "    Main Cooling Coil 1 Inlet Node,  !- Supply Air Outlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Secondary Air Inlet Node Name",
        "    Main Heating Coil 1 Inlet Node;  !- Secondary Air Outlet Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetControllerInput();

    ASSERT_EQ(WaterCoil(1).Name, "DETAILED PRE COOLING COIL");
    ASSERT_EQ(WaterCoil(1).WaterCoilType_Num, WaterCoils::WaterCoil_DetFlatFinCooling);

    OutputReportPredefined::SetPredefinedTables();
    SimAirServingZones::GetAirLoopInputFlag = false;

    DataAirLoop::NumOASystems = 1;
    DataAirLoop::OutsideAirSys.allocate(1);
    DataAirLoop::OutsideAirSys(1).Name = "AIRLOOP OASYSTEM";
    DataAirLoop::OutsideAirSys(1).NumControllers = 1;
    DataAirLoop::OutsideAirSys(1).ControllerName.allocate(1);
    DataAirLoop::OutsideAirSys(1).ControllerName(1) = "OA CONTROLLER 1";
    DataAirLoop::OutsideAirSys(1).NumComponents = 2;
    DataAirLoop::OutsideAirSys(1).ComponentType.allocate(2);
    DataAirLoop::OutsideAirSys(1).ComponentType(1) = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilWater_CoolingHXAssisted);
    DataAirLoop::OutsideAirSys(1).ComponentType(2) = "OutdoorAir:Mixer";
    DataAirLoop::OutsideAirSys(1).ComponentName.allocate(2);
    DataAirLoop::OutsideAirSys(1).ComponentName(1) = "HXAssisting Cooling Coil";
    DataAirLoop::OutsideAirSys(1).ComponentName(2) = "OAMixer";
    DataAirLoop::OutsideAirSys(1).ComponentType_Num.allocate(2);
    DataAirLoop::OutsideAirSys(1).ComponentType_Num(1) = SimAirServingZones::WaterCoil_CoolingHXAsst;
    DataAirLoop::OutsideAirSys(1).ComponentType_Num(2) = SimAirServingZones::OAMixer_Num;

    OAMixer.allocate(1);
    OAMixer(1).Name = "OAMixer";
    OAMixer(1).InletNode = 2;

    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Name = "PrimaryAirLoop";
    DataAirSystems::PrimaryAirSystem(1).NumBranches = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).TotalComponents = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).Name = DataAirLoop::OutsideAirSys(1).Name;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";

    bool WaterCoilOnAirLoop = true;
    std::string CompType = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_CoolingWaterDetailed);
    std::string CompName = WaterCoil(1).Name;
    int CoilTypeNum = SimAirServingZones::WaterCoil_DetailedCool;

    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilOnPrimaryAirLoopBranch(CoilTypeNum, CompName);
    EXPECT_FALSE(WaterCoilOnAirLoop);

    WaterCoilOnAirLoop = true;
    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilOnOASystem(CoilTypeNum, CompName);
    EXPECT_FALSE(WaterCoilOnAirLoop);

    WaterCoilOnAirLoop = false;
    WaterCoilOnAirLoop = SimAirServingZones::CheckWaterCoilSystemOnAirLoopOrOASystem(CoilTypeNum, CompName);
    EXPECT_TRUE(WaterCoilOnAirLoop);

    WaterCoilOnAirLoop = false;
    SimAirServingZones::CheckWaterCoilIsOnAirLoop(CoilTypeNum, CompType, CompName, WaterCoilOnAirLoop);
    EXPECT_TRUE(WaterCoilOnAirLoop);
}
} // namespace EnergyPlus
