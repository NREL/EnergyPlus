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

// EnergyPlus::FanCoilUnits Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <General.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace ObjexxFCL;
using namespace EnergyPlus;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::FanCoilUnits;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::GlobalNames;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::OutputProcessor;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::WaterCoils;

using DataHeatBalFanSys::TempControlType;
using DataZoneEnergyDemands::CurDeadBandOrSetback;
using DataZoneEnergyDemands::ZoneSysEnergyDemand;
using General::TrimSigDigits;
using MixedAir::OAMixer;

namespace EnergyPlus {
TEST_F(EnergyPlusFixture, MultiStage4PipeFanCoilHeatingTest)
{

    int FanCoilNum(1);
    int ZoneNum(1);
    bool FirstHVACIteration(false);
    bool ErrorsFound(false);
    Real64 PartLoadRatio(1.0);
    Real64 SpeedRatio(0.0);
    Real64 QZnReq(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    WaterCoils::GetWaterCoilsInputFlag = true;
    NumCoils = 0;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.3;",
        "	Zone,",
        "	EAST ZONE, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	autocalculate, !- Ceiling Height { m }",
        "	autocalculate; !- Volume { m3 }",
        "	ZoneHVAC:EquipmentConnections,",
        "	EAST ZONE, !- Zone Name",
        "	Zone1Equipment, !- Zone Conditioning Equipment List Name",
        "	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
        "	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
        "	Zone 1 Node, !- Zone Air Node Name",
        "	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	Zone1Equipment, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
        "	Zone1FanCoil, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "   NodeList,",
        "	Zone1Inlets, !- Name",
        "	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
        "	NodeList,",
        "	Zone1Exhausts, !- Name",
        "	Zone1FanCoilAirInletNode; !- Node 1 Name",
        "	OutdoorAir:NodeList,",
        "	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
        "	OutdoorAir:Mixer,",
        "	Zone1FanCoilOAMixer, !- Name",
        "	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
        "	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
        "	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
        "	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
        "	Schedule:Constant,",
        "	FanAndCoilAvailSched, !- Name",
        "	FRACTION, !- Schedule Type",
        "	1;        !- TimeStep Value",
        "	ScheduleTypeLimits,",
        "	Fraction, !- Name",
        "	0.0, !- Lower Limit Value",
        "	1.0, !- Upper Limit Value",
        "	CONTINUOUS;              !- Numeric Type",
        "   Fan:OnOff,",
        "	Zone1FanCoilFan, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	0.5, !- Fan Total Efficiency",
        "	75.0, !- Pressure Rise { Pa }",
        "	0.6, !- Maximum Flow Rate { m3 / s }",
        "	0.9, !- Motor Efficiency",
        "	1.0, !- Motor In Airstream Fraction",
        "	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
        "	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
        "	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
        "	Coil:Cooling:Water,",
        "	Zone1FanCoilCoolingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Namev",
        "	0.0002, !- Design Water Flow Rate { m3 / s }",
        "	0.5000, !- Design Air Flow Rate { m3 / s }",
        "	7.22,   !- Design Inlet Water Temperature { Cv }",
        "	24.340, !- Design Inlet Air Temperature { C }",
        "	14.000, !- Design Outlet Air Temperature { C }",
        "	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
        "	SimpleAnalysis, !- Type of Analysis",
        "	CrossFlow;               !- Heat Exchanger Configuration",
        "	Coil:Heating:Water,",
        "   Zone1FanCoilHeatingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	150.0,   !- U - Factor Times Area Value { W / K }",
        "	0.00014, !- Maximum Water Flow Rate { m3 / s }",
        "	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "	autosize, !- Rated Capacity { W }",
        "	82.2, !- Rated Inlet Water Temperature { C }",
        "	16.6, !- Rated Inlet Air Temperature { C }",
        "	71.1, !- Rated Outlet Water Temperature { C }",
        "	32.2, !- Rated Outlet Air Temperature { C }",
        "	;     !- Rated Ratio for Air and Water Convection",
        "	ZoneHVAC:FourPipeFanCoil,",
        "	Zone1FanCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	MultiSpeedFan, !- Capacity Control Method",
        "	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
        "	0.3, !- Low Speed Supply Air Flow Ratio",
        "	0.6, !- Medium Speed Supply Air Flow Ratio",
        "	0.0, !- Maximum Outdoor Air Flow Rate { m3 / s }",
        "	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
        "	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
        "	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
        "	Fan:OnOff, !- Supply Air Fan Object Type",
        "	Zone1FanCoilFan, !- Supply Air Fan Name",
        "	Coil:Cooling:Water, !- Cooling Coil Object Type",
        "	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
        "	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
        "	0.001, !- Cooling Convergence Tolerance",
        "	Coil:Heating:Water, !- Heating Coil Object Type",
        "	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
        "	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
        "	0.001; !- Heating Convergence Tolerance",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetZoneData(ErrorsFound);
    EXPECT_EQ("EAST ZONE", Zone(1).Name);

    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetFanInput();
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, Fan(1).FanType_Num);

    GetFanCoilUnits();
    EXPECT_EQ("MULTISPEEDFAN", FanCoil(1).CapCtrlMeth);
    EXPECT_EQ("OUTDOORAIR:MIXER", FanCoil(1).OAMixType);
    EXPECT_EQ("FAN:ONOFF", FanCoil(1).FanType);
    EXPECT_EQ("COIL:COOLING:WATER", FanCoil(1).CCoilType);
    EXPECT_EQ("COIL:HEATING:WATER", FanCoil(1).HCoilType);

    TotNumLoops = 2;
    PlantLoop.allocate(TotNumLoops);

    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;
    // heating load only
    ColdWaterMassFlowRate = 0.0;
    HotWaterMassFlowRate = 1.0;

    Node(OAMixer(1).RetNode).MassFlowRate = AirMassFlow;
    Node(OAMixer(1).RetNode).MassFlowRateMax = MaxAirMassFlow;

    Node(OAMixer(1).RetNode).Temp = 22.0;
    Node(OAMixer(1).RetNode).Enthalpy = 36000;
    Node(OAMixer(1).RetNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).RetNode).Temp, Node(OAMixer(1).RetNode).Enthalpy);

    Node(OAMixer(1).InletNode).Temp = 10.0;
    Node(OAMixer(1).InletNode).Enthalpy = 18000;
    Node(OAMixer(1).InletNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).InletNode).Temp, Node(OAMixer(1).InletNode).Enthalpy);

    Node(FanCoil(1).AirInNode).MassFlowRate = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMin = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMinAvail = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMax = MaxAirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMaxAvail = MaxAirMassFlow;

    FanCoil(1).OutAirMassFlow = 0.0;
    FanCoil(1).MaxAirMassFlow = MaxAirMassFlow;
    Node(FanCoil(1).OutsideAirNode).MassFlowRateMax = 0.0;

    Fan(1).InletAirMassFlowRate = AirMassFlow;
    Fan(1).MaxAirMassFlowRate = MaxAirMassFlow;

    Node(Fan(1).InletNodeNum).MassFlowRate = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).UACoilTotal = 470.0;
    WaterCoil(2).UACoilExternal = 611.0;
    WaterCoil(2).UACoilInternal = 2010.0;
    WaterCoil(2).TotCoilOutsideSurfArea = 50.0;

    Node(WaterCoil(2).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    WaterCoil(2).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).Temp = 6.0;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    Node(WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    Node(WaterCoil(1).WaterInletNodeNum).Temp = 60.0;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    WaterCoil(1).InletWaterMassFlowRate = HotWaterMassFlowRate;
    WaterCoil(1).MaxWaterMassFlowRate = HotWaterMassFlowRate;

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    WaterCoil(2).WaterLoopNum = 1;
    WaterCoil(2).WaterLoopSide = 1;
    WaterCoil(2).WaterLoopBranchNum = 1;
    WaterCoil(2).WaterLoopCompNum = 1;

    WaterCoil(1).WaterLoopNum = 2;
    WaterCoil(1).WaterLoopSide = 1;
    WaterCoil(1).WaterLoopBranchNum = 1;
    WaterCoil(1).WaterLoopCompNum = 1;

    PlantLoop(2).Name = "ChilledWaterLoop";
    PlantLoop(2).FluidName = "ChilledWater";
    PlantLoop(2).FluidIndex = 1;
    PlantLoop(2).FluidName = "WATER";
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(2).Name;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_Cooling;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(2).WaterInletNodeNum;

    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "HotWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(1).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_SimpleHeating;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(1).WaterInletNodeNum;

    CoolingLoad = false;
    HeatingLoad = true;
    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = 0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 4000.0;
    FanCoil(1).SpeedFanSel = 2;
    QUnitOut = 0.0;
    QZnReq = 4000.0;

    MyUAAndFlowCalcFlag.allocate(2);
    MyUAAndFlowCalcFlag(1) = true;
    MyUAAndFlowCalcFlag(2) = true;
    DataGlobals::DoingSizing = true;

    LocalTurnFansOff = false;
    LocalTurnFansOn = true;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(Month, DayOfMonth, 1);
    UpdateScheduleValues();

    CalcMultiStage4PipeFanCoil(FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut);

    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    DataGlobals::DoingSizing = false;

    PlantLoop.deallocate();
    ZoneSysEnergyDemand.deallocate();
    FanCoil.deallocate();
    Node.deallocate();
    WaterCoil.deallocate();
    ZoneEquipConfig.deallocate();
    Zone.deallocate();
    CoilNames.clear();
}
TEST_F(EnergyPlusFixture, MultiStage4PipeFanCoilCoolingTest)
{

    int FanCoilNum(1);
    int ZoneNum(1);
    bool FirstHVACIteration(false);
    bool ErrorsFound(false);
    Real64 PartLoadRatio(1.0);
    Real64 SpeedRatio(0.0);
    Real64 QZnReq(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    WaterCoils::GetWaterCoilsInputFlag = true;
    NumCoils = 0;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.3;",
        "	Zone,",
        "	EAST ZONE, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	autocalculate, !- Ceiling Height { m }",
        "	autocalculate; !- Volume { m3 }",
        "	ZoneHVAC:EquipmentConnections,",
        "	EAST ZONE, !- Zone Name",
        "	Zone1Equipment, !- Zone Conditioning Equipment List Name",
        "	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
        "	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
        "	Zone 1 Node, !- Zone Air Node Name",
        "	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	Zone1Equipment, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
        "	Zone1FanCoil, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "   NodeList,",
        "	Zone1Inlets, !- Name",
        "	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
        "	NodeList,",
        "	Zone1Exhausts, !- Name",
        "	Zone1FanCoilAirInletNode; !- Node 1 Name",
        "	OutdoorAir:NodeList,",
        "	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
        "	OutdoorAir:Mixer,",
        "	Zone1FanCoilOAMixer, !- Name",
        "	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
        "	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
        "	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
        "	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
        "	Schedule:Compact,",
        "	FanAndCoilAvailSched, !- Name",
        "	Fraction, !- Schedule Type Limits Name",
        "	Through: 12/31, !- Field 1",
        "	For: AllDays, !- Field 2",
        "	Until: 24:00, 1.0;        !- Field 3",
        "	ScheduleTypeLimits,",
        "	Fraction, !- Name",
        "	0.0, !- Lower Limit Value",
        "	1.0, !- Upper Limit Value",
        "	CONTINUOUS;              !- Numeric Type",
        "   Fan:OnOff,",
        "	Zone1FanCoilFan, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	0.5, !- Fan Total Efficiency",
        "	75.0, !- Pressure Rise { Pa }",
        "	0.6, !- Maximum Flow Rate { m3 / s }",
        "	0.9, !- Motor Efficiency",
        "	1.0, !- Motor In Airstream Fraction",
        "	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
        "	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
        "	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
        "	Coil:Cooling:Water,",
        "	Zone1FanCoilCoolingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Namev",
        "	0.0002, !- Design Water Flow Rate { m3 / s }",
        "	0.5000, !- Design Air Flow Rate { m3 / s }",
        "	7.22,   !- Design Inlet Water Temperature { Cv }",
        "	24.340, !- Design Inlet Air Temperature { C }",
        "	14.000, !- Design Outlet Air Temperature { C }",
        "	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
        "	SimpleAnalysis, !- Type of Analysis",
        "	CrossFlow;               !- Heat Exchanger Configuration",
        "	Coil:Heating:Water,",
        "   Zone1FanCoilHeatingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	150.0,   !- U - Factor Times Area Value { W / K }",
        "	0.00014, !- Maximum Water Flow Rate { m3 / s }",
        "	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "	autosize, !- Rated Capacity { W }",
        "	82.2, !- Rated Inlet Water Temperature { C }",
        "	16.6, !- Rated Inlet Air Temperature { C }",
        "	71.1, !- Rated Outlet Water Temperature { C }",
        "	32.2, !- Rated Outlet Air Temperature { C }",
        "	;     !- Rated Ratio for Air and Water Convection",
        "	ZoneHVAC:FourPipeFanCoil,",
        "	Zone1FanCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	MultiSpeedFan, !- Capacity Control Method",
        "	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
        "	0.3, !- Low Speed Supply Air Flow Ratio",
        "	0.6, !- Medium Speed Supply Air Flow Ratio",
        "	0.1, !- Maximum Outdoor Air Flow Rate { m3 / s }",
        "	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
        "	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
        "	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
        "	Fan:OnOff, !- Supply Air Fan Object Type",
        "	Zone1FanCoilFan, !- Supply Air Fan Name",
        "	Coil:Cooling:Water, !- Cooling Coil Object Type",
        "	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
        "	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
        "	0.001, !- Cooling Convergence Tolerance",
        "	Coil:Heating:Water, !- Heating Coil Object Type",
        "	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
        "	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
        "	0.001; !- Heating Convergence Tolerance",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetZoneData(ErrorsFound);
    EXPECT_EQ("EAST ZONE", Zone(1).Name);

    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetFanInput();
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, Fan(1).FanType_Num);

    GetFanCoilUnits();
    EXPECT_EQ("MULTISPEEDFAN", FanCoil(1).CapCtrlMeth);
    EXPECT_EQ("OUTDOORAIR:MIXER", FanCoil(1).OAMixType);
    EXPECT_EQ("FAN:ONOFF", FanCoil(1).FanType);
    EXPECT_EQ("COIL:COOLING:WATER", FanCoil(1).CCoilType);
    EXPECT_EQ("COIL:HEATING:WATER", FanCoil(1).HCoilType);

    TotNumLoops = 2;
    PlantLoop.allocate(TotNumLoops);

    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;

    // cooling load only
    HotWaterMassFlowRate = 0.0;
    ColdWaterMassFlowRate = 1.0;

    Node(OAMixer(1).RetNode).MassFlowRate = AirMassFlow;
    Node(OAMixer(1).RetNode).MassFlowRateMax = MaxAirMassFlow;

    Node(OAMixer(1).RetNode).Temp = 24.0;
    Node(OAMixer(1).RetNode).Enthalpy = 36000;
    Node(OAMixer(1).RetNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).RetNode).Temp, Node(OAMixer(1).RetNode).Enthalpy);

    Node(OAMixer(1).InletNode).Temp = 30.0;
    Node(OAMixer(1).InletNode).Enthalpy = 53000;
    Node(OAMixer(1).InletNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).InletNode).Temp, Node(OAMixer(1).InletNode).Enthalpy);

    Node(FanCoil(1).AirInNode).MassFlowRate = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMin = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMinAvail = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMax = MaxAirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMaxAvail = MaxAirMassFlow;

    FanCoil(1).OutAirMassFlow = 0.0;
    FanCoil(1).MaxAirMassFlow = MaxAirMassFlow;
    Node(FanCoil(1).OutsideAirNode).MassFlowRateMax = 0.0;

    Fan(1).InletAirMassFlowRate = AirMassFlow;
    Fan(1).MaxAirMassFlowRate = MaxAirMassFlow;

    Node(Fan(1).InletNodeNum).MassFlowRate = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).UACoilTotal = 470.0;
    WaterCoil(2).UACoilExternal = 611.0;
    WaterCoil(2).UACoilInternal = 2010.0;
    WaterCoil(2).TotCoilOutsideSurfArea = 50.0;

    Node(WaterCoil(2).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    WaterCoil(2).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).Temp = 6.0;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    Node(WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    Node(WaterCoil(1).WaterInletNodeNum).Temp = 60.0;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    WaterCoil(1).InletWaterMassFlowRate = HotWaterMassFlowRate;
    WaterCoil(1).MaxWaterMassFlowRate = HotWaterMassFlowRate;

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    WaterCoil(2).WaterLoopNum = 1;
    WaterCoil(2).WaterLoopSide = 1;
    WaterCoil(2).WaterLoopBranchNum = 1;
    WaterCoil(2).WaterLoopCompNum = 1;

    WaterCoil(1).WaterLoopNum = 2;
    WaterCoil(1).WaterLoopSide = 1;
    WaterCoil(1).WaterLoopBranchNum = 1;
    WaterCoil(1).WaterLoopCompNum = 1;

    PlantLoop(2).Name = "ChilledWaterLoop";
    PlantLoop(2).FluidName = "ChilledWater";
    PlantLoop(2).FluidIndex = 1;
    PlantLoop(2).FluidName = "WATER";
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(2).Name;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_Cooling;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(2).WaterInletNodeNum;

    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "HotWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(1).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_SimpleHeating;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(1).WaterInletNodeNum;

    HeatingLoad = false;
    CoolingLoad = true;
    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -4000.00;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 0.0;
    FanCoil(1).SpeedFanSel = 2;
    QUnitOut = 0.0;
    QZnReq = -4000.0;

    MyUAAndFlowCalcFlag.allocate(2);
    MyUAAndFlowCalcFlag(1) = true;
    MyUAAndFlowCalcFlag(2) = true;
    DataGlobals::DoingSizing = true;

    LocalTurnFansOff = false;
    LocalTurnFansOn = true;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(Month, DayOfMonth, 1);
    UpdateScheduleValues();

    CalcMultiStage4PipeFanCoil(FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut);

    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    DataGlobals::DoingSizing = false;
    PlantLoop.deallocate();
    ZoneSysEnergyDemand.deallocate();
    FanCoil.deallocate();
    Node.deallocate();
    WaterCoil.deallocate();
    ZoneEquipConfig.deallocate();
    Zone.deallocate();
    CoilNames.clear();
}
TEST_F(EnergyPlusFixture, ConstantFanVariableFlowFanCoilHeatingTest)
{

    int FanCoilNum(1);
    int ZoneNum(1);
    int ControlledZoneNum(1);
    bool FirstHVACIteration(true);
    bool ErrorsFound(false);
    Real64 QZnReq(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);
    Real64 LatOutputProvided(0.0);

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    WaterCoils::GetWaterCoilsInputFlag = true;
    NumCoils = 0;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.3;",
        "	Zone,",
        "	EAST ZONE, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	autocalculate, !- Ceiling Height { m }",
        "	autocalculate; !- Volume { m3 }",
        "	ZoneHVAC:EquipmentConnections,",
        "	EAST ZONE, !- Zone Name",
        "	Zone1Equipment, !- Zone Conditioning Equipment List Name",
        "	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
        "	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
        "	Zone 1 Node, !- Zone Air Node Name",
        "	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	Zone1Equipment, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
        "	Zone1FanCoil, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "   NodeList,",
        "	Zone1Inlets, !- Name",
        "	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
        "	NodeList,",
        "	Zone1Exhausts, !- Name",
        "	Zone1FanCoilAirInletNode; !- Node 1 Name",
        "	OutdoorAir:NodeList,",
        "	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
        "	OutdoorAir:Mixer,",
        "	Zone1FanCoilOAMixer, !- Name",
        "	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
        "	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
        "	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
        "	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
        "	Schedule:Constant,",
        "	FanAndCoilAvailSched, !- Name",
        "	FRACTION, !- Schedule Type",
        "	1;        !- TimeStep Value",
        "	ScheduleTypeLimits,",
        "	Fraction, !- Name",
        "	0.0, !- Lower Limit Value",
        "	1.0, !- Upper Limit Value",
        "	CONTINUOUS;              !- Numeric Type",
        "   Fan:OnOff,",
        "	Zone1FanCoilFan, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	0.5, !- Fan Total Efficiency",
        "	75.0, !- Pressure Rise { Pa }",
        "	0.6, !- Maximum Flow Rate { m3 / s }",
        "	0.9, !- Motor Efficiency",
        "	1.0, !- Motor In Airstream Fraction",
        "	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
        "	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
        "	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
        "	Coil:Cooling:Water,",
        "	Zone1FanCoilCoolingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Namev",
        "	0.0002, !- Design Water Flow Rate { m3 / s }",
        "	0.5000, !- Design Air Flow Rate { m3 / s }",
        "	7.22,   !- Design Inlet Water Temperature { Cv }",
        "	24.340, !- Design Inlet Air Temperature { C }",
        "	14.000, !- Design Outlet Air Temperature { C }",
        "	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
        "	SimpleAnalysis, !- Type of Analysis",
        "	CrossFlow;               !- Heat Exchanger Configuration",
        "	Coil:Heating:Water,",
        "   Zone1FanCoilHeatingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	150.0,   !- U - Factor Times Area Value { W / K }",
        "	0.00014, !- Maximum Water Flow Rate { m3 / s }",
        "	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "	autosize, !- Rated Capacity { W }",
        "	82.2, !- Rated Inlet Water Temperature { C }",
        "	16.6, !- Rated Inlet Air Temperature { C }",
        "	71.1, !- Rated Outlet Water Temperature { C }",
        "	32.2, !- Rated Outlet Air Temperature { C }",
        "	;     !- Rated Ratio for Air and Water Convection",
        "	ZoneHVAC:FourPipeFanCoil,",
        "	Zone1FanCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	ConstantFanVariableFlow, !- Capacity Control Method",
        "	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
        "	0.3, !- Low Speed Supply Air Flow Ratio",
        "	0.6, !- Medium Speed Supply Air Flow Ratio",
        "	0.0, !- Maximum Outdoor Air Flow Rate { m3 / s }",
        "	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
        "	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
        "	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
        "	Fan:OnOff, !- Supply Air Fan Object Type",
        "	Zone1FanCoilFan, !- Supply Air Fan Name",
        "	Coil:Cooling:Water, !- Cooling Coil Object Type",
        "	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
        "	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
        "	0.001, !- Cooling Convergence Tolerance",
        "	Coil:Heating:Water, !- Heating Coil Object Type",
        "	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
        "	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
        "	0.001; !- Heating Convergence Tolerance",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetZoneData(ErrorsFound);
    EXPECT_EQ("EAST ZONE", Zone(1).Name);

    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetFanInput();
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, Fan(1).FanType_Num);

    GetFanCoilUnits();
    EXPECT_EQ("CONSTANTFANVARIABLEFLOW", FanCoil(1).CapCtrlMeth);
    EXPECT_EQ("OUTDOORAIR:MIXER", FanCoil(1).OAMixType);
    EXPECT_EQ("FAN:ONOFF", FanCoil(1).FanType);
    EXPECT_EQ("COIL:COOLING:WATER", FanCoil(1).CCoilType);
    EXPECT_EQ("COIL:HEATING:WATER", FanCoil(1).HCoilType);

    TotNumLoops = 2;
    PlantLoop.allocate(TotNumLoops);

    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;
    // heating load only
    ColdWaterMassFlowRate = 0.0;
    HotWaterMassFlowRate = 0.14;

    Node(OAMixer(1).RetNode).MassFlowRate = AirMassFlow;
    Node(OAMixer(1).RetNode).MassFlowRateMax = MaxAirMassFlow;

    Node(OAMixer(1).RetNode).Temp = 22.0;
    Node(OAMixer(1).RetNode).Enthalpy = 36000;
    Node(OAMixer(1).RetNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).RetNode).Temp, Node(OAMixer(1).RetNode).Enthalpy);

    Node(OAMixer(1).InletNode).Temp = 10.0;
    Node(OAMixer(1).InletNode).Enthalpy = 18000;
    Node(OAMixer(1).InletNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).InletNode).Temp, Node(OAMixer(1).InletNode).Enthalpy);

    Node(FanCoil(1).AirInNode).MassFlowRate = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMin = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMinAvail = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMax = MaxAirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMaxAvail = MaxAirMassFlow;

    FanCoil(1).OutAirMassFlow = 0.0;
    FanCoil(1).MaxAirMassFlow = MaxAirMassFlow;
    FanCoil(1).MaxCoolCoilFluidFlow = 0.14;
    FanCoil(1).MaxHeatCoilFluidFlow = 0.14;

    Node(FanCoil(1).OutsideAirNode).MassFlowRateMax = 0.0;
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMax = 0.14;
    Node(FanCoil(1).HeatCoilFluidInletNode).MassFlowRateMax = 0.14;
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMaxAvail = 0.14;
    Node(FanCoil(1).HeatCoilFluidInletNode).MassFlowRateMaxAvail = 0.14;

    Fan(1).InletAirMassFlowRate = AirMassFlow;
    Fan(1).MaxAirMassFlowRate = MaxAirMassFlow;

    Node(Fan(1).InletNodeNum).MassFlowRate = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).UACoilTotal = 470.0;
    WaterCoil(2).UACoilExternal = 611.0;
    WaterCoil(2).UACoilInternal = 2010.0;
    WaterCoil(2).TotCoilOutsideSurfArea = 50.0;

    Node(WaterCoil(2).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    WaterCoil(2).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).Temp = 6.0;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    Node(WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    Node(WaterCoil(1).WaterInletNodeNum).Temp = 60.0;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    WaterCoil(1).InletWaterMassFlowRate = HotWaterMassFlowRate;
    WaterCoil(1).MaxWaterMassFlowRate = HotWaterMassFlowRate;

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    TempControlType.allocate(1);
    TempControlType(1) = 4;

    WaterCoil(2).WaterLoopNum = 1;
    WaterCoil(2).WaterLoopSide = 1;
    WaterCoil(2).WaterLoopBranchNum = 1;
    WaterCoil(2).WaterLoopCompNum = 1;

    WaterCoil(1).WaterLoopNum = 2;
    WaterCoil(1).WaterLoopSide = 1;
    WaterCoil(1).WaterLoopBranchNum = 1;
    WaterCoil(1).WaterLoopCompNum = 1;

    PlantLoop(2).Name = "ChilledWaterLoop";
    PlantLoop(2).FluidName = "ChilledWater";
    PlantLoop(2).FluidIndex = 1;
    PlantLoop(2).FluidName = "WATER";
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(2).Name;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_Cooling;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(2).WaterInletNodeNum;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(2).WaterOutletNodeNum;
    PlantLoop(2).LoopSide(1).FlowLock = 0;

    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "HotWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(1).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_SimpleHeating;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(1).WaterInletNodeNum;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(1).WaterOutletNodeNum;
    PlantLoop(1).LoopSide(1).FlowLock = 0;

    FanCoil(1).CoolCoilLoopNum = 2;
    FanCoil(1).HeatCoilLoopNum = 1;
    FanCoil(1).CoolCoilLoopSide = 1;
    FanCoil(1).HeatCoilLoopSide = 1;
    FanCoil(1).HeatCoilFluidOutletNodeNum = WaterCoil(1).WaterOutletNodeNum;
    FanCoil(1).CoolCoilFluidOutletNodeNum = WaterCoil(2).WaterOutletNodeNum;
    FanCoil(1).CoolCoilBranchNum = 1;
    FanCoil(1).CoolCoilCompNum = 1;
    FanCoil(1).HeatCoilBranchNum = 1;
    FanCoil(1).HeatCoilCompNum = 1;

    CoolingLoad = false;
    HeatingLoad = true;
    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = 8000.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 4000.0;
    FanCoil(1).SpeedFanSel = 2;
    QUnitOut = 0.0;
    QZnReq = 4000.0;

    MyUAAndFlowCalcFlag.allocate(2);
    MyUAAndFlowCalcFlag(1) = true;
    MyUAAndFlowCalcFlag(2) = true;
    DataGlobals::DoingSizing = true;

    LocalTurnFansOff = false;
    LocalTurnFansOn = true;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(Month, DayOfMonth, 1);
    UpdateScheduleValues();

    // Normal heating simulation for fan coil with constant fan, variable water flow
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);
    FirstHVACIteration = false;
    PlantLoop(1).LoopSide(1).FlowLock = 1;
    Node(FanCoil(1).HeatCoilFluidInletNode).MassFlowRate = 0.2;
    // Simulate with flow lock on and locked flow > demand flow; bypass extra flow
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_NEAR(55.31, Node(10).Temp, 0.1);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);
    // heating simulation with flow lock on and locked flow < flow required for load; use locked flow
    Node(FanCoil(1).HeatCoilFluidInletNode).MassFlowRate = 0.05;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(3780.0, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);
    // normal heating, no flow lock, heating capacity exceeded
    QZnReq = 5000.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 5000.00;
    PlantLoop(1).LoopSide(1).FlowLock = 0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(4420.0, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // Coil Off Capacity Test #1 - low heating load, no flow lock, setting QUnitOutNoHC when flow lock = 0
    QZnReq = 80.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 80.00;
    PlantLoop(1).LoopSide(1).FlowLock = 0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    // FC hits the 80 W target load
    EXPECT_NEAR(80.0, QUnitOut, 1.0);
    EXPECT_NEAR(75.0, FanCoil(1).QUnitOutNoHC, 1.0);
    // water mass flow rate needed to provide output of 80 W (including 75 W coil off capacity)
    EXPECT_NEAR(0.0000315, Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).MassFlowRate, 0.000001);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // Coil Off Capacity Test #2 - lock plant flow after previous call
    PlantLoop(1).LoopSide(1).FlowLock = 1;
    Node(OAMixer(1).RetNode).Temp = 25.0; // change inlet air condition so off capacity will change to see if QUnitOutNoHC remains fixed
    Node(OAMixer(1).RetNode).Enthalpy = 39000;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    // FC does not hit the 80 W target load since flow is locked at a low value
    EXPECT_NEAR(52.0, QUnitOut, 1.0);
    // off coil capacity is same as just prior to flow being locked
    EXPECT_NEAR(75.0, FanCoil(1).QUnitOutNoHC, 1.0);
    // same water flow rate as before
    EXPECT_NEAR(0.0000315, Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).MassFlowRate, 0.000001);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // Coil Off Capacity Test #3 - unlock plant flow to ensure that water flow rate would have been different had flow not been locked
    PlantLoop(1).LoopSide(1).FlowLock = 0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    // FC hits the 80 W target load
    EXPECT_NEAR(80.0, QUnitOut, 1.0);
    // actual coil off output when inlet air temp = 25 C and h = 39000 J/kg
    EXPECT_NEAR(48.0, FanCoil(1).QUnitOutNoHC, 1.0); // interesting that this is very different for a heating system (from Coil Off Capacity Test #1)
    // water flow rate had to increase to get to 80 W since coil off capacity was much different at -1752 W
    EXPECT_NEAR(0.000219, Node(FanCoil(FanCoilNum).HeatCoilFluidInletNode).MassFlowRate, 0.000001);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);
}
TEST_F(EnergyPlusFixture, ElectricCoilFanCoilHeatingTest)
{

    int FanCoilNum(1);
    int ZoneNum(1);
    int ControlledZoneNum(1);
    bool FirstHVACIteration(true);
    bool ErrorsFound(false);
    Real64 QZnReq(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);
    Real64 LatOutputProvided(0.0);

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    WaterCoils::GetWaterCoilsInputFlag = true;
    NumCoils = 0;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.3;",
        "	Zone,",
        "	EAST ZONE, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	autocalculate, !- Ceiling Height { m }",
        "	autocalculate; !- Volume { m3 }",
        "	ZoneHVAC:EquipmentConnections,",
        "	EAST ZONE, !- Zone Name",
        "	Zone1Equipment, !- Zone Conditioning Equipment List Name",
        "	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
        "	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
        "	Zone 1 Node, !- Zone Air Node Name",
        "	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	Zone1Equipment, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
        "	Zone1FanCoil, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "   NodeList,",
        "	Zone1Inlets, !- Name",
        "	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
        "	NodeList,",
        "	Zone1Exhausts, !- Name",
        "	Zone1FanCoilAirInletNode; !- Node 1 Name",
        "	OutdoorAir:NodeList,",
        "	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
        "	OutdoorAir:Mixer,",
        "	Zone1FanCoilOAMixer, !- Name",
        "	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
        "	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
        "	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
        "	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
        "	Schedule:Constant,",
        "	FanAndCoilAvailSched, !- Name",
        "	FRACTION, !- Schedule Type",
        "	1;        !- TimeStep Value",
        "	ScheduleTypeLimits,",
        "	Fraction, !- Name",
        "	0.0, !- Lower Limit Value",
        "	1.0, !- Upper Limit Value",
        "	CONTINUOUS;              !- Numeric Type",
        "	Fan:OnOff,",
        "	Zone1FanCoilFan, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	0.5, !- Fan Total Efficiency",
        "	75.0, !- Pressure Rise { Pa }",
        "	0.6, !- Maximum Flow Rate { m3 / s }",
        "	0.9, !- Motor Efficiency",
        "	1.0, !- Motor In Airstream Fraction",
        "	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
        "	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
        "	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
        "	Coil:Cooling:Water,",
        "	Zone1FanCoilCoolingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Namev",
        "	0.0002, !- Design Water Flow Rate { m3 / s }",
        "	0.5000, !- Design Air Flow Rate { m3 / s }",
        "	7.22,   !- Design Inlet Water Temperature { Cv }",
        "	24.340, !- Design Inlet Air Temperature { C }",
        "	14.000, !- Design Outlet Air Temperature { C }",
        "	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
        "	SimpleAnalysis, !- Type of Analysis",
        "	CrossFlow;               !- Heat Exchanger Configuration",
        "	Coil:Heating:Electric,",
        "   Zone1FanCoilHeatingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	0.9,   !- Efficiency",
        "	4500., !- Nominal Capacity",
        "	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode; !- Air Outlet Node Name",
        "	ZoneHVAC:FourPipeFanCoil,",
        "	Zone1FanCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	ConstantFanVariableFlow, !- Capacity Control Method",
        "	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
        "	0.3, !- Low Speed Supply Air Flow Ratio",
        "	0.6, !- Medium Speed Supply Air Flow Ratio",
        "	0.0, !- Maximum Outdoor Air Flow Rate { m3 / s }",
        "	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
        "	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
        "	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
        "	Fan:OnOff, !- Supply Air Fan Object Type",
        "	Zone1FanCoilFan, !- Supply Air Fan Name",
        "	Coil:Cooling:Water, !- Cooling Coil Object Type",
        "	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
        "	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
        "	0.001, !- Cooling Convergence Tolerance",
        "	Coil:Heating:Electric, !- Heating Coil Object Type",
        "	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
        "	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
        "	0.001; !- Heating Convergence Tolerance",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetZoneData(ErrorsFound);
    EXPECT_EQ("EAST ZONE", Zone(1).Name);

    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetFanInput();
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, Fan(1).FanType_Num);

    GetFanCoilUnits();
    EXPECT_EQ("CONSTANTFANVARIABLEFLOW", FanCoil(1).CapCtrlMeth);
    EXPECT_EQ("OUTDOORAIR:MIXER", FanCoil(1).OAMixType);
    EXPECT_EQ("FAN:ONOFF", FanCoil(1).FanType);
    EXPECT_EQ("COIL:COOLING:WATER", FanCoil(1).CCoilType);
    EXPECT_EQ("COIL:HEATING:ELECTRIC", FanCoil(1).HCoilType);

    TotNumLoops = 1;
    PlantLoop.allocate(TotNumLoops);

    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;
    // heating load only
    ColdWaterMassFlowRate = 0.0;
    HotWaterMassFlowRate = 0.14;

    Node(OAMixer(1).RetNode).MassFlowRate = AirMassFlow;
    Node(OAMixer(1).RetNode).MassFlowRateMax = MaxAirMassFlow;

    Node(OAMixer(1).RetNode).Temp = 22.0;
    Node(OAMixer(1).RetNode).Enthalpy = 36000;
    Node(OAMixer(1).RetNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).RetNode).Temp, Node(OAMixer(1).RetNode).Enthalpy);

    Node(OAMixer(1).InletNode).Temp = 10.0;
    Node(OAMixer(1).InletNode).Enthalpy = 18000;
    Node(OAMixer(1).InletNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).InletNode).Temp, Node(OAMixer(1).InletNode).Enthalpy);

    Node(FanCoil(1).AirInNode).MassFlowRate = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMin = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMinAvail = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMax = MaxAirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMaxAvail = MaxAirMassFlow;

    FanCoil(1).OutAirMassFlow = 0.0;
    FanCoil(1).MaxAirMassFlow = MaxAirMassFlow;
    FanCoil(1).MaxCoolCoilFluidFlow = 0.14;
    FanCoil(1).MaxHeatCoilFluidFlow = 0.14;

    Node(FanCoil(1).OutsideAirNode).MassFlowRateMax = 0.0;
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMax = 0.14;
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMaxAvail = 0.14;

    Fan(1).InletAirMassFlowRate = AirMassFlow;
    Fan(1).MaxAirMassFlowRate = MaxAirMassFlow;

    Node(Fan(1).InletNodeNum).MassFlowRate = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(1).UACoilTotal = 470.0;
    WaterCoil(1).UACoilExternal = 611.0;
    WaterCoil(1).UACoilInternal = 2010.0;
    WaterCoil(1).TotCoilOutsideSurfArea = 50.0;

    Node(WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(1).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    WaterCoil(1).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    Node(WaterCoil(1).WaterInletNodeNum).Temp = 6.0;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    Node(WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    TempControlType.allocate(1);
    TempControlType(1) = 4;

    WaterCoil(1).WaterLoopNum = 1;
    WaterCoil(1).WaterLoopSide = 1;
    WaterCoil(1).WaterLoopBranchNum = 1;
    WaterCoil(1).WaterLoopCompNum = 1;

    PlantLoop(1).Name = "ChilledWaterLoop";
    PlantLoop(1).FluidName = "ChilledWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(1).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_Cooling;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(1).WaterInletNodeNum;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(1).WaterOutletNodeNum;
    PlantLoop(1).LoopSide(1).FlowLock = 0;

    FanCoil(1).CoolCoilLoopNum = 1;
    FanCoil(1).HeatCoilLoopNum = 0;
    FanCoil(1).CoolCoilLoopSide = 1;
    FanCoil(1).HeatCoilLoopSide = 0;
    FanCoil(1).HeatCoilFluidOutletNodeNum = 0;
    FanCoil(1).CoolCoilFluidOutletNodeNum = WaterCoil(1).WaterOutletNodeNum;
    FanCoil(1).CoolCoilBranchNum = 1;
    FanCoil(1).CoolCoilCompNum = 1;
    FanCoil(1).HeatCoilBranchNum = 0;
    FanCoil(1).HeatCoilCompNum = 0;

    CoolingLoad = false;
    HeatingLoad = true;
    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = 8000.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 4000.0;
    FanCoil(1).SpeedFanSel = 2;
    QUnitOut = 0.0;
    QZnReq = 4000.0;

    MyUAAndFlowCalcFlag.allocate(2);
    MyUAAndFlowCalcFlag(1) = true;
    MyUAAndFlowCalcFlag(2) = true;
    DataGlobals::DoingSizing = true;

    LocalTurnFansOff = false;
    LocalTurnFansOn = true;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(Month, DayOfMonth, 1);
    UpdateScheduleValues();

    // Normal heating simulation for fan coil with constant fan, electric heating
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);
    // normal heating, heating capacity exceeded
    QZnReq = 5000.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 5000.00;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(4575.0, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    DataGlobals::DoingSizing = false;
    PlantLoop.deallocate();
    ZoneSysEnergyDemand.deallocate();
    FanCoil.deallocate();
    Node.deallocate();
    WaterCoil.deallocate();
    ZoneEquipConfig.deallocate();
    Zone.deallocate();
    CoilNames.clear();
}
TEST_F(EnergyPlusFixture, ConstantFanVariableFlowFanCoilCoolingTest)
{

    int FanCoilNum(1);
    int ZoneNum(1);
    int ControlledZoneNum(1);
    bool FirstHVACIteration(true);
    bool ErrorsFound(false);
    Real64 QZnReq(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);
    Real64 LatOutputProvided(0.0);

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    WaterCoils::GetWaterCoilsInputFlag = true;
    NumCoils = 0;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.3;",
        "	Zone,",
        "	EAST ZONE, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	autocalculate, !- Ceiling Height { m }",
        "	autocalculate; !- Volume { m3 }",
        "	ZoneHVAC:EquipmentConnections,",
        "	EAST ZONE, !- Zone Name",
        "	Zone1Equipment, !- Zone Conditioning Equipment List Name",
        "	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
        "	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
        "	Zone 1 Node, !- Zone Air Node Name",
        "	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	Zone1Equipment, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
        "	Zone1FanCoil, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "   NodeList,",
        "	Zone1Inlets, !- Name",
        "	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
        "	NodeList,",
        "	Zone1Exhausts, !- Name",
        "	Zone1FanCoilAirInletNode; !- Node 1 Name",
        "	OutdoorAir:NodeList,",
        "	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
        "	OutdoorAir:Mixer,",
        "	Zone1FanCoilOAMixer, !- Name",
        "	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
        "	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
        "	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
        "	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
        "	Schedule:Compact,",
        "	FanAndCoilAvailSched, !- Name",
        "	Fraction, !- Schedule Type Limits Name",
        "	Through: 12/31, !- Field 1",
        "	For: AllDays, !- Field 2",
        "	Until: 24:00, 1.0;        !- Field 3",
        "	ScheduleTypeLimits,",
        "	Fraction, !- Name",
        "	0.0, !- Lower Limit Value",
        "	1.0, !- Upper Limit Value",
        "	CONTINUOUS;              !- Numeric Type",
        "   Fan:OnOff,",
        "	Zone1FanCoilFan, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	0.5, !- Fan Total Efficiency",
        "	75.0, !- Pressure Rise { Pa }",
        "	0.6, !- Maximum Flow Rate { m3 / s }",
        "	0.9, !- Motor Efficiency",
        "	1.0, !- Motor In Airstream Fraction",
        "	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
        "	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
        "	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
        "	Coil:Cooling:Water,",
        "	Zone1FanCoilCoolingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Namev",
        "	0.0002, !- Design Water Flow Rate { m3 / s }",
        "	0.5000, !- Design Air Flow Rate { m3 / s }",
        "	7.22,   !- Design Inlet Water Temperature { Cv }",
        "	24.340, !- Design Inlet Air Temperature { C }",
        "	14.000, !- Design Outlet Air Temperature { C }",
        "	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
        "	SimpleAnalysis, !- Type of Analysis",
        "	CrossFlow;               !- Heat Exchanger Configuration",
        "	Coil:Heating:Water,",
        "   Zone1FanCoilHeatingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	150.0,   !- U - Factor Times Area Value { W / K }",
        "	0.00014, !- Maximum Water Flow Rate { m3 / s }",
        "	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "	autosize, !- Rated Capacity { W }",
        "	82.2, !- Rated Inlet Water Temperature { C }",
        "	16.6, !- Rated Inlet Air Temperature { C }",
        "	71.1, !- Rated Outlet Water Temperature { C }",
        "	32.2, !- Rated Outlet Air Temperature { C }",
        "	;     !- Rated Ratio for Air and Water Convection",
        "	ZoneHVAC:FourPipeFanCoil,",
        "	Zone1FanCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	ConstantFanVariableFlow, !- Capacity Control Method",
        "	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
        "	0.3, !- Low Speed Supply Air Flow Ratio",
        "	0.6, !- Medium Speed Supply Air Flow Ratio",
        "	0.1, !- Maximum Outdoor Air Flow Rate { m3 / s }",
        "	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
        "	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
        "	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
        "	Fan:OnOff, !- Supply Air Fan Object Type",
        "	Zone1FanCoilFan, !- Supply Air Fan Name",
        "	Coil:Cooling:Water, !- Cooling Coil Object Type",
        "	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
        "	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
        "	0.001, !- Cooling Convergence Tolerance",
        "	Coil:Heating:Water, !- Heating Coil Object Type",
        "	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
        "	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
        "	0.001; !- Heating Convergence Tolerance",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetZoneData(ErrorsFound);
    EXPECT_EQ("EAST ZONE", Zone(1).Name);

    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetFanInput();
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, Fan(1).FanType_Num);

    GetFanCoilUnits();
    EXPECT_EQ("CONSTANTFANVARIABLEFLOW", FanCoil(1).CapCtrlMeth);
    EXPECT_EQ("OUTDOORAIR:MIXER", FanCoil(1).OAMixType);
    EXPECT_EQ("FAN:ONOFF", FanCoil(1).FanType);
    EXPECT_EQ("COIL:COOLING:WATER", FanCoil(1).CCoilType);
    EXPECT_EQ("COIL:HEATING:WATER", FanCoil(1).HCoilType);

    TotNumLoops = 2;
    PlantLoop.allocate(TotNumLoops);

    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;

    // cooling load only
    HotWaterMassFlowRate = 0.0;
    ColdWaterMassFlowRate = 0.14;

    Node(OAMixer(1).RetNode).MassFlowRate = AirMassFlow;
    Node(OAMixer(1).RetNode).MassFlowRateMax = MaxAirMassFlow;

    Node(OAMixer(1).RetNode).Temp = 24.0;
    Node(OAMixer(1).RetNode).Enthalpy = 36000;
    Node(OAMixer(1).RetNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).RetNode).Temp, Node(OAMixer(1).RetNode).Enthalpy);

    Node(OAMixer(1).InletNode).Temp = 30.0;
    Node(OAMixer(1).InletNode).Enthalpy = 53000;
    Node(OAMixer(1).InletNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).InletNode).Temp, Node(OAMixer(1).InletNode).Enthalpy);

    Node(FanCoil(1).AirInNode).MassFlowRate = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMin = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMinAvail = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMax = MaxAirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMaxAvail = MaxAirMassFlow;

    FanCoil(1).OutAirMassFlow = 0.0;
    FanCoil(1).MaxAirMassFlow = MaxAirMassFlow;
    FanCoil(1).MaxCoolCoilFluidFlow = 0.14;
    FanCoil(1).MaxHeatCoilFluidFlow = 0.14;

    Node(FanCoil(1).OutsideAirNode).MassFlowRateMax = 0.0;
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMax = 0.14;
    Node(FanCoil(1).HeatCoilFluidInletNode).MassFlowRateMax = 0.14;
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMaxAvail = 0.14;
    Node(FanCoil(1).HeatCoilFluidInletNode).MassFlowRateMaxAvail = 0.14;

    Fan(1).InletAirMassFlowRate = AirMassFlow;
    Fan(1).MaxAirMassFlowRate = MaxAirMassFlow;

    Node(Fan(1).InletNodeNum).MassFlowRate = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).UACoilTotal = 470.0;
    WaterCoil(2).UACoilExternal = 611.0;
    WaterCoil(2).UACoilInternal = 2010.0;
    WaterCoil(2).TotCoilOutsideSurfArea = 50.0;

    Node(WaterCoil(2).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    WaterCoil(2).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).Temp = 6.0;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    Node(WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    Node(WaterCoil(1).WaterInletNodeNum).Temp = 60.0;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    WaterCoil(1).InletWaterMassFlowRate = HotWaterMassFlowRate;
    WaterCoil(1).MaxWaterMassFlowRate = HotWaterMassFlowRate;

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    TempControlType.allocate(1);
    TempControlType(1) = 4;

    WaterCoil(2).WaterLoopNum = 1;
    WaterCoil(2).WaterLoopSide = 1;
    WaterCoil(2).WaterLoopBranchNum = 1;
    WaterCoil(2).WaterLoopCompNum = 1;

    WaterCoil(1).WaterLoopNum = 2;
    WaterCoil(1).WaterLoopSide = 1;
    WaterCoil(1).WaterLoopBranchNum = 1;
    WaterCoil(1).WaterLoopCompNum = 1;

    PlantLoop(2).Name = "ChilledWaterLoop";
    PlantLoop(2).FluidName = "ChilledWater";
    PlantLoop(2).FluidIndex = 1;
    PlantLoop(2).FluidName = "WATER";
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(2).Name;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_Cooling;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(2).WaterInletNodeNum;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(2).WaterOutletNodeNum;
    PlantLoop(2).LoopSide(1).FlowLock = 0;

    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "HotWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(1).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_SimpleHeating;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(1).WaterInletNodeNum;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(1).WaterOutletNodeNum;
    PlantLoop(1).LoopSide(1).FlowLock = 0;

    FanCoil(1).CoolCoilLoopNum = 2;
    FanCoil(1).HeatCoilLoopNum = 1;
    FanCoil(1).CoolCoilLoopSide = 1;
    FanCoil(1).HeatCoilLoopSide = 1;
    FanCoil(1).HeatCoilFluidOutletNodeNum = WaterCoil(1).WaterOutletNodeNum;
    FanCoil(1).CoolCoilFluidOutletNodeNum = WaterCoil(2).WaterOutletNodeNum;
    FanCoil(1).CoolCoilBranchNum = 1;
    FanCoil(1).CoolCoilCompNum = 1;
    FanCoil(1).HeatCoilBranchNum = 1;
    FanCoil(1).HeatCoilCompNum = 1;

    HeatingLoad = false;
    CoolingLoad = true;
    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -4000.00;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -8000.0;
    FanCoil(1).SpeedFanSel = 2;
    QUnitOut = 0.0;
    QZnReq = -4000.0;

    MyUAAndFlowCalcFlag.allocate(2);
    MyUAAndFlowCalcFlag(1) = true;
    MyUAAndFlowCalcFlag(2) = true;
    DataGlobals::DoingSizing = true;

    LocalTurnFansOff = false;
    LocalTurnFansOn = true;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(Month, DayOfMonth, 1);
    UpdateScheduleValues();
    // normal cooling simulation for constant fan variable flow fan coil
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    FirstHVACIteration = false;
    PlantLoop(2).LoopSide(1).FlowLock = 1;
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRate = 0.2;
    // cooling simulation with flow lock on and locked flow > flow that meets load; bypass extra flow
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_NEAR(10.86, Node(13).Temp, 0.1);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // cooling simulation with flow lock on and locked flow < flow required for load; use locked flow
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRate = 0.05;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(-3000.0, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // normal cooling, no flow lock, cooling capacity exceeded
    QZnReq = -5000.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -5000.00;
    PlantLoop(2).LoopSide(1).FlowLock = 0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);
    EXPECT_NEAR(-4420.0, QUnitOut, 5.0);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);
}
TEST_F(EnergyPlusFixture, FanCoil_ASHRAE90VariableFan)
{

    int FanCoilNum(1);
    int ZoneNum(1);
    bool FirstHVACIteration(false);
    bool ErrorsFound(false);
    Real64 QZnReq(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 QLatOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    WaterCoils::GetWaterCoilsInputFlag = true;
    NumCoils = 0;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;
    DataSizing::CurZoneEqNum = 1;

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.3;",
        "	Zone,",
        "	EAST ZONE, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	autocalculate, !- Ceiling Height { m }",
        "	autocalculate; !- Volume { m3 }",
        "	ZoneHVAC:EquipmentConnections,",
        "	EAST ZONE, !- Zone Name",
        "	Zone1Equipment, !- Zone Conditioning Equipment List Name",
        "	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
        "	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
        "	Zone 1 Node, !- Zone Air Node Name",
        "	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	Zone1Equipment, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
        "	Zone1FanCoil, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "   NodeList,",
        "	Zone1Inlets, !- Name",
        "	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
        "	NodeList,",
        "	Zone1Exhausts, !- Name",
        "	Zone1FanCoilAirInletNode; !- Node 1 Name",
        "	OutdoorAir:NodeList,",
        "	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
        "	OutdoorAir:Mixer,",
        "	Zone1FanCoilOAMixer, !- Name",
        "	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
        "	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
        "	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
        "	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
        "	Schedule:Constant,",
        "	FanAndCoilAvailSched, !- Name",
        "	FRACTION, !- Schedule Type",
        "	1;        !- TimeStep Value",
        "	ScheduleTypeLimits,",
        "	Fraction, !- Name",
        "	0.0, !- Lower Limit Value",
        "	1.0, !- Upper Limit Value",
        "	CONTINUOUS;              !- Numeric Type",
        "   Fan:OnOff,",
        "	Zone1FanCoilFan, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	0.5, !- Fan Total Efficiency",
        "	75.0, !- Pressure Rise { Pa }",
        "	0.6, !- Maximum Flow Rate { m3 / s }",
        "	0.9, !- Motor Efficiency",
        "	1.0, !- Motor In Airstream Fraction",
        "	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
        "	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
        "	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
        "	Coil:Cooling:Water,",
        "	Zone1FanCoilCoolingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Namev",
        "	0.0002, !- Design Water Flow Rate { m3 / s }",
        "	0.5000, !- Design Air Flow Rate { m3 / s }",
        "	7.22,   !- Design Inlet Water Temperature { Cv }",
        "	24.340, !- Design Inlet Air Temperature { C }",
        "	14.000, !- Design Outlet Air Temperature { C }",
        "	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
        "	SimpleAnalysis, !- Type of Analysis",
        "	CrossFlow;               !- Heat Exchanger Configuration",
        "	Coil:Heating:Water,",
        "   Zone1FanCoilHeatingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	150.0,   !- U - Factor Times Area Value { W / K }",
        "	0.00014, !- Maximum Water Flow Rate { m3 / s }",
        "	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "	autosize, !- Rated Capacity { W }",
        "	82.2, !- Rated Inlet Water Temperature { C }",
        "	16.6, !- Rated Inlet Air Temperature { C }",
        "	71.1, !- Rated Outlet Water Temperature { C }",
        "	32.2, !- Rated Outlet Air Temperature { C }",
        "	;     !- Rated Ratio for Air and Water Convection",
        "	ZoneHVAC:FourPipeFanCoil,",
        "	Zone1FanCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	ASHRAE90VariableFan, !- Capacity Control Method",
        "	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
        "	0.3, !- Low Speed Supply Air Flow Ratio",
        "	0.6, !- Medium Speed Supply Air Flow Ratio",
        "	0.0, !- Maximum Outdoor Air Flow Rate { m3 / s }",
        "	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
        "	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
        "	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
        "	Fan:OnOff, !- Supply Air Fan Object Type",
        "	Zone1FanCoilFan, !- Supply Air Fan Name",
        "	Coil:Cooling:Water, !- Cooling Coil Object Type",
        "	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
        "	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
        "	0.001, !- Cooling Convergence Tolerance",
        "	Coil:Heating:Water, !- Heating Coil Object Type",
        "	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
        "	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
        "	0.001, !- Heating Convergence Tolerance",
        "	, !- Availability Manager List Name",
        "	, !- Design Specification ZoneHVAC Sizing Object Name",
        "	, !- Supply Air Fan Operating Mode Schedule Name",
        "	16.0, !- Minimum Supply Air Temperature in Cooling Mode",
        "	28.0; !- Maximum Supply Air Temperature in Heating Mode",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetZoneData(ErrorsFound);
    EXPECT_EQ("EAST ZONE", Zone(1).Name);

    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetFanInput();
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, Fan(1).FanType_Num);

    GetFanCoilUnits();
    EXPECT_EQ("ASHRAE90VARIABLEFAN", FanCoil(1).CapCtrlMeth);
    EXPECT_EQ("OUTDOORAIR:MIXER", FanCoil(1).OAMixType);
    EXPECT_EQ("FAN:ONOFF", FanCoil(1).FanType);
    EXPECT_EQ("COIL:COOLING:WATER", FanCoil(1).CCoilType);
    EXPECT_EQ("COIL:HEATING:WATER", FanCoil(1).HCoilType);

    TotNumLoops = 2;
    PlantLoop.allocate(TotNumLoops);

    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;
    // heating load only
    ColdWaterMassFlowRate = 0.0;
    HotWaterMassFlowRate = 1.0;

    Node(OAMixer(1).RetNode).MassFlowRate = AirMassFlow;
    Node(OAMixer(1).RetNode).MassFlowRateMax = MaxAirMassFlow;

    Node(OAMixer(1).RetNode).Temp = 22.0;
    Node(OAMixer(1).RetNode).Enthalpy = 36000;
    Node(OAMixer(1).RetNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).RetNode).Temp, Node(OAMixer(1).RetNode).Enthalpy);

    Node(OAMixer(1).InletNode).Temp = 10.0;
    Node(OAMixer(1).InletNode).Enthalpy = 18000;
    Node(OAMixer(1).InletNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).InletNode).Temp, Node(OAMixer(1).InletNode).Enthalpy);

    Node(FanCoil(1).AirInNode).MassFlowRate = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMin = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMinAvail = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMax = MaxAirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMaxAvail = MaxAirMassFlow;

    FanCoil(1).OutAirMassFlow = 0.0;
    FanCoil(1).MaxAirMassFlow = MaxAirMassFlow;
    Node(FanCoil(1).OutsideAirNode).MassFlowRateMax = 0.0;
    Node(FanCoil(1).NodeNumOfControlledZone).Temp = 22.0;

    Fan(1).InletAirMassFlowRate = AirMassFlow;
    Fan(1).MaxAirMassFlowRate = MaxAirMassFlow;

    Node(Fan(1).InletNodeNum).MassFlowRate = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).UACoilTotal = 470.0;
    WaterCoil(2).UACoilExternal = 611.0;
    WaterCoil(2).UACoilInternal = 2010.0;
    WaterCoil(2).TotCoilOutsideSurfArea = 50.0;

    Node(WaterCoil(2).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    WaterCoil(2).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).Temp = 6.0;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    Node(WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    Node(WaterCoil(1).WaterInletNodeNum).Temp = 60.0;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    WaterCoil(1).InletWaterMassFlowRate = HotWaterMassFlowRate;
    WaterCoil(1).MaxWaterMassFlowRate = HotWaterMassFlowRate;

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    WaterCoil(2).WaterLoopNum = 1;
    WaterCoil(2).WaterLoopSide = 1;
    WaterCoil(2).WaterLoopBranchNum = 1;
    WaterCoil(2).WaterLoopCompNum = 1;

    WaterCoil(1).WaterLoopNum = 2;
    WaterCoil(1).WaterLoopSide = 1;
    WaterCoil(1).WaterLoopBranchNum = 1;
    WaterCoil(1).WaterLoopCompNum = 1;

    PlantLoop(2).Name = "ChilledWaterLoop";
    PlantLoop(2).FluidName = "ChilledWater";
    PlantLoop(2).FluidIndex = 1;
    PlantLoop(2).FluidName = "WATER";
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(2).Name;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_Cooling;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(2).WaterInletNodeNum;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(2).WaterOutletNodeNum;

    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "HotWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(1).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_SimpleHeating;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(1).WaterInletNodeNum;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(1).WaterOutletNodeNum;

    CoolingLoad = false;
    HeatingLoad = true;
    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = 5000.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 4000.0;
    FanCoil(1).SpeedFanSel = 2;
    QUnitOut = 0.0;
    QLatOut = 0.0;
    QZnReq = 4000.0;

    MyUAAndFlowCalcFlag.allocate(2);
    MyUAAndFlowCalcFlag(1) = true;
    MyUAAndFlowCalcFlag(2) = true;
    DataGlobals::DoingSizing = true;

    LocalTurnFansOff = false;
    LocalTurnFansOn = true;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(Month, DayOfMonth, 1);
    UpdateScheduleValues();

    ZoneEqSizing.allocate(1);
    CurDeadBandOrSetback.allocate(1);
    CurDeadBandOrSetback(1) = false;
    TempControlType.allocate(1);
    TempControlType(1) = 4;
    ZoneSizingRunDone = true;
    FinalZoneSizing.allocate(1);
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.5;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.5;
    FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp = 30.0;
    FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat = 0.01;
    FinalZoneSizing(CurZoneEqNum).DesHeatCoilInTemp = 20.0;
    FinalZoneSizing(CurZoneEqNum).DesHeatCoilInHumRat = 0.005;
    FinalZoneSizing(CurZoneEqNum).DesCoolLoad = 4000.0;
    FinalZoneSizing(CurZoneEqNum).DesHeatLoad = 4000.0;
    StdRhoAir = 1.2;

    BeginEnvrnFlag = true;
    InitFanCoilUnits(FanCoilNum, ZoneNum, ZoneNum);
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);

    // expect full flow and meet capacity
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).MaxAirMassFlow, 0.0000000001);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // expect minimum flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 1000.0;
    QZnReq = 1000.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).MaxAirMassFlow * FanCoil(1).LowSpeedRatio, 0.0000000001);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // expect modulated flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 2500.0;
    QZnReq = 2500.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_GT(Node(1).MassFlowRate, FanCoil(1).MaxAirMassFlow * FanCoil(1).LowSpeedRatio);
    EXPECT_LT(Node(1).MassFlowRate, FanCoil(1).MaxAirMassFlow);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // expect full flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -5000.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -4000.0;
    QZnReq = -4000.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).MaxAirMassFlow, 0.0000000001);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // expect full flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -5000.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -4255.0;
    QZnReq = -4255.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).MaxAirMassFlow, 0.0000000001);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // expect minimum flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -1000.0;
    QZnReq = -1000.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).MaxAirMassFlow * FanCoil(1).LowSpeedRatio, 0.0000000001);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    // expect modulated flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -2500.0;
    QZnReq = -2500.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_GT(Node(1).MassFlowRate, FanCoil(1).MaxAirMassFlow * FanCoil(1).LowSpeedRatio);
    EXPECT_LT(Node(1).MassFlowRate, FanCoil(1).MaxAirMassFlow);
    // expect inlet and outlet node air mass flow rates are equal
    EXPECT_EQ(Node(FanCoil(1).AirInNode).MassFlowRate, Node(FanCoil(1).AirOutNode).MassFlowRate);

    DataGlobals::DoingSizing = false;

    PlantLoop.deallocate();
    ZoneSysEnergyDemand.deallocate();
    FanCoil.deallocate();
    Node.deallocate();
    WaterCoil.deallocate();
    ZoneEquipConfig.deallocate();
    Zone.deallocate();
    CoilNames.clear();
}

Real64 ResidualFancoil(Real64 const mdot,
                       Array1<Real64> const &Par // Function parameters
)
{
    int FanCoilNum = 1;
    int ControlledZoneNum = 1;
    bool FirstHVACIteration = false;
    Real64 QUnitOut;
    Real64 QZnReq = Par(1);
    Real64 Residual;

    Node(12).MassFlowRate = mdot;

    Calc4PipeFanCoil(FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);

    Residual = (QUnitOut - QZnReq) / QZnReq;

    return Residual;
}

TEST_F(EnergyPlusFixture, Test_TightenWaterFlowLimits)
{

    using General::SolveRoot;

    int FanCoilNum(1);
    bool FirstHVACIteration(false);
    bool ErrorsFound(false);
    Real64 QZnReq(-1000.0);
    DataPlant::TotNumLoops = 2;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    WaterCoils::GetWaterCoilsInputFlag = true;
    NumCoils = 0;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        " Zone, EAST ZONE, 0, 0, 0, 0, 1, 1, autocalculate, autocalculate;",
        " ZoneHVAC:EquipmentConnections, EAST ZONE, Zone1Equipment, Zone1Inlets, Zone1Exhausts, Zone 1 Node, Zone 1 Outlet Node;",
        " ZoneHVAC:EquipmentList, Zone1Equipment, SequentialLoad, ZoneHVAC:FourPipeFanCoil, Zone1FanCoil, 1, 1;",
        " OutdoorAir:NodeList, Zone1FCOAIn;",
        " OutdoorAir:Mixer, Zone1FanCoilOAMixer, Zone1OAMixOut, Zone1FCOAIn, Zone1FCExh, Zone1FCAirIn;",
        " Fan:ConstantVolume, Zone1FanCoilFan, FCAvailSch, 0.5, 75.0, 0.6, 0.9, 1.0, Zone1OAMixOut, Zone1FCFanOut;",
        " Schedule:Constant, FCAvailSch, FRACTION, 1;",
        " ScheduleTypeLimits, Fraction, 0.0, 1.0, CONTINUOUS;",
        " NodeList, Zone1Inlets, Zone1FCAirOut;",
        " NodeList, Zone1Exhausts, Zone1FCAirIn;",
        " Coil:Cooling:Water, Zone1FCCoolCoil, FCAvailSch, 0.0002, 0.5, 7.22, 24.34, 14.0, 0.0095, 0.009, Zone1FCChWIn, Zone1FCChWOut, "
        "Zone1FCFanOut, Zone1FCCCOut, SimpleAnalysis, CrossFlow;",
        " Coil:Heating:Water, Zone1FanCoilHeatingCoil, FCAvailSch, 150.0, 0.00014, Zone1FCHWIn, Zone1FCHWOut, Zone1FCCCOut, Zone1FCAirOut, "
        "UFactorTimesAreaAndDesignWaterFlowRate, autosize, 82.2, 16.6, 71.1, 32.2, ;",

        " ZoneHVAC:FourPipeFanCoil,",
        "  Zone1FanCoil, !- Name",
        "  FCAvailSch, !- Availability Schedule Name",
        "  MultiSpeedFan, !- Capacity Control Method",
        "  0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
        "  0.3, !- Low Speed Supply Air Flow Ratio",
        "  0.6, !- Medium Speed Supply Air Flow Ratio",
        "  0.0, !- Maximum Outdoor Air Flow Rate { m3 / s }",
        "  FCAvailSch, !- Outdoor Air Schedule Name",
        "  Zone1FCAirIn, !- Air Inlet Node Name",
        "  Zone1FCAirOut, !- Air Outlet Node Name",
        "  OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
        "  Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
        "  Fan:ConstantVolume, !- Supply Air Fan Object Type",
        "  Zone1FanCoilFan, !- Supply Air Fan Name",
        "  Coil:Cooling:Water, !- Cooling Coil Object Type",
        "  Zone1FCCoolCoil, !- Cooling Coil Name",
        "  0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
        "  0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
        "  0.001, !- Cooling Convergence Tolerance",
        "  Coil:Heating:Water, !- Heating Coil Object Type",
        "  Zone1FanCoilHeatingCoil, !- Heating Coil Name",
        "  0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
        "  0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
        "  0.001; !- Heating Convergence Tolerance",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    OutputProcessor::TimeValue.allocate(2);

    GetZoneData(ErrorsFound);
    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    SetPredefinedTables();
    GetFanInput();
    GetFanCoilUnits();

    PlantLoop.allocate(TotNumLoops);
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    WaterCoil(2).WaterLoopNum = 1;
    WaterCoil(2).WaterLoopSide = 1;
    WaterCoil(2).WaterLoopBranchNum = 1;
    WaterCoil(2).WaterLoopCompNum = 1;

    WaterCoil(1).WaterLoopNum = 2;
    WaterCoil(1).WaterLoopSide = 1;
    WaterCoil(1).WaterLoopBranchNum = 1;
    WaterCoil(1).WaterLoopCompNum = 1;

    PlantLoop(2).Name = "ChilledWaterLoop";
    PlantLoop(2).FluidName = "ChilledWater";
    PlantLoop(2).FluidIndex = 1;
    PlantLoop(2).FluidName = "WATER";
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(2).Name;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_Cooling;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(2).WaterInletNodeNum;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(2).WaterOutletNodeNum;

    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "HotWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(1).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_SimpleHeating;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(1).WaterInletNodeNum;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(1).WaterOutletNodeNum;

    bool CoolingLoad = true;
    bool HeatingLoad = false;
    int ControlledZoneNum = 1;
    Real64 MinWaterFlow = 0.0;
    Real64 MaxWaterFlow = 1.5;
    Node(FanCoil(FanCoilNum).AirInNode).Temp = 24.0;
    Node(FanCoil(FanCoilNum).AirInNode).HumRat = 0.00946;
    Node(FanCoil(FanCoilNum).AirInNode).Enthalpy = 48228.946;
    Node(FanCoil(FanCoilNum).AirInNode).MassFlowRate = 0.719999999;
    Node(FanCoil(FanCoilNum).AirInNode).MassFlowRateMax = 0.719999999;
    Node(6).MassFlowRateMaxAvail = 0.72;
    Node(5).MassFlowRateMaxAvail = 0.72;
    FanCoil(FanCoilNum).CCoilName_Index = 2;
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::DayOfYear_Schedule = 1;
    DataEnvironment::DayOfWeek = 2;
    DataGlobals::HourOfDay = 1;
    ProcessScheduleInput();
    UpdateScheduleValues();

    // fan coil can hit maximum iterations while trying to find the water mass flow rate to meet the load. In this case RegulaFalsi will return -1.
    // When this happens, this routine will find tighter limits on min/max water flow rate passed to RegulaFalsi
    // This routine is only called when RegulaFalsi returns -1

    // example usage of function
    // if( SolFlag == -1 ) {
    // tighten limits on water flow rate to see if this allows convergence
    //	CoolingLoad = true;
    //	HeatingLoad = false;
    //	TightenWaterFlowLimits( FanCoilNum, CoolingLoad, HeatingLoad, FanCoil( FanCoilNum ).CoolCoilFluidInletNode, ControlledZoneNum,
    // FirstHVACIteration,
    // QZnReq, MinWaterFlow, MaxWaterFlow );

    // run once to set up fan coil data
    TightenWaterFlowLimits(FanCoilNum,
                           CoolingLoad,
                           HeatingLoad,
                           FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                           ControlledZoneNum,
                           FirstHVACIteration,
                           QZnReq,
                           MinWaterFlow,
                           MaxWaterFlow);

    // full output of fan coil is around -7178 W, MaxWaterFlow should remain at 1.5 and MinWaterFlow should be set to 0.15
    MinWaterFlow = 0.0;
    MaxWaterFlow = 1.5;
    QZnReq = -8000.0;
    TightenWaterFlowLimits(FanCoilNum,
                           CoolingLoad,
                           HeatingLoad,
                           FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                           ControlledZoneNum,
                           FirstHVACIteration,
                           QZnReq,
                           MinWaterFlow,
                           MaxWaterFlow);
    EXPECT_NEAR(MinWaterFlow, 0.15, 0.0000001);
    EXPECT_NEAR(MaxWaterFlow, 1.50, 0.0000001);

    // lower output (using 10% of max water flow rate) of fan coil is around -715 W, MaxWaterFlow should be 10% of 1.5 = 0.15 and MinWaterFlow should
    // be 1% = 0.015
    MinWaterFlow = 0.0;
    MaxWaterFlow = 1.5;
    QZnReq = -800.0;
    TightenWaterFlowLimits(FanCoilNum,
                           CoolingLoad,
                           HeatingLoad,
                           FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                           ControlledZoneNum,
                           FirstHVACIteration,
                           QZnReq,
                           MinWaterFlow,
                           MaxWaterFlow);
    EXPECT_NEAR(MinWaterFlow, 0.015, 0.0000001);
    EXPECT_NEAR(MaxWaterFlow, 0.150, 0.0000001);

    // lower output of fan coil is around 30 W (fan heat is overtaking cooling output), MaxWaterFlow should be 1% of 1.5 = 0.015 and MinWaterFlow
    // should be 0.1% = 0.0015
    MinWaterFlow = 0.0;
    MaxWaterFlow = 1.5;
    QZnReq = -10.0;
    TightenWaterFlowLimits(FanCoilNum,
                           CoolingLoad,
                           HeatingLoad,
                           FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                           ControlledZoneNum,
                           FirstHVACIteration,
                           QZnReq,
                           MinWaterFlow,
                           MaxWaterFlow);
    EXPECT_NEAR(MinWaterFlow, 0.0015, 0.0000001);
    EXPECT_NEAR(MaxWaterFlow, 0.0150, 0.0000001);

    // lower output of fan coil is around 105 W, MaxWaterFlow should be 0.1% of 1.5 = 0.0015 and MinWaterFlow should be 0.01% = 0.00015
    MinWaterFlow = 0.0;
    MaxWaterFlow = 1.5;
    QZnReq = 40.0;
    TightenWaterFlowLimits(FanCoilNum,
                           CoolingLoad,
                           HeatingLoad,
                           FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                           ControlledZoneNum,
                           FirstHVACIteration,
                           QZnReq,
                           MinWaterFlow,
                           MaxWaterFlow);
    EXPECT_NEAR(MinWaterFlow, 0.00015, 0.0000001);
    EXPECT_NEAR(MaxWaterFlow, 0.00150, 0.0000001);

    // lower output of fan coil is around 112 W, MaxWaterFlow should be 0.01% of 1.5 = 0.00015 and MinWaterFlow should be 0.01% = 0.000015
    MinWaterFlow = 0.0;
    MaxWaterFlow = 1.5;
    QZnReq = 110.0;
    TightenWaterFlowLimits(FanCoilNum,
                           CoolingLoad,
                           HeatingLoad,
                           FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                           ControlledZoneNum,
                           FirstHVACIteration,
                           QZnReq,
                           MinWaterFlow,
                           MaxWaterFlow);
    EXPECT_NEAR(MinWaterFlow, 0.000015, 0.0000001);
    EXPECT_NEAR(MaxWaterFlow, 0.000150, 0.0000001);

    // lower output of fan coil is around 112 W, MaxWaterFlow should be 0.001% of 1.5 = 0.000015 and MinWaterFlow should remian at 0.0
    MinWaterFlow = 0.0;
    MaxWaterFlow = 1.5;
    QZnReq = 120.0;
    TightenWaterFlowLimits(FanCoilNum,
                           CoolingLoad,
                           HeatingLoad,
                           FanCoil(FanCoilNum).CoolCoilFluidInletNode,
                           ControlledZoneNum,
                           FirstHVACIteration,
                           QZnReq,
                           MinWaterFlow,
                           MaxWaterFlow);
    EXPECT_NEAR(MinWaterFlow, 0.000000, 0.0000001);
    EXPECT_NEAR(MaxWaterFlow, 0.000015, 0.0000001);

    MinWaterFlow = 0.0;
    MaxWaterFlow = 1.5;
    Real64 ErrorToler = 0.00001;
    int MaxIte = 4;
    int SolFla;
    Real64 mdot;
    Real64 minFlow;
    Real64 maxFlow;
    Array1D<Real64> Par(2); // Function parameters
    Par(1) = -1000.0;
    Par(2) = 0.0;

    General::SolveRoot(ErrorToler, MaxIte, SolFla, mdot, ResidualFancoil, MinWaterFlow, MaxWaterFlow, Par, 2, minFlow, maxFlow);
    EXPECT_EQ(-1, SolFla);
    EXPECT_NEAR(minFlow, 0.0, 0.0000001);
    EXPECT_NEAR(maxFlow, 0.09375, 0.0000001);
    MaxIte = 20;
    HVACSystemRootFinding.HVACSystemRootSolver = DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsi;
    General::SolveRoot(ErrorToler, MaxIte, SolFla, mdot, ResidualFancoil, minFlow, maxFlow, Par);
    EXPECT_EQ(3, SolFla);
}

TEST_F(EnergyPlusFixture, FanCoil_CyclingFanMode)
{

    int FanCoilNum(1);
    int ZoneNum(1);
    bool FirstHVACIteration(false);
    bool ErrorsFound(false);
    Real64 QZnReq(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 QLatOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    WaterCoils::GetWaterCoilsInputFlag = true;
    NumCoils = 0;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;
    DataSizing::CurZoneEqNum = 1;

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Zone,",
        "	EAST ZONE, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	autocalculate, !- Ceiling Height { m }",
        "	autocalculate; !- Volume { m3 }",

        "	ZoneHVAC:EquipmentConnections,",
        "	EAST ZONE, !- Zone Name",
        "	Zone1Equipment, !- Zone Conditioning Equipment List Name",
        "	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
        "	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
        "	Zone 1 Node, !- Zone Air Node Name",
        "	Zone 1 Outlet Node;      !- Zone Return Air Node Name",

        "	ZoneHVAC:EquipmentList,",
        "	Zone1Equipment, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
        "	Zone1FanCoil, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",

        "   NodeList,",
        "	Zone1Inlets, !- Name",
        "	Zone1FanCoilAirOutletNode;  !- Node 1 Name",

        "	NodeList,",
        "	Zone1Exhausts, !- Name",
        "	Zone1FanCoilAirInletNode; !- Node 1 Name",

        "	OutdoorAir:NodeList,",
        "	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",

        "	OutdoorAir:Mixer,",
        "	Zone1FanCoilOAMixer, !- Name",
        "	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
        "	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
        "	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
        "	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",

        "	Schedule:Constant,",
        "	FanAndCoilAvailSched, !- Name",
        "	FRACTION, !- Schedule Type",
        "	1;        !- TimeStep Value",

        "	ScheduleTypeLimits,",
        "	Fraction, !- Name",
        "	0.0, !- Lower Limit Value",
        "	1.0, !- Upper Limit Value",
        "	CONTINUOUS;              !- Numeric Type",

        "   Fan:OnOff,",
        "	Zone1FanCoilFan, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	0.5, !- Fan Total Efficiency",
        "	75.0, !- Pressure Rise { Pa }",
        "	0.6, !- Maximum Flow Rate { m3 / s }",
        "	0.9, !- Motor Efficiency",
        "	1.0, !- Motor In Airstream Fraction",
        "	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
        "	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
        "	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",

        "	Coil:Cooling:Water,",
        "	Zone1FanCoilCoolingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Namev",
        "	0.0002, !- Design Water Flow Rate { m3 / s }",
        "	0.5000, !- Design Air Flow Rate { m3 / s }",
        "	7.22,   !- Design Inlet Water Temperature { Cv }",
        "	24.340, !- Design Inlet Air Temperature { C }",
        "	14.000, !- Design Outlet Air Temperature { C }",
        "	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
        "	SimpleAnalysis, !- Type of Analysis",
        "	CrossFlow;               !- Heat Exchanger Configuration",

        "	Coil:Heating:Water,",
        "   Zone1FanCoilHeatingCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	150.0,   !- U - Factor Times Area Value { W / K }",
        "	0.00014, !- Maximum Water Flow Rate { m3 / s }",
        "	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
        "	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
        "	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "	autosize, !- Rated Capacity { W }",
        "	82.2, !- Rated Inlet Water Temperature { C }",
        "	16.6, !- Rated Inlet Air Temperature { C }",
        "	71.1, !- Rated Outlet Water Temperature { C }",
        "	32.2, !- Rated Outlet Air Temperature { C }",
        "	;     !- Rated Ratio for Air and Water Convection",

        "	ZoneHVAC:FourPipeFanCoil,",
        "	Zone1FanCoil, !- Name",
        "	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	CyclingFan,           !- Capacity Control Method",
        "	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
        "	0.3, !- Low Speed Supply Air Flow Ratio",
        "	0.6, !- Medium Speed Supply Air Flow Ratio",
        "	0.0, !- Maximum Outdoor Air Flow Rate { m3 / s }",
        "	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
        "	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
        "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
        "	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
        "	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
        "	Fan:OnOff, !- Supply Air Fan Object Type",
        "	Zone1FanCoilFan, !- Supply Air Fan Name",
        "	Coil:Cooling:Water, !- Cooling Coil Object Type",
        "	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
        "	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
        "	0.001, !- Cooling Convergence Tolerance",
        "	Coil:Heating:Water, !- Heating Coil Object Type",
        "	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
        "	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
        "	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
        "	0.001; !- Heating Convergence Tolerance",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetZoneData(ErrorsFound);
    EXPECT_EQ("EAST ZONE", Zone(1).Name);

    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetFanInput();
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, Fan(1).FanType_Num);

    GetFanCoilUnits();
    EXPECT_EQ("CYCLINGFAN", FanCoil(1).CapCtrlMeth);
    EXPECT_EQ("OUTDOORAIR:MIXER", FanCoil(1).OAMixType);
    EXPECT_EQ("FAN:ONOFF", FanCoil(1).FanType);
    EXPECT_EQ("COIL:COOLING:WATER", FanCoil(1).CCoilType);
    EXPECT_EQ("COIL:HEATING:WATER", FanCoil(1).HCoilType);

    TotNumLoops = 2;
    PlantLoop.allocate(TotNumLoops);

    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;
    // heating load only
    ColdWaterMassFlowRate = 0.0;
    HotWaterMassFlowRate = 1.0;

    Node(OAMixer(1).RetNode).MassFlowRate = AirMassFlow;
    Node(OAMixer(1).RetNode).MassFlowRateMax = MaxAirMassFlow;

    Node(OAMixer(1).RetNode).Temp = 22.0;
    Node(OAMixer(1).RetNode).Enthalpy = 36000;
    Node(OAMixer(1).RetNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).RetNode).Temp, Node(OAMixer(1).RetNode).Enthalpy);

    Node(OAMixer(1).InletNode).Temp = 10.0;
    Node(OAMixer(1).InletNode).Enthalpy = 18000;
    Node(OAMixer(1).InletNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).InletNode).Temp, Node(OAMixer(1).InletNode).Enthalpy);

    Node(FanCoil(1).AirInNode).MassFlowRate = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMin = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMinAvail = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMax = MaxAirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMaxAvail = MaxAirMassFlow;

    FanCoil(1).OutAirMassFlow = 0.0;
    FanCoil(1).MaxAirMassFlow = MaxAirMassFlow;
    Node(FanCoil(1).OutsideAirNode).MassFlowRateMax = 0.0;

    Fan(1).InletAirMassFlowRate = AirMassFlow;
    Fan(1).MaxAirMassFlowRate = MaxAirMassFlow;

    Node(Fan(1).InletNodeNum).MassFlowRate = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).UACoilTotal = 470.0;
    WaterCoil(2).UACoilExternal = 611.0;
    WaterCoil(2).UACoilInternal = 2010.0;
    WaterCoil(2).TotCoilOutsideSurfArea = 50.0;

    Node(WaterCoil(2).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(WaterCoil(2).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    WaterCoil(2).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    WaterCoil(2).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterInletNodeNum).Temp = 6.0;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(WaterCoil(2).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    Node(WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    Node(WaterCoil(1).WaterInletNodeNum).Temp = 60.0;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    WaterCoil(1).InletWaterMassFlowRate = HotWaterMassFlowRate;
    WaterCoil(1).MaxWaterMassFlowRate = HotWaterMassFlowRate;

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    WaterCoil(2).WaterLoopNum = 1;
    WaterCoil(2).WaterLoopSide = 1;
    WaterCoil(2).WaterLoopBranchNum = 1;
    WaterCoil(2).WaterLoopCompNum = 1;

    WaterCoil(1).WaterLoopNum = 2;
    WaterCoil(1).WaterLoopSide = 1;
    WaterCoil(1).WaterLoopBranchNum = 1;
    WaterCoil(1).WaterLoopCompNum = 1;

    PlantLoop(2).Name = "ChilledWaterLoop";
    PlantLoop(2).FluidName = "ChilledWater";
    PlantLoop(2).FluidIndex = 1;
    PlantLoop(2).FluidName = "WATER";
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(2).Name;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_Cooling;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(2).WaterInletNodeNum;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(2).WaterOutletNodeNum;

    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "HotWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = WaterCoil(1).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = WaterCoil_SimpleHeating;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = WaterCoil(1).WaterInletNodeNum;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = WaterCoil(1).WaterOutletNodeNum;

    CoolingLoad = false;
    HeatingLoad = true;
    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = 0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 4000.0;
    ZoneSysEnergyDemand(1).RemainingOutputRequired = 4000.0;
    FanCoil(1).SpeedFanSel = 2;
    QUnitOut = 0.0;
    QLatOut = 0.0;
    QZnReq = 4000.0;

    MyUAAndFlowCalcFlag.allocate(2);
    MyUAAndFlowCalcFlag(1) = true;
    MyUAAndFlowCalcFlag(2) = true;
    DataGlobals::DoingSizing = true;

    LocalTurnFansOff = false;
    LocalTurnFansOn = true;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(Month, DayOfMonth, 1);
    UpdateScheduleValues();

    ZoneEqSizing.allocate(1);
    CurDeadBandOrSetback.allocate(1);
    CurDeadBandOrSetback(1) = false;
    TempControlType.allocate(1);
    TempControlType(1) = 4;
    ZoneSizingRunDone = true;
    FinalZoneSizing.allocate(1);
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.5;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.5;
    FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp = 30.0;
    FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat = 0.01;
    FinalZoneSizing(CurZoneEqNum).DesHeatCoilInTemp = 20.0;
    FinalZoneSizing(CurZoneEqNum).DesHeatCoilInHumRat = 0.005;
    FinalZoneSizing(CurZoneEqNum).DesCoolLoad = 4000.0;
    FinalZoneSizing(CurZoneEqNum).DesHeatLoad = 4000.0;
    StdRhoAir = 1.2;

    BeginEnvrnFlag = true;
    InitFanCoilUnits(FanCoilNum, ZoneNum, ZoneNum);
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    // expect fan speed 3 and near full air and water flow and meet capacity
    EXPECT_EQ(3, FanCoil(1).SpeedFanSel);
    EXPECT_GT(FanCoil(1).PLR, 0.95);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // cycling fan proportional to PLR and fan speed ratio (is 1 here)
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).PLR * FanCoil(1).MaxAirMassFlow, 0.0000000001);

    // expect minimum flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 1000.0;
    ZoneSysEnergyDemand(1).RemainingOutputRequired = 1000.0;
    QZnReq = 1000.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    // expect fan speed 1 and moderate air and water flow and meet capacity
    EXPECT_EQ(1, FanCoil(1).SpeedFanSel);
    EXPECT_GT(FanCoil(1).PLR, 0.6);
    EXPECT_LT(FanCoil(1).PLR, 0.65);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // cycling fan proportional to PLR and fan speed ratio (is 0.3 here)
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).PLR * FanCoil(1).MaxAirMassFlow * FanCoil(1).LowSpeedRatio, 0.0000000001);

    // expect modulated flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 2500.0;
    ZoneSysEnergyDemand(1).RemainingOutputRequired = 2500.0;
    QZnReq = 2500.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    // expect fan speed 2 and moderate air and water flow and meet capacity
    EXPECT_EQ(2, FanCoil(1).SpeedFanSel);
    EXPECT_GT(FanCoil(1).PLR, 0.8);
    EXPECT_LT(FanCoil(1).PLR, 0.85);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // cycling fan proportional to PLR and fan speed ratio (is 0.6 here)
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).PLR * FanCoil(1).MaxAirMassFlow * FanCoil(1).MedSpeedRatio, 0.0000000001);

    // expect full flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = 0.0;
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -4000.0;
    ZoneSysEnergyDemand(1).RemainingOutputRequired = -4000.0;
    QZnReq = -4000.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    // expect fan speed 3 and near full air and water flow and meet capacity
    EXPECT_EQ(3, FanCoil(1).SpeedFanSel);
    EXPECT_GT(FanCoil(1).PLR, 0.9);
    EXPECT_LT(FanCoil(1).PLR, 0.95);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // cycling fan proportional to PLR and fan speed ratio (is 1 here)
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).PLR * FanCoil(1).MaxAirMassFlow, 0.0000000001);

    // expect minimum flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -1000.0;
    ZoneSysEnergyDemand(1).RemainingOutputRequired = -1000.0;
    QZnReq = -1000.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    // expect fan speed 1 and moderate air and water flow and meet capacity
    EXPECT_EQ(1, FanCoil(1).SpeedFanSel);
    EXPECT_GT(FanCoil(1).PLR, 0.5);
    EXPECT_LT(FanCoil(1).PLR, 0.55);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    // cycling fan proportional to PLR and fan speed ratio (is 0.3 here)
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).PLR * FanCoil(1).MaxAirMassFlow * FanCoil(1).LowSpeedRatio, 0.0000000001);

    // expect modulated flow and meet capacity
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -2500.0;
    ZoneSysEnergyDemand(1).RemainingOutputRequired = -2500.0;
    QZnReq = -2500.0;
    Sim4PipeFanCoil(FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut);
    // expect fan speed 2 and moderate air and water flow and meet capacity
    EXPECT_EQ(2, FanCoil(1).SpeedFanSel);
    EXPECT_GT(FanCoil(1).PLR, 0.75);
    EXPECT_LT(FanCoil(1).PLR, 0.8);
    EXPECT_NEAR(QZnReq, QUnitOut, 5.0);
    EXPECT_NEAR(Node(1).MassFlowRate, FanCoil(1).PLR * FanCoil(1).MaxAirMassFlow * FanCoil(1).MedSpeedRatio, 0.0000000001);
}

} // namespace EnergyPlus
