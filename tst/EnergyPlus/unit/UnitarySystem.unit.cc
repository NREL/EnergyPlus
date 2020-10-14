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

#include <stdexcept>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus;
using namespace UnitarySystems;

class ZoneUnitarySysTest : public EnergyPlusFixture
{
public:
    int UnitarySysNum = 1;
    int NumNodes = 1; // number of zone inlet and zone exhaust nodes
    bool ErrorsFound = false;
    Real64 const CpWater = 4180.0;  // For estimating the expected result
    Real64 const RhoWater = 1000.0; // For estimating the expected result

protected:
    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up the base fixture first.

        DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0); // initialize StdRhoAir
        DataEnvironment::OutBaroPress = 101325.0;
        DataGlobals::NumOfZones = 1;
        DataHeatBalance::Zone.allocate(DataGlobals::NumOfZones);
        DataZoneEquipment::ZoneEquipConfig.allocate(DataGlobals::NumOfZones);
        DataZoneEquipment::ZoneEquipList.allocate(DataGlobals::NumOfZones);
        DataZoneEquipment::ZoneEquipAvail.dimension(DataGlobals::NumOfZones, DataHVACGlobals::NoAction);
        DataHeatBalance::Zone(1).Name = "EAST ZONE";
        DataZoneEquipment::NumOfZoneEquipLists = 1;
        DataHeatBalance::Zone(1).IsControlled = true;
        DataZoneEquipment::ZoneEquipConfig(1).IsControlled = true;
        DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum = 1;
        DataZoneEquipment::ZoneEquipConfig(1).ZoneName = "EAST ZONE";
        DataZoneEquipment::ZoneEquipConfig(1).EquipListName = "ZONE2EQUIPMENT";
        DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = 20;
        DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 1;
        DataZoneEquipment::ZoneEquipConfig(1).ReturnNode.allocate(1);
        DataZoneEquipment::ZoneEquipConfig(1).ReturnNode(1) = 21;
        DataZoneEquipment::ZoneEquipConfig(1).FixedReturnFlow.allocate(1);
        DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum).SystemZoneNodeNumber =
            DataZoneEquipment::ZoneEquipConfig(1).ZoneNode;
        DataZoneEquipment::ZoneEquipConfig(1).ReturnFlowSchedPtrNum = DataGlobals::ScheduleAlwaysOn;
        DataZoneEquipment::ZoneEquipList(1).Name = "ZONE2EQUIPMENT";
        int maxEquipCount = 1;
        DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes = maxEquipCount;
        DataZoneEquipment::ZoneEquipList(1).EquipType.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipType_Num.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipName.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipIndex = 1;
        DataZoneEquipment::ZoneEquipList(1).EquipData.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).CoolingPriority.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).HeatingPriority.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipType(1) = "AIRLOOPHVAC:UNITARYSYSTEM";
        DataZoneEquipment::ZoneEquipList(1).EquipName(1) = "UNITARY SYSTEM MODEL";
        DataZoneEquipment::ZoneEquipList(1).CoolingPriority(1) = 1;
        DataZoneEquipment::ZoneEquipList(1).HeatingPriority(1) = 1;
        DataZoneEquipment::ZoneEquipList(1).EquipType_Num(1) = DataZoneEquipment::ZoneUnitarySys_Num;
        DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = NumNodes;
        DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitCool.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitHeat.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 2;
        DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = NumNodes;
        DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = 1;
        DataZoneEquipment::ZoneEquipConfig(1).EquipListIndex = 1;

        DataSizing::CurSysNum = 0;
        DataSizing::CurZoneEqNum = 1;

        DataSizing::FinalZoneSizing.allocate(1);
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolVolFlow = 1.5;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow = 1.2;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInTemp = 25.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).ZoneTempAtCoolPeak = 25.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInHumRat = 0.009;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).ZoneHumRatAtCoolPeak = 0.009;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesTemp = 15.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesHumRat = 0.006;

        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatCoilInTemp = 20.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).ZoneTempAtHeatPeak = 20.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).HeatDesTemp = 30.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).HeatDesHumRat = 0.007;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatMassFlow =
            DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow * DataEnvironment::StdRhoAir;

        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).TimeStepNumAtCoolMax = 1;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDDNum = 1;
        DataSizing::DesDayWeath.allocate(1);
        DataSizing::DesDayWeath(1).Temp.allocate(1);
        DataSizing::DesDayWeath(1).Temp(1) = 35.0;

        DataSizing::ZoneEqSizing.allocate(1);
        DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod.allocate(25);
        DataSizing::ZoneSizingRunDone = true;

        // set up plant loop
        DataPlant::TotNumLoops = 2;
        DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
        DataSizing::PlantSizData.allocate(DataPlant::TotNumLoops);
        // int NumPltSizInput = DataPlant::TotNumLoops;
        DataSizing::NumPltSizInput = 2;

        for (int loopindex = 1; loopindex <= DataPlant::TotNumLoops; ++loopindex) {
            auto &loop(DataPlant::PlantLoop(loopindex));
            loop.LoopSide.allocate(2);
            auto &loopside(DataPlant::PlantLoop(loopindex).LoopSide(1));
            loopside.TotalBranches = 1;
            loopside.Branch.allocate(1);
            auto &loopsidebranch(DataPlant::PlantLoop(loopindex).LoopSide(1).Branch(1));
            loopsidebranch.TotalComponents = 2;
            loopsidebranch.Comp.allocate(2);
        }
        DataPlant::PlantLoop(1).Name = "Hot Water Loop";
        DataPlant::PlantLoop(1).FluidName = "WATER";
        DataPlant::PlantLoop(1).FluidIndex = 1;

        DataPlant::PlantLoop(2).Name = "Chilled Water Loop";
        DataPlant::PlantLoop(2).FluidName = "WATER";
        DataPlant::PlantLoop(2).FluidIndex = 1;

        DataSizing::PlantSizData(1).PlantLoopName = "Hot Water Loop";
        DataSizing::PlantSizData(1).ExitTemp = 80.0;
        DataSizing::PlantSizData(1).DeltaT = 10.0;

        DataSizing::PlantSizData(2).PlantLoopName = "Chilled Water Loop";
        DataSizing::PlantSizData(2).ExitTemp = 6.0;
        DataSizing::PlantSizData(2).DeltaT = 5.0;
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!
    }
};

class AirloopUnitarySysTest : public EnergyPlusFixture
{

public:
    static void SetUpTestCase()
    {
        EnergyPlusFixture::SetUpTestCase(); // Sets up the base fixture
    }
    static void TearDownTestCase()
    {
    }

    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up individual test cases.

        DataSizing::CurZoneEqNum = 0;
        DataSizing::CurSysNum = 0;
        DataSizing::CurOASysNum = 0;
        state.dataWaterCoils->NumWaterCoils = 2;
        state.dataWaterCoils->WaterCoil.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->WaterCoilNumericFields.allocate(state.dataWaterCoils->NumWaterCoils);
        for (int i = 1; i <= state.dataWaterCoils->NumWaterCoils; ++i) {
            state.dataWaterCoils->WaterCoilNumericFields(i).FieldNames.allocate(17); // max N fields for water coil
        }
        DataPlant::TotNumLoops = 2;
        DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
        DataSizing::PlantSizData.allocate(2);
        DataSizing::ZoneEqSizing.allocate(2);
        DataSizing::UnitarySysEqSizing.allocate(2);
        DataSizing::OASysEqSizing.allocate(2);
        DataSizing::SysSizInput.allocate(1);
        DataSizing::ZoneSizingInput.allocate(1);
        DataSizing::SysSizPeakDDNum.allocate(1);
        DataSizing::SysSizPeakDDNum(1).TimeStepAtSensCoolPk.allocate(1);
        DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk.allocate(1);
        DataSizing::SysSizPeakDDNum(1).TimeStepAtTotCoolPk.allocate(1);
        DataSizing::SysSizPeakDDNum(1).SensCoolPeakDD = 1;
        DataSizing::SysSizPeakDDNum(1).CoolFlowPeakDD = 1;
        DataSizing::SysSizPeakDDNum(1).TotCoolPeakDD = 1;
        DataSizing::FinalSysSizing.allocate(1);
        DataSizing::FinalZoneSizing.allocate(1);
        DataHVACGlobals::NumPrimaryAirSys = 1;
        DataAirSystems::PrimaryAirSystem.allocate(1);
        state.dataAirLoop->AirLoopControlInfo.allocate(1);
        Psychrometrics::InitializePsychRoutines();
        DataLoopNode::Node.allocate(30);
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!

        state.dataWaterCoils->NumWaterCoils = 0;
        state.dataWaterCoils->WaterCoil.clear();
        state.dataWaterCoils->WaterCoilNumericFields.clear();
        DataPlant::PlantLoop.clear();
        DataSizing::PlantSizData.clear();
        DataSizing::ZoneEqSizing.clear();
        DataSizing::UnitarySysEqSizing.clear();
        DataSizing::OASysEqSizing.clear();
        DataSizing::SysSizInput.clear();
        DataSizing::SysSizPeakDDNum.clear();
        DataSizing::FinalSysSizing.clear();
        DataSizing::SysSizPeakDDNum.clear();
        DataHVACGlobals::NumPrimaryAirSys = 0;
        DataAirSystems::PrimaryAirSystem.clear();
        state.dataAirLoop->AirLoopControlInfo.clear();
        Psychrometrics::cached_Twb.clear();
        Psychrometrics::cached_Psat.clear();
        DataLoopNode::Node.clear();
    }
};

TEST_F(AirloopUnitarySysTest, MultipleWaterCoolingCoilSizing)
{

    // Set up raw water coil sizes as coil-on-branch configuration then
    // test against sizing of same water coils in UntarySystem

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    DataSizing::SysSizingRunDone = true;

    // set up plant sizing
    DataSizing::NumPltSizInput = 2;
    DataSizing::PlantSizData(1).PlantLoopName = "ColdWaterLoop";
    DataSizing::PlantSizData(2).PlantLoopName = "HotWaterLoop";
    DataSizing::CurDuctType = DataHVACGlobals::Main;

    // set up plant loop
    for (int l = 1; l <= DataPlant::TotNumLoops; ++l) {
        auto &loop(DataPlant::PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(DataPlant::PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(DataPlant::PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    DataPlant::PlantLoop(1).Name = "ColdWaterLoop";
    DataPlant::PlantLoop(1).FluidName = "FluidWaterLoop";
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).FluidName = "WATER";
    DataPlant::PlantLoop(1).FluidIndex = 1;

    DataPlant::PlantLoop(2).Name = "HotWaterLoop";
    DataPlant::PlantLoop(2).FluidName = "FluidWaterLoop";
    DataPlant::PlantLoop(2).FluidIndex = 1;
    DataPlant::PlantLoop(2).FluidName = "WATER";
    DataPlant::PlantLoop(2).FluidIndex = 1;

    // set up sizing data
    DataSizing::FinalSysSizing(1).MixTempAtCoolPeak = 20.0;
    DataSizing::FinalSysSizing(1).CoolSupTemp = 10.0;
    DataSizing::FinalSysSizing(1).MixHumRatAtCoolPeak = 0.01;
    DataSizing::FinalSysSizing(1).DesMainVolFlow = 0.159;
    DataSizing::FinalSysSizing(1).DesCoolVolFlow = 0.159;
    DataSizing::FinalSysSizing(1).DesHeatVolFlow = 0.159;
    DataSizing::FinalSysSizing(1).HeatSupTemp = 25.0;
    DataSizing::FinalSysSizing(1).HeatOutTemp = 5.0;
    DataSizing::FinalSysSizing(1).HeatRetTemp = 20.0;

    // set up water coil
    int CoilNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Cooling Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopSide = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopBranchNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopCompNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Cooling;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_Cooling;
    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = DataSizing::AutoSize;

    state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(3) = "Maximum Flow Rate";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum = 2;
    state.dataWaterCoils->WaterCoil(CoilNum).AirInletNodeNum = 3;
    state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum = 4;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    DataSizing::CurZoneEqNum = 0;
    DataSizing::CurSysNum = 1;
    DataSizing::CurOASysNum = 0;
    DataSizing::FinalSysSizing(1).SysAirMinFlowRat = 0.3;
    Real64 heatFlowRat = 0.3;
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VAV;
    DataSizing::PlantSizData(1).ExitTemp = 5.7;
    DataSizing::PlantSizData(1).DeltaT = 5.0;
    DataSizing::FinalSysSizing(1).MassFlowAtCoolPeak = DataSizing::FinalSysSizing(1).DesMainVolFlow * DataEnvironment::StdRhoAir;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(CoilNum).Name;
    DataSizing::DataWaterLoopNum = 1;
    FluidProperties::NumOfGlycols = 1;

    createCoilSelectionReportObj();
    WaterCoils::SizeWaterCoil(state, CoilNum);

    EXPECT_DOUBLE_EQ(0.159, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);
    EXPECT_NEAR(6779.0, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate, 1.0);
    EXPECT_EQ(20.0, state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp); // coil inlet does not include fan heat
    // save coil capacity for comparison to UnitarySyste
    Real64 coil1CoolingCoilRate = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate;
    // save cooling coil air flow rate for use in fan heat calculation
    Real64 coil1CoolingAirFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate;

    // reset coil sizing for next test
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = DataSizing::AutoSize;

    // size heating coil
    CoilNum = 2;
    DataSizing::FinalSysSizing(1).MassFlowAtCoolPeak = DataSizing::FinalSysSizing(1).DesMainVolFlow * DataEnvironment::StdRhoAir;
    state.dataAirLoop->AirLoopControlInfo(1).UnitarySys = true;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 5;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum = 6;
    state.dataWaterCoils->WaterCoil(CoilNum).AirInletNodeNum = 7;
    state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum = 8;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Heating Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 2;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopSide = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopBranchNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopCompNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_SimpleHeating;
    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = DataSizing::AutoSize;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(CoilNum).Name;
    DataSizing::DataWaterLoopNum = 2;
    DataSizing::PlantSizData(2).DeltaT = 5.0;

    WaterCoils::SizeWaterCoil(state, CoilNum);

    // heating flow rate adjusted by FinalSysSizing(1).SysAirMinFlowRat = 0.3
    EXPECT_NEAR(0.159 * heatFlowRat, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate, 0.00001);
    EXPECT_NEAR(1154.0, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate, 1.0);
    Real64 coil2HeatingCoilRate = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate;

    // reset coil sizing for next test
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = DataSizing::AutoSize;

    // resize cooling coil with fan on branch
    CoilNum = 1;
    Fans::Fan.allocate(1);
    Fans::Fan(1).DeltaPress = 600.0;
    Fans::Fan(1).FanEff = 0.9;
    Fans::Fan(1).MotEff = 0.7;
    Fans::Fan(1).MotInAirFrac = 1.0;

    DataAirSystems::PrimaryAirSystem(1).supFanModelTypeEnum = DataAirSystems::structArrayLegacyFanModels;
    DataAirSystems::PrimaryAirSystem(1).supFanVecIndex = 1;
    DataAirSystems::PrimaryAirSystem(1).SupFanNum = 1;
    DataAirSystems::PrimaryAirSystem(1).supFanLocation = DataAirSystems::fanPlacement::BlowThru;
    Real64 FanCoolLoad = Fans::FanDesHeatGain(state, DataAirSystems::PrimaryAirSystem(1).SupFanNum, coil1CoolingAirFlowRate);
    WaterCoils::SizeWaterCoil(state, CoilNum);

    EXPECT_NEAR(FanCoolLoad, 106.0, 1.0); // make sure there is enough fan heat to change results
    EXPECT_NEAR(6779.0 + FanCoolLoad, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate, 1.0);
    EXPECT_NEAR(20.541, state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp, 0.001); // coil inlet does include fan heat

    // reset coil sizing for next test
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = DataSizing::AutoSize;
    // reset primary air system fan type and location as if is doesn't exist
    DataAirSystems::PrimaryAirSystem(1).supFanModelTypeEnum = DataAirSystems::fanModelTypeNotYetSet;
    DataAirSystems::PrimaryAirSystem(1).supFanLocation = DataAirSystems::fanPlacement::fanPlaceNotSet;

    // size same coils in UnitarySystem
    int AirLoopNum(1);
    bool FirstHVACIteration(true);
    UnitarySys thisSys;
    UnitarySys *mySys(&thisSys);
    mySys->m_CoolCoilExists = true;
    mySys->m_HeatCoilExists = true;
    mySys->m_MaxCoolAirVolFlow = DataSizing::AutoSize;
    mySys->m_MaxHeatAirVolFlow = DataSizing::AutoSize;
    mySys->m_CoolingSAFMethod = DataSizing::SupplyAirFlowRate;
    mySys->m_HeatingSAFMethod = DataSizing::SupplyAirFlowRate;
    mySys->m_DesignCoolingCapacity = DataSizing::AutoSize;
    mySys->m_DesignHeatingCapacity = DataSizing::AutoSize;
    mySys->m_CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingWater;
    mySys->m_CoolingCoilName = "Test Water Cooling Coil";
    mySys->m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingWater;
    mySys->m_HeatingCoilName = "Test Water Heating Coil";
    state.dataWaterCoils->GetWaterCoilsInputFlag = false;             // don't overwrite these coil data
    state.dataWaterCoils->MySizeFlag = true;                          // need to size again for UnitarySystem
    state.dataWaterCoils->WaterCoil(1).DesWaterCoolingCoilRate = 0.0; // reset these to be sure they get recalculated
    state.dataWaterCoils->WaterCoil(2).DesWaterHeatingCoilRate = 0.0;
    OutputReportPredefined::SetPredefinedTables();
    DataGlobals::DoingSizing = true;

    mySys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

    // Show coil sizes in UnitarySystem, cooling coil does not include fan heat
    // Same sizes as coil on branch without fan heat
    EXPECT_NEAR(6672.0, state.dataWaterCoils->WaterCoil(1).DesWaterCoolingCoilRate, 1.0);
    EXPECT_EQ(20.0, state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp); // coil inlet does not include fan heat
    // heating coil in UnitarySystem sized at higher air flow rate, i.e., not using SysAirMinFlowRat
    EXPECT_NEAR(3848.0, state.dataWaterCoils->WaterCoil(2).DesWaterHeatingCoilRate, 1.0);
    // note size of heating coil on branch is smaller than heating coil in UnitarySystem minus 10 W
    EXPECT_LT(coil2HeatingCoilRate, 3838.0);
    // heating flow rate of coil in UnitarySystem NOT adjusted by FinalSysSizing(1).SysAirMinFlowRat = 0.3
    EXPECT_NEAR(0.159, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate, 0.00001);

    // the water cooling coil sizes are only different by the air density used in capacity calculation
    // water coils use StdRhoAir and UnitarySystem coils use actual air density
    Real64 CoilInTemp = DataSizing::FinalSysSizing(1).MixTempAtCoolPeak;
    Real64 CoilInHumRat = DataSizing::FinalSysSizing(1).MixHumRatAtCoolPeak;
    Real64 rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, CoilInTemp, CoilInHumRat);
    // this is the ratio of UnitarySystem cooling coil size to coil-on-branch water cooling coil size
    Real64 rhoRatio = DataEnvironment::StdRhoAir / rhoair;

    EXPECT_NEAR(coil1CoolingCoilRate, rhoRatio * state.dataWaterCoils->WaterCoil(1).DesWaterCoolingCoilRate, 1.0);
    EXPECT_NEAR(coil1CoolingCoilRate, rhoRatio * mySys->m_DesignCoolingCapacity, 1.0);
    // the heating coils are sized differently since SysAirMinFlowRat is not accounted for
    EXPECT_LT(coil2HeatingCoilRate, state.dataWaterCoils->WaterCoil(2).DesWaterHeatingCoilRate);
    EXPECT_LT(coil2HeatingCoilRate, mySys->m_DesignHeatingCapacity);

    // add fan to UnitarySystem
    mySys->m_FanExists = true;
    mySys->m_FanIndex = 1;
    mySys->m_FanPlace = UnitarySys::FanPlace::BlowThru;
    // reset sizing information
    mySys->m_MaxCoolAirVolFlow = DataSizing::AutoSize;
    mySys->m_MaxHeatAirVolFlow = DataSizing::AutoSize;
    mySys->m_CoolingSAFMethod = DataSizing::SupplyAirFlowRate;
    mySys->m_HeatingSAFMethod = DataSizing::SupplyAirFlowRate;
    mySys->m_DesignCoolingCapacity = DataSizing::AutoSize;
    mySys->m_DesignHeatingCapacity = DataSizing::AutoSize;
    // pretend this is first call and UnitarySystem doesn't know the coil index
    mySys->m_CoolingCoilIndex = 0;
    mySys->m_HeatingCoilIndex = 0;
    state.dataWaterCoils->MySizeFlag = true;
    state.dataWaterCoils->WaterCoil(1).DesAirVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(1).DesInletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(1).DesOutletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(1).DesInletWaterTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(1).DesInletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(1).DesOutletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(1).MaxWaterVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(1).MaxWaterVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(2).DesAirVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(2).DesInletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(2).DesOutletAirTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(2).DesInletWaterTemp = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(2).DesInletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(2).DesOutletAirHumRat = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(2).MaxWaterVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(2).MaxWaterVolFlowRate = DataSizing::AutoSize;
    state.dataWaterCoils->WaterCoil(1).DesWaterCoolingCoilRate = 0.0; // reset these to be sure they get recalculated
    state.dataWaterCoils->WaterCoil(2).DesWaterHeatingCoilRate = 0.0;

    mySys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

    // Show coil sizes in UnitarySystem, cooling coil now includes fan heat
    // Same sizes as coil on branch with fan heat
    EXPECT_NEAR(6672.0 + FanCoolLoad, state.dataWaterCoils->WaterCoil(1).DesWaterCoolingCoilRate, 1.0);
    EXPECT_NEAR(6672.0 + FanCoolLoad, mySys->m_DesignCoolingCapacity, 1.0);
    EXPECT_NEAR(20.541, state.dataWaterCoils->WaterCoil(1).DesInletAirTemp, 0.001); // coil inlet does include fan heat
    // the heating coils are sized differently since SysAirMinFlowRat is not accounted for
    EXPECT_NEAR(3848.0, state.dataWaterCoils->WaterCoil(2).DesWaterHeatingCoilRate, 1.0);
}

TEST_F(ZoneUnitarySysTest, Test_UnitarySystemModel_factory)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  ,                               !- Heating Coil Object Type",
        "  ,                               !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:MultiSpeed,     !- Cooling Coil Object Type",
        "  DX Cooling Coil,                !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  AutoSize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Supply air Flow Rate Method During Heating Operation",
        "  AutoSize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  AutoSize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,                     !- Design Specification Multispeed Object Type",
        "  DX Cool MultiSpd Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  DX Cool MultiSpd Unitary System MultiSpeed Performance,  !- Name",
        "  1,                              !- Number of Speeds for Heating",
        "  2,                              !- Number of Speeds for Cooling",
        "  No,                             !- Single Mode Operation",
        "  ,                               !- No Load Supply Air Flow Rate Ratio",
        "  AutoSize,                       !- Heating Speed 1 Supply Air Flow Ratio",
        "  AutoSize,                       !- Cooling Speed 1 Supply Air Flow Ratio",
        "  AutoSize,                       !- Heating Speed 2 Supply Air Flow Ratio",
        "  AutoSize;                       !- Cooling Speed 2 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  AutoSize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Cooling Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Cooling:DX:MultiSpeed,",
        "  DX Cooling Coil,                !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Cooling Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  ,                               !- Condenser Air Inlet Node Name",
        "  AirCooled,                      !- Condenser Type",
        "  ,                               !- Minimum Outdoor Dry - Bulb Temperature for Compressor Operation{ C }",
        "  ,                               !- Supply Water Storage Tank Name",
        "  ,                               !- Condensate Collection Water Storage Tank Name",
        "  No,                             !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No,                             !- Apply Latent Degradation to Speeds Greater than 1",
        "  0,                              !- Crankcase Heater Capacity{ W }",
        "  10,                             !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  0,                              !- Basin Heater Capacity{ W / K }",
        "  2,                              !- Basin Heater Setpoint Temperature{ C }",
        "  ,                               !- Basin Heater Operating Schedule Name",
        "  Electricity,                    !- Fuel Type",
        "  2,                              !- Number of Speeds",
        "  AutoSize,                       !- Speed 1 Gross Rated Total Cooling Capacity{ W }",
        "  AutoSize,                       !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  5.12895662368113,               !- Speed 1 Gross Rated Cooling COP{ W / W }",
        "  AutoSize,                       !- Speed 1 Rated Air Flow Rate{ m3 / s }",
        "  773.3,                          !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  Biquadratic,                    !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Quadratic,                      !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  0,                              !- Speed 1 Nominal Time for Condensate Removal to Begin{ s }",
        "  0,                              !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity{ dimensionless }",
        "  0,                              !- Speed 1 Maximum Cycling Rate{ cycles / hr }",
        "  0,                              !- Speed 1 Latent Capacity Time Constant{ s }",
        "  0.5,                            !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  Biquadratic,                    !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  0.9,                            !- Speed 1 Evaporative Condenser Effectiveness{ dimensionless }",
        "  AutoSize,                       !- Speed 1 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  AutoSize,                       !- Speed 1 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  AutoSize,                       !- Speed 2 Gross Rated Total Cooling Capacity{ W }",
        "  AutoSize,                       !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  4.68933177022274,               !- Speed 2 Gross Rated Cooling COP{ W / W }",
        "  AutoSize,                       !- Speed 2 Rated Air Flow Rate{ m3 / s }",
        "  773.3,                          !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  Biquadratic,                    !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Quadratic,                      !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  0,                              !- Speed 2 Nominal Time for Condensate Removal to Begin{ s }",
        "  0,                              !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  0,                              !- Speed 2 Maximum Cycling Rate{ cycles / hr }",
        "  0,                              !- Speed 2 Latent Capacity Time Constant{ s }",
        "  0.5,                            !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  Biquadratic,                    !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  0.9,                            !- Speed 2 Evaporative Condenser Effectiveness{ dimensionless }",
        "  AutoSize,                       !- Speed 2 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  AutoSize;                       !- Speed 2 Rated Evaporative Condenser Pump Power Consumption{ W }",

        "ScheduleTypeLimits,",
        "  Any Number;                     !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,                      !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 1.0;              !- Field 3",

        "Schedule:Compact,",
        "  Always 20C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 20.0;             !- Field 3",

        "SetpointManager:Scheduled,",
        "  Cooling Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 20C,                     !- Schedule Name",
        "  Zone 2 Inlet Node;              !- Setpoint Node or NodeList Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  0.8,                            !- Coefficient1 Constant",
        "  0.2,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.5,                            !- Minimum Value of x",
        "  1.5;                            !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Biquadratic,                    !- Name",
        "  0.942587793,                    !- Coefficient1 Constant",
        "  0.009543347,                    !- Coefficient2 x",
        "  0.000683770,                    !- Coefficient3 x**2",
        "  -0.011042676,                   !- Coefficient4 y",
        "  0.000005249,                    !- Coefficient5 y**2",
        "  -0.000009720,                   !- Coefficient6 x*y",
        "  12.77778,                       !- Minimum Value of x",
        "  23.88889,                       !- Maximum Value of x",
        "  18.0,                           !- Minimum Value of y",
        "  46.11111,                       !- Maximum Value of y",
        "  ,                               !- Minimum Curve Output",
        "  ,                               !- Maximum Curve Output",
        "  Temperature,                    !- Input Unit Type for X",
        "  Temperature,                    !- Input Unit Type for Y",
        "  Dimensionless;                  !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    // call the UnitarySystem factory
    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above

    // verify the size of the vector and the processed names
    // 1 UnitarySystem objects
    EXPECT_EQ(1u, unitarySys.size());

    // test the object name
    EXPECT_EQ(compName, thisSys->Name);

    OutputReportPredefined::SetPredefinedTables();

    ScheduleManager::ProcessScheduleInput(state); // read schedules

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node = 1
    DataLoopNode::Node(1).MassFlowRate = 1.0;
    DataLoopNode::Node(1).MassFlowRateMaxAvail = 1.0;
    // DataLoopNode::Node(1).MassFlowRate = thisSys->designMassFlowRate;
    // DataLoopNode::Node(1).MassFlowRateMaxAvail = thisSys->designMassFlowRate; // max avail at fan inlet so fan won't limit flow

    // test COOLING condition
    DataLoopNode::Node(1).Temp = 24.0;         // 24C db
    DataLoopNode::Node(1).HumRat = 0.00922;    // 17C wb
    DataLoopNode::Node(1).Enthalpy = 47597.03; // www.sugartech.com/psychro/index.php

    // Cooling coil air inlet node = 3
    DataLoopNode::Node(3).MassFlowRateMax = 1.0; // max at fan outlet so fan won't limit flow
    // DataLoopNode::Node(3).MassFlowRateMax = thisSys->designMassFlowRate; // max at fan outlet so fan won't limit flow
    // Cooling coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 17.0;

    // test calling the sim routine
    int AirLoopNum = 0;
    int CompIndex = 0; // zero based index
    bool HeatingActive = false;
    bool CoolingActive = false;
    int OAUnitNum = 0;
    Real64 OAUCoilOutTemp = 0.0;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;
    DataGlobals::SysSizingCalc = false; // permits unitary system sizing
    EXPECT_EQ(compName, thisSys->Name);
    // simulate function is overloaded, but only to report back SysOutputProvided and LatOutputProvided. Either signature should give same result.
    thisSys->simulate(state,
                      compName,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatingActive,
                      CoolingActive,
                      OAUnitNum,
                      OAUCoilOutTemp,
                      zoneEquipment,
                      sensOut,
                      latOut);
    EXPECT_EQ(compName, thisSys->Name);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_TwoSpeedDXCoolCoil_Only)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  ,                               !- Heating Coil Object Type",
        "  ,                               !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:TwoSpeed,       !- Cooling Coil Object Type",
        "  DX Cooling Coil,                !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0;                           !- Maximum Supply Air Temperature{ C }",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Cooling Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Cooling:DX:TwoSpeed,",
        "  DX Cooling Coil,                !- Name",
        "  ,                               !- Availability Schedule Name",
        "  autosize,                       !- High Speed Gross Rated Total Cooling Capacity{ W }",
        "  0.8,                            !- High Speed Rated Sensible Heat Ratio",
        "  3.0,                            !- High Speed Gross Rated Cooling COP{ W / W }",
        " autosize,                        !- High Speed Rated Air Flow Rate{ m3 / s }",
        " 450,                             !- Unit Internal Static Air Pressure{ Pa }",
        " Cooling Coil Air Inlet Node,     !- Air Inlet Node Name",
        " Zone 2 Inlet Node,               !- Air Outlet Node Name",
        " Biquadratic,                     !- Total Cooling Capacity Function of Temperature Curve Name",
        " Quadratic,                       !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        " Biquadratic,                     !- Energy Input Ratio Function of Temperature Curve Name",
        " Quadratic,                       !- Energy Input Ratio Function of Flow Fraction Curve Name",
        " Quadratic,                       !- Part Load Fraction Correlation Curve Name",
        " autosize,                        !- Low Speed Gross Rated Total Cooling Capacity{ W }",
        " 0.8,                             !- Low Speed Gross Rated Sensible Heat Ratio",
        " 4.2,                             !- Low Speed Gross Rated Cooling COP{ W / W }",
        " autosize,                        !- Low Speed Rated Air Flow Rate{ m3 / s }",
        " Biquadratic,                     !- Low Speed Total Cooling Capacity Function of Temperature Curve Name",
        " Biquadratic,                     !- Low Speed Energy Input Ratio Function of Temperature Curve Name",
        " ,                                !- Condenser Air Inlet Node Name",
        " EvaporativelyCooled; !- Condenser Type",

        "ScheduleTypeLimits,",
        "  Any Number;                     !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,                      !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 1.0;              !- Field 3",

        "Schedule:Compact,",
        "  Always 20C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 20.0;             !- Field 3",

        "SetpointManager:Scheduled,",
        "  Cooling Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 20C,                     !- Schedule Name",
        "  Zone 2 Inlet Node;              !- Setpoint Node or NodeList Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  0.8,                            !- Coefficient1 Constant",
        "  0.2,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.5,                            !- Minimum Value of x",
        "  1.5;                            !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Biquadratic,                    !- Name",
        "  0.942587793,                    !- Coefficient1 Constant",
        "  0.009543347,                    !- Coefficient2 x",
        "  0.000683770,                    !- Coefficient3 x**2",
        "  -0.011042676,                   !- Coefficient4 y",
        "  0.000005249,                    !- Coefficient5 y**2",
        "  -0.000009720,                   !- Coefficient6 x*y",
        "  12.77778,                       !- Minimum Value of x",
        "  23.88889,                       !- Maximum Value of x",
        "  18.0,                           !- Minimum Value of y",
        "  46.11111,                       !- Maximum Value of y",
        "  ,                               !- Minimum Curve Output",
        "  ,                               !- Maximum Curve Output",
        "  Temperature,                    !- Input Unit Type for X",
        "  Temperature,                    !- Input Unit Type for Y",
        "  Dimensionless;                  !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    // call the UnitarySystem factory
    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    OutputReportPredefined::SetPredefinedTables();

    // UnitarySystem used as zone equipment will not be modeled when FirstHAVCIteration is true, first time FirstHVACIteration = false will disable
    // the 'return' on FirstHVACIteration = true set FirstHVACIteration to false for unit testing to size water coils
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;

    // overwrite outdoor weather temp to variable speed coil rated water temp until this gets fixed
    DataSizing::DesDayWeath(1).Temp(1) = 29.4;

    // test #6274 where coil inlet air flow rate was non-zero prior to sizing
    // this simulates another UnitarySystem upstream of this UnitarySystem that ran before this system coil was sized (and placed a non-zero air flow
    // rate on this system's inlet node)
    DataLoopNode::Node(thisSys->CoolCoilInletNodeNum).MassFlowRate = 0.05;

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node = 1
    DataLoopNode::Node(1).MassFlowRate = thisSys->m_DesignMassFlowRate;
    DataLoopNode::Node(1).MassFlowRateMaxAvail = thisSys->m_DesignMassFlowRate; // max avail at fan inlet so fan won't limit flow

    // test COOLING condition
    DataLoopNode::Node(1).Temp = 24.0;         // 24C db
    DataLoopNode::Node(1).HumRat = 0.00922;    // 17C wb
    DataLoopNode::Node(1).Enthalpy = 47597.03; // www.sugartech.com/psychro/index.php

    ScheduleManager::ProcessScheduleInput(state); // read schedules

    // Cooling coil air inlet node = 3
    DataLoopNode::Node(3).MassFlowRateMax = thisSys->m_DesignMassFlowRate; // max at fan outlet so fan won't limit flow
    // Cooling coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 17.0;

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // COOLING mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that cooling coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    // cooling coil air inlet node temp is greater than cooling coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(3).Temp, DataLoopNode::Node(2).Temp);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_MultiSpeedDXCoolCoil_Only)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  ,                               !- Heating Coil Object Type",
        "  ,                               !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:MultiSpeed,     !- Cooling Coil Object Type",
        "  DX Cooling Coil,                !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,                     !- Design Specification Multispeed Object Type",
        "  DX Cool MultiSpd Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  DX Cool MultiSpd Unitary System MultiSpeed Performance,  !- Name",
        "  1,                              !- Number of Speeds for Heating",
        "  2,                              !- Number of Speeds for Cooling",
        "  No,                             !- Single Mode Operation",
        "  ,                               !- No Load Supply Air Flow Rate Ratio",
        "  1,                              !- Heating Speed 1 Supply Air Flow Ratio",
        "  1,                              !- Cooling Speed 1 Supply Air Flow Ratio",
        "  Autosize,                       !- Heating Speed 2 Supply Air Flow Ratio",
        "  Autosize;                       !- Cooling Speed 2 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Cooling Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Cooling:DX:MultiSpeed,",
        "  DX Cooling Coil,                !- Name",
        "  ,                               !- Availability Schedule Name",
        "  Cooling Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  ,                               !- Condenser Air Inlet Node Name",
        "  AirCooled,                      !- Condenser Type",
        "  ,                               !- Minimum Outdoor Dry - Bulb Temperature for Compressor Operation{ C }",
        "  ,                               !- Supply Water Storage Tank Name",
        "  ,                               !- Condensate Collection Water Storage Tank Name",
        "  No,                             !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No,                             !- Apply Latent Degradation to Speeds Greater than 1",
        "  0,                              !- Crankcase Heater Capacity{ W }",
        "  10,                             !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  0,                              !- Basin Heater Capacity{ W / K }",
        "  2,                              !- Basin Heater Setpoint Temperature{ C }",
        "  ,                               !- Basin Heater Operating Schedule Name",
        "  Electricity,                    !- Fuel Type",
        "  2,                              !- Number of Speeds",
        "  AutoSize,                       !- Speed 1 Gross Rated Total Cooling Capacity{ W }",
        "  AutoSize,                       !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  5.12895662368113,               !- Speed 1 Gross Rated Cooling COP{ W / W }",
        "  AutoSize,                       !- Speed 1 Rated Air Flow Rate{ m3 / s }",
        "  773.3,                          !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  Biquadratic,                    !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Quadratic,                      !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  0,                              !- Speed 1 Nominal Time for Condensate Removal to Begin{ s }",
        "  0,                              !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity{ dimensionless }",
        "  0,                              !- Speed 1 Maximum Cycling Rate{ cycles / hr }",
        "  0,                              !- Speed 1 Latent Capacity Time Constant{ s }",
        "  0.5,                            !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  Biquadratic,                    !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  0.9,                            !- Speed 1 Evaporative Condenser Effectiveness{ dimensionless }",
        "  AutoSize,                       !- Speed 1 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  AutoSize,                       !- Speed 1 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  AutoSize,                       !- Speed 2 Gross Rated Total Cooling Capacity{ W }",
        "  AutoSize,                       !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  4.68933177022274,               !- Speed 2 Gross Rated Cooling COP{ W / W }",
        "  AutoSize,                       !- Speed 2 Rated Air Flow Rate{ m3 / s }",
        "  773.3,                          !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  Biquadratic,                    !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Quadratic,                      !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  0,                              !- Speed 2 Nominal Time for Condensate Removal to Begin{ s }",
        "  0,                              !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  0,                              !- Speed 2 Maximum Cycling Rate{ cycles / hr }",
        "  0,                              !- Speed 2 Latent Capacity Time Constant{ s }",
        "  0.5,                            !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  Biquadratic,                    !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  0.9,                            !- Speed 2 Evaporative Condenser Effectiveness{ dimensionless }",
        "  AutoSize,                       !- Speed 2 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  AutoSize;                       !- Speed 2 Rated Evaporative Condenser Pump Power Consumption{ W }",

        "ScheduleTypeLimits,",
        "  Any Number;                     !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,                      !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 1.0;              !- Field 3",

        "Schedule:Compact,",
        "  Always 20C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 20.0;             !- Field 3",

        "SetpointManager:Scheduled,",
        "  Cooling Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 20C,                     !- Schedule Name",
        "  Zone 2 Inlet Node;              !- Setpoint Node or NodeList Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  0.8,                            !- Coefficient1 Constant",
        "  0.2,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.5,                            !- Minimum Value of x",
        "  1.5;                            !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Biquadratic,                    !- Name",
        "  0.942587793,                    !- Coefficient1 Constant",
        "  0.009543347,                    !- Coefficient2 x",
        "  0.000683770,                    !- Coefficient3 x**2",
        "  -0.011042676,                   !- Coefficient4 y",
        "  0.000005249,                    !- Coefficient5 y**2",
        "  -0.000009720,                   !- Coefficient6 x*y",
        "  12.77778,                       !- Minimum Value of x",
        "  23.88889,                       !- Maximum Value of x",
        "  18.0,                           !- Minimum Value of y",
        "  46.11111,                       !- Maximum Value of y",
        "  ,                               !- Minimum Curve Output",
        "  ,                               !- Maximum Curve Output",
        "  Temperature,                    !- Input Unit Type for X",
        "  Temperature,                    !- Input Unit Type for Y",
        "  Dimensionless;                  !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    // call the UnitarySystem factory
    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    OutputReportPredefined::SetPredefinedTables();

    // UnitarySystem used as zone equipment will not be modeled when FirstHAVCIteration is true, first time FirstHVACIteration = false will disable
    // the 'return' on FirstHVACIteration = true set FirstHVACIteration to false for unit testing to size water coils
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;

    // overwrite outdoor weather temp to variable speed coil rated water temp until this gets fixed
    DataSizing::DesDayWeath(1).Temp(1) = 29.4;

    // test #6274 where coil inlet air flow rate was non-zero prior to sizing
    // this simulates another UnitarySystem upstream of this UnitarySystem that ran before this system coil was sized (and placed a non-zero air flow
    // rate on this system's inlet node)
    DataLoopNode::Node(thisSys->CoolCoilInletNodeNum).MassFlowRate = 0.05;

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node = 1
    DataLoopNode::Node(1).MassFlowRate = thisSys->m_DesignMassFlowRate;
    DataLoopNode::Node(1).MassFlowRateMaxAvail = thisSys->m_DesignMassFlowRate; // max avail at fan inlet so fan won't limit flow

    // test COOLING condition
    DataLoopNode::Node(1).Temp = 24.0;         // 24C db
    DataLoopNode::Node(1).HumRat = 0.00922;    // 17C wb
    DataLoopNode::Node(1).Enthalpy = 47597.03; // www.sugartech.com/psychro/index.php

    ScheduleManager::ProcessScheduleInput(state); // read schedules

    // Cooling coil air inlet node = 3
    DataLoopNode::Node(3).MassFlowRateMax = thisSys->m_DesignMassFlowRate; // max at fan outlet so fan won't limit flow
                                                                           // Cooling coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 17.0;

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // COOLING mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that cooling coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    // cooling coil air inlet node temp is greater than cooling coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(3).Temp, DataLoopNode::Node(2).Temp);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_MultiStageGasHeatCoil_Only)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Gas:MultiStage,    !- Heating Coil Object Type",
        "  Gas Heating Coil,               !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  ,                               !- Cooling Coil Object Type",
        "  ,                               !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,                     !- Design Specification Multispeed Object Type",
        "  DX Heat MultiSpd Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  DX Heat MultiSpd Unitary System MultiSpeed Performance,  !- Name",
        "  2,                              !- Number of Speeds for Heating",
        "  1,                              !- Number of Speeds for Cooling",
        "  No,                             !- Single Mode Operation",
        "  0,                              !- No Load Supply Air Flow Rate Ratio",
        "  1,                              !- Heating Speed 1 Supply Air Flow Ratio",
        "  1,                              !- Cooling Speed 1 Supply Air Flow Ratio",
        "  1,                              !- Heating Speed 2 Supply Air Flow Ratio",
        "  1;                              !- Cooling Speed 2 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Heating:Gas:MultiStage,",
        "  Gas Heating Coil,               !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Heating Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  ,                               !- Temperature Setpoint Node Name",
        "  Quadratic,                      !- Part Load Fraction Correlation Curve Name",
        "  10,                             !- Parasitic Gas Load{ W }",
        "  2,                              !- Number of Stages",
        "  0.8,                            !- Stage 1 Gas Burner Efficiency{ W / W }",
        "  7689.33,                        !- Stage 1 Nominal Capacity{ W }",
        "  100,                            !- Stage 1 Parasitic Electric Load{ W }",
        "  0.8,                            !- Stage 2 Gas Burner Efficiency{ W / W }",
        "  15378.66,                       !- Stage 2 Nominal Capacity{ W }",
        "  100;                            !- Stage 2 Parasitic Electric Load{ W }",

        "ScheduleTypeLimits,",
        "  Any Number;                     !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,                      !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 1.0;              !- Field 3",

        "Schedule:Compact,",
        "  Always 18C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 18.0;             !- Field 3",

        "SetpointManager:Scheduled,",
        "  Heating Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 18C,                     !- Schedule Name",
        "  Zone 2 Inlet Node;              !- Setpoint Node or NodeList Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  0.8,                            !- Coefficient1 Constant",
        "  0.2,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.5,                            !- Minimum Value of x",
        "  1.5;                            !- Maximum Value of x",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    OutputReportPredefined::SetPredefinedTables();

    // UnitarySystem used as zone equipment will not be modeled when FirstHAVCIteration is true, first time FirstHVACIteration = false will disable
    // the 'return' on FirstHVACIteration = true set FirstHVACIteration to false for unit testing to size water coils
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node = 1
    DataLoopNode::Node(1).MassFlowRate = thisSys->m_DesignMassFlowRate;
    DataLoopNode::Node(1).MassFlowRateMaxAvail = thisSys->m_DesignMassFlowRate; // max avail at fan inlet so fan won't limit flow

    // test HEATING condition
    DataLoopNode::Node(1).Temp = 24.0;         // 24C db
    DataLoopNode::Node(1).HumRat = 0.00922;    // 17C wb
    DataLoopNode::Node(1).Enthalpy = 47597.03; // www.sugartech.com/psychro/index.php

    ScheduleManager::ProcessScheduleInput(state); // read schedules

    // Heating coil air inlet node = 3
    DataLoopNode::Node(3).MassFlowRateMax = thisSys->m_DesignMassFlowRate; // max at fan outlet so fan won't limit flow
                                                                           // Heating coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 25.0;

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // Heating mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that heating coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    // heating coil air inlet node temp is less than heating coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(2).Temp, DataLoopNode::Node(3).Temp);
    // no load air flow rate in UnitarySystemPerformance:Multispeed equals 0
    EXPECT_EQ(0.0, thisSys->m_IdleMassFlowRate);
    // make sure control works at speed = 1
    EXPECT_EQ(thisSys->m_HeatingSpeedNum, 1);

    // Heating coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 34.0;

    // Heating mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that heating coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    // heating coil air inlet node temp is less than heating coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(2).Temp, DataLoopNode::Node(3).Temp);
    // make sure control works at speed = 2
    EXPECT_EQ(thisSys->m_HeatingSpeedNum, 2);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_MultiStageElecHeatCoil_Only)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Electric:MultiStage,    !- Heating Coil Object Type",
        "  Electric Heating Coil,          !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  ,                               !- Cooling Coil Object Type",
        "  ,                               !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,                     !- Design Specification Multispeed Object Type",
        "  DX Heat MultiSpd Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  DX Heat MultiSpd Unitary System MultiSpeed Performance,  !- Name",
        "  2,                              !- Number of Speeds for Heating",
        "  1,                              !- Number of Speeds for Cooling",
        "  No,                             !- Single Mode Operation",
        "  0,                              !- No Load Supply Air Flow Rate Ratio",
        "  1,                              !- Heating Speed 1 Supply Air Flow Ratio",
        "  1,                              !- Cooling Speed 1 Supply Air Flow Ratio",
        "  1,                              !- Heating Speed 2 Supply Air Flow Ratio",
        "  1;                              !- Cooling Speed 2 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Heating:Electric:MultiStage,",
        "  Electric Heating Coil,               !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Heating Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  ,                               !- Temperature Setpoint Node Name",
        "  2,                              !- Number of Stages",
        "  1.0,                            !- Stage 1 Efficiency",
        "  autosize,                       !- Stage 1 Nominal Capacity",
        "  1.0,                            !- Stage 2 Efficency",
        "  autosize;                       !- Stage 2 Nominal Capacity",

        "ScheduleTypeLimits,",
        "  Any Number;                     !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,                      !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 1.0;              !- Field 3",

        "Schedule:Compact,",
        "  Always 18C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 18.0;             !- Field 3",

        "SetpointManager:Scheduled,",
        "  Heating Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 18C,                     !- Schedule Name",
        "  Zone 2 Inlet Node;              !- Setpoint Node or NodeList Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  0.8,                            !- Coefficient1 Constant",
        "  0.2,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.5,                            !- Minimum Value of x",
        "  1.5;                            !- Maximum Value of x",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    OutputReportPredefined::SetPredefinedTables();

    // UnitarySystem used as zone equipment will not be modeled when FirstHAVCIteration is true, first time FirstHVACIteration = false will disable
    // the 'return' on FirstHVACIteration = true set FirstHVACIteration to false for unit testing to size water coils
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // expect no output since mass flow rate is 0 here since it's not defined yet
    EXPECT_EQ(0.0, sensOut);
    EXPECT_EQ(0.0, thisSys->m_SensibleLoadMet);

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node = 1
    DataLoopNode::Node(1).MassFlowRate = thisSys->m_DesignMassFlowRate;
    DataLoopNode::Node(1).MassFlowRateMaxAvail = thisSys->m_DesignMassFlowRate; // max avail at fan inlet so fan won't limit flow

    // test HEATING condition
    DataLoopNode::Node(1).Temp = 24.0;         // 24C db
    DataLoopNode::Node(1).HumRat = 0.00922;    // 17C wb
    DataLoopNode::Node(1).Enthalpy = 47597.03; // www.sugartech.com/psychro/index.php

    ScheduleManager::ProcessScheduleInput(state); // read schedules

    // Heating coil air inlet node = 3
    DataLoopNode::Node(3).MassFlowRateMax = thisSys->m_DesignMassFlowRate; // max at fan outlet so fan won't limit flow
                                                                           // Heating coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 25.0;

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // Heating mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that heating coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    // heating coil air inlet node temp is less than heating coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(2).Temp, DataLoopNode::Node(3).Temp);
    // no load air flow rate in UnitarySystemPerformance:Multispeed equals 0
    EXPECT_EQ(0.0, thisSys->m_IdleMassFlowRate);
    // make sure control works at speed = 1
    EXPECT_EQ(thisSys->m_HeatingSpeedNum, 1);

    // expect heating output
    EXPECT_GT(sensOut, 0.0);
    EXPECT_GT(thisSys->m_SensibleLoadMet, 0.0);
    EXPECT_GT(thisSys->m_SensHeatEnergyRate, 0.0);
    EXPECT_EQ(sensOut, thisSys->m_SensibleLoadMet);
    EXPECT_EQ(thisSys->m_SensHeatEnergyRate, thisSys->m_SensibleLoadMet);

    // Heating coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 34.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that heating coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    // heating coil air inlet node temp is less than heating coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(2).Temp, DataLoopNode::Node(3).Temp);
    // make sure control works at speed = 2
    EXPECT_EQ(thisSys->m_HeatingSpeedNum, 2);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_ElecHeatCoil_Only)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Electric,          !- Heating Coil Object Type",
        "  Electric Heating Coil,          !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  ,                               !- Cooling Coil Object Type",
        "  ,                               !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  ,                               !- Design Specification Multispeed Object Type",
        "  ;                               !- Design Specification Multispeed Object Name",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Heating:Electric,",
        "  Electric Heating Coil,               !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  1.0,                            !- Efficiency",
        "  autosize,                       !- Nominal Capacity",
        "  Heating Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  ;                               !- Temperature Setpoint Node Name",

        "ScheduleTypeLimits,",
        "  Any Number;                     !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,                      !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 1.0;              !- Field 3",

        "Schedule:Compact,",
        "  Always 18C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 18.0;             !- Field 3",

        "SetpointManager:Scheduled,",
        "  Heating Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 18C,                     !- Schedule Name",
        "  Zone 2 Inlet Node;              !- Setpoint Node or NodeList Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  0.8,                            !- Coefficient1 Constant",
        "  0.2,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.5,                            !- Minimum Value of x",
        "  1.5;                            !- Maximum Value of x",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    OutputReportPredefined::SetPredefinedTables();

    // UnitarySystem used as zone equipment will not be modeled when FirstHAVCIteration is true, first time FirstHVACIteration = false will disable
    // the 'return' on FirstHVACIteration = true set FirstHVACIteration to false for unit testing to size water coils
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node = 1
    DataLoopNode::Node(1).MassFlowRate = thisSys->m_DesignMassFlowRate;
    DataLoopNode::Node(1).MassFlowRateMaxAvail = thisSys->m_DesignMassFlowRate; // max avail at fan inlet so fan won't limit flow

    // test HEATING condition
    DataLoopNode::Node(1).Temp = 24.0;         // 24C db
    DataLoopNode::Node(1).HumRat = 0.00922;    // 17C wb
    DataLoopNode::Node(1).Enthalpy = 47597.03; // www.sugartech.com/psychro/index.php

    ScheduleManager::ProcessScheduleInput(state); // read schedules

    // Heating coil air inlet node = 3
    DataLoopNode::Node(3).MassFlowRateMax = thisSys->m_DesignMassFlowRate; // max at fan outlet so fan won't limit flow
                                                                           // Heating coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 25.0;

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // Heating mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that heating coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    // heating coil air inlet node temp is less than heating coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(2).Temp, DataLoopNode::Node(3).Temp);
    // #6282 idle air flow rate for electric heating coils should equal 0
    EXPECT_EQ(0.0, thisSys->m_IdleMassFlowRate);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_MultiStageGasHeatCoil_Only_ContFan)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Gas:MultiStage,    !- Heating Coil Object Type",
        "  Gas Heating Coil,               !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  ,                               !- Cooling Coil Object Type",
        "  ,                               !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,                     !- Design Specification Multispeed Object Type",
        "  DX Heat MultiSpd Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  DX Heat MultiSpd Unitary System MultiSpeed Performance,  !- Name",
        "  2,                              !- Number of Speeds for Heating",
        "  1,                              !- Number of Speeds for Cooling",
        "  No,                             !- Single Mode Operation",
        "  ,                               !- No Load Supply Air Flow Rate Ratio",
        "  1,                              !- Heating Speed 1 Supply Air Flow Ratio",
        "  1,                              !- Cooling Speed 1 Supply Air Flow Ratio",
        "  1,                              !- Heating Speed 2 Supply Air Flow Ratio",
        "  1;                              !- Cooling Speed 2 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Heating:Gas:MultiStage,",
        "  Gas Heating Coil,               !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Heating Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  ,                               !- Temperature Setpoint Node Name",
        "  Quadratic,                      !- Part Load Fraction Correlation Curve Name",
        "  10,                             !- Parasitic Gas Load{ W }",
        "  2,                              !- Number of Stages",
        "  0.8,                            !- Stage 1 Gas Burner Efficiency{ W / W }",
        "  7689.33,                        !- Stage 1 Nominal Capacity{ W }",
        "  100,                            !- Stage 1 Parasitic Electric Load{ W }",
        "  0.8,                            !- Stage 2 Gas Burner Efficiency{ W / W }",
        "  15378.66,                       !- Stage 2 Nominal Capacity{ W }",
        "  100;                            !- Stage 2 Parasitic Electric Load{ W }",

        "ScheduleTypeLimits,",
        "  Any Number;                     !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,                      !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 1.0;              !- Field 3",

        "Schedule:Compact,",
        "  Always 18C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 18.0;             !- Field 3",

        "SetpointManager:Scheduled,",
        "  Heating Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 18C,                     !- Schedule Name",
        "  Zone 2 Inlet Node;              !- Setpoint Node or NodeList Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  0.8,                            !- Coefficient1 Constant",
        "  0.2,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.5,                            !- Minimum Value of x",
        "  1.5;                            !- Maximum Value of x",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    OutputReportPredefined::SetPredefinedTables();

    // UnitarySystem used as zone equipment will not be modeled when FirstHAVCIteration is true, first time FirstHVACIteration = false will disable
    // the 'return' on FirstHVACIteration = true set FirstHVACIteration to false for unit testing to size water coils
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node = 1
    DataLoopNode::Node(1).MassFlowRate = thisSys->m_DesignMassFlowRate;
    DataLoopNode::Node(1).MassFlowRateMaxAvail = thisSys->m_DesignMassFlowRate; // max avail at fan inlet so fan won't limit flow

    // test HEATING condition
    DataLoopNode::Node(1).Temp = 24.0;         // 24C db
    DataLoopNode::Node(1).HumRat = 0.00922;    // 17C wb
    DataLoopNode::Node(1).Enthalpy = 47597.03; // www.sugartech.com/psychro/index.php

    ScheduleManager::ProcessScheduleInput(state); // read schedules

    // Heating coil air inlet node = 3
    DataLoopNode::Node(3).MassFlowRateMax = thisSys->m_DesignMassFlowRate; // max at fan outlet so fan won't limit flow
                                                                           // Heating coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 25.0;

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // Heating mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that heating coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    // heating coil air inlet node temp is less than heating coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(2).Temp, DataLoopNode::Node(3).Temp);
    // no load air flow rate in UnitarySystemPerformance:Multispeed is blank (DS no load flow ratio defaults to 1) so idle mass flow rate = speed 1
    // heating flow
    EXPECT_EQ(thisSys->m_HeatMassFlowRate[0], thisSys->m_IdleMassFlowRate);
    // make sure control works at speed = 1
    EXPECT_EQ(thisSys->m_HeatingSpeedNum, 1);

    // Heating coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 34.0;

    // Heating mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that heating coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    // heating coil air inlet node temp is less than heating coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(2).Temp, DataLoopNode::Node(3).Temp);
    // make sure control works at speed = 2
    EXPECT_EQ(thisSys->m_HeatingSpeedNum, 2);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_MultispeedPerformance)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  AlwaysOne,                      !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:DX:VariableSpeed,  !- Heating Coil Object Type",
        "  DX Heating Coil,                !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:VariableSpeed,  !- Cooling Coil Object Type",
        "  DX Cooling Coil,                !- Cooling Coil Name",
        "  ,                               !- Use DOAS DX Cooling Coil",
        "  15.0,                           !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                               !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,              !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  35.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,                     !- Design Specification Multispeed Object Type",
        "  DX Cool MultiSpd Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  DX Cool MultiSpd Unitary System MultiSpeed Performance,  !- Name",
        "  10,                             !- Number of Speeds for Heating",
        "  10,                             !- Number of Speeds for Cooling",
        "  No,                             !- Single Mode Operation",
        "  0.05,                           !- No Load Supply Air Flow Rate Ratio",
        "  0.101,                          !- Heating Speed 1 Supply Air Flow Ratio",
        "  0.1,                            !- Cooling Speed 1 Supply Air Flow Ratio",
        "  0.201,                          !- Heating Speed 2 Supply Air Flow Ratio",
        "  0.2,                            !- Cooling Speed 2 Supply Air Flow Ratio",
        "  0.301,                          !- Heating Speed 3 Supply Air Flow Ratio",
        "  0.3,                            !- Cooling Speed 3 Supply Air Flow Ratio",
        "  0.401,                          !- Heating Speed 4 Supply Air Flow Ratio",
        "  0.4,                            !- Cooling Speed 4 Supply Air Flow Ratio",
        "  0.501,                          !- Heating Speed 5 Supply Air Flow Ratio",
        "  0.5,                            !- Cooling Speed 5 Supply Air Flow Ratio",
        "  0.601,                          !- Heating Speed 6 Supply Air Flow Ratio",
        "  0.6,                            !- Cooling Speed 6 Supply Air Flow Ratio",
        "  0.701,                          !- Heating Speed 7 Supply Air Flow Ratio",
        "  0.7,                            !- Cooling Speed 7 Supply Air Flow Ratio",
        "  0.801,                          !- Heating Speed 8 Supply Air Flow Ratio",
        "  0.8,                            !- Cooling Speed 8 Supply Air Flow Ratio",
        "  0.901,                          !- Heating Speed 9 Supply Air Flow Ratio",
        "  0.9,                            !- Cooling Speed 9 Supply Air Flow Ratio",
        "  1.0,                            !- Heating Speed 10 Supply Air Flow Ratio",
        "  1.0;                            !- Cooling Speed 10 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Cooling Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Cooling:DX:VariableSpeed,",
        "  DX Cooling Coil,                !- Name",
        "  Cooling Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,    !- Air Outlet Node Name",
        "  10.0,                           !- Number of Speeds{ dimensionless }",
        "  10.0,                           !- Nominal Speed Level{ dimensionless }",
        "  autosize,                       !- Gross Rated Total Cooling Capacity At Selected Nominal Speed Level{ w }",
        "  autosize,                       !- Rated Air Flow Rate At Selected Nominal Speed Level{ m3 / s }",
        "  0.0,                            !- Nominal Time for Condensate to Begin Leaving the Coil{ s }",
        "  0.0,                            !- Initial Moisture Evaporation Rate Divided by Steady - State AC Latent Capacity{ dimensionless }",
        "  Quadratic,                      !- Energy Part Load Fraction Curve Name",
        "  ,                               !- Condenser Air Inlet Node Name",
        "  AirCooled,                      !- Condenser Type",
        "  ,                               !- Evaporative Condenser Pump Rated Power Consumption{ W }",
        "  200.0,                          !- Crankcase Heater Capacity{ W }",
        "  10.0,                           !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  ,                               !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                               !- Supply Water Storage Tank Name",
        "  ,                               !- Condensate Collection Water Storage Tank Name",
        "  ,                               !- Basin Heater Capacity{ W / K }",
        "  ,                               !- Basin Heater Setpoint Temperature{ C }",
        "  ,                               !- Basin Heater Operating Schedule Name",
        "  1524.1,                         !- Speed 1 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 1 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 1 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.1359072,                      !- Speed 1 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.26,                           !- Speed 1 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 1 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  1877.9,                         !- Speed 2 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 2 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 2 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.151008,                       !- Speed 2 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.30,                           !- Speed 2 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 2 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2226.6,                         !- Speed 3 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 3 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 3 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.1661088,                      !- Speed 3 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.33,                           !- Speed 3 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 3 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2911.3,                         !- Speed 4 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 4 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 4 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.1963104,                      !- Speed 4 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.38,                           !- Speed 4 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 4 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  3581.7,                         !- Speed 5 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 5 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 5 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.226512,                       !- Speed 5 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.44,                           !- Speed 5 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 5 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  4239.5,                         !- Speed 6 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 6 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 6 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.2567136,                      !- Speed 6 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.50,                           !- Speed 6 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 6 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 6 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 6 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  4885.7,                         !- Speed 7 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 7 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 7 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.2869152,                      !- Speed 7 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.57,                           !- Speed 7 Reference Unit Condenser Flow Rate{ m3 / s }",
        "  ,                               !- Speed 7 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 7 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 7 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  5520.7,                         !- Speed 8 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 8 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 8 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.3171168,                      !- Speed 8 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.63,                           !- Speed 8 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 8 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 8 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 8 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  6144.8,                         !- Speed 9 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 9 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 9 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.3473184,                      !- Speed 9 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.69,                           !- Speed 9 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 9 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 9 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 9 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  6758.0,                         !- Speed 10 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 10 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 10 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.37752,                        !- Speed 10 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.74,                           !- Speed 10 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 10 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 10 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 10 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic;                      !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",

        "Coil:Heating:DX:VariableSpeed, ",
        "  DX Heating Coil,                !- Name",
        "  Heating Coil Air Inlet Node,    !- Indoor Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Indoor Air Outlet Node Name",
        "  10.0,                           !- Number of Speeds {dimensionless}",
        "  10.0,                           !- Nominal Speed Level {dimensionless}",
        "  autosize,                       !- Rated Heating Capacity At Selected Nominal Speed Level {w}",
        "  1.7,                            !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
        "  Quadratic,                      !- Energy Part Load Fraction Curve Name",
        "      ,                           !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "  -5.0,                           !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "  5.0,                            !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "  200.0,                          !- Crankcase Heater Capacity {W}",
        "  10.0,                           !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  Resistive,                      !- Defrost Strategy",
        "  TIMED,                          !- Defrost Control",
        "  0.166667,                       !- Defrost Time Period Fraction",
        "  20000,                          !- Resistive Defrost Heater Capacity {W}",
        "  1838.7,                         !- Speed 1 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 1 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.1661088,                      !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2295.5,                         !- Speed 2 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 2 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.179322,                       !- Speed 2 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2751.3,                         !- Speed 3 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 3 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.1925352,                      !- Speed 3 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  3659.6,                         !- Speed 4 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 4 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.2189616,                      !- Speed 4 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  4563.7,                         !- Speed 5 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 5 Reference Unit Gross Rated Heating COP {dimensionless}",
        "   0.245388,                      !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  5463.3,                         !- Speed 6 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 6 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.2718144,                      !- Speed 6 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 6 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 6 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  6358.4,                         !- Speed 7 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 7 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.2982408,                      !- Speed 7 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 7 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 7 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  7248.5,                         !- Speed 8 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 8 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.3246672,                      !- Speed 8 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 8 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 8 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  8133.6,                         !- Speed 9 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 9 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.3510936,                      !- Speed 9 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 9 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 9 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  9013.2,                         !- Speed 10 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 10 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.37752,                        !- Speed 10 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 10 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 10 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic;                      !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",

        "ScheduleTypeLimits,",
        "  Any Number;                     !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,                      !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 1.0;              !- Field 3",

        "Schedule:Compact,",
        "  Always 16C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 16.0;             !- Field 3",

        "Schedule:Compact,",
        "  Always 18C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 18.0;             !- Field 3",

        "Schedule:Compact,",
        "  Always 20C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 20.0;             !- Field 3",

        "SetpointManager:Scheduled,",
        "  Cooling Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 20C,                     !- Schedule Name",
        "  Heating Coil Air Inlet Node;    !- Setpoint Node or NodeList Name",

        "SetpointManager:Scheduled,",
        "  Heating Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 18C,                     !- Schedule Name",
        "  Zone 2 Inlet Node;              !- Setpoint Node or NodeList Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  0.8,                            !- Coefficient1 Constant",
        "  0.2,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.5,                            !- Minimum Value of x",
        "  1.5;                            !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Biquadratic,                    !- Name",
        "  0.942587793,                    !- Coefficient1 Constant",
        "  0.009543347,                    !- Coefficient2 x",
        "  0.000683770,                    !- Coefficient3 x**2",
        "  -0.011042676,                   !- Coefficient4 y",
        "  0.000005249,                    !- Coefficient5 y**2",
        "  -0.000009720,                   !- Coefficient6 x*y",
        "  12.77778,                       !- Minimum Value of x",
        "  23.88889,                       !- Maximum Value of x",
        "  18.0,                           !- Minimum Value of y",
        "  46.11111,                       !- Maximum Value of y",
        "  ,                               !- Minimum Curve Output",
        "  ,                               !- Maximum Curve Output",
        "  Temperature,                    !- Input Unit Type for X",
        "  Temperature,                    !- Input Unit Type for Y",
        "  Dimensionless;                  !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    // Verify UnitarySystem air flow rates are read in as AutoSized
    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxNoCoolHeatAirVolFlow, DataSizing::AutoSize);

    OutputReportPredefined::SetPredefinedTables();

    // UnitarySystem used as zone equipment will not be modeled when FirstHAVCIteration is true, first time FirstHVACIteration = false will disable
    // the 'return' on FirstHVACIteration = true set FirstHVACIteration to false for unit testing to size water coils
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;

    // sizing routine will overwrite water coil air and water inlet nodes with design conditions so no need set set up node conditions yet
    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node = 1
    DataLoopNode::Node(1).MassFlowRate = thisSys->m_DesignMassFlowRate;
    DataLoopNode::Node(1).MassFlowRateMaxAvail = thisSys->m_DesignMassFlowRate; // max avail at fan inlet so fan won't limit flow

    // test COOLING condition
    DataLoopNode::Node(1).Temp = 24.0;         // 24C db
    DataLoopNode::Node(1).HumRat = 0.00922;    // 17C wb
    DataLoopNode::Node(1).Enthalpy = 47597.03; // www.sugartech.com/psychro/index.php

    // Cooling coil air inlet node = 3
    DataLoopNode::Node(3).MassFlowRateMax = thisSys->m_DesignMassFlowRate; // max at fan outlet so fan won't limit flow
                                                                           // Cooling coil air outlet node = 4
    DataLoopNode::Node(4).TempSetPoint = 20.0;
    // Heating coil air inlet node = 4
    // Heating coil air outlet node = 2
    DataLoopNode::Node(2).TempSetPoint = 16.0;

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // COOLING mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that cooling coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(4).Temp, DataLoopNode::Node(4).TempSetPoint, 0.001);
    // cooling coil air inlet node temp is greater than cooling coil air outlet node temp
    EXPECT_GT(DataLoopNode::Node(3).Temp, DataLoopNode::Node(4).Temp);
    // heating coil air inlet and outlet nodes are at same temp since the heating coil is off
    EXPECT_EQ(DataLoopNode::Node(4).MassFlowRate, DataLoopNode::Node(2).MassFlowRate);
    // expect heating coil outlet air temp to be greater than heating coil outlet air temp set point
    EXPECT_GT(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint);

    // HEATING mode
    // Unitary system air inlet node = 1
    DataLoopNode::Node(1).Temp = 14.0;      // 14C db
    DataLoopNode::Node(1).HumRat = 0.00693; // 11C wb
    DataLoopNode::Node(1).Enthalpy = 31598.76;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // cooling coil air inlet node temp is equal to cooling coil air outlet node temp since cooling coil is off
    EXPECT_EQ(DataLoopNode::Node(3).Temp, DataLoopNode::Node(4).Temp);
    // check that heating coil outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).TempSetPoint, 0.001);
    EXPECT_NEAR(DataLoopNode::Node(2).Temp, 16.0, 0.001);

    // expect design spec data to match inputs
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[0], 0.1000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[0], 0.1010, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[1], 0.2000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[1], 0.2010, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[2], 0.3000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[2], 0.3010, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[3], 0.4000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[3], 0.4010, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[4], 0.5000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[4], 0.5010, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[5], 0.6000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[5], 0.6010, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[6], 0.7000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[6], 0.7010, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[7], 0.8000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[7], 0.8010, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[8], 0.9000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[8], 0.9010, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[9], 1.0000, 0.00001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[9], 1.0000, 0.00001);

    // autosized air flow and capacity, unitary sytsem capacity matches coils
    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, 1.5);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, 1.5);

    // TotCapTempModFac is evaluated at the OutTemp which is 35°C
    // In the Fixture's SetUp: `DataSizing::DesDayWeath(1).Temp(1) = 35.0`
    Real64 RatedSourceTempCool = DataSizing::DesDayWeath(1).Temp(1);
    EXPECT_EQ(RatedSourceTempCool, 35.0);
    Real64 CoolCoolCapAtPeak = 32454.876753104443;
    Real64 TotCapTempModFac = CurveManager::CurveValue(
        state, state.dataVariableSpeedCoils->VarSpeedCoil(1).MSCCapFTemp(state.dataVariableSpeedCoils->VarSpeedCoil(1).NormSpedLevel), 17.410329442560833, RatedSourceTempCool);

    EXPECT_NEAR(TotCapTempModFac, 0.930018048445091, 0.001);
    Real64 RatedCapCoolTotalDes = CoolCoolCapAtPeak / TotCapTempModFac;
    EXPECT_NEAR(RatedCapCoolTotalDes, 34897.0396944, 0.001);

    EXPECT_NEAR(thisSys->m_DesignCoolingCapacity, RatedCapCoolTotalDes, 0.001);
    EXPECT_EQ(thisSys->m_DesignCoolingCapacity, state.dataVariableSpeedCoils->VarSpeedCoil(1).RatedCapCoolTotal);
    EXPECT_NEAR(thisSys->m_DesignHeatingCapacity, RatedCapCoolTotalDes, 0.001);
    EXPECT_EQ(thisSys->m_DesignHeatingCapacity, state.dataVariableSpeedCoils->VarSpeedCoil(2).RatedCapHeat);
    // variable speed coils size air flow differently than other models. The design air volume flow rate is back calculated from design capacity
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).RatedAirVolFlowRate,
              state.dataVariableSpeedCoils->VarSpeedCoil(1).RatedCapCoolTotal * state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowPerRatedTotCap(10));
    Real64 fullFlow = 1.949442;
    EXPECT_NEAR(state.dataVariableSpeedCoils->VarSpeedCoil(1).RatedAirVolFlowRate, fullFlow, 0.00001); // different than unitary system air volume flow rate
    EXPECT_NEAR(state.dataVariableSpeedCoils->VarSpeedCoil(2).RatedAirVolFlowRate, 1.70, 0.01);        // VS DX heating coil was not autosized

    // checks on autosized cooling air flow rates
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[1], fullFlow * 0.1, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(1), thisSys->m_CoolVolumeFlowRate[1]);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[2], fullFlow * 0.2, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(2), thisSys->m_CoolVolumeFlowRate[2]);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[3], fullFlow * 0.3, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(3), thisSys->m_CoolVolumeFlowRate[3]);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[4], fullFlow * 0.4, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(4), thisSys->m_CoolVolumeFlowRate[4]);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[5], fullFlow * 0.5, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(5), thisSys->m_CoolVolumeFlowRate[5]);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[6], fullFlow * 0.6, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(6), thisSys->m_CoolVolumeFlowRate[6]);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[7], fullFlow * 0.7, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(7), thisSys->m_CoolVolumeFlowRate[7]);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[8], fullFlow * 0.8, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(8), thisSys->m_CoolVolumeFlowRate[8]);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[9], fullFlow * 0.9, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(9), thisSys->m_CoolVolumeFlowRate[9]);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[10], fullFlow * 1.0, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(10), thisSys->m_CoolVolumeFlowRate[10]);

    // checks on autosized heating air flow rates
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[1], 0.171700, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(1), thisSys->m_HeatVolumeFlowRate[1]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[2], 0.341700, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(2), thisSys->m_HeatVolumeFlowRate[2]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[3], 0.511699, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(3), thisSys->m_HeatVolumeFlowRate[3]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[4], 0.681699, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(4), thisSys->m_HeatVolumeFlowRate[4]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[5], 0.851700, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(5), thisSys->m_HeatVolumeFlowRate[5]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[6], 1.021699, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(6), thisSys->m_HeatVolumeFlowRate[6]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[7], 1.191700, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(7), thisSys->m_HeatVolumeFlowRate[7]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[8], 1.361700, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(8), thisSys->m_HeatVolumeFlowRate[8]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[9], 1.531700, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(9), thisSys->m_HeatVolumeFlowRate[9]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[10], 1.700000, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(10), thisSys->m_HeatVolumeFlowRate[10]);

    // spot check MSHP volume flow rate data. NOTE: design spec object is 0 based
    EXPECT_EQ(thisSys->m_CoolVolumeFlowRate[7], thisSys->m_CoolVolumeFlowRate[10] * UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[6]);
    EXPECT_EQ(thisSys->m_HeatVolumeFlowRate[7], thisSys->m_HeatVolumeFlowRate[10] * UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[6]);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_WaterCoilSPControl)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,    !- Name",
        "  Setpoint,                !- Control Type",
        "  East Zone,               !- Controlling Zone or Thermostat Location",
        "  None,                    !- Dehumidification Control Type",
        "  AlwaysOne,               !- Availability Schedule Name",
        "  Zone Exhaust Node,       !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,       !- Air Outlet Node Name",
        "  Fan:OnOff,               !- Supply Fan Object Type",
        "  Supply Fan 1,            !- Supply Fan Name",
        "  BlowThrough,             !- Fan Placement",
        "  AlwaysOne,               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Water,      !- Heating Coil Object Type",
        "  Water Heating Coil,      !- Heating Coil Name",
        "  ,                        !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:Water,      !- Cooling Coil Object Type",
        "  Water Cooling Coil,      !- Cooling Coil Name",
        "  ,                        !- Use DOAS DX Cooling Coil",
        "  15.0,                    !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                        !- Latent Load Control",
        "  Coil:Heating:Water,      !- Supplemental Heating Coil Object Type",
        "  Supp Water Heating Coil, !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,       !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                     !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                        !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,       !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                     !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                        !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,       !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  0.8,                     !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                        !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  25.0;                    !- Maximum Supply Air Temperature{ C }",

        "Fan:OnOff,",
        "  Supply Fan 1,            !- Name",
        "  AlwaysOne,               !- Availability Schedule Name",
        "  0.7,                     !- Fan Total Efficiency",
        "  600.0,                   !- Pressure Rise{ Pa }",
        "  1.6,                     !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                     !- Motor Efficiency",
        "  1.0,                     !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,       !- Air Inlet Node Name",
        "  Water Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Cooling:Water,",
        "  Water Cooling Coil,      !- Name",
        "  AlwaysOne,               !- Availability Schedule Namev",
        "  0.0004,                  !- Design Water Flow Rate { m3 / s }",
        "  1.6000,                  !- Design Air Flow Rate { m3 / s }",
        "  7.22,                    !- Design Inlet Water Temperature { Cv }",
        "  24.340,                  !- Design Inlet Air Temperature { C }",
        "  14.000,                  !- Design Outlet Air Temperature { C }",
        "  0.0095,                  !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "  0.0090,                  !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "  ChWInletNode,            !- Water Inlet Node Name",
        "  ChWOutletNode,           !- Water Outlet Node Name",
        "  Water Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Water Heating Coil Air Inlet Node, !- Air Outlet Node Name",
        "  SimpleAnalysis,          !- Type of Analysis",
        "  CrossFlow;               !- Heat Exchanger Configuration",

        "Coil:Heating:Water,",
        "  Water Heating Coil,      !- Name",
        "  AlwaysOne,               !- Availability Schedule Name",
        "  300.0,                   !- U - Factor Times Area Value { W / K }",
        "  0.0006,                  !- Maximum Water Flow Rate { m3 / s }",
        "  HWInletNode,             !- Water Inlet Node Name",
        "  HWOutletNode,            !- Water Outlet Node Name",
        "  Water Heating Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Water Heating Coil Air Outlet Node, !- Air Outlet Node Name",
        "  UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "  5000.0,                  !- Rated Capacity { W }",
        "  82.2,                    !- Rated Inlet Water Temperature { C }",
        "  16.6,                    !- Rated Inlet Air Temperature { C }",
        "  71.1,                    !- Rated Outlet Water Temperature { C }",
        "  32.2,                    !- Rated Outlet Air Temperature { C }",
        "  ;                        !- Rated Ratio for Air and Water Convection",

        "Coil:Heating:Water,",
        "  Supp Water Heating Coil, !- Name",
        "  AlwaysOne,               !- Availability Schedule Name",
        "  300.0,                   !- U - Factor Times Area Value { W / K }",
        "  0.0006,                  !- Maximum Water Flow Rate { m3 / s }",
        "  SuppHWInletNode,         !- Water Inlet Node Name",
        "  SuppHWOutletNode,        !- Water Outlet Node Name",
        "  Water Heating Coil Air Outlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,       !- Air Outlet Node Name",
        "  UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "  5000.0,                  !- Rated Capacity { W }",
        "  82.2,                    !- Rated Inlet Water Temperature { C }",
        "  16.6,                    !- Rated Inlet Air Temperature { C }",
        "  71.1,                    !- Rated Outlet Water Temperature { C }",
        "  32.2,                    !- Rated Outlet Air Temperature { C }",
        "  ;                        !- Rated Ratio for Air and Water Convection",

        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,               !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00, 1.0;       !- Field 3",

        "Schedule:Compact,",
        "  Always 16C,              !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00, 16.0;      !- Field 3",

        "Schedule:Compact,",
        "  Always 18C,              !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00, 18.0;      !- Field 3",

        "Schedule:Compact,",
        "  Always 20C,              !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00, 20.0;      !- Field 3",

        "SetpointManager:Scheduled,",
        "  CW Coil Setpoint Manager, !- Name",
        "  Temperature, !- Control Variable",
        "  Always 20C, !- Schedule Name",
        "  Water Heating Coil Air Inlet Node;  !- Setpoint Node or NodeList Name",

        "SetpointManager:Scheduled,",
        "  HW Coil Setpoint Manager, !- Name",
        "  Temperature, !- Control Variable",
        "  Always 16C, !- Schedule Name",
        "  Water Heating Coil Air Outlet Node;  !- Setpoint Node or NodeList Name",

        "SetpointManager:Scheduled,",
        "  Supp HW Coil Setpoint Manager, !- Name",
        "  Temperature, !- Control Variable",
        "  Always 18C, !- Schedule Name",
        "  Zone 2 Inlet Node;  !- Setpoint Node or NodeList Name",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    // Verify UnitarySystem air flow rates are read in as input
    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, 1.6);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, 1.6);
    EXPECT_EQ(thisSys->m_MaxNoCoolHeatAirVolFlow, 0.8);

    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = "WATER COOLING COIL";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterCooling;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 10;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 11;

    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = "WATER HEATING COIL";
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterSimpleHeating;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 4;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 5;

    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(2).Name = "SUPP WATER HEATING COIL";
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(2).TypeOf_Num = DataPlant::TypeOf_CoilWaterSimpleHeating;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(2).NodeNumIn = 8;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(2).NodeNumOut = 9;

    OutputReportPredefined::SetPredefinedTables();

    // UnitarySystem used as zone equipment will not be modeled when FirstHAVCIteration is true, first time FirstHVACIteration = false will disable
    // the 'return' on FirstHVACIteration = true set FirstHVACIteration to false for unit testing to size water coils
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;

    // sizing routine will overwrite water coil air and water inlet nodes with design conditions so no need set set up node conditions yet
    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    auto unitarySystemAirInletNodeIndex = UtilityRoutines::FindItemInList("ZONE EXHAUST NODE", DataLoopNode::NodeID);                 // was Node 1
    auto coolingCoilAirInletNodeIndex = UtilityRoutines::FindItemInList("WATER COOLING COIL AIR INLET NODE", DataLoopNode::NodeID);   // was Node 3
    auto coolingCoilAirOutletNodeIndex = UtilityRoutines::FindItemInList("WATER HEATING COIL AIR INLET NODE", DataLoopNode::NodeID);  // was Node 6
    auto heatingCoilAirOutletNodeIndex = UtilityRoutines::FindItemInList("WATER HEATING COIL AIR OUTLET NODE", DataLoopNode::NodeID); // was Node 7
    auto suppHeatingAirOutletNodeIndex = UtilityRoutines::FindItemInList("ZONE 2 INLET NODE", DataLoopNode::NodeID);                  // was Node 2
    auto coolingCoilWaterInletNodeIndex = UtilityRoutines::FindItemInList("CHWINLETNODE", DataLoopNode::NodeID);                      // was Node 10
    auto heatingCoilWaterInletNodeIndex = UtilityRoutines::FindItemInList("HWINLETNODE", DataLoopNode::NodeID);                       // was Node 4
    auto suppHeatingCoilWaterInletNodeIndex = UtilityRoutines::FindItemInList("SUPPHWINLETNODE", DataLoopNode::NodeID);               // was Node 8

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node = 1
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).MassFlowRate = 1.9;
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).MassFlowRateMaxAvail = 1.9; // max avail at fan inlet so fan won't limit flow

    // test COOLING condition
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).Temp = 24.0;         // 24C db
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).HumRat = 0.00922;    // 17C wb
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).Enthalpy = 47597.03; // www.sugartech.com/psychro/index.php

    // Cooling coil air inlet node = 3
    DataLoopNode::Node(coolingCoilAirInletNodeIndex).MassFlowRateMax = 1.9; // max at fan outlet so fan won't limit flow
                                                                            // Cooling coil air outlet node = 6
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint = 20.0;
    // Heating coil air inlet node = 6
    // Heating coil air outlet node = 7
    DataLoopNode::Node(heatingCoilAirOutletNodeIndex).TempSetPoint = 16.0;
    // Supp heating coil air inlet node = 7
    // Supp heating coil air outlet node = 2
    DataLoopNode::Node(suppHeatingAirOutletNodeIndex).TempSetPoint = 18.0;

    // Cooling coil water inlet node = 10
    DataLoopNode::Node(coolingCoilWaterInletNodeIndex).Temp = 6.0;
    DataLoopNode::Node(coolingCoilWaterInletNodeIndex).Enthalpy = 25321.8; // www.peacesoftware.de/einigewerte/calc_dampf.php5

    // Heating coil water inlet node = 4
    DataLoopNode::Node(heatingCoilWaterInletNodeIndex).Temp = 60.0;
    DataLoopNode::Node(heatingCoilWaterInletNodeIndex).Enthalpy = 251221.6; // www.peacesoftware.de/einigewerte/calc_dampf.php5

    // Supp heating coil water inlet node = 8
    DataLoopNode::Node(suppHeatingCoilWaterInletNodeIndex).Temp = 60.0;
    DataLoopNode::Node(suppHeatingCoilWaterInletNodeIndex).Enthalpy = 251221.6; // www.peacesoftware.de/einigewerte/calc_dampf.php5

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // COOLING mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that CW coil air outlet node is at set point
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    EXPECT_NEAR(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint, 0.01);
    // CW air inlet node temp is greater than CW air outlet node temp
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    EXPECT_GT(DataLoopNode::Node(coolingCoilAirInletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp);
    // CW water inlet node flow is greater than 0
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_GT( Node( coolingCoilWaterInletNodeIndex ).MassFlowRate, 0.0 );
    // CW water node flow is the same at inlet and outlet
    EXPECT_NEAR(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(11).MassFlowRate, 0.0001);
    // CW water outlet node temp is greater than CW inlet node temp
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_GT( Node( 11 ).Temp, Node( coolingCoilWaterInletNodeIndex ).Temp );
    // HW air inlet and outlet nodes are at same temp
    EXPECT_EQ(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).MassFlowRate, DataLoopNode::Node(heatingCoilAirOutletNodeIndex).MassFlowRate);
    // Supp HW air inlet and outlet nodes are at same temp
    EXPECT_EQ(DataLoopNode::Node(heatingCoilAirOutletNodeIndex).MassFlowRate, DataLoopNode::Node(suppHeatingAirOutletNodeIndex).MassFlowRate);
    // HW water node flow is 0
    EXPECT_EQ(DataLoopNode::Node(heatingCoilWaterInletNodeIndex).MassFlowRate, 0.0);
    // HW water node flow is the same at inlet and outlet
    EXPECT_EQ(DataLoopNode::Node(heatingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(5).MassFlowRate);
    // HW water outlet node temp is equal to water inlet node temp
    EXPECT_EQ(DataLoopNode::Node(heatingCoilWaterInletNodeIndex).Temp, DataLoopNode::Node(5).Temp);
    // Supp HW water inlet node flow is equal to 0
    EXPECT_EQ(DataLoopNode::Node(suppHeatingCoilWaterInletNodeIndex).MassFlowRate, 0.0);
    // Supp HW water node flow is the same at inlet and outlet
    EXPECT_EQ(DataLoopNode::Node(suppHeatingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(9).MassFlowRate);
    // Supp HW water outlet node temp is equal to water inlet node temp
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_EQ( Node( suppHeatingCoilWaterInletNodeIndex ).Temp, Node( 9 ).Temp );

    // if cooling coil meets cooling set point temperature expect cooling coil water flow to be less than max water flow
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMax);
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate,
              DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMaxAvail);
    // expect cooling coil outlet air temp to be less than cooling coil inlet air temp
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_LT( Node( coolingCoilAirOutletNodeIndex ).Temp, Node( coolingCoilAirInletNodeIndex ).Temp );
    // expect heating coil outlet air temp to be greater than heating coil outlet air temp set point
    EXPECT_GT(DataLoopNode::Node(heatingCoilAirOutletNodeIndex).Temp, DataLoopNode::Node(heatingCoilAirOutletNodeIndex).TempSetPoint);
    // expect supp heating coil outlet air temp to be greater than supp heating coil outlet air temp set point
    EXPECT_GT(DataLoopNode::Node(suppHeatingAirOutletNodeIndex).Temp, DataLoopNode::Node(suppHeatingAirOutletNodeIndex).TempSetPoint);

    // verify dehumidify set points are not set
    EXPECT_LT(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax, 0.0);
    EXPECT_LT(DataLoopNode::Node(thisSys->AirOutNode).HumRatMax, 0.0);
    EXPECT_GT(DataLoopNode::Node(thisSys->AirOutNode).HumRat, 0.009); // and air outlet HumRat > 0.009 without dehumidification control
    EXPECT_LT(thisSys->m_CoolingPartLoadFrac, 1.0);
    Real64 sensOnlyPartLoadFrac = thisSys->m_CoolingPartLoadFrac;
    Real64 sensOnlyOutletAirHumRat = DataLoopNode::Node(thisSys->AirOutNode).HumRat;

    // now test that the coil outlet node HumRatMax set point works for the cooling coil
    thisSys->m_DehumidControlType_Num = UnitarySys::DehumCtrlType::CoolReheat;
    thisSys->m_RunOnLatentLoad = true;
    DataLoopNode::Node(thisSys->AirOutNode).HumRatMax = 0.008;
    // COOLING mode
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // why doesn't water cooling coil decrease outlet air hum rat when PLR is increased?
    EXPECT_GT(thisSys->m_CoolingPartLoadFrac, sensOnlyPartLoadFrac);
    EXPECT_LE(DataLoopNode::Node(thisSys->AirOutNode).HumRat, sensOnlyOutletAirHumRat);

    // reset system to original values
    thisSys->m_DehumidControlType_Num = UnitarySys::DehumCtrlType::None;
    thisSys->m_RunOnLatentLoad = false;
    DataLoopNode::Node(thisSys->AirOutNode).HumRatMax = DataLoopNode::SensedNodeFlagValue;

    // HEATING mode
    // Unitary system AIR inlet node = 1
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).Temp = 14.0;      // 14C db
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).HumRat = 0.00693; // 11C wb
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).Enthalpy = 31598.76;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // CW air inlet node temp is equal to CW air outlet node temp
    EXPECT_EQ(DataLoopNode::Node(coolingCoilAirInletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp);
    // check that heating coil outlet node is at set point
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_NEAR( Node( heatingCoilAirOutletNodeIndex ).Temp, Node( heatingCoilAirOutletNodeIndex ).TempSetPoint, 0.001 );
    // EXPECT_NEAR( Node( heatingCoilAirOutletNodeIndex ).Temp, 16.0, 0.001 );
    // check that supp heating coil outlet node is at set point
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_NEAR( Node( suppHeatingAirOutletNodeIndex ).Temp, Node( suppHeatingAirOutletNodeIndex ).TempSetPoint, 0.001 );
    // EXPECT_NEAR( Node( suppHeatingAirOutletNodeIndex ).Temp, 18.0, 0.001 );

    // CW water inlet node flow is equal to 0
    EXPECT_EQ(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, 0.0);
    // CW water node flow is the same at inlet and outlet
    EXPECT_EQ(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(11).MassFlowRate);
    // CW water outlet node temp is equal to CW inlet node temp
    EXPECT_EQ(DataLoopNode::Node(11).Temp, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).Temp);
    // HW water node flow is greater than 0
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_GT( Node( heatingCoilWaterInletNodeIndex ).MassFlowRate, 0.0 );
    // HW water node flow is the same at inlet and outlet
    EXPECT_EQ(DataLoopNode::Node(heatingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(5).MassFlowRate);
    // HW water outlet node temp is lower than water inlet node temp
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_LT( Node( 5 ).Temp, Node( heatingCoilWaterInletNodeIndex ).Temp );
    // Supp HW water node flow is greater than 0 (since supp outlet SP is higher than HW coil outlet SP)
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_GT( Node( suppHeatingCoilWaterInletNodeIndex ).MassFlowRate, 0.0 );
    // HW water node flow is the same at inlet and outlet
    EXPECT_EQ(DataLoopNode::Node(suppHeatingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(9).MassFlowRate);
    // HW water outlet node temp is lower than water inlet node temp
    EXPECT_LT(DataLoopNode::Node(9).Temp, DataLoopNode::Node(suppHeatingCoilWaterInletNodeIndex).Temp);

    // if heating coil meets set point temperature expect heating coil water flow to be less than max water flow
    EXPECT_LT(DataLoopNode::Node(heatingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(heatingCoilWaterInletNodeIndex).MassFlowRateMax);
    EXPECT_LT(DataLoopNode::Node(heatingCoilWaterInletNodeIndex).MassFlowRate,
              DataLoopNode::Node(heatingCoilWaterInletNodeIndex).MassFlowRateMaxAvail);

    // if supp heating coil meets set point temperature expect supp heating coil water flow to be less than max water flow
    // TODO: FIXME: following is failing for some reason even after correcting nodes.
    // EXPECT_LT( Node( suppHeatingCoilWaterInletNodeIndex ).MassFlowRate, Node( suppHeatingCoilWaterInletNodeIndex ).MassFlowRateMax );
    // EXPECT_LT( Node( suppHeatingCoilWaterInletNodeIndex ).MassFlowRate, Node( suppHeatingCoilWaterInletNodeIndex ).MassFlowRateMaxAvail );
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_WaterCoilSPControl_Latent)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,    !- Name",
        "  Setpoint,                !- Control Type",
        "  East Zone,               !- Controlling Zone or Thermostat Location",
        "  CoolReheat,                    !- Dehumidification Control Type",
        "  AlwaysOne,               !- Availability Schedule Name",
        "  Water Cooling Coil Air Inlet Node,       !- Air Inlet Node Name",
        "  Zone Inlet Node,       !- Air Outlet Node Name",
        "  Fan:OnOff,               !- Supply Fan Object Type",
        "  Supply Fan 1,            !- Supply Fan Name",
        "  DrawThrough,             !- Fan Placement",
        "  AlwaysOne,               !- Supply Air Fan Operating Mode Schedule Name",
        "  ,      !- Heating Coil Object Type",
        "  ,      !- Heating Coil Name",
        "  ,                        !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:Water,      !- Cooling Coil Object Type",
        "  Water Cooling Coil,      !- Cooling Coil Name",
        "  ,                        !- Use DOAS DX Cooling Coil",
        "  5.0,                    !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  LatentOrSensibleLoadControl,  !- Latent Load Control",
        "  ,      !- Supplemental Heating Coil Object Type",
        "  , !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,       !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                     !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                        !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,       !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                     !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                        !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,       !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  0.8,                     !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                        !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  25.0;                    !- Maximum Supply Air Temperature{ C }",

        "Fan:OnOff,",
        "  Supply Fan 1,            !- Name",
        "  AlwaysOne,               !- Availability Schedule Name",
        "  0.7,                     !- Fan Total Efficiency",
        "  600.0,                   !- Pressure Rise{ Pa }",
        "  1.6,                     !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                     !- Motor Efficiency",
        "  1.0,                     !- Motor In Airstream Fraction",
        "  Water Cooling Coil Air Outlet Node,       !- Air Inlet Node Name",
        "  Zone Inlet Node;  !- Air Outlet Node Name",

        "Coil:Cooling:Water,",
        "  Water Cooling Coil,      !- Name",
        "  AlwaysOne,               !- Availability Schedule Namev",
        "  0.0008,                  !- Design Water Flow Rate { m3 / s }",
        "  1.6000,                  !- Design Air Flow Rate { m3 / s }",
        "  7.22,                    !- Design Inlet Water Temperature { Cv }",
        "  24.340,                  !- Design Inlet Air Temperature { C }",
        "  14.000,                  !- Design Outlet Air Temperature { C }",
        "  0.013,                  !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "  0.0070,                  !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "  ChWInletNode,            !- Water Inlet Node Name",
        "  ChWOutletNode,           !- Water Outlet Node Name",
        "  Water Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Water Cooling Coil Air Outlet Node, !- Air Outlet Node Name",
        "  SimpleAnalysis,          !- Type of Analysis",
        "  CrossFlow;               !- Heat Exchanger Configuration",

        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,               !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00, 1.0;       !- Field 3",

        "Schedule:Compact,",
        "  Always 20C,              !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00, 20.0;      !- Field 3",

        "SetpointManager:Scheduled,",
        "  CW Coil Setpoint Manager, !- Name",
        "  Temperature, !- Control Variable",
        "  Always 20C, !- Schedule Name",
        "  Water Cooling Coil Air Outlet Node;  !- Setpoint Node or NodeList Name",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    auto unitarySystemAirInletNodeIndex = UtilityRoutines::FindItemInList("WATER COOLING COIL AIR INLET NODE", DataLoopNode::NodeID); // was Node 1
    auto coolingCoilAirInletNodeIndex = UtilityRoutines::FindItemInList("WATER COOLING COIL AIR INLET NODE", DataLoopNode::NodeID);   // was Node 3
    auto coolingCoilAirOutletNodeIndex = UtilityRoutines::FindItemInList("WATER COOLING COIL AIR OUTLET NODE", DataLoopNode::NodeID); // was Node 6
    auto coolingCoilWaterInletNodeIndex = UtilityRoutines::FindItemInList("CHWINLETNODE", DataLoopNode::NodeID);                      // was Node 10
    auto coolingCoilWaterOutletNodeIndex = UtilityRoutines::FindItemInList("CHWOUTLETNODE", DataLoopNode::NodeID);                    // was Node 10

    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = "WATER COOLING COIL";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterCooling;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = coolingCoilWaterInletNodeIndex;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = coolingCoilWaterOutletNodeIndex;

    OutputReportPredefined::SetPredefinedTables();

    // UnitarySystem used as zone equipment will not be modeled when FirstHAVCIteration is true, first time FirstHVACIteration = false will disable
    // the 'return' on FirstHVACIteration = true set FirstHVACIteration to false for unit testing to size water coils
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;

    // sizing routine will overwrite water coil air and water inlet nodes with design conditions so no need set set up node conditions yet
    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // set up node conditions to test UnitarySystem set point based control
    // Unitary system air inlet node
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).MassFlowRate = 1.9;
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).MassFlowRateMaxAvail = 1.9; // max avail at fan inlet so fan won't limit flow

    // test COOLING condition
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).Temp = 24.0;    // 24C db
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).HumRat = 0.013; // 18C dp
    DataLoopNode::Node(unitarySystemAirInletNodeIndex).Enthalpy = 57217.0;

    // Cooling coil air inlet node
    DataLoopNode::Node(coolingCoilAirInletNodeIndex).MassFlowRate = 1.9;
    DataLoopNode::Node(coolingCoilAirInletNodeIndex).MassFlowRateMax = 1.9; // max at inlet

    // Cooling coil water inlet node
    DataLoopNode::Node(coolingCoilWaterInletNodeIndex).Temp = 6.0;
    DataLoopNode::Node(coolingCoilWaterInletNodeIndex).Enthalpy = 25321.8; // www.peacesoftware.de/einigewerte/calc_dampf.php5

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    // Case 0 - COOLING mode - no load, sensible or latent
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax = 0.02;
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint = 30.0;
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that CW coil air outlet node is < setpoint
    EXPECT_LT(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint);
    // check that CW coil air outlet node humrat is <= set point
    EXPECT_LE(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRat, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax);
    // CW air inlet node temp equals CW air outlet node temp
    EXPECT_EQ(DataLoopNode::Node(coolingCoilAirInletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp);
    // CW water inlet node flow is 0
    EXPECT_EQ(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, 0.0);
    // CW water node flow is the same at inlet and outlet
    EXPECT_EQ(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(coolingCoilWaterOutletNodeIndex).MassFlowRate);
    // CW water outlet node temp is same as CW inlet node temp
    EXPECT_EQ(DataLoopNode::Node(coolingCoilWaterOutletNodeIndex).Temp, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).Temp);

    // if cooling coil meets cooling set point temperature expect cooling coil water flow to be less than max water flow
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMax);
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate,
              DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMaxAvail);

    // Case 1 - COOLING mode - sensible control only, no extra dehumidification required
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax = 0.02;
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint = 20.0;
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that CW coil air outlet node is at set point
    EXPECT_NEAR(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint, 0.001);
    // check that CW coil air outlet node humrat is >= set point
    EXPECT_LE(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRat, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax);
    // CW air inlet node temp is greater than CW air outlet node temp
    EXPECT_GT(DataLoopNode::Node(coolingCoilAirInletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp);
    // CW water inlet node flow is greater than 0
    EXPECT_GT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, 0.0);
    // CW water node flow is the same at inlet and outlet
    EXPECT_EQ(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(coolingCoilWaterOutletNodeIndex).MassFlowRate);
    // CW water outlet node temp is greater than CW inlet node temp
    EXPECT_GT(DataLoopNode::Node(coolingCoilWaterOutletNodeIndex).Temp, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).Temp);

    // if cooling coil meets cooling set point temperature expect cooling coil water flow to be less than max water flow
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMax);
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate,
              DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMaxAvail);

    // Case 2 - COOLING mode - sensible and latent load, extra dehumidification required
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax = 0.009;
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint = 20.0;
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that CW coil air outlet node is below set point
    EXPECT_LT(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint);
    // check that CW coil air outlet node humrat is at set point
    EXPECT_NEAR(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRat, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax, 0.0001);
    // CW air inlet node temp is greater than CW air outlet node temp
    EXPECT_GT(DataLoopNode::Node(coolingCoilAirInletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp);
    // CW water inlet node flow is greater than 0
    EXPECT_GT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, 0.0);
    // CW water node flow is the same at inlet and outlet
    EXPECT_EQ(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(coolingCoilWaterOutletNodeIndex).MassFlowRate);
    // CW water outlet node temp is greater than CW inlet node temp
    EXPECT_GT(DataLoopNode::Node(coolingCoilWaterOutletNodeIndex).Temp, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).Temp);

    // if cooling coil meets cooling set point temperature expect cooling coil water flow to be less than max water flow
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMax);
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate,
              DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMaxAvail);

    // Case 3 - COOLING mode - only latent load
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax = 0.009;
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint = 30.0;
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    // check that CW coil air outlet node is below set point
    EXPECT_LT(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).TempSetPoint);
    // check that CW coil air outlet node humrat is at set point
    EXPECT_NEAR(DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRat, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax, 0.0001);
    // CW air inlet node temp is greater than CW air outlet node temp
    EXPECT_GT(DataLoopNode::Node(coolingCoilAirInletNodeIndex).Temp, DataLoopNode::Node(coolingCoilAirOutletNodeIndex).Temp);
    // CW water inlet node flow is greater than 0
    EXPECT_GT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, 0.0);
    // CW water node flow is the same at inlet and outlet
    EXPECT_EQ(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(coolingCoilWaterOutletNodeIndex).MassFlowRate);
    // CW water outlet node temp is greater than CW inlet node temp
    EXPECT_GT(DataLoopNode::Node(coolingCoilWaterOutletNodeIndex).Temp, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).Temp);

    // if cooling coil meets cooling set point temperature expect cooling coil water flow to be less than max water flow
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate, DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMax);
    EXPECT_LT(DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRate,
              DataLoopNode::Node(coolingCoilWaterInletNodeIndex).MassFlowRateMaxAvail);

    // Case 4 - COOLING mode - dehumidification set point at air outlet instead of coil outlet
    DataLoopNode::Node(coolingCoilAirOutletNodeIndex).HumRatMax = DataLoopNode::SensedNodeFlagValue;
    DataLoopNode::Node(thisSys->AirOutNode).HumRatMax = 0.009;
    Real64 partLoadRatio = thisSys->m_CoolingPartLoadFrac;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
    // should get same answer as before
    EXPECT_NEAR(thisSys->m_CoolingPartLoadFrac, partLoadRatio, 0.0000001);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_SetOnOffMassFlowRateTest)
{
    std::string const idf_objects = delimited_string({
        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",
        "  ",
        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    UnitarySys thisSys;
    UnitarySystems::numUnitarySystems = 1;

    Real64 OnOffAirFlowRatio; // This is a return value
    Real64 PartLoadRatio(1.0);
    DataHVACGlobals::TurnFansOn = true; // enable fan to run

    thisSys.m_MultiOrVarSpeedHeatCoil = true;
    thisSys.m_MultiOrVarSpeedCoolCoil = true;
    DataLoopNode::Node.allocate(1);

    DataHVACGlobals::MSHPMassFlowRateLow = 0.0;
    DataHVACGlobals::MSHPMassFlowRateHigh = 0.0;

    thisSys.m_SysAvailSchedPtr = ScheduleManager::GetScheduleIndex(state, "FanAndCoilAvailSched"); // "Get" the schedule inputs
    thisSys.m_FanAvailSchedPtr = ScheduleManager::GetScheduleIndex(state, "FanAndCoilAvailSched");
    ScheduleManager::Schedule(1).CurrentValue = 1.0; // set availability and fan schedule to 1

    thisSys.m_HeatMassFlowRate.resize(4);
    thisSys.m_CoolMassFlowRate.resize(4);
    thisSys.m_MSHeatingSpeedRatio.resize(4);
    thisSys.m_MSCoolingSpeedRatio.resize(4);

    thisSys.m_LastMode = HeatingMode;
    thisSys.m_IdleMassFlowRate = 0.2;
    thisSys.m_IdleSpeedRatio = 0.2;
    thisSys.m_FanAvailSchedPtr = DataGlobals::ScheduleAlwaysOn;
    thisSys.AirInNode = 1;

    thisSys.m_HeatMassFlowRate[1] = 0.25;
    thisSys.m_MSHeatingSpeedRatio[1] = 0.25;
    thisSys.m_HeatMassFlowRate[2] = 0.5;
    thisSys.m_MSHeatingSpeedRatio[2] = 0.5;
    thisSys.m_HeatMassFlowRate[3] = 1.0;
    thisSys.m_MSHeatingSpeedRatio[3] = 1.0;

    thisSys.m_CoolMassFlowRate[1] = 0.3;
    thisSys.m_MSCoolingSpeedRatio[1] = 0.3;
    thisSys.m_CoolMassFlowRate[2] = 0.6;
    thisSys.m_MSCoolingSpeedRatio[2] = 0.6;
    thisSys.m_CoolMassFlowRate[3] = 1.2;
    thisSys.m_MSCoolingSpeedRatio[3] = 1.2;

    // heating load at various speeds
    thisSys.m_HeatingSpeedNum = 3;
    thisSys.m_CoolingSpeedNum = 0;
    UnitarySystems::HeatingLoad = true;
    UnitarySystems::CoolingLoad = false;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.5, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(1.0, DataHVACGlobals::MSHPMassFlowRateHigh);

    thisSys.setSpeedVariables(HeatingLoad, PartLoadRatio);
    EXPECT_EQ(0.5, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(1.0, DataHVACGlobals::MSHPMassFlowRateHigh);

    PartLoadRatio = 0.7; // PLR should have no affect for constant fan operating mode
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.5, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(1.0, DataHVACGlobals::MSHPMassFlowRateHigh);

    thisSys.setSpeedVariables(HeatingLoad, PartLoadRatio);
    EXPECT_EQ(0.5, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(1.0, DataHVACGlobals::MSHPMassFlowRateHigh);

    PartLoadRatio = 1.0;
    thisSys.m_HeatingSpeedNum = 2;
    thisSys.m_CoolingSpeedNum = 0;
    UnitarySystems::HeatingLoad = true;
    UnitarySystems::CoolingLoad = false;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.5, DataHVACGlobals::MSHPMassFlowRateHigh);

    // constant fan mode should not drop to idle flow rate at speed = 1
    thisSys.m_FanOpMode = DataHVACGlobals::ContFanCycCoil;

    thisSys.m_HeatingSpeedNum = 1;
    thisSys.m_CoolingSpeedNum = 0;
    UnitarySystems::HeatingLoad = true;
    UnitarySystems::CoolingLoad = false;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateHigh);

    PartLoadRatio = 0.7;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateHigh);

    thisSys.setSpeedVariables(HeatingLoad, PartLoadRatio);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateHigh);

    PartLoadRatio = 1.0;
    // heating load with moisture load (cooling coil operates)
    UnitarySystems::MoistureLoad = -0.001;
    thisSys.m_Humidistat = true;
    thisSys.m_DehumidControlType_Num = UnitarySys::DehumCtrlType::CoolReheat;
    thisSys.m_CoolingSpeedNum = 3;
    UnitarySystems::HeatingLoad = true;
    UnitarySystems::CoolingLoad = false;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.6, UnitarySystems::CompOffMassFlow);
    EXPECT_EQ(1.2, UnitarySystems::CompOnMassFlow);
    EXPECT_EQ(0.6, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(1.2, DataHVACGlobals::MSHPMassFlowRateHigh);

    PartLoadRatio = 0.5;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.6, UnitarySystems::CompOffMassFlow);
    EXPECT_EQ(1.2, UnitarySystems::CompOnMassFlow);
    EXPECT_EQ(0.6, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(1.2, DataHVACGlobals::MSHPMassFlowRateHigh);

    PartLoadRatio = 1.0;
    MoistureLoad = 0.0;
    thisSys.m_Humidistat = false;
    thisSys.m_DehumidControlType_Num = UnitarySys::DehumCtrlType::None;

    // cycling fan mode should drop to 0 flow rate for cycling fan mode only below speed = 1
    thisSys.m_FanOpMode = DataHVACGlobals::CycFanCycCoil;

    thisSys.m_HeatingSpeedNum = 1;
    thisSys.m_CoolingSpeedNum = 0;
    HeatingLoad = true;
    CoolingLoad = false;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.0, UnitarySystems::CompOffMassFlow);
    EXPECT_EQ(0.25, UnitarySystems::CompOnMassFlow);
    EXPECT_EQ(0.0, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateHigh);

    // cooling load at various speeds
    thisSys.m_HeatingSpeedNum = 0;
    thisSys.m_CoolingSpeedNum = 3;
    HeatingLoad = false;
    CoolingLoad = true;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.6, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(1.2, DataHVACGlobals::MSHPMassFlowRateHigh);

    thisSys.m_HeatingSpeedNum = 0;
    thisSys.m_CoolingSpeedNum = 2;
    UnitarySystems::HeatingLoad = false;
    UnitarySystems::CoolingLoad = true;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.6, DataHVACGlobals::MSHPMassFlowRateHigh);

    // cycling fan mode should drop to 0 flow rate at speed = 1
    thisSys.m_HeatingSpeedNum = 0;
    thisSys.m_CoolingSpeedNum = 1;
    UnitarySystems::HeatingLoad = false;
    UnitarySystems::CoolingLoad = true;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.0, UnitarySystems::CompOffMassFlow); // CompOffMassFlow equal to 0 mass flow rate for cycling fan
    EXPECT_EQ(0.3, UnitarySystems::CompOnMassFlow);
    EXPECT_EQ(0.0, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateHigh);

    // constant fan mode should not drop to idle flow rate at speed = 1
    thisSys.m_FanOpMode = DataHVACGlobals::ContFanCycCoil;

    thisSys.m_HeatingSpeedNum = 0;
    thisSys.m_CoolingSpeedNum = 1;
    UnitarySystems::HeatingLoad = false;
    UnitarySystems::CoolingLoad = true;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.3, UnitarySystems::CompOffMassFlow); // CompOffMassFlow equal to speed 1 mass flow rate
    EXPECT_EQ(0.3, UnitarySystems::CompOnMassFlow);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateHigh);

    // no load condition (operates at idle speed)
    thisSys.m_HeatingSpeedNum = 0;
    thisSys.m_CoolingSpeedNum = 0;
    UnitarySystems::HeatingLoad = false;
    UnitarySystems::CoolingLoad = false;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.2, UnitarySystems::CompOffMassFlow); // CompOffMassFlow equal to speed 1 mass flow rate
    EXPECT_EQ(0.2, UnitarySystems::CompOnMassFlow);
    EXPECT_EQ(0.2, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.2, DataHVACGlobals::MSHPMassFlowRateHigh);

    thisSys.m_MultiSpeedHeatingCoil = true;
    thisSys.m_HeatingSpeedNum = 1;
    UnitarySystems::HeatingLoad = true;
    PartLoadRatio = 0.7;
    // PLR has no impact for constant fan flow case
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateHigh);

    thisSys.setSpeedVariables(HeatingLoad, PartLoadRatio);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateHigh);

    // test for cycling fan flow case where MSHPMassFlowRateLow variable is proportional to PLR (flow @ 0.25 * PLR @ 0.7 = 0.175)
    thisSys.m_FanOpMode = DataHVACGlobals::CycFanCycCoil;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.175, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateHigh);

    thisSys.setSpeedVariables(HeatingLoad, PartLoadRatio);
    EXPECT_EQ(0.175, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.25, DataHVACGlobals::MSHPMassFlowRateHigh);

    // same test for cooling mode (flow @ 0.3 * PLR @ 0.7 = 0.21)
    thisSys.m_HeatingSpeedNum = 0;
    thisSys.m_CoolingSpeedNum = 1;
    thisSys.m_DiscreteSpeedCoolingCoil = true;
    UnitarySystems::HeatingLoad = false;
    UnitarySystems::CoolingLoad = true;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.21, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateHigh);

    thisSys.setSpeedVariables(CoolingLoad, PartLoadRatio);
    EXPECT_EQ(0.21, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateHigh);

    // and flip back to constant fan and both variables should be the same
    thisSys.m_FanOpMode = DataHVACGlobals::ContFanCycCoil;
    thisSys.setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateHigh);

    thisSys.setSpeedVariables(CoolingLoad, PartLoadRatio);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateLow);
    EXPECT_EQ(0.3, DataHVACGlobals::MSHPMassFlowRateHigh);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_ConfirmUnitarySystemSizingTest)
{
    int AirLoopNum(1);
    int iCoolingSizingType(1);
    int iHeatingSizingType(1);
    bool FirstHVACIteration(true);
    Array1D_int SizingTypes({DataSizing::None,
                             DataSizing::SupplyAirFlowRate,
                             DataSizing::FlowPerFloorArea,
                             DataSizing::FractionOfAutosizedCoolingAirflow,
                             DataSizing::FractionOfAutosizedHeatingAirflow,
                             DataSizing::FlowPerCoolingCapacity,
                             DataSizing::FlowPerHeatingCapacity});

    //	int const None( 1 );
    //	int const SupplyAirFlowRate( 2 );
    //	int const FlowPerFloorArea( 3 );
    //	int const FractionOfAutosizedCoolingAirflow( 4 );
    //	int const FractionOfAutosizedHeatingAirflow( 5 );
    //	int const FlowPerCoolingCapacity( 6 );
    //	int const FlowPerHeatingCapacity( 7 );
    //	int const CoolingDesignCapacity( 8 );
    //	int const HeatingDesignCapacity( 9 );
    //	int const CapacityPerFloorArea( 10 );
    //	int const FractionOfAutosizedCoolingCapacity( 11 );
    //	int const FractionOfAutosizedHeatingCapacity( 12 );

    DataSizing::FinalZoneSizing.allocate(1);
    DataSizing::ZoneEqSizing.allocate(1);
    DataSizing::SysSizPeakDDNum.allocate(1);

    DataSizing::CurSysNum = 0;
    DataSizing::CurOASysNum = 0;
    DataSizing::CurZoneEqNum = 1;
    DataEnvironment::StdRhoAir = 1.0; // Prevent divide by zero in Sizer

    UnitarySys thisSys;
    UnitarySys *mySys(&thisSys);
    UnitarySystems::numUnitarySystems = 1;

    thisSys.UnitType = "AirLoopHVAC:UnitarySystem";
    thisSys.m_MultiOrVarSpeedCoolCoil = false;
    thisSys.m_MultiOrVarSpeedHeatCoil = false;
    thisSys.UnitarySystemType_Num = DataHVACGlobals::UnitarySys_AnyCoilType;
    thisSys.m_RequestAutoSize = true;

    DataSizing::ZoneSizingRunDone = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesignSizeFromParent = false;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod.allocate(25);
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;

    // test cooling only sizing
    thisSys.m_FanExists = true;
    thisSys.m_CoolCoilExists = true;
    thisSys.m_HeatCoilExists = false;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolVolFlow = 1.005;

    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInTemp = 30.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInHumRat = 0.001;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesTemp = 15.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesHumRat = 0.0006;

    for (int iSizingType = DataSizing::None; iSizingType <= DataSizing::FlowPerCoolingCapacity; ++iSizingType) {

        if (iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow) continue; // not allowed for cooling air flow

        thisSys.Name = "UnitarySystem:CoolingOnly #" + General::TrimSigDigits(iSizingType);
        thisSys.m_CoolingSAFMethod = SizingTypes(iSizingType);
        thisSys.m_DesignCoolingCapacity = DataSizing::AutoSize;
        thisSys.m_MaxCoolAirVolFlow = DataSizing::AutoSize;
        thisSys.m_MaxHeatAirVolFlow = DataSizing::AutoSize;
        thisSys.m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
        thisSys.m_DesignFanVolFlowRate = DataSizing::AutoSize;

        // for FractionOfAutosizedCoolingAirflow, set sizing data to 1.005 and UnitarySystem MaxCoolAirVolFlow to 1, they will multiply and
        // yield 1.005
        if (iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow)
            DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolVolFlow = 1.005;
        if (iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow) thisSys.m_MaxCoolAirVolFlow = 1.0;
        // for FlowPerCoolingCapacity, do the division so sizing will yield 1.005
        if (iSizingType == DataSizing::FlowPerCoolingCapacity) thisSys.m_MaxCoolAirVolFlow = 1.005 / 18827.616766698276;

        mySys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

        EXPECT_EQ(1.005, thisSys.m_DesignFanVolFlowRate);
        EXPECT_EQ(1.005, thisSys.m_MaxCoolAirVolFlow);
        EXPECT_EQ(1.005, thisSys.m_MaxHeatAirVolFlow);
        EXPECT_EQ(1.005, thisSys.m_MaxNoCoolHeatAirVolFlow);
        EXPECT_EQ(18827.616766698276, DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesCoolingLoad);
    }

    // #6200 defect file shows fan flow rate when cooling coil is off and no cooling coil exists. Allow user to set flow rate = 0 when coil does not
    // exist.
    thisSys.Name = "UnitarySystem:CoolingOnly No Heating Coil";
    thisSys.m_CoolingSAFMethod = SizingTypes(DataSizing::SupplyAirFlowRate);
    thisSys.m_DesignCoolingCapacity = DataSizing::AutoSize;
    thisSys.m_MaxCoolAirVolFlow = DataSizing::AutoSize;
    thisSys.m_MaxHeatAirVolFlow = 0.0; // no heating coil
    thisSys.m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
    thisSys.m_DesignFanVolFlowRate = DataSizing::AutoSize;

    mySys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

    EXPECT_EQ(1.005, thisSys.m_DesignFanVolFlowRate);
    EXPECT_EQ(1.005, thisSys.m_MaxCoolAirVolFlow);
    EXPECT_EQ(0.0, thisSys.m_MaxHeatAirVolFlow);
    EXPECT_EQ(1.005, thisSys.m_MaxNoCoolHeatAirVolFlow);
    EXPECT_EQ(18827.616766698276, DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesCoolingLoad);

    // continue with unit testing of heating only system
    thisSys.m_CoolCoilExists = false;
    thisSys.m_HeatCoilExists = true;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow = 1.005;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatMassFlow = 1.005;

    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatCoilInTemp = 15.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).ZoneTempAtHeatPeak = 15.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatCoilInHumRat = 0.001;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).ZoneHumRatAtHeatPeak = 0.001;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).HeatDesTemp = 30.0;

    for (int iSizingType = DataSizing::None; iSizingType <= DataSizing::FlowPerHeatingCapacity; ++iSizingType) {

        if (iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow) continue; // not allowed for heating air flow
        if (iSizingType == DataSizing::FlowPerCoolingCapacity) continue;            // not allowed for heating air flow

        thisSys.Name = "UnitarySystem:HeatingOnly #" + General::TrimSigDigits(iSizingType);
        thisSys.m_HeatingSAFMethod = SizingTypes(iSizingType);
        thisSys.m_DesignHeatingCapacity = DataSizing::AutoSize;
        thisSys.m_MaxCoolAirVolFlow = DataSizing::AutoSize;
        thisSys.m_MaxHeatAirVolFlow = DataSizing::AutoSize;
        thisSys.m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
        thisSys.m_DesignFanVolFlowRate = DataSizing::AutoSize;

        // for FractionOfAutosizedHeatingAirflow, set sizing data to 1.005 and UnitarySystem MaxHeatAirVolFlow to 1, they will multiply and
        // yield 1.005
        if (iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow)
            DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow = 1.005;
        if (iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow) thisSys.m_MaxHeatAirVolFlow = 1.0;
        // for FlowPerHeatingCapacity, do the division so sizing will yield 1.005
        if (iSizingType == DataSizing::FlowPerHeatingCapacity) thisSys.m_MaxHeatAirVolFlow = 1.005 / 15148.243236712493;

        mySys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

        EXPECT_NEAR(1.005, thisSys.m_DesignFanVolFlowRate, 0.0000000001);
        EXPECT_NEAR(1.005, thisSys.m_MaxCoolAirVolFlow, 0.0000000001);
        EXPECT_EQ(1.005, thisSys.m_MaxHeatAirVolFlow);
        EXPECT_EQ(1.005, thisSys.m_MaxNoCoolHeatAirVolFlow);
        EXPECT_NEAR(15148.243236712493, DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesHeatingLoad, 0.0000000001);
    }

    // #6200 defect file shows fan flow rate when cooling coil is off and no cooling coil exists. Allow user to set flow rate = 0 when coil does not
    // exist.
    thisSys.Name = "UnitarySystem:HeatingOnly No Cooling Coil";
    thisSys.m_HeatingSAFMethod = SizingTypes(DataSizing::SupplyAirFlowRate);
    thisSys.m_DesignHeatingCapacity = DataSizing::AutoSize;
    thisSys.m_MaxCoolAirVolFlow = 0.0; // nocooling coil
    thisSys.m_MaxHeatAirVolFlow = DataSizing::AutoSize;
    thisSys.m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
    thisSys.m_DesignFanVolFlowRate = DataSizing::AutoSize;

    mySys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

    EXPECT_EQ(1.005, thisSys.m_DesignFanVolFlowRate);
    EXPECT_EQ(0.0, thisSys.m_MaxCoolAirVolFlow);
    EXPECT_EQ(1.005, thisSys.m_MaxHeatAirVolFlow);
    EXPECT_EQ(1.005, thisSys.m_MaxNoCoolHeatAirVolFlow);
    EXPECT_NEAR(15148.243236712493, DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesHeatingLoad, 0.0000000001);

    // continue with unit testing of cooling and heating system
    thisSys.m_CoolCoilExists = true;
    thisSys.m_HeatCoilExists = true;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolVolFlow = 1.005;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow = 0.095;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatMassFlow = 0.095;

    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInTemp = 30.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInHumRat = 0.001;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesTemp = 15.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesHumRat = 0.0006;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatCoilInTemp = 15.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatCoilInHumRat = 0.001;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).HeatDesTemp = 30.0;

    for (int iSizingType = DataSizing::None; iSizingType <= DataSizing::FlowPerHeatingCapacity; ++iSizingType) {

        iCoolingSizingType = iSizingType;
        iHeatingSizingType = iSizingType;
        if (iSizingType == DataSizing::FractionOfAutosizedCoolingAirflow) iHeatingSizingType = DataSizing::FractionOfAutosizedHeatingAirflow;
        if (iSizingType == DataSizing::FractionOfAutosizedHeatingAirflow) iCoolingSizingType = DataSizing::FractionOfAutosizedCoolingAirflow;
        if (iSizingType == DataSizing::FlowPerCoolingCapacity) iHeatingSizingType = DataSizing::FlowPerHeatingCapacity;
        if (iSizingType == DataSizing::FlowPerHeatingCapacity) iCoolingSizingType = DataSizing::FlowPerCoolingCapacity;
        thisSys.Name = "UnitarySystem:CoolingAndHeating #" + General::TrimSigDigits(iSizingType);
        thisSys.m_CoolingSAFMethod = SizingTypes(iCoolingSizingType);
        thisSys.m_HeatingSAFMethod = SizingTypes(iHeatingSizingType);
        thisSys.m_DesignCoolingCapacity = DataSizing::AutoSize;
        thisSys.m_DesignHeatingCapacity = DataSizing::AutoSize;
        thisSys.m_MaxCoolAirVolFlow = DataSizing::AutoSize;
        thisSys.m_MaxHeatAirVolFlow = DataSizing::AutoSize;
        thisSys.m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
        thisSys.m_DesignFanVolFlowRate = DataSizing::AutoSize;

        // for FractionOfAutosizedCoolingAirflow, set sizing data to 1.005 and UnitarySystem MaxCoolAirVolFlow to 1, they will multiply and
        // yield 1.005
        if (iCoolingSizingType == DataSizing::FractionOfAutosizedCoolingAirflow)
            DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolVolFlow = 1.005;
        if (iCoolingSizingType == DataSizing::FractionOfAutosizedCoolingAirflow) thisSys.m_MaxCoolAirVolFlow = 1.0;
        // for FlowPerCoolingCapacity, do the division so sizing will yield 1.005
        if (iCoolingSizingType == DataSizing::FlowPerCoolingCapacity) thisSys.m_MaxCoolAirVolFlow = 1.005 / 18827.616766698276;
        // for FractionOfAutosizedHeatingAirflow, set sizing data to 1.005 and UnitarySystem MaxHeatAirVolFlow to 1, they will multiply and
        // yield 1.005
        if (iHeatingSizingType == DataSizing::FractionOfAutosizedHeatingAirflow)
            DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow = 1.005;
        if (iHeatingSizingType == DataSizing::FractionOfAutosizedHeatingAirflow) thisSys.m_MaxHeatAirVolFlow = 1.0;
        // for FlowPerHeatingCapacity, do the division so sizing will yield 1.005
        if (iHeatingSizingType == DataSizing::FlowPerHeatingCapacity) thisSys.m_MaxHeatAirVolFlow = 1.005 / 1431.9234900374995;

        mySys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

        EXPECT_NEAR(1.005, thisSys.m_DesignFanVolFlowRate, 0.0000000001);
        EXPECT_NEAR(1.005, thisSys.m_MaxCoolAirVolFlow, 0.0000000001);
        EXPECT_NEAR(1.005, thisSys.m_MaxHeatAirVolFlow, 0.0000000001);
        EXPECT_NEAR(1.005, thisSys.m_MaxNoCoolHeatAirVolFlow, 0.0000000001);
        EXPECT_EQ(18827.616766698276, DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesCoolingLoad);
        EXPECT_NEAR(1431.9234900374995, DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesHeatingLoad, 0.0000000001);
    }
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_CalcUnitaryHeatingSystem)
{

    int AirLoopNum(1);
    bool FirstHVACIteration(false);
    int CompOn(1);
    Real64 OnOffAirFlowRatio(1.0);
    Real64 HeatCoilLoad(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 AirMassFlowRate(0.0);

    UnitarySys thisSys;
    UnitarySystems::numUnitarySystems = 1;

    DataPlant::TotNumLoops = 1;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    thisSys.m_MultiOrVarSpeedHeatCoil = true;
    thisSys.m_MultiOrVarSpeedCoolCoil = true;
    DataLoopNode::Node.allocate(10);
    state.dataWaterCoils->WaterCoil.allocate(1);

    thisSys.m_HeatMassFlowRate.resize(4);
    thisSys.m_CoolMassFlowRate.resize(4);
    thisSys.m_MSHeatingSpeedRatio.resize(4);
    thisSys.m_MSCoolingSpeedRatio.resize(4);
    thisSys.m_LastMode = HeatingMode;
    thisSys.m_IdleMassFlowRate = 0.2;
    thisSys.m_IdleSpeedRatio = 0.2;
    thisSys.m_FanAvailSchedPtr = DataGlobals::ScheduleAlwaysOn;
    thisSys.AirInNode = 1;
    thisSys.m_HeatMassFlowRate[1] = 0.25;
    thisSys.m_MSHeatingSpeedRatio[1] = 0.25;
    thisSys.m_HeatMassFlowRate[2] = 0.5;
    thisSys.m_MSHeatingSpeedRatio[2] = 0.5;
    thisSys.m_HeatMassFlowRate[3] = 1.0;
    thisSys.m_MSHeatingSpeedRatio[3] = 1.0;
    thisSys.m_CoolMassFlowRate[1] = 0.3;
    thisSys.m_MSCoolingSpeedRatio[1] = 0.3;
    thisSys.m_CoolMassFlowRate[2] = 0.6;
    thisSys.m_MSCoolingSpeedRatio[2] = 0.6;
    thisSys.m_CoolMassFlowRate[3] = 1.0;
    thisSys.m_MSCoolingSpeedRatio[3] = 1.0;

    // heating load at speed 3
    thisSys.m_NumOfSpeedHeating = 3;
    thisSys.m_HeatingSpeedNum = 3;
    thisSys.m_NumOfSpeedCooling = 3;
    thisSys.m_CoolingSpeedNum = 0;
    UnitarySystems::HeatingLoad = true;
    UnitarySystems::CoolingLoad = false;

    // cycling fan mode
    thisSys.m_FanOpMode = DataHVACGlobals::CycFanCycCoil;

    // heating load only
    MoistureLoad = 0.0;
    HeatCoilLoad = 12000.0;
    thisSys.m_Humidistat = false;

    AirMassFlowRate = 1.0;
    HotWaterMassFlowRate = 1.0;
    thisSys.MaxHeatCoilFluidFlow = HotWaterMassFlowRate;
    thisSys.m_DiscreteSpeedCoolingCoil = true;
    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingWater;
    thisSys.m_HeatingSpeedRatio = 1.0;
    thisSys.m_HeatingCycRatio = 1.0;
    thisSys.m_HeatingSpeedNum = 3;

    state.dataWaterCoils->CheckEquipName.allocate(1);
    state.dataWaterCoils->NumWaterCoils = 1;
    state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    state.dataWaterCoils->WaterCoil(1).SchedPtr = DataGlobals::ScheduleAlwaysOn;
    state.dataWaterCoils->WaterCoil(1).Name = "Water Heating Coil";
    state.dataWaterCoils->WaterCoil(1).WaterCoilType = DataHVACGlobals::Coil_HeatingWater;
    state.dataWaterCoils->WaterCoil(1).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_SimpleHeating;
    state.dataWaterCoils->WaterCoil(1).DesAirVolFlowRate = 1.0;
    state.dataWaterCoils->WaterCoil(1).MaxWaterVolFlowRate = HotWaterMassFlowRate;
    state.dataWaterCoils->WaterCoil(1).UACoil = 400.0;
    state.dataWaterCoils->WaterCoil(1).InletAirTemp = 10.0;
    state.dataWaterCoils->WaterCoil(1).InletAirEnthalpy = 18000.0;
    state.dataWaterCoils->WaterCoil(1).InletAirHumRat =
        Psychrometrics::PsyWFnTdbH(state.dataWaterCoils->WaterCoil(1).InletAirTemp, state.dataWaterCoils->WaterCoil(1).InletAirEnthalpy);

    state.dataWaterCoils->WaterCoil(1).AirInletNodeNum = 4;
    state.dataWaterCoils->WaterCoil(1).AirOutletNodeNum = 5;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).Temp = 10.0;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).Enthalpy = 18000;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).HumRat = Psychrometrics::PsyWFnTdbH(
        DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).Temp, DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).Enthalpy);

    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).MassFlowRateMax = AirMassFlowRate;

    state.dataWaterCoils->WaterCoil(1).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(1).WaterLoopSide = 1;
    state.dataWaterCoils->WaterCoil(1).WaterLoopBranchNum = 1;
    state.dataWaterCoils->WaterCoil(1).WaterLoopCompNum = 1;
    state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum = 6;
    state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum = 7;
    state.dataWaterCoils->WaterCoil(1).InletWaterTemp = 60.0;
    state.dataWaterCoils->WaterCoil(1).InletWaterMassFlowRate = HotWaterMassFlowRate;
    state.dataWaterCoils->WaterCoil(1).MaxWaterMassFlowRate = HotWaterMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum).Temp = state.dataWaterCoils->WaterCoil(1).InletWaterTemp;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;

    for (int l = 1; l <= DataPlant::TotNumLoops; ++l) {
        auto &loop(DataPlant::PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(DataPlant::PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(DataPlant::PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    DataPlant::PlantLoop(1).Name = "WaterLoop";
    DataPlant::PlantLoop(1).FluidName = "FluidWaterLoop";
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).FluidName = "WATER";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(1).Name;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state.dataWaterCoils->WaterCoil_SimpleHeating;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum;

    thisSys.m_HeatingCoilIndex = 1;
    thisSys.m_HeatingCoilName = state.dataWaterCoils->WaterCoil(1).Name;
    thisSys.HeatCoilFluidInletNode = state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum;
    thisSys.HeatCoilFluidOutletNodeNum = state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum;
    DataGlobals::DoingSizing = true;
    state.dataWaterCoils->WaterCoil(1).TotWaterHeatingCoilRate = 0.0;

    thisSys.calcUnitaryHeatingSystem(state, AirLoopNum, FirstHVACIteration, thisSys.m_HeatingCycRatio, CompOn, OnOffAirFlowRatio, HeatCoilLoad);

    EXPECT_NEAR(15750.0, state.dataWaterCoils->WaterCoil(1).TotWaterHeatingCoilRate, 2.0);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_CalcUnitaryCoolingSystem)
{

    int CompOn(1);
    int AirLoopNum(1);
    bool FirstHVACIteration(false);
    Real64 OnOffAirFlowRatio(1.0);
    Real64 CoilCoolHeatRat(1.0);
    Real64 AirMassFlowRate(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);

    UnitarySys thisSys;
    UnitarySystems::numUnitarySystems = 1;

    DataPlant::TotNumLoops = 1;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    Psychrometrics::InitializePsychRoutines();

    thisSys.m_MultiOrVarSpeedHeatCoil = true;
    thisSys.m_MultiOrVarSpeedCoolCoil = true;
    DataLoopNode::Node.allocate(10);
    state.dataWaterCoils->WaterCoil.allocate(1);

    thisSys.m_HeatMassFlowRate.resize(4);
    thisSys.m_CoolMassFlowRate.resize(4);
    thisSys.m_MSHeatingSpeedRatio.resize(4);
    thisSys.m_MSCoolingSpeedRatio.resize(4);
    thisSys.m_LastMode = HeatingMode;
    thisSys.m_IdleMassFlowRate = 0.2;
    thisSys.m_IdleSpeedRatio = 0.2;
    thisSys.m_FanAvailSchedPtr = DataGlobals::ScheduleAlwaysOn;
    thisSys.AirInNode = 1;
    thisSys.m_HeatMassFlowRate[1] = 0.25;
    thisSys.m_MSHeatingSpeedRatio[1] = 0.25;
    thisSys.m_HeatMassFlowRate[2] = 0.5;
    thisSys.m_MSHeatingSpeedRatio[2] = 0.5;
    thisSys.m_HeatMassFlowRate[3] = 1.0;
    thisSys.m_MSHeatingSpeedRatio[3] = 1.0;
    thisSys.m_CoolMassFlowRate[1] = 0.3;
    thisSys.m_MSCoolingSpeedRatio[1] = 0.3;
    thisSys.m_CoolMassFlowRate[2] = 0.6;
    thisSys.m_MSCoolingSpeedRatio[2] = 0.6;
    thisSys.m_CoolMassFlowRate[3] = 1.0;
    thisSys.m_MSCoolingSpeedRatio[3] = 1.0;
    thisSys.m_FanOpMode = DataHVACGlobals::CycFanCycCoil;

    // cooling load at speed 3
    thisSys.m_Humidistat = false;
    thisSys.m_NumOfSpeedHeating = 3;
    thisSys.m_HeatingSpeedNum = 0;
    thisSys.m_NumOfSpeedCooling = 3;
    thisSys.m_CoolingSpeedNum = 3;
    UnitarySystems::HeatingLoad = false;
    UnitarySystems::CoolingLoad = true;
    UnitarySystems::MoistureLoad = 0.0;
    AirMassFlowRate = 1.0;
    HotWaterMassFlowRate = 1.0;
    ColdWaterMassFlowRate = 1.0;

    thisSys.MaxCoolCoilFluidFlow = ColdWaterMassFlowRate;
    thisSys.m_DiscreteSpeedCoolingCoil = true;
    thisSys.m_CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingWater;
    thisSys.m_CoolingSpeedRatio = 1.0;
    thisSys.m_CoolingCycRatio = 1.0;
    thisSys.m_CoolingSpeedNum = 3;

    state.dataWaterCoils->CheckEquipName.allocate(1);
    state.dataWaterCoils->NumWaterCoils = 1;
    state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    state.dataWaterCoils->WaterCoil(1).SchedPtr = DataGlobals::ScheduleAlwaysOn;
    state.dataWaterCoils->WaterCoil(1).Name = "Water Cooling Coil";
    state.dataWaterCoils->WaterCoil(1).WaterCoilType = state.dataWaterCoils->CoilType_Cooling;
    state.dataWaterCoils->WaterCoil(1).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_Cooling;
    state.dataWaterCoils->WaterCoil(1).WaterCoilModel = state.dataWaterCoils->CoilModel_Cooling;
    state.dataWaterCoils->WaterCoil(1).DesAirVolFlowRate = 1.0;
    state.dataWaterCoils->WaterCoil(1).MaxWaterVolFlowRate = ColdWaterMassFlowRate;
    state.dataWaterCoils->WaterCoil(1).CoolingCoilAnalysisMode = state.dataWaterCoils->SimpleAnalysis;
    state.dataWaterCoils->WaterCoil(1).HeatExchType = state.dataWaterCoils->CrossFlow;
    state.dataWaterCoils->WaterCoil(1).UACoilTotal = 4689.0;
    state.dataWaterCoils->WaterCoil(1).UACoilExternal = 6110.0;
    state.dataWaterCoils->WaterCoil(1).UACoilInternal = 20164.0;
    state.dataWaterCoils->WaterCoil(1).TotCoilOutsideSurfArea = 50.0;

    state.dataWaterCoils->WaterCoil(1).MaxWaterVolFlowRate = 0.001;
    state.dataWaterCoils->WaterCoil(1).DesInletWaterTemp = 6.67;
    state.dataWaterCoils->WaterCoil(1).DesInletAirTemp = 30.0;
    state.dataWaterCoils->WaterCoil(1).DesOutletAirTemp = 12.0;
    state.dataWaterCoils->WaterCoil(1).DesInletAirHumRat = 0.013;
    state.dataWaterCoils->WaterCoil(1).DesOutletAirHumRat = 0.008;
    state.dataWaterCoils->WaterCoil(1).AirInletNodeNum = 4;
    state.dataWaterCoils->WaterCoil(1).AirOutletNodeNum = 5;
    state.dataWaterCoils->WaterCoil(1).InletAirTemp = 30.0;
    state.dataWaterCoils->WaterCoil(1).InletAirEnthalpy = 53000;
    state.dataWaterCoils->WaterCoil(1).InletAirHumRat =
        Psychrometrics::PsyWFnTdbH(state.dataWaterCoils->WaterCoil(1).InletAirTemp, state.dataWaterCoils->WaterCoil(1).InletAirEnthalpy);
    state.dataWaterCoils->WaterCoil(1).InletWaterTemp = 6.0;
    state.dataWaterCoils->WaterCoil(1).InletAirMassFlowRate = AirMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).MassFlowRateMax = AirMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).Temp = 30.0;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).Enthalpy = 53000;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).HumRat = Psychrometrics::PsyWFnTdbH(
        DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).Temp, DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).Enthalpy);

    state.dataWaterCoils->WaterCoil(1).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(1).WaterLoopSide = 1;
    state.dataWaterCoils->WaterCoil(1).WaterLoopBranchNum = 1;
    state.dataWaterCoils->WaterCoil(1).WaterLoopCompNum = 1;
    state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum = 6;
    state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum = 7;
    state.dataWaterCoils->WaterCoil(1).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    state.dataWaterCoils->WaterCoil(1).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum).Temp = state.dataWaterCoils->WaterCoil(1).InletWaterTemp;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    DataLoopNode::Node(state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    for (int l = 1; l <= DataPlant::TotNumLoops; ++l) {
        auto &loop(DataPlant::PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(DataPlant::PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(DataPlant::PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    DataPlant::PlantLoop(1).Name = "WaterLoop";
    DataPlant::PlantLoop(1).FluidName = "FluidWaterLoop";
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).FluidName = "WATER";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(1).Name;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state.dataWaterCoils->WaterCoil_Cooling;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum;

    thisSys.m_CoolingCoilIndex = 1;
    thisSys.m_CoolingCoilName = state.dataWaterCoils->WaterCoil(1).Name;
    thisSys.CoolCoilFluidInletNode = state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum;
    thisSys.CoolCoilFluidOutletNodeNum = state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum;

    state.dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state.dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;
    DataGlobals::DoingSizing = true;

    state.dataWaterCoils->WaterCoil(1).TotWaterCoolingCoilRate = 0.0;

    thisSys.calcUnitaryCoolingSystem(
        state, AirLoopNum, FirstHVACIteration, thisSys.m_CoolingCycRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, false);

    EXPECT_NEAR(27530.0, state.dataWaterCoils->WaterCoil(1).TotWaterCoolingCoilRate, 2.0);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_GetInput)
{

    bool ErrorsFound(false);
    bool FirstHVACIteration(false);
    Real64 Qsens_sys(0.0);   // UnitarySystem delivered sensible capacity wrt zone
    Real64 ZoneTemp(0.0);    // control zone temperature
    int InletNode(0);        // UnitarySystem inlet node number
    int OutletNode(0);       // UnitarySystem outlet node number
    int ControlZoneNum(0);   // index to control zone

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",
        "  ",
        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
        "  ",
        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,          !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",
        "  ",
        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model, !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,         !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,   !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "  Furnace Heating Coil 1, !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
        "  Furnace ACDXCoil 1,     !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80;                     !- Maximum Supply Air Temperature{ C }",
        "  ",
        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  1.6,                    !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Cooling:DX:SingleSpeed,",
        "  Furnace ACDXCoil 1,      !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  32000,                   !- Gross Rated Total Cooling Capacity {W}",
        "  0.75,                    !- Gross Rated Sensible Heat Ratio",
        "  3.0,                     !- Gross Rated Cooling COP {W/W}",
        "  1.6,                     !- Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Part Load Fraction Correlation Curve Name",
        "   ,                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  1000,                    !- Nominal Time for Condensate Removal to Begin {s}",
        "  0.4,                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  4,                       !- Maximum Cycling Rate {cycles/hr}",
        "  45;                      !- Latent Capacity Time Constant {s}",
        "  ",
        "Coil:Heating:Fuel,",
        "  Furnace Heating Coil 1, !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8,                    !- Gas Burner Efficiency",
        "  32000,                  !- Nominal Capacity{ W }",
        "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8, !- Gas Burner Efficiency",
        "  32000, !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;    !- Air Outlet Node Name",
        "  ",
        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",
        "  ",
        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Curve:Quadratic,",
        "  WindACCoolCapFFF,       !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  WindACEIRFFF,           !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  WindACPLFFPLR,          !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",
        "  ",
        "Curve:Biquadratic,",
        "  WindACCoolCapFT,        !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  WindACEIRFT,            !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(state); // read zone equipment configuration and list objects

    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    int compTypeOfNum = DataHVACGlobals::UnitarySys_AnyCoilType;
    FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, compTypeOfNum, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects
    EXPECT_EQ(thisSys->UnitType, DataHVACGlobals::cFurnaceTypes(compTypeOfNum)); // compare UnitarySystem type string to valid type

    DataGlobals::SysSizingCalc = true; // DISABLE SIZING - don't call UnitarySystem::sizeSystem, much more work needed to set up sizing arrays

    InletNode = thisSys->AirInNode;
    OutletNode = thisSys->AirOutNode;
    ControlZoneNum = thisSys->NodeNumOfControlledZone;

    // set up unitary system inlet condtions
    DataLoopNode::Node(InletNode).Temp = 26.666667;             // AHRI condition 80F dry-bulb temp
    DataLoopNode::Node(InletNode).HumRat = 0.01117049542334198; // AHRI condition at 80F DB/67F WB lb/lb or kg/kg
    DataLoopNode::Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat);

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 20.0; // set zone temperature during heating season used to determine system delivered capacity

    // initialize other incidentals that are used within the UnitarySystem module during calculations
    DataSizing::CurZoneEqNum = 1;
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 1000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP = 2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP = 1000.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).RemainingOutputReqToDehumidSP = -0.00001;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP(1) =
        DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP;

    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);
    // UnitarySystem does not care (or look at) if Tstat is in deadband
    // This line tests case where other zone equipment changes deadband status
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = true;
    ScheduleManager::Schedule(1).CurrentValue = 1.0;
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0); // initialize RhoAir
    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail = thisSys->m_MaxCoolAirVolFlow * DataEnvironment::StdRhoAir;

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, thisSys->m_SensibleLoadMet, 0.01); // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxHeatAirMassFlow * thisSys->m_PartLoadFrac);               // cycling fan
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);
    EXPECT_DOUBLE_EQ(thisSys->m_MoistureLoadPredicted, 0.0); // dehumidification control type = none so MoistureLoad reset o 0

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -1000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = -1000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = -2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP(1) =
        DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP;

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 24.0; // set zone temperature during cooling season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;         // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;
    DXCoils::DXCoil(1).RatedCBF(1) = 0.1;                            // autosizing is disabled so initialize coil bypass factor
    DXCoils::DXCoil(1).RatedAirMassFlowRate(1) = 1.9268939689375426; // autosizing is disabled so initialize cooling coil rated air mass flow rate

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, thisSys->m_SensibleLoadMet, 0.025); // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxCoolAirMassFlow * thisSys->m_PartLoadFrac);                // cycling fan
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);

    // new tests for #5287, need to add an air loop to do this unit test justice
    EXPECT_TRUE(thisSys->m_FanIndex > 0);                                    // ZoneHVAC must contain a fan object to provide flow
    EXPECT_EQ(thisSys->m_FanType_Num, DataHVACGlobals::FanType_SimpleOnOff); // fan must be FanOnOff when used with cycling fan

    // switch to SingleZoneVAV control type and test that answer does not change since cycling fan is allowed but will not call the ASHRAE model
    // note that the input objects above show a constant fan operating mode, but since the schedules were never handled the schedule value = 0 which
    // means cycling fan
    thisSys->m_ControlType = UnitarySys::ControlType::CCMASHRAE; // control type = 3
    thisSys->m_ValidASHRAECoolCoil = true;
    thisSys->m_ValidASHRAEHeatCoil = true;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, thisSys->m_SensibleLoadMet, 0.025); // Watts
    // test simulate function return value for sysOutputRequired
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, sensOut, 0.025);     // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxCoolAirMassFlow * thisSys->m_PartLoadFrac); // cycling fan
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);

    // turn on dehumidification control and check that moisture load is < 0
    thisSys->m_DehumidControlType_Num = UnitarySys::DehumCtrlType::CoolReheat;
    thisSys->m_RunOnLatentLoad = true;
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
    EXPECT_LT(thisSys->m_MoistureLoadPredicted, 0.0); // dehumidification control type = CoolReheat so MoistureLoad < 0
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_VSDXCoilSizing)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({

        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",

        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,    !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,   !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,      !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:DX:VariableSpeed,       !- Heating Coil Object Type",
        "  Furnace Heating Coil 1, !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
        "  Furnace ACDXCoil 1,     !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80;                     !- Maximum Supply Air Temperature{ C }",

        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  1.6,                    !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Cooling:DX:SingleSpeed,",
        "  Furnace ACDXCoil 1,      !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  32000,                   !- Gross Rated Total Cooling Capacity {W}",
        "  0.75,                    !- Gross Rated Sensible Heat Ratio",
        "  3.0,                     !- Gross Rated Cooling COP {W/W}",
        "  1.6,                     !- Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Part Load Fraction Correlation Curve Name",
        "   ,                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  1000,                    !- Nominal Time for Condensate Removal to Begin {s}",
        "  0.4,                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  4,                       !- Maximum Cycling Rate {cycles/hr}",
        "  45;                      !- Latent Capacity Time Constant {s}",

        "Coil:Heating:DX:VariableSpeed, ",
        "  Furnace Heating Coil 1, !- Name",
        "  Heating Coil Air Inlet Node,  !- Indoor Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node,  !- Indoor Air Outlet Node Name",
        "  10.0,                    !- Number of Speeds {dimensionless}",
        "  10.0,                    !- Nominal Speed Level {dimensionless}",
        "  autosize,                !- Rated Heating Capacity At Selected Nominal Speed Level {w}",
        "  1.7,                     !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
        "  HPACCOOLPLFFPLR,         !- Energy Part Load Fraction Curve Name",
        "      ,                    !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "  -5.0,                    !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "  5.0,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "  200.0,                   !- Crankcase Heater Capacity {W}",
        "  10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  Resistive,               !- Defrost Strategy",
        "  TIMED,                   !- Defrost Control",
        "  0.166667,                !- Defrost Time Period Fraction",
        "  20000,                   !- Resistive Defrost Heater Capacity {W}",
        "  1838.7,                  !- Speed 1 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 1 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.1661088,               !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 1 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 1 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2295.5,                  !- Speed 2 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 2 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.179322,                !- Speed 2 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 2 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 2 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2751.3,                  !- Speed 3 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 3 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.1925352,               !- Speed 3 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 3 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 3 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  3659.6,                  !- Speed 4 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 4 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.2189616,               !- Speed 4 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 4 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 4 Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  4563.7,                  !- Speed 5 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 5 Reference Unit Gross Rated Heating COP {dimensionless}",
        "   0.245388,               !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 5 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 5 Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  5463.3,                  !- Speed 6 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 6 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.2718144,               !- Speed 6 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 6 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 6 Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "   6358.4,                 !- Speed 7 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 7 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.2982408,             !- Speed 7 Reference Unit Rated Air Flow Rate {m3/s}",
        "    HPACHeatCapFT,         !- Speed 7 Heating Capacity Function of Temperature Curve Name",
        "    HPACHeatCapFFF,        !- Speed 7 Heating Capacity Function of Air Flow Fraction Curve Name",
        "    HPACHeatEIRFT,         !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
        "    HPACHeatEIRFFF,        !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    7248.5,                !- Speed 8 Reference Unit Gross Rated Heating Capacity {w}",
        "    5.0,                   !- Speed 8 Reference Unit Gross Rated Heating COP {dimensionless}",
        "     0.3246672,            !- Speed 8 Reference Unit Rated Air Flow Rate {m3/s}",
        "    HPACHeatCapFT,         !- Speed 8 Heating Capacity Function of Temperature Curve Name",
        "    HPACHeatCapFFF,        !- Speed 8 Heating Capacity Function of Air Flow Fraction Curve Name",
        "    HPACHeatEIRFT,         !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
        "    HPACHeatEIRFFF,        !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    8133.6,                !- Speed 9 Reference Unit Gross Rated Heating Capacity {w}",
        "    5.0,                   !- Speed 9 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.3510936,             !- Speed 9 Reference Unit Rated Air Flow Rate {m3/s}",
        "    HPACHeatCapFT,         !- Speed 9 Heating Capacity Function of Temperature Curve Name",
        "    HPACHeatCapFFF,        !- Speed 9 Heating Capacity Function of Air Flow Fraction Curve Name",
        "     HPACHeatEIRFT,        !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
        "    HPACHeatEIRFFF,        !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    9013.2,                !- Speed 10 Reference Unit Gross Rated Heating Capacity {w}",
        "    5.0,                   !- Speed 10 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.37752,               !- Speed 10 Reference Unit Rated Air Flow Rate {m3/s}",
        "    HPACHeatCapFT,         !- Speed 10 Heating Capacity Function of Temperature Curve Name",
        "     HPACHeatCapFFF,       !- Speed 10 Heating Capacity Function of Air Flow Fraction Curve Name",
        "     HPACHeatEIRFT,        !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
        "      HPACHeatEIRFFF;      !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",

        "  Curve:Quadratic,",
        "    HPACHeatEIRFFF,          !- Name",
        "    1.3824,                  !- Coefficient1 Constant",
        "    -0.4336,                 !- Coefficient2 x",
        "    0.0512,                  !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACCOOLPLFFPLR,         !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Cubic,",
        "    HPACHeatCapFFF,          !- Name",
        "    0.84,                    !- Coefficient1 Constant",
        "    0.16,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "    Curve:Biquadratic,",
        "      HPACHeatCapFT,           !- Name",
        "      0.8529681407,            !- Coefficient1 Constant",
        "      -0.0004847169,           !- Coefficient2 x",
        "     -0.0000010693,            !- Coefficient3 x**2",
        "      0.0185542164,            !- Coefficient4 y",
        "      0.0000872425,            !- Coefficient5 y**2",
        "      -0.0000166868,           !- Coefficient6 x*y",
        "      17.78,                   !- Minimum Value of x",
        "      23.33,                   !- Maximum Value of x",
        "      -28.89,                  !- Minimum Value of y",
        "      17.22,                   !- Maximum Value of y",
        "      0.3799,                  !- Minimum Curve Output",
        "      1.1896,                  !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "    Curve:Biquadratic,",
        "      HPACHeatEIRFT,           !- Name",
        "      0.7077081462,            !- Coefficient1 Constant",
        "      0.0148163478,            !- Coefficient2 x",
        "      0.0002622589,            !- Coefficient3 x**2",
        "      -0.0113239622,           !- Coefficient4 y",
        "      0.0002939277,            !- Coefficient5 y**2",
        "      -0.0003605284,           !- Coefficient6 x*y",
        "      17.78,                   !- Minimum Value of x",
        "      23.33,                   !- Maximum Value of x",
        "      -28.89,                  !- Minimum Value of y",
        "      17.22,                   !- Maximum Value of y",
        "      0.8266,                  !- Minimum Curve Output",
        "      2.0277,                  !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1,    !- Name",
        "  FanAndCoilAvailSched,        !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8,                         !- Gas Burner Efficiency",
        "  32000,                       !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;           !- Air Outlet Node Name",

        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",

        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Curve:Quadratic,",
        "  WindACCoolCapFFF,       !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACEIRFFF,           !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACPLFFPLR,          !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  WindACCoolCapFT,        !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",

        "Curve:Biquadratic,",
        "  WindACEIRFT,            !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(state); // read zone equipment configuration and list objects

    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInput(state, compName, zoneEquipment, 0); // get UnitarySystem input from object above

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects

    ASSERT_EQ(thisSys->m_DesignHeatingCapacity, DataSizing::AutoSize);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_VarSpeedCoils)
{

    bool ErrorsFound(false);
    bool FirstHVACIteration(false);
    Real64 Qsens_sys(0.0);   // UnitarySystem delivered sensible capacity wrt zone
    Real64 ZoneTemp(0.0);    // control zone temperature
    int InletNode(0);        // UnitarySystem inlet node number
    int OutletNode(0);       // UnitarySystem outlet node number
    int ControlZoneNum(0);   // index to control zone

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",
        "  ",
        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
        "  ",
        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,    !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",
        "  ",
        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model, !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,         !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,   !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "  Furnace Heating Coil 1, !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:VariableSpeed, !- Cooling Coil Object Type",
        "  Furnace ACDXCoil 1,     !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80;                     !- Maximum Supply Air Temperature{ C }",
        "  ",
        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  300.0,                  !- Pressure Rise{ Pa }",
        "  1.6,                    !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Cooling:DX:VariableSpeed,",
        "  Furnace ACDXCoil 1, !- Name",
        "  DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node, !- Air Outlet Node Name",
        "  10.0, !- Number of Speeds{ dimensionless }",
        "  10.0, !- Nominal Speed Level{ dimensionless }",
        "  32000.0, !- Gross Rated Total Cooling Capacity At Selected Nominal Speed Level{ w }",
        "  1.6, !- Rated Air Flow Rate At Selected Nominal Speed Level{ m3 / s }",
        "  0.0, !- Nominal Time for Condensate to Begin Leaving the Coil{ s }",
        "  0.0, !- Initial Moisture Evaporation Rate Divided by Steady - State AC Latent Capacity{ dimensionless }",
        "  PLFFPLR, !- Energy Part Load Fraction Curve Name",
        "  , !- Condenser Air Inlet Node Name",
        "  AirCooled, !- Condenser Type",
        "  , !- Evaporative Condenser Pump Rated Power Consumption{ W }",
        "  200.0, !- Crankcase Heater Capacity{ W }",
        "  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  , !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  , !- Supply Water Storage Tank Name",
        "  , !- Condensate Collection Water Storage Tank Name",
        "  , !- Basin Heater Capacity{ W / K }",
        "  , !- Basin Heater Setpoint Temperature{ C }",
        "  , !- Basin Heater Operating Schedule Name",
        "  1524.1, !- Speed 1 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 1 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 1 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.1359072, !- Speed 1 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.26, !- Speed 1 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 1 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  1877.9, !- Speed 2 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 2 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 2 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.151008, !- Speed 2 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.30, !- Speed 2 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 2 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2226.6, !- Speed 3 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 3 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 3 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.1661088, !- Speed 3 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.33, !- Speed 3 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 3 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2911.3, !- Speed 4 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 4 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 4 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.1963104, !- Speed 4 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.38, !- Speed 4 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 4 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  3581.7, !- Speed 5 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 5 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 5 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.226512, !- Speed 5 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.44, !- Speed 5 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 5 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  4239.5, !- Speed 6 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 6 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 6 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.2567136, !- Speed 6 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.50, !- Speed 6 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 6 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 6 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 6 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  4885.7, !- Speed 7 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 7 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 7 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.2869152, !- Speed 7 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.57, !- Speed 7 Reference Unit Condenser Flow Rate{ m3 / s }",
        "  , !- Speed 7 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 7 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 7 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  5520.7, !- Speed 8 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 8 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 8 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.3171168, !- Speed 8 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.63, !- Speed 8 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 8 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 8 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 8 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  6144.8, !- Speed 9 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 9 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 9 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.3473184, !- Speed 9 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.69, !- Speed 9 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 9 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 9 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 9 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  6758.0, !- Speed 10 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 10 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 10 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.37752, !- Speed 10 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.74, !- Speed 10 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 10 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 10 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 10 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF;          !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  ",
        "Coil:Heating:Fuel,",
        "  Furnace Heating Coil 1, !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8,                    !- Gas Burner Efficiency",
        "  32000,                  !- Nominal Capacity{ W }",
        "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8, !- Gas Burner Efficiency",
        "  32000, !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;    !- Air Outlet Node Name",
        "  ",
        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",
        "  ",
        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Curve:Quadratic,",
        "  CoolCapFFF,       !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  COOLEIRFFF,           !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  PLFFPLR,          !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",
        "  ",
        "Curve:Biquadratic,",
        "  CoolCapFT,        !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  COOLEIRFT,            !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(state); // read zone equipment configuration and list objects

    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    std::string compName = "UNITARY SYSTEM MODEL";
    int compTypeOfNum = DataHVACGlobals::UnitarySys_AnyCoilType;
    bool zoneEquipment = true;
    UnitarySystems::UnitarySys::factory(state, compTypeOfNum, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInput(state, compName, zoneEquipment, 0); // get UnitarySystem input from object above

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects
    EXPECT_EQ(thisSys->UnitType, DataHVACGlobals::cFurnaceTypes(compTypeOfNum)); // compare UnitarySystem type string to valid type

    DataGlobals::SysSizingCalc = false; // DISABLE SIZING - don't call UnitarySystem::sizeSystem, much more work needed to set up sizing arrays

    InletNode = thisSys->AirInNode;
    OutletNode = thisSys->AirOutNode;
    ControlZoneNum = thisSys->NodeNumOfControlledZone;

    // set up unitary system inlet condtions
    DataLoopNode::Node(InletNode).Temp = 26.666667;             // AHRI condition 80F dry-bulb temp
    DataLoopNode::Node(InletNode).HumRat = 0.01117049542334198; // AHRI condition at 80F DB/67F WB lb/lb or kg/kg
    DataLoopNode::Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat);

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = DataLoopNode::Node(InletNode).Temp; // set zone temperature, used to determine system delivered capacity
    DataLoopNode::Node(ControlZoneNum).HumRat =
        DataLoopNode::Node(InletNode).HumRat; // set zone humidity ratio, used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;   // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    // initialize other incidentals that are used within the UnitarySystem module during calculations
    DataSizing::CurZoneEqNum = 1;
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 1000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = 2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = 1000.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP = 0.0;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP(1) =
        DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP;

    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;
    ScheduleManager::Schedule(1).CurrentValue = 1.0; // FanAndCoilAvailSchedule
    ScheduleManager::Schedule(2).CurrentValue = 1.0; // ContinuousFanSchedule
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0); // initialize RhoAir
    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail = thisSys->m_MaxCoolAirVolFlow * DataEnvironment::StdRhoAir;

    OutputReportPredefined::SetPredefinedTables();

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 0.01); // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxHeatAirMassFlow);                        // constant fan
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -1000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = -1000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = -2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 24.0; // set zone temperature during cooling season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;         // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 1.0); // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->m_CoolMassFlowRate[thisSys->m_CoolingSpeedNum]);
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_VarSpeedCoils_CyclingFan)
{

    bool ErrorsFound(false);
    bool FirstHVACIteration(false);
    Real64 Qsens_sys(0.0);   // UnitarySystem delivered sensible capacity wrt zone
    Real64 ZoneTemp(0.0);    // control zone temperature
    int InletNode(0);        // UnitarySystem inlet node number
    int OutletNode(0);       // UnitarySystem outlet node number
    int ControlZoneNum(0);   // index to control zone

    std::string const idf_objects = delimited_string({

        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",
        "  ",
        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
        "  ",
        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,    !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",
        "  ",
        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,   !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,      !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  CyclingFanSchedule,     !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Fuel,      !- Heating Coil Object Type",
        "  Furnace Heating Coil 1, !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:VariableSpeed, !- Cooling Coil Object Type",
        "  Furnace ACDXCoil 1,     !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,      !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80;                     !- Maximum Supply Air Temperature{ C }",
        "  ",
        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  1.6,                    !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Cooling:DX:VariableSpeed,",
        "  Furnace ACDXCoil 1, !- Name",
        "  DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node, !- Air Outlet Node Name",
        "  10.0, !- Number of Speeds{ dimensionless }",
        "  10.0, !- Nominal Speed Level{ dimensionless }",
        "  32000.0, !- Gross Rated Total Cooling Capacity At Selected Nominal Speed Level{ w }",
        "  1.6, !- Rated Air Flow Rate At Selected Nominal Speed Level{ m3 / s }",
        "  0.0, !- Nominal Time for Condensate to Begin Leaving the Coil{ s }",
        "  0.0, !- Initial Moisture Evaporation Rate Divided by Steady - State AC Latent Capacity{ dimensionless }",
        "  PLFFPLR, !- Energy Part Load Fraction Curve Name",
        "  , !- Condenser Air Inlet Node Name",
        "  AirCooled, !- Condenser Type",
        "  , !- Evaporative Condenser Pump Rated Power Consumption{ W }",
        "  200.0, !- Crankcase Heater Capacity{ W }",
        "  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  , !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  , !- Supply Water Storage Tank Name",
        "  , !- Condensate Collection Water Storage Tank Name",
        "  , !- Basin Heater Capacity{ W / K }",
        "  , !- Basin Heater Setpoint Temperature{ C }",
        "  , !- Basin Heater Operating Schedule Name",
        "  1524.1, !- Speed 1 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 1 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 1 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.1359072, !- Speed 1 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.26, !- Speed 1 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 1 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  1877.9, !- Speed 2 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 2 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 2 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.151008, !- Speed 2 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.30, !- Speed 2 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 2 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2226.6, !- Speed 3 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 3 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 3 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.1661088, !- Speed 3 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.33, !- Speed 3 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 3 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2911.3, !- Speed 4 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 4 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 4 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.1963104, !- Speed 4 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.38, !- Speed 4 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 4 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  3581.7, !- Speed 5 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 5 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 5 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.226512, !- Speed 5 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.44, !- Speed 5 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 5 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  4239.5, !- Speed 6 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 6 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 6 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.2567136, !- Speed 6 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.50, !- Speed 6 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 6 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 6 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 6 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  4885.7, !- Speed 7 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 7 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 7 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.2869152, !- Speed 7 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.57, !- Speed 7 Reference Unit Condenser Flow Rate{ m3 / s }",
        "  , !- Speed 7 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 7 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 7 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  5520.7, !- Speed 8 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 8 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 8 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.3171168, !- Speed 8 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.63, !- Speed 8 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 8 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 8 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 8 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  6144.8, !- Speed 9 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 9 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 9 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.3473184, !- Speed 9 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.69, !- Speed 9 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 9 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 9 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 9 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF, !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  6758.0, !- Speed 10 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75, !- Speed 10 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0, !- Speed 10 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.37752, !- Speed 10 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.74, !- Speed 10 Reference Unit Condenser Air Flow Rate{ m3 / s }",
        "  , !- Speed 10 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  CoolCapFT, !- Speed 10 Total Cooling Capacity Function of Temperature Curve Name",
        "  CoolCapFFF, !- Speed 10 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  COOLEIRFT, !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
        "  COOLEIRFFF;          !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  ",
        "Coil:Heating:Fuel,",
        "  Furnace Heating Coil 1, !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8,                    !- Gas Burner Efficiency",
        "  32000,                  !- Nominal Capacity{ W }",
        "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1, !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8,                    !- Gas Burner Efficiency",
        "  32000,                  !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;      !- Air Outlet Node Name",
        "  ",
        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",
        "  ",
        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Schedule:Compact,",
        "  CyclingFanSchedule,     !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 0.0;      !- Field 3",
        "  ",
        "Curve:Quadratic,",
        "  CoolCapFFF,             !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  COOLEIRFFF,             !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  PLFFPLR,                !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",
        "  ",
        "Curve:Biquadratic,",
        "  CoolCapFT,              !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  COOLEIRFT,              !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(state); // read zone equipment configuration and list objects

    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    std::string compName = "UNITARY SYSTEM MODEL";
    int compTypeOfNum = DataHVACGlobals::UnitarySys_AnyCoilType;
    bool zoneEquipment = true;
    UnitarySystems::UnitarySys::factory(state, compTypeOfNum, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInput(state, compName, zoneEquipment, 0); // get UnitarySystem input from object above

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects
    EXPECT_EQ(thisSys->UnitType, DataHVACGlobals::cFurnaceTypes(compTypeOfNum)); // compare UnitarySystem type string to valid type

    DataGlobals::SysSizingCalc = false; // DISABLE SIZING - don't call UnitarySystem::sizeSystem, much more work needed to set up sizing arrays

    InletNode = thisSys->AirInNode;
    OutletNode = thisSys->AirOutNode;
    ControlZoneNum = thisSys->NodeNumOfControlledZone;

    // set up unitary system inlet condtions
    DataLoopNode::Node(InletNode).Temp = 26.666667;             // AHRI condition 80F dry-bulb temp
    DataLoopNode::Node(InletNode).HumRat = 0.01117049542334198; // AHRI condition at 80F DB/67F WB lb/lb or kg/kg
    DataLoopNode::Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat);

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 20.0; // set zone temperature during heating season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;         // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    // initialize other incidentals that are used within the UnitarySystem module during calculations
    DataSizing::CurZoneEqNum = 1;
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 1000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = 2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = 1000.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP = 0.0;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP(1) =
        DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP;

    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;
    ScheduleManager::Schedule(1).CurrentValue = 1.0;
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0); // initialize RhoAir
    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail = thisSys->m_MaxCoolAirVolFlow * DataEnvironment::StdRhoAir;

    OutputReportPredefined::SetPredefinedTables();

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 0.01);    // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxHeatAirMassFlow * thisSys->m_PartLoadFrac); // cycling fan
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);

    // compare fan RTF with fan PLR and global PLF
    Real64 FanPLR = DataLoopNode::Node(InletNode).MassFlowRate / Fans::Fan(1).MaxAirMassFlowRate;
    Real64 FanRTF = FanPLR / DataHVACGlobals::OnOffFanPartLoadFraction;
    EXPECT_DOUBLE_EQ(FanRTF, FanPLR);
    EXPECT_DOUBLE_EQ(FanRTF, Fans::Fan(1).FanRuntimeFraction);
    EXPECT_DOUBLE_EQ(DataHVACGlobals::OnOffFanPartLoadFraction, 1.0);

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -1000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = -1000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = -2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 24.0; // set zone temperature during cooling season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;         // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 1.0); // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate,
                     thisSys->m_CoolMassFlowRate[thisSys->m_CoolingSpeedNum] * thisSys->m_PartLoadFrac); // cycling fan
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);

    // compare fan RTF with fan PLR and global PLF
    FanPLR = DataLoopNode::Node(InletNode).MassFlowRate / Fans::Fan(1).MaxAirMassFlowRate;
    // blow thru fan resets OnOffFanPartLoadFraction = 1 so other equipment not using PLF are not affected. OnOffFanPartLoadFraction = 1 here.
    // Unitary System also sets OnOffFanPartLoadFraction = 1 (see end of ReportUnitarySystem) so this variable will = 1
    EXPECT_EQ(1.0, DataHVACGlobals::OnOffFanPartLoadFraction);
    EXPECT_GT(Fans::Fan(1).FanRuntimeFraction, FanPLR);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_GetBadSupplyAirMethodInput)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",
        "  ",
        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
        "  ",
        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,    !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",
        "  ",
        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,   !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,      !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  FanAndCoilAvailSched,   !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "  Furnace Heating Coil 1, !- Heating Coil Name",
        "  1,                      !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
        "  Furnace ACDXCoil 1,     !- Cooling Coil Name",
        "  No,                     !- Use DOAS DX Cooling Coil",
        "  2,                      !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl, !- Latent Load Control",
        "  ,                       !- Supplemental Heating Coil Object Type",
        "  ,                       !- Supplemental Heating Coil Name",
        "  ,                       !- Supply Air Flow Rate Method During Cooling Operation",
        "  ,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Supply air Flow Rate Method During Heating Operation", // blank input not allowed with gas heating coil
        "  ,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                       !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  ,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80,                     !- Maximum Supply Air Temperature{ C }",
        "  21,                     !- Maximum Outdoor Dry - Bulb Temperature for Supplemental Heater Operation{ C }",
        "  ,                       !- Outdoor Dry - Bulb Temperature Sensor Node Name",
        "  2.5,                    !- Maximum Cycling Rate{ cycles / hr }",
        "  60,                     !- Heat Pump Time Constant{ s }",
        "  0.01,                   !- Fraction of On - Cycle Power Use",
        "  60,                     !- Heat Pump Fan Delay Time{ s }",
        "  ,                       !- Ancillary On - Cycle Electric Power{ W }",
        "  ,                       !- Ancillary Off - Cycle Electric Power{ W }",
        "  ,                       !- Design Heat Recovery Water Flow Rate{ m3 / s }",
        "  80;                     !- Maximum Temperature for Heat Recovery{ C }",
        "  ",
        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  1.6,                    !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Cooling:DX:SingleSpeed,",
        "  Furnace ACDXCoil 1,      !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  32000,                   !- Gross Rated Total Cooling Capacity {W}",
        "  0.75,                    !- Gross Rated Sensible Heat Ratio",
        "  3.0,                     !- Gross Rated Cooling COP {W/W}",
        "  1.6,                     !- Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  Biquadratic,             !- Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,               !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Biquadratic,             !- Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,               !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Quadratic,               !- Part Load Fraction Correlation Curve Name",
        "  ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  1000,                    !- Nominal Time for Condensate Removal to Begin {s}",
        "  0.4,                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  4,                       !- Maximum Cycling Rate {cycles/hr}",
        "  45;                      !- Latent Capacity Time Constant {s}",
        "  ",
        "Coil:Heating:Fuel,",
        "  Furnace Heating Coil 1, !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8,                    !- Gas Burner Efficiency",
        "  32000,                  !- Nominal Capacity{ W }",
        "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;      !- Air Outlet Node Name",
        "  ",
        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",
        "  ",
        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Curve:Quadratic,",
        "  Quadratic,              !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Biquadratic,",
        "  Biquadratic,            !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(state); // read zone equipment configuration and list objects

    HeatingCoils::GetCoilsInputFlag = true;
    HeatingCoils::HeatingCoil.deallocate();

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_TRUE(ErrorsFound);                                                           // expect error on ill-formed input
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_ReportingTest)
{

    bool ErrorsFound(false);
    int InletNode(0);      // UnitarySystem inlet node number
    int OutletNode(0);     // UnitarySystem outlet node number
    int ControlZoneNum(0); // index to control zone
    int AirLoopNum(0);     // UnitarySystem airloop index

    std::string const idf_objects = delimited_string({

        "  Zone,",
        "    SPACE2-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",

        "ZoneControl:Thermostat,",
        "  SPACE2-1 Thermostat,                                     !- Name",
        "  SPACE2-1,                                                !- Zone or ZoneList Name",
        "  HVACTemplate-Always 4,                                   !- Control Type Schedule Name",
        "  ThermostatSetpoint:DualSetpoint,                         !- Control Object Type",
        "  All Zones Dual SP Control;                               !- Control Name",

        "ZoneHVAC:EquipmentConnections,",
        "  SPACE2-1,                                                !- Zone Name",
        "  SPACE2-1 Equipment,                                      !- Zone Conditioning Equipment List Name",
        "  SPACE2-1 Zone Inlet Node,                                !- Zone Air Inlet Node or NodeList Name",
        "  SPACE2-1 Zone Exhaust Node,                              !- Zone Air Exhaust Node or NodeList Name",
        "  SPACE2-1 Zone Air Node,                                  !- Zone Air Node Name",
        "  SPACE2-1 Return Outlet;                                  !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        "  SPACE2-1 Equipment,                                      !- Name",
        "  SequentialLoad,                                          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem,                               !- Zone Equipment Object Type",
        "  Unitary System Model,                                    !- Zone Equipment Name",
        "  1,                                                       !- Zone Equipment Cooling Sequence",
        "  1,                                                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                                                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                                                        !- Zone Equipment 1 Sequential Heating Fraction",

        "ThermostatSetpoint:DualSetpoint,",
        "  All Zones Dual SP Control,                               !- Name",
        "  Htg-SetP-Sch,                                            !- Heating Setpoint Temperature Schedule Name",
        "  Clg-SetP-Sch;                                            !- Cooling Setpoint Temperature Schedule Name",

        "ScheduleTypeLimits,",
        "  HVACTemplate Any Number;                                 !- Name",

        "Schedule:Compact,",
        "  HVACTemplate-Always 4,                                   !- Name",
        "  HVACTemplate Any Number,                                 !- Schedule Type Limits Name",
        "  Through: 12/31,                                          !- Field 1",
        "  For: AllDays,                                            !- Field 2",
        "  Until: 24:00,                                            !- Field 3",
        "  4;                                                       !- Field 4",

        "  Schedule:Compact,",
        "    Htg-SetP-Sch,            !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2, !- Field 2",
        "    Until: 6:00,13.0,        !- Field 3",
        "    Until: 7:00,18.0,        !- Field 5",
        "    Until: 21:00,23.0,       !- Field 7",
        "    Until: 24:00,13.0,       !- Field 9",
        "    For: WeekEnds Holiday,   !- Field 11",
        "    Until: 24:00,13.0,       !- Field 12",
        "    For: SummerDesignDay,    !- Field 14",
        "    Until: 24:00,13.0,       !- Field 15",
        "    For: WinterDesignDay,    !- Field 17",
        "    Until: 24:00,23.0;       !- Field 18",

        "! For cooling, recover 1 hr early",

        "  Schedule:Compact,",
        "    Clg-SetP-Sch,            !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2, !- Field 2",
        "    Until: 7:00,32.0,        !- Field 3",
        "    Until: 21:00,24.0,       !- Field 5",
        "    Until: 24:00,32.0,       !- Field 7",
        "    For: WeekEnds Holiday,   !- Field 9",
        "    Until: 24:00,32.0,       !- Field 10",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00,24.0,       !- Field 13",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00,32.0;       !- Field 16",

        "  Schedule:Compact,",
        "    FanAvailSched,           !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2, !- Field 2",
        "    Until: 7:00,0.0,         !- Field 3",
        "    Until: 21:00,1.0,        !- Field 5",
        "    Until: 24:00,0.0,        !- Field 7",
        "    For: Weekends Holiday,   !- Field 9",
        "    Until: 24:00,0.0,        !- Field 10",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00,1.0,        !- Field 13",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00,1.0;        !- Field 16",

        "  Schedule:Compact,",
        "    Min OA Sched,            !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2, !- Field 2",
        "    Until: 8:00,0.0,         !- Field 3",
        "    Until: 21:00,1.0,        !- Field 5",
        "    Until: 24:00,0.0,        !- Field 7",
        "    For: Weekends Holiday,   !- Field 9",
        "    Until: 24:00,0.0,        !- Field 10",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00,1.0,        !- Field 13",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00,1.0;        !- Field 16",

        "  Sizing:Parameters,",
        "    1.2,                     !- Heating Sizing Factor",
        "    1.2;                     !- Cooling Sizing Factor",

        "AvailabilityManagerAssignmentList,",
        "  Sys 2 Furnace DX Cool MultiSpd Availability Managers,    !- Name",
        "  AvailabilityManager:Scheduled,                           !- Availability Manager Object Type",
        "  Sys 2 Furnace DX Cool MultiSpd Availability;             !- Availability Manager Name",

        "AvailabilityManager:Scheduled,",
        "  Sys 2 Furnace DX Cool MultiSpd Availability,             !- Name",
        "  HVACTemplate-Always 1;                                   !- Schedule Name",

        "Schedule:Compact,",
        "  HVACTemplate-Always 1,                                   !- Name",
        "  HVACTemplate Any Number,                                 !- Schedule Type Limits Name",
        "  Through: 12/31,                                          !- Field 1",
        "  For: AllDays,                                            !- Field 2",
        "  Until: 24:00,                                            !- Field 3",
        "  1;                                                       !- Field 4",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,                                    !- Name",
        "  Load,                                                    !- Control Type",
        "  SPACE2-1,                                                !- Controlling Zone or Thermostat Location",
        "  None,                                                    !- Dehumidification Control Type",
        "  ,                                                        !- Availability Schedule Name",
        "  SPACE2-1 Zone Exhaust Node,                              !- Air Inlet Node Name",
        "  SPACE2-1 Zone Inlet Node,                                !- Air Outlet Node Name",
        "  Fan:VariableVolume,                                      !- Supply Fan Object Type",
        "  Sys 2 Furnace DX Cool MultiSpd Supply Fan,               !- Supply Fan Name",
        "  DrawThrough,                                             !- Fan Placement",
        "  FanAvailSched,                                           !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Electric,                                   !- Heating Coil Object Type",
        "  Sys 2 Furnace DX Cool MultiSpd Heating Coil,             !- Heating Coil Name",
        "  1.0,                                                     !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:MultiSpeed,                              !- Cooling Coil Object Type",
        "  Sys 2 Furnace DX Cool MultiSpd Cooling Coil,             !- Cooling Coil Name",
        "  ,                                                        !- Use DOAS DX Cooling Coil",
        "  ,                                                        !- DOAS DX Cooling Coil Leaving Minimum Air Temperature {C}",
        "  ,                                                        !- Latent Load Control",
        "  ,                                                        !- Supplemental Heating Coil Object Type",
        "  ,                                                        !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,                                       !- Cooling Supply Air Flow Rate Method",
        "  0.23122,                                                 !- Cooling Supply Air Flow Rate {m3/s}",
        "  ,                                                        !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2",
        "  ,                                                        !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "  ,                                                        !- Cooling Supply Air Flow Rate Per Unit of Capacity {m3/s-W",
        "  SupplyAirFlowRate,                                       !- Heating Supply Air Flow Rate Method",
        "  0.23122,                                                 !- Heating Supply Air Flow Rate {m3/s}",
        "  ,                                                        !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2",
        "  ,                                                        !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "  ,                                                        !- Heating Supply Air Flow Rate Per Unit of Capacity {m3/s-W",
        "  SupplyAirFlowRate,                                       !- No Load Supply Air Flow Rate Method",
        "  0.23122,                                                 !- No Load Supply Air Flow Rate {m3/s}",
        "  ,                                                        !- No Load Supply Air Flow Rate Per Floor Area {m3/s-m2",
        "  ,                                                        !- No Load Fraction of Autosized Cooling Supply Air Flow Rate",
        "  ,                                                        !- No Load Fraction of Autosized Heating Supply Air Flow Rate",
        "  ,                                                        !- No Load Supply Air Flow Rate Per Unit of Capacity During Cooling Operation "
        "{m3/s-W",
        "  ,                                                        !- No Load Supply Air Flow Rate Per Unit of Capacity During Heating Operation "
        "{m3/s-W",
        "  Autosize,                                                !- Maximum Supply Air Temperature {C}",
        "  21,                                                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                                                        !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                                                        !- Maximum Cycling Rate",
        "  ,                                                        !- Heat Pump Time Constant",
        "  ,                                                        !- Fraction of On-Cycle Power Use",
        "  ,                                                        !- Heat Pump Fan Delay Time",
        "  ,                                                        !- Ancilliary On-Cycle Electric Power",
        "  ,                                                        !- Ancilliary Off-Cycle Electric Power",
        "  ,                                                        !- Design Heat Recovery Water Flow Rate",
        "  ,                                                        !- Maximum Temperature for Heat Recovery",
        "  ,                                                        !- Heat Recovery Water Inlet Node Name",
        "  ,                                                        !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,                     !- Design Specification Multispeed Object Type",
        "  Sys 2 Furnace DX Cool MultiSpd Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  Sys 2 Furnace DX Cool MultiSpd Unitary System MultiSpeed Performance,  !- Name",
        "  1,                                                       !- Number of Speeds for Heating",
        "  3,                                                       !- Number of Speeds for Cooling",
        "  No,                                                      !- Single Mode Operation",
        "  ,                                                        !- No Load Supply Air Flow Rate Ratio",
        "  1.0,                                                     !- Heating Speed 1 Supply Air Flow Ratio",
        "  0.333,                                                   !- Cooling Speed 1 Supply Air Flow Ratio",
        "  1.0,                                                     !- Heating Speed 2 Supply Air Flow Ratio",
        "  0.666,                                                   !- Cooling Speed 2 Supply Air Flow Ratio",
        "  1.0,                                                     !- Heating Speed 3 Supply Air Flow Ratio",
        "  1.0;                                                     !- Cooling Speed 3 Supply Air Flow Ratio",

        "Coil:Cooling:DX:MultiSpeed,",
        "  Sys 2 Furnace DX Cool MultiSpd Cooling Coil,             !- Name",
        "  ,                                                        !- Availability Schedule Name",
        "  SPACE2-1 Zone Exhaust Node,                              !- Air Inlet Node Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cooling Coil Outlet,      !- Air Outlet Node Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cooling Coil Condenser Inlet,  !- Condenser Air Inlet Node Name",
        "  AirCooled,                                               !- Condenser Type",
        "  ,                                                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                                                        !- Supply Water Storage Tank Name",
        "  ,                                                        !- Condensate Collection Water Storage Tank Name",
        "  No,                                                      !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No,                                                      !- Apply Latent Degradation to Speeds Greater than 1",
        "  0.0,                                                     !- Crankcase Heater Capacity {W}",
        "  10.0,                                                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  ,                                                        !- Basin Heater Capacity {W/K}",
        "  ,                                                        !- Basin Heater Setpoint Temperature {C}",
        "  ,                                                        !- Basin Heater Operating Schedule Name",
        "  Electricity,                                             !- Fuel Type",
        "  3,                                                       !- Number of Speeds",
        "  1459.77157,                                              !- Speed 1 Gross Rated Total Cooling Capacity {W}",
        "  0.75232,                                                 !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  3,                                                       !- Speed 1 Gross Rated Cooling COP {W/W}",
        "  7.70720E-002,                                            !- Speed 1 Rated Air Flow Rate {m3/s}",
        "  ,                                                        !- Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil Cap-FT,         !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil Cap-FF,         !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil EIR-FT,         !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil EIR-FF,         !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil PLF,            !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  0,                                                       !- Speed 1 Nominal Time for Condensate Removal to Begin",
        "  0,                                                       !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent "
        "Capacity",
        "  0,                                                       !- Speed 1 Maximum Cycling Rate",
        "  0,                                                       !- Speed 1 Latent Capacity Time Constant",
        "  0.2,                                                     !- Speed 1 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil WH-FT,          !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  ,                                                        !- Speed 1 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                                                        !- Speed 1 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ,                                                        !- Speed 1 Rated Evaporative Condenser Pump Power Consumption {W}",
        "  2919.54314,                                              !- Speed 2 Gross Rated Total Cooling Capacity {W}",
        "  0.75232,                                                 !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  3,                                                       !- Speed 2 Gross Rated Cooling COP {W/W}",
        "  0.15414,                                                 !- Speed 2 Rated Air Flow Rate {m3/s}",
        "  ,                                                        !- Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil Cap-FT,         !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil Cap-FF,         !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil EIR-FT,         !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil EIR-FF,         !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil PLF,            !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  0,                                                       !- Speed 2 Nominal Time for Condensate Removal to Begin",
        "  0,                                                       !- Speed 2 Ratio of Initial Moisture Evaporation Rate and Steady State Latent "
        "Capacity",
        "  0,                                                       !- Speed 2 Maximum Cycling Rate",
        "  0,                                                       !- Speed 2 Latent Capacity Time Constant",
        "  0.2,                                                     !- Speed 2 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil WH-FT,          !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  ,                                                        !- Speed 2 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                                                        !- Speed 2 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ,                                                        !- Speed 2 Rated Evaporative Condenser Pump Power Consumption {W}",
        "  4379.31471,                                              !- Speed 3 Gross Rated Total Cooling Capacity {W}",
        "  0.75232,                                                 !- Speed 3 Gross Rated Sensible Heat Ratio",
        "  3,                                                       !- Speed 3 Gross Rated Cooling COP {W/W}",
        "  0.23122,                                                 !- Speed 3 Rated Air Flow Rate {m3/s}",
        "  ,                                                        !- Speed 3 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil Cap-FT,         !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil Cap-FF,         !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil EIR-FT,         !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil EIR-FF,         !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil PLF,            !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  0,                                                       !- Speed 3 Nominal Time for Condensate Removal to Begin",
        "  0,                                                       !- Speed 3 Ratio of Initial Moisture Evaporation Rate and Steady State Latent "
        "Capacity",
        "  0,                                                       !- Speed 3 Maximum Cycling Rate",
        "  0,                                                       !- Speed 3 Latent Capacity Time Constant",
        "  0.2,                                                     !- Speed 3 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil WH-FT,          !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  ,                                                        !- Speed 3 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                                                        !- Speed 3 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ;                                                        !- Speed 3 Rated Evaporative Condenser Pump Power Consumption {W}",

        "Curve:Biquadratic,",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil Cap-FT,         !- Name",
        "  0.476428E+00,                                            !- Coefficient1 Constant",
        "  0.401147E-01,                                            !- Coefficient2 x",
        "  0.226411E-03,                                            !- Coefficient3 x**2",
        "  -0.827136E-03,                                           !- Coefficient4 y",
        "  -0.732240E-05,                                           !- Coefficient5 y**2",
        "  -0.446278E-03,                                           !- Coefficient6 x*y",
        "  0.0,                                                     !- Minimum Value of x",
        "  50.0,                                                    !- Maximum Value of x",
        "  0.0,                                                     !- Minimum Value of y",
        "  50.0,                                                    !- Maximum Value of y",
        "  0.0,                                                     !- Minimum Curve Output",
        "  5.0,                                                     !- Maximum Curve Output",
        "  Temperature,                                             !- Input Unit Type for X",
        "  Temperature,                                             !- Input Unit Type for Y",
        "  Dimensionless;                                           !- Output Unit Type",

        "Curve:Cubic,",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil Cap-FF,         !- Name",
        "  .47278589,                                               !- Coefficient1 Constant",
        "  1.2433415,                                               !- Coefficient2 x",
        "  -1.0387055,                                              !- Coefficient3 x**2",
        "  .32257813,                                               !- Coefficient4 x**3",
        "  0.5,                                                     !- Minimum Value of x",
        "  1.5;                                                     !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil EIR-FT,         !- Name",
        "  0.632475E+00,                                            !- Coefficient1 Constant",
        "  -0.121321E-01,                                           !- Coefficient2 x",
        "  0.507773E-03,                                            !- Coefficient3 x**2",
        "  0.155377E-01,                                            !- Coefficient4 y",
        "  0.272840E-03,                                            !- Coefficient5 y**2",
        "  -0.679201E-03,                                           !- Coefficient6 x*y",
        "  0.0,                                                     !- Minimum Value of x",
        "  50.0,                                                    !- Maximum Value of x",
        "  0.0,                                                     !- Minimum Value of y",
        "  50.0,                                                    !- Maximum Value of y",
        "  0.0,                                                     !- Minimum Curve Output",
        "  5.0,                                                     !- Maximum Curve Output",
        "  Temperature,                                             !- Input Unit Type for X",
        "  Temperature,                                             !- Input Unit Type for Y",
        "  Dimensionless;                                           !- Output Unit Type",

        "Curve:Cubic,",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil EIR-FF,         !- Name",
        "  .47278589,                                               !- Coefficient1 Constant",
        "  1.2433415,                                               !- Coefficient2 x",
        "  -1.0387055,                                              !- Coefficient3 x**2",
        "  .32257813,                                               !- Coefficient4 x**3",
        "  0.5,                                                     !- Minimum Value of x",
        "  1.5;                                                     !- Maximum Value of x",

        "Curve:Quadratic,",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil PLF,            !- Name",
        "  0.85,                                                    !- Coefficient1 Constant",
        "  0.15,                                                    !- Coefficient2 x",
        "  0,                                                       !- Coefficient3 x**2",
        "  0,                                                       !- Minimum Value of x",
        "  1;                                                       !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Sys 2 Furnace DX Cool MultiSpd Cool Coil WH-FT,          !- Name",
        "  1.0,                                                     !- Coefficient1 Constant",
        "  0.0,                                                     !- Coefficient2 x",
        "  0.0,                                                     !- Coefficient3 x**2",
        "  0.0,                                                     !- Coefficient4 y",
        "  0.0,                                                     !- Coefficient5 y**2",
        "  0.0,                                                     !- Coefficient6 x*y",
        "  0,                                                       !- Minimum Value of x",
        "  50,                                                      !- Maximum Value of x",
        "  0,                                                       !- Minimum Value of y",
        "  50,                                                      !- Maximum Value of y",
        "  ,                                                        !- Minimum Curve Output",
        "  ,                                                        !- Maximum Curve Output",
        "  Temperature,                                             !- Input Unit Type for X",
        "  Temperature,                                             !- Input Unit Type for Y",
        "  Dimensionless;                                           !- Output Unit Type",

        "OutdoorAir:Node,",
        "  Sys 2 Furnace DX Cool MultiSpd Cooling Coil Condenser Inlet,  !- Name",
        "  -1;                                                      !- Height Above Ground",

        "Coil:Heating:Electric,",
        "  Sys 2 Furnace DX Cool MultiSpd Heating Coil,             !- Name",
        "  ,                                                        !- Availability Schedule Name",
        "  1,                                                       !- Efficiency",
        "  0.23122,                                                 !- Nominal Capacity of the Coil {W}",
        "  Sys 2 Furnace DX Cool MultiSpd Cooling Coil Outlet,      !- Air Inlet Node Name",
        "  Sys 2 Furnace DX Cool MultiSpd Heating Coil Outlet,      !- Air Outlet Node Name",
        "  ;                                                        !- Coil Temp Setpoint Node",

        "Fan:VariableVolume,",
        "  Sys 2 Furnace DX Cool MultiSpd Supply Fan,               !- Name",
        "  HVACTemplate-Always 1,                                   !- Availability Schedule Name",
        "  0.7,                                                     !- Fan Efficiency",
        "  600,                                                     !- Pressure Rise {Pa}",
        "  0.23122,                                                 !- Maximum Flow Rate {m3/s}",
        "  Fraction,                                                !- Fan Power Minimum Flow Rate Input Method",
        "  0.0,                                                     !- Fan Power Minimum Flow Fraction",
        "  ,                                                        !- Fan Power Minimum Air Flow Rate {m3/s}",
        "  0.9,                                                     !- Motor Efficiency",
        "  1,                                                       !- Motor in Airstream Fraction",
        "  0.0015302446,                                            !- Fan Power Coefficient 1",
        "  0.0052080574,                                            !- Fan Power Coefficient 2",
        "  1.1086242,                                               !- Fan Power Coefficient 3",
        "  -0.11635563,                                             !- Fan Power Coefficient 4",
        "  0,                                                       !- Fan Power Coefficient 5",
        "  Sys 2 Furnace DX Cool MultiSpd Heating Coil Outlet,      !- Air Inlet Node Name",
        "  SPACE2-1 Zone Inlet Node;                                !- Air Outlet Node Name",

        "OutdoorAir:NodeList,",
        "  Sys 2 Furnace DX Cool MultiSpd Outdoor Air Inlet;        !- Node or NodeList Name 1",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(state);

    HeatBalanceManager::GetZoneData(state, ErrorsFound);   // read zone data
    EXPECT_FALSE(ErrorsFound);                      // expect no errors
    DataZoneEquipment::GetZoneEquipmentData(state); // read zone equipment

    std::string compName = "UNITARY SYSTEM MODEL";
    int compTypeOfNum = DataHVACGlobals::UnitarySys_AnyCoilType;
    bool zoneEquipment = true;
    UnitarySystems::UnitarySys::factory(state, compTypeOfNum, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects
    EXPECT_EQ(thisSys->UnitType, DataHVACGlobals::cFurnaceTypes(compTypeOfNum)); // compare UnitarySystem type string to valid type

    InletNode = thisSys->AirInNode;
    OutletNode = thisSys->AirOutNode;
    ControlZoneNum = thisSys->NodeNumOfControlledZone;

    AirLoopNum = 0;
    UnitarySystems::HeatingLoad = false;
    UnitarySystems::CoolingLoad = false;

    // zone predicted load is assume to be heating and the unitary system zone equipment
    // inlet and outlet air conditions were set for heating
    UnitarySystems::HeatingLoad = true;
    // set up zone equipment inlet node condtions
    DataLoopNode::Node(InletNode).Temp = 17.57;
    DataLoopNode::Node(InletNode).HumRat = 0.007;
    DataLoopNode::Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat);
    DataLoopNode::Node(InletNode).MassFlowRate = 0.25;
    // set  zone equipment outlet node conditions
    DataLoopNode::Node(OutletNode).Temp = 21.1;
    DataLoopNode::Node(OutletNode).HumRat = 0.007;
    DataLoopNode::Node(OutletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat);
    DataLoopNode::Node(OutletNode).MassFlowRate = 0.25;
    // set zone conditions
    DataLoopNode::Node(ControlZoneNum).Temp = 23.0;
    DataLoopNode::Node(ControlZoneNum).HumRat = 0.0070;
    DataLoopNode::Node(ControlZoneNum).Enthalpy =
        Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(ControlZoneNum).Temp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // calculate the "Unitary System Total Cooling/Heating Rate" report variables
    thisSys->reportUnitarySystem(state, AirLoopNum);
    EXPECT_NEAR(483.5, thisSys->m_TotCoolEnergyRate, 1.0);
    EXPECT_EQ(0.0, thisSys->m_TotHeatEnergyRate);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_MultispeedDXCoilSizing)
{

    std::string const idf_objects = delimited_string({

        "  Timestep,6;",

        "  Site:Location,",
        "    USA IL-CHICAGO-OHARE,    !- Name",
        "    41.77,                   !- Latitude {deg}",
        "    -87.75,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190;                     !- Elevation {m}",

        "SimulationControl, YES, NO, NO, YES, NO;",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO Ann Clg .4% Condns WB=>MDB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    31.2,                    !- Maximum Dry-Bulb Temperature {C}",
        "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    25.5,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    5.3,                     !- Wind Speed {m/s}",
        "    230,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.00;                    !- Sky Clearness",

        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",

        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,    !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,   !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,      !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:DX:VariableSpeed,       !- Heating Coil Object Type",
        "  VS Heating Coil 1,      !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:MultiSpeed, !- Cooling Coil Object Type",
        "  MultiSpd Cooling Coil,  !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
        "  FlowPerCoolingCapacity, !- Supply Air Flow Rate Method During Cooling Operation",
        "  ,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  0.00005,                !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  FlowPerHeatingCapacity, !- Supply air Flow Rate Method During Heating Operation",
        "  ,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  0.00005,                !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80,                     !- Maximum Supply Air Temperature{ C }",
        "  21,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                        !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                        !- Maximum Cycling Rate {cycles/hr}",
        "  ,                        !- Heat Pump Time Constant {s}",
        "  ,                        !- Fraction of On-Cycle Power Use",
        "  ,                        !- Heat Pump Fan Delay Time {s}",
        "  ,                        !- Ancillary On-Cycle Electric Power {W}",
        "  ,                        !- Ancillary Off-Cycle Electric Power {W}",
        "  ,                        !- Design Heat Recovery Water Flow Rate {m3/s}",
        "  ,                        !- Maximum Temperature for Heat Recovery {C}",
        "  ,                        !- Heat Recovery Water Inlet Node Name",
        "  ,                        !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,  !- Design Specification Multispeed Heat Pump Object Type",
        "  MultiSpeed Performance;  !- Design Specification Multispeed Heat Pump Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  MultiSpeed Performance,  !- Name",
        "  10,                      !- Number of Speeds for Heating",
        "  3,                       !- Number of Speeds for Cooling",
        "  No,                      !- Single Mode Operation",
        "  ,                        !- No Load Supply Air Flow Rate Ratio",
        "  autosize,                !- Heating Speed 1 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 1 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 2 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 2 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 3 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 3 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 4 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 4 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 5 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 5 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 6 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 6 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 7 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 7 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 8 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 8 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 9 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 9 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 10 Supply Air Flow Ratio",
        "  autosize;                !- Cooling Speed 10 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  autosize,               !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Cooling:DX:MultiSpeed,",
        "  MultiSpd Cooling Coil,   !- Name",
        ",                          !- Availability Schedule Name",
        "  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  ,                        !- Condenser Air Inlet Node Name",
        "  AirCooled,               !- Condenser Type",
        "  ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                        !- Supply Water Storage Tank Name",
        "  ,                        !- Condensate Collection Water Storage Tank Name",
        "  No,                      !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No,                      !- Apply Latent Degradation to Speeds Greater than 1",
        "  0.0,                     !- Crankcase Heater Capacity {W}",
        "  10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  ,                        !- Basin Heater Capacity {W/K}",
        "  ,                        !- Basin Heater Setpoint Temperature {C}",
        "  ,                        !- Basin Heater Operating Schedule Name",
        "  Electricity,             !- Fuel Type",
        "  3,                       !- Number of Speeds",
        "  autosize,                !- Speed 1 Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  3,                       !- Speed 1 Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Speed 1 Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  WindACCoolCapFT,         !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  0,                       !- Speed 1 Nominal Time for Condensate Removal to Begin {s}",
        "  0,                       !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  0,                       !- Speed 1 Maximum Cycling Rate {cycles/hr}",
        "  0,                       !- Speed 1 Latent Capacity Time Constant {s}",
        "  0.2,                     !- Speed 1 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  WindACCoolCapFT,         !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  ,                        !- Speed 1 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Speed 1 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ,                        !- Speed 1 Rated Evaporative Condenser Pump Power Consumption {W}",
        "  autosize,                !- Speed 2 Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  3,                       !- Speed 2 Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Speed 2 Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  WindACCoolCapFT,         !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  0,                       !- Speed 2 Nominal Time for Condensate Removal to Begin {s}",
        "  0,                       !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
        "  0,                       !- Speed 2 Maximum Cycling Rate {cycles/hr}",
        "  0,                       !- Speed 2 Latent Capacity Time Constant {s}",
        "  0.2,                     !- Speed 2 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  WindACCoolCapFT,         !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  ,                        !- Speed 2 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Speed 2 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ,                        !- Speed 2 Rated Evaporative Condenser Pump Power Consumption {W}",
        "  autosize,                !- Speed 3 Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Speed 3 Gross Rated Sensible Heat Ratio",
        "  3,                       !- Speed 3 Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Speed 3 Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  WindACCoolCapFT,         !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  0,                       !- Speed 3 Nominal Time for Condensate Removal to Begin {s}",
        "  0,                       !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
        "  0,                       !- Speed 3 Maximum Cycling Rate {cycles/hr}",
        "  0,                       !- Speed 3 Latent Capacity Time Constant {s}",
        "  0.2,                     !- Speed 3 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  WindACCoolCapFT,         !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  ,                        !- Speed 3 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Speed 3 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ;                        !- Speed 3 Rated Evaporative Condenser Pump Power Consumption {W}",

        "Coil:Heating:DX:VariableSpeed, ",
        "  VS Heating Coil 1,       !- Name",
        "  Heating Coil Air Inlet Node,  !- Indoor Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node,  !- Indoor Air Outlet Node Name",
        "  10.0,                    !- Number of Speeds {dimensionless}",
        "  10.0,                    !- Nominal Speed Level {dimensionless}",
        "  autosize,                !- Rated Heating Capacity At Selected Nominal Speed Level {w}",
        "  autosize,                !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
        "  HPACCOOLPLFFPLR,         !- Energy Part Load Fraction Curve Name",
        "      ,                    !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "  -5.0,                    !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "  5.0,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "  200.0,                   !- Crankcase Heater Capacity {W}",
        "  10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  Resistive,               !- Defrost Strategy",
        "  TIMED,                   !- Defrost Control",
        "  0.166667,                !- Defrost Time Period Fraction",
        "  20000,                   !- Resistive Defrost Heater Capacity {W}",
        "  1838.7,                  !- Speed 1 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 1 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.1661088,               !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 1 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 1 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2295.5,                  !- Speed 2 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 2 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.179322,                !- Speed 2 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 2 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 2 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  2751.3,                  !- Speed 3 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 3 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.1925352,               !- Speed 3 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 3 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 3 Total  Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  3659.6,                  !- Speed 4 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 4 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.2189616,               !- Speed 4 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 4 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 4 Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  4563.7,                  !- Speed 5 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 5 Reference Unit Gross Rated Heating COP {dimensionless}",
        "   0.245388,               !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 5 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 5 Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  5463.3,                  !- Speed 6 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 6 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.2718144,               !- Speed 6 Reference Unit Rated Air Flow Rate {m3/s}",
        "  HPACHeatCapFT,           !- Speed 6 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 6 Heating Capacity Function of Air Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "   6358.4,                 !- Speed 7 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                     !- Speed 7 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.2982408,             !- Speed 7 Reference Unit Rated Air Flow Rate {m3/s}",
        "    HPACHeatCapFT,         !- Speed 7 Heating Capacity Function of Temperature Curve Name",
        "    HPACHeatCapFFF,        !- Speed 7 Heating Capacity Function of Air Flow Fraction Curve Name",
        "    HPACHeatEIRFT,         !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
        "    HPACHeatEIRFFF,        !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    7248.5,                !- Speed 8 Reference Unit Gross Rated Heating Capacity {w}",
        "    5.0,                   !- Speed 8 Reference Unit Gross Rated Heating COP {dimensionless}",
        "     0.3246672,            !- Speed 8 Reference Unit Rated Air Flow Rate {m3/s}",
        "    HPACHeatCapFT,         !- Speed 8 Heating Capacity Function of Temperature Curve Name",
        "    HPACHeatCapFFF,        !- Speed 8 Heating Capacity Function of Air Flow Fraction Curve Name",
        "    HPACHeatEIRFT,         !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
        "    HPACHeatEIRFFF,        !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    8133.6,                !- Speed 9 Reference Unit Gross Rated Heating Capacity {w}",
        "    5.0,                   !- Speed 9 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.3510936,             !- Speed 9 Reference Unit Rated Air Flow Rate {m3/s}",
        "    HPACHeatCapFT,         !- Speed 9 Heating Capacity Function of Temperature Curve Name",
        "    HPACHeatCapFFF,        !- Speed 9 Heating Capacity Function of Air Flow Fraction Curve Name",
        "     HPACHeatEIRFT,        !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
        "    HPACHeatEIRFFF,        !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "    9013.2,                !- Speed 10 Reference Unit Gross Rated Heating Capacity {w}",
        "    5.0,                   !- Speed 10 Reference Unit Gross Rated Heating COP {dimensionless}",
        "    0.37752,               !- Speed 10 Reference Unit Rated Air Flow Rate {m3/s}",
        "    HPACHeatCapFT,         !- Speed 10 Heating Capacity Function of Temperature Curve Name",
        "     HPACHeatCapFFF,       !- Speed 10 Heating Capacity Function of Air Flow Fraction Curve Name",
        "     HPACHeatEIRFT,        !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
        "      HPACHeatEIRFFF;      !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",

        "  Curve:Quadratic,",
        "    HPACHeatEIRFFF,          !- Name",
        "    1.3824,                  !- Coefficient1 Constant",
        "    -0.4336,                 !- Coefficient2 x",
        "    0.0512,                  !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACCOOLPLFFPLR,         !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Cubic,",
        "    HPACHeatCapFFF,          !- Name",
        "    0.84,                    !- Coefficient1 Constant",
        "    0.16,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "    Curve:Biquadratic,",
        "      HPACHeatCapFT,           !- Name",
        "      0.8529681407,            !- Coefficient1 Constant",
        "      -0.0004847169,           !- Coefficient2 x",
        "     -0.0000010693,            !- Coefficient3 x**2",
        "      0.0185542164,            !- Coefficient4 y",
        "      0.0000872425,            !- Coefficient5 y**2",
        "      -0.0000166868,           !- Coefficient6 x*y",
        "      17.78,                   !- Minimum Value of x",
        "      23.33,                   !- Maximum Value of x",
        "      -28.89,                  !- Minimum Value of y",
        "      17.22,                   !- Maximum Value of y",
        "      0.3799,                  !- Minimum Curve Output",
        "      1.1896,                  !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "    Curve:Biquadratic,",
        "      HPACHeatEIRFT,           !- Name",
        "      0.7077081462,            !- Coefficient1 Constant",
        "      0.0148163478,            !- Coefficient2 x",
        "      0.0002622589,            !- Coefficient3 x**2",
        "      -0.0113239622,           !- Coefficient4 y",
        "      0.0002939277,            !- Coefficient5 y**2",
        "      -0.0003605284,           !- Coefficient6 x*y",
        "      17.78,                   !- Minimum Value of x",
        "      23.33,                   !- Maximum Value of x",
        "      -28.89,                  !- Minimum Value of y",
        "      17.22,                   !- Maximum Value of y",
        "      0.8266,                  !- Minimum Curve Output",
        "      2.0277,                  !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8, !- Gas Burner Efficiency",
        "  32000, !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;    !- Air Outlet Node Name",

        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",

        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Curve:Quadratic,",
        "  WindACCoolCapFFF,       !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACEIRFFF,           !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACPLFFPLR,          !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  WindACCoolCapFT,        !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",

        "Curve:Biquadratic,",
        "  WindACEIRFT,            !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",

        "  Sizing:Zone,",
        "    EAST ZONE,      !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    12.8000,                 !- Zone Cooling Design Supply Air Temperature {C}",
        "    ,                        !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    50.0000,                 !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.0085,                  !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.0080,                  !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA Zone One,        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Zone Heating Sizing Factor",
        "    ,                        !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    ,                        !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    ,                        !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    ;                        !- Heating Maximum Air Flow Fraction",

        "  DesignSpecification:OutdoorAir,",
        "    SZ DSOA Zone One,  !- Name",
        "    Flow/Person,             !- Outdoor Air Method",
        "    0.0125;                  !- Outdoor Air Flow per Person {m3/s-person}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "     ,                       !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Adiabatic,               !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0.000000,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.000000,15.24000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000,15.24000,4.572,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,4.572,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0.000000,4.572,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572;  !- X,Y,Z ==> Vertex 4 {m}",

        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",

        "  Construction,",
        "   FLOOR,                   !- Name",
        "    C5 - 4 IN HW CONCRETE;   !- Outside Layer",

        "  Construction,",
        "    ROOF31,                  !- Name",
        "    R31LAYER;                !- Outside Layer",

        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    R31LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    5.456,                   !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    C5 - 4 IN HW CONCRETE,   !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  ZoneControl:Thermostat,",
        "    EAST ZONE Thermostat,    !- Name",
        "    EAST ZONE,               !- Zone or ZoneList Name",
        "    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    Zone One DualSPSched;    !- Control 1 Name",

        "  Schedule:Compact,",
        "    Dual Zone Control Type Sched,  !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,4;          !- Field 3",

        "  ThermostatSetpoint:DualSetpoint,",
        "    Zone One DualSPSched,    !- Name",
        "    HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
        "    CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",

        "  Schedule:Compact,",
        "    CLGSETP_SCH,             !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 19",
        "    Until: 24:00,26.7;       !- Field 20",

        "  Schedule:Compact,",
        "    HTGSETP_SCH,             !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 22",
        "    Until: 24:00,15.6;       !- Field 23",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    SimulationManager::GetProjectData(state);
    createFacilityElectricPowerServiceObject();
    DataGlobals::BeginSimFlag = true;
    DataGlobals::DoingSizing = true;
    SizingManager::ManageSizing(state);
    DataGlobals::DoingSizing = false;
    DataGlobals::SysSizingCalc = false;
    DataSizing::CurZoneEqNum = 1;
    DataSizing::ZoneEqSizing.allocate(1);

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool ErrorsFound = false;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above

    OutputReportPredefined::SetPredefinedTables();

    DataSizing::ZoneSizingRunDone = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesignSizeFromParent = false;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod.allocate(25);
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;

    bool FirstHVACIteration = true;
    int AirLoopNum = 0;

    thisSys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

    EXPECT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects

    EXPECT_EQ(thisSys->m_DesignHeatingCapacity * 0.00005, thisSys->m_MaxHeatAirVolFlow);
    EXPECT_EQ(thisSys->m_DesignCoolingCapacity * 0.00005, thisSys->m_MaxCoolAirVolFlow);
    EXPECT_EQ(thisSys->m_DesignCoolingCapacity, DXCoils::DXCoil(thisSys->m_CoolingCoilIndex).MSRatedTotCap(thisSys->m_NumOfSpeedCooling));
    // 64-bit MSVS shows these next variables as identical yet other compilers show diff's, changing ASSERT_EQ to EXPECT_NEAR
    EXPECT_NEAR(thisSys->m_DesignHeatingCapacity,
                state.dataVariableSpeedCoils->VarSpeedCoil(thisSys->m_HeatingCoilIndex).MSRatedTotCap(thisSys->m_NumOfSpeedHeating),
                0.001);

    // 3 cooling speeds with autosized MSHP design spec yielding equally distributed air flow at 1/3 per speed
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[1], 0.032774, 0.000001);
    EXPECT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(1), thisSys->m_CoolVolumeFlowRate[1], 0.000001);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[2], 0.065549, 0.000001);
    EXPECT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(2), thisSys->m_CoolVolumeFlowRate[2], 0.000001);
    EXPECT_NEAR(thisSys->m_CoolVolumeFlowRate[3], 0.098323, 0.000001);
    EXPECT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(3), thisSys->m_CoolVolumeFlowRate[3], 0.000001);

    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[0], 0.333333, 0.000001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[1], 0.666666, 0.000001);
    EXPECT_NEAR(UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[2], 1.000000, 0.000001);

    EXPECT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(1),
                thisSys->m_MaxCoolAirVolFlow * UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[0],
                0.000001);
    EXPECT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(2),
                thisSys->m_MaxCoolAirVolFlow * UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[1],
                0.000001);
    EXPECT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(3),
                thisSys->m_MaxCoolAirVolFlow * UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[2],
                0.000001);

    // 10 heating speeds with autosized MSHP design spec yielding equally distributed air flow at 1/10 per speed
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[1], 0.008237, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(1), thisSys->m_HeatVolumeFlowRate[1]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[2], 0.016473, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(2), thisSys->m_HeatVolumeFlowRate[2]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[3], 0.024710, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(3), thisSys->m_HeatVolumeFlowRate[3]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[4], 0.032946, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(4), thisSys->m_HeatVolumeFlowRate[4]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[5], 0.041183, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(5), thisSys->m_HeatVolumeFlowRate[5]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[6], 0.049420, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(6), thisSys->m_HeatVolumeFlowRate[6]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[7], 0.057656, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(7), thisSys->m_HeatVolumeFlowRate[7]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[8], 0.065892, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(8), thisSys->m_HeatVolumeFlowRate[8]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[9], 0.074129, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(9), thisSys->m_HeatVolumeFlowRate[9]);
    EXPECT_NEAR(thisSys->m_HeatVolumeFlowRate[10], 0.082366, 0.000001);
    EXPECT_EQ(state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(10), thisSys->m_HeatVolumeFlowRate[10]);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_WaterToAirHeatPump)
{

    bool ErrorsFound(false);
    bool FirstHVACIteration(false);
    Real64 Qsens_sys(0.0);   // UnitarySystem delivered sensible capacity wrt zone
    Real64 ZoneTemp(0.0);    // control zone temperature
    int InletNode(0);        // UnitarySystem inlet node number
    int OutletNode(0);       // UnitarySystem outlet node number
    int ControlZoneNum(0);   // index to control zone

    std::string const idf_objects = delimited_string({

        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",
        "  ",

        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
        "  ",

        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,    !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",
        "  ",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,   !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,      !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:WaterToAirHeatPump:EquationFit,       !- Heating Coil Object Type",
        "  Sys 1 Heat Pump Heating Mode,  !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:WaterToAirHeatPump:EquationFit, !- Cooling Coil Object Type",
        "  Sys 1 Heat Pump Cooling Mode,  !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80;                     !- Maximum Supply Air Temperature{ C }",

        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  1.6,                    !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Cooling:WaterToAirHeatPump:EquationFit,",
        "  Sys 1 Heat Pump Cooling Mode,  !- Name",
        "  Sys 1 Water to Air Heat Pump Source Side1 Inlet Node,  !- Water Inlet Node Name",
        "  Sys 1 Water to Air Heat Pump Source Side1 Outlet Node,  !- Water Outlet Node Name",
        "  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  1.4,                     !- Rated Air Flow Rate {m3/s}",
        "  0.00165,                 !- Rated Water Flow Rate {m3/s}",
        "  40125.6,                 !- Gross Rated Total Cooling Capacity {W}",
        "  28267.06,                !- Gross Rated Sensible Cooling Capacity {W}",
        "  7.007757577,             !- Gross Rated Cooling COP",
        "  -0.68126221,             !- Total Cooling Capacity Coefficient 1",
        "  1.99529297,              !- Total Cooling Capacity Coefficient 2",
        "  -0.93611888,             !- Total Cooling Capacity Coefficient 3",
        "  0.02081177,              !- Total Cooling Capacity Coefficient 4",
        "  0.008438868,             !- Total Cooling Capacity Coefficient 5",
        "  2.24209455,              !- Sensible Cooling Capacity Coefficient 1",
        "  7.28913391,              !- Sensible Cooling Capacity Coefficient 2",
        "  -9.06079896,             !- Sensible Cooling Capacity Coefficient 3",
        "  -0.36729404,             !- Sensible Cooling Capacity Coefficient 4",
        "  0.218826161,             !- Sensible Cooling Capacity Coefficient 5",
        "  0.00901534,              !- Sensible Cooling Capacity Coefficient 6",
        "  -3.20456384,             !- Cooling Power Consumption Coefficient 1",
        "  0.47656454,              !- Cooling Power Consumption Coefficient 2",
        "  3.16734236,              !- Cooling Power Consumption Coefficient 3",
        "  0.10244637,              !- Cooling Power Consumption Coefficient 4",
        "  -0.038132556,            !- Cooling Power Consumption Coefficient 5",
        "  0,                       !- Nominal Time for Condensate Removal to Begin {s}",
        "  0;                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",

        "Coil:Heating:WaterToAirHeatPump:EquationFit,",
        "  Sys 1 Heat Pump Heating Mode,  !- Name",
        "  Sys 1 Water to Air Heat Pump Source Side2 Inlet Node,  !- Water Inlet Node Name",
        "  Sys 1 Water to Air Heat Pump Source Side2 Outlet Node,  !- Water Outlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  1.4,                     !- Rated Air Flow Rate {m3/s}",
        "  0.00165,                 !- Rated Water Flow Rate {m3/s}",
        "  33156.73,                !- Gross Rated Heating Capacity {W}",
        "  3.167053691,             !- Gross Rated Heating COP",
        "  -5.50102734,             !- Heating Capacity Coefficient 1",
        "  -0.96688754,             !- Heating Capacity Coefficient 2",
        "  7.70755007,              !- Heating Capacity Coefficient 3",
        "  0.031928881,             !- Heating Capacity Coefficient 4",
        "  0.028112522,             !- Heating Capacity Coefficient 5",
        "  -7.47517858,             !- Heating Power Consumption Coefficient 1",
        "  6.40876653,              !- Heating Power Consumption Coefficient 2",
        "  1.99711665,              !- Heating Power Consumption Coefficient 3",
        "  -0.050682973,            !- Heating Power Consumption Coefficient 4",
        "  0.011385145;             !- Heating Power Consumption Coefficient 5",

        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8, !- Gas Burner Efficiency",
        "  32000, !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;    !- Air Outlet Node Name",

        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",

        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Curve:Quadratic,",
        "  CoolCapFFF,       !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  COOLEIRFFF,           !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  PLFFPLR,          !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  CoolCapFT,        !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",

        "Curve:Biquadratic,",
        "  COOLEIRFT,            !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(state); // read zone equipment configuration and list objects

    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    for (int l = 1; l <= DataPlant::TotNumLoops; ++l) {
        auto &loop(DataPlant::PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(DataPlant::PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(DataPlant::PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    DataPlant::PlantLoop(1).Name = "ChilledWaterLoop";
    DataPlant::PlantLoop(1).FluidName = "FluidWaterLoop";
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).FluidName = "WATER";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = "SYS 1 HEAT PUMP COOLING MODE";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWAHPCoolingEquationFit;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 6;

    DataPlant::PlantLoop(2).Name = "HotWaterLoop";
    DataPlant::PlantLoop(2).FluidName = "FluidWaterLoop";
    DataPlant::PlantLoop(2).FluidIndex = 1;
    DataPlant::PlantLoop(2).FluidName = "WATER";
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = "SYS 1 HEAT PUMP HEATING MODE";
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWAHPHeatingEquationFit;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 9;

    std::string compName = "UNITARY SYSTEM MODEL";
    int compTypeOfNum = DataHVACGlobals::UnitarySys_AnyCoilType;
    bool zoneEquipment = true;
    UnitarySystems::UnitarySys::factory(state, compTypeOfNum, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects
    EXPECT_EQ(thisSys->UnitType, DataHVACGlobals::cFurnaceTypes(compTypeOfNum)); // compare UnitarySystem type string to valid type

    DataGlobals::SysSizingCalc = false; // DISABLE SIZING - don't call UnitarySystem::sizeSystem, much more work needed to set up sizing arrays

    InletNode = thisSys->AirInNode;
    OutletNode = thisSys->AirOutNode;
    ControlZoneNum = thisSys->NodeNumOfControlledZone;

    // set up unitary system inlet condtions
    DataLoopNode::Node(InletNode).Temp = 26.666667;             // AHRI condition 80F dry-bulb temp
    DataLoopNode::Node(InletNode).HumRat = 0.01117049542334198; // AHRI condition at 80F DB/67F WB lb/lb or kg/kg
    DataLoopNode::Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat);
    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail = thisSys->m_DesignMassFlowRate;

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 20.0; // set zone temperature during heating season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;         // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    // initialize other incidentals that are used within the UnitarySystem module during calculations
    DataSizing::CurZoneEqNum = 1;
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 1000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = 2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = 1000.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP = 0.0;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP(1) =
        DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP;

    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;
    ScheduleManager::Schedule(1).CurrentValue = 1.0;
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0); // initialize RhoAir
    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail = thisSys->m_DesignFanVolFlowRate * DataEnvironment::StdRhoAir;

    OutputReportPredefined::SetPredefinedTables();
    // system output should match RemainingOutputRequired = 1000.0 W (heating mode)

    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 0.01);    // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxHeatAirMassFlow * thisSys->m_PartLoadFrac); // cycling fan
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -1000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = -1000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = -2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 24.0; // set zone temperature during cooling season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;         // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    // system output should match RemainingOutputRequired = -1000.0 W (cooling mode)
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 1.0); // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxCoolAirMassFlow * thisSys->m_PartLoadFrac);
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);

    // water to air HP coils do not have a Minimum OAT for Compressor Operation input field
    // Unitary System mines data from coil objects
    EXPECT_EQ(thisSys->m_MinOATCompressorCooling, -1000.0);
    EXPECT_EQ(thisSys->m_MinOATCompressorHeating, -1000.0);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_ASHRAEModel_WaterCoils)
{

    bool ErrorsFound(false);
    bool FirstHVACIteration(false);
    Real64 Qsens_sys(0.0);   // UnitarySystem delivered sensible capacity wrt zone
    Real64 ZoneTemp(0.0);    // control zone temperature
    int InletNode(0);        // UnitarySystem inlet node number
    int OutletNode(0);       // UnitarySystem outlet node number
    int ControlZoneNum(0);   // index to control zone

    std::string const idf_objects = delimited_string({

        "Zone,",
        "  EAST ZONE,                !- Name",
        "  0,                        !- Direction of Relative North{ deg }",
        "  0,                        !- X Origin{ m }",
        "  0,                        !- Y Origin{ m }",
        "  0,                        !- Z Origin{ m }",
        "  1,                        !- Type",
        "  1,                        !- Multiplier",
        "  autocalculate,            !- Ceiling Height{ m }",
        "  autocalculate;            !- Volume{ m3 }",
        "  ",

        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                  !- Zone Name",
        "  Zone2Equipment,           !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,        !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,        !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,              !- Zone Air Node Name",
        "  Zone 2 Outlet Node;       !- Zone Return Air Node Name",
        "  ",

        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,           !- Name",
        "  SequentialLoad,                                          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,     !- Zone Equipment 1 Name",
        "  1,                        !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",
        "  ",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,    !- Name",
        "  SingleZoneVAV,           !- Control Type",
        "  East Zone,               !- Controlling Zone or Thermostat Location",
        "  None,                    !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  Zone Exhaust Node,       !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,       !- Air Outlet Node Name",
        "  Fan:OnOff,               !- Supply Fan Object Type",
        "  Supply Fan 1,            !- Supply Fan Name",
        "  BlowThrough,             !- Fan Placement",
        "  ContinuousFanSchedule,   !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Water,      !- Heating Coil Object Type",
        "  Water Heating Coil,      !- Heating Coil Name",
        "  ,                        !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:Water,      !- Cooling Coil Object Type",
        "  Water Cooling Coil,      !- Cooling Coil Name",
        "  ,                        !- Use DOAS DX Cooling Coil",
        "  15.0,                    !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                        !- Latent Load Control",
        "  ,                        !- Supplemental Heating Coil Object Type",
        "  ,                        !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,       !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                     !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                        !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,       !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                     !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                        !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,       !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  0.8,                     !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                        !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  25.0;                    !- Maximum Supply Air Temperature{ C }",

        "Fan:OnOff,",
        "  Supply Fan 1,            !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  0.7,                     !- Fan Total Efficiency",
        "  600.0,                   !- Pressure Rise{ Pa }",
        "  1.6,                     !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                     !- Motor Efficiency",
        "  1.0,                     !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,       !- Air Inlet Node Name",
        "  Water Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Cooling:Water,",
        "  Water Cooling Coil,      !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Namev",
        "  0.0002,                  !- Design Water Flow Rate { m3 / s }",
        "  1.6000,                  !- Design Air Flow Rate { m3 / s }",
        "  7.22,                    !- Design Inlet Water Temperature { Cv }",
        "  24.340,                  !- Design Inlet Air Temperature { C }",
        "  14.000,                  !- Design Outlet Air Temperature { C }",
        "  0.0095,                  !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
        "  0.0090,                  !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
        "  ChWInletNode,            !- Water Inlet Node Name",
        "  ChWOutletNode,           !- Water Outlet Node Name",
        "  Water Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Water Heating Coil Air Inlet Node, !- Air Outlet Node Name",
        "  SimpleAnalysis,          !- Type of Analysis",
        "  CrossFlow;               !- Heat Exchanger Configuration",

        "Coil:Heating:Water,",
        "  Water Heating Coil,      !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  300.0,                   !- U - Factor Times Area Value { W / K }",
        "  0.0006,                  !- Maximum Water Flow Rate { m3 / s }",
        "  HWInletNode,             !- Water Inlet Node Name",
        "  HWOutletNode,            !- Water Outlet Node Name",
        "  Water Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,       !- Air Outlet Node Name",
        "  UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "  5000.0,                  !- Rated Capacity { W }",
        "  82.2,                    !- Rated Inlet Water Temperature { C }",
        "  16.6,                    !- Rated Inlet Air Temperature { C }",
        "  71.1,                    !- Rated Outlet Water Temperature { C }",
        "  32.2,                    !- Rated Outlet Air Temperature { C }",
        "  ;                        !- Rated Ratio for Air and Water Convection",

        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",

        "Schedule:Compact,",
        "  FanAndCoilAvailSched,    !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00, 1.0;       !- Field 3",

        "Schedule:Compact,",
        "  ContinuousFanSchedule,   !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00, 1.0;       !- Field 3",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(state); // read zone equipment configuration and list objects

    DataSizing::CurZoneEqNum = 1;
    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
    for (int l = 1; l <= DataPlant::TotNumLoops; ++l) {
        auto &loop(DataPlant::PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(DataPlant::PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(DataPlant::PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    DataPlant::PlantLoop(1).Name = "ChilledWaterLoop";
    DataPlant::PlantLoop(1).FluidName = "FluidWaterLoop";
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).FluidName = "WATER";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = "WATER COOLING COIL";
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterCooling;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 9;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 10;

    DataPlant::PlantLoop(2).Name = "HotWaterLoop";
    DataPlant::PlantLoop(2).FluidName = "FluidWaterLoop";
    DataPlant::PlantLoop(2).FluidIndex = 1;
    DataPlant::PlantLoop(2).FluidName = "WATER";
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = "WATER HEATING COIL";
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterSimpleHeating;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 6;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 7;

    std::string compName = "UNITARY SYSTEM MODEL";
    int compTypeOfNum = DataHVACGlobals::UnitarySys_AnyCoilType;
    bool zoneEquipment = true;
    UnitarySystems::UnitarySys::factory(state, compTypeOfNum, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects
    EXPECT_EQ(thisSys->UnitType, DataHVACGlobals::cFurnaceTypes(compTypeOfNum)); // compare UnitarySystem type string to valid type

    DataGlobals::SysSizingCalc = false; // DISABLE SIZING - don't call UnitarySystem::sizeSystem, much more work needed to set up sizing arrays

    InletNode = thisSys->AirInNode;
    OutletNode = thisSys->AirOutNode;
    ControlZoneNum = thisSys->NodeNumOfControlledZone;

    // set up unitary system inlet condtions
    DataLoopNode::Node(InletNode).Temp = 20.0;    // zone winter dry-bulb temp
    DataLoopNode::Node(InletNode).HumRat = 0.005; // dry winter condition
    DataLoopNode::Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat);
    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail = thisSys->m_DesignMassFlowRate;

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 20.0; // set zone temperature during heating season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;         // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    // initialize other incidentals that are used within the UnitarySystem module during calculations
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 2000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = 4000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = 2000.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP = 0.0;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP(1) =
        DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP;

    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;
    // fill the schedule values
    ScheduleManager::Schedule(1).CurrentValue = 1.0; // availability
    ScheduleManager::Schedule(2).CurrentValue = 1.0; // constant fan
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0); // initialize RhoAir
    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail = thisSys->m_DesignFanVolFlowRate * DataEnvironment::StdRhoAir;

    OutputReportPredefined::SetPredefinedTables();
    // call once to initialize some variables (i.e., min air flow rate not correct on first pass)
    int AirLoopNum = 0;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = true;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
    // reset air flow control since schedule value was not update prior to GetInput usage
    thisSys->m_AirFlowControl = UnitarySys::UseCompFlow::UseCompressorOffFlow;

    // 4 general tests for heating and cooling:
    // 1 - low load, min fan speed and coil modulates to meet load
    // 2 - moderate load, fan speed and water flow modulate to meet load. Outlet air temp is typically at limit
    // 3 - high load, max fan speed, water modulate to meet load
    // 4 - very high load, max fan speed and water flow rate, load not met
    //
    // HEATING LOAD
    // Heating Test 1 - low load, operate at min fan flow, modulate water flow to meet load
    // system output should match RemainingOutputRequired = 2000.0 W (heating mode)
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 2.0); // Watts (2.0 = 0.001 * load)
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxNoCoolHeatAirMassFlow);                 // low speed fan flow
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);       // inlet = outlet flow rate
    // these next 2 variables are used to modulate the coil PLR irrespective of the fan PLR - they are non-zero when the model is called and CAN be 0
    // when load exceeds capacity the ASHRAE model is the only model that uses these variables, and flow is determined by Heat/CoolWaterFlowRatio *
    // max other models will show 0 here and in this case water flow will equal max flow * PartLoadRatio
    EXPECT_NEAR(thisSys->HeatCoilWaterFlowRatio, 0.01374, 0.0001); // heating coil water flow ratio, heating coil is on
    EXPECT_NEAR(thisSys->CoolCoilWaterFlowRatio, 0.0, 0.0001);     // cooling coil water flow ratio, cooling coil is off
    EXPECT_NEAR(thisSys->FanPartLoadRatio, thisSys->MaxNoCoolHeatAirMassFlow / thisSys->MaxHeatAirMassFlow,
                0.0001);                                                          // fan PLR at minimum speed
    EXPECT_LT(DataLoopNode::Node(OutletNode).Temp, thisSys->DesignMaxOutletTemp); // outlet temperature does not exceed max limit

    // test with 0 water flow rate to ensure divide by 0 does not happen (plant off, size = 0, etc.)
    Real64 saveSystemHeatWaterFlowRate = thisSys->MaxHeatCoilFluidFlow;
    // test that heating coil was operating prior to the next call to simulate
    EXPECT_GT(DataLoopNode::Node(thisSys->HeatCoilFluidInletNode).MassFlowRate, 0.0);
    EXPECT_GT(state.dataWaterCoils->WaterCoil(thisSys->m_HeatingCoilIndex).TotWaterHeatingCoilRate, 0.0);
    EXPECT_LT(DataLoopNode::Node(thisSys->HeatCoilInletNodeNum).Temp, DataLoopNode::Node(thisSys->HeatCoilOutletNodeNum).Temp);
    thisSys->MaxHeatCoilFluidFlow = 0.0;
    // use a smaller heat load so fan heat exceeds the load and the SZVAV model will be called
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 800.0;
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
    // test that heating coil was NOT operating due to this specific test
    // the fact that the unit test proceeded to here means the program did not crash
    EXPECT_EQ(1.0, thisSys->m_HeatingPartLoadFrac);                                   // model tried to turn on coil
    EXPECT_EQ(0.0, DataLoopNode::Node(thisSys->HeatCoilFluidInletNode).MassFlowRate); // these show coil is off
    EXPECT_EQ(0.0, state.dataWaterCoils->WaterCoil(thisSys->m_HeatingCoilIndex).TotWaterHeatingCoilRate);
    EXPECT_EQ(DataLoopNode::Node(thisSys->HeatCoilInletNodeNum).Temp, DataLoopNode::Node(thisSys->HeatCoilOutletNodeNum).Temp);
    // reset for next unit tests
    UnitarySystems::unitarySys[0].MaxHeatCoilFluidFlow = saveSystemHeatWaterFlowRate;

    // increase heating load so that upper temperature limit is reached
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 6000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP = 8000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP = 6000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP;

    // Heating Test 2 - moderate load, operate above min fan flow, modulate water flow to meet load
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 6.0); // Watts
    EXPECT_GT(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxNoCoolHeatAirMassFlow); // air flow higher than low speed fan flow
    EXPECT_LT(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxHeatAirMassFlow);       // air flow lower than high speed fan flow
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate); // inlet = outlet flow rate
    EXPECT_NEAR(thisSys->HeatCoilWaterFlowRatio, 0.0667, 0.0001); // heating coil water flow ratio, heating coil is on
    EXPECT_NEAR(thisSys->CoolCoilWaterFlowRatio, 0.0, 0.0001);    // cooling coil water flow ratio, cooling coil is off
    EXPECT_NEAR(thisSys->FanPartLoadRatio,
                0.6198,
                0.0001); // fan PLR above minimum and below maximum speed (0-1 means fraction between no load flow and full flow)
    EXPECT_NEAR(DataLoopNode::Node(OutletNode).Temp, thisSys->DesignMaxOutletTemp, 0.02); // outlet temperature modulated to meet max limit

    // increase heating load again so that upper temperature limit is exceeded to meet load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 10000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP = 12000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP = 10000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP;

    // Heating Test 3 - high load, operate at max fan flow, modulate water flow to meet load
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 10.0); // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);        // inlet = outlet flow rate
    EXPECT_NEAR(thisSys->HeatCoilWaterFlowRatio, 0.2532, 0.001); // heating coil water flow ratio, heating coil is on
    EXPECT_NEAR(thisSys->CoolCoilWaterFlowRatio, 0.0, 0.0001);   // cooling coil water flow ratio, cooling coil is off
    EXPECT_EQ(thisSys->FanPartLoadRatio, 1.0);                   // fan PLR at maximum speed (0-1 means fraction between no load flow and full flow)
    EXPECT_GT(DataLoopNode::Node(OutletNode).Temp, thisSys->DesignMaxOutletTemp); // outlet temperature exceeds max limit

    // increase heating load again to push water flow rate towards max
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 12000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP = 14000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP = 12000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP;

    // Heating Test 4 - very high load, operate at max fan and water flow, load not met
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_GT(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys);   // Watts - system CANNOT meet load
    EXPECT_NEAR(Qsens_sys, 11316.64, 0.1);                                                                      // system maxed out on capacity
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);  // inlet = outlet flow rate
    EXPECT_EQ(DataLoopNode::Node(thisSys->HeatCoilFluidInletNode).MassFlowRate, thisSys->MaxHeatCoilFluidFlow); // water coil water flow rate at max
    EXPECT_NEAR(thisSys->HeatCoilWaterFlowRatio,
                0.0,
                0.0001); // heating coil water flow ratio not set, heating coil is on since function returned when load exceeded capacity
    EXPECT_NEAR(thisSys->CoolCoilWaterFlowRatio, 0.0, 0.0001); // cooling coil water flow ratio, cooling coil is off
    EXPECT_EQ(thisSys->FanPartLoadRatio, 1.0);                 // fan PLR at maximum speed (0-1 means fraction between no load flow and full flow)
    EXPECT_GT(DataLoopNode::Node(OutletNode).Temp, thisSys->DesignMaxOutletTemp); // outlet temperature exceeds max limit
    EXPECT_NEAR(DataLoopNode::Node(OutletNode).Temp, 25.85, 0.01); // system allowed to exceed max outlet air temp to meet additional load

    // COOLING LOAD
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -2000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP = -2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP = -4000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP;

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 24.0; // zone summer dry-bulb temp
    DataLoopNode::Node(InletNode).Temp = 24.0;      // system inlet node dry-bulb temp
    DataLoopNode::Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat);
    DataEnvironment::OutDryBulbTemp = 35.0; // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    // Cooling Test 1 - low load, operate at min fan flow, modulate water flow to meet load
    // system output should match RemainingOutputRequired = -2000.0 W (cooling mode)
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 3.0); // Watts
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxNoCoolHeatAirMassFlow);                 // low speed fan flow
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);       // inlet = outlet flow rate
    EXPECT_NEAR(thisSys->HeatCoilWaterFlowRatio, 0.0, 0.0001);  // heating coil water flow ratio, heating coil is off
    EXPECT_NEAR(thisSys->CoolCoilWaterFlowRatio, 0.103, 0.001); // cooling coil water flow ratio, cooling coil is on
    EXPECT_NEAR(thisSys->FanPartLoadRatio, thisSys->MaxNoCoolHeatAirMassFlow / thisSys->MaxCoolAirMassFlow,
                0.0001);                                                          // fan PLR at minimum speed
    EXPECT_GT(DataLoopNode::Node(OutletNode).Temp, thisSys->DesignMinOutletTemp); // outlet temperature is not below min limit

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -9000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP = -9000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP = -11000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP;

    // Cooling Test 2 - moderate load, operate above min fan flow, modulate water flow to meet load
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 9.0); // Watts
    EXPECT_GT(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxNoCoolHeatAirMassFlow); // air flow higher than low speed fan flow
    EXPECT_LT(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxCoolAirMassFlow);       // air flow lower than high speed fan flow
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate); // inlet = outlet flow rate
    EXPECT_NEAR(thisSys->HeatCoilWaterFlowRatio, 0.0, 0.0001);                            // heating coil water flow ratio, heating coil is off
    EXPECT_NEAR(thisSys->CoolCoilWaterFlowRatio, 0.396, 0.001);                           // cooling coil water flow ratio, cooling coil is on
    EXPECT_NEAR(thisSys->FanPartLoadRatio, 0.5117, 0.0001);                               // fan PLR above minimum speed
    EXPECT_NEAR(DataLoopNode::Node(OutletNode).Temp, thisSys->DesignMinOutletTemp, 0.09); // outlet temperature modulated to meet max limit

    // test with 0 water flow rate to ensure divide by 0 does not happen (plant off, size = 0, etc.)
    Real64 saveSystemCoolWaterFlowRate = thisSys->MaxCoolCoilFluidFlow;
    // test that cooling coil was operating prior to the next call to simulate
    EXPECT_GT(DataLoopNode::Node(thisSys->CoolCoilFluidInletNode).MassFlowRate, 0.0);
    EXPECT_GT(state.dataWaterCoils->WaterCoil(thisSys->m_CoolingCoilIndex).TotWaterCoolingCoilRate, 0.0);
    EXPECT_GT(DataLoopNode::Node(thisSys->CoolCoilInletNodeNum).Temp, DataLoopNode::Node(thisSys->CoolCoilOutletNodeNum).Temp);
    thisSys->MaxCoolCoilFluidFlow = 0.0;
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
    // test that cooling coil was NOT operating due to this specific test
    // the fact that the unit test proceeded to here means the program did not crash
    EXPECT_EQ(1.0, thisSys->m_CoolingPartLoadFrac);                                   // model tried to turn on coil
    EXPECT_EQ(0.0, DataLoopNode::Node(thisSys->CoolCoilFluidInletNode).MassFlowRate); // these show coil is off
    EXPECT_EQ(0.0, state.dataWaterCoils->WaterCoil(thisSys->m_CoolingCoilIndex).TotWaterCoolingCoilRate);
    EXPECT_EQ(DataLoopNode::Node(thisSys->CoolCoilInletNodeNum).Temp, DataLoopNode::Node(thisSys->CoolCoilOutletNodeNum).Temp);
    // reset for next unit tests
    thisSys->MaxCoolCoilFluidFlow = saveSystemCoolWaterFlowRate;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -18000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP = -18000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP = -20000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP;

    // Cooling Test 3 - high load, operate at max fan flow, modulate water flow to meet load
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_NEAR(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys, 18.0); // Watts
    EXPECT_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxCoolAirMassFlow);                        // air flow at high speed fan flow
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate); // inlet = outlet flow rate
    EXPECT_NEAR(thisSys->HeatCoilWaterFlowRatio, 0.0, 0.0001);                    // heating coil water flow ratio, heating coil is off
    EXPECT_NEAR(thisSys->CoolCoilWaterFlowRatio, 0.803, 0.001);                   // cooling coil water flow ratio, cooling coil is on
    EXPECT_NEAR(thisSys->FanPartLoadRatio, 1.0, 0.0001);                          // fan PLR at maximum speed
    EXPECT_LT(DataLoopNode::Node(OutletNode).Temp, thisSys->DesignMinOutletTemp); // outlet temperature below minimum temperature limit

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -22000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP = -22000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP = -24000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToCoolSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputReqToHeatSP;

    // Cooling Test 4 - very high load, operate at max fan and water flow, load not met
    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(
                    DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat, ZoneTemp, DataLoopNode::Node(ControlZoneNum).HumRat);

    // test model performance
    EXPECT_LT(DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired, Qsens_sys);   // Watts - system CANNOT meet load
    EXPECT_EQ(DataLoopNode::Node(InletNode).MassFlowRate, thisSys->MaxCoolAirMassFlow);                         // air flow at high speed fan flow
    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);  // inlet = outlet flow rate
    EXPECT_EQ(DataLoopNode::Node(thisSys->CoolCoilFluidInletNode).MassFlowRate, thisSys->MaxCoolCoilFluidFlow); // water coil water flow rate at max
    EXPECT_NEAR(thisSys->HeatCoilWaterFlowRatio, 0.0, 0.0001); // heating coil water flow ratio, heating coil is off
    EXPECT_NEAR(thisSys->CoolCoilWaterFlowRatio,
                0.0,
                0.001); // cooling coil water flow ratio not set, cooling coil is on since function returned when load exceeded capacity
    EXPECT_NEAR(thisSys->FanPartLoadRatio, 1.0, 0.0001);                          // fan PLR at maximum speed
    EXPECT_LT(DataLoopNode::Node(OutletNode).Temp, thisSys->DesignMinOutletTemp); // outlet temperature below minimum temperature limit
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_MultispeedDXHeatingCoilOnly)
{

    std::string const idf_objects = delimited_string({

        "  Timestep,6;",

        "  Site:Location,",
        "    USA IL-CHICAGO-OHARE,    !- Name",
        "    41.77,                   !- Latitude {deg}",
        "    -87.75,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190;                     !- Elevation {m}",

        "SimulationControl, YES, NO, NO, YES, NO;",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO Ann Clg .4% Condns WB=>MDB,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    11.2,                    !- Maximum Dry-Bulb Temperature {C}",
        "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    5.5,                     !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    5.3,                     !- Wind Speed {m/s}",
        "    230,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    0.00;                    !- Sky Clearness",

        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",

        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,    !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,   !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,      !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:DX:MultiSpeed,       !- Heating Coil Object Type",
        "  MS Heating Coil 1,      !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  ,                       !- Cooling Coil Object Type",
        "  ,                       !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  ,                       !- Supplemental Heating Coil Object Type",
        "  ,                       !- Supplemental Heating Coil Name",
        "  None,                   !- Supply Air Flow Rate Method During Cooling Operation",
        "  ,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  0.00005,                !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,               !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  None,                   !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  ,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80,                     !- Maximum Supply Air Temperature{ C }",
        "  21,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                        !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                        !- Maximum Cycling Rate {cycles/hr}",
        "  ,                        !- Heat Pump Time Constant {s}",
        "  ,                        !- Fraction of On-Cycle Power Use",
        "  ,                        !- Heat Pump Fan Delay Time {s}",
        "  ,                        !- Ancillary On-Cycle Electric Power {W}",
        "  ,                        !- Ancillary Off-Cycle Electric Power {W}",
        "  ,                        !- Design Heat Recovery Water Flow Rate {m3/s}",
        "  ,                        !- Maximum Temperature for Heat Recovery {C}",
        "  ,                        !- Heat Recovery Water Inlet Node Name",
        "  ,                        !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,  !- Design Specification Multispeed Heat Pump Object Type",
        "  MultiSpeed Performance;  !- Design Specification Multispeed Heat Pump Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  MultiSpeed Performance,  !- Name",
        "  4,                       !- Number of Speeds for Heating",
        "  0,                       !- Number of Speeds for Cooling",
        "  No,                      !- Single Mode Operation",
        "  ,                        !- No Load Supply Air Flow Rate Ratio",
        "  autosize,                !- Heating Speed 1 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 1 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 2 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 2 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 3 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 3 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 4 Supply Air Flow Ratio",
        "  autosize;                !- Cooling Speed 4 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  autosize,               !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Heating Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Heating:DX:MultiSpeed,",
        "  MS Heating Coil 1,       !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  DX Heating Coil Air Inlet Node,  !- Indoor Air Inlet Node Name",
        "  Zone 2 Inlet Node,       !- Indoor Air Outlet Node Name",
        "  -8.0,                    !- Minimum Outdoor Dry - Bulb Temperature for Compressor Operation{ C }",
        "  -5.0,                    !- Outdoor Dry - Bulb Temperature to Turn On Compressor{ C }",
        "  200.0,                   !- Crankcase Heater Capacity{ W }",
        "  10.0,                    !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  ,                        !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "  7.22,                    !- Maximum Outdoor Dry - Bulb Temperature for Defrost Operation{ C }",
        "  Resistive,               !- Defrost Strategy",
        "  timed,                   !- Defrost Control",
        "  0.058333,                !- Defrost Time Period Fraction",
        "  autosize,                !- Resistive Defrost Heater Capacity{ W }",
        "  No,                      !- Apply Part Load Fraction to Speeds Greater than 1",
        "  NaturalGas,              !- Fuel Type",
        "  4,                       !- Region number for Calculating HSPF",
        "  4,                       !- Number of Speeds",
        "  autosize,                !- Speed 1 Gross Rated Heating Capacity{ W }",
        "  2.75,                    !- Speed 1 Gross Rated Heating COP{ W / W }",
        "  autosize,                !- Speed 1 Rated Air Flow Rate{ m3 / s }",
        "  345.0,                   !- Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT,           !- Speed 1 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 1 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR,         !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  ,                        !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  ,                        !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  autosize,                !- Speed 2 Gross Rated Heating Capacity{ W }",
        "  2.75,                    !- Speed 2 Gross Rated Heating COP{ W / W }",
        "  autosize,                !- Speed 2 Rated Air Flow Rate{ m3 / s }",
        "  425.0,                   !- Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT,           !- Speed 2 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 2 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR,         !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  ,                        !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  ,                        !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  autosize,                !- Speed 3 Gross Rated Heating Capacity{ W }",
        "  2.75,                    !- Speed 3 Gross Rated Heating COP{ W / W }",
        "  autosize,                !- Speed 3 Rated Air Flow Rate{ m3 / s }",
        "  525.0,                   !- Speed 3 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT,           !- Speed 3 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 3 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR,         !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  ,                        !- Speed 3 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  ,                        !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  autosize,                !- Speed 4 Gross Rated Heating Capacity{ W }",
        "  2.75,                    !- Speed 4 Gross Rated Heating COP{ W / W }",
        "  autosize,                !- Speed 4 Rated Air Flow Rate{ m3 / s }",
        "  673.0,                   !- Speed 4 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT,           !- Speed 4 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFFF,          !- Speed 4 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT,           !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFFF,          !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR,         !- Speed 4 Part Load Fraction Correlation Curve Name",
        "  ,                        !- Speed 4 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  ;                        !- Speed 4 Waste Heat Function of Temperature Curve Name",

        "  Curve:Quadratic,",
        "    HPACHeatEIRFFF,          !- Name",
        "    1.3824,                  !- Coefficient1 Constant",
        "    -0.4336,                 !- Coefficient2 x",
        "    0.0512,                  !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACCOOLPLFFPLR,         !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Cubic,",
        "    HPACHeatCapFFF,          !- Name",
        "    0.84,                    !- Coefficient1 Constant",
        "    0.16,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "    Curve:Biquadratic,",
        "      HPACHeatCapFT,           !- Name",
        "      0.8529681407,            !- Coefficient1 Constant",
        "      -0.0004847169,           !- Coefficient2 x",
        "     -0.0000010693,            !- Coefficient3 x**2",
        "      0.0185542164,            !- Coefficient4 y",
        "      0.0000872425,            !- Coefficient5 y**2",
        "      -0.0000166868,           !- Coefficient6 x*y",
        "      17.78,                   !- Minimum Value of x",
        "      23.33,                   !- Maximum Value of x",
        "      -28.89,                  !- Minimum Value of y",
        "      17.22,                   !- Maximum Value of y",
        "      0.3799,                  !- Minimum Curve Output",
        "      1.1896,                  !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "    Curve:Biquadratic,",
        "      HPACHeatEIRFT,           !- Name",
        "      0.7077081462,            !- Coefficient1 Constant",
        "      0.0148163478,            !- Coefficient2 x",
        "      0.0002622589,            !- Coefficient3 x**2",
        "      -0.0113239622,           !- Coefficient4 y",
        "      0.0002939277,            !- Coefficient5 y**2",
        "      -0.0003605284,           !- Coefficient6 x*y",
        "      17.78,                   !- Minimum Value of x",
        "      23.33,                   !- Maximum Value of x",
        "      -28.89,                  !- Minimum Value of y",
        "      17.22,                   !- Maximum Value of y",
        "      0.8266,                  !- Minimum Curve Output",
        "      2.0277,                  !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",

        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "  Sizing:Zone,",
        "    EAST ZONE,      !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    12.8000,                 !- Zone Cooling Design Supply Air Temperature {C}",
        "    ,                        !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    50.0000,                 !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.0085,                  !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.0080,                  !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA Zone One,        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Zone Heating Sizing Factor",
        "    ,                        !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    ,                        !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    ,                        !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    ;                        !- Heating Maximum Air Flow Fraction",

        "  DesignSpecification:OutdoorAir,",
        "    SZ DSOA Zone One,  !- Name",
        "    Flow/Person,             !- Outdoor Air Method",
        "    0.0125;                  !- Outdoor Air Flow per Person {m3/s-person}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "     ,                       !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Adiabatic,               !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0.000000,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.000000,15.24000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000,15.24000,4.572,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,4.572,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0.000000,4.572,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572;  !- X,Y,Z ==> Vertex 4 {m}",

        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",

        "  Construction,",
        "   FLOOR,                   !- Name",
        "    C5 - 4 IN HW CONCRETE;   !- Outside Layer",

        "  Construction,",
        "    ROOF31,                  !- Name",
        "    R31LAYER;                !- Outside Layer",

        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    R31LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    5.456,                   !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    C5 - 4 IN HW CONCRETE,   !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  ZoneControl:Thermostat,",
        "    EAST ZONE Thermostat,    !- Name",
        "    EAST ZONE,               !- Zone or ZoneList Name",
        "    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    Zone One DualSPSched;    !- Control 1 Name",

        "  Schedule:Compact,",
        "    Dual Zone Control Type Sched,  !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,4;          !- Field 3",

        "  ThermostatSetpoint:DualSetpoint,",
        "    Zone One DualSPSched,    !- Name",
        "    HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
        "    CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",

        "  Schedule:Compact,",
        "    CLGSETP_SCH,             !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 19",
        "    Until: 24:00,26.7;       !- Field 20",

        "  Schedule:Compact,",
        "    HTGSETP_SCH,             !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 22",
        "    Until: 24:00,15.6;       !- Field 23",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    SimulationManager::GetProjectData(state);
    createFacilityElectricPowerServiceObject();
    DataGlobals::BeginSimFlag = true;
    DataGlobals::DoingSizing = true;
    SizingManager::ManageSizing(state);
    DataGlobals::DoingSizing = false;
    DataGlobals::SysSizingCalc = false;
    DataSizing::CurZoneEqNum = 1;
    DataSizing::ZoneEqSizing.allocate(1);

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool ErrorsFound = false;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    OutputReportPredefined::SetPredefinedTables();

    DataSizing::ZoneSizingRunDone = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesignSizeFromParent = false;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod.allocate(25);
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;

    bool FirstHVACIteration = true;
    int AirLoopNum = 0;
    thisSys->sizeSystem(state, FirstHVACIteration, AirLoopNum);
    DXCoils::SizeDXCoil(state, 1);

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects

    ASSERT_NEAR(thisSys->m_DesignHeatingCapacity, 1303.091, 0.001);
    ASSERT_EQ(thisSys->m_DesignCoolingCapacity, 0.0);
    ASSERT_NEAR(DXCoils::DXCoil(1).MSRatedTotCap(1), 325.773, 0.001);
    ASSERT_NEAR(DXCoils::DXCoil(1).MSRatedTotCap(2), 651.545, 0.001);
    ASSERT_NEAR(DXCoils::DXCoil(1).MSRatedTotCap(3), 977.318, 0.001);
    ASSERT_NEAR(DXCoils::DXCoil(1).MSRatedTotCap(4), 1303.091, 0.001);
    ASSERT_NEAR(thisSys->m_HeatVolumeFlowRate[1], 0.0131, 0.0001);
    ASSERT_NEAR(thisSys->m_HeatVolumeFlowRate[2], 0.0262, 0.0001);
    ASSERT_NEAR(thisSys->m_HeatVolumeFlowRate[3], 0.0393, 0.0001);
    ASSERT_NEAR(thisSys->m_HeatVolumeFlowRate[4], 0.0524, 0.0001);
    ASSERT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(1), 0.0131, 0.0001);
    ASSERT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(2), 0.0262, 0.0001);
    ASSERT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(3), 0.0393, 0.0001);
    ASSERT_NEAR(DXCoils::DXCoil(1).MSRatedAirVolFlowRate(4), 0.0524, 0.0001);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_MultiSpeedCoils_SingleMode)
{

    bool ErrorsFound(false);
    bool FirstHVACIteration(false);
    int InletNode(0);      // UnitarySystem inlet node number
    int OutletNode(0);     // UnitarySystem outlet node number
    int ControlZoneNum(0); // index to control zone

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",
        "  ",
        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
        "  ",

        "  ZoneHVAC:EquipmentList,",
        "    Zone2Equipment,          !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    Zone2DirectAirADU,       !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "    ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "    ;                        !- Zone Equipment 1 Sequential Heating Fraction",

        "  ZoneHVAC:AirDistributionUnit,",
        "    Zone2DirectAirADU,       !- Name",
        "    Zone 2 Inlet Node,       !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        "    Zone2DirectAir;          !- Air Terminal Name",

        "  AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        "    Zone2DirectAir,          !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    Zone 2 Inlet Node 2AT,   !- Air Inlet Node Name",
        "    Zone 2 Inlet Node,       !- Air Outlet Node Name",
        "    0.467;                   !- Maximum Air Flow Rate {m3/s}",

        "  ",
        "BranchList,",
        "  Air Loop Branches, !- Name",
        "  Air Loop Main Branch;    !- Branch 1 Name",
        "  ",
        "Branch,",
        "  Air Loop Main Branch, !- Name",
        "  , !- Pressure Drop Curve Name",
        "  AirLoopHVAC:UnitarySystem, !- Component 2 Object Type",
        "  Unitary System Model, !- Component 2 Name",
        "  	Mixed Air Node, !- Component 2 Inlet Node Name",
        "  Air Loop Outlet Node; !- Component 2 Outlet Node Name",
        "  ",
        "AirLoopHVAC,",
        "  Heat Pump Sys 1, !- Name",
        "  , !- Controller List Name",
        "  Heat Pump 1 Avail List, !- Availability Manager List Name",
        "  1.7, !- Design Supply Air Flow Rate{ m3 / s }",
        "  Air Loop Branches, !- Branch List Name",
        "  , !- Connector List Name",
        "  Mixed Air Node, !- Supply Side Inlet Node Name",
        "  Return Air Mixer Outlet, !- Demand Side Outlet Node Name",
        "  Zone Equipment Inlet Node, !- Demand Side Inlet Node Names",
        "  Air Loop Outlet Node;    !- Supply Side Outlet Node Names",
        "  ",
        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model, !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Mixed Air Node,         !- Air Inlet Node Name",
        "  Air Loop Outlet Node,   !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  CyclingFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:DX:MultiSpeed,       !- Heating Coil Object Type",
        "  Heat Pump DX Heating Coil 2, !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:MultiSpeed, !- Cooling Coil Object Type",
        "  Heat Pump ACDXCoil 2,     !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "  Heat Pump DX Supp Heating Coil 1, !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.7,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
        "  1.7,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  0,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  50, !- Maximum Supply Air Temperature{ C }",
        "  		21, !- Maximum Outdoor Dry - Bulb Temperature for Supplemental Heater Operation{ C }",
        "  		, !- Outdoor Dry - Bulb Temperature Sensor Node Name",
        "  		, !- Maximum Cycling Rate",
        "  		, !- Heat Pump Time Constant",
        "  		, !- Fraction of On - Cycle Power Use",
        "  		, !- Heat Pump Fan Delay Time",
        "  		, !- Ancillary On - Cycle Electric Power",
        "  		, !- Ancillary Off - Cycle Electric Power",
        "  		, !- Design Heat Recovery Water Flow Rate",
        "  		, !- Maximum Temperature for Heat Recovery",
        "  		, !- Heat Recovery Water Inlet Node Name",
        "  		, !- Heat Recovery Water Outlet Node Name",
        "  	UnitarySystemPerformance:Multispeed, !- Design Specification Multispeed Object Type",
        "   MyMultiSpeed;            !- Design Specification Multispeed Object Name",
        "  ",
        "UnitarySystemPerformance:Multispeed,",
        "   MyMultiSpeed, !- Name",
        "   4, !- Number of Speeds for Heating",
        "   4, !- Number of Speeds for Cooling",
        "   Yes, !- Single Mode Operation",
        "   ,     !- No Load Supply Air Flow Rate Ratio",
        "   0.24, !- Heating Speed 1 Supply Air Flow Ratio",
        "   0.24, !- Cooling Speed 1 Supply Air Flow Ratio",
        "   0.47, !- Heating Speed 2 Supply Air Flow Ratio",
        "   0.47, !- Cooling Speed 2 Supply Air Flow Ratio",
        "   0.75, !- Heating Speed 3 Supply Air Flow Ratio",
        "   0.75, !- Cooling Speed 3 Supply Air Flow Ratio",
        "   1.0,  !- Heating Speed 4 Supply Air Flow Ratio",
        "   1.0;  !- Cooling Speed 4 Supply Air Flow Ratio",
        "  ",
        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  300.0,                  !- Pressure Rise{ Pa }",
        "  1.7,                    !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Mixed Air Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Heating:Fuel,",
        "  Heat Pump DX Supp Heating Coil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8, !- Gas Burner Efficiency",
        "  45000, !- Nominal Capacity{ W }",
        "  SuppHeating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Air Loop Outlet Node;    !- Air Outlet Node Name",
        "  ",
        "Coil:Heating:DX:MultiSpeed,",
        "  Heat Pump DX Heating Coil 2, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  SuppHeating Coil Air Inlet Node, !- Air Outlet Node Name",
        "  -8.0, !- Minimum Outdoor Dry - Bulb Temperature for Compressor Operation{ C }",
        "  -5.0, !- Outdoor Dry - Bulb Temperature to Turn On Compressor{ C }",
        "  200.0, !- Crankcase Heater Capacity{ W }",
        "  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  HPACDefrostCAPFT, !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "  7.22, !- Maximum Outdoor Dry - Bulb Temperature for Defrost Operation{ C }",
        "  ReverseCycle, !- Defrost Strategy",
        "  timed, !- Defrost Control",
        "  0.058333, !- Defrost Time Period Fraction",
        "  2000.0, !- Resistive Defrost Heater Capacity{ W }",
        "  No, !- Apply Part Load Fraction to Speeds Greater than 1",
        "  NaturalGas, !- Fuel Type",
        "  4, !- Region number for Calculating HSPF",
        "  4, !- Number of Speeds",
        "  7500, !- Speed 1 Gross Rated Heating Capacity{ W }",
        "  2.75, !- Speed 1 Gross Rated Heating COP{ W / W }",
        "  0.45, !- Speed 1 Rated Air Flow Rate{ m3 / s }",
        "  345.0, !- Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT Speed 1, !- Speed 1 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFF Speed 1, !- Speed 1 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT Speed 1, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFF Speed 1, !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACHeatPLFFPLR Speed 1, !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  0.2, !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCHeatWHFT Speed 1, !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  17500, !- Speed 2 Gross Rated Heating Capacity{ W }",
        "  2.75, !- Speed 2 Gross Rated Heating COP{ W / W }",
        "  0.85, !- Speed 2 Rated Air Flow Rate{ m3 / s }",
        "  425.0, !- Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT Speed 2, !- Speed 2 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFF Speed 2, !- Speed 2 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT Speed 2, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFF Speed 2, !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACHeatPLFFPLR Speed 2, !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  0.2, !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCHeatWHFT Speed 2, !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  25500, !- Speed 3 Gross Rated Heating Capacity{ W }",
        "  2.75, !- Speed 3 Gross Rated Heating COP{ W / W }",
        "  1.25, !- Speed 3 Rated Air Flow Rate{ m3 / s }",
        "  525.0, !- Speed 3 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT Speed 3, !- Speed 3 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFF Speed 3, !- Speed 3 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT Speed 3, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFF Speed 3, !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACHeatPLFFPLR Speed 3, !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  0.2, !- Speed 3 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCHeatWHFT Speed 3, !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  35500, !- Speed 4 Gross Rated Heating Capacity{ W }",
        "  2.75, !- Speed 4 Gross Rated Heating COP{ W / W }",
        "  1.75, !- Speed 4 Rated Air Flow Rate{ m3 / s }",
        "  673.0, !- Speed 4 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT Speed 4, !- Speed 4 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFF Speed 4, !- Speed 4 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT Speed 4, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFF Speed 4, !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACHeatPLFFPLR Speed 4, !- Speed 4 Part Load Fraction Correlation Curve Name",
        "  0.2, !- Speed 4 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCHeatWHFT Speed 4;    !- Speed 4 Waste Heat Function of Temperature Curve Name",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFT Speed 1, !- Name",
        "  0.758746, !- Coefficient1 Constant",
        "  0.027626, !- Coefficient2 x",
        "  0.000148716, !- Coefficient3 x**2",
        "  0.0000034992, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit ",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFT Speed 2, !- Name",
        "  0.758746, !- Coefficient1 Constant",
        "  0.027626, !- Coefficient2 x",
        "  0.000148716, !- Coefficient3 x**2",
        "  0.0000034992, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFT Speed 3, !- Name",
        "  0.758746, !- Coefficient1 Constant",
        "  0.027626, !- Coefficient2 x",
        "  0.000148716, !- Coefficient3 x**2",
        "  0.0000034992, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFT Speed 4, !- Name",
        "  0.758746, !- Coefficient1 Constant",
        "  0.027626, !- Coefficient2 x",
        "  0.000148716, !- Coefficient3 x**2",
        "  0.0000034992, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFF Speed 1, !- Name",
        "  0.84, !- Coefficient1 Constant",
        "  0.16, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFF Speed 2, !- ",
        "  0.84, !- Coefficient1 Constant",
        "  0.16, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFF Speed 3, !- Name",
        "  0.84, !- Coefficient1 Constant",
        "  0.16, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFF Speed 4, !- Name",
        "  0.84, !- Coefficient1 Constant",
        "  0.16, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatEIRFT Speed 1, !- Name",
        "  1.19248, !- Coefficient1 Constant",
        "  -0.0300438, !- Coefficient2 x",
        "  0.00103745, !- Coefficient3 x**2",
        "  -0.000023328, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatEIRFT Speed 2, !- Name",
        "  1.19248, !- Coefficient1 Constant",
        "  -0.0300438, !- Coefficient2 x",
        "  0.00103745, !- Coefficient3 x**2",
        "  -0.000023328, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatEIRFT Speed 3, !- Name",
        "  1.19248, !- Coefficient1 Constant",
        "  -0.0300438, !- Coefficient2 x",
        "  0.00103745, !- Coefficient3 x**2",
        "  -0.000023328, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatEIRFT Speed 4, !- Name",
        "  1.19248, !- Coefficient1 Constant",
        "  -0.0300438, !- Coefficient2 x",
        "  0.00103745, !- Coefficient3 x**2",
        "  -0.000023328, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatEIRFF Speed 1, !- Name",
        "  1.3824, !- Coefficient1 Constant",
        "  -0.4336, !- Coefficient2 x",
        "  0.0512, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatEIRFF Speed 2, !- Name",
        "  1.3824, !- Coefficient1 Constant",
        "  -0.4336, !- Coefficient2 x",
        "  0.0512, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatEIRFF Speed 3, !- Name",
        "  1.3824, !- Coefficient1 Constant",
        "  -0.4336, !- Coefficient2 x",
        "  0.0512, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatEIRFF Speed 4, !- Name",
        "  1.3824, !- Coefficient1 Constant",
        "  -0.4336, !- Coefficient2 x",
        "  0.0512, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatPLFFPLR Speed 1, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatPLFFPLR Speed 2, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatPLFFPLR Speed 3, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatPLFFPLR Speed 4, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        " Curve:Biquadratic,",
        "  HPACDefrostCAPFT, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  HAPCHeatWHFT Speed 1, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  HAPCHeatWHFT Speed 2, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  HAPCHeatWHFT Speed 3, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  HAPCHeatWHFT Speed 4, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Coil:Cooling:DX:MultiSpeed,",
        "  Heat Pump ACDXCoil 2, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node, !- Air Outlet Node Name",
        "  Outdoor Condenser Air Node, !- Condenser Air Inlet Node Name",
        "  AirCooled, !- Condenser Type",
        "  , !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  , !- Supply Water Storage Tank Name",
        "  , !- Condensate Collection Water Storage Tank Name",
        "  No, !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No, !- Apply Latent Degradation to Speeds Greater than 1",
        "  200.0, !- Crankcase Heater Capacity{ W }",
        "  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  , !- Basin Heater Capacity{ W / K }",
        "  , !- Basin Heater Setpoint Temperature{ C }",
        "  , !- Basin Heater Operating Schedule Name",
        "  NaturalGas, !- Fuel Type",
        "  4, !- Number of Speeds",
        "  7500, !- Speed 1 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 1 Gross Rated Cooling COP{ W / W }",
        "  0.40, !- Speed 1 Rated Air Flow Rate{ m3 / s }",
        "  453.3, !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACCoolCapFT Speed 1, !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed 1, !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed 1, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed 1, !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed 1, !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 1 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 1 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 1 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCCoolWHFT Speed 1, !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 1 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.05, !- Speed 1 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  50, !- Speed 1 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  17500, !- Speed 2 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 2 Gross Rated Cooling COP{ W / W }",
        "  0.85, !- Speed 2 Rated Air Flow Rate{ m3 / s }",
        "  523.3, !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACCoolCapFT Speed 1, !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed 1, !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed 1, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed 1, !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed 1, !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 2 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 2 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 2 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCCoolWHFT Speed 1, !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 2 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.1, !- Speed 2 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  60, !- Speed 2 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  25500, !- Speed 3 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 3 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 3 Gross Rated Cooling COP{ W / W }",
        "  1.25, !- Speed 3 Rated Air Flow Rate{ m3 / s }",
        "  573.3, !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACCoolCapFT Speed 1, !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed 1, !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed 1, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed 1, !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed 1, !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 3 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 3 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 3 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 3 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCCoolWHFT Speed 1, !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 3 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.2, !- Speed 3 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  80, !- Speed 3 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  35500, !- Speed 4 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 4 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 4 Gross Rated Cooling COP{ W / W }",
        "  1.75, !- Speed 4 Rated Air Flow Rate{ m3 / s }",
        "  673.3, !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACCoolCapFT Speed 1, !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed 1, !- Speed 4 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed 1, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed 1, !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed 1, !- Speed 4 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 4 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 4 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 4 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 4 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 4 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCCoolWHFT Speed 1, !- Speed 4 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 4 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.3, !- Speed 4 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  100;                     !- Speed 4 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  ",
        "OutdoorAir:Node,",
        "  Outdoor Condenser Air Node, !- Name",
        "  1.0;                     !- Height Above Ground{ m }",
        "  ",
        "Curve:Biquadratic,",
        "  HPACCoolCapFT Speed 1, !- Name",
        "  1, !- Coefficient1 Constant",
        "  0, !- Coefficient2 x",
        "  0, !- Coefficient3 x**2",
        "  0, !- Coefficient4 y",
        "  0, !- Coefficient5 y**2",
        "  0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  0, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  46.11111, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACCoolCapFF Speed 1, !- Name",
        "  1, !- Coefficient1 Constant",
        "  0, !- Coefficient2 x",
        "  0, !- Coefficient3 x**2",
        "  0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Biquadratic,",
        "  HPACCOOLEIRFT Speed 1, !- Name",
        "  1, !- Coefficient1 Constant",
        "  0, !- Coefficient2 x",
        "  0, !- Coefficient3 x**2",
        "  0, !- Coefficient4 y",
        "  0, !- Coefficient5 y**2",
        "  0, !- Coefficient6 x*y",
        "  12.77778, !- Minimum Value of x",
        "  23.88889, !- Maximum Value of x",
        "  23.88889, !- Minimum Value of y",
        "  46.11111, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACCOOLEIRFF Speed 1, !- Name",
        "  1, !- Coefficient1 Constant",
        "  0, !- Coefficient2 x",
        "  0, !- Coefficient3 x**2",
        "  0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACCOOLPLFFPLR Speed 1, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Biquadratic,",
        "  HAPCCoolWHFT Speed 1, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",
        "  ",
        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Schedule:Compact,",
        "  CyclingFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 0.0;      !- Field 3",
        "  ",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects
    DataLoopNode::Node.allocate(10);
    DataSizing::ZoneEqSizing.deallocate();
    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum = 1;

    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData(state); // read zone equipment configuration and list objects
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(state);
    SingleDuct::GetSysInput(state);

    BranchInputManager::ManageBranchInput(state); // just gets input and returns.

    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).NumBranches = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).TotalComponents = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).Name = "UNITARY SYSTEM MODEL";
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:UnitarySystem";

    DataZoneControls::NumTempControlledZones = 1;
    DataZoneControls::TempControlledZone.allocate(DataZoneControls::NumTempControlledZones);
    DataZoneControls::TempControlledZone(DataZoneControls::NumTempControlledZones).ActualZoneNum = 1;

    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above

    OutputReportPredefined::SetPredefinedTables();

    ControlZoneNum = 1;
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP.allocate(1);

    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(thisSys->ControlZoneNum).SequencedOutputRequiredToDehumidSP.allocate(1);
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;

    InletNode = thisSys->AirInNode;
    OutletNode = thisSys->AirOutNode;
    ControlZoneNum = thisSys->NodeNumOfControlledZone;

    // set up unitary system inlet condtions
    DataLoopNode::Node(InletNode).Temp = 26.666667;             // AHRI condition 80F dry-bulb temp
    DataLoopNode::Node(InletNode).HumRat = 0.01117049542334198; // AHRI condition at 80F DB/67F WB lb/lb or kg/kg
    DataLoopNode::Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat);

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 24.0;    // set zone temperature during cooling season used to determine system delivered capacity
    DataLoopNode::Node(ControlZoneNum).HumRat = 0.001; // set zone temperature during cooling season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;            // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    state.dataAirLoop->AirLoopControlInfo.allocate(1);
    DataGlobals::SysSizingCalc = true;

    thisSys->m_ZoneInletNode = DataZoneEquipment::ZoneEquipConfig(1).InletNode(1);

    ScheduleManager::Schedule(thisSys->m_SysAvailSchedPtr).CurrentValue = 1.0;

    DataSizing::CurSysNum = 1;
    DataSizing::UnitarySysEqSizing.allocate(1);

    int Iter;
    DataEnvironment::StdRhoAir = 1.2;
    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail = thisSys->m_MaxCoolAirVolFlow * DataEnvironment::StdRhoAir;
    Iter = 4;
    thisSys->m_CoolVolumeFlowRate[Iter] = thisSys->m_MaxCoolAirVolFlow * UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[Iter - 1];
    thisSys->m_CoolMassFlowRate[Iter] = thisSys->m_CoolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
    thisSys->m_MSCoolingSpeedRatio[Iter] =
        thisSys->m_CoolVolumeFlowRate[Iter] / thisSys->m_CoolVolumeFlowRate[UnitarySystems::designSpecMSHP[0].numOfSpeedCooling];
    DXCoils::DXCoil(1).MSRatedAirMassFlowRate(Iter) = DXCoils::DXCoil(1).MSRatedAirVolFlowRate(Iter) * DataEnvironment::StdRhoAir;
    DXCoils::DXCoil(2).MSRatedAirMassFlowRate(Iter) = DXCoils::DXCoil(2).MSRatedAirVolFlowRate(Iter) * DataEnvironment::StdRhoAir;
    Iter = 1;
    thisSys->m_CoolVolumeFlowRate[Iter] = thisSys->m_MaxCoolAirVolFlow * UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[Iter - 1];
    thisSys->m_CoolMassFlowRate[Iter] = thisSys->m_CoolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
    thisSys->m_MSCoolingSpeedRatio[Iter] =
        thisSys->m_CoolVolumeFlowRate[Iter] / thisSys->m_CoolVolumeFlowRate[UnitarySystems::designSpecMSHP[0].numOfSpeedCooling];
    DXCoils::DXCoil(1).MSRatedAirMassFlowRate(Iter) = DXCoils::DXCoil(1).MSRatedAirVolFlowRate(Iter) * DataEnvironment::StdRhoAir;
    DXCoils::DXCoil(2).MSRatedAirMassFlowRate(Iter) = DXCoils::DXCoil(2).MSRatedAirVolFlowRate(Iter) * DataEnvironment::StdRhoAir;
    Iter = 2;
    thisSys->m_CoolVolumeFlowRate[Iter] = thisSys->m_MaxCoolAirVolFlow * UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[Iter - 1];
    thisSys->m_CoolMassFlowRate[Iter] = thisSys->m_CoolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
    thisSys->m_MSCoolingSpeedRatio[Iter] =
        thisSys->m_CoolVolumeFlowRate[Iter] / thisSys->m_CoolVolumeFlowRate[UnitarySystems::designSpecMSHP[0].numOfSpeedCooling];
    DXCoils::DXCoil(1).MSRatedAirMassFlowRate(Iter) = DXCoils::DXCoil(1).MSRatedAirVolFlowRate(Iter) * DataEnvironment::StdRhoAir;
    DXCoils::DXCoil(2).MSRatedAirMassFlowRate(Iter) = DXCoils::DXCoil(2).MSRatedAirVolFlowRate(Iter) * DataEnvironment::StdRhoAir;
    Iter = 3;
    thisSys->m_CoolVolumeFlowRate[Iter] = thisSys->m_MaxCoolAirVolFlow * UnitarySystems::designSpecMSHP[0].coolingVolFlowRatio[Iter - 1];
    thisSys->m_CoolMassFlowRate[Iter] = thisSys->m_CoolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
    thisSys->m_MSCoolingSpeedRatio[Iter] =
        thisSys->m_CoolVolumeFlowRate[Iter] / thisSys->m_CoolVolumeFlowRate[UnitarySystems::designSpecMSHP[0].numOfSpeedCooling];
    DXCoils::DXCoil(1).MSRatedAirMassFlowRate(Iter) = DXCoils::DXCoil(1).MSRatedAirVolFlowRate(Iter) * DataEnvironment::StdRhoAir;
    DXCoils::DXCoil(2).MSRatedAirMassFlowRate(Iter) = DXCoils::DXCoil(2).MSRatedAirVolFlowRate(Iter) * DataEnvironment::StdRhoAir;

    Iter = 4;
    thisSys->m_HeatVolumeFlowRate[Iter] = thisSys->m_MaxHeatAirVolFlow * UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[Iter - 1];
    thisSys->m_HeatMassFlowRate[Iter] = thisSys->m_HeatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
    thisSys->m_MSHeatingSpeedRatio[Iter] =
        thisSys->m_HeatVolumeFlowRate[Iter] / thisSys->m_HeatVolumeFlowRate[UnitarySystems::designSpecMSHP[0].numOfSpeedHeating];
    Iter = 1;
    thisSys->m_HeatVolumeFlowRate[Iter] = thisSys->m_MaxHeatAirVolFlow * UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[Iter - 1];
    thisSys->m_HeatMassFlowRate[Iter] = thisSys->m_HeatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
    thisSys->m_MSHeatingSpeedRatio[Iter] =
        thisSys->m_HeatVolumeFlowRate[Iter] / thisSys->m_HeatVolumeFlowRate[UnitarySystems::designSpecMSHP[0].numOfSpeedHeating];
    Iter = 2;
    thisSys->m_HeatVolumeFlowRate[Iter] = thisSys->m_MaxHeatAirVolFlow * UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[Iter - 1];
    thisSys->m_HeatMassFlowRate[Iter] = thisSys->m_HeatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
    thisSys->m_MSHeatingSpeedRatio[Iter] =
        thisSys->m_HeatVolumeFlowRate[Iter] / thisSys->m_HeatVolumeFlowRate[UnitarySystems::designSpecMSHP[0].numOfSpeedHeating];
    Iter = 3;
    thisSys->m_HeatVolumeFlowRate[Iter] = thisSys->m_MaxHeatAirVolFlow * UnitarySystems::designSpecMSHP[0].heatingVolFlowRatio[Iter - 1];
    thisSys->m_HeatMassFlowRate[Iter] = thisSys->m_HeatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
    thisSys->m_MSHeatingSpeedRatio[Iter] =
        thisSys->m_HeatVolumeFlowRate[Iter] / thisSys->m_HeatVolumeFlowRate[UnitarySystems::designSpecMSHP[0].numOfSpeedHeating];

    thisSys->m_IdleMassFlowRate = thisSys->m_CoolMassFlowRate[1];
    // flow rates are set up now, don't call getInput again
    thisSys->m_ThisSysInputShouldBeGotten = false;

    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0); // initialize RhoAir

    state.dataAirLoop->AirLoopFlow.allocate(1);

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -10000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = -10000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = -20000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;

    int AirLoopNum = 1;
    int CompIndex = 1;
    bool HeatActive = false;
    bool CoolActive = true;
    int const ZoneOAUnitNum = 0;
    Real64 const OAUCoilOutTemp = 0.0;
    bool const ZoneEquipment = false;
    Real64 sensOut = 0.0;
    Real64 latOut = 0.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);

    EXPECT_NEAR(0.9684, thisSys->m_CycRatio, 0.0001); // cycling ratio
    EXPECT_EQ(2, thisSys->m_CoolingSpeedNum);
    EXPECT_EQ(1.0, thisSys->m_CoolingSpeedRatio);

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 1000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = 2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = 1000.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;

    DataEnvironment::OutDryBulbTemp = 0.0; // initialize weather
    DataEnvironment::OutHumRat = 0.0001;
    DataEnvironment::OutBaroPress = 101325.0;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
    EXPECT_NEAR(0.16177, thisSys->m_CycRatio, 0.0001); // cycling ratio
    EXPECT_EQ(1, thisSys->m_HeatingSpeedNum);
    EXPECT_EQ(0.0, thisSys->m_HeatingSpeedRatio);

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 20000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = 30000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = 20000.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;

    thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
    EXPECT_NEAR(0.920083, thisSys->m_CycRatio, 0.0001); // cycling ratio
    EXPECT_EQ(3, thisSys->m_HeatingSpeedNum);
    EXPECT_EQ(1.0, thisSys->m_HeatingSpeedRatio);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_MultispeedDXCoilHeatRecoveryHandling)
{

    std::string const idf_objects = delimited_string({

        "  Timestep,6;",

        "  Site:Location,",
        "    USA IL-CHICAGO-OHARE,    !- Name",
        "    41.77,                   !- Latitude {deg}",
        "    -87.75,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190;                     !- Elevation {m}",

        "SimulationControl, YES, NO, NO, YES, NO;",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO Ann Clg .4% Condns WB=>MDB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    31.2,                    !- Maximum Dry-Bulb Temperature {C}",
        "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    25.5,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    5.3,                     !- Wind Speed {m/s}",
        "    230,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.00;                    !- Sky Clearness",

        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",

        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,                                          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,    !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,        !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,      !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:DX:MultiSpeed,       !- Heating Coil Object Type",
        "  Heat Pump DX Heating Coil,      !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:MultiSpeed, !- Cooling Coil Object Type",
        "  MultiSpd Cooling Coil,  !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
        "  FlowPerCoolingCapacity, !- Supply Air Flow Rate Method During Cooling Operation",
        "  ,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  0.00005,                !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  FlowPerHeatingCapacity, !- Supply air Flow Rate Method During Heating Operation",
        "  ,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  0.00005,                !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80,                     !- Maximum Supply Air Temperature{ C }",
        "  21,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                        !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                        !- Maximum Cycling Rate {cycles/hr}",
        "  ,                        !- Heat Pump Time Constant {s}",
        "  ,                        !- Fraction of On-Cycle Power Use",
        "  ,                        !- Heat Pump Fan Delay Time {s}",
        "  ,                        !- Ancillary On-Cycle Electric Power {W}",
        "  ,                        !- Ancillary Off-Cycle Electric Power {W}",
        "  0.002,                   !- Design Heat Recovery Water Flow Rate {m3/s}",
        "  80,                      !- Maximum Temperature for Heat Recovery {C}",
        "  Water Inlet Node Name,   !- Heat Recovery Water Inlet Node Name",
        "  Water Outlet Node Name,  !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,  !- Design Specification Multispeed Heat Pump Object Type",
        "  MultiSpeed Performance;  !- Design Specification Multispeed Heat Pump Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  MultiSpeed Performance,  !- Name",
        "  4,                       !- Number of Speeds for Heating",
        "  3,                       !- Number of Speeds for Cooling",
        "  No,                      !- Single Mode Operation",
        "  ,                        !- No Load Supply Air Flow Rate Ratio",
        "  autosize,                !- Heating Speed 1 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 1 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 2 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 2 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 3 Supply Air Flow Ratio",
        "  autosize,                !- Cooling Speed 3 Supply Air Flow Ratio",
        "  autosize,                !- Heating Speed 4 Supply Air Flow Ratio",
        "  autosize;                !- Cooling Speed 4 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  autosize,               !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Cooling:DX:MultiSpeed,",
        "  MultiSpd Cooling Coil,   !- Name",
        ",                          !- Availability Schedule Name",
        "  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  ,                        !- Condenser Air Inlet Node Name",
        "  AirCooled,               !- Condenser Type",
        "  ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                        !- Supply Water Storage Tank Name",
        "  ,                        !- Condensate Collection Water Storage Tank Name",
        "  No,                      !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No,                      !- Apply Latent Degradation to Speeds Greater than 1",
        "  0.0,                     !- Crankcase Heater Capacity {W}",
        "  10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  ,                        !- Basin Heater Capacity {W/K}",
        "  ,                        !- Basin Heater Setpoint Temperature {C}",
        "  ,                        !- Basin Heater Operating Schedule Name",
        "  Electricity,             !- Fuel Type",
        "  3,                       !- Number of Speeds",
        "  autosize,                !- Speed 1 Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  3,                       !- Speed 1 Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Speed 1 Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  WindACCoolCapFT,         !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  0,                       !- Speed 1 Nominal Time for Condensate Removal to Begin {s}",
        "  0,                       !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  0,                       !- Speed 1 Maximum Cycling Rate {cycles/hr}",
        "  0,                       !- Speed 1 Latent Capacity Time Constant {s}",
        "  0.2,                     !- Speed 1 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  WindACCoolCapFT,         !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  ,                        !- Speed 1 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Speed 1 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ,                        !- Speed 1 Rated Evaporative Condenser Pump Power Consumption {W}",
        "  autosize,                !- Speed 2 Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  3,                       !- Speed 2 Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Speed 2 Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  WindACCoolCapFT,         !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  0,                       !- Speed 2 Nominal Time for Condensate Removal to Begin {s}",
        "  0,                       !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
        "  0,                       !- Speed 2 Maximum Cycling Rate {cycles/hr}",
        "  0,                       !- Speed 2 Latent Capacity Time Constant {s}",
        "  0.2,                     !- Speed 2 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  WindACCoolCapFT,         !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  ,                        !- Speed 2 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Speed 2 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ,                        !- Speed 2 Rated Evaporative Condenser Pump Power Consumption {W}",
        "  autosize,                !- Speed 3 Gross Rated Total Cooling Capacity {W}",
        "  autosize,                !- Speed 3 Gross Rated Sensible Heat Ratio",
        "  3,                       !- Speed 3 Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Speed 3 Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  WindACCoolCapFT,         !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  0,                       !- Speed 3 Nominal Time for Condensate Removal to Begin {s}",
        "  0,                       !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
        "  0,                       !- Speed 3 Maximum Cycling Rate {cycles/hr}",
        "  0,                       !- Speed 3 Latent Capacity Time Constant {s}",
        "  0.2,                     !- Speed 3 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "  WindACCoolCapFT,         !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  ,                        !- Speed 3 Evaporative Condenser Effectiveness {dimensionless}",
        "  ,                        !- Speed 3 Evaporative Condenser Air Flow Rate {m3/s}",
        "  ;                        !- Speed 3 Rated Evaporative Condenser Pump Power Consumption {W}",
        "  ",
        "Coil:Heating:DX:MultiSpeed,",
        "  Heat Pump DX Heating Coil, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node, !- Air Outlet Node Name",
        "  -8.0, !- Minimum Outdoor Dry - Bulb Temperature for Compressor Operation{ C }",
        "  -5.0, !- Outdoor Dry - Bulb Temperature to Turn On Compressor{ C }",
        "  200.0, !- Crankcase Heater Capacity{ W }",
        "  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  HPACDefrostCAPFT, !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "  7.22, !- Maximum Outdoor Dry - Bulb Temperature for Defrost Operation{ C }",
        "  ReverseCycle, !- Defrost Strategy",
        "  timed, !- Defrost Control",
        "  0.058333, !- Defrost Time Period Fraction",
        "  2000.0, !- Resistive Defrost Heater Capacity{ W }",
        "  No, !- Apply Part Load Fraction to Speeds Greater than 1",
        "  NaturalGas, !- Fuel Type",
        "  4, !- Region number for Calculating HSPF",
        "  4, !- Number of Speeds",
        "  7500, !- Speed 1 Gross Rated Heating Capacity{ W }",
        "  2.75, !- Speed 1 Gross Rated Heating COP{ W / W }",
        "  0.45, !- Speed 1 Rated Air Flow Rate{ m3 / s }",
        "  345.0, !- Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT Speed 1, !- Speed 1 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFF Speed 1, !- Speed 1 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT Speed 1, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFF Speed 1, !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACHeatPLFFPLR Speed 1, !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  0.2, !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCHeatWHFT Speed 1, !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  17500, !- Speed 2 Gross Rated Heating Capacity{ W }",
        "  2.75, !- Speed 2 Gross Rated Heating COP{ W / W }",
        "  0.85, !- Speed 2 Rated Air Flow Rate{ m3 / s }",
        "  425.0, !- Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT Speed 2, !- Speed 2 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFF Speed 2, !- Speed 2 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT Speed 2, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFF Speed 2, !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACHeatPLFFPLR Speed 2, !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  0.2, !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCHeatWHFT Speed 2, !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  25500, !- Speed 3 Gross Rated Heating Capacity{ W }",
        "  2.75, !- Speed 3 Gross Rated Heating COP{ W / W }",
        "  1.25, !- Speed 3 Rated Air Flow Rate{ m3 / s }",
        "  525.0, !- Speed 3 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT Speed 3, !- Speed 3 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFF Speed 3, !- Speed 3 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT Speed 3, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFF Speed 3, !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACHeatPLFFPLR Speed 3, !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  0.2, !- Speed 3 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCHeatWHFT Speed 3, !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  35500, !- Speed 4 Gross Rated Heating Capacity{ W }",
        "  2.75, !- Speed 4 Gross Rated Heating COP{ W / W }",
        "  1.75, !- Speed 4 Rated Air Flow Rate{ m3 / s }",
        "  673.0, !- Speed 4 Rated Supply Air Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  HPACHeatCapFT Speed 4, !- Speed 4 Heating Capacity Function of Temperature Curve Name",
        "  HPACHeatCapFF Speed 4, !- Speed 4 Heating Capacity Function of Flow Fraction Curve Name",
        "  HPACHeatEIRFT Speed 4, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACHeatEIRFF Speed 4, !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACHeatPLFFPLR Speed 4, !- Speed 4 Part Load Fraction Correlation Curve Name",
        "  0.2, !- Speed 4 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  HAPCHeatWHFT Speed 4;    !- Speed 4 Waste Heat Function of Temperature Curve Name",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFT Speed 1, !- Name",
        "  0.758746, !- Coefficient1 Constant",
        "  0.027626, !- Coefficient2 x",
        "  0.000148716, !- Coefficient3 x**2",
        "  0.0000034992, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit ",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFT Speed 2, !- Name",
        "  0.758746, !- Coefficient1 Constant",
        "  0.027626, !- Coefficient2 x",
        "  0.000148716, !- Coefficient3 x**2",
        "  0.0000034992, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFT Speed 3, !- Name",
        "  0.758746, !- Coefficient1 Constant",
        "  0.027626, !- Coefficient2 x",
        "  0.000148716, !- Coefficient3 x**2",
        "  0.0000034992, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFT Speed 4, !- Name",
        "  0.758746, !- Coefficient1 Constant",
        "  0.027626, !- Coefficient2 x",
        "  0.000148716, !- Coefficient3 x**2",
        "  0.0000034992, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFF Speed 1, !- Name",
        "  0.84, !- Coefficient1 Constant",
        "  0.16, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFF Speed 2, !- ",
        "  0.84, !- Coefficient1 Constant",
        "  0.16, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFF Speed 3, !- Name",
        "  0.84, !- Coefficient1 Constant",
        "  0.16, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatCapFF Speed 4, !- Name",
        "  0.84, !- Coefficient1 Constant",
        "  0.16, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatEIRFT Speed 1, !- Name",
        "  1.19248, !- Coefficient1 Constant",
        "  -0.0300438, !- Coefficient2 x",
        "  0.00103745, !- Coefficient3 x**2",
        "  -0.000023328, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatEIRFT Speed 2, !- Name",
        "  1.19248, !- Coefficient1 Constant",
        "  -0.0300438, !- Coefficient2 x",
        "  0.00103745, !- Coefficient3 x**2",
        "  -0.000023328, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatEIRFT Speed 3, !- Name",
        "  1.19248, !- Coefficient1 Constant",
        "  -0.0300438, !- Coefficient2 x",
        "  0.00103745, !- Coefficient3 x**2",
        "  -0.000023328, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Cubic,",
        "  HPACHeatEIRFT Speed 4, !- Name",
        "  1.19248, !- Coefficient1 Constant",
        "  -0.0300438, !- Coefficient2 x",
        "  0.00103745, !- Coefficient3 x**2",
        "  -0.000023328, !- Coefficient4 x**3",
        "  -20.0, !- Minimum Value of x",
        "  20.0, !- Maximum Value of x",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatEIRFF Speed 1, !- Name",
        "  1.3824, !- Coefficient1 Constant",
        "  -0.4336, !- Coefficient2 x",
        "  0.0512, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatEIRFF Speed 2, !- Name",
        "  1.3824, !- Coefficient1 Constant",
        "  -0.4336, !- Coefficient2 x",
        "  0.0512, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatEIRFF Speed 3, !- Name",
        "  1.3824, !- Coefficient1 Constant",
        "  -0.4336, !- Coefficient2 x",
        "  0.0512, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatEIRFF Speed 4, !- Name",
        "  1.3824, !- Coefficient1 Constant",
        "  -0.4336, !- Coefficient2 x",
        "  0.0512, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatPLFFPLR Speed 1, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatPLFFPLR Speed 2, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatPLFFPLR Speed 3, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  HPACHeatPLFFPLR Speed 4, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
        "  ",
        " Curve:Biquadratic,",
        "  HPACDefrostCAPFT, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  HAPCHeatWHFT Speed 1, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  HAPCHeatWHFT Speed 2, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  HAPCHeatWHFT Speed 3, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  HAPCHeatWHFT Speed 4, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  0, !- Minimum Value of x",
        "  50, !- Maximum Value of x",
        "  0, !- Minimum Value of y",
        "  50, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "  ",

        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8, !- Gas Burner Efficiency",
        "  32000, !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;    !- Air Outlet Node Name",

        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",

        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Curve:Quadratic,",
        "  WindACCoolCapFFF,       !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACEIRFFF,           !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACPLFFPLR,          !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  WindACCoolCapFT,        !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",

        "Curve:Biquadratic,",
        "  WindACEIRFT,            !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",

        "  Sizing:Zone,",
        "    EAST ZONE,      !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    12.8000,                 !- Zone Cooling Design Supply Air Temperature {C}",
        "    ,                        !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    50.0000,                 !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.0085,                  !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.0080,                  !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA Zone One,        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Zone Heating Sizing Factor",
        "    ,                        !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    ,                        !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    ,                        !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    ;                        !- Heating Maximum Air Flow Fraction",

        "  DesignSpecification:OutdoorAir,",
        "    SZ DSOA Zone One,  !- Name",
        "    Flow/Person,             !- Outdoor Air Method",
        "    0.0125;                  !- Outdoor Air Flow per Person {m3/s-person}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "     ,                       !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Adiabatic,               !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0.000000,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.000000,15.24000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000,15.24000,4.572,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,4.572,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0.000000,4.572,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572;  !- X,Y,Z ==> Vertex 4 {m}",

        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",

        "  Construction,",
        "   FLOOR,                   !- Name",
        "    C5 - 4 IN HW CONCRETE;   !- Outside Layer",

        "  Construction,",
        "    ROOF31,                  !- Name",
        "    R31LAYER;                !- Outside Layer",

        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    R31LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    5.456,                   !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    C5 - 4 IN HW CONCRETE,   !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  ZoneControl:Thermostat,",
        "    EAST ZONE Thermostat,    !- Name",
        "    EAST ZONE,               !- Zone or ZoneList Name",
        "    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    Zone One DualSPSched;    !- Control 1 Name",

        "  Schedule:Compact,",
        "    Dual Zone Control Type Sched,  !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,4;          !- Field 3",

        "  ThermostatSetpoint:DualSetpoint,",
        "    Zone One DualSPSched,    !- Name",
        "    HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
        "    CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",

        "  Schedule:Compact,",
        "    CLGSETP_SCH,             !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 19",
        "    Until: 24:00,26.7;       !- Field 20",

        "  Schedule:Compact,",
        "    HTGSETP_SCH,             !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 22",
        "    Until: 24:00,15.6;       !- Field 23",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    SimulationManager::GetProjectData(state);
    createFacilityElectricPowerServiceObject();

    DataGlobals::BeginSimFlag = true;
    DataGlobals::DoingSizing = true;
    SizingManager::ManageSizing(state);
    DataGlobals::DoingSizing = false;
    DataGlobals::SysSizingCalc = false;
    DataSizing::CurZoneEqNum = 1;
    DataSizing::ZoneEqSizing.allocate(1);

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool ErrorsFound = false;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above

    ASSERT_FALSE(DXCoils::DXCoil(1).MSHPHeatRecActive); // electricity
    ASSERT_TRUE(DXCoils::DXCoil(2).MSHPHeatRecActive);  // natural gas
                                                        // Minimum Outdoor Temperature for Compressor Operation blank field defaults to -25.0 C
    EXPECT_EQ(DXCoils::DXCoil(1).MinOATCompressor, -25.0);
    // Minimum Outdoor Temperature for Compressor read from input field as -8.0 C
    EXPECT_EQ(DXCoils::DXCoil(2).MinOATCompressor, -8.0);
    // Unitary System mines data from coil objects
    EXPECT_EQ(DXCoils::DXCoil(1).MinOATCompressor, thisSys->m_MinOATCompressorCooling);
    EXPECT_EQ(DXCoils::DXCoil(2).MinOATCompressor, thisSys->m_MinOATCompressorHeating);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_SizingWithFans)
{

    // Add three fans to this model - one Fan:ConstantVolume, and three Fan:SystemModel in order to make the SupFanIndex=2
    std::string const idf_objects = delimited_string({

        "  Fan:SystemModel,",
        "    Test Fan 1 ,                   !- Name",
        "    ,                            !- Availability Schedule Name",
        "    TestFanAirInletNode,         !- Air Inlet Node Name",
        "    TestFanOutletNode,           !- Air Outlet Node Name",
        "    1.0 ,                        !- Design Maximum Air Flow Rate",
        "    Discrete ,                   !- Speed Control Method",
        "    0.0,                         !- Electric Power Minimum Flow Rate Fraction",
        "    50.0,                       !- Design Pressure Rise",
        "    0.9 ,                        !- Motor Efficiency",
        "    1.0 ,                        !- Motor In Air Stream Fraction",
        "    AUTOSIZE,                    !- Design Electric Power Consumption",
        "    TotalEfficiencyAndPressure,  !- Design Power Sizing Method",
        "    ,                            !- Electric Power Per Unit Flow Rate",
        "    ,                            !- Electric Power Per Unit Flow Rate Per Unit Pressure",
        "    0.50;                        !- Fan Total Efficiency",
        "  ",
        "  Fan:SystemModel,",
        "    Test Fan 2 ,                   !- Name",
        "    ,                            !- Availability Schedule Name",
        "    TestFan2AirInletNode,         !- Air Inlet Node Name",
        "    TestFan2OutletNode,           !- Air Outlet Node Name",
        "    1.0 ,                        !- Design Maximum Air Flow Rate",
        "    Discrete ,                   !- Speed Control Method",
        "    0.0,                         !- Electric Power Minimum Flow Rate Fraction",
        "    100.0,                       !- Design Pressure Rise",
        "    0.9 ,                        !- Motor Efficiency",
        "    1.0 ,                        !- Motor In Air Stream Fraction",
        "    AUTOSIZE,                    !- Design Electric Power Consumption",
        "    TotalEfficiencyAndPressure,  !- Design Power Sizing Method",
        "    ,                            !- Electric Power Per Unit Flow Rate",
        "    ,                            !- Electric Power Per Unit Flow Rate Per Unit Pressure",
        "    0.50;                        !- Fan Total Efficiency",
        "  Fan:SystemModel,",
        "    Test Fan 3 ,                   !- Name",
        "    ,                            !- Availability Schedule Name",
        "    TestFan3AirInletNode,         !- Air Inlet Node Name",
        "    TestFan3OutletNode,           !- Air Outlet Node Name",
        "    1.005 ,                        !- Design Maximum Air Flow Rate",
        "    Discrete ,                   !- Speed Control Method",
        "    0.0,                         !- Electric Power Minimum Flow Rate Fraction",
        "    200.0,                       !- Design Pressure Rise",
        "    0.9 ,                        !- Motor Efficiency",
        "    1.0 ,                        !- Motor In Air Stream Fraction",
        "    AUTOSIZE,                    !- Design Electric Power Consumption",
        "    TotalEfficiencyAndPressure,  !- Design Power Sizing Method",
        "    ,                            !- Electric Power Per Unit Flow Rate",
        "    ,                            !- Electric Power Per Unit Flow Rate Per Unit Pressure",
        "    0.50;                        !- Fan Total Efficiency",
        "  Fan:ConstantVolume,",
        "    Test Fan 4,            !- Name",
        "    ,    !- Availability Schedule Name",
        "    0.5,                     !- Fan Total Efficiency",
        "    25.0,                   !- Pressure Rise {Pa}",
        "    1.0,                  !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    TestFan4AirInletNode,         !- Air Inlet Node Name",
        "    TestFan4OutletNode;           !- Air Outlet Node Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    std::string fanName = "TEST FAN 1";
    HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(state, fanName)); // call constructor

    fanName = "TEST FAN 2";
    HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(state, fanName)); // call constructor

    fanName = "TEST FAN 3";
    HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(state, fanName)); // call constructor
    DataSizing::CurZoneEqNum = 0;
    DataSizing::CurSysNum = 0;
    DataSizing::CurOASysNum = 0;
    DataEnvironment::StdRhoAir = 1.2;
    HVACFan::fanObjs[2]->simulate(state, _, _, _, _);                  // triggers sizing call
    Real64 locFanSizeVdot = HVACFan::fanObjs[2]->designAirVolFlowRate; // get function
    Real64 locDesignHeatGain3 = HVACFan::fanObjs[2]->getFanDesignHeatGain(state, locFanSizeVdot);
    EXPECT_NEAR(locDesignHeatGain3, 402.0, 0.1);

    Fans::GetFanInput(state);
    Real64 locDesignHeatGain4 = Fans::FanDesHeatGain(state, 1, locFanSizeVdot);
    EXPECT_NEAR(locDesignHeatGain4, 50.25, 0.1);

    DataSizing::DataTotCapCurveIndex = 0;
    DataSizing::DataDesOutletAirTemp = 0.0;

    DataSizing::CurSysNum = 1;
    DataSizing::FinalSysSizing.allocate(1);
    DataSizing::FinalSysSizing(DataSizing::CurSysNum).CoolSupTemp = 12.0;
    DataSizing::FinalSysSizing(DataSizing::CurSysNum).CoolSupHumRat = 0.0085;
    DataSizing::FinalSysSizing(DataSizing::CurSysNum).MixTempAtCoolPeak = 28.0;
    DataSizing::FinalSysSizing(DataSizing::CurSysNum).MixHumRatAtCoolPeak = 0.0075;
    DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesCoolVolFlow = 1.005;
    DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesOutAirVolFlow = 0.2;

    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).NumOACoolCoils = 0;
    DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).SupFanNum = 0;
    DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).RetFanNum = 0;
    DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).supFanModelTypeEnum = DataAirSystems::fanModelTypeNotYetSet;

    DataSizing::SysSizingRunDone = true;
    DataSizing::SysSizInput.allocate(1);
    DataSizing::SysSizInput(1).AirLoopNum = DataSizing::CurSysNum;
    DataSizing::NumSysSizInput = 1;

    DataEnvironment::StdBaroPress = 101325.0;
    Psychrometrics::InitializePsychRoutines();

    // Need this to prevent crash in Sizers
    DataSizing::UnitarySysEqSizing.allocate(1);
    DataSizing::OASysEqSizing.allocate(1);
    DataSizing::SysSizPeakDDNum.allocate(1);

    int AirLoopNum(1);
    bool FirstHVACIteration(true);
    UnitarySys thisSys;
    UnitarySys *mySys(&thisSys);
    UnitarySystems::numUnitarySystems = 1;

    DataEnvironment::StdRhoAir = 1.2; // Prevent divide by zero in Sizer

    thisSys.UnitType = "AirLoopHVAC:UnitarySystem";
    thisSys.m_MultiOrVarSpeedCoolCoil = false;
    thisSys.m_MultiOrVarSpeedHeatCoil = false;
    thisSys.UnitarySystemType_Num = DataHVACGlobals::UnitarySys_AnyCoilType;
    thisSys.m_RequestAutoSize = true;

    // test cooling only sizing
    thisSys.m_FanExists = true;
    thisSys.m_CoolCoilExists = true;
    thisSys.m_HeatCoilExists = false;

    thisSys.Name = "UnitarySystem:CoolingOnly";
    thisSys.m_CoolingSAFMethod = DataSizing::FractionOfAutosizedCoolingAirflow;
    thisSys.m_DesignCoolingCapacity = DataSizing::AutoSize;
    thisSys.m_MaxCoolAirVolFlow = 1.0;
    thisSys.m_MaxHeatAirVolFlow = DataSizing::AutoSize;
    thisSys.m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
    thisSys.m_DesignFanVolFlowRate = DataSizing::AutoSize;

    // With Test Fan 3 fan heat - this fails before the #6026 fix in UnitarySystem (and in Sizer)
    thisSys.m_FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
    thisSys.m_FanIndex = 2; // Fan:SystemModel is zero-based subscripts, so 2 is 3
    Real64 expectedSize = 18976.394 + locDesignHeatGain3;

    mySys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

    EXPECT_EQ(1.005, thisSys.m_MaxCoolAirVolFlow);
    EXPECT_NEAR(expectedSize, thisSys.m_DesignCoolingCapacity, 0.001);

    // reset for next test
    thisSys.m_DesignCoolingCapacity = DataSizing::AutoSize;
    thisSys.m_MaxCoolAirVolFlow = 1.0;
    thisSys.m_MaxHeatAirVolFlow = DataSizing::AutoSize;
    thisSys.m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
    thisSys.m_DesignFanVolFlowRate = DataSizing::AutoSize;

    // With Test Fan 4 fan heat
    thisSys.m_FanType_Num = DataHVACGlobals::FanType_SimpleConstVolume;
    thisSys.m_FanIndex = 1; // Fan:ConstantVolume is one-based subscripts, so 1 is 1
    expectedSize = 18976.394 + locDesignHeatGain4;

    mySys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

    EXPECT_EQ(1.005, thisSys.m_MaxCoolAirVolFlow);
    EXPECT_NEAR(expectedSize, thisSys.m_DesignCoolingCapacity, 0.001);

    // clean
    DataSizing::NumSysSizInput = 0;
    DataSizing::FinalSysSizing.deallocate();
    DataAirSystems::PrimaryAirSystem.deallocate();
    DataSizing::SysSizInput.deallocate();
    DataSizing::UnitarySysEqSizing.deallocate();
    DataSizing::OASysEqSizing.deallocate();
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_GetInputATMixerInlet)
{

    std::string const idf_objects = delimited_string({

        "AirTerminal:SingleDuct:Mixer,",
        "    East Zone DOAS Air Terminal,   !- Name",
        "    AirLoopHVAC:UnitarySystem,     !- ZoneHVAC Terminal Unit Object Type",
        "    Unitary System Model,          !- ZoneHVAC Terminal Unit Name",
        "    East Zone Unitary System Inlet,!- Terminal Unit Outlet Node Name",
        "    East Zone Air Terminal Mixer Primary Inlet,   !- Terminal Unit Primary Air Inlet Node Name",
        "    East Zone Air Terminal Mixer Secondary Inlet, !- Terminal Unit Secondary Air Inlet Node Name",
        "    InletSide,          !- Terminal Unit Connection Type",
        "    ;                   !- Design Specification Outdoor Air Object Name",

        "ZoneHVAC:AirDistributionUnit,",
        "    East Zone DOAS ATU,       !- Name",
        "    East Zone Unitary System Inlet, !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:Mixer, !- Air Terminal Object Type",
        "    East Zone DOAS Air Terminal;  !- Air Terminal Name",

        "ZoneHVAC:EquipmentList,",
        "    East Zone Equipment,     !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    East Zone DOAS ATU,      !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "    ,                        !- Zone Equipment 1 Sequential Heating Fraction",
        "    AirLoopHVAC:UnitarySystem,  !- Zone Equipment 2 Object Type",
        "    Unitary System Model,    !- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2,                       !- Zone Equipment 2 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 2 Sequential Cooling Fraction",
        "    ;                        !- Zone Equipment 2 Sequential Heating Fraction",

        "Zone,",
        "    East Zone;                !- Name",

        "ZoneHVAC:EquipmentConnections,",
        "    East Zone,                !- Zone Name",
        "    East Zone Equipment,      !- Zone Conditioning Equipment List Name",
        "    East Zone Supply Inlet,   !- Zone Air Inlet Node or NodeList Name",
        "    East Zone Air Terminal Mixer Secondary Inlet,  !- Zone Air Exhaust Node or NodeList Name",
        "    East Zone Zone Air Node,  !- Zone Air Node Name",
        "    East Zone Return Outlet;  !- Zone Return Air Node Name",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  ,                               !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  ,                               !- Availability Schedule Name",
        "  East Zone Unitary System Inlet, !- Air Inlet Node Name",
        "  East Zone Supply Inlet,         !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Electric,          !- Heating Coil Object Type",
        "  Electric Heating Coil,          !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  ,                               !- Cooling Coil Object Type",
        "  ,                               !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  ,                               !- Design Specification Multispeed Object Type",
        "  ;                               !- Design Specification Multispeed Object Name",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  ,                               !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  East Zone Unitary System Inlet, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Heating:Electric,",
        "  Electric Heating Coil,          !- Name",
        "  ,                               !- Availability Schedule Name",
        "  1.0,                            !- Efficiency",
        "  autosize,                       !- Nominal Capacity",
        "  Heating Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  East Zone Supply Inlet,         !- Air Outlet Node Name",
        "  ;                               !- Temperature Setpoint Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1(state);
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(state);

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    UnitarySys mySys;
    HVACSystemData *thisSys = DataZoneEquipment::ZoneEquipList(1).compPointer[2];    // UnitarySystem is the 2nd in the zone equipment list
    DataZoneEquipment::ZoneEquipInputsFilled = true;                                 // indicate zone data is available
    mySys.getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    mySys = UnitarySystems::unitarySys[0];

    EXPECT_NE(nullptr, thisSys);
    EXPECT_FALSE(ErrorsFound); // expect no errors
    EXPECT_TRUE(mySys.ATMixerExists);
    EXPECT_EQ(DataHVACGlobals::ATMixer_InletSide, mySys.ATMixerType);
    // EXPECT_FALSE(mySys.m_AirLoopEquipment);
    EXPECT_EQ(0, mySys.ControlZoneNum); // control zone name/index not required for setpoint control
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_GetInputATMixerSupply)
{

    std::string const idf_objects = delimited_string({

        "AirTerminal:SingleDuct:Mixer,",
        "    East Zone DOAS Air Terminal,   !- Name",
        "    AirLoopHVAC:UnitarySystem,     !- ZoneHVAC Terminal Unit Object Type",
        "    Unitary System Model,          !- ZoneHVAC Terminal Unit Name",
        "    East Zone Supply Inlet,        !- Terminal Unit Outlet Node Name",
        "    East Zone Air Terminal Mixer Primary Inlet,   !- Terminal Unit Primary Air Inlet Node Name",
        "    East Zone Unitary System Outlet, !- Terminal Unit Secondary Air Inlet Node Name",
        "    SupplySide,         !- Terminal Unit Connection Type",
        "    ;                   !- Design Specification Outdoor Air Object Name",

        "ZoneHVAC:AirDistributionUnit,",
        "    East Zone DOAS ATU,       !- Name",
        "    East Zone Supply Inlet,   !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:Mixer, !- Air Terminal Object Type",
        "    East Zone DOAS Air Terminal;  !- Air Terminal Name",

        "ZoneHVAC:EquipmentList,",
        "    East Zone Equipment,     !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    East Zone DOAS ATU,      !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "    ,                        !- Zone Equipment 1 Sequential Heating Fraction",
        "    AirLoopHVAC:UnitarySystem,  !- Zone Equipment 2 Object Type",
        "    Unitary System Model,    !- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2,                       !- Zone Equipment 2 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 2 Sequential Cooling Fraction",
        "    ;                        !- Zone Equipment 2 Sequential Heating Fraction",

        "Zone,",
        "    East Zone;                !- Name",

        "ZoneHVAC:EquipmentConnections,",
        "    East Zone,                !- Zone Name",
        "    East Zone Equipment,      !- Zone Conditioning Equipment List Name",
        "    East Zone Supply Inlet,   !- Zone Air Inlet Node or NodeList Name",
        "    East Zone Unitary System Inlet,  !- Zone Air Exhaust Node or NodeList Name",
        "    East Zone Zone Air Node,  !- Zone Air Node Name",
        "    East Zone Return Outlet;  !- Zone Return Air Node Name",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  ,                               !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  ,                               !- Availability Schedule Name",
        "  East Zone Unitary System Inlet, !- Air Inlet Node Name",
        "  East Zone Unitary System Outlet,  !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Electric,          !- Heating Coil Object Type",
        "  Electric Heating Coil,          !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  ,                               !- Cooling Coil Object Type",
        "  ,                               !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  ,                               !- Design Specification Multispeed Object Type",
        "  ;                               !- Design Specification Multispeed Object Name",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  ,                               !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  East Zone Unitary System Inlet, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Heating:Electric,",
        "  Electric Heating Coil,          !- Name",
        "  ,                               !- Availability Schedule Name",
        "  1.0,                            !- Efficiency",
        "  autosize,                       !- Nominal Capacity",
        "  Heating Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  East Zone Supply Inlet,         !- Air Outlet Node Name",
        "  ;                               !- Temperature Setpoint Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1(state);
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(state);

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    UnitarySys mySys;
    HVACSystemData *thisSys = DataZoneEquipment::ZoneEquipList(1).compPointer[2];    // UnitarySystem is the 2nd in the zone equipment list
    DataZoneEquipment::ZoneEquipInputsFilled = true;                                 // indicate zone data is available
    mySys.getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    mySys = UnitarySystems::unitarySys[0];
    EXPECT_TRUE(thisSys);
    EXPECT_FALSE(ErrorsFound); // expect no errors
    EXPECT_TRUE(mySys.ATMixerExists);
    EXPECT_EQ(DataHVACGlobals::ATMixer_SupplySide, mySys.ATMixerType);
    // EXPECT_FALSE(mySys.m_AirLoopEquipment);
    EXPECT_EQ(0, mySys.ControlZoneNum); // control zone name/index not required for setpoint control
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_GetInputZoneEquipment)
{

    std::string const idf_objects = delimited_string({

        "ZoneHVAC:EquipmentList,",
        "    East Zone Equipment,     !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    AirLoopHVAC:UnitarySystem,  !- Zone Equipment 1 Object Type",
        "    Unitary System Model,    !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "    ;                        !- Zone Equipment 1 Sequential Heating Fraction",

        "Zone,",
        "    East Zone;                !- Name",

        "ZoneHVAC:EquipmentConnections,",
        "    East Zone,                !- Zone Name",
        "    East Zone Equipment,      !- Zone Conditioning Equipment List Name",
        "    East Zone Supply Inlet,   !- Zone Air Inlet Node or NodeList Name",
        "    East Zone Unitary System Inlet,  !- Zone Air Exhaust Node or NodeList Name",
        "    East Zone Zone Air Node,  !- Zone Air Node Name",
        "    East Zone Return Outlet;  !- Zone Return Air Node Name",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  ,                               !- Availability Schedule Name",
        "  East Zone Unitary System Inlet, !- Air Inlet Node Name",
        "  East Zone Supply Inlet,         !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Electric,          !- Heating Coil Object Type",
        "  Electric Heating Coil,          !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  ,                               !- Cooling Coil Object Type",
        "  ,                               !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  ,                               !- Design Specification Multispeed Object Type",
        "  ;                               !- Design Specification Multispeed Object Name",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  ,                               !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  East Zone Unitary System Inlet, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Heating:Electric,",
        "  Electric Heating Coil,          !- Name",
        "  ,                               !- Availability Schedule Name",
        "  1.0,                            !- Efficiency",
        "  autosize,                       !- Nominal Capacity",
        "  Heating Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  East Zone Supply Inlet,         !- Air Outlet Node Name",
        "  ;                               !- Temperature Setpoint Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1(state);
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(state);

    // call the UnitarySystem factory
    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input

    EXPECT_FALSE(ErrorsFound); // expect no errors
    EXPECT_FALSE(thisSys->ATMixerExists);
    EXPECT_EQ(1, thisSys->ControlZoneNum);
    EXPECT_EQ(DataLoopNode::NodeID(2), "EAST ZONE UNITARY SYSTEM INLET");
    EXPECT_EQ(2, thisSys->AirInNode);
    EXPECT_EQ(DataLoopNode::NodeID(3), "EAST ZONE SUPPLY INLET");
    EXPECT_EQ(3, thisSys->AirOutNode);
    EXPECT_EQ(DataLoopNode::NodeID(5), "HEATING COIL AIR INLET NODE");
    EXPECT_EQ(5, thisSys->HeatCoilInletNodeNum);
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_GetInputZoneEquipmentBlankCtrlZone)
{

    std::string const idf_objects = delimited_string({

        "ZoneHVAC:EquipmentList,",
        "    East Zone Equipment,     !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    AirLoopHVAC:UnitarySystem,  !- Zone Equipment 1 Object Type",
        "    Unitary System Model,    !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "    ;                        !- Zone Equipment 1 Sequential Heating Fraction",

        "Zone,",
        "    East Zone;                !- Name",

        "ZoneHVAC:EquipmentConnections,",
        "    East Zone,                !- Zone Name",
        "    East Zone Equipment,      !- Zone Conditioning Equipment List Name",
        "    East Zone Supply Inlet,   !- Zone Air Inlet Node or NodeList Name",
        "    East Zone Unitary System Inlet,  !- Zone Air Exhaust Node or NodeList Name",
        "    East Zone Zone Air Node,  !- Zone Air Node Name",
        "    East Zone Return Outlet;  !- Zone Return Air Node Name",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Load,                           !- Control Type",
        "  ,                               !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  ,                               !- Availability Schedule Name",
        "  East Zone Unitary System Inlet, !- Air Inlet Node Name",
        "  East Zone Supply Inlet,         !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Electric,          !- Heating Coil Object Type",
        "  Electric Heating Coil,          !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  ,                               !- Cooling Coil Object Type",
        "  ,                               !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  ,                               !- Design Specification Multispeed Object Type",
        "  ;                               !- Design Specification Multispeed Object Name",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  ,                               !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  East Zone Unitary System Inlet, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Heating:Electric,",
        "  Electric Heating Coil,          !- Name",
        "  ,                               !- Availability Schedule Name",
        "  1.0,                            !- Efficiency",
        "  autosize,                       !- Nominal Capacity",
        "  Heating Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  East Zone Supply Inlet,         !- Air Outlet Node Name",
        "  ;                               !- Temperature Setpoint Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1(state);
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(state);
    UnitarySys thisSys;

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    DataZoneEquipment::ZoneEquipInputsFilled = true;                                   // indicate zone data is available
    thisSys.getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input
    EXPECT_TRUE(ErrorsFound); // expect errors when control zone name is blank and Control Type = Load
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_FractionOfAutoSizedCoolingValueTest)
{

    std::string const idf_objects = delimited_string({

        "  AirLoopHVAC:UnitarySystem,",
        "    Unitary System Model,                   !- Name",
        "    Load,                                   !- Control Type",
        "    EAST ZONE,                              !- Controlling Zone or Thermostat Location",
        "    None,                                   !- Dehumidification Control Type",
        "    ,                                       !- Availability Schedule Name",
        "    Node 29,                                !- Air Inlet Node Name",
        "    Node 30,                                !- Air Outlet Node Name",
        "    Fan:OnOff,                              !- Supply Fan Object Type",
        "    Supply Fan,                             !- Supply Fan Name",
        "    DrawThrough,                            !- Fan Placement",
        "    ,                                       !- Supply Air Fan Operating Mode Schedule Name",
        "    Coil:Heating:Water,                     !- Heating Coil Object Type",
        "    Water Heating Coil,                     !- Heating Coil Name",
        "    1,                                      !- DX Heating Coil Sizing Ratio",
        "    Coil:Cooling:Water,                     !- Cooling Coil Object Type",
        "    Water Cooling Coil,                     !- Cooling Coil Name",
        "    No,                                     !- Use DOAS DX Cooling Coil",
        "    12.0,                                   !- Minimum Supply Air Temperature {C}",
        "    SensibleOnlyLoadControl,                !- Latent Load Control",
        "    ,                                       !- Supplemental Heating Coil Object Type",
        "    ,                                       !- Supplemental Heating Coil Name",
        "    ,                                       !- Cooling Supply Air Flow Rate Method",
        "    Autosize,                               !- Cooling Supply Air Flow Rate {m3/s}",
        "    ,                                       !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                                       !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                                       !- Cooling Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "    ,                                       !- Heating Supply Air Flow Rate Method",
        "    Autosize,                               !- Heating Supply Air Flow Rate {m3/s}",
        "    ,                                       !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                                       !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "    ,                                       !- Heating Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "    FractionOfAutosizedCoolingValue,        !- No Load Supply Air Flow Rate Method",
        "    ,                                       !- No Load Supply Air Flow Rate {m3/s}",
        "    ,                                       !- No Load Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    0.9,                                    !- No Load Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                                       !- No Load Fraction of Autosized Heating Supply Air Flow Rate",
        "    ,                                       !- No Load Supply Air Flow Rate Per Unit of Capacity During Cooling Operation {m3/s-W}",
        "    ,                                       !- No Load Supply Air Flow Rate Per Unit of Capacity During Heating Operation {m3/s-W}",
        "    28.0,                                   !- Maximum Supply Air Temperature {C}",
        "    21,                                     !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "    ,                                       !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "    2.5,                                    !- Maximum Cycling Rate {cycles/hr}",
        "    60,                                     !- Heat Pump Time Constant {s}",
        "    0.01,                                   !- Fraction of On-Cycle Power Use",
        "    60,                                     !- Heat Pump Fan Delay Time {s}",
        "    0,                                      !- Ancillary On-Cycle Electric Power {W}",
        "    0;                                      !- Ancillary Off-Cycle Electric Power {W}",

        "  Fan:OnOff,",
        "    Supply Fan,                             !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    0.7,                                    !- Fan Total Efficiency",
        "    600.0,                                  !- Pressure Rise{ Pa }",
        "    autosize,                               !- Maximum Flow Rate{ m3 / s }",
        "    0.9,                                    !- Motor Efficiency",
        "    1.0,                                    !- Motor In Airstream Fraction",
        "    Thermal Zone one System Heating Coil - Fan Node,  !- Air Inlet Node Name",
        "    Node 30;                                !- Air Outlet Node Name",

        "  Coil:Heating:Water,",
        "    Water Heating Coil,                     !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    Autosize,                               !- U-Factor Times Area Value {W/K}",
        "    Autosize,                               !- Maximum Water Flow Rate {m3/s}",
        "    Node 31,                                !- Water Inlet Node Name",
        "    Node 32,                                !- Water Outlet Node Name",
        "    Thermal Zone one System Cooling Coil - Heating Coil Node, !- Air Inlet Node Name",
        "    Thermal Zone one System Heating Coil - Fan Node, !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "    Autosize,                               !- Rated Capacity {W}",
        "    82.2,                                   !- Rated Inlet Water Temperature {C}",
        "    16.6,                                   !- Rated Inlet Air Temperature {C}",
        "    71.1,                                   !- Rated Outlet Water Temperature {C}",
        "    32.2,                                   !- Rated Outlet Air Temperature {C}",
        "    0.5;                                    !- Rated Ratio for Air and Water Convection",

        "  Coil:Cooling:Water,",
        "    Water Cooling Coil,                     !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    Autosize,                               !- Design Water Flow Rate {m3/s}",
        "    Autosize,                               !- Design Air Flow Rate {m3/s}",
        "    Autosize,                               !- Design Inlet Water Temperature {C}",
        "    Autosize,                               !- Design Inlet Air Temperature {C}",
        "    Autosize,                               !- Design Outlet Air Temperature {C}",
        "    Autosize,                               !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    Autosize,                               !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    Node 33,                                !- Water Inlet Node Name",
        "    Node 34,                                !- Water Outlet Node Name",
        "    Node 29,                                !- Air Inlet Node Name",
        "    Thermal Zone one System Cooling Coil - Heating Coil Node, !- Air Outlet Node Name",
        "    SimpleAnalysis,                         !- Type of Analysis",
        "    CrossFlow;                              !- Heat Exchanger Configuration",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    // call the UnitarySystem factory
    bool ErrorsFound = false;
    bool zoneEquipment = true;
    std::string compName = "UNITARY SYSTEM MODEL";
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    OutputReportPredefined::SetPredefinedTables();
    DataSizing::ZoneSizingRunDone = true;
    // DataSizing::NumPltSizInput = 2;

    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(1).Name;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterSimpleHeating;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum;

    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(2).Name;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterCooling;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(2).WaterInletNodeNum;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state.dataWaterCoils->WaterCoil(2).WaterOutletNodeNum;

    // check user specified values before overriding during sizing
    Real64 userspecifiedFractionOfAutoSizedCoolingFlowRateValue = thisSys->m_MaxNoCoolHeatAirVolFlow;
    EXPECT_EQ(thisSys->m_NoCoolHeatSAFMethod, UnitarySystems::FractionOfAutoSizedCoolingValue);
    EXPECT_EQ(userspecifiedFractionOfAutoSizedCoolingFlowRateValue, 0.9);

    bool FirstHVACIteration = true;
    int AirLoopNum = 0;
    thisSys->sizeSystem(state, FirstHVACIteration, AirLoopNum);
    // check autosized cooling and heating flow rates
    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, 1.5);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, 1.5);
    // check autosized no cooling and no heating flow rates
    EXPECT_NEAR(thisSys->m_MaxNoCoolHeatAirVolFlow, userspecifiedFractionOfAutoSizedCoolingFlowRateValue * thisSys->m_MaxCoolAirVolFlow, 0.000001);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_FlowPerCoolingCapacityTest)
{

    std::string const idf_objects = delimited_string({

        "  AirLoopHVAC:UnitarySystem,",
        "    Unitary System Model,                   !- Name",
        "    Load,                                   !- Control Type",
        "    EAST ZONE,                              !- Controlling Zone or Thermostat Location",
        "    None,                                   !- Dehumidification Control Type",
        "    ,                                       !- Availability Schedule Name",
        "    Node 29,                                !- Air Inlet Node Name",
        "    Node 30,                                !- Air Outlet Node Name",
        "    Fan:OnOff,                              !- Supply Fan Object Type",
        "    Supply Fan,                             !- Supply Fan Name",
        "    DrawThrough,                            !- Fan Placement",
        "    ,                                       !- Supply Air Fan Operating Mode Schedule Name",
        "    Coil:Heating:Water,                     !- Heating Coil Object Type",
        "    Water Heating Coil,                     !- Heating Coil Name",
        "    1,                                      !- DX Heating Coil Sizing Ratio",
        "    Coil:Cooling:Water,                     !- Cooling Coil Object Type",
        "    Water Cooling Coil,                     !- Cooling Coil Name",
        "    No,                                     !- Use DOAS DX Cooling Coil",
        "    12.0,                                   !- Minimum Supply Air Temperature {C}",
        "    SensibleOnlyLoadControl,                !- Latent Load Control",
        "    ,                                       !- Supplemental Heating Coil Object Type",
        "    ,                                       !- Supplemental Heating Coil Name",
        "    ,                                       !- Cooling Supply Air Flow Rate Method",
        "    Autosize,                               !- Cooling Supply Air Flow Rate {m3/s}",
        "    ,                                       !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                                       !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                                       !- Cooling Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "    ,                                       !- Heating Supply Air Flow Rate Method",
        "    Autosize,                               !- Heating Supply Air Flow Rate {m3/s}",
        "    ,                                       !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                                       !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "    ,                                       !- Heating Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "    FlowPerCoolingCapacity,                 !- No Load Supply Air Flow Rate Method",
        "    ,                                       !- No Load Supply Air Flow Rate {m3/s}",
        "    ,                                       !- No Load Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                                       !- No Load Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                                       !- No Load Fraction of Autosized Heating Supply Air Flow Rate",
        "    0.0000462180155978106,                  !- No Load Supply Air Flow Rate Per Unit of Capacity During Cooling Operation {m3/s-W}",
        "    ,                                       !- No Load Supply Air Flow Rate Per Unit of Capacity During Heating Operation {m3/s-W}",
        "    28.0,                                   !- Maximum Supply Air Temperature {C}",
        "    21,                                     !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "    ,                                       !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "    2.5,                                    !- Maximum Cycling Rate {cycles/hr}",
        "    60,                                     !- Heat Pump Time Constant {s}",
        "    0.01,                                   !- Fraction of On-Cycle Power Use",
        "    60,                                     !- Heat Pump Fan Delay Time {s}",
        "    0,                                      !- Ancillary On-Cycle Electric Power {W}",
        "    0;                                      !- Ancillary Off-Cycle Electric Power {W}",

        "  Fan:OnOff,",
        "    Supply Fan,                             !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    0.7,                                    !- Fan Total Efficiency",
        "    600.0,                                  !- Pressure Rise{ Pa }",
        "    autosize,                               !- Maximum Flow Rate{ m3 / s }",
        "    0.9,                                    !- Motor Efficiency",
        "    1.0,                                    !- Motor In Airstream Fraction",
        "    Thermal Zone one System Heating Coil - Fan Node,  !- Air Inlet Node Name",
        "    Node 30;                                !- Air Outlet Node Name",

        "  Coil:Heating:Water,",
        "    Water Heating Coil,                     !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    Autosize,                               !- U-Factor Times Area Value {W/K}",
        "    Autosize,                               !- Maximum Water Flow Rate {m3/s}",
        "    Node 31,                                !- Water Inlet Node Name",
        "    Node 32,                                !- Water Outlet Node Name",
        "    Thermal Zone one System Cooling Coil - Heating Coil Node, !- Air Inlet Node Name",
        "    Thermal Zone one System Heating Coil - Fan Node, !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "    Autosize,                               !- Rated Capacity {W}",
        "    82.2,                                   !- Rated Inlet Water Temperature {C}",
        "    16.6,                                   !- Rated Inlet Air Temperature {C}",
        "    71.1,                                   !- Rated Outlet Water Temperature {C}",
        "    32.2,                                   !- Rated Outlet Air Temperature {C}",
        "    0.5;                                    !- Rated Ratio for Air and Water Convection",

        "  Coil:Cooling:Water,",
        "    Water Cooling Coil,                     !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    Autosize,                               !- Design Water Flow Rate {m3/s}",
        "    Autosize,                               !- Design Air Flow Rate {m3/s}",
        "    Autosize,                               !- Design Inlet Water Temperature {C}",
        "    Autosize,                               !- Design Inlet Air Temperature {C}",
        "    Autosize,                               !- Design Outlet Air Temperature {C}",
        "    Autosize,                               !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    Autosize,                               !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    Node 33,                                !- Water Inlet Node Name",
        "    Node 34,                                !- Water Outlet Node Name",
        "    Node 29,                                !- Air Inlet Node Name",
        "    Thermal Zone one System Cooling Coil - Heating Coil Node, !- Air Outlet Node Name",
        "    SimpleAnalysis,                         !- Type of Analysis",
        "    CrossFlow;                              !- Heat Exchanger Configuration",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    // call the UnitarySystem factory
    bool ErrorsFound = false;
    bool zoneEquipment = true;
    std::string compName = "UNITARY SYSTEM MODEL";
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    OutputReportPredefined::SetPredefinedTables();
    DataSizing::ZoneSizingRunDone = true;

    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(1).Name;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterSimpleHeating;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum;
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum;

    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(2).Name;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_CoilWaterCooling;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(2).WaterInletNodeNum;
    DataPlant::PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state.dataWaterCoils->WaterCoil(2).WaterOutletNodeNum;

    // check user specified values before overriding during sizing
    Real64 userspecifiedFlowPerCoolingCapacityValue = thisSys->m_MaxNoCoolHeatAirVolFlow;
    EXPECT_EQ(thisSys->m_NoCoolHeatSAFMethod, UnitarySystems::FlowPerCoolingCapacity);
    EXPECT_EQ(userspecifiedFlowPerCoolingCapacityValue, 0.0000462180155978106);

    bool FirstHVACIteration = true;
    int AirLoopNum = 0;
    thisSys->sizeSystem(state, FirstHVACIteration, AirLoopNum);
    // check autosized cooling and heating flow rates
    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, 1.5);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, 1.5);
    // check autosized no cooling and no heating flow rates
    EXPECT_NEAR(thisSys->m_MaxNoCoolHeatAirVolFlow, userspecifiedFlowPerCoolingCapacityValue * thisSys->m_DesignCoolingCapacity, 0.0000001);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_getUnitarySystemInputDataTest)
{

    std::string const idf_objects = delimited_string({

        "  AirLoopHVAC:UnitarySystem,",
        "    Unitary System Model,                   !- Name",
        "    Load,                                   !- Control Type",
        "    EAST ZONE,                              !- Controlling Zone or Thermostat Location",
        "    None,                                   !- Dehumidification Control Type",
        "    ,                                       !- Availability Schedule Name",
        "    Node 29,                                !- Air Inlet Node Name",
        "    Node 30,                                !- Air Outlet Node Name",
        "    Fan:OnOff,                              !- Supply Fan Object Type",
        "    Supply Fan,                             !- Supply Fan Name",
        "    DrawThrough,                            !- Fan Placement",
        "    ,                                       !- Supply Air Fan Operating Mode Schedule Name",
        "    Coil:Heating:Water,                     !- Heating Coil Object Type",
        "    Water Heating Coil,                     !- Heating Coil Name",
        "    1,                                      !- DX Heating Coil Sizing Ratio",
        "    Coil:Cooling:Water,                     !- Cooling Coil Object Type",
        "    Water Cooling Coil,                     !- Cooling Coil Name",
        "    No,                                     !- Use DOAS DX Cooling Coil",
        "    7.0,                                    !- Minimum Supply Air Temperature {C}",
        "    SensibleOnlyLoadControl,                !- Latent Load Control",
        "    Coil:Heating:Fuel,                      !- Supplemental Heating Coil Object Type",
        "    Supplemental Heating Coil,              !- Supplemental Heating Coil Name",
        "    FractionOfAutosizedCoolingValue,        !- Cooling Supply Air Flow Rate Method",
        "    ,                                       !- Cooling Supply Air Flow Rate {m3/s}",
        "    ,                                       !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    0.5,                                    !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                                       !- Cooling Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "    FractionOfAutosizedHeatingValue,        !- Heating Supply Air Flow Rate Method",
        "    ,                                       !- Heating Supply Air Flow Rate {m3/s}",
        "    ,                                       !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    0.8,                                    !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "    ,                                       !- Heating Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "    FlowPerCoolingCapacity,                 !- No Load Supply Air Flow Rate Method",
        "    ,                                       !- No Load Supply Air Flow Rate {m3/s}",
        "    ,                                       !- No Load Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                                       !- No Load Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                                       !- No Load Fraction of Autosized Heating Supply Air Flow Rate",
        "    0.0000462180155978106,                  !- No Load Supply Air Flow Rate Per Unit of Capacity During Cooling Operation {m3/s-W}",
        "    ,                                       !- No Load Supply Air Flow Rate Per Unit of Capacity During Heating Operation {m3/s-W}",
        "    30.0,                                   !- Maximum Supply Air Temperature {C}",
        "    20.0,                                   !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "    System Outdoor Air Node,                !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "    3.0,                                    !- Maximum Cycling Rate {cycles/hr}",
        "    120.0,                                  !- Heat Pump Time Constant {s}",
        "    0.02,                                   !- Fraction of On-Cycle Power Use",
        "    90,                                     !- Heat Pump Fan Delay Time {s}",
        "    40,                                     !- Ancillary On-Cycle Electric Power {W}",
        "    10,                                     !- Ancillary Off-Cycle Electric Power {W}",
        "    0.005,                                  !- Design Heat Recovery Water Flow Rate {m3/s}",
        "    75.0,                                   !- Maximum Temperature for Heat Recovery {C}",
        "    Water Inlet Node Name,                  !- Heat Recovery Water Inlet Node Name",
        "    Water Outlet Node Name,                 !- Heat Recovery Water Outlet Node Name",
        "    UnitarySystemPerformance:Multispeed,    !- Design Specification Multispeed Heat Pump Object Type",
        "    MultiSpeed Performance;                 !- Design Specification Multispeed Heat Pump Object Name",

        "  OutdoorAir:Node,",
        "    System Outdoor Air Node;",

        "  UnitarySystemPerformance:Multispeed,",
        "    MultiSpeed Performance,                 !- Name",
        "    1,                                      !- Number of Speeds for Heating",
        "    2,                                      !- Number of Speeds for Cooling",
        "    No,                                     !- Single Mode Operation",
        "    ,                                       !- No Load Supply Air Flow Rate Ratio",
        "    AutoSize,                               !- Heating Speed 1 Supply Air Flow Ratio",
        "    AutoSize,                               !- Cooling Speed 1 Supply Air Flow Ratio",
        "    AutoSize,                               !- Heating Speed 2 Supply Air Flow Ratio",
        "    AutoSize;                               !- Cooling Speed 2 Supply Air Flow Ratio",

        "  Fan:OnOff,",
        "    Supply Fan,                             !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    0.7,                                    !- Fan Total Efficiency",
        "    600.0,                                  !- Pressure Rise{ Pa }",
        "    autosize,                               !- Maximum Flow Rate{ m3 / s }",
        "    0.9,                                    !- Motor Efficiency",
        "    1.0,                                    !- Motor In Airstream Fraction",
        "    Thermal Zone one System Heating Coil - Fan Node,  !- Air Inlet Node Name",
        "    Supplemental Heating Coil Inlet Node;   !- Air Outlet Node Name",

        "  Coil:Heating:Water,",
        "    Water Heating Coil,                     !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    Autosize,                               !- U-Factor Times Area Value {W/K}",
        "    Autosize,                               !- Maximum Water Flow Rate {m3/s}",
        "    Node 31,                                !- Water Inlet Node Name",
        "    Node 32,                                !- Water Outlet Node Name",
        "    Thermal Zone one System Cooling Coil - Heating Coil Node, !- Air Inlet Node Name",
        "    Thermal Zone one System Heating Coil - Fan Node, !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
        "    Autosize,                               !- Rated Capacity {W}",
        "    82.2,                                   !- Rated Inlet Water Temperature {C}",
        "    16.6,                                   !- Rated Inlet Air Temperature {C}",
        "    71.1,                                   !- Rated Outlet Water Temperature {C}",
        "    32.2,                                   !- Rated Outlet Air Temperature {C}",
        "    0.5;                                    !- Rated Ratio for Air and Water Convection",

        "  Coil:Cooling:Water,",
        "    Water Cooling Coil,                     !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    Autosize,                               !- Design Water Flow Rate {m3/s}",
        "    Autosize,                               !- Design Air Flow Rate {m3/s}",
        "    Autosize,                               !- Design Inlet Water Temperature {C}",
        "    Autosize,                               !- Design Inlet Air Temperature {C}",
        "    Autosize,                               !- Design Outlet Air Temperature {C}",
        "    Autosize,                               !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    Autosize,                               !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    Node 33,                                !- Water Inlet Node Name",
        "    Node 34,                                !- Water Outlet Node Name",
        "    Node 29,                                !- Air Inlet Node Name",
        "    Thermal Zone one System Cooling Coil - Heating Coil Node, !- Air Outlet Node Name",
        "    SimpleAnalysis,                         !- Type of Analysis",
        "    CrossFlow;                              !- Heat Exchanger Configuration",

        "  Coil:Heating:Fuel,",
        "    Supplemental Heating Coil,              !- Name",
        "    ,                                       !- Availability Schedule Name",
        "    NaturalGas,                             !- Fuel Type",
        "    0.8,                                    !- Gas Burner Efficiency",
        "    32000,                                  !- Nominal Capacity{ W }",
        "    Supplemental Heating Coil Inlet Node,   !- Air Inlet Node Name",
        "    Node 30;                                !- Air Outlet Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    // call the UnitarySystem factory
    bool ErrorsFound = false;
    bool zoneEquipment = true;
    std::string compName = "UNITARY SYSTEM MODEL";
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors
    // check each input fields of unitary system
    EXPECT_EQ("UNITARY SYSTEM MODEL", thisSys->Name);                              // checks object name
    EXPECT_EQ(UnitarySys::ControlType::Load, thisSys->m_ControlType);              // checks control type
    EXPECT_EQ(UnitarySys::DehumCtrlType::None, thisSys->m_DehumidControlType_Num); // checks Dehumidification Control type type
    EXPECT_EQ(UtilityRoutines::FindItemInList("EAST ZONE", DataHeatBalance::Zone), thisSys->ControlZoneNum); // checks zone ID
    EXPECT_EQ(DataGlobals::ScheduleAlwaysOn, thisSys->m_SysAvailSchedPtr);                                   // checks availability schedule name
    EXPECT_EQ("NODE 29", DataLoopNode::NodeID(thisSys->AirInNode));                                          // checks air inlet node name
    EXPECT_EQ("NODE 30", DataLoopNode::NodeID(thisSys->AirOutNode));                                         // checks air outlet node name
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, thisSys->m_FanType_Num);                                 // checks fan object type "FAN:ONOFF"
    EXPECT_EQ("SUPPLY FAN", thisSys->m_FanName);                                                             // checks fan object name
    EXPECT_EQ(UnitarySys::FanPlace::DrawThru, thisSys->m_FanPlace);                                          // checks fan placement, "DrawThrough"
    EXPECT_EQ(0, thisSys->m_FanOpModeSchedPtr);                                    // checks Supply Air Fan Operating Mode Schedule Name
    EXPECT_EQ("COIL:HEATING:WATER", thisSys->m_HeatingCoilTypeName);               // checks heating coil object type
    EXPECT_EQ("WATER HEATING COIL", thisSys->m_HeatingCoilName);                   // checks heating coil object type
    EXPECT_EQ(1, thisSys->m_HeatingSizingRatio);                                   // checks dx heating coil sizing ratio
    EXPECT_EQ(DataHVACGlobals::Coil_CoolingWater, thisSys->m_CoolingCoilType_Num); // checks cooling coil object type
    EXPECT_EQ("WATER COOLING COIL", thisSys->m_CoolingCoilName);                   // checks cooling coil name
    EXPECT_FALSE(thisSys->m_ISHundredPercentDOASDXCoil);                           // checks DX Coil is for DOAS use
    EXPECT_EQ(7.0, thisSys->DesignMinOutletTemp);                                  // checks minimum supply air temperature
    EXPECT_TRUE(thisSys->m_RunOnSensibleLoad);                                     // checks for "SENSIBLEONLYLOADCONTROL"
    EXPECT_EQ("COIL:HEATING:FUEL", thisSys->m_SuppHeatCoilTypeName);               // checks supplemental heating coil object type
    EXPECT_EQ("SUPPLEMENTAL HEATING COIL", thisSys->m_SuppHeatCoilName);           // checks supplemental heating coil name
    EXPECT_EQ(4, thisSys->m_CoolingSAFMethod);    // checks cooling supply air flow rate sizing method, FractionOfAutosizedCoolingAirflow
    EXPECT_EQ(0.5, thisSys->m_MaxCoolAirVolFlow); // checks Cooling Fraction of Autosized Cooling Supply Air Flow Rate value
    EXPECT_EQ(5, thisSys->m_HeatingSAFMethod);    // checks cooling supply air flow rate sizing method, FractionOfAutosizedHeatingAirflow
    EXPECT_EQ(0.8, thisSys->m_MaxHeatAirVolFlow); // checks Heating Fraction of Autosized Heating Supply Air Flow Rate value
    EXPECT_EQ(6, thisSys->m_NoCoolHeatSAFMethod); // checks cooling supply air flow rate sizing method, FlowPerCoolingCapacity
    EXPECT_EQ(0.0000462180155978106, thisSys->m_MaxNoCoolHeatAirVolFlow); // checks Heating Fraction of Autosized Heating Supply Air Flow Rate value
    EXPECT_EQ(30.0, thisSys->DesignMaxOutletTemp);                        // checks Maximum Supply Air Temperature value
    EXPECT_EQ(20.0, thisSys->m_MaxOATSuppHeat); // checks Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation value
    EXPECT_EQ("SYSTEM OUTDOOR AIR NODE", DataLoopNode::NodeID(thisSys->m_CondenserNodeNum)); // checks condenser air inlet node name
    EXPECT_EQ(3.0, thisSys->m_MaxONOFFCyclesperHour);                                        // checks Maximum Cycling Rate value
    EXPECT_EQ(120.0, thisSys->m_HPTimeConstant);                                             // checks Heat Pump Time Constant value
    EXPECT_EQ(0.02, thisSys->m_OnCyclePowerFraction);                                        // checks Fraction of On-Cycle Power Use value
    EXPECT_EQ(90.0, thisSys->m_FanDelayTime);                                                // checks Heat Pump Fan Delay Time value
    EXPECT_EQ(40.0, thisSys->m_AncillaryOnPower);                                            // checks Ancillary On-Cycle Electric Power value
    EXPECT_EQ(10.0, thisSys->m_AncillaryOffPower);                                           // checks Ancillary Off-Cycle Electric Power value
    EXPECT_EQ(0.005, thisSys->m_DesignHRWaterVolumeFlow);                                    // checks Design Heat Recovery Water Flow Rate value
    EXPECT_EQ(75.0, thisSys->m_MaxHROutletWaterTemp);                                        // checks Maximum Temperature for Heat Recovery value
    EXPECT_EQ("WATER INLET NODE NAME", DataLoopNode::NodeID(thisSys->m_HeatRecoveryInletNodeNum));   // checks Heat Recovery Water Inlet Node Name ID
    EXPECT_EQ("WATER OUTLET NODE NAME", DataLoopNode::NodeID(thisSys->m_HeatRecoveryOutletNodeNum)); // checks Heat Recovery Water Outlet Node Name ID
    EXPECT_EQ("UNITARYSYSTEMPERFORMANCE:MULTISPEED",
              thisSys->m_DesignSpecMultispeedHPType);                           // checks design_specification_multispeed_object_type value
    EXPECT_EQ("MULTISPEED PERFORMANCE", thisSys->m_DesignSpecMultispeedHPName); // checks design_specification_multispeed_object_name value
}
TEST_F(EnergyPlusFixture, UnitarySystemModel_GetInputwithTradeOff)
{
    // Test #7601
    bool ErrorsFound(false);
    bool FirstHVACIteration(false);

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",
        "  ",
        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
        "  ",
        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,          !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",
        "  ",
        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model, !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailTest,   !- Availability Schedule Name",
        "  Zone Exhaust Node,         !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,   !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "  Furnace Heating Coil 1, !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
        "  Furnace ACDXCoil 1,     !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80;                     !- Maximum Supply Air Temperature{ C }",
        "  ",
        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  1.6,                    !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Cooling:DX:SingleSpeed,",
        "  Furnace ACDXCoil 1,      !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  32000,                   !- Gross Rated Total Cooling Capacity {W}",
        "  0.75,                    !- Gross Rated Sensible Heat Ratio",
        "  3.0,                     !- Gross Rated Cooling COP {W/W}",
        "  1.6,                     !- Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Part Load Fraction Correlation Curve Name",
        "   ,                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  1000,                    !- Nominal Time for Condensate Removal to Begin {s}",
        "  0.4,                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  4,                       !- Maximum Cycling Rate {cycles/hr}",
        "  45;                      !- Latent Capacity Time Constant {s}",
        "  ",
        "Coil:Heating:Fuel,",
        "  Furnace Heating Coil 1, !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8,                    !- Gas Burner Efficiency",
        "  32000,                  !- Nominal Capacity{ W }",
        "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8, !- Gas Burner Efficiency",
        "  32000, !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;    !- Air Outlet Node Name",
        "  ",
        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",
        "  ",
        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Curve:Quadratic,",
        "  WindACCoolCapFFF,       !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  WindACEIRFFF,           !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  WindACPLFFPLR,          !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",
        "  ",
        "Curve:Biquadratic,",
        "  WindACCoolCapFT,        !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  WindACEIRFT,            !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(state); // read zone equipment configuration and list objects

    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    UnitarySys mySys;
    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataGlobals::DoCoilDirectSolutions = true;
    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    // Issue 7777
    std::string const error_string = delimited_string({
        "   ** Warning ** getUnitarySystemInputDataAirLoopHVAC:UnitarySystem=\"UNITARY SYSTEM MODEL\", invalid Availability Schedule Name = "
        "FANANDCOILAVAILTEST",
        "   **   ~~~   ** Set the default as Always On. Simulation continues.",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, UnitarySystemModel_AllFlowFieldsBlankInputTest)
{
    std::string const idf_objects = delimited_string({
        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",

        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  Unitary System Model,          !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1,                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction",
        "  ;                        !- Zone Equipment 1 Sequential Heating Fraction",

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,   !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,      !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan,             !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Fuel,      !- Heating Coil Object Type",
        "  Furnace Heating Coil,   !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
        "  Furnace ACDXCoil,       !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,      !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil, !- Supplemental Heating Coil Name",
        "  ,                       !- Supply Air Flow Rate Method During Cooling Operation",
        "  ,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Supply air Flow Rate Method During Heating Operation",
        "  ,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                       !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  ,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80;                     !- Maximum Supply Air Temperature{ C }",

        "Fan:OnOff,",
        "  Supply Fan,             !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  autosize,               !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Cooling:DX:SingleSpeed,",
        "  Furnace ACDXCoil,        !- Name",
        "  FanAndCoilAvailSched,    !- Availability Schedule Name",
        "  autosize,                !- Gross Rated Total Cooling Capacity {W}",
        "  0.75,                    !- Gross Rated Sensible Heat Ratio",
        "  3.0,                     !- Gross Rated Cooling COP {W/W}",
        "  autosize,                !- Rated Air Flow Rate {m3/s}",
        "  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,        !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,            !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Part Load Fraction Correlation Curve Name",
        "   ,                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  1000,                    !- Nominal Time for Condensate Removal to Begin {s}",
        "  0.4,                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  4,                       !- Maximum Cycling Rate {cycles/hr}",
        "  45;                      !- Latent Capacity Time Constant {s}",

        "Coil:Heating:Fuel,",
        "  Furnace Heating Coil,   !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8,                    !- Gas Burner Efficiency",
        "  autosize,               !- Nominal Capacity{ W }",
        "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil, !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  NaturalGas,             !- Fuel Type",
        "  0.8,                    !- Gas Burner Efficiency",
        "  autosize,               !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;      !- Air Outlet Node Name",

        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",

        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",

        "Curve:Quadratic,",
        "  WindACCoolCapFFF,       !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACEIRFFF,           !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACPLFFPLR,          !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  WindACCoolCapFT,        !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",

        "Curve:Biquadratic,",
        "  WindACEIRFT,            !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    HeatBalanceManager::GetZoneData(state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1(state);
    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;

    UnitarySys mySys;
    std::string compName = "UNITARY SYSTEM MODEL";
    int compTypeOfNum = DataHVACGlobals::UnitarySys_AnyCoilType;
    bool zoneEquipment(true);
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];
    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems);
    EXPECT_EQ(thisSys->UnitType, DataHVACGlobals::cFurnaceTypes(compTypeOfNum));

    EXPECT_TRUE(thisSys->m_FanExists);
    EXPECT_TRUE(thisSys->m_CoolCoilExists);
    EXPECT_TRUE(thisSys->m_HeatCoilExists);

    auto &thisClgCoil = DXCoils::DXCoil(1);
    auto &thisHtgCoil = HeatingCoils::HeatingCoil(1);

    EXPECT_EQ(thisClgCoil.RatedTotCap(1), DataSizing::AutoSize);
    EXPECT_EQ(thisHtgCoil.NominalCapacity, DataSizing::AutoSize);

    ASSERT_EQ(thisSys->m_CoolingSAFMethod, UnitarySystems::None);
    ASSERT_EQ(thisSys->m_HeatingSAFMethod, UnitarySystems::None);
    ASSERT_EQ(thisSys->m_NoCoolHeatSAFMethod, UnitarySystems::None);

    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_DesignCoolingCapacity, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_DesignHeatingCapacity, DataSizing::AutoSize);

    DataSizing::CurZoneEqNum = 1;
    DataSizing::ZoneSizingRunDone = true;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).DesignSizeFromParent = false;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod.allocate(25);
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;

    DataSizing::FinalZoneSizing.allocate(1);
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolVolFlow = 1.005;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInTemp = 30.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInHumRat = 0.001;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesTemp = 15.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesHumRat = 0.0006;

    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow = 1.005;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatCoilInTemp = 20.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).ZoneTempAtHeatPeak = 20.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).HeatDesTemp = 30.0;
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).HeatDesHumRat = 0.007;

    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0);
    DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatMassFlow =
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow * DataEnvironment::StdRhoAir;

    int AirLoopNum(0);
    bool FirstHVACIteration(true);

    thisSys->sizeSystem(state, FirstHVACIteration, AirLoopNum);
    // check autosized flows and capacity
    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, 1.005);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, 1.005);
    EXPECT_NEAR(thisSys->m_DesignCoolingCapacity, 20155.3, 0.1);
    EXPECT_NEAR(thisSys->m_DesignHeatingCapacity, 12319.4, 0.1);
}

TEST_F(EnergyPlusFixture, Test_UnitarySystemModel_SubcoolReheatCoil)
{

    std::string const idf_objects = delimited_string({

        "  Timestep,4;",

        "  Building,",
        "    Simple One Zone (Wireframe DXF),  !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    0.04,                    !- Loads Convergence Tolerance Value",
        "    0.004,                   !- Temperature Convergence Tolerance Value {deltaC}",
        "    MinimalShadowing,        !- Solar Distribution",
        "    30,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  HeatBalanceAlgorithm,ConductionTransferFunction;",

        "  SurfaceConvectionAlgorithm:Inside,TARP;",

        "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "  SimulationControl,",
        "    No,                      !- Do Zone Sizing Calculation",
        "    No,                      !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    No;                     !- Run Simulation for Weather File Run Periods",

        "  RunPeriod,",
        "    Run Period 1,            !- Name",
        "    1,                       !- Begin Month",
        "    1,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    12,                      !- End Month",
        "    31,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    Yes;                     !- Use Weather File Snow Indicators",

        "  Site:Location,",
        "    CHICAGO_IL_USA TMY2-94846,  !- Name",
        "    41.78,                   !- Latitude {deg}",
        "    -87.75,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190.00;                  !- Elevation {m}",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
        "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    4.9,                     !- Wind Speed {m/s}",
        "    270,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    0.0;                     !- Sky Clearness",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Cooling 1% Design Conditions DB/MCWB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    31.5,                    !- Maximum Dry-Bulb Temperature {C}",
        "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    23.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    5.3,                     !- Wind Speed {m/s}",
        "    230,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.0;                     !- Sky Clearness",

        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    R31LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    5.456,                   !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    C5 - 4 IN HW CONCRETE,   !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",

        "  Construction,",
        "    FLOOR,                   !- Name",
        "    C5 - 4 IN HW CONCRETE;   !- Outside Layer",

        "  Construction,",
        "    ROOF31,                  !- Name",
        "    R31LAYER;                !- Outside Layer",

        "  Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Adiabatic,               !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0.000000,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.000000,15.24000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000,15.24000,4.572,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,4.572,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0.000000,4.572,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572;  !- X,Y,Z ==> Vertex 4 {m}",

        "  Exterior:Lights,",
        "    ExtLights,               !- Name",
        "    AlwaysOn,                !- Schedule Name",
        "    5250,                    !- Design Level {W}",
        "    AstronomicalClock,       !- Control Option",
        "    Grounds Lights;          !- End-Use Subcategory",

        "  ScheduleTypeLimits,",
        "    On/Off,                  !- Name",
        "    0,                       !- Lower Limit Value",
        "    1,                       !- Upper Limit Value",
        "    DISCRETE;                !- Numeric Type",

        "  ZoneInfiltration:DesignFlowRate,",
        "    SPACE1-1 Infil 1,        !- Name",
        "    ZONE ONE,                !- Zone or ZoneList Name",
        "    INFIL-SCH,               !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.0167,                  !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow per Exterior Surface Area {m3/s-m2}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    0,                       !- Constant Term Coefficient",
        "    0,                       !- Temperature Term Coefficient",
        "    0.2237,                  !- Velocity Term Coefficient",
        "    0;                       !- Velocity Squared Term Coefficient",

        "  Schedule:Compact,",
        "    INFIL-SCH,               !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2, !- Field 2",
        "    Until: 7:00,1.0,         !- Field 3",
        "    Until: 21:00,0.0,        !- Field 5",
        "    Until: 24:00,1.0,        !- Field 7",
        "    For: Weekends Holiday,   !- Field 9",
        "    Until: 24:00,1.0,        !- Field 10",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00,1.0,        !- Field 13",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00,1.0;        !- Field 16",

        "  People,",
        "    SPACE1-1 People 1,       !- Name",
        "    ZONE ONE,                !- Zone or ZoneList Name",
        "    OCCUPY-1,                !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    11,                      !- Number of People",
        "    ,                        !- People per Zone Floor Area {person/m2}",
        "    ,                        !- Zone Floor Area per Person {m2/person}",
        "    0.3,                     !- Fraction Radiant",
        "    ,                        !- Sensible Heat Fraction",
        "    ActSchd;                 !- Activity Level Schedule Name",

        "  Schedule:Compact,",
        "    OCCUPY-1,                !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays SummerDesignDay CustomDay1 CustomDay2, !- Field 2",
        "    Until: 8:00,0.0,         !- Field 3",
        "    Until: 11:00,1.00,       !- Field 5",
        "    Until: 12:00,0.80,       !- Field 7",
        "    Until: 13:00,0.40,       !- Field 9",
        "    Until: 14:00,0.80,       !- Field 11",
        "    Until: 18:00,1.00,       !- Field 13",
        "    Until: 19:00,0.50,       !- Field 15",
        "    Until: 21:00,0.10,       !- Field 17",
        "    Until: 24:00,0.0,        !- Field 19",
        "    For: Weekends WinterDesignDay Holiday, !- Field 21",
        "    Until: 24:00,0.0;        !- Field 22",

        "  Schedule:Compact,",
        "    ActSchd,                 !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,117.239997864; !- Field 3",

        "  Lights,",
        "    SPACE1-1 Lights 1,       !- Name",
        "    ZONE ONE,                !- Zone or ZoneList Name",
        "    LIGHTS-1,                !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    1584,                    !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Return Air Fraction",
        "    0.59,                    !- Fraction Radiant",
        "    0.2,                     !- Fraction Visible",
        "    0,                       !- Fraction Replaceable",
        "    GeneralLights;           !- End-Use Subcategory",

        "  Schedule:Compact,",
        "    LIGHTS-1,                !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays SummerDesignDay CustomDay1 CustomDay2, !- Field 2",
        "    Until: 8:00,0.05,        !- Field 3",
        "    Until: 9:00,0.9,         !- Field 5",
        "    Until: 10:00,0.95,       !- Field 7",
        "    Until: 11:00,1.00,       !- Field 9",
        "    Until: 12:00,0.95,       !- Field 11",
        "    Until: 13:00,0.8,        !- Field 13",
        "    Until: 14:00,0.9,        !- Field 15",
        "    Until: 18:00,1.00,       !- Field 17",
        "    Until: 19:00,0.60,       !- Field 19",
        "    Until: 21:00,0.40,       !- Field 21",
        "    Until: 24:00,0.05,       !- Field 23",
        "    For: Weekends WinterDesignDay Holiday, !- Field 25",
        "    Until: 24:00,0.05;       !- Field 26",

        "  ElectricEquipment,",
        "    SPACE1-1 ElecEq 1,       !- Name",
        "    ZONE ONE,                !- Zone or ZoneList Name",
        "    EQUIP-1,                 !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    1056,                    !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Fraction Latent",
        "    0.3,                     !- Fraction Radiant",
        "    0;                       !- Fraction Lost",

        "  Schedule:Compact,",
        "    EQUIP-1,                 !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays SummerDesignDay CustomDay1 CustomDay2, !- Field 2",
        "    Until: 8:00,0.02,        !- Field 3",
        "    Until: 9:00,0.4,         !- Field 5",
        "    Until: 14:00,0.9,        !- Field 7",
        "    Until: 15:00,0.8,        !- Field 9",
        "    Until: 16:00,0.7,        !- Field 11",
        "    Until: 18:00,0.5,        !- Field 13",
        "    Until: 21:00,0.3,        !- Field 15",
        "    Until: 24:00,0.02,       !- Field 17",
        "    For: Weekends WinterDesignDay Holiday, !- Field 19",
        "    Until: 24:00,0.02;       !- Field 20",

        "  Schedule:Constant,AlwaysOn,On/Off,1.0;",

        "  DesignSpecification:OutdoorAir,",
        "    SZ DSOA SPACE1-1,        !- Name",
        "    flow/person,             !- Outdoor Air Method",
        "    0.00944,                 !- Outdoor Air Flow per Person {m3/s-person}",
        "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "    0.0;                     !- Outdoor Air Flow per Zone {m3/s}",

        "  Sizing:Zone,",
        "    ZONE ONE,                !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    12.8,                    !- Zone Cooling Design Supply Air Temperature {C}",
        "    11.11,                   !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    50,                      !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.008,                   !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.008,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA SPACE1-1,        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Zone Heating Sizing Factor",
        "    ,                        !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    0,                       !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    0,                       !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    0,                       !- Heating Maximum Air Flow Fraction",
        "    SZ DSZAD SPACE1-1;       !- Design Specification Zone Air Distribution Object Name",

        "  DesignSpecification:ZoneAirDistribution,",
        "    SZ DSZAD SPACE1-1,       !- Name",
        "    1,                       !- Zone Air Distribution Effectiveness in Cooling Mode {dimensionless}",
        "    1;                       !- Zone Air Distribution Effectiveness in Heating Mode {dimensionless}",

        "  ZoneHVAC:EquipmentConnections,",
        "    ZONE ONE,                !- Zone Name",
        "    SPACE1-1 Equipment,      !- Zone Conditioning Equipment List Name",
        "    SPACE1-1 Zone Equip Inlet,  !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE1-1 Zone Air Node,  !- Zone Air Node Name",
        "    SPACE1-1 Return Outlet;  !- Zone Return Air Node or NodeList Name",

        "  ZoneHVAC:EquipmentList,",
        "    SPACE1-1 Equipment,      !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE1-1 DirectAir ADU,   !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 1 Sequential Cooling Fraction Schedule Name",
        "    ;                        !- Zone Equipment 1 Sequential Heating Fraction Schedule Name",

        "  AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        "    SPACE1-1 Air Terminal,          !- Name",
        "    ,    !- Availability Schedule Name",
        "    SPACE1-1 Inlet Node ATInlet,  !- Air Inlet Node Name",
        "    SPACE1-1 Zone Equip Inlet,       !- Air Outlet Node Name",
        "    0.46609,                 !- Maximum Air Flow Rate {m3/s}",
        "    ,                        !- Design Specification Outdoor Air Object Name",
        "    ;                        !- Per Person Ventilation Rate Mode",

        "  ZoneHVAC:AirDistributionUnit,",
        "    SPACE1-1 DirectAir ADU,      !- Name",
        "    SPACE1-1 Zone Equip Inlet,       !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        "    SPACE1-1 Air Terminal,          !- Air Terminal Name",
        "    ,                        !- Nominal Upstream Leakage Fraction",
        "    ,                        !- Constant Downstream Leakage Fraction",
        "    ;                        !- Design Specification Air Terminal Sizing Object Name",

        "  AirLoopHVAC,",
        "    Sys 1 Furnace DX Cool,   !- Name",
        "    ,                        !- Controller List Name",
        "    Sys 1 Furnace DX Cool Availability Managers,  !- Availability Manager List Name",
        "    0.46609,                !- Design Supply Air Flow Rate {m3/s}",
        "    Sys 1 Furnace DX Cool Branches,  !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    Sys 1 Furnace DX Cool Air Loop Inlet,  !- Supply Side Inlet Node Name",
        "    Sys 1 Furnace DX Cool Return Air Outlet,  !- Demand Side Outlet Node Name",
        "    Sys 1 Furnace DX Cool Supply Path Inlet,  !- Demand Side Inlet Node Names",
        "    Sys 1 Furnace DX Cool Heating Coil Outlet;  !- Supply Side Outlet Node Names",

        "  BranchList,",
        "    Sys 1 Furnace DX Cool Branches,  !- Name",
        "    Sys 1 Furnace DX Cool Main Branch;  !- Branch 1 Name",

        "  Branch,",
        "    Sys 1 Furnace DX Cool Main Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
        "    Sys 1 Furnace DX Cool OA System,  !- Component 1 Name",
        "    Sys 1 Furnace DX Cool Air Loop Inlet,  !- Component 1 Inlet Node Name",
        "    Sys 1 Furnace DX Cool Mixed Air Outlet,  !- Component 1 Outlet Node Name",
        "    AirLoopHVAC:UnitarySystem,  !- Component 2 Object Type",
        "    Sys 1 Furnace DX Cool Unitary System,  !- Component 2 Name",
        "    Sys 1 Furnace DX Cool Mixed Air Outlet,  !- Component 2 Inlet Node Name",
        "    Sys 1 Furnace DX Cool Heating Coil Outlet;  !- Component 2 Outlet Node Name",

        "  AirLoopHVAC:SupplyPath,",
        "    Sys 1 Furnace DX Cool Supply Path,  !- Name",
        "    Sys 1 Furnace DX Cool Supply Path Inlet,  !- Supply Air Path Inlet Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
        "    Sys 1 Furnace DX Cool Zone Splitter;  !- Component 1 Name",

        "  AirLoopHVAC:ZoneSplitter,",
        "    Sys 1 Furnace DX Cool Zone Splitter,  !- Name",
        "    Sys 1 Furnace DX Cool Supply Path Inlet,  !- Inlet Node Name",
        "    SPACE1-1 Inlet Node ATInlet;  !- Outlet 1 Node Name",

        "  AirLoopHVAC:ReturnPath,",
        "    Sys 1 Furnace DX Cool Return Path,  !- Name",
        "    Sys 1 Furnace DX Cool Return Air Outlet,  !- Return Air Path Outlet Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
        "    Sys 1 Furnace DX Cool Zone Mixer;  !- Component 1 Name",

        "  AirLoopHVAC:ZoneMixer,",
        "    Sys 1 Furnace DX Cool Zone Mixer,  !- Name",
        "    Sys 1 Furnace DX Cool Return Air Outlet,  !- Outlet Node Name",
        "    SPACE1-1 Return Outlet;  !- Inlet 1 Node Name",

        "  AvailabilityManagerAssignmentList,",
        "    Sys 1 Furnace DX Cool Availability Managers,  !- Name",
        "    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
        "    Sys 1 Furnace DX Cool Availability;  !- Availability Manager 1 Name",

        "  AvailabilityManager:Scheduled,",
        "    Sys 1 Furnace DX Cool Availability,  !- Name",
        "    HVACTemplate-Always 1;   !- Schedule Name",

        "  Schedule:Compact,",
        "    HVACTemplate-Always 1,   !- Name",
        "    HVACTemplate Any Number, !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1;          !- Field 3",

        "  AirLoopHVAC:UnitarySystem,",
        "    Sys 1 Furnace DX Cool Unitary System,  !- Name",
        "    Load,                    !- Control Type",
        "    ZONE ONE,                !- Controlling Zone or Thermostat Location",
        "    None,                    !- Dehumidification Control Type",
        "    ,                        !- Availability Schedule Name",
        "    Sys 1 Furnace DX Cool Mixed Air Outlet,  !- Air Inlet Node Name",
        "    Sys 1 Furnace DX Cool Heating Coil Outlet,  !- Air Outlet Node Name",
        "    Fan:OnOff,               !- Supply Fan Object Type",
        "    Sys 1 Furnace DX Cool Supply Fan,  !- Supply Fan Name",
        "    BlowThrough,             !- Fan Placement",
        "    FanAvailSched,           !- Supply Air Fan Operating Mode Schedule Name",
        "    Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "    Sys 1 Furnace DX Cool Heating Coil,  !- Heating Coil Name",
        "    1.0,                     !- DX Heating Coil Sizing Ratio",
        "    Coil:Cooling:DX,         !- Cooling Coil Object Type",
        "    Sys 1 Furnace DX Cool Cooling Coil,  !- Cooling Coil Name",
        "    ,                        !- Use DOAS DX Cooling Coil",
        "    ,                        !- Minimum Supply Air Temperature {C}",
        "    ,                        !- Latent Load Control",
        "    ,                        !- Supplemental Heating Coil Object Type",
        "    ,                        !- Supplemental Heating Coil Name",
        "    SupplyAirFlowRate,       !- Cooling Supply Air Flow Rate Method",
        "    0.46609,                !- Cooling Supply Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                        !- Cooling Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "    SupplyAirFlowRate,       !- Heating Supply Air Flow Rate Method",
        "    0.46609,                !- Heating Supply Air Flow Rate {m3/s}",
        "    ,                        !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                        !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "    ,                        !- Heating Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "    SupplyAirFlowRate,       !- No Load Supply Air Flow Rate Method",
        "    0.46609,                !- No Load Supply Air Flow Rate {m3/s}",
        "    ,                        !- No Load Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                        !- No Load Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                        !- No Load Fraction of Autosized Heating Supply Air Flow Rate",
        "    ,                        !- No Load Supply Air Flow Rate Per Unit of Capacity During Cooling Operation {m3/s-W}",
        "    ,                        !- No Load Supply Air Flow Rate Per Unit of Capacity During Heating Operation {m3/s-W}",
        "    50.00000,                !- Maximum Supply Air Temperature {C}",
        "    21,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "    ,                        !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "    ,                        !- Maximum Cycling Rate {cycles/hr}",
        "    ,                        !- Heat Pump Time Constant {s}",
        "    ,                        !- Fraction of On-Cycle Power Use",
        "    ,                        !- Heat Pump Fan Delay Time {s}",
        "    ,                        !- Ancillary On-Cycle Electric Power {W}",
        "    ,                        !- Ancillary Off-Cycle Electric Power {W}",
        "    ,                        !- Design Heat Recovery Water Flow Rate {m3/s}",
        "    ,                        !- Maximum Temperature for Heat Recovery {C}",
        "    ,                        !- Heat Recovery Water Inlet Node Name",
        "    ,                        !- Heat Recovery Water Outlet Node Name",
        "    ,                        !- Design Specification Multispeed Object Type",
        "    ;                        !- Design Specification Multispeed Object Name",

        "  Coil:Cooling:DX,",
        "    Sys 1 Furnace DX Cool Cooling Coil,  !- Name",
        "    Sys 1 Furnace DX Cool Supply Fan Outlet,  !- Evaporator Inlet Node Name",
        "    Sys 1 Furnace DX Cool Cooling Coil Outlet,  !- Evaporator Outlet Node Name",
        "    ,                        !- Availability Schedule Name",
        "    ,                        !- Condenser Zone Name",
        "    Sys 1 Furnace DX Cool Cooling Coil Condenser Inlet,  !- Condenser Inlet Node Name",
        "    Sys 1 Furnace DX Cool Cooling Coil Condenser Outlet Node,  !- Condenser Outlet Node Name",
        "    Sys 1 Furnace DX Cool Cooling Coil Performance,  !- Performance Object Name",
        "    ,                        !- Condensate Collection Water Storage Tank Name",
        "    ;                        !- Evaporative Condenser Supply Water Storage Tank Name",

        "  Coil:Cooling:DX:CurveFit:Performance,",
        "    Sys 1 Furnace DX Cool Cooling Coil Performance,  !- Name",
        "    0,                       !- Crankcase Heater Capacity {W}",
        "    ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    10,                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "    ,                        !- Unit Internal Static Air Pressure {Pa}",
        "    ,                        !- Capacity Control Method",
        "    ,                        !- Evaporative Condenser Basin Heater Capacity {W/K}",
        "    ,                        !- Evaporative Condenser Basin Heater Setpoint Temperature {C}",
        "    ,                        !- Evaporative Condenser Basin Heater Operating Schedule Name",
        "    Electricity,             !- Compressor Fuel Type",
        "    Sys 1 Furnace DX Cool Cooling Coil Operating Mode,  !- Base Operating Mode",
        "    Sys 1 Furnace DX Cool Cooling Coil Alternative Mode 1,  !- Alternative Operating Mode 1",
        "    Sys 1 Furnace DX Cool Cooling Coil Alternative Mode 2;  !- Alternative Operating Mode 1",

        "  Coil:Cooling:DX:CurveFit:OperatingMode,",
        "    Sys 1 Furnace DX Cool Cooling Coil Operating Mode,  !- Name",
        "    9930.90361,                !- Rated Gross Total Cooling Capacity {W}",
        "    0.46609,                !- Rated Evaporator Air Flow Rate {m3/s}",
        "    ,                        !- Rated Condenser Air Flow Rate {m3/s}",
        "    0,                       !- Maximum Cycling Rate {cycles/hr}",
        "    0,                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "    0,                       !- Latent Capacity Time Constant {s}",
        "    0,                       !- Nominal Time for Condensate Removal to Begin {s}",
        "    ,                        !- Apply Latent Degradation to Speeds Greater than 1",
        "    AirCooled,               !- Condenser Type",
        "    0,                       !- Nominal Evaporative Condenser Pump Power {W}",
        "    1,                       !- Nominal Speed Number",
        "    Sys 1 Furnace DX Cool Cooling Coil Speed 1 Performance;  !- Speed 1 Name",

        "  Coil:Cooling:DX:CurveFit:Speed,",
        "    Sys 1 Furnace DX Cool Cooling Coil Speed 1 Performance,  !- Name",
        "    1.0,                     !- Gross Total Cooling Capacity Fraction",
        "    1.0,                     !- Evaporator Air Flow Rate Fraction",
        "    1.0,                     !- Condenser Air Flow Rate Fraction",
        "    0.77,                !- Gross Sensible Heat Ratio",
        "    4.17,                       !- Gross Cooling COP {W/W}",
        "    1.0,                     !- Active Fraction of Coil Face Area",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    1.0,                     !- Evaporative Condenser Pump Power Fraction",
        "    0,                       !- Evaporative Condenser Effectiveness {dimensionless}",
        "    NormalTempCoolingCAPFTemp,  !- Total Cooling Capacity Modifier Function of Temperature Curve Name",
        "    NormalFlowCoolingCAPFFF,  !- Total Cooling Capacity Modifier Function of Air Flow Fraction Curve Name",
        "    NormalTempCoolingEIRFTemp,  !- Energy Input Ratio Modifier Function of Temperature Curve Name",
        "    NormalFlowCoolingEIRFFF,  !- Energy Input Ratio Modifier Function of Air Flow Fraction Curve Name",
        "    Sys 1 Furnace DX Cool Cool Coil PLF,  !- Part Load Fraction Correlation Curve Name",
        "    ,                        !- Rated Waste Heat Fraction of Power Input {dimensionless}",
        "    ,                        !- Waste Heat Modifier Function of Temperature Curve Name",
        "    NormalSHRTempCoolingFFF,  !- Sensible Heat Ratio Modifier Function of Temperature Curve Name",
        "    NormalSHRFlowCoolingFFF;  !- Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name",

        "  Curve:Biquadratic,  ",
        "  NormalTempCoolingCAPFTemp,  !- Name",
        "  1.2047763641,  !- Coefficient1 Constant",
        "  -0.0319503564,  !- Coefficient2 x",
        "  0.0020745905,  !- Coefficient3 x**2",
        "  -0.000310579,  !- Coefficient4 y",
        "  -0.0000344954,  !- Coefficient5 y**2",
        "  -0.0004608937,  !- Coefficient6 x*y",
        "  16.67,  !- Minimum Value of x",
        "  22.22,  !- Maximum Value of x",
        "  18.33,  !- Minimum Value of y",
        "  51.67,  !- Maximum Value of y",
        "  0.7491,  !- Minimum Curve Output",
        "  1.3089,  !- Maximum Curve Output",
        "  Temperature,  !- Input Unit Type for X",
        "  Temperature,  !- Input Unit Type for Y",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Biquadratic,  ",
        "  NormalTempCoolingEIRFTemp,  !- Name",
        "  -0.0439174307,  !- Coefficient1 Constant",
        "  0.0620812106,  !- Coefficient2 x",
        "  -0.0014354652,  !- Coefficient3 x**2",
        "  0.0048739692,  !- Coefficient4 y",
        "  0.0007105116,  !- Coefficient5 y**2",
        "  -0.000974921,  !- Coefficient6 x*y",
        "  16.67,  !- Minimum Value of x",
        "  22.22,  !- Maximum Value of x",
        "  18.33,  !- Minimum Value of y",
        "  51.67,  !- Maximum Value of y",
        "  0.5453,  !- Minimum Curve Output",
        "  1.9105,  !- Maximum Curve Output",
        "  Temperature,  !- Input Unit Type for X",
        "  Temperature,  !- Input Unit Type for Y",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Quadratic,  ",
        "  NormalFlowCoolingCAPFFF,  !- Name",
        "  0.7333333334,  !- Coefficient1 Constant",
        "  0.3733333333,  !- Coefficient2 x",
        "  -0.1066666667,  !- Coefficient3 x**2",
        "  0.75,  !- Minimum Value of x",
        "  1.25,  !- Maximum Value of x",
        "  0.9533,  !- Minimum Curve Output",
        "  1.0333,  !- Maximum Curve Output",
        "  Dimensionless,  !- Input Unit Type for X",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Quadratic,  ",
        "  NormalFlowCoolingEIRFFF,  !- Name",
        "  1.1970044563,  !- Coefficient1 Constant",
        "  -0.2513822149,  !- Coefficient2 x",
        "  0.0543777586,  !- Coefficient3 x**2",
        "  0.75,  !- Minimum Value of x",
        "  1.25,  !- Maximum Value of x",
        "  0.9677,  !- Minimum Curve Output",
        "  1.0391,  !- Maximum Curve Output",
        "  Dimensionless,  !- Input Unit Type for X",
        "  Dimensionless;  !- Output Unit Type",

        "Curve:Biquadratic,	",
        "NormalSHRTempCoolingFFF,	!- Name",
        //"3.9903739056,	!- Coefficient1 Constant",
        //"-0.2158168778,	!- Coefficient2 x",
        //"0.0034746418,	!- Coefficient3 x**2",
        //"0.0310505116,	!- Coefficient4 y",
        //"-0.0000360698,	!- Coefficient5 y**2",
        //"-0.0016903602,	!- Coefficient6 x*y",
        "1.0,	!- Coefficient1 Constant",
        "0.0,	!- Coefficient2 x",
        "0.0,	!- Coefficient3 x**2",
        "0.0,	!- Coefficient4 y",
        "0.0,	!- Coefficient5 y**2",
        "0.0,	!- Coefficient6 x*y",
        "16.67,	!- Minimum Value of x",
        "22.22,	!- Maximum Value of x",
        "23.89, !- Minimum Value of y",
        "51.67,	!- Maximum Value of y",
        "0.4786,	!- Minimum Curve Output",
        "1.4194,	!- Maximum Curve Output",
        "Temperature,	!- Input Unit Type for X",
        "Temperature,	!- Input Unit Type for Y",
        "Dimensionless;	!- Output Unit Type",

        "Curve:Quadratic,	",
        "NormalSHRFlowCoolingFFF,	!- Name",
        "0.5620186265,	!- Coefficient1 Constant",
        "0.4708409870,	!- Coefficient2 x",
        "-0.0285306092,	!- Coefficient3 x**2",
        "0.7500000000,	!- Minimum Value of x",
        "1.2500000000,	!- Maximum Value of x",
        "0.8991000000,	!- Minimum Curve Output",
        "1.11,	!- Maximum Curve Output",
        "Dimensionless,	!- Input Unit Type for X",
        "Dimensionless;	!- Output Unit Type",

        "  Coil:Cooling:DX:CurveFit:OperatingMode,",
        "    Sys 1 Furnace DX Cool Cooling Coil Alternative Mode 1,  !- Name",
        "    9930.90361,                !- Rated Gross Total Cooling Capacity {W}",
        "    0.46609,                !- Rated Evaporator Air Flow Rate {m3/s}",
        "    ,                        !- Rated Condenser Air Flow Rate {m3/s}",
        "    0,                       !- Maximum Cycling Rate {cycles/hr}",
        "    0,                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "    0,                       !- Latent Capacity Time Constant {s}",
        "    0,                       !- Nominal Time for Condensate Removal to Begin {s}",
        "    ,                        !- Apply Latent Degradation to Speeds Greater than 1",
        "    AirCooled,               !- Condenser Type",
        "    0,                       !- Nominal Evaporative Condenser Pump Power {W}",
        "    1,                       !- Nominal Speed Number",
        "    Sys 1 Furnace DX Cool Cooling Coil Alternative Mode 1 Performance;  !- Speed 1 Name",

        "  Coil:Cooling:DX:CurveFit:Speed,",
        "    Sys 1 Furnace DX Cool Cooling Coil Alternative Mode 1 Performance,  !- Name",
        "    1.0,                     !- Gross Total Cooling Capacity Fraction",
        "    1.0,                     !- Evaporator Air Flow Rate Fraction",
        "    1.0,                     !- Condenser Air Flow Rate Fraction",
        "    0.66,                !- Gross Sensible Heat Ratio",
        "    4.10,                       !- Gross Cooling COP {W/W}",
        "    1.0,                     !- Active Fraction of Coil Face Area",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    1.0,                     !- Evaporative Condenser Pump Power Fraction",
        "    0,                       !- Evaporative Condenser Effectiveness {dimensionless}",
        "    SubcoolTempCoolingCAPFTemp,  !- Total Cooling Capacity Modifier Function of Temperature Curve Name",
        "    SubcoolFlowCoolingCAPFFF,  !- Total Cooling Capacity Modifier Function of Air Flow Fraction Curve Name",
        "    SubcoolTempCoolingEIRFTemp,  !- Energy Input Ratio Modifier Function of Temperature Curve Name",
        "    SubcoolFlowCoolingEIRFFF,  !- Energy Input Ratio Modifier Function of Air Flow Fraction Curve Name",
        "    Sys 1 Furnace DX Cool Cool Coil PLF,  !- Part Load Fraction Correlation Curve Name",
        "    ,                        !- Rated Waste Heat Fraction of Power Input {dimensionless}",
        "    ,                        !- Waste Heat Modifier Function of Temperature Curve Name",
        "    SubcoolSHRTempCoolingCAPFTemp,   !- Sensible Heat Ratio Modifier Function of Temperature Curve Name",
        "    SubcoolSHRFlowCoolingCAPFFF;     !- Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name",

        "  Curve:Biquadratic,  ",
        "  SubcoolTempCoolingCAPFTemp,  !- Name",
        "  1.0162460317,  !- Coefficient1 Constant",
        "  -0.0108091429,  !- Coefficient2 x",
        "  0.001296,  !- Coefficient3 x**2",
        "  -0.0040457143,  !- Coefficient4 y",
        "  -0.0000154286,  !- Coefficient5 y**2",
        "  -0.0002962286,  !- Coefficient6 x*y",
        "  16.67,  !- Minimum Value of x",
        "  22.22,  !- Maximum Value of x",
        "  23.89,  !- Minimum Value of y",
        "  51.67,  !- Maximum Value of y",
        "  0.6933,  !- Minimum Curve Output",
        "  1.1533,  !- Maximum Curve Output",
        "  Temperature,  !- Input Unit Type for X",
        "  Temperature,  !- Input Unit Type for Y",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Biquadratic,  ",
        "  SubcoolTempCoolingEIRFTemp,  !- Name",
        "  0.2634289781,  !- Coefficient1 Constant",
        "  0.0311157806,  !- Coefficient2 x",
        "  -0.0004774101,  !- Coefficient3 x**2",
        "  0.006660679,  !- Coefficient4 y",
        "  0.0006632907,  !- Coefficient5 y**2",
        "  -0.0010671286,  !- Coefficient6 x*y",
        "  16.67,  !- Minimum Value of x",
        "  22.22,  !- Maximum Value of x",
        "  23.89,  !- Minimum Value of y",
        "  51.67,  !- Maximum Value of y",
        "  0.6811,  !- Minimum Curve Output",
        "  1.8512,  !- Maximum Curve Output",
        "  Temperature,  !- Input Unit Type for X",
        "  Temperature,  !- Input Unit Type for Y",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Quadratic,  ",
        "  SubcoolFlowCoolingCAPFFF,  !- Name",
        "  0.6,  !- Coefficient1 Constant",
        "  0.6285714286,  !- Coefficient2 x",
        "  -0.2285714286,  !- Coefficient3 x**2",
        "  0.75,  !- Minimum Value of x",
        "  1.25,  !- Maximum Value of x",
        "  0.9429,  !- Minimum Curve Output",
        "  1.0286,  !- Maximum Curve Output",
        "  Dimensionless,  !- Input Unit Type for X",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Quadratic,  ",
        "  SubcoolFlowCoolingEIRFFF,  !- Name",
        "  1.2856060606,  !- Coefficient1 Constant",
        "  -0.4563131313,  !- Coefficient2 x",
        "  0.1707070707,  !- Coefficient3 x**2",
        "  0.75,  !- Minimum Value of x",
        "  1.25,  !- Maximum Value of x",
        "  0.9819,  !- Minimum Curve Output",
        "  1.0394,  !- Maximum Curve Output",
        "  Dimensionless,  !- Input Unit Type for X",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Biquadratic,  ",
        "  SubcoolSHRTempCoolingCAPFTemp,  !- Name",
        "  3.9903739056,  !- Coefficient1 Constant",
        "  -0.2158168778,  !- Coefficient2 x",
        "  0.0034746418,  !- Coefficient3 x**2",
        "  0.0310505116,  !- Coefficient4 y",
        "  -0.0000360698,  !- Coefficient5 y**2",
        "  -0.0016903602,  !- Coefficient6 x*y",
        "  16.67,  !- Minimum Value of x",
        "  22.22,  !- Maximum Value of x",
        "  23.89,  !- Minimum Value of y",
        "  51.67,  !- Maximum Value of y",
        "  0.4786,  !- Minimum Curve Output",
        "  1.4194,  !- Maximum Curve Output",
        "  Temperature,  !- Input Unit Type for X",
        "  Temperature,  !- Input Unit Type for Y",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Quadratic,  ",
        "  SubcoolSHRFlowCoolingCAPFFF,  !- Name",
        "  0.4306934378,  !- Coefficient1 Constant",
        "  0.606484437,  !- Coefficient2 x",
        "  -0.0371126532,  !- Coefficient3 x**2",
        "  0.75,  !- Minimum Value of x",
        "  1.25,  !- Maximum Value of x",
        "  0.8647,  !- Minimum Curve Output",
        "  1.1308,  !- Maximum Curve Output",
        "  Dimensionless,  !- Input Unit Type for X",
        "  Dimensionless;  !- Output Unit Type",

        "  Coil:Cooling:DX:CurveFit:OperatingMode,",
        "    Sys 1 Furnace DX Cool Cooling Coil Alternative Mode 2,  !- Name",
        "    9930.90361,                !- Rated Gross Total Cooling Capacity {W}",
        "    0.46609,                !- Rated Evaporator Air Flow Rate {m3/s}",
        "    ,                        !- Rated Condenser Air Flow Rate {m3/s}",
        "    0,                       !- Maximum Cycling Rate {cycles/hr}",
        "    0,                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "    0,                       !- Latent Capacity Time Constant {s}",
        "    0,                       !- Nominal Time for Condensate Removal to Begin {s}",
        "    ,                        !- Apply Latent Degradation to Speeds Greater than 1",
        "    AirCooled,               !- Condenser Type",
        "    0,                       !- Nominal Evaporative Condenser Pump Power {W}",
        "    1,                       !- Nominal Speed Number",
        "    Sys 1 Furnace DX Cool Cooling Coil Alternative Mode 2 Performance;  !- Speed 1 Name",

        "  Coil:Cooling:DX:CurveFit:Speed,",
        "    Sys 1 Furnace DX Cool Cooling Coil Alternative Mode 2 Performance,  !- Name",
        "    1.0,                     !- Gross Total Cooling Capacity Fraction",
        "    1.0,                     !- Evaporator Air Flow Rate Fraction",
        "    1.0,                     !- Condenser Air Flow Rate Fraction",
        "    0.16,                !- Gross Sensible Heat Ratio",
        "    1.41,                       !- Gross Cooling COP {W/W}",
        "    1.0,                     !- Active Fraction of Coil Face Area",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    1.0,                     !- Evaporative Condenser Pump Power Fraction",
        "    0,                       !- Evaporative Condenser Effectiveness {dimensionless}",
        "    ReheatTempCoolingCAPFTemp,  !- Total Cooling Capacity Modifier Function of Temperature Curve Name",
        "    ReheatFlowCoolingCAPFFF,  !- Total Cooling Capacity Modifier Function of Air Flow Fraction Curve Name",
        "    ReheatTempCoolingEIRFTemp,  !- Energy Input Ratio Modifier Function of Temperature Curve Name",
        "    ReheatFlowCoolingEIRFFF,  !- Energy Input Ratio Modifier Function of Air Flow Fraction Curve Name",
        "    Sys 1 Furnace DX Cool Cool Coil PLF,  !- Part Load Fraction Correlation Curve Name",
        "    ,                        !- Rated Waste Heat Fraction of Power Input {dimensionless}",
        "    ,                        !- Waste Heat Modifier Function of Temperature Curve Name",
        "    ReheatSHRTempCoolingCAPFTemp,  !- Sensible Heat Ratio Modifier Function of Temperature Curve Name",
        "    ReheatSHRFlowCoolingCAPFFF;    !- Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name",

        "  Curve:Biquadratic,  ",
        "  ReheatTempCoolingCAPFTemp,  !- Name",
        "  1.5813223182,  !- Coefficient1 Constant",
        "  -0.0633198089,  !- Coefficient2 x",
        "  0.0032367633,  !- Coefficient3 x**2",
        "  -0.0007595651,  !- Coefficient4 y",
        "  0.0000014718,  !- Coefficient5 y**2",
        "  -0.0009643593,  !- Coefficient6 x*y",
        "  16.94,  !- Minimum Value of x",
        "  18.5,  !- Maximum Value of x",
        "  4.44,  !- Minimum Value of y",
        "  26.67,  !- Maximum Value of y",
        "  0.9818,  !- Minimum Curve Output",
        "  1.4364,  !- Maximum Curve Output",
        "  Temperature,  !- Input Unit Type for X",
        "  Temperature,  !- Input Unit Type for Y",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Biquadratic,  ",
        "  ReheatTempCoolingEIRFTemp,  !- Name",
        "  0.4050271551,  !- Coefficient1 Constant",
        "  0.0140945368,  !- Coefficient2 x",
        "  -0.0005979622,  !- Coefficient3 x**2",
        "  0.0093711466,  !- Coefficient4 y",
        "  0.0004496998,  !- Coefficient5 y**2",
        "  -0.0000795439,  !- Coefficient6 x*y",
        "  16.94,  !- Minimum Value of x",
        "  18.5,  !- Maximum Value of x",
        "  4.44,  !- Minimum Value of y",
        "  26.67,  !- Maximum Value of y",
        "  0.5036,  !- Minimum Curve Output",
        "  1.0084,  !- Maximum Curve Output",
        "  Temperature,  !- Input Unit Type for X",
        "  Temperature,  !- Input Unit Type for Y",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Quadratic,  ",
        "  ReheatFlowCoolingCAPFFF,  !- Name",
        "  0.7454545455,  !- Coefficient1 Constant",
        "  0.4,  !- Coefficient2 x",
        "  -0.1454545455,  !- Coefficient3 x**2",
        "  0.75,  !- Minimum Value of x",
        "  1.25,  !- Maximum Value of x",
        "  0.9636,  !- Minimum Curve Output",
        "  1.0182,  !- Maximum Curve Output",
        "  Dimensionless,  !- Input Unit Type for X",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Quadratic,  ",
        "  ReheatFlowCoolingEIRFFF,  !- Name",
        "  1.3219073627,  !- Coefficient1 Constant",
        "  -0.5498593181,  !- Coefficient2 x",
        "  0.2279519554,  !- Coefficient3 x**2",
        "  0.75,  !- Minimum Value of x",
        "  1.25,  !- Maximum Value of x",
        "  0.9908,  !- Minimum Curve Output",
        "  1.0377,  !- Maximum Curve Output",
        "  Dimensionless,  !- Input Unit Type for X",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Biquadratic,  ",
        "  ReheatSHRTempCoolingCAPFTemp,  !- Name",
        "  3.9903739056,  !- Coefficient1 Constant",
        "  -0.2158168778,  !- Coefficient2 x",
        "  0.0034746418,  !- Coefficient3 x**2",
        "  0.0310505116,  !- Coefficient4 y",
        "  -0.0000360698,  !- Coefficient5 y**2",
        "  -0.0016903602,  !- Coefficient6 x*y",
        "  16.67,  !- Minimum Value of x",
        "  22.22,  !- Maximum Value of x",
        "  23.89,  !- Minimum Value of y",
        "  51.67,  !- Maximum Value of y",
        "  0.4786,  !- Minimum Curve Output",
        "  1.4194,  !- Maximum Curve Output",
        "  Temperature,  !- Input Unit Type for X",
        "  Temperature,  !- Input Unit Type for Y",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Quadratic,  ",
        "  ReheatSHRFlowCoolingCAPFFF,  !- Name",
        "  -1.4384495222,  !- Coefficient1 Constant",
        "  0.9171312178,  !- Coefficient2 x",
        "  1.5440455771,  !- Coefficient3 x**2",
        "  0.75,  !- Minimum Value of x",
        "  1.25,  !- Maximum Value of x",
        "  0.1179,  !- Minimum Curve Output",
        "  2.1205,  !- Maximum Curve Output",
        "  Dimensionless,  !- Input Unit Type for X",
        "  Dimensionless;  !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    Sys 1 Furnace DX Cool Cool Coil Cap-FT,  !- Name",
        "    0.942587793,             !- Coefficient1 Constant",
        "    0.009543347,             !- Coefficient2 x",
        "    0.00068377,              !- Coefficient3 x**2",
        "    -0.011042676,            !- Coefficient4 y",
        "    0.000005249,             !- Coefficient5 y**2",
        "    -0.00000972,             !- Coefficient6 x*y",
        "    12.77778,                !- Minimum Value of x",
        "    23.88889,                !- Maximum Value of x",
        "    18.0,                    !- Minimum Value of y",
        "    46.11111;                !- Maximum Value of y",

        "  Curve:Quadratic,",
        "    Sys 1 Furnace DX Cool Cool Coil Cap-FF,  !- Name",
        "    0.8,                     !- Coefficient1 Constant",
        "    0.2,                     !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Biquadratic,",
        "    Sys 1 Furnace DX Cool Cool Coil EIR-FT,  !- Name",
        "    0.342414409,             !- Coefficient1 Constant",
        "    0.034885008,             !- Coefficient2 x",
        "    -0.0006237,              !- Coefficient3 x**2",
        "    0.004977216,             !- Coefficient4 y",
        "    0.000437951,             !- Coefficient5 y**2",
        "    -0.000728028,            !- Coefficient6 x*y",
        "    12.77778,                !- Minimum Value of x",
        "    23.88889,                !- Maximum Value of x",
        "    18.0,                    !- Minimum Value of y",
        "    46.11111;                !- Maximum Value of y",

        "  Curve:Quadratic,",
        "    Sys 1 Furnace DX Cool Cool Coil EIR-FF,  !- Name",
        "    1.1552,                  !- Coefficient1 Constant",
        "    -0.1808,                 !- Coefficient2 x",
        "    0.0256,                  !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    Sys 1 Furnace DX Cool Cool Coil PLF,  !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**2",
        "    0,                       !- Minimum Value of x",
        "    1;                       !- Maximum Value of x",

        "  OutdoorAir:Node,",
        "    Sys 1 Furnace DX Cool Cooling Coil Condenser Inlet,  !- Name",
        "    -1;                      !- Height Above Ground {m}",

        "  Coil:Heating:Fuel,",
        "    Sys 1 Furnace DX Cool Heating Coil,  !- Name",
        "    ,                        !- Availability Schedule Name",
        "    NaturalGas,              !- Fuel Type",
        "    0.8,                     !- Burner Efficiency",
        "    19814.84997,                !- Nominal Capacity {W}",
        "    Sys 1 Furnace DX Cool Cooling Coil Outlet,  !- Air Inlet Node Name",
        "    Sys 1 Furnace DX Cool Heating Coil Outlet,  !- Air Outlet Node Name",
        "    ,                        !- Temperature Setpoint Node Name",
        "    0,                       !- Parasitic Electric Load {W}",
        "    Sys 1 Furnace DX Cool Heating Coil PLF-FPLR,  !- Part Load Fraction Correlation Curve Name",
        "    0;                       !- Parasitic Fuel Load {W}",

        "  Curve:Cubic,",
        "    Sys 1 Furnace DX Cool Heating Coil PLF-FPLR,  !- Name",
        "    0.8,                     !- Coefficient1 Constant",
        "    0.2,                     !- Coefficient2 x",
        "    0,                       !- Coefficient3 x**2",
        "    0,                       !- Coefficient4 x**3",
        "    0,                       !- Minimum Value of x",
        "    1;                       !- Maximum Value of x",

        "  Fan:OnOff,",
        "    Sys 1 Furnace DX Cool Supply Fan,  !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    600,                     !- Pressure Rise {Pa}",
        "    0.46609,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    Sys 1 Furnace DX Cool Mixed Air Outlet,  !- Air Inlet Node Name",
        "    Sys 1 Furnace DX Cool Supply Fan Outlet;  !- Air Outlet Node Name",

        "  OutdoorAir:NodeList,",
        "    Sys 1 Furnace DX Cool Outdoor Air Inlet;  !- Node or NodeList Name 1",

        "  AirLoopHVAC:OutdoorAirSystem,",
        "    Sys 1 Furnace DX Cool OA System,  !- Name",
        "    Sys 1 Furnace DX Cool OA System Controllers,  !- Controller List Name",
        "    Sys 1 Furnace DX Cool OA System Equipment,  !- Outdoor Air Equipment List Name",
        "    Sys 1 Furnace DX Cool Availability Managers;  !- Availability Manager List Name",

        "  AirLoopHVAC:ControllerList,",
        "    Sys 1 Furnace DX Cool OA System Controllers,  !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    Sys 1 Furnace DX Cool OA Controller;  !- Controller 1 Name",

        "  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    Sys 1 Furnace DX Cool OA System Equipment,  !- Name",
        "    OutdoorAir:Mixer,        !- Component 1 Object Type",
        "    Sys 1 Furnace DX Cool OA Mixing Box;  !- Component 1 Name",

        "  OutdoorAir:Mixer,",
        "    Sys 1 Furnace DX Cool OA Mixing Box,  !- Name",
        "    Sys 1 Furnace DX Cool Mixed Air Outlet,  !- Mixed Air Node Name",
        "    Sys 1 Furnace DX Cool Outdoor Air Inlet,  !- Outdoor Air Stream Node Name",
        "    Sys 1 Furnace DX Cool Relief Air Outlet,  !- Relief Air Stream Node Name",
        "    Sys 1 Furnace DX Cool Air Loop Inlet;  !- Return Air Stream Node Name",

        "  SetpointManager:SingleZone:Cooling,",
        "    Sys 1 Furnace DX Cool Economizer Supply Air Temp Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    13,                      !- Minimum Supply Air Temperature {C}",
        "    45,                      !- Maximum Supply Air Temperature {C}",
        "    ZONE ONE,                !- Control Zone Name",
        "    SPACE1-1 Zone Air Node,  !- Zone Node Name",
        "    SPACE1-1 Zone Equip Inlet,  !- Zone Inlet Node Name",
        "    Sys 1 Furnace DX Cool Heating Coil Outlet;  !- Setpoint Node or NodeList Name",

        "  SetpointManager:MixedAir,",
        "    Sys 1 Furnace DX Cool Cooling Coil Air Temp Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    Sys 1 Furnace DX Cool Heating Coil Outlet,  !- Reference Setpoint Node Name",
        "    Sys 1 Furnace DX Cool Mixed Air Outlet,  !- Fan Inlet Node Name",
        "    Sys 1 Furnace DX Cool Supply Fan Outlet,  !- Fan Outlet Node Name",
        "    Sys 1 Furnace DX Cool Mixed Air Outlet;  !- Setpoint Node or NodeList Name",

        "  Controller:OutdoorAir,",
        "    Sys 1 Furnace DX Cool OA Controller,  !- Name",
        "    Sys 1 Furnace DX Cool Relief Air Outlet,  !- Relief Air Outlet Node Name",
        "    Sys 1 Furnace DX Cool Air Loop Inlet,  !- Return Air Node Name",
        "    Sys 1 Furnace DX Cool Mixed Air Outlet,  !- Mixed Air Node Name",
        "    Sys 1 Furnace DX Cool Outdoor Air Inlet,  !- Actuator Node Name",
        "    0.10384,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    0.46609,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    DifferentialDryBulb,     !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    20,                      !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    LockoutWithCompressor,   !- Lockout Type",
        "    FixedMinimum,            !- Minimum Limit Type",
        "    Min OA Sched,            !- Minimum Outdoor Air Schedule Name",
        "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
        "    ;                        !- Mechanical Ventilation Controller Name",

        "  ZoneControl:Thermostat,",
        "    SPACE1-1 Thermostat,     !- Name",
        "    ZONE ONE,                !- Zone or ZoneList Name",
        "    HVACTemplate-Always 4,   !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    All Zones Dual SP Control;  !- Control 1 Name",

        "  ZoneControl:Humidistat,",
        "    SPACE1-1 Humidistat,  !- Name",
        "    ZONE ONE,                !- Zone Name",
        "    HVACTemplate-Always 4,   !- Humidifying Relative Humidity Setpoint Schedule Name",
        "    HVACTemplate-Always 60;  !- Dehumidifying Relative Humidity Setpoint Schedule Name",

        "  ThermostatSetpoint:DualSetpoint,",
        "    All Zones Dual SP Control,  !- Name",
        "    Htg-SetP-Sch,            !- Heating Setpoint Temperature Schedule Name",
        "    Clg-SetP-Sch;            !- Cooling Setpoint Temperature Schedule Name",

        "  ScheduleTypeLimits,",
        "    HVACTemplate Any Number; !- Name",

        "  Schedule:Compact,",
        "    HVACTemplate-Always 4,   !- Name",
        "    HVACTemplate Any Number, !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,4;          !- Field 3",

        "  Schedule:Compact,",
        "    Htg-SetP-Sch,            !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2, !- Field 2",
        "    Until: 6:00,13.0,        !- Field 3",
        "    Until: 7:00,18.0,        !- Field 5",
        "    Until: 21:00,23.0,       !- Field 7",
        "    Until: 24:00,13.0,       !- Field 9",
        "    For: WeekEnds Holiday,   !- Field 11",
        "    Until: 24:00,13.0,       !- Field 12",
        "    For: SummerDesignDay,    !- Field 14",
        "    Until: 24:00,13.0,       !- Field 15",
        "    For: WinterDesignDay,    !- Field 17",
        "    Until: 24:00,23.0;       !- Field 18",

        "  Schedule:Compact,",
        "    HVACTemplate-Always 60,  !- Name",
        "    HVACTemplate Any Number, !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,60;         !- Field 3",

        "  Schedule:Compact,",
        "    Clg-SetP-Sch,            !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2, !- Field 2",
        "    Until: 7:00,32.0,        !- Field 3",
        "    Until: 21:00,24.0,       !- Field 5",
        "    Until: 24:00,32.0,       !- Field 7",
        "    For: WeekEnds Holiday,   !- Field 9",
        "    Until: 24:00,32.0,       !- Field 10",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00,24.0,       !- Field 13",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00,32.0;       !- Field 16",

        "  Schedule:Compact,",
        "    FanAvailSched,           !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2, !- Field 2",
        "    Until: 7:00,0.0,         !- Field 3",
        "    Until: 21:00,1.0,        !- Field 5",
        "    Until: 24:00,0.0,        !- Field 7",
        "    For: Weekends Holiday,   !- Field 9",
        "    Until: 24:00,0.0,        !- Field 10",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00,1.0,        !- Field 13",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00,1.0;        !- Field 16",

        "  Schedule:Compact,",
        "    Min OA Sched,            !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2, !- Field 2",
        "    Until: 8:00,0.0,         !- Field 3",
        "    Until: 21:00,1.0,        !- Field 5",
        "    Until: 24:00,0.0,        !- Field 7",
        "    For: Weekends Holiday,   !- Field 9",
        "    Until: 24:00,0.0,        !- Field 10",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00,1.0,        !- Field 13",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00,1.0;        !- Field 16",

        "  Sizing:Parameters,",
        "    1.2,                     !- Heating Sizing Factor",
        "    1.2;                     !- Cooling Sizing Factor",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    // Read objects
    HeatBalanceManager::GetProjectControlData(state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetWindowGlassSpectralData(state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    SurfaceGeometry::CosBldgRotAppGonly = 1.0;
    SurfaceGeometry::SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::GetSurfaceData(state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    ScheduleManager::ProcessScheduleInput(state); // read schedules

    ScheduleManager::Schedule(1).CurrentValue = 1.0; // Enable schedule without calling schedule manager
    ScheduleManager::Schedule(6).CurrentValue = 1.0; // Enable schedule without calling schedule manager
    ScheduleManager::Schedule(7).CurrentValue = 4.0; // Enable schedule without calling schedule manager

    ZoneEquipmentManager::GetZoneEquipment(state);
    SimAirServingZones::GetAirPathData(state);
    ScheduleManager::Schedule(7).MinValue = 4.0; // Enable schedule without calling schedule manager
    ScheduleManager::Schedule(7).MaxValue = 4.0; // Enable schedule without calling schedule manager
    ScheduleManager::Schedule(7).MaxMinSet = true;
    ZoneTempPredictorCorrector::GetZoneAirSetPoints(state);

    std::string compName = "SYS 1 FURNACE DX COOL UNITARY SYSTEM";
    bool zoneEquipment = false;
    int compTypeOfNum = DataHVACGlobals::UnitarySys_AnyCoilType;
    bool FirstHVACIteration = true;
    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;
    UnitarySystems::UnitarySys::factory(state, compTypeOfNum, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
                                                                                        // verify the size of the vector and the processed names
    // 1 UnitarySystem objects
    EXPECT_EQ(1u, unitarySys.size());

    DataGlobals::BeginEnvrnFlag = true; // act as if simulation is beginning

    //// test calling the sim routine
    int AirLoopNum = 1;
    int CompIndex = 1; // zero based index
    bool HeatingActive = false;
    bool CoolingActive = true;
    int OAUnitNum = 0;
    Real64 OAUCoilOutTemp = 0.0;
    DataGlobals::SysSizingCalc = false;
    DataSizing::CurSysNum = 1;
    bool ZoneEquipFlag = false;
    // simulate function is overloaded, but only to report back SysOutputProvided and LatOutputProvided. Either signature should give same result.
    // thisSys->m_OKToPrintSizing = true;
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -1000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -1000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -200.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).RemainingOutputReqToDehumidSP = -200.0 / 2500940.0;
    // test COOLING condition
    DataLoopNode::Node(1).Temp = 24.0;      // 24C db
    DataLoopNode::Node(1).HumRat = 0.01522; // 17C wb
    DataLoopNode::Node(1).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(1).Temp, DataLoopNode::Node(1).HumRat);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRat(1) = DataLoopNode::Node(1).HumRat;
    DataHeatBalFanSys::MAT(1) = DataLoopNode::Node(1).Temp;
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);

    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = 4;
    DataLoopNode::Node(8).Temp = 23.822;        // 24C db
    DataLoopNode::Node(8).HumRat = 0.0145946;   // 17C wb
    DataLoopNode::Node(8).Enthalpy = 61084.266; // www.sugartech.com/psychro/index.php
    Real64 SenOutput;
    Real64 LatOutput;

    // OperatingMode 3 above the range
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -227.705;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -227.705;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -50.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).RemainingOutputReqToDehumidSP = -0.007806893;
    DataEnvironment::StdRhoAir = 1.2043;
    thisSys->simulate(state,
                      compName,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatingActive,
                      CoolingActive,
                      OAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipFlag,
                      SenOutput,
                      LatOutput);
    EXPECT_EQ(EnergyPlus::coilCoolingDXs[0].performance.OperatingMode, 3);
    EXPECT_EQ(EnergyPlus::coilCoolingDXs[0].performance.ModeRatio, 1.0);
    EXPECT_NEAR(thisSys->CoilSHR, thisSys->LoadSHR, 0.001);
    EXPECT_NEAR(SenOutput, -227.705, 0.1);
    EXPECT_NEAR(LatOutput, -737.9931, 0.1);

    // OperatingMode 3 with mode ratio < 1
    thisSys->m_ZoneSequenceCoolingNum = 0;
    thisSys->m_ZoneSequenceHeatingNum = 0;
    DataLoopNode::Node(1).HumRat = 0.0114544;   // 17C wb
    DataLoopNode::Node(1).Enthalpy = 53273.99;  // www.sugartech.com/psychro/index.php
    DataLoopNode::Node(8).Temp = 24.18496;      // 24C db
    DataLoopNode::Node(8).HumRat = 0.0121542;   // 17C wb
    DataLoopNode::Node(8).Enthalpy = 55245.434; // www.sugartech.com/psychro/index.php
    DataHeatBalFanSys::ZoneAirHumRat(1) = DataLoopNode::Node(1).HumRat;
    DataHeatBalFanSys::MAT(1) = DataLoopNode::Node(1).Temp;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -397.162;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -397.162;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -3601.8;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).RemainingOutputReqToDehumidSP = -1.1696238E-4;
    thisSys->simulate(state,
                      compName,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatingActive,
                      CoolingActive,
                      OAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipFlag,
                      SenOutput,
                      LatOutput);
    EXPECT_EQ(EnergyPlus::coilCoolingDXs[0].performance.OperatingMode, 3);
    EXPECT_NEAR(EnergyPlus::coilCoolingDXs[0].performance.ModeRatio, 0.6607, 0.001);
    EXPECT_NEAR(thisSys->LoadSHR, 0.57154, 0.001);
    EXPECT_NEAR(thisSys->CoilSHR, 0.44387, 0.001);
    EXPECT_NEAR(SenOutput, -397.162, 0.1);
    EXPECT_NEAR(LatOutput, -523.848, 0.1);

    // OperatingMode 2
    thisSys->m_ZoneSequenceCoolingNum = 0;
    thisSys->m_ZoneSequenceHeatingNum = 0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.00;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -2000.00;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(1).RemainingOutputReqToDehumidSP = -1.1696238E-5;
    thisSys->simulate(state,
                      compName,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatingActive,
                      CoolingActive,
                      OAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipFlag,
                      SenOutput,
                      LatOutput);
    EXPECT_EQ(EnergyPlus::coilCoolingDXs[0].performance.OperatingMode, 1);
    EXPECT_EQ(EnergyPlus::coilCoolingDXs[0].performance.ModeRatio, 0.0);
    EXPECT_NEAR(thisSys->LoadSHR, 0.98533, 0.001);
    EXPECT_NEAR(thisSys->CoilSHR, 0.97600, 0.001);
    EXPECT_NEAR(SenOutput, -2000.0, 0.5);
    EXPECT_NEAR(LatOutput, -327.04, 0.1);
}
// This issue tests for GetInput with respect to Autosizing, especially for issue #7771 where
// 'Minimum Supply Air Temperature' == 'Autosize' produces a crash
TEST_F(EnergyPlusFixture, UnitarySystemModel_GetInput_Autosizing)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  EAST ZONE,                !- Name",
        "  0,                        !- Direction of Relative North",
        "  0,                        !- X Origin",
        "  0,                        !- Y Origin",
        "  0,                        !- Z Origin",
        "  1,                        !- Type",
        "  1,                        !- Multiplier",
        "  autocalculate,            !- Ceiling Height",
        "  autocalculate;            !- Volume",

        "ZoneHVAC:EquipmentConnections,",
        "  EAST ZONE,                !- Zone Name",
        "  Zone2Equipment,           !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,        !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,        !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,              !- Zone Air Node Name",
        "  Zone 2 Outlet Node;       !- Zone Return Air Node or NodeList Name",

        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,           !- Name",
        "  SequentialLoad,           !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem,    !- Zone Equipment 1 Object Type",
        "  Unitary System Model,     !- Zone Equipment 1 Name",
        "  1,                        !- Zone Equipment 1 Cooling Sequence",
        "  1,                        !- Zone Equipment 1 Heating or NoLoad Sequence",
        "  ,                         !- Zone Equipment 1 Sequential Cooling Fraction Schedule Name",
        "  ;                         !- Zone Equipment 1 Sequential Heating Fraction Schedule Name",

        // Note: Control type MUST be SingleZoneVAV if you are autosizing 'Minimum Supply Air Temperature'
        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,     !- Name",
        "  SingleZoneVAV,            !- Control Type",
        "  East Zone,                !- Controlling Zone or Thermostat Location",
        "  None,                     !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,     !- Availability Schedule Name",
        "  Zone Exhaust Node,        !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,        !- Air Outlet Node Name",
        "  Fan:OnOff,                !- Supply Fan Object Type",
        "  Supply Fan 1,             !- Supply Fan Name",
        "  BlowThrough,              !- Fan Placement",
        "  ContinuousFanSchedule,    !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Fuel,        !- Heating Coil Object Type",
        "  Furnace Heating Coil 1,    !- Heating Coil Name",
        "  ,                         !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:SingleSpeed,    !- Cooling Coil Object Type",
        "  Furnace ACDXCoil 1,       !- Cooling Coil Name",
        "  No,                       !- Use DOAS DX Cooling Coil",
        "  Autosize,                 !- Minimum Supply Air Temperature",
        "  ,                         !- Latent Load Control",
        "  Coil:Heating:Fuel,        !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1,    !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,        !- Cooling Supply Air Flow Rate Method",
        "  Autosize,                 !- Cooling Supply Air Flow Rate",
        "  ,                         !- Cooling Supply Air Flow Rate Per Floor Area",
        "  ,                         !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "  ,                         !- Cooling Supply Air Flow Rate Per Unit of Capacity",
        "  SupplyAirFlowRate,        !- Heating Supply Air Flow Rate Method",
        "  Autosize,                 !- Heating Supply Air Flow Rate",
        "  ,                         !- Heating Supply Air Flow Rate Per Floor Area",
        "  ,                         !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "  ,                         !- Heating Supply Air Flow Rate Per Unit of Capacity",
        "  SupplyAirFlowRate,        !- No Load Supply Air Flow Rate Method",
        "  Autosize,                 !- No Load Supply Air Flow Rate",
        "  ,                         !- No Load Supply Air Flow Rate Per Floor Area",
        "  ,                         !- No Load Fraction of Autosized Cooling Supply Air Flow Rate",
        "  ,                         !- No Load Fraction of Autosized Heating Supply Air Flow Rate",
        "  ,                         !- No Load Supply Air Flow Rate Per Unit of Capacity During Cooling Operation",
        "  ,                         !- No Load Supply Air Flow Rate Per Unit of Capacity During Heating Operation",
        "  Autosize;                 !- Maximum Supply Air Temperature",

        "Fan:OnOff,",
        "  Supply Fan 1,             !- Name",
        "  FanAndCoilAvailSched,     !- Availability Schedule Name",
        "  0.7,                      !- Fan Total Efficiency",
        "  600,                      !- Pressure Rise",
        "  1.6,                      !- Maximum Flow Rate",
        "  0.9,                      !- Motor Efficiency",
        "  1,                        !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,        !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Cooling:DX:SingleSpeed,",
        "  Furnace ACDXCoil 1,       !- Name",
        "  FanAndCoilAvailSched,     !- Availability Schedule Name",
        "  32000,                    !- Gross Rated Total Cooling Capacity",
        "  0.75,                     !- Gross Rated Sensible Heat Ratio",
        "  3,                        !- Gross Rated Cooling COP",
        "  1.6,                      !- Rated Air Flow Rate",
        "  ,                         !- Rated Evaporator Fan Power Per Volume Flow Rate",
        "  DX Cooling Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,    !- Air Outlet Node Name",
        "  WindACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,              !- Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,             !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,            !- Part Load Fraction Correlation Curve Name",
        "  ,                         !- Minimum Outdoor DryBulb Temperature for Compressor Operation",
        "  1000,                     !- Nominal Time for Condensate Removal to Begin",
        "  0.4,                      !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
        "  4,                        !- Maximum Cycling Rate",
        "  45;                       !- Latent Capacity Time Constant",

        "Coil:Heating:Fuel,",
        "  Furnace Heating Coil 1,    !- Name",
        "  FanAndCoilAvailSched,     !- Availability Schedule Name",
        "  NaturalGas,               !- Fuel Type",
        "  0.8,                      !- Burner Efficiency",
        "  32000,                    !- Nominal Capacity",
        "  Heating Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1, !- Name",
        "  FanAndCoilAvailSched,     !- Availability Schedule Name",
        "  NaturalGas,               !- Fuel Type",
        "  0.8,                      !- Burner Efficiency",
        "  32000,                    !- Nominal Capacity",
        "  Reheat Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;        !- Air Outlet Node Name",

        "ScheduleTypeLimits,",
        "  Any Number;               !- Name",

        "Schedule:Compact,",
        "  FanAndCoilAvailSched,     !- Name",
        "  Any Number,               !- Schedule Type Limits Name",
        "  Through: 12/31,           !- Field 1",
        "  For: AllDays,             !- Field 2",
        "  Until: 24:00,             !- Field 3",
        "  1.0;                      !- Field 4",

        "Schedule:Compact,",
        "  ContinuousFanSchedule,    !- Name",
        "  Any Number,               !- Schedule Type Limits Name",
        "  Through: 12/31,           !- Field 1",
        "  For: AllDays,             !- Field 2",
        "  Until: 24:00,             !- Field 3",
        "  1.0;                      !- Field 4",

        "Curve:Quadratic,",
        "  WindACCoolCapFFF,         !- Name",
        "  0.8,                      !- Coefficient1 Constant",
        "  0.2,                      !- Coefficient2 x",
        "  0,                        !- Coefficient3 x2",
        "  0.5,                      !- Minimum Value of x",
        "  1.5;                      !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACEIRFFF,             !- Name",
        "  1.1552,                   !- Coefficient1 Constant",
        "  -0.1808,                  !- Coefficient2 x",
        "  0.0256,                   !- Coefficient3 x2",
        "  0.5,                      !- Minimum Value of x",
        "  1.5;                      !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACPLFFPLR,            !- Name",
        "  0.85,                     !- Coefficient1 Constant",
        "  0.15,                     !- Coefficient2 x",
        "  0,                        !- Coefficient3 x2",
        "  0,                        !- Minimum Value of x",
        "  1;                        !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  WindACCoolCapFT,          !- Name",
        "  0.942587793,              !- Coefficient1 Constant",
        "  0.009543347,              !- Coefficient2 x",
        "  0.00068377,               !- Coefficient3 x2",
        "  -0.011042676,             !- Coefficient4 y",
        "  5.249e-06,                !- Coefficient5 y2",
        "  -9.72e-06,                !- Coefficient6 xy",
        "  12.77778,                 !- Minimum Value of x",
        "  23.88889,                 !- Maximum Value of x",
        "  18,                       !- Minimum Value of y",
        "  46.11111,                 !- Maximum Value of y",
        "  ,                         !- Minimum Curve Output",
        "  ,                         !- Maximum Curve Output",
        "  Temperature,              !- Input Unit Type for X",
        "  Temperature,              !- Input Unit Type for Y",
        "  Dimensionless;            !- Output Unit Type",

        "Curve:Biquadratic,",
        "  WindACEIRFT,              !- Name",
        "  0.342414409,              !- Coefficient1 Constant",
        "  0.034885008,              !- Coefficient2 x",
        "  -0.0006237,               !- Coefficient3 x2",
        "  0.004977216,              !- Coefficient4 y",
        "  0.000437951,              !- Coefficient5 y2",
        "  -0.000728028,             !- Coefficient6 xy",
        "  12.77778,                 !- Minimum Value of x",
        "  23.88889,                 !- Maximum Value of x",
        "  18,                       !- Minimum Value of y",
        "  46.11111,                 !- Maximum Value of y",
        "  ,                         !- Minimum Curve Output",
        "  ,                         !- Maximum Curve Output",
        "  Temperature,              !- Input Unit Type for X",
        "  Temperature,              !- Input Unit Type for Y",
        "  Dimensionless;            !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    bool ErrorsFound(false);
    HeatBalanceManager::GetZoneData(state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(state); // read zone equipment configuration and list objects

    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    int compTypeOfNum = DataHVACGlobals::UnitarySys_AnyCoilType;

    UnitarySystems::UnitarySys::factory(state, compTypeOfNum, compName, zoneEquipment, 0);
    // only 1 unitary system above so expect 1 as number of unitary system objects
    ASSERT_EQ(1, UnitarySystems::numUnitarySystems);
    // Now retrieve it
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    // indicate zone data is available
    DataZoneEquipment::ZoneEquipInputsFilled = true;

    // get UnitarySystem input from object above
    // Before fix for #7771, it throws
    // C++ exception with description "[json.exception.type_error.302] type must be number, but is string" thrown in the test body.
    ASSERT_NO_THROW(thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound));
    EXPECT_FALSE(ErrorsFound);

    // Like I said above in the IDF snippet section, control type has to be SingleZoneVAV or autosizing of
    // 'Minimum Supply Air Temperature' (DesignMinOutletTemp) isn't allowed
    EXPECT_EQ(thisSys->m_ControlType, EnergyPlus::UnitarySystems::UnitarySys::ControlType::CCMASHRAE);
    EXPECT_EQ(thisSys->DesignMinOutletTemp, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxNoCoolHeatAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->DesignMaxOutletTemp, DataSizing::AutoSize);
    EXPECT_TRUE(thisSys->m_RequestAutoSize);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_VariableSpeedDXCoilsNoLoadFlowRateSizing)
{

    std::string const idf_objects = delimited_string({

        "AirLoopHVAC:UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Load,                           !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  ,                               !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  East Zone Inlet Node,           !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan One,                 !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:DX:VariableSpeed,  !- Heating Coil Object Type",
        "  DX Heating Coil,                !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:VariableSpeed,  !- Cooling Coil Object Type",
        "  DX Cooling Coil,                !- Cooling Coil Name",
        "  ,                               !- Use DOAS DX Cooling Coil",
        "  15.0,                           !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                               !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,              !- Supply Air Flow Rate Method During Cooling Operation",
        "  autosize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply air Flow Rate Method During Heating Operation",
        "  autosize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,              !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  autosize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ;                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",

        "Fan:OnOff,",
        "  Supply Fan One,                 !- Name",
        "  ,                               !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  autosize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Cooling Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Cooling:DX:VariableSpeed,",
        "  DX Cooling Coil,                !- Name",
        "  Cooling Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,    !- Air Outlet Node Name",
        "  5.0,                            !- Number of Speeds{ dimensionless }",
        "  5.0,                            !- Nominal Speed Level{ dimensionless }",
        "  autosize,                       !- Gross Rated Total Cooling Capacity At Selected Nominal Speed Level{ w }",
        "  autosize,                       !- Rated Air Flow Rate At Selected Nominal Speed Level{ m3 / s }",
        "  0.0,                            !- Nominal Time for Condensate to Begin Leaving the Coil{ s }",
        "  0.0,                            !- Initial Moisture Evaporation Rate Divided by Steady - State AC Latent Capacity{ dimensionless }",
        "  Quadratic,                      !- Energy Part Load Fraction Curve Name",
        "  ,                               !- Condenser Air Inlet Node Name",
        "  AirCooled,                      !- Condenser Type",
        "  ,                               !- Evaporative Condenser Pump Rated Power Consumption{ W }",
        "  200.0,                          !- Crankcase Heater Capacity{ W }",
        "  10.0,                           !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  ,                               !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                               !- Supply Water Storage Tank Name",
        "  ,                               !- Condensate Collection Water Storage Tank Name",
        "  ,                               !- Basin Heater Capacity{ W / K }",
        "  ,                               !- Basin Heater Setpoint Temperature{ C }",
        "  ,                               !- Basin Heater Operating Schedule Name",
        "  3500.0,                         !- Speed 1 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 1 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 1 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.188,                          !- Speed 1 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.260,                          !- Speed 1 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 1 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  7000.0,                         !- Speed 2 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 2 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 2 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.376,                          !- Speed 2 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.520,                          !- Speed 2 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 2 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  10500.0,                        !- Speed 3 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 3 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 3 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.564,                          !- Speed 3 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  0.780,                          !- Speed 3 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 3 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  14000.0,                        !- Speed 4 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 4 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 4 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.752,                          !- Speed 4 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  1.040,                          !- Speed 4 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 4 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  17500.0,                        !- Speed 5 Reference Unit Gross Rated Total Cooling Capacity{ w }",
        "  0.75,                           !- Speed 5 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
        "  4.0,                            !- Speed 5 Reference Unit Gross Rated Cooling COP{ dimensionless }",
        "  0.940,                          !- Speed 5 Reference Unit Rated Air Flow Rate{ m3 / s }",
        "  1.300,                          !- Speed 5 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
        "  ,                               !- Speed 5 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
        "  Biquadratic,                    !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic;                      !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",

        "Coil:Heating:DX:VariableSpeed, ",
        "  DX Heating Coil,                !- Name",
        "  Heating Coil Air Inlet Node,    !- Indoor Air Inlet Node Name",
        "  East Zone Inlet Node,           !- Indoor Air Outlet Node Name",
        "  5.0,                            !- Number of Speeds {dimensionless}",
        "  5.0,                            !- Nominal Speed Level {dimensionless}",
        "  autosize,                       !- Rated Heating Capacity At Selected Nominal Speed Level {w}",
        "  1.7,                            !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
        "  Quadratic,                      !- Energy Part Load Fraction Curve Name",
        "      ,                           !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "  -5.0,                           !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "  5.0,                            !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "  200.0,                          !- Crankcase Heater Capacity {W}",
        "  10.0,                           !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  Resistive,                      !- Defrost Strategy",
        "  TIMED,                          !- Defrost Control",
        "  0.166667,                       !- Defrost Time Period Fraction",
        "  20000,                          !- Resistive Defrost Heater Capacity {W}",
        "  3600.0,                         !- Speed 1 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 1 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.169,                          !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  7200.0,                         !- Speed 2 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 2 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.387,                          !- Speed 2 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  10800.0,                        !- Speed 3 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 3 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.580,                          !- Speed 3 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  14400.0,                        !- Speed 4 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 4 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.773,                          !- Speed 4 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
        "  18000.0,                        !- Speed 5 Reference Unit Gross Rated Heating Capacity {w}",
        "  5.0,                            !- Speed 5 Reference Unit Gross Rated Heating COP {dimensionless}",
        "  0.966,                          !- Speed 5 Reference Unit Rated Air Flow Rate {m3/s}",
        "  Biquadratic,                    !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic;                      !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  1.0,                            !- Coefficient1 Constant",
        "  0.0,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  1.0,                            !- Minimum Value of x",
        "  1.0;                            !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Biquadratic,                    !- Name",
        "  1.0,                            !- Coefficient1 Constant",
        "  0.0,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.0,                            !- Coefficient4 y",
        "  0.0,                            !- Coefficient5 y**2",
        "  0.0,                            !- Coefficient6 x*y",
        "  0.0,                            !- Minimum Value of x",
        "  100.0,                          !- Maximum Value of x",
        "  0.0,                            !- Minimum Value of y",
        "  100.0,                          !- Maximum Value of y",
        "  1.0,                            !- Minimum Curve Output",
        "  1.0,                            !- Maximum Curve Output",
        "  Temperature,                    !- Input Unit Type for X",
        "  Temperature,                    !- Input Unit Type for Y",
        "  Dimensionless;                  !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors
    ;
    // Verify UnitarySystem air flow rates are read in as AutoSized
    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxNoCoolHeatAirVolFlow, DataSizing::AutoSize);

    OutputReportPredefined::SetPredefinedTables();
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;
    int AirLoopNum = 0;

    thisSys->sizeSystem(state, FirstHVACIteration, AirLoopNum);
    EXPECT_EQ(1.500, thisSys->m_DesignFanVolFlowRate);
    EXPECT_EQ(1.500, thisSys->m_MaxCoolAirVolFlow);
    EXPECT_EQ(1.500, thisSys->m_MaxHeatAirVolFlow);

    Real64 results_noLoadHeatingFlowRatio =
        state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(1) / state.dataVariableSpeedCoils->VarSpeedCoil(1).MSRatedAirVolFlowRate(5);
    Real64 results_noLoadCoolingFlowRatio =
        state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(1) / state.dataVariableSpeedCoils->VarSpeedCoil(2).MSRatedAirVolFlowRate(5);
    Real64 results_noLoadFlowRatioMin = min(results_noLoadHeatingFlowRatio, results_noLoadCoolingFlowRatio);

    EXPECT_NEAR(results_noLoadFlowRatioMin, thisSys->m_NoLoadAirFlowRateRatio, 0.00001);
    EXPECT_NEAR(0.17495, thisSys->m_NoLoadAirFlowRateRatio, 0.00001);
    EXPECT_NEAR(0.26242, thisSys->m_MaxNoCoolHeatAirVolFlow, 0.00001);
    EXPECT_NEAR(0.26242, thisSys->m_MaxCoolAirVolFlow * thisSys->m_NoLoadAirFlowRateRatio, 0.00001);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_MultiSpeedDXCoilsNoLoadFlowRateSizing)
{

    std::string const idf_objects = delimited_string({

        "    AirLoopHVAC:UnitarySystem,",
        "      UNITARY SYSTEM MODEL,    !- Name",
        "      Load,                    !- Control Type",
        "      East Zone,               !- Controlling Zone or Thermostat Location",
        "      None,                    !- Dehumidification Control Type",
        "      ,                        !- Availability Schedule Name",
        "      Zone Exhaust Node,       !- Air Inlet Node Name",
        "      East Zone Inlet Node,    !- Air Outlet Node Name",
        "      Fan:OnOff,               !- Supply Fan Object Type",
        "      Furnace DX Cool Supply Fan,  !- Supply Fan Name",
        "      BlowThrough,             !- Fan Placement",
        "      ,                        !- Supply Air Fan Operating Mode Schedule Name",
        "      Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "      Furnace DX Cool Heating Coil,  !- Heating Coil Name",
        "      1.0,                     !- DX Heating Coil Sizing Ratio",
        "      Coil:Cooling:DX,         !- Cooling Coil Object Type",
        "      Furnace DX Cool Cooling Coil,  !- Cooling Coil Name",
        "      ,                        !- Use DOAS DX Cooling Coil",
        "      ,                        !- Minimum Supply Air Temperature {C}",
        "      ,                        !- Latent Load Control",
        "      ,                        !- Supplemental Heating Coil Object Type",
        "      ,                        !- Supplemental Heating Coil Name",
        "      SupplyAirFlowRate,       !- Cooling Supply Air Flow Rate Method",
        "      autosize,                !- Cooling Supply Air Flow Rate {m3/s}",
        "      ,                        !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "      ,                        !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "      ,                        !- Cooling Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "      SupplyAirFlowRate,       !- Heating Supply Air Flow Rate Method",
        "      autosize,                !- Heating Supply Air Flow Rate {m3/s}",
        "      ,                        !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "      ,                        !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "      ,                        !- Heating Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "      SupplyAirFlowRate,       !- No Load Supply Air Flow Rate Method",
        "      autosize,                !- No Load Supply Air Flow Rate {m3/s}",
        "      ,                        !- No Load Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "      ,                        !- No Load Fraction of Autosized Cooling Supply Air Flow Rate",
        "      ,                        !- No Load Fraction of Autosized Heating Supply Air Flow Rate",
        "      ,                        !- No Load Supply Air Flow Rate Per Unit of Capacity During Cooling Operation {m3/s-W}",
        "      ,                        !- No Load Supply Air Flow Rate Per Unit of Capacity During Heating Operation {m3/s-W}",
        "      Autosize,                !- Maximum Supply Air Temperature {C}",
        "      21,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "      ,                        !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "      ,                        !- Maximum Cycling Rate {cycles/hr}",
        "      ,                        !- Heat Pump Time Constant {s}",
        "      ,                        !- Fraction of On-Cycle Power Use",
        "      ,                        !- Heat Pump Fan Delay Time {s}",
        "      ,                        !- Ancillary On-Cycle Electric Power {W}",
        "      ,                        !- Ancillary Off-Cycle Electric Power {W}",
        "      ,                        !- Design Heat Recovery Water Flow Rate {m3/s}",
        "      ,                        !- Maximum Temperature for Heat Recovery {C}",
        "      ,                        !- Heat Recovery Water Inlet Node Name",
        "      ,                        !- Heat Recovery Water Outlet Node Name",
        "      UnitarySystemPerformance:Multispeed,  !- Design Specification Multispeed Object Type",
        "      Furnace DX Cool Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "    UnitarySystemPerformance:Multispeed,",
        "      Furnace DX Cool Unitary System MultiSpeed Performance,  !- Name",
        "      1,                       !- Number of Speeds for Heating",
        "      2,                       !- Number of Speeds for Cooling",
        "      No,                      !- Single Mode Operation",
        "      ,                        !- No Load Supply Air Flow Rate Ratio",
        "      autosize,                !- Heating Speed 1 Supply Air Flow Ratio",
        "      autosize,                !- Cooling Speed 1 Supply Air Flow Ratio",
        "      autosize,                !- Heating Speed 2 Supply Air Flow Ratio",
        "      autosize;                !- Cooling Speed 2 Supply Air Flow Ratio",

        "    Coil:Cooling:DX,",
        "      Furnace DX Cool Cooling Coil,  !- Name",
        "      Furnace DX Cool Supply Fan Outlet,  !- Evaporator Inlet Node Name",
        "      Furnace DX Cool Cooling Coil Outlet,  !- Evaporator Outlet Node Name",
        "      ,                        !- Availability Schedule Name",
        "      ,                        !- Condenser Zone Name",
        "      Furnace DX Cool Cooling Coil Condenser Inlet,  !- Condenser Inlet Node Name",
        "      Furnace DX Cool Cooling Coil Condenser Outlet Node,  !- Condenser Outlet Node Name",
        "      Furnace DX Cool Cooling Coil Performance,  !- Performance Object Name",
        "      ,                        !- Condensate Collection Water Storage Tank Name",
        "      ;                        !- Evaporative Condenser Supply Water Storage Tank Name",

        "    Coil:Cooling:DX:CurveFit:Performance,",
        "      Furnace DX Cool Cooling Coil Performance,  !- Name",
        "      0.0,                     !- Crankcase Heater Capacity {W}",
        "      ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "      10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "      ,                        !- Unit Internal Static Air Pressure {Pa}",
        "      Discrete,                !- Capacity Control Method",
        "      ,                        !- Evaporative Condenser Basin Heater Capacity {W/K}",
        "      ,                        !- Evaporative Condenser Basin Heater Setpoint Temperature {C}",
        "      ,                        !- Evaporative Condenser Basin Heater Operating Schedule Name",
        "      Electricity,             !- Compressor Fuel Type",
        "      Furnace DX Cool Cooling Coil Operating Mode;  !- Base Operating Mode",

        "    Coil:Cooling:DX:CurveFit:OperatingMode,",
        "      Furnace DX Cool Cooling Coil Operating Mode,  !- Name",
        "      autosize,                !- Rated Gross Total Cooling Capacity {W}",
        "      autosize,                !- Rated Evaporator Air Flow Rate {m3/s}",
        "      ,                        !- Rated Condenser Air Flow Rate {m3/s}",
        "      0,                       !- Maximum Cycling Rate {cycles/hr}",
        "      0,                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "      0,                       !- Latent Capacity Time Constant {s}",
        "      0,                       !- Nominal Time for Condensate Removal to Begin {s}",
        "      No,                      !- Apply Latent Degradation to Speeds Greater than 1",
        "      AirCooled,               !- Condenser Type",
        "      ,                        !- Nominal Evaporative Condenser Pump Power {W}",
        "      2,                       !- Nominal Speed Number",
        "      Furnace DX Cool Cooling Coil Speed 1 Performance,  !- Speed 1 Name",
        "      Furnace DX Cool Cooling Coil Speed 2 Performance;  !- Speed 2 Name",

        "    Coil:Cooling:DX:CurveFit:Speed,",
        "      Furnace DX Cool Cooling Coil Speed 1 Performance,  !- Name",
        "      0.5000,                  !- Gross Total Cooling Capacity Fraction",
        "      0.5000,                  !- Evaporator Air Flow Rate Fraction",
        "      ,                        !- Condenser Air Flow Rate Fraction",
        "      autosize,                !- Gross Sensible Heat Ratio",
        "      3,                       !- Gross Cooling COP {W/W}",
        "      1.0,                     !- Active Fraction of Coil Face Area",
        "      ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "      ,                        !- Evaporative Condenser Pump Power Fraction",
        "      ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
        "      Furnace DX Cool Cool Coil Cap-FT,  !- Total Cooling Capacity Modifier Function of Temperature Curve Name",
        "      Furnace DX Cool Cool Coil Cap-FF,  !- Total Cooling Capacity Modifier Function of Air Flow Fraction Curve Name",
        "      Furnace DX Cool Cool Coil EIR-FT,  !- Energy Input Ratio Modifier Function of Temperature Curve Name",
        "      Furnace DX Cool Cool Coil EIR-FF,  !- Energy Input Ratio Modifier Function of Air Flow Fraction Curve Name",
        "      Furnace DX Cool Cool Coil PLF,  !- Part Load Fraction Correlation Curve Name",
        "      0.2,                     !- Rated Waste Heat Fraction of Power Input {dimensionless}",
        "      Furnace DX Cool Cool Coil WH-FT,  !- Waste Heat Modifier Function of Temperature Curve Name",
        "      ,                        !- Sensible Heat Ratio Modifier Function of Temperature Curve Name",
        "      ;                        !- Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name",

        "    Coil:Cooling:DX:CurveFit:Speed,",
        "      Furnace DX Cool Cooling Coil Speed 2 Performance,  !- Name",
        "      1.0000,                  !- Gross Total Cooling Capacity Fraction",
        "      1.0000,                  !- Evaporator Air Flow Rate Fraction",
        "      ,                        !- Condenser Air Flow Rate Fraction",
        "      autosize,                !- Gross Sensible Heat Ratio",
        "      3,                       !- Gross Cooling COP {W/W}",
        "      1.0,                     !- Active Fraction of Coil Face Area",
        "      ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "      ,                        !- Evaporative Condenser Pump Power Fraction",
        "      ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
        "      Furnace DX Cool Cool Coil Cap-FT,  !- Total Cooling Capacity Modifier Function of Temperature Curve Name",
        "      Furnace DX Cool Cool Coil Cap-FF,  !- Total Cooling Capacity Modifier Function of Air Flow Fraction Curve Name",
        "      Furnace DX Cool Cool Coil EIR-FT,  !- Energy Input Ratio Modifier Function of Temperature Curve Name",
        "      Furnace DX Cool Cool Coil EIR-FF,  !- Energy Input Ratio Modifier Function of Air Flow Fraction Curve Name",
        "      Furnace DX Cool Cool Coil PLF,  !- Part Load Fraction Correlation Curve Name",
        "      0.2,                     !- Rated Waste Heat Fraction of Power Input {dimensionless}",
        "      Furnace DX Cool Cool Coil WH-FT,  !- Waste Heat Modifier Function of Temperature Curve Name",
        "      ,                        !- Sensible Heat Ratio Modifier Function of Temperature Curve Name",
        "      ;                        !- Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name",

        "    Curve:Biquadratic,",
        "      Furnace DX Cool Cool Coil Cap-FT,  !- Name",
        "      0.476428E+00,            !- Coefficient1 Constant",
        "      0.401147E-01,            !- Coefficient2 x",
        "      0.226411E-03,            !- Coefficient3 x**2",
        "      -0.827136E-03,           !- Coefficient4 y",
        "      -0.732240E-05,           !- Coefficient5 y**2",
        "      -0.446278E-03,           !- Coefficient6 x*y",
        "      0.0,                     !- Minimum Value of x",
        "      50.0,                    !- Maximum Value of x",
        "      0.0,                     !- Minimum Value of y",
        "      50.0,                    !- Maximum Value of y",
        "      0.0,                     !- Minimum Curve Output",
        "      5.0,                     !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "    Curve:Cubic,",
        "      Furnace DX Cool Cool Coil Cap-FF,  !- Name",
        "      .47278589,               !- Coefficient1 Constant",
        "      1.2433415,               !- Coefficient2 x",
        "      -1.0387055,              !- Coefficient3 x**2",
        "      .32257813,               !- Coefficient4 x**3",
        "      0.5,                     !- Minimum Value of x",
        "      1.5;                     !- Maximum Value of x",

        "    Curve:Biquadratic,",
        "      Furnace DX Cool Cool Coil EIR-FT,  !- Name",
        "      0.632475E+00,            !- Coefficient1 Constant",
        "      -0.121321E-01,           !- Coefficient2 x",
        "      0.507773E-03,            !- Coefficient3 x**2",
        "      0.155377E-01,            !- Coefficient4 y",
        "      0.272840E-03,            !- Coefficient5 y**2",
        "      -0.679201E-03,           !- Coefficient6 x*y",
        "      0.0,                     !- Minimum Value of x",
        "      50.0,                    !- Maximum Value of x",
        "      0.0,                     !- Minimum Value of y",
        "      50.0,                    !- Maximum Value of y",
        "      0.0,                     !- Minimum Curve Output",
        "      5.0,                     !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "    Curve:Cubic,",
        "      Furnace DX Cool Cool Coil EIR-FF,  !- Name",
        "      .47278589,               !- Coefficient1 Constant",
        "      1.2433415,               !- Coefficient2 x",
        "      -1.0387055,              !- Coefficient3 x**2",
        "      .32257813,               !- Coefficient4 x**3",
        "      0.5,                     !- Minimum Value of x",
        "      1.5;                     !- Maximum Value of x",

        "    Curve:Quadratic,",
        "      Furnace DX Cool Cool Coil PLF,  !- Name",
        "      1.00,                    !- Coefficient1 Constant",
        "      0.00,                    !- Coefficient2 x",
        "      0,                       !- Coefficient3 x**2",
        "      0,                       !- Minimum Value of x",
        "      1;                       !- Maximum Value of x",

        "    Curve:Biquadratic,",
        "      Furnace DX Cool Cool Coil WH-FT,  !- Name",
        "      1.0,                     !- Coefficient1 Constant",
        "      0.0,                     !- Coefficient2 x",
        "      0.0,                     !- Coefficient3 x**2",
        "      0.0,                     !- Coefficient4 y",
        "      0.0,                     !- Coefficient5 y**2",
        "      0.0,                     !- Coefficient6 x*y",
        "      0,                       !- Minimum Value of x",
        "      50,                      !- Maximum Value of x",
        "      0,                       !- Minimum Value of y",
        "      50,                      !- Maximum Value of y",
        "      ,                        !- Minimum Curve Output",
        "      ,                        !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "    Coil:Heating:Fuel,",
        "      Furnace DX Cool Heating Coil,  !- Name",
        "      ,                        !- Availability Schedule Name",
        "      NaturalGas,              !- Fuel Type",
        "      0.8,                     !- Burner Efficiency",
        "      autosize,                !- Nominal Capacity {W}",
        "      Furnace DX Cool Cooling Coil Outlet,  !- Air Inlet Node Name",
        "      East Zone Inlet Node,    !- Air Outlet Node Name",
        "      ,                        !- Temperature Setpoint Node Name",
        "      0,                       !- Parasitic Electric Load {W}",
        "      Furnace DX Cool Heating Coil PLF-FPLR,  !- Part Load Fraction Correlation Curve Name",
        "      0;                       !- Parasitic Fuel Load {W}",

        "    Curve:Cubic,",
        "      Furnace DX Cool Heating Coil PLF-FPLR,  !- Name",
        "      0.8,                     !- Coefficient1 Constant",
        "      0.2,                     !- Coefficient2 x",
        "      0,                       !- Coefficient3 x**2",
        "      0,                       !- Coefficient4 x**3",
        "      0,                       !- Minimum Value of x",
        "      1;                       !- Maximum Value of x",

        "    Fan:OnOff,",
        "      Furnace DX Cool Supply Fan,  !- Name",
        "      ,                               !- Availability Schedule Name",
        "      0.7,                            !- Fan Total Efficiency",
        "      600.0,                          !- Pressure Rise{ Pa }",
        "      1.5,                            !- Maximum Flow Rate{ m3 / s }",
        "      0.9,                            !- Motor Efficiency",
        "      1.0,                            !- Motor In Airstream Fraction",
        "      Zone Exhaust Node,              !- Air Inlet Node Name",
        "      Furnace DX Cool Supply Fan Outlet;  !- Air Outlet Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors
                               // Verify UnitarySystem air flow rates are read in as AutoSized
    EXPECT_EQ(thisSys->m_MaxCoolAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxHeatAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_MaxNoCoolHeatAirVolFlow, DataSizing::AutoSize);
    EXPECT_EQ(thisSys->m_NoLoadAirFlowRateRatio, 1.0);

    OutputReportPredefined::SetPredefinedTables();
    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;
    int AirLoopNum = 0;

    thisSys->sizeSystem(state, FirstHVACIteration, AirLoopNum);
    EXPECT_EQ(1.500, thisSys->m_DesignFanVolFlowRate);
    EXPECT_EQ(1.500, thisSys->m_MaxCoolAirVolFlow);
    EXPECT_EQ(1.500, thisSys->m_MaxHeatAirVolFlow);

    int MSHPIndex = thisSys->m_DesignSpecMSHPIndex;
    Real64 results_noLoadHeatingFlowRatio = 1.0 / double(designSpecMSHP[MSHPIndex].numOfSpeedCooling);
    Real64 results_noLoadCoolingFlowRatio = 1.0 / double(designSpecMSHP[MSHPIndex].numOfSpeedHeating);
    Real64 results_noLoadFlowRatioMin = min(results_noLoadHeatingFlowRatio, results_noLoadCoolingFlowRatio);

    EXPECT_NEAR(results_noLoadFlowRatioMin, thisSys->m_NoLoadAirFlowRateRatio, 0.00001);
    EXPECT_NEAR(0.50, thisSys->m_NoLoadAirFlowRateRatio, 0.00001);
    EXPECT_NEAR(0.75, thisSys->m_MaxNoCoolHeatAirVolFlow, 0.00001);
    EXPECT_NEAR(0.75, thisSys->m_MaxCoolAirVolFlow * thisSys->m_NoLoadAirFlowRateRatio, 0.00001);
}

TEST_F(ZoneUnitarySysTest, UnitarySystemModel_MultiSpeedDXCoilsDirectSolutionTest)
{

    std::string const idf_objects = delimited_string({

        "    AirLoopHVAC:UnitarySystem,",
        "      UNITARY SYSTEM MODEL,    !- Name",
        "      Load,                    !- Control Type",
        "      East Zone,               !- Controlling Zone or Thermostat Location",
        "      None,                    !- Dehumidification Control Type",
        "      ,                        !- Availability Schedule Name",
        "      Zone Exhaust Node,       !- Air Inlet Node Name",
        "      East Zone Inlet Node,    !- Air Outlet Node Name",
        "      Fan:OnOff,               !- Supply Fan Object Type",
        "      Furnace DX Cool Supply Fan,  !- Supply Fan Name",
        "      BlowThrough,             !- Fan Placement",
        "      ,                        !- Supply Air Fan Operating Mode Schedule Name",
        "      Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "      Furnace DX Cool Heating Coil,  !- Heating Coil Name",
        "      1.0,                     !- DX Heating Coil Sizing Ratio",
        "      Coil:Cooling:DX,         !- Cooling Coil Object Type",
        "      Furnace DX Cool Cooling Coil,  !- Cooling Coil Name",
        "      ,                        !- Use DOAS DX Cooling Coil",
        "      ,                        !- Minimum Supply Air Temperature {C}",
        "      ,                        !- Latent Load Control",
        "      ,                        !- Supplemental Heating Coil Object Type",
        "      ,                        !- Supplemental Heating Coil Name",
        "      SupplyAirFlowRate,       !- Cooling Supply Air Flow Rate Method",
        "      autosize,                !- Cooling Supply Air Flow Rate {m3/s}",
        "      ,                        !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "      ,                        !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "      ,                        !- Cooling Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "      SupplyAirFlowRate,       !- Heating Supply Air Flow Rate Method",
        "      autosize,                !- Heating Supply Air Flow Rate {m3/s}",
        "      ,                        !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "      ,                        !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "      ,                        !- Heating Supply Air Flow Rate Per Unit of Capacity {m3/s-W}",
        "      SupplyAirFlowRate,       !- No Load Supply Air Flow Rate Method",
        "      autosize,                !- No Load Supply Air Flow Rate {m3/s}",
        "      ,                        !- No Load Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "      ,                        !- No Load Fraction of Autosized Cooling Supply Air Flow Rate",
        "      ,                        !- No Load Fraction of Autosized Heating Supply Air Flow Rate",
        "      ,                        !- No Load Supply Air Flow Rate Per Unit of Capacity During Cooling Operation {m3/s-W}",
        "      ,                        !- No Load Supply Air Flow Rate Per Unit of Capacity During Heating Operation {m3/s-W}",
        "      Autosize,                !- Maximum Supply Air Temperature {C}",
        "      21,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "      ,                        !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "      ,                        !- Maximum Cycling Rate {cycles/hr}",
        "      ,                        !- Heat Pump Time Constant {s}",
        "      ,                        !- Fraction of On-Cycle Power Use",
        "      ,                        !- Heat Pump Fan Delay Time {s}",
        "      ,                        !- Ancillary On-Cycle Electric Power {W}",
        "      ,                        !- Ancillary Off-Cycle Electric Power {W}",
        "      ,                        !- Design Heat Recovery Water Flow Rate {m3/s}",
        "      ,                        !- Maximum Temperature for Heat Recovery {C}",
        "      ,                        !- Heat Recovery Water Inlet Node Name",
        "      ,                        !- Heat Recovery Water Outlet Node Name",
        "      UnitarySystemPerformance:Multispeed,  !- Design Specification Multispeed Object Type",
        "      Furnace DX Cool Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "    UnitarySystemPerformance:Multispeed,",
        "      Furnace DX Cool Unitary System MultiSpeed Performance,  !- Name",
        "      1,                       !- Number of Speeds for Heating",
        "      2,                       !- Number of Speeds for Cooling",
        "      No,                      !- Single Mode Operation",
        "      ,                        !- No Load Supply Air Flow Rate Ratio",
        "      autosize,                !- Heating Speed 1 Supply Air Flow Ratio",
        "      autosize,                !- Cooling Speed 1 Supply Air Flow Ratio",
        "      autosize,                !- Heating Speed 2 Supply Air Flow Ratio",
        "      autosize;                !- Cooling Speed 2 Supply Air Flow Ratio",

        "    Coil:Cooling:DX,",
        "      Furnace DX Cool Cooling Coil,  !- Name",
        "      Furnace DX Cool Supply Fan Outlet,  !- Evaporator Inlet Node Name",
        "      Furnace DX Cool Cooling Coil Outlet,  !- Evaporator Outlet Node Name",
        "      ,                        !- Availability Schedule Name",
        "      ,                        !- Condenser Zone Name",
        "      Furnace DX Cool Cooling Coil Condenser Inlet,  !- Condenser Inlet Node Name",
        "      Furnace DX Cool Cooling Coil Condenser Outlet Node,  !- Condenser Outlet Node Name",
        "      Furnace DX Cool Cooling Coil Performance,  !- Performance Object Name",
        "      ,                        !- Condensate Collection Water Storage Tank Name",
        "      ;                        !- Evaporative Condenser Supply Water Storage Tank Name",

        "    Coil:Cooling:DX:CurveFit:Performance,",
        "      Furnace DX Cool Cooling Coil Performance,  !- Name",
        "      0.0,                     !- Crankcase Heater Capacity {W}",
        "      ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "      10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "      ,                        !- Unit Internal Static Air Pressure {Pa}",
        "      Discrete,                !- Capacity Control Method",
        "      ,                        !- Evaporative Condenser Basin Heater Capacity {W/K}",
        "      ,                        !- Evaporative Condenser Basin Heater Setpoint Temperature {C}",
        "      ,                        !- Evaporative Condenser Basin Heater Operating Schedule Name",
        "      Electricity,             !- Compressor Fuel Type",
        "      Furnace DX Cool Cooling Coil Operating Mode;  !- Base Operating Mode",

        "    Coil:Cooling:DX:CurveFit:OperatingMode,",
        "      Furnace DX Cool Cooling Coil Operating Mode,  !- Name",
        "      autosize,                !- Rated Gross Total Cooling Capacity {W}",
        "      autosize,                !- Rated Evaporator Air Flow Rate {m3/s}",
        "      ,                        !- Rated Condenser Air Flow Rate {m3/s}",
        "      0,                       !- Maximum Cycling Rate {cycles/hr}",
        "      0,                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "      0,                       !- Latent Capacity Time Constant {s}",
        "      0,                       !- Nominal Time for Condensate Removal to Begin {s}",
        "      No,                      !- Apply Latent Degradation to Speeds Greater than 1",
        "      AirCooled,               !- Condenser Type",
        "      ,                        !- Nominal Evaporative Condenser Pump Power {W}",
        "      2,                       !- Nominal Speed Number",
        "      Furnace DX Cool Cooling Coil Speed 1 Performance,  !- Speed 1 Name",
        "      Furnace DX Cool Cooling Coil Speed 2 Performance;  !- Speed 2 Name",

        "    Coil:Cooling:DX:CurveFit:Speed,",
        "      Furnace DX Cool Cooling Coil Speed 1 Performance,  !- Name",
        "      0.5000,                  !- Gross Total Cooling Capacity Fraction",
        "      0.5000,                  !- Evaporator Air Flow Rate Fraction",
        "      ,                        !- Condenser Air Flow Rate Fraction",
        "      autosize,                !- Gross Sensible Heat Ratio",
        "      3,                       !- Gross Cooling COP {W/W}",
        "      1.0,                     !- Active Fraction of Coil Face Area",
        "      ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "      ,                        !- Evaporative Condenser Pump Power Fraction",
        "      ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
        "      Furnace DX Cool Cool Coil Cap-FT,  !- Total Cooling Capacity Modifier Function of Temperature Curve Name",
        "      Furnace DX Cool Cool Coil Cap-FF,  !- Total Cooling Capacity Modifier Function of Air Flow Fraction Curve Name",
        "      Furnace DX Cool Cool Coil EIR-FT,  !- Energy Input Ratio Modifier Function of Temperature Curve Name",
        "      Furnace DX Cool Cool Coil EIR-FF,  !- Energy Input Ratio Modifier Function of Air Flow Fraction Curve Name",
        "      Furnace DX Cool Cool Coil PLF,  !- Part Load Fraction Correlation Curve Name",
        "      0.2,                     !- Rated Waste Heat Fraction of Power Input {dimensionless}",
        "      Furnace DX Cool Cool Coil WH-FT,  !- Waste Heat Modifier Function of Temperature Curve Name",
        "      ,                        !- Sensible Heat Ratio Modifier Function of Temperature Curve Name",
        "      ;                        !- Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name",

        "    Coil:Cooling:DX:CurveFit:Speed,",
        "      Furnace DX Cool Cooling Coil Speed 2 Performance,  !- Name",
        "      1.0000,                  !- Gross Total Cooling Capacity Fraction",
        "      1.0000,                  !- Evaporator Air Flow Rate Fraction",
        "      ,                        !- Condenser Air Flow Rate Fraction",
        "      autosize,                !- Gross Sensible Heat Ratio",
        "      3,                       !- Gross Cooling COP {W/W}",
        "      1.0,                     !- Active Fraction of Coil Face Area",
        "      ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "      ,                        !- Evaporative Condenser Pump Power Fraction",
        "      ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
        "      Furnace DX Cool Cool Coil Cap-FT,  !- Total Cooling Capacity Modifier Function of Temperature Curve Name",
        "      Furnace DX Cool Cool Coil Cap-FF,  !- Total Cooling Capacity Modifier Function of Air Flow Fraction Curve Name",
        "      Furnace DX Cool Cool Coil EIR-FT,  !- Energy Input Ratio Modifier Function of Temperature Curve Name",
        "      Furnace DX Cool Cool Coil EIR-FF,  !- Energy Input Ratio Modifier Function of Air Flow Fraction Curve Name",
        "      Furnace DX Cool Cool Coil PLF,  !- Part Load Fraction Correlation Curve Name",
        "      0.2,                     !- Rated Waste Heat Fraction of Power Input {dimensionless}",
        "      Furnace DX Cool Cool Coil WH-FT,  !- Waste Heat Modifier Function of Temperature Curve Name",
        "      ,                        !- Sensible Heat Ratio Modifier Function of Temperature Curve Name",
        "      ;                        !- Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name",

        "    Curve:Biquadratic,",
        "      Furnace DX Cool Cool Coil Cap-FT,  !- Name",
        "      0.476428E+00,            !- Coefficient1 Constant",
        "      0.401147E-01,            !- Coefficient2 x",
        "      0.226411E-03,            !- Coefficient3 x**2",
        "      -0.827136E-03,           !- Coefficient4 y",
        "      -0.732240E-05,           !- Coefficient5 y**2",
        "      -0.446278E-03,           !- Coefficient6 x*y",
        "      0.0,                     !- Minimum Value of x",
        "      50.0,                    !- Maximum Value of x",
        "      0.0,                     !- Minimum Value of y",
        "      50.0,                    !- Maximum Value of y",
        "      0.0,                     !- Minimum Curve Output",
        "      5.0,                     !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "    Curve:Cubic,",
        "      Furnace DX Cool Cool Coil Cap-FF,  !- Name",
        "      .47278589,               !- Coefficient1 Constant",
        "      1.2433415,               !- Coefficient2 x",
        "      -1.0387055,              !- Coefficient3 x**2",
        "      .32257813,               !- Coefficient4 x**3",
        "      0.5,                     !- Minimum Value of x",
        "      1.5;                     !- Maximum Value of x",

        "    Curve:Biquadratic,",
        "      Furnace DX Cool Cool Coil EIR-FT,  !- Name",
        "      0.632475E+00,            !- Coefficient1 Constant",
        "      -0.121321E-01,           !- Coefficient2 x",
        "      0.507773E-03,            !- Coefficient3 x**2",
        "      0.155377E-01,            !- Coefficient4 y",
        "      0.272840E-03,            !- Coefficient5 y**2",
        "      -0.679201E-03,           !- Coefficient6 x*y",
        "      0.0,                     !- Minimum Value of x",
        "      50.0,                    !- Maximum Value of x",
        "      0.0,                     !- Minimum Value of y",
        "      50.0,                    !- Maximum Value of y",
        "      0.0,                     !- Minimum Curve Output",
        "      5.0,                     !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "    Curve:Cubic,",
        "      Furnace DX Cool Cool Coil EIR-FF,  !- Name",
        "      .47278589,               !- Coefficient1 Constant",
        "      1.2433415,               !- Coefficient2 x",
        "      -1.0387055,              !- Coefficient3 x**2",
        "      .32257813,               !- Coefficient4 x**3",
        "      0.5,                     !- Minimum Value of x",
        "      1.5;                     !- Maximum Value of x",

        "    Curve:Quadratic,",
        "      Furnace DX Cool Cool Coil PLF,  !- Name",
        "      1.00,                    !- Coefficient1 Constant",
        "      0.00,                    !- Coefficient2 x",
        "      0,                       !- Coefficient3 x**2",
        "      0,                       !- Minimum Value of x",
        "      1;                       !- Maximum Value of x",

        "    Curve:Biquadratic,",
        "      Furnace DX Cool Cool Coil WH-FT,  !- Name",
        "      1.0,                     !- Coefficient1 Constant",
        "      0.0,                     !- Coefficient2 x",
        "      0.0,                     !- Coefficient3 x**2",
        "      0.0,                     !- Coefficient4 y",
        "      0.0,                     !- Coefficient5 y**2",
        "      0.0,                     !- Coefficient6 x*y",
        "      0,                       !- Minimum Value of x",
        "      50,                      !- Maximum Value of x",
        "      0,                       !- Minimum Value of y",
        "      50,                      !- Maximum Value of y",
        "      ,                        !- Minimum Curve Output",
        "      ,                        !- Maximum Curve Output",
        "      Temperature,             !- Input Unit Type for X",
        "      Temperature,             !- Input Unit Type for Y",
        "      Dimensionless;           !- Output Unit Type",

        "    Coil:Heating:Fuel,",
        "      Furnace DX Cool Heating Coil,  !- Name",
        "      ,                        !- Availability Schedule Name",
        "      NaturalGas,              !- Fuel Type",
        "      0.8,                     !- Burner Efficiency",
        "      autosize,                !- Nominal Capacity {W}",
        "      Furnace DX Cool Cooling Coil Outlet,  !- Air Inlet Node Name",
        "      East Zone Inlet Node,    !- Air Outlet Node Name",
        "      ,                        !- Temperature Setpoint Node Name",
        "      0,                       !- Parasitic Electric Load {W}",
        "      Furnace DX Cool Heating Coil PLF-FPLR,  !- Part Load Fraction Correlation Curve Name",
        "      0;                       !- Parasitic Fuel Load {W}",

        "    Curve:Cubic,",
        "      Furnace DX Cool Heating Coil PLF-FPLR,  !- Name",
        "      0.8,                     !- Coefficient1 Constant",
        "      0.2,                     !- Coefficient2 x",
        "      0,                       !- Coefficient3 x**2",
        "      0,                       !- Coefficient4 x**3",
        "      0,                       !- Minimum Value of x",
        "      1;                       !- Maximum Value of x",

        "    Fan:OnOff,",
        "      Furnace DX Cool Supply Fan,  !- Name",
        "      ,                               !- Availability Schedule Name",
        "      0.7,                            !- Fan Total Efficiency",
        "      600.0,                          !- Pressure Rise{ Pa }",
        "      1.5,                            !- Maximum Flow Rate{ m3 / s }",
        "      0.9,                            !- Motor Efficiency",
        "      1.0,                            !- Motor In Airstream Fraction",
        "      Zone Exhaust Node,              !- Air Inlet Node Name",
        "      Furnace DX Cool Supply Fan Outlet;  !- Air Outlet Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    bool ErrorsFound = false;

    std::string compName = "UNITARY SYSTEM MODEL";
    bool zoneEquipment = true;
    bool FirstHVACIteration = true;
    UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    UnitarySystems::UnitarySys *thisSys = &UnitarySystems::unitarySys[0];

    DataZoneEquipment::ZoneEquipInputsFilled = true;                                    // indicate zone data is available
    thisSys->getUnitarySystemInputData(state, compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above
    EXPECT_FALSE(ErrorsFound);                                                          // expect no errors

    FirstHVACIteration = false;
    DataGlobals::BeginEnvrnFlag = false;
    int AirLoopNum = 0;

    thisSys->sizeSystem(state, FirstHVACIteration, AirLoopNum);

    // Test direct solution
     DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
     DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
     DataLoopNode::Node.redimension(7);
     DataLoopNode::Node(1).Temp = 24.0;      // 24C db
     DataLoopNode::Node(1).HumRat = 0.01522; // 17C wb
     DataLoopNode::Node(1).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(1).Temp, DataLoopNode::Node(1).HumRat);
     DataHeatBalFanSys::ZoneAirHumRat.allocate(1);
     DataHeatBalFanSys::MAT.allocate(1);
     DataHeatBalFanSys::ZoneAirHumRat(1) = DataLoopNode::Node(1).HumRat;
     DataHeatBalFanSys::MAT(1) = DataLoopNode::Node(1).Temp;
     DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1;
     DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);

     DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
     DataHeatBalFanSys::TempControlType.allocate(1);
     DataHeatBalFanSys::TempControlType(1) = 4;
     DataLoopNode::Node(7).FluidType = 1;
     DataLoopNode::Node(7).Temp = 24.0;                               // 24C db
     DataLoopNode::Node(7).HumRat = 0.01522;                          // 17C wb
     DataLoopNode::Node(7).Enthalpy = DataLoopNode::Node(1).Enthalpy; // www.sugartech.com/psychro/index.php
     thisSys->NodeNumOfControlledZone = 7;
     DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -227.705;
     DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -227.705;
     DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -50.0;
     DataZoneEnergyDemands::ZoneSysMoistureDemand(1).RemainingOutputReqToDehumidSP = -0.007806893;
     DataEnvironment::StdRhoAir = 1.2043;
     DataLoopNode::Node(3).MassFlowRateMax = 1.5 * DataEnvironment::StdRhoAir;
     int CompIndex = 1;
     bool HeatActive = false;
     bool CoolActive = true;
     int const ZoneOAUnitNum = 0;
     Real64 const OAUCoilOutTemp = 0.0;
     bool const ZoneEquipment = true;
     Real64 sensOut = 0.0;
     Real64 latOut = 0.0;

     //Speed 1
     thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
     EXPECT_NEAR(thisSys->m_CycRatio, 0.02422, 0.001);
     EXPECT_NEAR(sensOut, -227.705, 0.1);
     DataGlobals::DoCoilDirectSolutions = true;
     thisSys->FullOutput.resize(3);
     thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
     EXPECT_NEAR(thisSys->m_CycRatio, 0.02422, 0.001);
     EXPECT_NEAR(sensOut, -227.705, 0.1);
    // speed 2
     DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -12000.0;
     DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -12000.0;
     DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -5000.0;
     DataGlobals::DoCoilDirectSolutions = false;
     thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
     EXPECT_NEAR(thisSys->m_CycRatio, 1.000, 0.001);
     EXPECT_NEAR(thisSys->m_SpeedRatio, 0.228062, 0.001);
     EXPECT_NEAR(sensOut, -11998.0, 3.0);

     DataGlobals::DoCoilDirectSolutions = true;
     thisSys->simulate(state,
                      thisSys->Name,
                      FirstHVACIteration,
                      AirLoopNum,
                      CompIndex,
                      HeatActive,
                      CoolActive,
                      ZoneOAUnitNum,
                      OAUCoilOutTemp,
                      ZoneEquipment,
                      sensOut,
                      latOut);
     EXPECT_NEAR(thisSys->m_CycRatio, 1.000, 0.001);
     EXPECT_NEAR(thisSys->m_SpeedRatio, 0.228062, 0.02);
     EXPECT_NEAR(sensOut, -11998.0, 210.0);
}
