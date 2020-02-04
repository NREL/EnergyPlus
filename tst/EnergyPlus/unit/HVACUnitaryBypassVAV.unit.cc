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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HVACUnitaryBypassVAV.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus;

class CBVAVSys : public EnergyPlusFixture
{
public:
    int cbvavNum = 1;
    bool FirstHVACIteration = true;
    int AirLoopNum = 1;
    Real64 OnOffAirFlowRatio = 1.0;
    bool HXUnitOn = true;
    int NumNodes = 1; // number of zone inlet and zone exhaust nodes
    bool ErrorsFound = false;

protected:
    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up the base fixture first.

        DataGlobals::DayOfSim = 1;
        DataGlobals::HourOfDay = 1;

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
        DataZoneEquipment::ZoneEquipConfig(1).EquipListName = "ZONEEQUIPMENT";
        DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = 20;
        DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 1;
        DataZoneEquipment::ZoneEquipConfig(1).ReturnNode.allocate(1);
        DataZoneEquipment::ZoneEquipConfig(1).ReturnNode(1) = 21;
        DataZoneEquipment::ZoneEquipConfig(1).FixedReturnFlow.allocate(1);
        DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum).SystemZoneNodeNumber =
            DataZoneEquipment::ZoneEquipConfig(1).ZoneNode;
        DataZoneEquipment::ZoneEquipConfig(1).ReturnFlowSchedPtrNum = DataGlobals::ScheduleAlwaysOn;
        DataZoneEquipment::ZoneEquipList(1).Name = "ZONEEQUIPMENT";
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
        DataZoneEquipment::ZoneEquipList(1).EquipType(1) = "ZONEHVAC:AIRDISTRIBUTIONUNIT";
        DataZoneEquipment::ZoneEquipList(1).EquipName(1) = "ZONEREHEATTU";
        DataZoneEquipment::ZoneEquipList(1).CoolingPriority(1) = 1;
        DataZoneEquipment::ZoneEquipList(1).HeatingPriority(1) = 1;
        DataZoneEquipment::ZoneEquipList(1).EquipType_Num(1) = DataZoneEquipment::AirDistUnit_Num;
        DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = NumNodes;
        DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitCool.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitHeat.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 2;
        DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = NumNodes;
        DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = 1;
        DataZoneEquipment::ZoneEquipConfig(1).EquipListIndex = 1;

        DataSizing::CurSysNum = 1;
        DataSizing::CurZoneEqNum = 0;
        DataSizing::CurOASysNum = 0;

        DataSizing::FinalSysSizing.allocate(1);
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesMainVolFlow = 1.5;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesCoolVolFlow = 1.5;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesHeatVolFlow = 1.2;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).DesOutAirVolFlow = 0.3;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).MixTempAtCoolPeak = 25.0;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).MixHumRatAtCoolPeak = 0.009;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).CoolSupTemp = 15.0;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).CoolSupHumRat = 0.006;

        DataSizing::FinalSysSizing(DataSizing::CurSysNum).HeatSupTemp = 35.0;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).HeatRetTemp = 20.0;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).HeatRetHumRat = 0.007;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).HeatOutTemp = 10.0;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).HeatOutHumRat = 0.004;
        DataSizing::FinalSysSizing(DataSizing::CurSysNum).CoolDDNum = 1;
        DataSizing::DesDayWeath.allocate(1);
        DataSizing::DesDayWeath(1).Temp.allocate(1);
        DataSizing::DesDayWeath(1).Temp(1) = 35.0;

        DataSizing::ZoneEqSizing.allocate(1);
        DataSizing::ZoneEqSizing(DataSizing::CurSysNum).SizingMethod.allocate(25);
        DataSizing::ZoneSizingRunDone = true;

        DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
        DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP.allocate(1);
        DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP.allocate(1);
        DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP = 0.0;
        DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP = 0.0;
        DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);
        DataZoneEnergyDemands::CurDeadBandOrSetback = false;
        DataLoopNode::Node.allocate(50);

        // note no fan used for these tests
        HVACUnitaryBypassVAV::NumCBVAV = 1;
        HVACUnitaryBypassVAV::CBVAV.allocate(1);
        auto &cbvav(HVACUnitaryBypassVAV::CBVAV(1));
        cbvav.Name = "CBVAVAirLoop";
        cbvav.UnitType = "AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass";
        cbvav.SchedPtr = -1;
        cbvav.ActualZoneNodeNum.allocate(1);
        cbvav.ActualZoneNodeNum(1) = 1;
        cbvav.DXCoolCoilIndexNum = 1;
        DXCoils::DXCoil.allocate(1);
        DXCoils::DXCoilNumericFields.allocate(1);
        DXCoils::DXCoilNumericFields(1).PerfMode.allocate(1);
        DXCoils::DXCoilNumericFields(1).PerfMode(1).FieldNames.allocate(20);
        DXCoils::DXCoil(1).Name = "MyDXCoolCoil";
        DXCoils::DXCoil(1).DXCoilType = "COIL:COOLING:DX:SINGLESPEED";
        DXCoils::NumDXCoils = 1;
        DXCoils::CheckEquipName.dimension(1, true);
        DXCoils::GetCoilsInputFlag = false;
        DXCoils::DXCoil(1).CCapFFlow.allocate(1);
        DXCoils::DXCoil(1).CCapFFlow(1) = 1;
        DXCoils::DXCoil(1).CCapFTemp.allocate(1);
        DXCoils::DXCoil(1).CCapFTemp(1) = 1;
        DXCoils::DXCoil(1).EIRFFlow.allocate(1);
        DXCoils::DXCoil(1).EIRFFlow(1) = 1;
        DXCoils::DXCoil(1).EIRFTemp.allocate(1);
        DXCoils::DXCoil(1).EIRFTemp(1) = 1;
        DXCoils::DXCoil(1).PLFFPLR.allocate(1);
        DXCoils::DXCoil(1).PLFFPLR(1) = 1;
        DXCoils::DXCoilFullLoadOutAirTemp.allocate(1);
        DXCoils::DXCoilFullLoadOutAirHumRat.allocate(1);
        DXCoils::DXCoil(1).RatedAirVolFlowRate.allocate(1);
        DXCoils::DXCoil(1).RatedAirVolFlowRate(1) = 0.5;
        DXCoils::DXCoil(1).RatedTotCap.allocate(1);
        DXCoils::DXCoil(1).RatedTotCap(1) = 10000.0;
        DXCoils::DXCoil(1).RatedCOP(1) = 3.3333;
        DXCoils::DXCoil(1).RatedEIR.allocate(1);
        DXCoils::DXCoil(1).RatedEIR(1) = 0.3;
        DXCoils::DXCoil(1).RatedSHR.allocate(1);
        DXCoils::DXCoil(1).RatedSHR(1) = 0.7;
        DXCoils::DXCoil(1).SchedPtr = -1;
        DXCoils::DXCoilOutletTemp.allocate(1);
        DXCoils::DXCoilOutletHumRat.allocate(1);
        DXCoils::DXCoilPartLoadRatio.allocate(1);
        DXCoils::DXCoilFanOpMode.allocate(1);
        DataHeatBalance::HeatReclaimDXCoil.allocate(1);

        cbvav.DXCoolCoilName = "MyDXCoolCoil";
        DXCoils::DXCoil(1).DXCoilType_Num = DataHVACGlobals::CoilDX_CoolingSingleSpeed;
        HeatingCoils::HeatingCoil.allocate(1);
        HeatingCoils::HeatingCoilNumericFields.allocate(1);
        HeatingCoils::HeatingCoilNumericFields(1).FieldNames.allocate(20);
        HeatingCoils::HeatingCoil(1).Name = "MyHeatingCoil";
        HeatingCoils::HeatingCoil(1).HCoilType_Num = DataHVACGlobals::Coil_HeatingElectric;
        HeatingCoils::NumHeatingCoils = 1;
        HeatingCoils::ValidSourceType.dimension(HeatingCoils::NumHeatingCoils, false);
        HeatingCoils::GetCoilsInputFlag = false;
        DataSizing::UnitarySysEqSizing.allocate(1);
        cbvav.HeatCoilName = "MyHeatingCoil";
        cbvav.DXCoolCoilType_Num = DataHVACGlobals::CoilDX_CoolingSingleSpeed;
        cbvav.HeatCoilType_Num = DataHVACGlobals::Coil_HeatingElectric;
        cbvav.minModeChangeTime = 0.0;
        cbvav.AirInNode = 1;
        cbvav.AirOutNode = 2;
        cbvav.MixerOutsideAirNode = 3;
        cbvav.MixerReliefAirNode = 4;
        cbvav.MixerMixedAirNode = 5;
        cbvav.MixerInletAirNode = 6;
        cbvav.HeatingCoilOutletNode = 9;
        cbvav.SplitterOutletAirNode = 9;
        cbvav.NumControlledZones = 1;
        cbvav.ControlledZoneNum.allocate(1);
        cbvav.ControlledZoneNum = 1;
        cbvav.MinLATCooling = 7.0;
        cbvav.MaxLATHeating = 40.0;
        cbvav.ZoneSequenceCoolingNum.allocate(1);
        cbvav.ZoneSequenceHeatingNum.allocate(1);
        cbvav.ZoneSequenceCoolingNum = 1;
        cbvav.ZoneSequenceHeatingNum = 1;
        cbvav.OAMixName = "MyOAMixer";
        MixedAir::OAMixer.allocate(1);
        MixedAir::OAMixer(1).Name = "MyOAMixer";
        MixedAir::OAMixer(1).InletNode = 3;
        MixedAir::OAMixer(1).RelNode = 4;
        MixedAir::OAMixer(1).RetNode = 6;
        MixedAir::OAMixer(1).MixNode = 7;
        DXCoils::DXCoil(1).AirInNode = 7;
        cbvav.DXCoilInletNode = DXCoils::DXCoil(1).AirInNode;
        DXCoils::DXCoil(1).AirOutNode = 8;
        cbvav.DXCoilOutletNode = DXCoils::DXCoil(1).AirOutNode;
        HeatingCoils::HeatingCoil(1).AirInletNodeNum = 8;
        cbvav.HeatingCoilInletNode = HeatingCoils::HeatingCoil(1).AirInletNodeNum;
        HeatingCoils::HeatingCoil(1).AirOutletNodeNum = 9;
        HeatingCoils::HeatingCoil(1).TempSetPointNodeNum = 9;
        cbvav.HeatingCoilOutletNode = HeatingCoils::HeatingCoil(1).AirOutletNodeNum;
        HeatingCoils::HeatingCoil(1).NominalCapacity = 10000.0;
        HeatingCoils::HeatingCoil(1).Efficiency = 1.0;
        HeatingCoils::HeatingCoil(1).SchedPtr = -1;

        cbvav.CBVAVBoxOutletNode.allocate(1);
        cbvav.CBVAVBoxOutletNode(1) = 11;

        CurveManager::PerfCurve.allocate(1);
        CurveManager::NumCurves = 1;
        CurveManager::PerfCurve(1).InterpolationType = CurveManager::EvaluateCurveToLimits;
        CurveManager::PerfCurve(1).CurveType = CurveManager::Linear;
        CurveManager::PerfCurve(1).Coeff1 = 1.0;

        DataEnvironment::OutDryBulbTemp = 35.0;
        DataEnvironment::OutHumRat = 0.0141066;
        DataEnvironment::OutWetBulbTemp = 23.9;
        DataEnvironment::OutBaroPress = 101325.0;

        DataAirLoop::AirLoopFlow.allocate(1);
        DataAirSystems::PrimaryAirSystem.allocate(1);
        DataAirLoop::AirLoopControlInfo.allocate(1);
        OutputReportPredefined::SetPredefinedTables();
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!
    }
};

TEST_F(EnergyPlusFixture, UnitaryBypassVAV_GetInputZoneEquipment)
{

    std::string const idf_objects = delimited_string({

        "Zone,",
        "  Zone 2;                                 !- Name",
        "Zone,",
        "  Zone 1;                                 !- Name",

        "BuildingSurface:Detailed,",
        "  Surface_1,               !- Name",
        "  WALL,                    !- Surface Type",
        "  EXTWALL80,               !- Construction Name",
        "  Zone 1,                  !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  SunExposed,              !- Sun Exposure",
        "  WindExposed,             !- Wind Exposure",
        "  0.5000000,               !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
        "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
        "  0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
        "  0,0,3.048000;            !- X,Y,Z ==> Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        "  Surface_2,               !- Name",
        "  WALL,                    !- Surface Type",
        "  EXTWALL80,               !- Construction Name",
        "  Zone 2,                  !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  SunExposed,              !- Sun Exposure",
        "  WindExposed,             !- Wind Exposure",
        "  0.5000000,               !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
        "  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
        "  0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
        "  0,0,3.048000;            !- X,Y,Z ==> Vertex 4 {m}",
        "Construction,",
        "  EXTWALL80,               !- Name",
        "  C10 - 8 IN HW CONCRETE;        !- Outside Layer",

        "Material,",
        "  C10 - 8 IN HW CONCRETE,  !- Name",
        "  MediumRough,             !- Roughness",
        "  0.2033016,               !- Thickness {m}",
        "  1.729577,                !- Conductivity {W/m-K}",
        "  2242.585,                !- Density {kg/m3}",
        "  836.8000,                !- Specific Heat {J/kg-K}",
        "  0.9000000,               !- Thermal Absorptance",
        "  0.6500000,               !- Solar Absorptance",
        "  0.6500000;               !- Visible Absorptance",

        "  ZoneControl:Thermostat,",
        "    Zone Thermostat,         !- Name",
        "    Zone 1,           !- Zone or ZoneList Name",
        "    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    Setpoints;               !- Control 1 Name",

        "  ThermostatSetpoint:DualSetpoint,",
        "    Setpoints,               !- Name",
        "    Dual Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name",
        "    Dual Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name",

        "  Schedule:Compact,",
        "    Dual Heating Setpoints,  !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    23.0;                    !- Field 4",

        "  Schedule:Compact,",
        "    Dual Cooling Setpoints,  !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    23.0;                    !- Field 4",

        "  Schedule:Compact,",
        "    Dual Zone Control Type Sched,  !- Name",
        "    Control Type,            !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,            !- Field 3",
        "    4;                       !- Field 4",

        "ZoneHVAC:EquipmentConnections,",
        "  Zone 1,                                 !- Zone Name",
        "  Zone 1 Equipment List,                  !- Zone Conditioning Equipment List Name",
        "  Zone 1 Inlet Node List,                 !- Zone Air Inlet Node or NodeList Name",
        "  ,                                       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 1 Zone Air Node,                   !- Zone Air Node Name",
        "  Zone 1 Zone Return Air Node;            !- Zone Return Air Node or NodeList Name",

        "NodeList,",
        "  Zone 1 Inlet Node List,                 !- Name",
        "  Zone 1 Dummy Inlet Node,",
        "  Zone 1 ATU VAVHeatAndCoolNoReheat Outlet Node; !- Node Name 1",

        "ZoneHVAC:AirDistributionUnit,",
        "  ADU Zone 1 ATU VAVHeatAndCoolNoReheat,  !- Name",
        "  Zone 1 ATU VAVHeatAndCoolNoReheat Outlet Node, !- Air Distribution Unit Outlet Node Name",
        "  AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat, !- Air Terminal Object Type",
        "  Zone 1 ATU VAVHeatAndCoolNoReheat;      !- Air Terminal Name",

        "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat,",
        "  Zone 1 ATU VAVHeatAndCoolNoReheat,      !- Name",
        "  ,                                       !- Availability Schedule Name",
        "  Zone 1 ATU VAVHeatAndCoolNoReheat Outlet Node, !- Air Outlet Node Name",
        "  Zone 1 ATU VAVHeatAndCoolNoReheat Inlet Node, !- Air Inlet Node Name",
        "  0.02,                               !- Maximum Air Flow Rate {m3/s}",
        "  0;                                      !- Zone Minimum Air Flow Fraction",

        "ZoneHVAC:EquipmentList,",
        "  Zone 1 Equipment List,                  !- Name",
        "  ,                                       !- Load Distribution Scheme",
        "  ZoneHVAC:AirDistributionUnit,           !- Zone Equipment Object Type 1",
        "  ADU Zone 1 ATU VAVHeatAndCoolNoReheat,  !- Zone Equipment Name 1",
        "  1,                                      !- Zone Equipment Cooling Sequence 1",
        "  1;                                      !- Zone Equipment Heating or No-Load Sequence 1",

        "OutdoorAir:Node,",
        "  Model Outdoor Air Node;                 !- Name",

        "AirLoopHVAC,",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop, !- Name",
        "  ,                                       !- Controller List Name",
        "  , !- Availability Manager List Name",
        "  0.02,                               !- Design Supply Air Flow Rate {m3/s}",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Branches, !- Branch List Name",
        "  ,                                       !- Connector List Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Inlet Node, !- Supply Side Inlet Node Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Outlet Node, !- Demand Side Outlet Node Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Nodes, !- Demand Side Inlet Node Names",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Nodes; !- Supply Side Outlet Node Names",

        "NodeList,",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Nodes, !- Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Node; !- Node Name 1",

        "NodeList,",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Nodes, !- Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Node; !- Node Name 1",

        "BranchList,",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Branches, !- Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Main Branch; !- Branch Name 1",

        "Branch,",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Main Branch, !- Name",
        "  ,                                       !- Pressure Drop Curve Name",
        "  AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass, !- Component Object Type 1",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1, !- Component Name 1",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Inlet Node, !- Component Inlet Node Name 1",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Node; !- Component Outlet Node Name 1",

        "AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass,",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1, !- Name",
        "  ,                                       !- Availability Schedule Name",
        "  0.021,                               !- Cooling Supply Air Flow Rate {m3/s}",
        "  0.022,                               !- Heating Supply Air Flow Rate {m3/s}",
        "  0.023,                               !- No Load Supply Air Flow Rate {m3/s}",
        "  0.011,                               !- Cooling Outdoor Air Flow Rate {m3/s}",
        "  0.012,                               !- Heating Outdoor Air Flow Rate {m3/s}",
        "  0.013,                               !- No Load Outdoor Air Flow Rate {m3/s}",
        "  ,                                       !- Outdoor Air Flow Rate Multiplier Schedule Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Inlet Node, !- Air Inlet Node Name",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Bypass Duct Mixer Node, !- Bypass Duct Mixer Node Name",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Bypass Duct Splitter Node, !- Bypass Duct Splitter Node Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Node, !- Air Outlet Node Name",
        "  OutdoorAir:Mixer,                       !- Outdoor Air Mixer Object Type",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Outdoor Air Mixer, !- Outdoor Air Mixer Name",
        "  Fan:ConstantVolume,                     !- Supply Air Fan Object Type",
        "  Fan Constant Volume 1,                  !- Supply Air Fan Name",
        "  DrawThrough,                            !- Supply Air Fan Placement",
        "  ,                                       !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Cooling:DX:SingleSpeed,            !- Cooling Coil Object Type",
        "  Coil Cooling DX Single Speed 1,         !- Cooling Coil Name",
        "  Coil:Heating:Fuel,                      !- Heating Coil Object Type",
        "  Coil Heating Gas 1,                     !- Heating Coil Name",
        "  ZonePriority,                           !- Priority Control Mode",
        "  8,                                      !- Minimum Outlet Air Temperature During Cooling Operation {C}",
        "  50,                                     !- Maximum Outlet Air Temperature During Heating Operation {C}",
        "  None;                                   !- Dehumidification Control Type",

        "Fan:ConstantVolume,",
        "  Fan Constant Volume 1,                  !- Name",
        "  ,                                       !- Availability Schedule Name",
        "  0.7,                                    !- Fan Total Efficiency",
        "  250,                                    !- Pressure Rise {Pa}",
        "  0.023,                               !- Maximum Flow Rate {m3/s}",
        "  0.9,                                    !- Motor Efficiency",
        "  1,                                      !- Motor In Airstream Fraction",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Heating Coil Outlet Node, !- Air Inlet Node Name",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Bypass Duct Splitter Node; !- Air Outlet Node Name",

        "Coil:Heating:Fuel,",
        "  Coil Heating Gas 1,                     !- Name",
        "  ,                                       !- Availability Schedule Name",
        "  NaturalGas,                             !- Fuel Type",
        "  0.8,                                    !- Burner Efficiency",
        "  1000.0,                               !- Nominal Capacity {W}",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Cooling Coil Outlet Node, !- Air Inlet Node Name",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Heating Coil Outlet Node, !- Air Outlet Node Name",
        "  ,                                       !- Temperature Setpoint Node Name",
        "  0,                                      !- Parasitic Electric Load {W}",
        "  ,                                       !- Part Load Fraction Correlation Curve Name",
        "  0;                                      !- Parasitic Fuel Load {W}",

        "Curve:Biquadratic,",
        "  Curve Biquadratic 1,                    !- Name",
        "  0.942587793,                            !- Coefficient1 Constant",
        "  0.009543347,                            !- Coefficient2 x",
        "  0.00068377,                             !- Coefficient3 x**2",
        "  -0.011042676,                           !- Coefficient4 y",
        "  5.249e-06,                              !- Coefficient5 y**2",
        "  -9.72e-06,                              !- Coefficient6 x*y",
        "  17,                                     !- Minimum Value of x {BasedOnField A2}",
        "  22,                                     !- Maximum Value of x {BasedOnField A2}",
        "  13,                                     !- Minimum Value of y {BasedOnField A3}",
        "  46;                                     !- Maximum Value of y {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Curve Quadratic 1,                      !- Name",
        "  0.8,                                    !- Coefficient1 Constant",
        "  0.2,                                    !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  0.5,                                    !- Minimum Value of x {BasedOnField A2}",
        "  1.5;                                    !- Maximum Value of x {BasedOnField A2}",

        "Curve:Biquadratic,",
        "  Curve Biquadratic 2,                    !- Name",
        "  0.342414409,                            !- Coefficient1 Constant",
        "  0.034885008,                            !- Coefficient2 x",
        "  -0.0006237,                             !- Coefficient3 x**2",
        "  0.004977216,                            !- Coefficient4 y",
        "  0.000437951,                            !- Coefficient5 y**2",
        "  -0.000728028,                           !- Coefficient6 x*y",
        "  17,                                     !- Minimum Value of x {BasedOnField A2}",
        "  22,                                     !- Maximum Value of x {BasedOnField A2}",
        "  13,                                     !- Minimum Value of y {BasedOnField A3}",
        "  46;                                     !- Maximum Value of y {BasedOnField A3}",

        "Curve:Quadratic,",
        "  Curve Quadratic 2,                      !- Name",
        "  1.1552,                                 !- Coefficient1 Constant",
        "  -0.1808,                                !- Coefficient2 x",
        "  0.0256,                                 !- Coefficient3 x**2",
        "  0.5,                                    !- Minimum Value of x {BasedOnField A2}",
        "  1.5;                                    !- Maximum Value of x {BasedOnField A2}",

        "Curve:Quadratic,",
        "  Curve Quadratic 3,                      !- Name",
        "  0.85,                                   !- Coefficient1 Constant",
        "  0.15,                                   !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  1;                                      !- Maximum Value of x {BasedOnField A2}",

        "Coil:Cooling:DX:SingleSpeed,",
        "  Coil Cooling DX Single Speed 1,         !- Name",
        "  ,                                       !- Availability Schedule Name",
        "  1000,                               !- Gross Rated Total Cooling Capacity {W}",
        "  0.7,                               !- Gross Rated Sensible Heat Ratio",
        "  3,                                      !- Gross Rated Cooling COP {W/W}",
        "  0.021,                               !- Rated Air Flow Rate {m3/s}",
        "  773.3,                                  !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Mixed Air Node, !- Air Inlet Node Name",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Cooling Coil Outlet Node, !- Air Outlet Node Name",
        "  Curve Biquadratic 1,                    !- Total Cooling Capacity Function of Temperature Curve Name",
        "  Curve Quadratic 1,                      !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Curve Biquadratic 2,                    !- Energy Input Ratio Function of Temperature Curve Name",
        "  Curve Quadratic 2,                      !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Curve Quadratic 3,                      !- Part Load Fraction Correlation Curve Name",
        "  ,                                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                                       !- Nominal Time for Condensate Removal to Begin {s}",
        "  ,                                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "  ,                                       !- Maximum Cycling Rate {cycles/hr}",
        "  ,                                       !- Latent Capacity Time Constant {s}",
        "  ,                                       !- Condenser Air Inlet Node Name",
        "  EvaporativelyCooled,                    !- Condenser Type",
        "  0,                                      !- Evaporative Condenser Effectiveness {dimensionless}",
        "  Autosize,                               !- Evaporative Condenser Air Flow Rate {m3/s}",
        "  Autosize,                               !- Evaporative Condenser Pump Rated Power Consumption {W}",
        "  0,                                      !- Crankcase Heater Capacity {W}",
        "  0,                                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  ,                                       !- Supply Water Storage Tank Name",
        "  ,                                       !- Condensate Collection Water Storage Tank Name",
        "  0,                                      !- Basin Heater Capacity {W/K}",
        "  10;                                     !- Basin Heater Setpoint Temperature {C}",

        "OutdoorAir:Mixer,",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Outdoor Air Mixer, !- Name",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Mixed Air Node, !- Mixed Air Node Name",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 OA Node, !- Outdoor Air Stream Node Name",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Relief Air Node, !- Relief Air Stream Node Name",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Bypass Duct Mixer Node; !- Return Air Stream Node Name",

        "OutdoorAir:NodeList,",
        "  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 OA Node; !- Node or NodeList Name 1",

        "AirLoopHVAC:SupplyPath,",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Node Supply Path, !- Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Node, !- Supply Air Path Inlet Node Name",
        "  AirLoopHVAC:ZoneSplitter,               !- Component Object Type 1",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Splitter; !- Component Name 1",

        "AirLoopHVAC:ZoneSplitter,",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Splitter, !- Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Node, !- Inlet Node Name",
        "  Zone 1 ATU VAVHeatAndCoolNoReheat Inlet Node; !- Outlet Node Name 1",

        "AirLoopHVAC:ReturnPath,",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Return Path, !- Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Outlet Node, !- Return Air Path Outlet Node Name",
        "  AirLoopHVAC:ZoneMixer,                  !- Component Object Type 1",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Mixer; !- Component Name 1",

        "AirLoopHVAC:ZoneMixer,",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Mixer, !- Name",
        "  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Outlet Node, !- Outlet Node Name",
        "  Zone 1 Zone Return Air Node;            !- Inlet Node Name 1",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    bool ErrorsFound = false;
    bool firstHVACIteration = true;
    // Read objects
    SimulationManager::GetProjectData(OutputFiles::getSingleton());
    HeatBalanceManager::GetProjectControlData(OutputFiles::getSingleton(), ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(OutputFiles::getSingleton(), ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetHeatBalanceInput();
    HeatBalanceManager::AllocateHeatBalArrays();
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    HeatBalanceManager::AllocateHeatBalArrays();
    ZoneTempPredictorCorrector::InitZoneAirSetPoints();
    bool simZone = false;
    bool simAir = false;
    DataHeatBalance::MassConservation.allocate(DataGlobals::NumOfZones);
    ZoneEquipmentManager::ManageZoneEquipment(firstHVACIteration, simZone, simAir);
    SimAirServingZones::GetAirPathData();
    SplitterComponent::GetSplitterInput();
    SimAirServingZones::InitAirLoops(firstHVACIteration);

    // set up zone load indicators
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(2);
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = true;
    DataZoneEnergyDemands::CurDeadBandOrSetback(2) = false;
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(2);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(2).SequencedOutputRequiredToCoolingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(2).SequencedOutputRequiredToHeatingSP.allocate(1);
    // CBVAV serves second zone as indicated by order of Zone objects in input deck, set cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(2).SequencedOutputRequiredToCoolingSP(1) = -1000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(2).SequencedOutputRequiredToHeatingSP(1) = -2000.0;

    HVACUnitaryBypassVAV::GetCBVAV(); // get UnitarySystem input from object above

    int CBVAVNum = 1;
    int zoneIndex = 1;
    auto &cbvav(HVACUnitaryBypassVAV::CBVAV(CBVAVNum));
    // should be the second zone in the zone list as well as actual zone number
    EXPECT_EQ(2, cbvav.ControlledZoneNum(CBVAVNum));
    EXPECT_EQ(2, cbvav.ActualZoneNum(CBVAVNum));
    // reflects sequence number in ZoneHVAC:EquipmentList
    EXPECT_EQ(1, cbvav.ZoneSequenceCoolingNum(cbvav.ZoneSequenceCoolingNum(zoneIndex)));
    EXPECT_EQ(1, cbvav.ZoneSequenceHeatingNum(cbvav.ZoneSequenceHeatingNum(zoneIndex)));
    // test air volume flow inputs
    EXPECT_EQ(0.011, cbvav.CoolOutAirVolFlow);
    EXPECT_EQ(0.012, cbvav.HeatOutAirVolFlow);
    EXPECT_EQ(0.013, cbvav.NoCoolHeatOutAirVolFlow);
    EXPECT_EQ(0.021, cbvav.MaxCoolAirVolFlow);
    EXPECT_EQ(0.022, cbvav.MaxHeatAirVolFlow);
    EXPECT_EQ(0.023, cbvav.MaxNoCoolHeatAirVolFlow);
    EXPECT_EQ(0.023, cbvav.FanVolFlow);

    // set time so that time is greater than CBVAV.changeOverTimer (-1.0) and GetZoneLoads executes
    // First time through GetZoneLoads CBVAV.HeatCoolMode gets set and won't exectute again until the simulation time increases
    // If Init or GetZoneLoads is called again, with a different load (i.e., was cooling and now is heating) then changeOverTimer must be reset
    // if the loads do not change then there is no need to reset the timer, resetting here as an example.
    cbvav.changeOverTimer = -1.0; // reset timer so GetZoneLoads executes
    DataGlobals::DayOfSim = 1;
    DataGlobals::HourOfDay = 1;
    // test zone indexing for loads
    HVACUnitaryBypassVAV::GetZoneLoads(CBVAVNum);
    // only 1 conditioned zone
    EXPECT_EQ(1, cbvav.NumZonesCooled);
    EXPECT_EQ(HVACUnitaryBypassVAV::CoolingMode, cbvav.HeatCoolMode);
}

TEST_F(CBVAVSys, UnitaryBypassVAV_AutoSize)
{

    //  reference CBVAV and FinalSysSizing data
    auto &cbvav(HVACUnitaryBypassVAV::CBVAV(1));
    auto &finalSysSizing(DataSizing::FinalSysSizing(DataSizing::CurSysNum));

    DataSizing::SysSizingRunDone = true; // inform sizing that system sizing run is done
    // override CBVAVSys fixture set up of hard sized inputs to AutoSize
    cbvav.FanVolFlow = DataSizing::AutoSize;
    cbvav.MaxCoolAirVolFlow = DataSizing::AutoSize;
    cbvav.MaxHeatAirVolFlow = DataSizing::AutoSize;
    cbvav.MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
    cbvav.CoolOutAirVolFlow = DataSizing::AutoSize;
    cbvav.HeatOutAirVolFlow = DataSizing::AutoSize;
    cbvav.NoCoolHeatOutAirVolFlow = DataSizing::AutoSize;
    HeatingCoils::HeatingCoil(1).NominalCapacity = DataSizing::AutoSize;
    DXCoils::DXCoil(1).RatedAirVolFlowRate(1) = DataSizing::AutoSize;
    DXCoils::DXCoil(1).RatedTotCap(1) = DataSizing::AutoSize;

    cbvav.OpMode = DataHVACGlobals::CycFanCycCoil;   // must set one type of fan operating mode to initialize CalcSetPointTempTarget
    DataLoopNode::Node(cbvav.AirInNode).Temp = 24.0; // initialize inlet node temp used to initialize CalcSetPointTempTarget

    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);

    // expect CBVAV air flow to match FinalSysSizing info
    EXPECT_EQ(cbvav.MaxCoolAirVolFlow, finalSysSizing.DesMainVolFlow);
    EXPECT_EQ(cbvav.MaxHeatAirVolFlow, finalSysSizing.DesMainVolFlow);
    EXPECT_EQ(cbvav.MaxNoCoolHeatAirVolFlow, finalSysSizing.DesMainVolFlow);

    EXPECT_EQ(cbvav.CoolOutAirVolFlow, finalSysSizing.DesOutAirVolFlow);
    EXPECT_EQ(cbvav.HeatOutAirVolFlow, finalSysSizing.DesOutAirVolFlow);
    EXPECT_EQ(cbvav.NoCoolHeatOutAirVolFlow, finalSysSizing.DesOutAirVolFlow);

    // expect coils to size appropriately based on sizing inputs
    EXPECT_EQ(DXCoils::DXCoil(1).RatedAirVolFlowRate(1), finalSysSizing.DesMainVolFlow);
    EXPECT_GT(DXCoils::DXCoil(1).RatedTotCap(1), 30000.0);
    EXPECT_GT(HeatingCoils::HeatingCoil(1).NominalCapacity, 45000.0);
}

TEST_F(CBVAVSys, UnitaryBypassVAV_NoOASys)
{

    //  reference CBVAV data
    auto &cbvav(HVACUnitaryBypassVAV::CBVAV(1));
    cbvav.FanVolFlow = 0.5;
    cbvav.MaxCoolAirVolFlow = 0.5;
    cbvav.MaxHeatAirVolFlow = 0.5;
    cbvav.MaxNoCoolHeatAirVolFlow = 0.0;
    cbvav.CoolOutAirVolFlow = 0.0;
    cbvav.HeatOutAirVolFlow = 0.0;
    cbvav.NoCoolHeatOutAirVolFlow = 0.0;

    //  initialize node data
    DataLoopNode::Node(cbvav.AirInNode).Temp = 24.0; // sugartech.co.za using 24C db and 17 wb
    DataLoopNode::Node(cbvav.AirInNode).HumRat = 0.009222;
    DataLoopNode::Node(cbvav.AirInNode).Enthalpy = 47591.3;
    DataLoopNode::Node(cbvav.AirInNode).MassFlowRate = 0.57;

    DataLoopNode::Node(MixedAir::OAMixer(1).InletNode).Temp = DataEnvironment::OutDryBulbTemp;
    DataLoopNode::Node(MixedAir::OAMixer(1).InletNode).HumRat = DataEnvironment::OutHumRat;
    DataLoopNode::Node(MixedAir::OAMixer(1).InletNode).Enthalpy = 71299.267;

    DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRateMax = 0.61;
    DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRate = 0.61;

    cbvav.OpMode = DataHVACGlobals::CycFanCycCoil; // set fan operating mode

    // First time through GetZoneLoads CBVAV.HeatCoolMode gets set IF there is a load and won't exectute again until the simulation time increases
    // There is no load here and CBVAV.HeatCoolMode did not change so cbvav.changeOverTimer also did not get set (change) in previous call
    // so there is no need to reset cbvav.changeOverTimer here but it wouldn't hurt if it was reset to -1.0
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(cbvav.HeatCoolMode, 0);
    EXPECT_EQ(cbvav.NumZonesCooled, 0);
    EXPECT_EQ(cbvav.NumZonesHeated, 0);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.AirInNode).Temp, DataLoopNode::Node(cbvav.AirOutNode).Temp, 0.0001);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.AirInNode).HumRat, DataLoopNode::Node(cbvav.AirOutNode).HumRat, 0.000001);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.AirInNode).Enthalpy, DataLoopNode::Node(cbvav.AirOutNode).Enthalpy, 0.1);
    EXPECT_EQ(cbvav.changeOverTimer, -1.0); // expect no change in timer, remains at default value

    // initialize priority control
    cbvav.PriorityControl = HVACUnitaryBypassVAV::CoolingPriority;

    // initialize cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = -9000.0;  // load to cooling set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = -15000.0; // more load to heating set point
    // No need to reset changeOverTimer since previous load was not cooling or heating and changeOverTimer DID NOT get set
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    // Now changeOverTimer = 0 and GetZoneLoads will not execute again unless changeOverTimer is reset, but only needs to occur when load changes
    EXPECT_EQ(cbvav.changeOverTimer, 0.0); // expect timer now set to current time (0.0) plus minModeChangeTime(0.0), so = 0.0
    EXPECT_EQ(cbvav.HeatCoolMode, HVACUnitaryBypassVAV::CoolingMode);
    EXPECT_EQ(cbvav.NumZonesCooled, 1);
    EXPECT_EQ(cbvav.NumZonesHeated, 0);
    EXPECT_GE(cbvav.OutletTempSetPoint, cbvav.MinLATCooling);
    EXPECT_LE(cbvav.OutletTempSetPoint, cbvav.MaxLATHeating);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 9.56, 0.01);

    Real64 PartLoadFrac = 0.0;
    HVACUnitaryBypassVAV::ControlCBVAVOutput(cbvavNum, FirstHVACIteration, PartLoadFrac, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(PartLoadFrac, 1.0); // load = -9000 W, coil capacity = 10,000 W, SHR = 0.7 so max sensible is around 7,000 W

    // reduce load and check that outlet temp meets set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = -7000.0;  // load to cooling set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = -15000.0; // more load to heating set point
    // No need to reset changeOverTimer since load did not change
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    Real64 FullOutput = 0.0;
    HVACUnitaryBypassVAV::CalcCBVAV(cbvavNum, FirstHVACIteration, PartLoadFrac, FullOutput, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_NEAR(PartLoadFrac, 0.935, 0.001); // load = -7000 W, coil capacity = 10,000 W, SHR = 0.7 so max sensible is just over 7,000 W
    EXPECT_NEAR(cbvav.OutletTempSetPoint, DataLoopNode::Node(cbvav.AirOutNode).Temp, 0.0001);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 12.771, 0.001);

    // switch to heating load and check that outlet temp meets set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = 15000.0; // more load to cooling set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = 7000.0;  // load to heating set point
    cbvav.changeOverTimer = -1.0; // The load switched to heating so reset timer so GetZoneLoads executes
    // test timer function by setting minModeChangeTime to 2 hrs
    cbvav.minModeChangeTime = 2.0;
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(cbvav.changeOverTimer, 2.0); // expect timer now set to current time (0.0) plus minModeChangeTime(2.0), so = 2.0
    EXPECT_EQ(cbvav.HeatCoolMode, HVACUnitaryBypassVAV::HeatingMode);
    EXPECT_EQ(cbvav.NumZonesCooled, 0);
    EXPECT_EQ(cbvav.NumZonesHeated, 1);
    EXPECT_GE(cbvav.OutletTempSetPoint, cbvav.MinLATCooling);
    EXPECT_LE(cbvav.OutletTempSetPoint, cbvav.MaxLATHeating);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 35.23, 0.01);
    FullOutput = 0.0;
    HVACUnitaryBypassVAV::CalcCBVAV(cbvavNum, FirstHVACIteration, PartLoadFrac, FullOutput, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_LT(PartLoadFrac, 1.0); // load = 7000 W, coil capacity = 10,000 W
    EXPECT_NEAR(cbvav.OutletTempSetPoint, DataLoopNode::Node(cbvav.AirOutNode).Temp, 0.0001);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 35.228, 0.001);
}

TEST_F(CBVAVSys, UnitaryBypassVAV_InternalOAMixer)
{

    //  reference CBVAV data
    auto &cbvav(HVACUnitaryBypassVAV::CBVAV(1));
    cbvav.FanVolFlow = 0.5;
    cbvav.MaxCoolAirVolFlow = 0.5;
    cbvav.MaxHeatAirVolFlow = 0.5;
    cbvav.MaxNoCoolHeatAirVolFlow = 0.0;
    cbvav.CoolOutAirVolFlow = 0.1;
    cbvav.HeatOutAirVolFlow = 0.1;
    cbvav.NoCoolHeatOutAirVolFlow = 0.1;

    //  initialize node data
    DataLoopNode::Node(cbvav.AirInNode).Temp = 24.0; // sugartech.co.za using 24C db and 17 wb
    DataLoopNode::Node(cbvav.AirInNode).HumRat = 0.009222;
    DataLoopNode::Node(cbvav.AirInNode).Enthalpy = 47591.3;
    DataLoopNode::Node(cbvav.AirInNode).MassFlowRate = 0.57;

    DataLoopNode::Node(MixedAir::OAMixer(1).InletNode).Temp = DataEnvironment::OutDryBulbTemp;
    DataLoopNode::Node(MixedAir::OAMixer(1).InletNode).HumRat = DataEnvironment::OutHumRat;
    DataLoopNode::Node(MixedAir::OAMixer(1).InletNode).Enthalpy = 71299.267;

    DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRateMax = 0.61;
    DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRate = 0.61;

    cbvav.OpMode = DataHVACGlobals::CycFanCycCoil; // set fan operating mode

    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(cbvav.HeatCoolMode, 0);
    EXPECT_EQ(cbvav.NumZonesCooled, 0);
    EXPECT_EQ(cbvav.NumZonesHeated, 0);
    // coil off outlet condition is affected by outdoor air therefore inlet/outlet conditions are different
    EXPECT_NE(DataLoopNode::Node(cbvav.AirInNode).Temp, DataLoopNode::Node(cbvav.AirOutNode).Temp);
    EXPECT_NE(DataLoopNode::Node(cbvav.AirInNode).HumRat, DataLoopNode::Node(cbvav.AirOutNode).HumRat);
    EXPECT_NE(DataLoopNode::Node(cbvav.AirInNode).Enthalpy, DataLoopNode::Node(cbvav.AirOutNode).Enthalpy);

    // initialize priority control
    cbvav.PriorityControl = HVACUnitaryBypassVAV::CoolingPriority;

    // initialize cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = -9000.0;  // load to cooling set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = -15000.0; // more load to heating set point
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(cbvav.HeatCoolMode, HVACUnitaryBypassVAV::CoolingMode);
    EXPECT_EQ(cbvav.NumZonesCooled, 1);
    EXPECT_EQ(cbvav.NumZonesHeated, 0);
    EXPECT_GE(cbvav.OutletTempSetPoint, cbvav.MinLATCooling);
    EXPECT_LE(cbvav.OutletTempSetPoint, cbvav.MaxLATHeating);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 9.59, 0.01);

    Real64 PartLoadFrac = 0.0;
    HVACUnitaryBypassVAV::ControlCBVAVOutput(cbvavNum, FirstHVACIteration, PartLoadFrac, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(PartLoadFrac, 1.0); // load = -9000 W, coil capacity = 10,000 W, SHR = 0.7 so max sensible is around 7,000 W

    // reduce load and check that outlet temp meets set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = -6000.0;  // load to cooling set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = -15000.0; // more load to heating set point
    cbvav.changeOverTimer = -1.0; // reset timer so GetZoneLoads executes
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    Real64 FullOutput = 0.0;
    HVACUnitaryBypassVAV::CalcCBVAV(cbvavNum, FirstHVACIteration, PartLoadFrac, FullOutput, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_LT(PartLoadFrac, 1.0); // load = -6000 W, coil capacity = 10,000 W, SHR = 0.7 so max sensible is around 7,000 W
    EXPECT_NEAR(cbvav.OutletTempSetPoint, DataLoopNode::Node(cbvav.AirOutNode).Temp, 0.0001);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 14.392, 0.001);

    // switch to heating load and check that outlet temp meets set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = 15000.0; // more capacity needed to get to cooling set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = 7000.0;  // load to heating set point
    cbvav.changeOverTimer = -1.0;                                                                  // reset timer so GetZoneLoads executes
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(cbvav.HeatCoolMode, HVACUnitaryBypassVAV::HeatingMode);
    EXPECT_EQ(cbvav.NumZonesCooled, 0);
    EXPECT_EQ(cbvav.NumZonesHeated, 1);
    EXPECT_GE(cbvav.OutletTempSetPoint, cbvav.MinLATCooling);
    EXPECT_LE(cbvav.OutletTempSetPoint, cbvav.MaxLATHeating);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 35.21, 0.01);
    FullOutput = 0.0;
    HVACUnitaryBypassVAV::CalcCBVAV(cbvavNum, FirstHVACIteration, PartLoadFrac, FullOutput, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_LT(PartLoadFrac, 1.0); // load = 7000 W, coil capacity = 10,000 W
    EXPECT_NEAR(cbvav.OutletTempSetPoint, DataLoopNode::Node(cbvav.AirOutNode).Temp, 0.0001);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 35.208, 0.001);
}

TEST_F(CBVAVSys, UnitaryBypassVAV_Mixerconnected)
{

    //  reference CBVAV data
    auto &cbvav(HVACUnitaryBypassVAV::CBVAV(1));
    cbvav.FanVolFlow = 0.5;
    cbvav.MaxCoolAirVolFlow = 0.5;
    cbvav.MaxHeatAirVolFlow = 0.5;
    cbvav.MaxNoCoolHeatAirVolFlow = 0.0;
    cbvav.CoolOutAirVolFlow = 0.0;
    cbvav.HeatOutAirVolFlow = 0.0;
    cbvav.NoCoolHeatOutAirVolFlow = 0.0;
    cbvav.mixerIndex = 1;               // denotes CBVAV is connected to mixer inlet node
    cbvav.PlenumMixerInletAirNode = 10; // mixer inlet node different than splitter outlet air node
    cbvav.AirLoopNumber = 1;

    //  initialize node data
    DataLoopNode::Node(cbvav.AirInNode).Temp = 24.0; // sugartech.co.za using 24C db and 17 wb
    DataLoopNode::Node(cbvav.AirInNode).HumRat = 0.009222;
    DataLoopNode::Node(cbvav.AirInNode).Enthalpy = 47591.3;
    DataLoopNode::Node(cbvav.AirInNode).MassFlowRate = 0.57;

    DataLoopNode::Node(MixedAir::OAMixer(1).InletNode).Temp = DataEnvironment::OutDryBulbTemp;
    DataLoopNode::Node(MixedAir::OAMixer(1).InletNode).HumRat = DataEnvironment::OutHumRat;
    DataLoopNode::Node(MixedAir::OAMixer(1).InletNode).Enthalpy = 71299.267;

    DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRateMax = 0.61;
    DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRate = 0.61;

    cbvav.OpMode = DataHVACGlobals::CycFanCycCoil; // set fan operating mode

    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(cbvav.HeatCoolMode, 0);
    EXPECT_EQ(cbvav.NumZonesCooled, 0);
    EXPECT_EQ(cbvav.NumZonesHeated, 0);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.AirInNode).Temp, DataLoopNode::Node(cbvav.AirOutNode).Temp, 0.0001);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.AirInNode).HumRat, DataLoopNode::Node(cbvav.AirOutNode).HumRat, 0.000001);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.AirInNode).Enthalpy, DataLoopNode::Node(cbvav.AirOutNode).Enthalpy, 0.1);

    // initialize priority control
    cbvav.PriorityControl = HVACUnitaryBypassVAV::CoolingPriority;

    // initialize cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = -9000.0;  // load to cooling set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = -15000.0; // more load to heating set point
    cbvav.changeOverTimer = -1.0; // reset timer so GetZoneLoads executes
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(cbvav.HeatCoolMode, HVACUnitaryBypassVAV::CoolingMode);
    EXPECT_EQ(cbvav.NumZonesCooled, 1);
    EXPECT_EQ(cbvav.NumZonesHeated, 0);
    EXPECT_GE(cbvav.OutletTempSetPoint, cbvav.MinLATCooling);
    EXPECT_LE(cbvav.OutletTempSetPoint, cbvav.MaxLATHeating);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 9.56, 0.01);

    Real64 PartLoadFrac = 0.0;
    HVACUnitaryBypassVAV::ControlCBVAVOutput(cbvavNum, FirstHVACIteration, PartLoadFrac, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(PartLoadFrac, 1.0); // load = -9000 W, coil capacity = 10,000 W, SHR = 0.7 so max sensible is around 7,000 W

    // set FirstHVACIteration = false and use box outlet mass flow rate to set bypass flow
    FirstHVACIteration = false;
    DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRate = 0.3;
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    HVACUnitaryBypassVAV::ControlCBVAVOutput(cbvavNum, FirstHVACIteration, PartLoadFrac, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.AirOutNode).MassFlowRate, DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRate, 0.000001);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 9.56, 0.01);
    EXPECT_NEAR(
        DataAirLoop::AirLoopFlow(cbvav.AirLoopNumber).BypassMassFlow, DataLoopNode::Node(cbvav.PlenumMixerInletAirNode).MassFlowRate, 0.00001);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.PlenumMixerInletAirNode).MassFlowRate, 0.302, 0.001);

    // reduce load and check that outlet temp meets set point
    FirstHVACIteration = true;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = -7000.0; // load to cooling set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = -15000.0; // more load to heating set point
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    Real64 FullOutput = 0.0;
    HVACUnitaryBypassVAV::CalcCBVAV(cbvavNum, FirstHVACIteration, PartLoadFrac, FullOutput, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_LT(PartLoadFrac, 1.0); // load = -7000 W, coil capacity = 10,000 W, SHR = 0.7 so max sensible is around 7,000 W
    EXPECT_NEAR(cbvav.OutletTempSetPoint, DataLoopNode::Node(cbvav.AirOutNode).Temp, 0.0001);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 12.771, 0.001);

    // set FirstHVACIteration = false and use box outlet mass flow rate to set bypass flow
    FirstHVACIteration = false;
    DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRate = 0.3;
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    HVACUnitaryBypassVAV::ControlCBVAVOutput(cbvavNum, FirstHVACIteration, PartLoadFrac, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.AirOutNode).MassFlowRate, DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRate, 0.000001);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 12.771, 0.01);
    EXPECT_NEAR(
        DataAirLoop::AirLoopFlow(cbvav.AirLoopNumber).BypassMassFlow, DataLoopNode::Node(cbvav.PlenumMixerInletAirNode).MassFlowRate, 0.00001);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.PlenumMixerInletAirNode).MassFlowRate, 0.302, 0.001);
    Real64 systemFlow = DataLoopNode::Node(cbvav.PlenumMixerInletAirNode).MassFlowRate + DataLoopNode::Node(cbvav.AirOutNode).MassFlowRate;
    EXPECT_NEAR(systemFlow, cbvav.MaxCoolAirMassFlow, 0.0001);

    // switch to heating load and check that outlet temp meets set point
    FirstHVACIteration = true;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = 15000.0; // more capacity needed to get to cooling set point
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = 7000.0;  // load to heating set point
    cbvav.changeOverTimer = -1.0;                                                                  // reset timer so GetZoneLoads executes
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_EQ(cbvav.HeatCoolMode, HVACUnitaryBypassVAV::HeatingMode);
    EXPECT_EQ(cbvav.NumZonesCooled, 0);
    EXPECT_EQ(cbvav.NumZonesHeated, 1);
    EXPECT_GE(cbvav.OutletTempSetPoint, cbvav.MinLATCooling);
    EXPECT_LE(cbvav.OutletTempSetPoint, cbvav.MaxLATHeating);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 35.23, 0.01);
    FullOutput = 0.0;
    HVACUnitaryBypassVAV::CalcCBVAV(cbvavNum, FirstHVACIteration, PartLoadFrac, FullOutput, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_LT(PartLoadFrac, 1.0); // load = 7000 W, coil capacity = 10,000 W
    EXPECT_NEAR(cbvav.OutletTempSetPoint, DataLoopNode::Node(cbvav.AirOutNode).Temp, 0.0001);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 35.228, 0.001);

    // set FirstHVACIteration = false and use box outlet mass flow rate to set bypass flow
    FirstHVACIteration = false;
    DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRate = 0.2;
    HVACUnitaryBypassVAV::InitCBVAV(cbvavNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, HXUnitOn);
    HVACUnitaryBypassVAV::ControlCBVAVOutput(cbvavNum, FirstHVACIteration, PartLoadFrac, OnOffAirFlowRatio, HXUnitOn);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.AirOutNode).MassFlowRate, DataLoopNode::Node(cbvav.CBVAVBoxOutletNode(1)).MassFlowRate, 0.000001);
    EXPECT_NEAR(cbvav.OutletTempSetPoint, 35.228, 0.01);
    EXPECT_NEAR(
        DataAirLoop::AirLoopFlow(cbvav.AirLoopNumber).BypassMassFlow, DataLoopNode::Node(cbvav.PlenumMixerInletAirNode).MassFlowRate, 0.00001);
    EXPECT_NEAR(DataLoopNode::Node(cbvav.PlenumMixerInletAirNode).MassFlowRate, 0.402, 0.001);
    systemFlow = DataLoopNode::Node(cbvav.PlenumMixerInletAirNode).MassFlowRate + DataLoopNode::Node(cbvav.AirOutNode).MassFlowRate;
    EXPECT_NEAR(systemFlow, cbvav.MaxHeatAirMassFlow, 0.0001);
}
