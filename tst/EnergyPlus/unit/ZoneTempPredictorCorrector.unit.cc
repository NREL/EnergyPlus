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

// EnergyPlus::ZoneTempPredictorCorrector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"


// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace DataGlobals;
using namespace DataStringGlobals;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::ZonePlenum;
using namespace EnergyPlus::ZoneTempPredictorCorrector;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataRoomAirModel;
using namespace EnergyPlus::HybridModel;
using namespace SimulationManager;

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_CorrectZoneHumRatTest)
{

    TimeStepSys = 15.0 / 60.0; // System timestep in hours

    ZoneEquipConfig.allocate(1);
    ZoneEquipConfig(1).ZoneName = "Zone 1";
    ZoneEquipConfig(1).ActualZoneNum = 1;

    ZoneEquipConfig(1).NumInletNodes = 2;
    ZoneEquipConfig(1).InletNode.allocate(2);
    ZoneEquipConfig(1).InletNode(1) = 1;
    ZoneEquipConfig(1).InletNode(2) = 2;
    ZoneEquipConfig(1).NumExhaustNodes = 1;
    ZoneEquipConfig(1).ExhaustNode.allocate(1);
    ZoneEquipConfig(1).ExhaustNode(1) = 3;
    ZoneEquipConfig(1).NumReturnNodes = 1;
    ZoneEquipConfig(1).ReturnNode.allocate(1);
    ZoneEquipConfig(1).ReturnNode(1) = 4;
    ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    Node.allocate(5);

    Zone.allocate(1);
    HybridModelZone.allocate(1);
    Zone(1).Name = ZoneEquipConfig(1).ZoneName;
    Zone(1).ZoneEqNum = 1;
    ZoneEqSizing.allocate(1);
    CurZoneEqNum = 1;
    Zone(1).Multiplier = 1.0;
    Zone(1).Volume = 1000.0;
    Zone(1).SystemZoneNodeNumber = 5;
    Zone(1).ZoneVolCapMultpMoist = 1.0;
    ZoneLatentGain.allocate(1);
    ZoneLatentGain(1) = 0.0;
    SumLatentHTRadSys.allocate(1);
    SumLatentHTRadSys(1) = 0.0;
    SumLatentPool.allocate(1);
    SumLatentPool(1) = 0.0;
    OutBaroPress = 101325.0;
    ZT.allocate(1); // Zone temperature C
    ZT(1) = 24.0;
    ZoneAirHumRat.allocate(1);

    Zone(1).SurfaceFirst = 1;
    Zone(1).SurfaceLast = 2;
    Surface.allocate(2);

    NumZoneReturnPlenums = 0;
    NumZoneSupplyPlenums = 0;

    OAMFL.allocate(1);
    VAMFL.allocate(1);
    EAMFL.allocate(1);
    EAMFLxHumRat.allocate(1);
    CTMFL.allocate(1);

    SumHmARaW.allocate(1);
    SumHmARa.allocate(1);
    MixingMassFlowXHumRat.allocate(1);
    MixingMassFlowZone.allocate(1);
    AirflowNetwork::SimulateAirflowNetwork = 0;
    MDotOA.allocate(1);

    ZoneAirSolutionAlgo = UseEulerMethod;
    ZoneAirHumRatTemp.allocate(1);
    ZoneW1.allocate(1);

    AirModel.allocate(1);
    ZoneIntGain.allocate(1);

    // Case 1 - All flows at the same humrat
    ZoneW1(1) = 0.008;
    Node(1).MassFlowRate = 0.01; // Zone inlet node 1
    Node(1).HumRat = 0.008;
    Node(2).MassFlowRate = 0.02; // Zone inlet node 2
    Node(2).HumRat = 0.008;
    ZoneEquipConfig(1).ZoneExhBalanced = 0.0;
    Node(3).MassFlowRate = 0.00; // Zone exhaust node 1
    ZoneEquipConfig(1).ZoneExh = Node(3).MassFlowRate;
    Node(3).HumRat = ZoneW1(1);
    Node(4).MassFlowRate = 0.03; // Zone return node
    Node(4).HumRat = 0.000;
    Node(5).HumRat = 0.000;
    ZoneAirHumRat(1) = 0.008;
    OAMFL(1) = 0.0;
    VAMFL(1) = 0.0;
    EAMFL(1) = 0.0;
    EAMFLxHumRat(1) = 0.0;
    CTMFL(1) = 0.0;
    OutHumRat = 0.008;
    MixingMassFlowXHumRat(1) = 0.0;
    MixingMassFlowZone(1) = 0.0;
    MDotOA(1) = 0.0;

    // HybridModel
    HybridModelZone(1).PeopleCountCalc_H = false;

    CorrectZoneHumRat(1);
    EXPECT_NEAR(0.008, Node(5).HumRat, 0.00001);

    // Case 2 - Unbalanced exhaust flow
    ZoneW1(1) = 0.008;
    Node(1).MassFlowRate = 0.01; // Zone inlet node 1
    Node(1).HumRat = 0.008;
    Node(2).MassFlowRate = 0.02; // Zone inlet node 2
    Node(2).HumRat = 0.008;
    ZoneEquipConfig(1).ZoneExhBalanced = 0.0;
    Node(3).MassFlowRate = 0.02; // Zone exhaust node 1
    ZoneEquipConfig(1).ZoneExh = Node(3).MassFlowRate;
    Node(3).HumRat = ZoneW1(1);
    Node(4).MassFlowRate = 0.01; // Zone return node
    Node(4).HumRat = ZoneW1(1);
    Node(5).HumRat = 0.000;
    ZoneAirHumRat(1) = 0.008;
    OAMFL(1) = 0.0;
    VAMFL(1) = 0.0;
    EAMFL(1) = 0.0;
    EAMFLxHumRat(1) = 0.0;
    CTMFL(1) = 0.0;
    OutHumRat = 0.004;
    MixingMassFlowXHumRat(1) = 0.0;
    MixingMassFlowZone(1) = 0.0;
    MDotOA(1) = 0.0;

    CorrectZoneHumRat(1);
    EXPECT_NEAR(0.008, Node(5).HumRat, 0.00001);

    // Case 3 - Balanced exhaust flow with proper source flow from mixing
    ZoneW1(1) = 0.008;
    Node(1).MassFlowRate = 0.01; // Zone inlet node 1
    Node(1).HumRat = 0.008;
    Node(2).MassFlowRate = 0.02; // Zone inlet node 2
    Node(2).HumRat = 0.008;
    ZoneEquipConfig(1).ZoneExhBalanced = 0.02;
    Node(3).MassFlowRate = 0.02; // Zone exhaust node 1
    ZoneEquipConfig(1).ZoneExh = Node(3).MassFlowRate;
    Node(3).HumRat = ZoneW1(1);
    Node(4).MassFlowRate = 0.03; // Zone return node
    Node(4).HumRat = ZoneW1(1);
    Node(5).HumRat = 0.000;
    ZoneAirHumRat(1) = 0.008;
    OAMFL(1) = 0.0;
    VAMFL(1) = 0.0;
    EAMFL(1) = 0.0;
    EAMFLxHumRat(1) = 0.0;
    CTMFL(1) = 0.0;
    OutHumRat = 0.004;
    MixingMassFlowXHumRat(1) = 0.02 * 0.008;
    MixingMassFlowZone(1) = 0.02;
    MDotOA(1) = 0.0;

    CorrectZoneHumRat(1);
    EXPECT_NEAR(0.008, Node(5).HumRat, 0.00001);

    // Case 4 - Balanced exhaust flow without source flow from mixing
    ZoneW1(1) = 0.008;
    Node(1).MassFlowRate = 0.01; // Zone inlet node 1
    Node(1).HumRat = 0.008;
    Node(2).MassFlowRate = 0.02; // Zone inlet node 2
    Node(2).HumRat = 0.008;
    ZoneEquipConfig(1).ZoneExhBalanced = 0.02;
    Node(3).MassFlowRate = 0.02; // Zone exhaust node 1
    ZoneEquipConfig(1).ZoneExh = Node(3).MassFlowRate;
    Node(3).HumRat = ZoneW1(1);
    Node(4).MassFlowRate = 0.01; // Zone return node
    Node(4).HumRat = ZoneW1(1);
    Node(5).HumRat = 0.000;
    ZoneAirHumRat(1) = 0.008;
    OAMFL(1) = 0.0;
    VAMFL(1) = 0.0;
    EAMFL(1) = 0.0;
    EAMFLxHumRat(1) = 0.0;
    CTMFL(1) = 0.0;
    OutHumRat = 0.004;
    MixingMassFlowXHumRat(1) = 0.0;
    MixingMassFlowZone(1) = 0.0;
    MDotOA(1) = 0.0;

    CorrectZoneHumRat(1);
    EXPECT_NEAR(0.008, Node(5).HumRat, 0.00001);

    // Add a section to check #6119 by L. Gu on 5/16/17
    CorrectZoneHumRat(1);
    EXPECT_NEAR(0.008, Node(5).HumRat, 0.00001);

    // Issue 6233
    Zone(1).IsControlled = true;
    CorrectZoneHumRat(1);
    EXPECT_NEAR(0.008, Node(5).HumRat, 0.00001);
}

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_ReportingTest)
{
    // AUTHOR: R. Raustad, FSEC
    // DATE WRITTEN: Aug 2015

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Core_top,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_top Thermostat,     !- Name",
        "  Core_top,                !- Zone or ZoneList Name",
        "  Single Heating Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleHeating,  !- Control 1 Object Type",
        "  Core_top HeatSPSched;    !- Control 1 Name",
        " ",
        "Schedule:Compact,",
        "  Single Heating Control Type Sched,  !- Name",
        "  Control Type,            !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,1;          !- Field 3",
        " ",
        "ThermostatSetpoint:SingleHeating,",
        "  Core_top HeatSPSched,    !- Name",
        "  SNGL_HTGSETP_SCH;        !- Heating Setpoint Temperature Schedule Name",
        " ",
        "Schedule:Compact,",
        "  SNGL_HTGSETP_SCH,        !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,15.0;       !- Field 3",
        " ",
        "Zone,",
        "  Core_middle,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_middle Thermostat,  !- Name",
        "  Core_middle,             !- Zone or ZoneList Name",
        "  Single Cooling Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleCooling,  !- Control 1 Object Type",
        "  Core_middle CoolSPSched; !- Control 1 Name",
        " ",
        "Schedule:Compact,",
        "  Single Cooling Control Type Sched,  !- Name",
        "  Control Type,            !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,2;          !- Field 3",
        " ",
        "ThermostatSetpoint:SingleCooling,",
        "  Core_middle CoolSPSched, !- Name",
        "  SNGL_CLGSETP_SCH;        !- Cooling Setpoint Temperature Schedule Name",
        " ",
        "Schedule:Compact,",
        "  SNGL_CLGSETP_SCH,        !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,24.0;       !- Field 3",
        " ",
        "Zone,",
        "  Core_basement,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_basement Thermostat,  !- Name",
        "  Core_basement,             !- Zone or ZoneList Name",
        "  Single Cooling Heating Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleHeatingOrCooling,  !- Control 1 Object Type",
        "  Core_basement CoolHeatSPSched; !- Control 1 Name",
        " ",
        "Schedule:Compact,",
        "  Single Cooling Heating Control Type Sched,  !- Name",
        "  Control Type,            !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,3;          !- Field 3",
        " ",
        "ThermostatSetpoint:SingleHeatingOrCooling,",
        "  Core_basement CoolHeatSPSched, !- Name",
        "  CLGHTGSETP_SCH;             !- Heating Setpoint Temperature Schedule Name",
        " ",
        "Zone,",
        "  Core_bottom,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_bottom Thermostat,  !- Name",
        "  Core_bottom,             !- Zone or ZoneList Name",
        "  Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "  Core_bottom DualSPSched; !- Control 1 Name",
        " ",
        "Schedule:Compact,",
        "  Dual Zone Control Type Sched,  !- Name",
        "  Control Type,            !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,4;          !- Field 3",
        " ",
        "ThermostatSetpoint:DualSetpoint,",
        "  Core_bottom DualSPSched, !- Name",
        "  HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
        "  CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",
        " ",
        "Schedule:Compact,",
        "  CLGSETP_SCH,             !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,24.0;       !- Field 3",
        " ",
        "Schedule:Compact,",
        "  HTGSETP_SCH,             !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,15.0;       !- Field 3",
        " ",
        "Schedule:Compact,",
        "  CLGHTGSETP_SCH,          !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,24.0;       !- Field 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input
    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    int HeatZoneNum(1);
    int CoolZoneNum(2);
    int CoolHeatZoneNum(3);
    int DualZoneNum(4);

    NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
    MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
    ProcessScheduleInput(OutputFiles::getSingleton());  // read schedules

    GetZoneAirSetPoints(OutputFiles::getSingleton());

    DeadBandOrSetback.allocate(NumTempControlledZones);
    CurDeadBandOrSetback.allocate(NumTempControlledZones);
    TempControlType.allocate(NumTempControlledZones);
    ZoneSysEnergyDemand.allocate(NumTempControlledZones);
    TempZoneThermostatSetPoint.allocate(NumTempControlledZones);
    ZoneSetPointLast.allocate(NumTempControlledZones);
    Setback.allocate(NumTempControlledZones);
    ZoneThermostatSetPointLo.allocate(NumTempControlledZones);
    ZoneThermostatSetPointHi.allocate(NumTempControlledZones);
    TempDepZnLd.allocate(NumTempControlledZones);
    TempIndZnLd.allocate(NumTempControlledZones);
    TempDepZnLd = 0.0;
    TempIndZnLd = 0.0;

    SNLoadPredictedRate.allocate(NumTempControlledZones);
    LoadCorrectionFactor.allocate(NumTempControlledZones);
    SNLoadPredictedHSPRate.allocate(NumTempControlledZones);
    SNLoadPredictedCSPRate.allocate(NumTempControlledZones);

    LoadCorrectionFactor(HeatZoneNum) = 1.0;
    LoadCorrectionFactor(CoolZoneNum) = 1.0;
    LoadCorrectionFactor(CoolHeatZoneNum) = 1.0;
    LoadCorrectionFactor(DualZoneNum) = 1.0;

    // The following parameters describe the setpoint types in TempControlType(ActualZoneNum)
    //	extern int const SingleHeatingSetPoint; = 1
    //	extern int const SingleCoolingSetPoint; = 2
    //	extern int const SingleHeatCoolSetPoint; = 3
    //	extern int const DualSetPointWithDeadBand; = 4
    Schedule(TempControlledZone(HeatZoneNum).CTSchedIndex).CurrentValue = DataHVACGlobals::SingleHeatingSetPoint;
    Schedule(TempControlledZone(CoolZoneNum).CTSchedIndex).CurrentValue = DataHVACGlobals::SingleCoolingSetPoint;
    Schedule(TempControlledZone(CoolHeatZoneNum).CTSchedIndex).CurrentValue = DataHVACGlobals::SingleHeatCoolSetPoint;

    Schedule(TempControlledZone(DualZoneNum).CTSchedIndex).CurrentValue = 0; // simulate no thermostat or non-controlled zone

    ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired = 0.0; // no load and no thermostat since control type is set to 0 above
    CalcZoneAirTempSetPoints();
    CalcPredictedSystemLoad(DualZoneNum, 1.0);

    EXPECT_EQ(0.0, TempZoneThermostatSetPoint(DualZoneNum)); // Set point initialized to 0 and never set since thermostat control type = 0

    Schedule(TempControlledZone(DualZoneNum).CTSchedIndex).CurrentValue =
        DataHVACGlobals::DualSetPointWithDeadBand; // reset Tstat control schedule to dual thermostat control

    // set up a back calculated load
    // for the first few, TempIndZnLd() = 0.0
    // LoadToHeatingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
    // LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
    int SetPointTempSchedIndex =
        SetPointSingleHeating(TempControlledZone(HeatZoneNum).ControlTypeSchIndx(TempControlledZone(HeatZoneNum).SchIndx_SingleHeatSetPoint))
            .TempSchedIndex;
    Schedule(SetPointTempSchedIndex).CurrentValue = 20.0;
    ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired = -1000.0; // cooling load
    TempDepZnLd(HeatZoneNum) = ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired / Schedule(SetPointTempSchedIndex).CurrentValue;

    CalcZoneAirTempSetPoints();
    CalcPredictedSystemLoad(HeatZoneNum, 1.0);

    EXPECT_EQ(20.0, TempZoneThermostatSetPoint(HeatZoneNum));
    EXPECT_EQ(-1000.0,
              ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load
    EXPECT_TRUE(CurDeadBandOrSetback(HeatZoneNum));                  // Tstat should show there is no load on a single heating SP

    SetPointTempSchedIndex =
        SetPointSingleHeating(TempControlledZone(HeatZoneNum).ControlTypeSchIndx(TempControlledZone(HeatZoneNum).SchIndx_SingleHeatSetPoint))
            .TempSchedIndex;
    Schedule(SetPointTempSchedIndex).CurrentValue = 21.0;
    ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired = 1000.0; // heating load
    TempDepZnLd(HeatZoneNum) = ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired / Schedule(SetPointTempSchedIndex).CurrentValue;

    SetPointTempSchedIndex =
        SetPointSingleCooling(TempControlledZone(CoolZoneNum).ControlTypeSchIndx(TempControlledZone(CoolZoneNum).SchIndx_SingleCoolSetPoint))
            .TempSchedIndex;
    Schedule(SetPointTempSchedIndex).CurrentValue = 23.0;
    ZoneSysEnergyDemand(CoolZoneNum).TotalOutputRequired = -3000.0; // cooling load
    TempDepZnLd(CoolZoneNum) = ZoneSysEnergyDemand(CoolZoneNum).TotalOutputRequired / Schedule(SetPointTempSchedIndex).CurrentValue;

    SetPointTempSchedIndex =
        SetPointSingleHeatCool(
            TempControlledZone(CoolHeatZoneNum).ControlTypeSchIndx(TempControlledZone(CoolHeatZoneNum).SchIndx_SingleHeatCoolSetPoint))
            .TempSchedIndex;
    Schedule(SetPointTempSchedIndex).CurrentValue = 22.0;
    ZoneSysEnergyDemand(CoolHeatZoneNum).TotalOutputRequired = -4000.0; // cooling load
    TempDepZnLd(CoolHeatZoneNum) = ZoneSysEnergyDemand(CoolHeatZoneNum).TotalOutputRequired / Schedule(SetPointTempSchedIndex).CurrentValue;

    SetPointTempSchedIndex =
        SetPointDualHeatCool(TempControlledZone(DualZoneNum).ControlTypeSchIndx(TempControlledZone(DualZoneNum).SchIndx_DualSetPointWDeadBand))
            .CoolTempSchedIndex;
    Schedule(SetPointTempSchedIndex).CurrentValue = 24.0;
    SetPointTempSchedIndex =
        SetPointDualHeatCool(TempControlledZone(DualZoneNum).ControlTypeSchIndx(TempControlledZone(DualZoneNum).SchIndx_DualSetPointWDeadBand))
            .HeatTempSchedIndex;
    Schedule(SetPointTempSchedIndex).CurrentValue = 20.0;
    ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired = 2500.0; // heating load
    TempDepZnLd(DualZoneNum) = ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired / Schedule(SetPointTempSchedIndex).CurrentValue;

    CalcZoneAirTempSetPoints();
    CalcPredictedSystemLoad(HeatZoneNum, 1.0);

    EXPECT_EQ(21.0, TempZoneThermostatSetPoint(HeatZoneNum));
    EXPECT_FALSE(CurDeadBandOrSetback(HeatZoneNum)); // Tstat should show there is load on a single heating SP
    EXPECT_EQ(1000.0,
              ZoneSysEnergyDemand(HeatZoneNum).TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

    CalcPredictedSystemLoad(CoolZoneNum, 1.0);

    EXPECT_EQ(23.0, TempZoneThermostatSetPoint(CoolZoneNum));
    EXPECT_FALSE(CurDeadBandOrSetback(CoolZoneNum)); // Tstat should show there is load on a single cooling SP
    EXPECT_EQ(-3000.0,
              ZoneSysEnergyDemand(CoolZoneNum).TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

    CalcPredictedSystemLoad(CoolHeatZoneNum, 1.0);

    ASSERT_EQ(22.0, TempZoneThermostatSetPoint(CoolHeatZoneNum));
    EXPECT_FALSE(CurDeadBandOrSetback(CoolHeatZoneNum)); // Tstat should show there is load on a single heating or cooling SP
    EXPECT_EQ(
        -4000.0,
        ZoneSysEnergyDemand(CoolHeatZoneNum).TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

    CalcPredictedSystemLoad(DualZoneNum, 1.0);

    EXPECT_EQ(20.0, TempZoneThermostatSetPoint(DualZoneNum));
    EXPECT_FALSE(CurDeadBandOrSetback(DualZoneNum)); // Tstat should show there is load on a dual SP
    EXPECT_EQ(2500.0,
              ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

    SetPointTempSchedIndex =
        SetPointDualHeatCool(TempControlledZone(DualZoneNum).ControlTypeSchIndx(TempControlledZone(DualZoneNum).SchIndx_DualSetPointWDeadBand))
            .CoolTempSchedIndex;
    Schedule(SetPointTempSchedIndex).CurrentValue = 25.0;
    ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired = 1000.0;
    // LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
    TempDepZnLd(DualZoneNum) = ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired / Schedule(SetPointTempSchedIndex).CurrentValue;
    TempIndZnLd(DualZoneNum) = 3500.0; // results in a cooling load

    CalcZoneAirTempSetPoints();
    CalcPredictedSystemLoad(DualZoneNum, 1.0);

    EXPECT_EQ(25.0, TempZoneThermostatSetPoint(DualZoneNum));
    EXPECT_FALSE(CurDeadBandOrSetback(DualZoneNum));                          // Tstat should show there is load on a dual SP
    EXPECT_EQ(-2500.0, ZoneSysEnergyDemand(DualZoneNum).TotalOutputRequired); // should show a cooling load
}

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_AdaptiveThermostat)
{
    // AUTHOR: Xuan Luo
    // DATE WRITTEN: Jan 2017
    using DataEnvironment::DayOfYear;
    using WeatherManager::DesDayInput;
    using WeatherManager::Environment;
    using WeatherManager::Envrn;

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Core_top,                !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "Zone,",
        "  Core_middle,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "Zone,",
        "  Core_basement,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "Zone,",
        "  Core_bottom,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_top Thermostat,                   !- Name",
        "  Core_top,                              !- Zone or ZoneList Name",
        "  Single Cooling Control Type Sched,     !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleCooling,      !- Control 1 Object Type",
        "  Core_top CoolSPSched;                  !- Control 1 Name",
        " ",
        "ZoneControl:Thermostat:OperativeTemperature,",
        "  Core_top Thermostat,                   !- Thermostat Name",
        "  CONSTANT,                              !- Radiative Fraction Input Mode",
        "  0.0,                                   !- Fixed Radiative Fraction",
        "  ,                                      !- Radiative Fraction Schedule Name",
        "  AdaptiveASH55CentralLine;              !- Adaptive Comfort Model Type",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_middle Thermostat,                !- Name",
        "  Core_middle,                           !- Zone or ZoneList Name",
        "  Single Cooling Control Type Sched,     !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleCooling,      !- Control 1 Object Type",
        "  Core_middle CoolSPSched;               !- Control 1 Name",
        " ",
        "ZoneControl:Thermostat:OperativeTemperature,",
        "  Core_middle Thermostat,                !- Thermostat Name",
        "  CONSTANT,                              !- Radiative Fraction Input Mode",
        "  0.0,                                   !- Fixed Radiative Fraction",
        "  ,                                      !- Radiative Fraction Schedule Name",
        "  AdaptiveCEN15251CentralLine;           !- Adaptive Comfort Model Type",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_basement Thermostat,                   !- Name",
        "  Core_basement,                              !- Zone or ZoneList Name",
        "  Single Cooling Heating Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:SingleHeatingOrCooling,  !- Control 1 Object Type",
        "  Core_basement CoolHeatSPSched;              !- Control 1 Name",
        " ",
        "ZoneControl:Thermostat:OperativeTemperature,",
        "  Core_basement Thermostat,              !- Thermostat Name",
        "  CONSTANT,                              !- Radiative Fraction Input Mode",
        "  0.0,                                   !- Fixed Radiative Fraction",
        "  ,                                      !- Radiative Fraction Schedule Name",
        "  None;                                  !- Adaptive Comfort Model Type",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_bottom Thermostat,                !- Name",
        "  Core_bottom,                           !- Zone or ZoneList Name",
        "  Dual Zone Control Type Sched,          !- Control Type Schedule Name",
        "  ThermostatSetpoint:DualSetpoint,       !- Control 1 Object Type",
        "  Core_bottom DualSPSched;               !- Control 1 Name",
        " ",
        "ZoneControl:Thermostat:OperativeTemperature,",
        "  Core_bottom Thermostat,                !- Thermostat Name",
        "  CONSTANT,                              !- Radiative Fraction Input Mode",
        "  0.0,                                   !- Fixed Radiative Fraction",
        "  ,                                      !- Radiative Fraction Schedule Name",
        "  AdaptiveASH55CentralLine;              !- Adaptive Comfort Model Type",
        " ",
        "ThermostatSetpoint:SingleCooling,",
        "  Core_middle CoolSPSched,               !- Name",
        "  SNGL_CLGSETP_SCH;                      !- Cooling Setpoint Temperature Schedule Name",
        " ",
        "ThermostatSetpoint:SingleHeatingOrCooling,",
        "  Core_basement CoolHeatSPSched,         !- Name",
        "  CLGHTGSETP_SCH;                        !- Heating Setpoint Temperature Schedule Name",
        " ",
        "ThermostatSetpoint:DualSetpoint,",
        "  Core_bottom DualSPSched,               !- Name",
        "  HTGSETP_SCH,                           !- Heating Setpoint Temperature Schedule Name",
        "  CLGSETP_SCH;                           !- Cooling Setpoint Temperature Schedule Name",
        " ",
        "Schedule:Compact,",
        "  Single Cooling Control Type Sched,  !- Name",
        "  Control Type,                          !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,2;                        !- Field 3",
        " ",
        "Schedule:Compact,",
        "  SNGL_CLGSETP_SCH,                      !- Name",
        "  Temperature,                           !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,24.0;                     !- Field 3",
        " ",
        "Schedule:Compact,",
        "  Single Cooling Heating Control Type Sched,  !- Name",
        "  Control Type,                          !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,3;                        !- Field 3",
        " ",
        "Schedule:Compact,",
        "  Dual Zone Control Type Sched,          !- Name",
        "  Control Type,                          !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,4;                        !- Field 3",
        " ",
        "Schedule:Compact,",
        "  CLGSETP_SCH,                           !- Name",
        "  Temperature,                           !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,24.0;                     !- Field 3",
        " ",
        "Schedule:Compact,",
        "  HTGSETP_SCH,                           !- Name",
        "  Temperature,                           !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,15.0;                     !- Field 3",
        " ",
        "Schedule:Compact,",
        "  CLGHTGSETP_SCH,                        !- Name",
        "  Temperature,                           !- Schedule Type Limits Name",
        "  Through: 12/31,                        !- Field 1",
        "  For: AllDays,                          !- Field 2",
        "  Until: 24:00,24.0;                     !- Field 3",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // Tstat should show if the idf is legel

    int ZoneNum(4);
    int CoolZoneASHNum(1);
    int CoolZoneCENNum(2);
    int NoneAdapZoneNum(3);
    int DualZoneNum(4);
    int summerDesignDayTypeIndex(9);
    int const ASH55_CENTRAL(2);
    int const CEN15251_CENTRAL(5);

    DayOfYear = 1;
    Envrn = 1;
    Environment.allocate(1);
    DesDayInput.allocate(1);
    Environment(Envrn).KindOfEnvrn = ksRunPeriodWeather;
    DesDayInput(Envrn).DayType = summerDesignDayTypeIndex;
    DesDayInput(Envrn).MaxDryBulb = 30.0;
    DesDayInput(Envrn).DailyDBRange = 10.0;
    Real64 ZoneAirSetPoint = 0.0;

    bool ErrorsFound(false); // If errors detected in input
    GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);                                  // Tstat should show if there is error in zone processing
    ASSERT_FALSE(AdapComfortDailySetPointSchedule.initialized); // Tstat should show there adaptive model is not initialized

    EPVector<Real64> runningAverageASH_1(365, 0.0);
    EPVector<Real64> runningAverageCEN_1(365, 0.0);
    CalculateAdaptiveComfortSetPointSchl(runningAverageASH_1, runningAverageCEN_1);
    // Tstat should show flage that adaptive comfort is not applicable (-1)
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(DayOfYear));

    EPVector<Real64> runningAverageASH_2(365, 40.0);
    EPVector<Real64> runningAverageCEN_2(365, 40.0);
    CalculateAdaptiveComfortSetPointSchl(runningAverageASH_2, runningAverageCEN_2);
    // Tstat should show flage that adaptive comfort is not applicable (-1)
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(DayOfYear));
    ASSERT_EQ(-1, AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(DayOfYear));

    EPVector<Real64> runningAverageASH(365, 25.0);
    EPVector<Real64> runningAverageCEN(365, 25.0);
    CalculateAdaptiveComfortSetPointSchl(runningAverageASH, runningAverageCEN);
    ASSERT_TRUE(AdapComfortDailySetPointSchedule.initialized); // Tstat should show there adaptive model is initialized
    ASSERT_EQ(
        25.55,
        AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(DayOfYear)); // Tstat should show ASH 55 CENTRAL LINE model set point
    ASSERT_EQ(
        28.05,
        AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90(DayOfYear)); // Tstat should show ASH 55 Upper 90 LINE model set point
    ASSERT_EQ(
        29.05,
        AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80(DayOfYear)); // Tstat should show ASH 55 Upper 80 LINE model set point
    ASSERT_EQ(27.05,
              AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central(
                  DayOfYear)); // Tstat should show CEN 15251 CENTRAL LINE model set point
    ASSERT_EQ(29.05,
              AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I(
                  DayOfYear)); // Tstat should show CEN 15251 Upper I LINE model set point
    ASSERT_EQ(30.05,
              AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II(
                  DayOfYear)); // Tstat should show CEN 15251 Upper II LINE model set point
    ASSERT_EQ(31.05,
              AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III(
                  DayOfYear));                            // Tstat should show CEN 15251 Upper III LINE model set point
    ASSERT_EQ(25.55, AdapComfortSetPointSummerDesDay(1)); // Tstat should show ASH 55 CENTRAL LINE model set point
    ASSERT_EQ(27.05, AdapComfortSetPointSummerDesDay(4)); // Tstat should show CEN 15251 CENTRAL LINE model set point

    TempControlledZone.allocate(ZoneNum);
    TempControlledZone(CoolZoneASHNum).AdaptiveComfortTempControl = true;
    TempControlledZone(CoolZoneASHNum).AdaptiveComfortModelTypeIndex = ASH55_CENTRAL;
    TempControlledZone(CoolZoneCENNum).AdaptiveComfortTempControl = true;
    TempControlledZone(CoolZoneCENNum).AdaptiveComfortModelTypeIndex = CEN15251_CENTRAL;
    TempControlledZone(NoneAdapZoneNum).AdaptiveComfortTempControl = true;
    TempControlledZone(NoneAdapZoneNum).AdaptiveComfortModelTypeIndex = ASH55_CENTRAL;
    TempControlledZone(DualZoneNum).AdaptiveComfortTempControl = true;
    TempControlledZone(DualZoneNum).AdaptiveComfortModelTypeIndex = ASH55_CENTRAL;

    ZoneAirSetPoint = 0.0;
    AdjustOperativeSetPointsforAdapComfort(CoolZoneASHNum, ZoneAirSetPoint);
    ASSERT_EQ(25.55, ZoneAirSetPoint); // Tstat should show set point overwritten by ASH 55 CENTRAL LINE model

    ZoneAirSetPoint = 0.0;
    AdjustOperativeSetPointsforAdapComfort(CoolZoneCENNum, ZoneAirSetPoint);
    ASSERT_EQ(27.05, ZoneAirSetPoint); // Tstat should show set point overwritten by CEN 15251 CENTRAL LINE model

    ZoneAirSetPoint = 0.0;
    AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central(DayOfYear) = -1;
    AdjustOperativeSetPointsforAdapComfort(NoneAdapZoneNum, ZoneAirSetPoint);
    ASSERT_EQ(0, ZoneAirSetPoint); // Tstat should show set point is not overwritten

    ZoneAirSetPoint = 26.0;
    AdjustOperativeSetPointsforAdapComfort(DualZoneNum, ZoneAirSetPoint);
    ASSERT_EQ(26.0, ZoneAirSetPoint); // Tstat should show set point is not overwritten
}

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_CalcZoneSums_SurfConvectionTest)
{
    // AUTHOR: L. Gu, FSEC
    // DATE WRITTEN: Jan 2017
    // #5906 Adaptive convection resulting in extremely low zone temperature which causes fatal error

    int ZoneNum = 1;         // Zone number
    Real64 SumIntGain = 0.0; // Zone sum of convective internal gains
    Real64 SumHA = 0.0;      // Zone sum of Hc*Area
    Real64 SumHATsurf = 0.0; // Zone sum of Hc*Area*Tsurf
    Real64 SumHATref = 0.0;  // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
    Real64 SumMCp = 0.0;     // Zone sum of MassFlowRate*Cp
    Real64 SumMCpT = 0.0;    // Zone sum of MassFlowRate*Cp*T
    Real64 SumSysMCp = 0.0;  // Zone sum of air system MassFlowRate*Cp
    Real64 SumSysMCpT = 0.0; // Zone sum of air system MassFlowRate*Cp*T

    DataHeatBalance::ZoneIntGain.allocate(ZoneNum);
    DataHeatBalFanSys::SumConvHTRadSys.allocate(ZoneNum);
    DataHeatBalFanSys::SumConvPool.allocate(ZoneNum);
    DataHeatBalFanSys::MCPI.allocate(ZoneNum);
    DataHeatBalFanSys::MCPV.allocate(ZoneNum);
    DataHeatBalFanSys::MCPM.allocate(ZoneNum);
    DataHeatBalFanSys::MCPE.allocate(ZoneNum);
    DataHeatBalFanSys::MCPC.allocate(ZoneNum);
    DataHeatBalFanSys::MCPTI.allocate(ZoneNum);
    DataHeatBalFanSys::MCPTV.allocate(ZoneNum);
    DataHeatBalFanSys::MCPTM.allocate(ZoneNum);
    DataHeatBalFanSys::MCPTE.allocate(ZoneNum);
    DataHeatBalFanSys::MCPTC.allocate(ZoneNum);
    DataHeatBalFanSys::MDotCPOA.allocate(ZoneNum);
    DataHeatBalFanSys::MCPI(ZoneNum) = 0.0;
    DataHeatBalFanSys::MCPV(ZoneNum) = 0.0;
    DataHeatBalFanSys::MCPM(ZoneNum) = 0.0;
    DataHeatBalFanSys::MCPE(ZoneNum) = 0.0;
    DataHeatBalFanSys::MCPC(ZoneNum) = 0.0;
    DataHeatBalFanSys::MCPTI(ZoneNum) = 0.0;
    DataHeatBalFanSys::MCPTV(ZoneNum) = 0.0;
    DataHeatBalFanSys::MCPTM(ZoneNum) = 0.0;
    DataHeatBalFanSys::MCPTE(ZoneNum) = 0.0;
    DataHeatBalFanSys::MCPTC(ZoneNum) = 0.0;
    DataHeatBalFanSys::MDotCPOA(ZoneNum) = 0.0;

    DataHeatBalance::ZoneIntGain(1).NumberOfDevices = 0;
    DataHeatBalFanSys::SumConvHTRadSys(1) = 0.0;
    DataHeatBalFanSys::SumConvPool(1) = 0.0;

    ZoneEquipConfig.allocate(1);
    ZoneEquipConfig(1).ZoneName = "Zone 1";
    ZoneEquipConfig(1).ActualZoneNum = 1;

    ZoneEquipConfig(1).NumInletNodes = 2;
    ZoneEquipConfig(1).InletNode.allocate(2);
    ZoneEquipConfig(1).InletNode(1) = 1;
    ZoneEquipConfig(1).InletNode(2) = 2;
    ZoneEquipConfig(1).NumExhaustNodes = 1;
    ZoneEquipConfig(1).ExhaustNode.allocate(1);
    ZoneEquipConfig(1).ExhaustNode(1) = 3;
    ZoneEquipConfig(1).NumReturnNodes = 1;
    ZoneEquipConfig(1).ReturnNode.allocate(1);
    ZoneEquipConfig(1).ReturnNode(1) = 4;
    ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    Zone.allocate(1);
    Zone(1).Name = ZoneEquipConfig(1).ZoneName;
    Zone(1).ZoneEqNum = 1;
    Zone(1).IsControlled = true;
    ZoneEqSizing.allocate(1);
    CurZoneEqNum = 1;
    Zone(1).Multiplier = 1.0;
    Zone(1).Volume = 1000.0;
    Zone(1).SystemZoneNodeNumber = 5;
    Zone(1).ZoneVolCapMultpMoist = 1.0;
    ZoneLatentGain.allocate(1);
    ZoneLatentGain(1) = 0.0;
    SumLatentHTRadSys.allocate(1);
    SumLatentHTRadSys(1) = 0.0;
    SumLatentPool.allocate(1);
    SumLatentPool(1) = 0.0;
    OutBaroPress = 101325.0;
    MAT.allocate(1); // Zone temperature C
    MAT(1) = 24.0;
    ZoneAirHumRat.allocate(1);
    ZoneAirHumRat(1) = 0.001;

    Zone(1).SurfaceFirst = 1;
    Zone(1).SurfaceLast = 3;
    Surface.allocate(3);
    HConvIn.allocate(3);
    Node.allocate(4);
    TempEffBulkAir.allocate(3);
    DataHeatBalSurface::TempSurfInTmp.allocate(3);

    Surface(1).HeatTransSurf = true;
    Surface(2).HeatTransSurf = true;
    Surface(3).HeatTransSurf = true;
    Surface(1).Area = 10.0;
    Surface(2).Area = 10.0;
    Surface(3).Area = 10.0;
    Surface(1).TAirRef = ZoneMeanAirTemp;
    Surface(2).TAirRef = AdjacentAirTemp;
    Surface(3).TAirRef = ZoneSupplyAirTemp;
    DataHeatBalSurface::TempSurfInTmp(1) = 15.0;
    DataHeatBalSurface::TempSurfInTmp(2) = 20.0;
    DataHeatBalSurface::TempSurfInTmp(3) = 25.0;
    TempEffBulkAir(1) = 10.0;
    TempEffBulkAir(2) = 10.0;
    TempEffBulkAir(3) = 10.0;

    Node(1).Temp = 20.0;
    Node(2).Temp = 20.0;
    Node(3).Temp = 20.0;
    Node(4).Temp = 20.0;
    Node(1).MassFlowRate = 0.1;
    Node(2).MassFlowRate = 0.1;
    Node(3).MassFlowRate = 0.1;
    Node(4).MassFlowRate = 0.1;

    HConvIn(1) = 0.5;
    HConvIn(2) = 0.5;
    HConvIn(3) = 0.5;

    NumZoneReturnPlenums = 0;
    NumZoneSupplyPlenums = 0;

    CalcZoneSums(ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT);
    EXPECT_EQ(5.0, SumHA);
    EXPECT_EQ(300.0, SumHATsurf);
    EXPECT_EQ(150.0, SumHATref);

    Node(1).MassFlowRate = 0.0;
    Node(2).MassFlowRate = 0.0;
    CalcZoneSums(ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT);
    EXPECT_EQ(10.0, SumHA);
    EXPECT_EQ(300.0, SumHATsurf);
    EXPECT_EQ(50.0, SumHATref);
}

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_EMSOverrideSetpointTest)
{
    // AUTHOR: L. Gu, FSEC
    // DATE WRITTEN: Jun. 2017
    // #5870 EMS actuators for Zone Temperature Control not working

    NumTempControlledZones = 1;
    NumComfortControlledZones = 0;
    TempControlledZone.allocate(1);
    TempControlledZone(1).EMSOverrideHeatingSetPointOn = true;
    TempControlledZone(1).EMSOverrideCoolingSetPointOn = true;
    TempControlledZone(1).ActualZoneNum = 1;
    TempControlledZone(1).EMSOverrideHeatingSetPointValue = 23;
    TempControlledZone(1).EMSOverrideCoolingSetPointValue = 26;

    TempControlType.allocate(1);
    TempZoneThermostatSetPoint.allocate(1);
    ZoneThermostatSetPointLo.allocate(1);
    ZoneThermostatSetPointHi.allocate(1);
    TempControlType(1) = DualSetPointWithDeadBand;

    OverrideAirSetPointsforEMSCntrl();
    EXPECT_EQ(23.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, ZoneThermostatSetPointHi(1));

    NumTempControlledZones = 0;
    NumComfortControlledZones = 1;
    ComfortControlledZone.allocate(1);
    ComfortControlType.allocate(1);
    ComfortControlledZone(1).ActualZoneNum = 1;
    ComfortControlledZone(1).EMSOverrideHeatingSetPointOn = true;
    ComfortControlledZone(1).EMSOverrideCoolingSetPointOn = true;
    ComfortControlType(1) = DualSetPointWithDeadBand;
    ComfortControlledZone(1).EMSOverrideHeatingSetPointValue = 22;
    ComfortControlledZone(1).EMSOverrideCoolingSetPointValue = 25;

    OverrideAirSetPointsforEMSCntrl();
    EXPECT_EQ(22.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(25.0, ZoneThermostatSetPointHi(1));
}

TEST_F(EnergyPlusFixture, temperatureAndCountInSch_test)
{
    // J.Glazer - August 2017

    std::string const idf_objects = delimited_string({
        "Version,9.3;",
        " ",
        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",
        " ",
        "Schedule:Compact,",
        " Sched1,                  !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00, 20.0;        !- Field 26",
        " ",
        "Schedule:Compact,",
        " Sched2,                  !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 1/31,            !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00, 24.0,        !- Field 26",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00, 26.0;        !- Field 26",
        " ",
        "Schedule:Compact,",
        " Sched3,                  !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 1/31,            !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 09:00, 24.0,        !- Field 26",
        " Until: 17:00, 26.0,        !- Field 26",
        " Until: 24:00, 24.0,        !- Field 26",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00, 26.0;        !- Field 26",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 15;
    DataEnvironment::CurrentYearIsLeapYear = false;

    Real64 valueAtTime;
    int numDays;
    std::string monthAssumed;
    const int wednesday = 4;

    DataEnvironment::Latitude = 30.; // northern hemisphere
    int sched1Index = GetScheduleIndex("SCHED1");
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(sched1Index, false, wednesday, 11);

    EXPECT_EQ(20, valueAtTime);
    EXPECT_EQ(365, numDays);
    EXPECT_EQ("January", monthAssumed);

    // test month selected based on hemisphere and isSummer flag.
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(sched1Index, true, wednesday, 11);
    EXPECT_EQ("July", monthAssumed);

    DataEnvironment::Latitude = -30.; // southern hemisphere
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(sched1Index, false, wednesday, 11);
    EXPECT_EQ("July", monthAssumed);

    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(sched1Index, true, wednesday, 11);
    EXPECT_EQ("January", monthAssumed);

    DataEnvironment::Latitude = 30.; // northern hemisphere
    int sched2Index = GetScheduleIndex("SCHED2");
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(sched2Index, false, wednesday, 11);

    EXPECT_EQ(24, valueAtTime);
    EXPECT_EQ(31, numDays);
    EXPECT_EQ("January", monthAssumed);

    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(sched2Index, true, wednesday, 11);

    EXPECT_EQ(26, valueAtTime);
    EXPECT_EQ(334, numDays);
    EXPECT_EQ("July", monthAssumed);

    int sched3Index = GetScheduleIndex("SCHED3");
    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(sched3Index, false, wednesday, 11);

    EXPECT_EQ(26, valueAtTime);
    EXPECT_EQ(365, numDays);
    EXPECT_EQ("January", monthAssumed);

    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(sched3Index, true, wednesday, 11);

    EXPECT_EQ(26, valueAtTime);
    EXPECT_EQ(365, numDays);
    EXPECT_EQ("July", monthAssumed);

    std::tie(valueAtTime, numDays, monthAssumed) = temperatureAndCountInSch(sched3Index, false, wednesday, 19);

    EXPECT_EQ(24, valueAtTime);
    EXPECT_EQ(31, numDays);
    EXPECT_EQ("January", monthAssumed);
}

TEST_F(EnergyPlusFixture, SetPointWithCutoutDeltaT_test)
{
    // On/Off thermostat
    Schedule.allocate(3);

    DataZoneControls::NumTempControlledZones = 1;

    // SingleHeatingSetPoint
    TempControlledZone.allocate(NumTempControlledZones);
    TempZoneThermostatSetPoint.allocate(1);
    MAT.allocate(1);
    ZoneThermostatSetPointLo.allocate(1);
    ZoneThermostatSetPointHi.allocate(1);
    ZoneT1.allocate(1);
    ZoneSysEnergyDemand.allocate(1);
    AIRRAT.allocate(1);
    TempDepZnLd.allocate(1);
    TempIndZnLd.allocate(1);
    DeadBandOrSetback.allocate(1);
    DataHeatBalance::Zone.allocate(1);
    ZoneSetPointLast.allocate(1);
    DataZoneEnergyDemands::Setback.allocate(1);

    SNLoadPredictedRate.allocate(1);
    SNLoadPredictedHSPRate.allocate(1);
    SNLoadPredictedCSPRate.allocate(1);
    CurDeadBandOrSetback.allocate(1);
    LoadCorrectionFactor.allocate(1);
    DeadBandOrSetback.allocate(1);

    ZoneAirSolutionAlgo = UseEulerMethod;

    TempControlledZone(1).DeltaTCutSet = 2.0;
    TempControlledZone(1).ActualZoneNum = 1;
    TempControlledZone(1).CTSchedIndex = 1;
    Schedule(1).CurrentValue = 1;
    TempControlType.allocate(1);
    TempControlledZone(1).SchIndx_SingleHeatSetPoint = 2;
    TempControlledZone(1).ControlTypeSchIndx.allocate(4);
    TempControlledZone(1).ControlTypeSchIndx(2) = 1;
    SetPointSingleHeating.allocate(1);
    SetPointSingleHeating(1).TempSchedIndex = 3;
    Schedule(3).CurrentValue = 22.0;
    AIRRAT(1) = 2000;
    TempDepZnLd(1) = 1.0;
    TempIndZnLd(1) = 1.0;
    MAT(1) = 20.0;
    ZoneT1(1) = MAT(1);
    NumOnOffCtrZone = 1;

    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(24.0, ZoneThermostatSetPointLo(1));

    MAT(1) = 23.0;
    ZoneT1(1) = MAT(1);
    TempControlledZone(1).HeatModeLast = true;
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(22.0, ZoneThermostatSetPointLo(1));
    TempControlledZone(1).HeatModeLast = false;

    // SingleCoolingSetPoint
    Schedule(1).CurrentValue = 2;
    TempControlledZone(1).SchIndx_SingleCoolSetPoint = 2;
    TempControlledZone(1).ControlTypeSchIndx(2) = 1;
    SetPointSingleCooling.allocate(1);
    SetPointSingleCooling(1).TempSchedIndex = 3;
    Schedule(3).CurrentValue = 26.0;
    MAT(1) = 25.0;
    ZoneT1(1) = MAT(1);

    TempControlledZone(1).CoolModeLast = true;
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(26.0, ZoneThermostatSetPointHi(1));
    TempControlledZone(1).CoolModeLast = false;

    MAT(1) = 27.0;
    ZoneT1(1) = MAT(1);
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(24.0, ZoneThermostatSetPointHi(1));

    // SingleHeatCoolSetPoint
    Schedule(1).CurrentValue = 3;
    TempControlledZone(1).SchIndx_SingleHeatCoolSetPoint = 2;
    TempControlledZone(1).ControlTypeSchIndx(2) = 1;
    SetPointSingleHeatCool.allocate(1);
    SetPointSingleHeatCool(1).TempSchedIndex = 3;
    Schedule(3).CurrentValue = 24.0;
    MAT(1) = 25.0;
    ZoneT1(1) = MAT(1);

    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(24.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(24.0, ZoneThermostatSetPointHi(1));

    // DualSetPointWithDeadBand : Adjust cooling setpoint
    SetPointDualHeatCool.allocate(1);
    Schedule(1).CurrentValue = 4;
    TempControlledZone(1).SchIndx_DualSetPointWDeadBand = 2;
    TempControlledZone(1).ControlTypeSchIndx(2) = 1;
    SetPointDualHeatCool(1).HeatTempSchedIndex = 2;
    SetPointDualHeatCool(1).CoolTempSchedIndex = 3;
    Schedule(2).CurrentValue = 22.0;
    Schedule(3).CurrentValue = 26.0;
    MAT(1) = 25.0;
    ZoneT1(1) = MAT(1);

    TempControlledZone(1).CoolModeLast = true;
    TempControlledZone(1).HeatModeLast = true;
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(22.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, ZoneThermostatSetPointHi(1));
    TempControlledZone(1).HeatModeLast = false;

    // DualSetPointWithDeadBand : Adjust heating setpoint
    MAT(1) = 21.0;
    ZoneT1(1) = MAT(1);
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(24.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, ZoneThermostatSetPointHi(1));

    // DualSetPointWithDeadBand : Adjust cooling setpoint
    TempControlledZone(1).CoolModeLast = true;
    MAT(1) = 27.0;
    ZoneT1(1) = MAT(1);
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(22.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(24.0, ZoneThermostatSetPointHi(1));
}

TEST_F(EnergyPlusFixture, TempAtPrevTimeStepWithCutoutDeltaT_test)
{
    Schedule.allocate(3);
    DataZoneControls::NumTempControlledZones = 1;

    // SingleHeatingSetPoint
    TempControlledZone.allocate(NumTempControlledZones);
    TempZoneThermostatSetPoint.allocate(1);
    MAT.allocate(1);
    ZoneThermostatSetPointLo.allocate(1);
    ZoneThermostatSetPointHi.allocate(1);
    XMPT.allocate(1);
    ZoneSysEnergyDemand.allocate(1);
    AIRRAT.allocate(1);
    TempDepZnLd.allocate(1);
    TempIndZnLd.allocate(1);
    DeadBandOrSetback.allocate(1);
    DataHeatBalance::Zone.allocate(1);
    ZoneSetPointLast.allocate(1);
    DataZoneEnergyDemands::Setback.allocate(1);

    SNLoadPredictedRate.allocate(1);
    SNLoadPredictedHSPRate.allocate(1);
    SNLoadPredictedCSPRate.allocate(1);
    CurDeadBandOrSetback.allocate(1);
    LoadCorrectionFactor.allocate(1);
    DeadBandOrSetback.allocate(1);

    ZoneAirSolutionAlgo = Use3rdOrder;

    TempControlledZone(1).DeltaTCutSet = 2.0;
    TempControlledZone(1).ActualZoneNum = 1;
    TempControlledZone(1).CTSchedIndex = 1;
    Schedule(1).CurrentValue = 1;
    TempControlType.allocate(1);
    TempControlledZone(1).SchIndx_SingleHeatSetPoint = 2;
    TempControlledZone(1).ControlTypeSchIndx.allocate(4);
    TempControlledZone(1).ControlTypeSchIndx(2) = 1;
    SetPointSingleHeating.allocate(1);
    SetPointSingleHeating(1).TempSchedIndex = 3;
    Schedule(3).CurrentValue = 22.0;
    AIRRAT(1) = 2000;
    TempDepZnLd(1) = 1.0;
    TempIndZnLd(1) = 1.0;
    MAT(1) = 20.0;
    XMPT(1) = 23.0;
    NumOnOffCtrZone = 1;

    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(24.0, ZoneThermostatSetPointLo(1));

    TempControlledZone(1).HeatModeLastSave = true;
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(true, false, 0.01);
    EXPECT_EQ(22.0, ZoneThermostatSetPointLo(1));

    // SingleCoolingSetPoint
    Schedule(1).CurrentValue = 2;
    TempControlledZone(1).SchIndx_SingleCoolSetPoint = 2;
    TempControlledZone(1).ControlTypeSchIndx(2) = 1;
    SetPointSingleCooling.allocate(1);
    SetPointSingleCooling(1).TempSchedIndex = 3;
    Schedule(3).CurrentValue = 26.0;
    MAT(1) = 25.0;
    XMPT(1) = 27;

    TempControlledZone(1).CoolModeLast = true;
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(26.0, ZoneThermostatSetPointHi(1));
    TempControlledZone(1).CoolModeLast = false;

    TempControlledZone(1).CoolModeLastSave = true;
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(true, false, 0.01);
    EXPECT_EQ(24.0, ZoneThermostatSetPointHi(1));

    // SingleHeatCoolSetPoint
    Schedule(1).CurrentValue = 3;
    TempControlledZone(1).SchIndx_SingleHeatCoolSetPoint = 2;
    TempControlledZone(1).ControlTypeSchIndx(2) = 1;
    SetPointSingleHeatCool.allocate(1);
    SetPointSingleHeatCool(1).TempSchedIndex = 3;
    Schedule(3).CurrentValue = 24.0;
    MAT(1) = 25.0;
    XMPT(1) = MAT(1);

    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(24.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(24.0, ZoneThermostatSetPointHi(1));

    // DualSetPointWithDeadBand : Adjust cooling setpoint
    SetPointDualHeatCool.allocate(1);
    Schedule(1).CurrentValue = 4;
    TempControlledZone(1).SchIndx_DualSetPointWDeadBand = 2;
    TempControlledZone(1).ControlTypeSchIndx(2) = 1;
    SetPointDualHeatCool(1).HeatTempSchedIndex = 2;
    SetPointDualHeatCool(1).CoolTempSchedIndex = 3;
    Schedule(2).CurrentValue = 22.0;
    Schedule(3).CurrentValue = 26.0;
    MAT(1) = 25.0;
    XMPT(1) = 21.0;

    TempControlledZone(1).CoolModeLast = true;
    TempControlledZone(1).HeatModeLast = true;
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(false, false, 0.01);
    EXPECT_EQ(22.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, ZoneThermostatSetPointHi(1));
    TempControlledZone(1).HeatModeLast = false;

    // DualSetPointWithDeadBand : Adjust heating setpoint
    TempControlledZone(1).HeatModeLastSave = true;
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(true, false, 0.01);
    EXPECT_EQ(24.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(26.0, ZoneThermostatSetPointHi(1));

    // DualSetPointWithDeadBand : Adjust cooling setpoint
    TempControlledZone(1).CoolModeLastSave = true;
    XMPT(1) = 27.0;
    CalcZoneAirTempSetPoints();
    PredictSystemLoads(true, false, 0.01);
    EXPECT_EQ(22.0, ZoneThermostatSetPointLo(1));
    EXPECT_EQ(24.0, ZoneThermostatSetPointHi(1));
}

TEST_F(EnergyPlusFixture, ReportMoistLoadsZoneMultiplier_Test)
{
    Real64 TotOutReq;
    Real64 OutReqToHumSP;
    Real64 OutReqToDehumSP;
    Real64 SingleZoneTotRate;
    Real64 SingleZoneHumRate;
    Real64 SingleZoneDehRate;
    Real64 ZoneMultiplier;
    Real64 ZoneMultiplierList;
    Real64 ExpectedResult;
    Real64 AcceptableTolerance = 0.00001;

    // Test 1: Zone Multipliers are all unity (1.0).  So, single zone loads should be the same as total loads
    TotOutReq = 1000.0;
    OutReqToHumSP = 2000.0;
    OutReqToDehumSP = 3000.0;
    ZoneMultiplier = 1.0;
    ZoneMultiplierList = 1.0;
    ReportMoistLoadsZoneMultiplier(TotOutReq,OutReqToHumSP,OutReqToDehumSP,
                                   SingleZoneTotRate,SingleZoneHumRate,SingleZoneDehRate,
                                   ZoneMultiplier,ZoneMultiplierList);
    EXPECT_NEAR(TotOutReq,SingleZoneTotRate,AcceptableTolerance);
    EXPECT_NEAR(OutReqToHumSP,SingleZoneHumRate,AcceptableTolerance);
    EXPECT_NEAR(OutReqToDehumSP,SingleZoneDehRate,AcceptableTolerance);

    // Test 2a: Zone Multiplier (non-list) is greater than 1, list Zone Multiplier is still one
    TotOutReq = 1000.0;
    OutReqToHumSP = 2000.0;
    OutReqToDehumSP = 3000.0;
    ZoneMultiplier = 7.0;
    ZoneMultiplierList = 1.0;
    ReportMoistLoadsZoneMultiplier(TotOutReq,OutReqToHumSP,OutReqToDehumSP,
                                   SingleZoneTotRate,SingleZoneHumRate,SingleZoneDehRate,
                                   ZoneMultiplier,ZoneMultiplierList);
    ExpectedResult = 1000.0;
    EXPECT_NEAR(ExpectedResult,SingleZoneTotRate,AcceptableTolerance);
    ExpectedResult = 2000.0;
    EXPECT_NEAR(ExpectedResult,SingleZoneHumRate,AcceptableTolerance);
    ExpectedResult = 3000.0;
    EXPECT_NEAR(ExpectedResult,SingleZoneDehRate,AcceptableTolerance);
    ExpectedResult = 7000.0;
    EXPECT_NEAR(TotOutReq,ExpectedResult,AcceptableTolerance);
    ExpectedResult = 14000.0;
    EXPECT_NEAR(OutReqToHumSP,ExpectedResult,AcceptableTolerance);
    ExpectedResult = 21000.0;
    EXPECT_NEAR(OutReqToDehumSP,ExpectedResult,AcceptableTolerance);

    // Test 2a: list Zone Multiplier is greater than 1, non-list Zone Multiplier is one
    TotOutReq = 1000.0;
    OutReqToHumSP = 2000.0;
    OutReqToDehumSP = 3000.0;
    ZoneMultiplier = 1.0;
    ZoneMultiplierList = 7.0;
    ReportMoistLoadsZoneMultiplier(TotOutReq,OutReqToHumSP,OutReqToDehumSP,
                                   SingleZoneTotRate,SingleZoneHumRate,SingleZoneDehRate,
                                   ZoneMultiplier,ZoneMultiplierList);
    ExpectedResult = 1000.0;
    EXPECT_NEAR(ExpectedResult,SingleZoneTotRate,AcceptableTolerance);
    ExpectedResult = 2000.0;
    EXPECT_NEAR(ExpectedResult,SingleZoneHumRate,AcceptableTolerance);
    ExpectedResult = 3000.0;
    EXPECT_NEAR(ExpectedResult,SingleZoneDehRate,AcceptableTolerance);
    ExpectedResult = 7000.0;
    EXPECT_NEAR(TotOutReq,ExpectedResult,AcceptableTolerance);
    ExpectedResult = 14000.0;
    EXPECT_NEAR(OutReqToHumSP,ExpectedResult,AcceptableTolerance);
    ExpectedResult = 21000.0;
    EXPECT_NEAR(OutReqToDehumSP,ExpectedResult,AcceptableTolerance);

    // Test 3: both zone multipliers are greater than 1.0
    TotOutReq = 300.0;
    OutReqToHumSP = 150.0;
    OutReqToDehumSP = 100.0;
    ZoneMultiplier = 2.0;
    ZoneMultiplierList = 3.0;
    ReportMoistLoadsZoneMultiplier(TotOutReq,OutReqToHumSP,OutReqToDehumSP,
                                   SingleZoneTotRate,SingleZoneHumRate,SingleZoneDehRate,
                                   ZoneMultiplier,ZoneMultiplierList);
    ExpectedResult = 300.0;
    EXPECT_NEAR(ExpectedResult,SingleZoneTotRate,AcceptableTolerance);
    ExpectedResult = 150.0;
    EXPECT_NEAR(ExpectedResult,SingleZoneHumRate,AcceptableTolerance);
    ExpectedResult = 100.0;
    EXPECT_NEAR(ExpectedResult,SingleZoneDehRate,AcceptableTolerance);
    ExpectedResult = 1800.0;
    EXPECT_NEAR(TotOutReq,ExpectedResult,AcceptableTolerance);
    ExpectedResult = 900.0;
    EXPECT_NEAR(OutReqToHumSP,ExpectedResult,AcceptableTolerance);
    ExpectedResult = 600.0;
    EXPECT_NEAR(OutReqToDehumSP,ExpectedResult,AcceptableTolerance);
}
