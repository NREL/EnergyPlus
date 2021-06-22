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

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, SysAvailManager_OptimumStart)
{

    std::string const idf_objects = delimited_string({

        " AvailabilityManager:OptimumStart,",
        "   OptStart Availability 1, !- Name",
        "   Sch_OptStart,            !- Applicability Schedule Name",
        "   Fan_Schedule,            !- Fan Schedule Name",
        "   MaximumofZoneList,       !- Control Type",
        "   ,                        !- Control Zone Name",
        "   List_Zones,              !- Zone List Name",
        "   4,                       !- Maximum Value for Optimum Start Time {hr}",
        "   AdaptiveTemperatureGradient,  !- Control Algorithm",
        "   ,                        !- Constant Temperature Gradient during Cooling {deltaC/hr}",
        "   ,                        !- Constant Temperature Gradient during Heating {deltaC/hr}",
        "   2,                       !- Initial Temperature Gradient during Cooling {deltaC/hr}",
        "   2,                       !- Initial Temperature Gradient during Heating {deltaC/hr}",
        "   ,                        !- Constant Start Time {hr}",
        "   2;                       !- Number of Previous Days {days}",

        " AvailabilityManager:OptimumStart,",
        "   OptStart Availability 2, !- Name",
        "   Sch_OptStart,            !- Applicability Schedule Name",
        "   Fan_Schedule,            !- Fan Schedule Name",
        "   ControlZone,             !- Control Type",
        "   Zone 4,                  !- Control Zone Name",
        "   ,                        !- Zone List Name",
        "   4,                       !- Maximum Value for Optimum Start Time {hr}",
        "   AdaptiveTemperatureGradient,  !- Control Algorithm",
        "   ,                        !- Constant Temperature Gradient during Cooling {deltaC/hr}",
        "   ,                        !- Constant Temperature Gradient during Heating {deltaC/hr}",
        "   2,                       !- Initial Temperature Gradient during Cooling {deltaC/hr}",
        "   2,                       !- Initial Temperature Gradient during Heating {deltaC/hr}",
        "   ,                        !- Constant Start Time {hr}",
        "   2;                       !- Number of Previous Days {days}",

        " AvailabilityManager:OptimumStart,",
        "   OptStart Availability 3, !- Name",
        "   Sch_OptStart,            !- Applicability Schedule Name",
        "   Fan_Schedule_Alt,            !- Fan Schedule Name",
        "   ControlZone,             !- Control Type",
        "   Zone 6,                  !- Control Zone Name",
        "   ,                        !- Zone List Name",
        "   1.5,                       !- Maximum Value for Optimum Start Time {hr}",
        "   AdaptiveTemperatureGradient,  !- Control Algorithm",
        "   ,                        !- Constant Temperature Gradient during Cooling {deltaC/hr}",
        "   ,                        !- Constant Temperature Gradient during Heating {deltaC/hr}",
        "   2,                       !- Initial Temperature Gradient during Cooling {deltaC/hr}",
        "   2,                       !- Initial Temperature Gradient during Heating {deltaC/hr}",
        "   ,                        !- Constant Start Time {hr}",
        "   2;                       !- Number of Previous Days {days}",

        " Schedule:Compact,",
        "   Sch_OptStart,            !- Name",
        "   Fraction,                !- Schedule Type Limits Name",
        "   Through: 12/31,          !- Field 1",
        "   For: AllDays,            !- Field 2",
        "   Until: 24:00, 1.0;       !- Field 3",

        " Schedule:Compact,",
        "   Fan_Schedule,            !- Name",
        "   Fraction,                !- Schedule Type Limits Name",
        "   Through: 12/31,          !- Field 1",
        "   For: AllDays,            !- Field 2",
        "   Until:  7:00, 0.0,       !- Field 3",
        "   Until: 24:00, 1.0;       !- Field 3",

        " Schedule:Compact,",
        "   Fan_Schedule_Alt,            !- Name",
        "   Fraction,                !- Schedule Type Limits Name",
        "   Through: 12/31,          !- Field 1",
        "   For: AllDays,            !- Field 2",
        "   Until:  6:30, 0.0,       !- Field 3",
        "   Until: 24:00, 1.0;       !- Field 3",

        " ZoneList,",
        "   List_Zones,              !- Name",
        "   Zone 1,                  !- Zone 1 Name",
        "   Zone 2,                  !- Zone 2 Name",
        "   Zone 3;                  !- Zone 3 Name",

        " ZoneControl:Thermostat,",
        "   LIST_ZONES Thermostat,  !- Name",
        "   LIST_ZONES,             !- Zone or ZoneList Name",
        "   Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "   ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "   Zone DualSPSched; !- Control 1 Name",

        " Schedule:Compact,",
        "   Dual Zone Control Type Sched,  !- Name",
        "   Control Type,            !- Schedule Type Limits Name",
        "   Through: 12/31,          !- Field 1",
        "   For: AllDays,            !- Field 2",
        "   Until: 24:00,4;          !- Field 3",

        " ThermostatSetpoint:DualSetpoint,",
        "   Zone DualSPSched, !- Name",
        "   HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
        "   CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",

        " Schedule:Compact,",
        "   CLGSETP_SCH,             !- Name",
        "   Temperature,             !- Schedule Type Limits Name",
        "   Through: 12/31,          !- Field 1",
        "   For: AllDays,            !- Field 2",
        "   Until: 7:00,29.4,       !- Field 3",
        "   Until: 18:00,24.0,       !- Field 3",
        "   Until: 24:00,29.4;       !- Field 3",

        " Schedule:Compact,",
        "   HTGSETP_SCH,             !- Name",
        "   Temperature,             !- Schedule Type Limits Name",
        "   Through: 12/31,          !- Field 1",
        "   For: AllDays,            !- Field 2",
        "   Until: 7:00,15.0,       !- Field 3",
        "   Until: 18:00,19.0,       !- Field 3",
        "   Until: 24:00,15.0;       !- Field 3",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataHeatBal->NumOfZoneLists = 1;
    state->dataHeatBal->ZoneList.allocate(state->dataHeatBal->NumOfZoneLists);
    state->dataHeatBal->ZoneList(1).Name = "LIST_ZONES";
    state->dataHeatBal->ZoneList(1).NumOfZones = 3;
    state->dataHeatBal->ZoneList(1).Zone.allocate(3);
    state->dataHeatBal->ZoneList(1).Zone(1) = 1;
    state->dataHeatBal->ZoneList(1).Zone(2) = 2;
    state->dataHeatBal->ZoneList(1).Zone(3) = 3;

    state->dataHVACGlobal->NumPrimaryAirSys = 3;
    state->dataAirLoop->PriAirSysAvailMgr.allocate(3);
    state->dataAirLoop->PriAirSysAvailMgr(1).NumAvailManagers = 1;
    state->dataAirLoop->PriAirSysAvailMgr(2).NumAvailManagers = 1;
    state->dataAirLoop->PriAirSysAvailMgr(3).NumAvailManagers = 1;

    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerType.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerName.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerNum.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(2).AvailManagerType.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(2).AvailManagerName.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(2).AvailManagerNum.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(3).AvailManagerType.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(3).AvailManagerName.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(3).AvailManagerNum.allocate(1);

    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerType(1) =
        12; // cValidSysAvailManagerTypes( { ......., "AvailabilityManager:OptimumStart" } );
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerName(1) = "OptStart Availability 1";
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerNum(1) = 1;
    state->dataAirLoop->PriAirSysAvailMgr(2).AvailManagerType(1) =
        12; // cValidSysAvailManagerTypes( { ......., "AvailabilityManager:OptimumStart" } );
    state->dataAirLoop->PriAirSysAvailMgr(2).AvailManagerName(1) = "OptStart Availability 2";
    state->dataAirLoop->PriAirSysAvailMgr(2).AvailManagerNum(1) = 2;
    state->dataAirLoop->PriAirSysAvailMgr(3).AvailManagerType(1) =
        12; // cValidSysAvailManagerTypes( { ......., "AvailabilityManager:OptimumStart" } );
    state->dataAirLoop->PriAirSysAvailMgr(3).AvailManagerName(1) = "OptStart Availability 3";
    state->dataAirLoop->PriAirSysAvailMgr(3).AvailManagerNum(1) = 3;

    state->dataAirLoop->AirToZoneNodeInfo.allocate(3);
    state->dataAirLoop->AirToZoneNodeInfo(1).NumZonesCooled = 3;
    state->dataAirLoop->AirToZoneNodeInfo(1).CoolCtrlZoneNums.allocate(3);
    state->dataAirLoop->AirToZoneNodeInfo(1).CoolCtrlZoneNums(1) = 1;
    state->dataAirLoop->AirToZoneNodeInfo(1).CoolCtrlZoneNums(2) = 2;
    state->dataAirLoop->AirToZoneNodeInfo(1).CoolCtrlZoneNums(3) = 3;

    state->dataAirLoop->AirToZoneNodeInfo(2).NumZonesCooled = 2;
    state->dataAirLoop->AirToZoneNodeInfo(2).CoolCtrlZoneNums.allocate(2);
    state->dataAirLoop->AirToZoneNodeInfo(2).CoolCtrlZoneNums(1) = 4;
    state->dataAirLoop->AirToZoneNodeInfo(2).CoolCtrlZoneNums(2) = 5;

    state->dataAirLoop->AirToZoneNodeInfo(3).NumZonesCooled = 1;
    state->dataAirLoop->AirToZoneNodeInfo(3).CoolCtrlZoneNums.allocate(1);
    state->dataAirLoop->AirToZoneNodeInfo(3).CoolCtrlZoneNums(1) = 6;

    state->dataGlobal->NumOfTimeStepInHour = 6;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 10;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules
    state->dataScheduleMgr->ScheduleInputProcessed = true;
    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->DayOfSim = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 1;
    state->dataEnvrn->DayOfWeekTomorrow = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues(*state);

    state->dataZoneEquip->ZoneEquipAvail.allocate(6);

    state->dataGlobal->NumOfZones = 6;

    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "ZONE 1";
    state->dataHeatBal->Zone(2).Name = "ZONE 2";
    state->dataHeatBal->Zone(3).Name = "ZONE 3";
    state->dataHeatBal->Zone(4).Name = "ZONE 4";
    state->dataHeatBal->Zone(5).Name = "ZONE 5";
    state->dataHeatBal->Zone(6).Name = "ZONE 6";

    state->dataZoneEquip->ZoneEquipConfig.allocate(state->dataGlobal->NumOfZones);

    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone 1";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneNode = 1;

    state->dataZoneEquip->ZoneEquipConfig(2).ZoneName = "Zone 2";
    state->dataZoneEquip->ZoneEquipConfig(2).ActualZoneNum = 2;
    state->dataZoneEquip->ZoneEquipConfig(2).ZoneNode = 2;

    state->dataZoneEquip->ZoneEquipConfig(3).ZoneName = "Zone 3";
    state->dataZoneEquip->ZoneEquipConfig(3).ActualZoneNum = 3;
    state->dataZoneEquip->ZoneEquipConfig(3).ZoneNode = 3;

    state->dataZoneEquip->ZoneEquipConfig(4).ZoneName = "Zone 4";
    state->dataZoneEquip->ZoneEquipConfig(4).ActualZoneNum = 4;
    state->dataZoneEquip->ZoneEquipConfig(4).ZoneNode = 4;

    state->dataZoneEquip->ZoneEquipConfig(5).ZoneName = "Zone 5";
    state->dataZoneEquip->ZoneEquipConfig(5).ActualZoneNum = 5;
    state->dataZoneEquip->ZoneEquipConfig(5).ZoneNode = 5;

    state->dataZoneEquip->ZoneEquipConfig(6).ZoneName = "Zone 6";
    state->dataZoneEquip->ZoneEquipConfig(6).ActualZoneNum = 6;
    state->dataZoneEquip->ZoneEquipConfig(6).ZoneNode = 6;

    state->dataZoneEquip->ZoneEquipInputsFilled = true;

    state->dataHeatBalFanSys->TempTstatAir.allocate(6);
    state->dataHeatBalFanSys->TempTstatAir(1) = 18.0; // all zones have different space temperature
    state->dataHeatBalFanSys->TempTstatAir(2) = 17.0;
    state->dataHeatBalFanSys->TempTstatAir(3) = 16.0;
    state->dataHeatBalFanSys->TempTstatAir(4) = 15.0;
    state->dataHeatBalFanSys->TempTstatAir(5) = 14.0;
    state->dataHeatBalFanSys->TempTstatAir(6) = 10.0;

    state->dataHeatBalFanSys->ZoneThermostatSetPointLo.allocate(6);
    state->dataHeatBalFanSys->ZoneThermostatSetPointHi.allocate(6);

    state->dataHeatBalFanSys->ZoneThermostatSetPointLo = 19.0; // all zones use same set point temperature
    state->dataHeatBalFanSys->ZoneThermostatSetPointHi = 24.0;

    state->dataZoneCtrls->OccRoomTSetPointHeat.allocate(6);
    state->dataZoneCtrls->OccRoomTSetPointCool.allocate(6);

    state->dataZoneCtrls->OccRoomTSetPointHeat = 19.0; // all zones use same set point temperature
    state->dataZoneCtrls->OccRoomTSetPointCool = 24.0;

    SystemAvailabilityManager::ManageSystemAvailability(*state); // 1st time through just gets input

    state->dataGlobal->WarmupFlag = true;
    state->dataGlobal->BeginDayFlag = true; // initialize optimum start data to beginning of day data
    state->dataGlobal->CurrentTime = 1.0;   // set the current time to 1 AM
    SystemAvailabilityManager::ManageSystemAvailability(*state);
    EXPECT_EQ(3, state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).ATGWCZoneNumLo); // zone 3 is farthest from heating set point
    EXPECT_EQ(1,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1)
                  .ATGWCZoneNumHi); // zone 1 is default for cooling set point when heating load exists
    EXPECT_EQ(-3.0, state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).TempDiffLo); // zone 3 is 3C below set point
    EXPECT_EQ(0.0, state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).TempDiffHi);  // cooling data did not get set so is 0
    EXPECT_EQ(DataHVACGlobals::NoAction,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).AvailStatus); // avail manager should not yet be set

    EXPECT_EQ(DataHVACGlobals::NoAction,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(2).AvailStatus); // avail manager should not be set until 6 AM

    state->dataGlobal->WarmupFlag = false;
    state->dataGlobal->BeginDayFlag = false; // start processing temp data to find optimum start time
    state->dataGlobal->CurrentTime = 2.0;    // set the current time to 2 AM
    SystemAvailabilityManager::ManageSystemAvailability(*state);
    // same data as before since zone temps are unchanged
    EXPECT_EQ(3, state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).ATGWCZoneNumLo); // zone 3 is farthest from heating set point
    EXPECT_EQ(1,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1)
                  .ATGWCZoneNumHi); // zone 1 is default for cooling set point when heating load exists
    EXPECT_EQ(-3.0, state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).TempDiffLo); // zone 3 is 3C below set point
    EXPECT_EQ(0.0, state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).TempDiffHi);  // cooling data did not get set so is 0
    EXPECT_EQ(DataHVACGlobals::NoAction,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).AvailStatus); // avail manager should not yet be set

    EXPECT_EQ(DataHVACGlobals::NoAction,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(2).AvailStatus); // avail manager should not be set until 6 AM

    state->dataGlobal->CurrentTime = 7.0; // set the current time to 7 AM which is past time to pre-start HVAC
    SystemAvailabilityManager::ManageSystemAvailability(*state);

    EXPECT_EQ(DataHVACGlobals::CycleOn,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).AvailStatus); // avail manager should be set to cycle on
    EXPECT_EQ(1.5,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).NumHoursBeforeOccupancy); // 1.5 hours = 3C from SP divided by 2C/hour

    EXPECT_EQ(DataHVACGlobals::CycleOn,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(2).AvailStatus); // avail manager should be set at 6 AM

    // #8013 - Check that the optimum start is available during the correct times when using a partial hour fan start
    state->dataGlobal->CurrentTime = 5.00; // set the current time to 5:00 AM, before max optimum start time
    SystemAvailabilityManager::ManageSystemAvailability(*state);
    EXPECT_FALSE(state->dataHVACGlobal->OptStartData.OptStartFlag(6)); // avail manager should be set to no action for Zone 6
    state->dataGlobal->CurrentTime = 6.50;                             // set the current time to 6:30 AM when occupancy begins
    SystemAvailabilityManager::ManageSystemAvailability(*state);
    EXPECT_TRUE(state->dataHVACGlobal->OptStartData.OptStartFlag(6)); // avail manager should be set to cycle on for Zone 6

    // Check that the system restores setpoints to unoccupied setpoints and don't use occupied setpoints post-occupancy
    ZoneTempPredictorCorrector::GetZoneAirSetPoints(*state);
    state->dataHeatBalFanSys->TempControlType.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.allocate(state->dataGlobal->NumOfZones);

    state->dataGlobal->CurrentTime = 19.0; // set the current time to 7 PM which is post-occupancy
    SystemAvailabilityManager::ManageSystemAvailability(*state);
    ZoneTempPredictorCorrector::CalcZoneAirTempSetPoints(*state);

    EXPECT_EQ(DataHVACGlobals::NoAction,
              state->dataSystemAvailabilityManager->OptStartSysAvailMgrData(1).AvailStatus); // avail manager should be set to no action
    EXPECT_EQ(15.0, state->dataHeatBalFanSys->ZoneThermostatSetPointLo(1));                  // 15.0C is the unoccupied heating setpoint
    EXPECT_EQ(29.4, state->dataHeatBalFanSys->ZoneThermostatSetPointHi(1));                  // 29.4C is the unoccupied cooling setpoint
}

TEST_F(EnergyPlusFixture, SysAvailManager_NightCycle_ZoneOutOfTolerance)
{
    int NumZones(4);
    state->dataHeatBalFanSys->TempControlType.allocate(NumZones);
    state->dataHeatBalFanSys->TempTstatAir.allocate(NumZones);
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.allocate(NumZones);
    state->dataHeatBalFanSys->ZoneThermostatSetPointHi.allocate(NumZones);
    state->dataHeatBalFanSys->ZoneThermostatSetPointLo.allocate(NumZones);

    state->dataHeatBalFanSys->TempControlType(1) = DataHVACGlobals::SingleCoolingSetPoint;
    state->dataHeatBalFanSys->TempTstatAir(1) = 30.0;
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint(1) = 25.0;

    state->dataHeatBalFanSys->TempControlType(2) = DataHVACGlobals::SingleHeatCoolSetPoint;
    state->dataHeatBalFanSys->TempTstatAir(2) = 25.0;
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint(2) = 25.0;

    state->dataHeatBalFanSys->TempControlType(3) = DataHVACGlobals::SingleHeatingSetPoint;
    state->dataHeatBalFanSys->TempTstatAir(3) = 10.0;
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint(3) = 20.0;

    state->dataHeatBalFanSys->TempControlType(4) = DataHVACGlobals::DualSetPointWithDeadBand;
    state->dataHeatBalFanSys->TempTstatAir(4) = 30.0;
    state->dataHeatBalFanSys->ZoneThermostatSetPointHi(4) = 25.0;
    state->dataHeatBalFanSys->ZoneThermostatSetPointLo(4) = 20.0;

    Real64 TempTol = 0.5;
    Array1D_int ZoneNumList;
    ZoneNumList.allocate(NumZones);
    ZoneNumList(1) = 3;
    ZoneNumList(2) = 2;
    ZoneNumList(3) = 1;
    ZoneNumList(4) = 4;

    // Test 1 - One zone is over cooling setpoint, one zone is under heating setpoint
    EXPECT_TRUE(SystemAvailabilityManager::CoolingZoneOutOfTolerance(*state, ZoneNumList, NumZones, TempTol));
    EXPECT_TRUE(SystemAvailabilityManager::HeatingZoneOutOfTolerance(*state, ZoneNumList, NumZones, TempTol));

    // Test 2 - All zones are within tolerance
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.1;
    state->dataHeatBalFanSys->TempTstatAir(2) = 24.9;
    state->dataHeatBalFanSys->TempTstatAir(3) = 19.8;
    state->dataHeatBalFanSys->TempTstatAir(4) = 23.0;
    EXPECT_FALSE(SystemAvailabilityManager::CoolingZoneOutOfTolerance(*state, ZoneNumList, NumZones, TempTol));
    EXPECT_FALSE(SystemAvailabilityManager::HeatingZoneOutOfTolerance(*state, ZoneNumList, NumZones, TempTol));

    state->dataHeatBalFanSys->TempControlType.deallocate();
    state->dataHeatBalFanSys->TempTstatAir.deallocate();
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.deallocate();
    state->dataHeatBalFanSys->ZoneThermostatSetPointHi.deallocate();
    state->dataHeatBalFanSys->ZoneThermostatSetPointLo.deallocate();
    ZoneNumList.deallocate();
}

TEST_F(EnergyPlusFixture, SysAvailManager_HybridVentilation_OT_CO2Control)
{

    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData.allocate(1);
    state->dataHVACGlobal->HybridVentSysAvailVentCtrl.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr.allocate(1);
    state->dataHeatBal->Zone.allocate(1);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBal->ZoneMRT.allocate(1);
    state->dataContaminantBalance->ZoneAirCO2.allocate(1);
    state->dataContaminantBalance->ZoneCO2SetPoint.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr.allocate(1);
    state->dataSystemAvailabilityManager->SchedSysAvailMgrData.allocate(1);
    state->dataScheduleMgr->Schedule.allocate(1);
    state->dataHVACGlobal->ZoneComp.allocate(DataZoneEquipment::NumValidSysAvailZoneComponents);
    state->dataHeatBalFanSys->TempControlType.allocate(1);
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.allocate(1);

    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).Name = "HybridControl";
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).ActualZoneNum = 1;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).AirLoopNum = 1;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).ControlModeSchedPtr = 1;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).UseRainIndicator = false;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MaxWindSpeed = 40.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MinOutdoorTemp = 15.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MaxOutdoorTemp = 35.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MinOutdoorEnth = 20000.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MaxOutdoorEnth = 30000.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MinOutdoorDewPoint = 15.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MaxOutdoorDewPoint = 35.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MinOASched = 2;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MinOperTime = 10.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).MinVentTime = 10.0;

    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).TimeVentDuration = 0.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).TimeOperDuration = 0.0;

    state->dataHeatBal->Zone(1).OutDryBulbTemp = 20.0;
    state->dataHeatBal->Zone(1).WindSpeed = 5.0;

    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).ControlMode = 5; // 80% acceptance
    state->dataThermalComforts->runningAverageASH = 20.0;
    state->dataHeatBalFanSys->MAT(1) = 23.0;
    state->dataHeatBal->ZoneMRT(1) = 27.0;

    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(1, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // Vent open

    state->dataHeatBalFanSys->MAT(1) = 26.0;
    state->dataHeatBal->ZoneMRT(1) = 30.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(2, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // System operation

    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).ControlMode = 6; // 90% acceptance
    state->dataHeatBalFanSys->MAT(1) = 23.0;
    state->dataHeatBal->ZoneMRT(1) = 27.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(1, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // Vent open

    state->dataHeatBalFanSys->MAT(1) = 26.0;
    state->dataHeatBal->ZoneMRT(1) = 30.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(2, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // System operation

    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).ControlMode = 7; // CO2 control with an AirLoop
    state->dataContaminantBalance->ZoneAirCO2(1) = 900.0;
    state->dataContaminantBalance->ZoneCO2SetPoint(1) = 800.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).HybridVentMgrConnectedToAirLoop = true;
    state->dataAirLoop->PriAirSysAvailMgr(1).NumAvailManagers = 1;
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerType.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerName.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerNum.allocate(1);
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailStatus = 1;
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerType(1) = 1; // Scheduled
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerName(1) = "Avail 1";
    state->dataAirLoop->PriAirSysAvailMgr(1).AvailManagerNum(1) = 1;
    state->dataSystemAvailabilityManager->SchedSysAvailMgrData(1).SchedPtr = 1;
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(2, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // System operation
    state->dataScheduleMgr->Schedule(1).CurrentValue = 0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(1, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // Vent open

    state->dataContaminantBalance->ZoneAirCO2(1) = 500.0;
    state->dataContaminantBalance->ZoneCO2SetPoint(1) = 800.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(0, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // No action

    state->dataHVACGlobal->ZoneComp(1).TotalNumComp = 1; //  CO2 control with zone equipment
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs.allocate(1);
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).AvailStatus = 2;
    state->dataContaminantBalance->ZoneAirCO2(1) = 900.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).HybridVentMgrConnectedToAirLoop = false;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).SimHybridVentSysAvailMgr = true;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(2, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // System operation
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).AvailStatus = 1;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(1, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // Vent open

    // time duration test
    state->dataHeatBal->Zone(1).OutDryBulbTemp = 40.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).ControlMode = 1;     // Temperature control
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl = 1; // Open
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).TimeOperDuration = 5.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(1, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // No change
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).TimeOperDuration = 11.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(2, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // Can change

    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl = 2; // close
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).TimeOperDuration = 0.0;
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).TimeVentDuration = 5.0;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = 20.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(2, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // No change
    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).TimeVentDuration = 11.0;
    state->dataHeatBalFanSys->TempControlType(1) = 1;
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint(1) = 25.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(*state, 1, 1);
    EXPECT_EQ(1, state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData(1).VentilationCtrl); // Can change

    state->dataSystemAvailabilityManager->HybridVentSysAvailMgrData.deallocate();
    state->dataHVACGlobal->HybridVentSysAvailVentCtrl.deallocate();
    state->dataAirLoop->PriAirSysAvailMgr.deallocate();
    state->dataHeatBal->Zone.deallocate();
    state->dataHeatBalFanSys->MAT.deallocate();
    state->dataHeatBal->ZoneMRT.deallocate();
    state->dataContaminantBalance->ZoneAirCO2.deallocate();
    state->dataContaminantBalance->ZoneCO2SetPoint.deallocate();
    state->dataAirLoop->PriAirSysAvailMgr.deallocate();
    state->dataSystemAvailabilityManager->SchedSysAvailMgrData.deallocate();
    state->dataScheduleMgr->Schedule.deallocate();
    state->dataHVACGlobal->ZoneComp.deallocate();
    state->dataHeatBalFanSys->TempControlType.deallocate();
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.deallocate();
}

TEST_F(EnergyPlusFixture, SysAvailManager_NightCycleGetInput)
{

    std::string const idf_objects = delimited_string({

        "  AvailabilityManager:NightCycle,",
        "    VAV Sys 1 Avail,         !- Name",
        "    SysAvailApplicSch,       !- Applicability Schedule Name",
        "    FanAvailSched,           !- Fan Schedule Name",
        "    CycleOnAny,              !- Control Type",
        "    1,                       !- Thermostat Tolerance {deltaC}",
        "    FixedRunTime,            !- Cycling Run Time Control Type",
        "    7200.0;                  !- Cycling Run Time {s}",

        "  Schedule:Compact,",
        "    SysAvailApplicSch,       !- Name",
        "    On/Off,                  !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 9",
        "    For: AllDays,            !- Field 10",
        "    Until: 24:00,1.0;        !- Field 11",

        " Schedule:Compact,",
        "   FanAvailSched,            !- Name",
        "   Fraction,                 !- Schedule Type Limits Name",
        "   Through: 12/31,           !- Field 1",
        "   For: AllDays,             !- Field 2",
        "   Until:  7:00, 0.0,        !- Field 3",
        "   Until: 24:00, 1.0;        !- Field 3",

        "  AvailabilityManager:NightCycle,",
        "    VAV Sys 2 Avail,         !- Name",
        "    SysAvailApplicSch,       !- Applicability Schedule Name",
        "    FanAvailSched,           !- Fan Schedule Name",
        "    CycleOnAny,              !- Control Type",
        "    1,                       !- Thermostat Tolerance {deltaC}",
        "    Thermostat,              !- Cycling Run Time Control Type",
        "    7200.0;                  !- Cycling Run Time {s}",

        "  AvailabilityManager:NightCycle,",
        "    VAV Sys 3 Avail,         !- Name",
        "    SysAvailApplicSch,       !- Applicability Schedule Name",
        "    FanAvailSched,           !- Fan Schedule Name",
        "    CycleOnAny,              !- Control Type",
        "    1,                       !- Thermostat Tolerance {deltaC}",
        "    ThermostatWithMinimumRunTime, !- Cycling Run Time Control Type",
        "    7200.0;                  !- Cycling Run Time {s}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules
    state->dataScheduleMgr->ScheduleInputProcessed = true;
    // get system availability schedule
    SystemAvailabilityManager::GetSysAvailManagerInputs(*state);
    // check the three cycling run time control types
    EXPECT_EQ(3, state->dataSystemAvailabilityManager->NumNCycSysAvailMgrs);
    EXPECT_EQ(state->dataSystemAvailabilityManager->FixedRunTime, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CycRunTimeCntrlType);
    EXPECT_EQ(state->dataSystemAvailabilityManager->Thermostat, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(2).CycRunTimeCntrlType);
    EXPECT_EQ(state->dataSystemAvailabilityManager->ThermostatWithMinimumRunTime,
              state->dataSystemAvailabilityManager->NCycSysAvailMgrData(3).CycRunTimeCntrlType);
}

TEST_F(EnergyPlusFixture, SysAvailManager_NightCycleZone_CalcNCycSysAvailMgr)
{
    int NumZones(1);
    int SysAvailNum = 1;
    int PriAirSysNum = 0;
    int AvailStatus;
    int const ZoneEquipType = 1;
    int const CompNum = 1;

    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(NumZones);
    state->dataHeatBal->Zone(1).Name = "SPACE1-1";
    state->dataHVACGlobal->ZoneComp.allocate(1);
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs.allocate(1);
    state->dataHVACGlobal->ZoneComp(1).TotalNumComp = 1;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).AvailStatus = 0;

    state->dataHeatBalFanSys->TempControlType.allocate(NumZones);
    state->dataHeatBalFanSys->TempTstatAir.allocate(NumZones);
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.allocate(NumZones);
    state->dataHeatBalFanSys->TempControlType(1) = DataHVACGlobals::SingleCoolingSetPoint;
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint(1) = 25.0;
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.1;

    state->dataSystemAvailabilityManager->NCycSysAvailMgrData.allocate(NumZones);
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).Name = "System Avail";
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlType = state->dataSystemAvailabilityManager->CycleOnAny;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).SchedPtr = 1;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).FanSchedPtr = 2;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).TempTolRange = 0.4;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CyclingTimeSteps = 4;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlZoneListName = state->dataHeatBal->Zone(1).Name;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).NumOfCtrlZones = NumZones;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlZonePtrs.allocate(1);
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlZonePtrs(1) = 1;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CoolingZoneListName = state->dataHeatBal->Zone(1).Name;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).NumOfCoolingZones = NumZones;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CoolingZonePtrs = NumZones;
    state->dataScheduleMgr->Schedule.allocate(2);
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 0;

    // Cycling Run Time Control Type = FixedRunTime
    // and current time is within the run time period, starting time is less than stopping time
    state->dataGlobal->SimTimeSteps = 0;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 0.0;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CycRunTimeCntrlType = state->dataSystemAvailabilityManager->FixedRunTime;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 0;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is cycling On
    EXPECT_EQ(DataHVACGlobals::CycleOn, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);
    // starting time is equal to stopping time
    state->dataGlobal->SimTimeSteps = 4;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 2;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is no action mode
    EXPECT_EQ(DataHVACGlobals::NoAction, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = Thermostat,  Run Time has no effect
    // starting time is less than stopping time, control is driven by temp differential
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlType = state->dataSystemAvailabilityManager->CycleOnControlZone;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CycRunTimeCntrlType = state->dataSystemAvailabilityManager->Thermostat;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 0;
    state->dataGlobal->SimTimeSteps = 0;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 0.0;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is cycling On, 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);
    // Cycling Run Time Control Type = Thermostat, Run Time has no effect
    // starting time and stopping time are the same, control is driven by temp differential
    state->dataGlobal->SimTimeSteps = 4;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 0;
    // the unit still cycles on because of high zone air temp
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.1;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // Check that the system is cycling On, run time has no effect, // 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);
    // Cycling Run Time Control Type = Thermostat, Run Time has no effect
    // Reduce zone air temperature, control is driven by temp differential
    state->dataGlobal->SimTimeSteps = 4;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 2;
    // Reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.04;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // Check that the system is no action mode, 25.04 < 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::NoAction, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = ThermostatWithMinimumRunTime and
    // current time is the end of run time period
    state->dataGlobal->SimTimeSteps = 4;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CycRunTimeCntrlType =
        state->dataSystemAvailabilityManager->ThermostatWithMinimumRunTime;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 0;
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.1;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is cycling On, zone air temp is outside T tolerance limits of 0.05, 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);
    // Cycling Run Time Control Type = ThermostatWithMinimumRunTime and
    // current time is the end of run time period
    state->dataGlobal->SimTimeSteps = 4;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 2;
    // Reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.04;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is no action mode, zone air temp is outside T tolerance limits of 0.05, 25.04 < 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::NoAction, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);

    // Test cycle time reset at beginning of day during warmup
    state->dataGlobal->WarmupFlag = true;
    state->dataGlobal->BeginDayFlag = true;
    state->dataGlobal->SimTimeSteps = 96;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    EXPECT_EQ(DataHVACGlobals::NoAction, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);
    EXPECT_EQ(state->dataGlobal->SimTimeSteps, state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StartTime);
    EXPECT_EQ(state->dataGlobal->SimTimeSteps, state->dataHVACGlobal->ZoneComp(1).ZoneCompAvailMgrs(1).StopTime);
}

TEST_F(EnergyPlusFixture, SysAvailManager_NightCycleSys_CalcNCycSysAvailMgr)
{
    int NumZones(1);
    int SysAvailNum = 1;
    int PriAirSysNum = 1;
    int AvailStatus;

    state->dataGlobal->NumOfZones = 1;
    state->dataAirLoop->PriAirSysAvailMgr.allocate(PriAirSysNum);
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData.allocate(NumZones);
    state->dataHeatBalFanSys->TempControlType.allocate(NumZones);
    state->dataHeatBalFanSys->TempTstatAir.allocate(NumZones);
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint.allocate(NumZones);
    state->dataHeatBalFanSys->TempControlType(1) = DataHVACGlobals::SingleCoolingSetPoint;
    state->dataHeatBalFanSys->TempZoneThermostatSetPoint(1) = 25.0;
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.1;
    state->dataHeatBal->Zone.allocate(NumZones);
    state->dataHeatBal->Zone(1).Name = "SPACE1-1";

    state->dataAirLoop->AirToZoneNodeInfo.allocate(1);
    state->dataAirLoop->AirToZoneNodeInfo(1).NumZonesCooled = 1;
    state->dataAirLoop->AirToZoneNodeInfo(1).CoolCtrlZoneNums.allocate(1);
    state->dataAirLoop->AirToZoneNodeInfo(1).CoolCtrlZoneNums(1) = 1;
    state->dataZoneEquip->ZoneEquipConfig.allocate(state->dataGlobal->NumOfZones);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "SPACE1-1";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneNode = 1;

    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).Name = "System Avail";
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlType = state->dataSystemAvailabilityManager->CycleOnAny;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).SchedPtr = 1;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).FanSchedPtr = 2;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).TempTolRange = 0.4;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CyclingTimeSteps = 4;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlZoneListName = state->dataHeatBal->Zone(1).Name;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).NumOfCtrlZones = NumZones;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlZonePtrs.allocate(1);
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlZonePtrs(1) = 1;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CoolingZoneListName = state->dataHeatBal->Zone(1).Name;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).NumOfCoolingZones = NumZones;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CoolingZonePtrs = NumZones;
    state->dataScheduleMgr->Schedule.allocate(2);
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 0;

    // Cycling Run Time Control Type = FixedRunTime
    // and current time is within the run time period, starting time is less than stopping time
    state->dataGlobal->SimTimeSteps = 0;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = 0.0;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CycRunTimeCntrlType = state->dataSystemAvailabilityManager->FixedRunTime;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).PriorAvailStatus = 2;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is cycling On
    EXPECT_EQ(DataHVACGlobals::CycleOn, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);
    // Starting time is equal to stopping time
    state->dataGlobal->SimTimeSteps = 4;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CycRunTimeCntrlType = state->dataSystemAvailabilityManager->FixedRunTime;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 2;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is no action mode because of run time limit
    EXPECT_EQ(DataHVACGlobals::NoAction, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = Thermostat,  Run Time has no effect
    // starting time is less than stopping time, control is driven by temp differential
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CtrlType = state->dataSystemAvailabilityManager->CycleOnControlZone;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CycRunTimeCntrlType = state->dataSystemAvailabilityManager->Thermostat;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 0;
    state->dataGlobal->SimTimeSteps = 0;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = 0.0;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is cycling On, 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = Thermostat, Run Time has no effect
    // starting time and stopping time are the same, control is driven by temp differential
    state->dataGlobal->SimTimeSteps = 4;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 0;
    // reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.1;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is cycling On, run time has no effect, // 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = Thermostat, Run Time has no effect
    // starting time and stopping time are the same, control is driven by temp differential
    state->dataGlobal->SimTimeSteps = 4;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 2;
    // Reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.04;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is no action mode, 25.04 < 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::NoAction, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = ThermostatWithMinimumRunTime and
    // starting time and stopping time are the same, control is driven by temp differential
    state->dataGlobal->SimTimeSteps = 4;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).CycRunTimeCntrlType =
        state->dataSystemAvailabilityManager->ThermostatWithMinimumRunTime;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 0;
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.1;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is cycling On, zone air temp is outside T tolerance limits of 0.05, 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = ThermostatWithMinimumRunTime and
    // starting time and stopping time are the same, control is driven by temp differential
    state->dataGlobal->SimTimeSteps = 4;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus = 2;
    // Reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    state->dataHeatBalFanSys->TempTstatAir(1) = 25.04;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is no action mode, zone air temp is within T tolerance limits of 0.05, 25.04 < 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::NoAction, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);

    // Test cycle time reset at beginning of day during warmup
    state->dataGlobal->WarmupFlag = true;
    state->dataGlobal->BeginDayFlag = true;
    state->dataGlobal->SimTimeSteps = 96;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(*state, SysAvailNum, PriAirSysNum, AvailStatus);
    EXPECT_EQ(DataHVACGlobals::NoAction, state->dataSystemAvailabilityManager->NCycSysAvailMgrData(1).AvailStatus);
    EXPECT_EQ(state->dataGlobal->SimTimeSteps, state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime);
    EXPECT_EQ(state->dataGlobal->SimTimeSteps, state->dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime);
}
