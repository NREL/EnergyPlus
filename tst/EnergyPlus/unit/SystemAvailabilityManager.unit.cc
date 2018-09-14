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

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/ThermalComfort.hh>

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

        " ZoneList,",
        "   List_Zones,              !- Name",
        "   Zone 1,                  !- Zone 1 Name",
        "   Zone 2,                  !- Zone 2 Name",
        "   Zone 3;                  !- Zone 3 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataHeatBalance::NumOfZoneLists = 1;
    DataHeatBalance::ZoneList.allocate(DataHeatBalance::NumOfZoneLists);
    DataHeatBalance::ZoneList(1).Name = "LIST_ZONES";
    DataHeatBalance::ZoneList(1).NumOfZones = 3;
    DataHeatBalance::ZoneList(1).Zone.allocate(3);
    DataHeatBalance::ZoneList(1).Zone(1) = 1;
    DataHeatBalance::ZoneList(1).Zone(2) = 2;
    DataHeatBalance::ZoneList(1).Zone(3) = 3;

    DataHVACGlobals::NumPrimaryAirSys = 2;
    DataAirLoop::PriAirSysAvailMgr.allocate(2);
    DataAirLoop::PriAirSysAvailMgr(1).NumAvailManagers = 1;
    DataAirLoop::PriAirSysAvailMgr(2).NumAvailManagers = 1;

    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerType.allocate(1);
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerName.allocate(1);
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerNum.allocate(1);
    DataAirLoop::PriAirSysAvailMgr(2).AvailManagerType.allocate(1);
    DataAirLoop::PriAirSysAvailMgr(2).AvailManagerName.allocate(1);
    DataAirLoop::PriAirSysAvailMgr(2).AvailManagerNum.allocate(1);

    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerType(1) = 12; // cValidSysAvailManagerTypes( { ......., "AvailabilityManager:OptimumStart" } );
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerName(1) = "OptStart Availability 1";
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerNum(1) = 1;
    DataAirLoop::PriAirSysAvailMgr(2).AvailManagerType(1) = 12; // cValidSysAvailManagerTypes( { ......., "AvailabilityManager:OptimumStart" } );
    DataAirLoop::PriAirSysAvailMgr(2).AvailManagerName(1) = "OptStart Availability 2";
    DataAirLoop::PriAirSysAvailMgr(2).AvailManagerNum(1) = 2;

    DataAirLoop::AirToZoneNodeInfo.allocate(2);
    DataAirLoop::AirToZoneNodeInfo(1).NumZonesCooled = 3;
    DataAirLoop::AirToZoneNodeInfo(1).CoolCtrlZoneNums.allocate(3);
    DataAirLoop::AirToZoneNodeInfo(1).CoolCtrlZoneNums(1) = 1;
    DataAirLoop::AirToZoneNodeInfo(1).CoolCtrlZoneNums(2) = 2;
    DataAirLoop::AirToZoneNodeInfo(1).CoolCtrlZoneNums(3) = 3;

    DataAirLoop::AirToZoneNodeInfo(2).NumZonesCooled = 2;
    DataAirLoop::AirToZoneNodeInfo(2).CoolCtrlZoneNums.allocate(2);
    DataAirLoop::AirToZoneNodeInfo(2).CoolCtrlZoneNums(1) = 4;
    DataAirLoop::AirToZoneNodeInfo(2).CoolCtrlZoneNums(2) = 5;

    DataGlobals::NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(); // read schedules
    ScheduleManager::ScheduleInputProcessed = true;
    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 1;
    DataGlobals::HourOfDay = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::DayOfSim = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 1;
    DataEnvironment::DayOfWeekTomorrow = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues();

    DataZoneEquipment::ZoneEquipAvail.allocate(5);

    DataZoneEquipment::NumOfZones = 5;

    DataHeatBalance::Zone.allocate(DataGlobals::NumOfZones);
    DataHeatBalance::Zone(1).Name = "ZONE 1";
    DataHeatBalance::Zone(2).Name = "ZONE 2";
    DataHeatBalance::Zone(3).Name = "ZONE 3";
    DataHeatBalance::Zone(4).Name = "ZONE 4";
    DataHeatBalance::Zone(5).Name = "ZONE 5";

    DataZoneEquipment::ZoneEquipConfig.allocate(DataZoneEquipment::NumOfZones);

    DataZoneEquipment::ZoneEquipConfig(1).ZoneName = "Zone 1";
    DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum = 1;
    DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = 1;

    DataZoneEquipment::ZoneEquipConfig(2).ZoneName = "Zone 2";
    DataZoneEquipment::ZoneEquipConfig(2).ActualZoneNum = 2;
    DataZoneEquipment::ZoneEquipConfig(2).ZoneNode = 2;

    DataZoneEquipment::ZoneEquipConfig(3).ZoneName = "Zone 3";
    DataZoneEquipment::ZoneEquipConfig(3).ActualZoneNum = 3;
    DataZoneEquipment::ZoneEquipConfig(3).ZoneNode = 3;

    DataZoneEquipment::ZoneEquipConfig(4).ZoneName = "Zone 4";
    DataZoneEquipment::ZoneEquipConfig(4).ActualZoneNum = 4;
    DataZoneEquipment::ZoneEquipConfig(4).ZoneNode = 4;

    DataZoneEquipment::ZoneEquipConfig(5).ZoneName = "Zone 5";
    DataZoneEquipment::ZoneEquipConfig(5).ActualZoneNum = 5;
    DataZoneEquipment::ZoneEquipConfig(5).ZoneNode = 5;

    DataZoneEquipment::ZoneEquipInputsFilled = true;

    DataHeatBalFanSys::TempTstatAir.allocate(5);
    DataHeatBalFanSys::TempTstatAir(1) = 18.0; // all zones have different space temperature
    DataHeatBalFanSys::TempTstatAir(2) = 17.0;
    DataHeatBalFanSys::TempTstatAir(3) = 16.0;
    DataHeatBalFanSys::TempTstatAir(4) = 15.0;
    DataHeatBalFanSys::TempTstatAir(5) = 14.0;

    DataHeatBalFanSys::ZoneThermostatSetPointLo.allocate(5);
    DataHeatBalFanSys::ZoneThermostatSetPointHi.allocate(5);

    DataHeatBalFanSys::ZoneThermostatSetPointLo = 19.0; // all zones use same set point temperature
    DataHeatBalFanSys::ZoneThermostatSetPointHi = 24.0;

    DataZoneControls::OccRoomTSetPointHeat.allocate(5);
    DataZoneControls::OccRoomTSetPointCool.allocate(5);

    DataZoneControls::OccRoomTSetPointHeat = 19.0; // all zones use same set point temperature
    DataZoneControls::OccRoomTSetPointCool = 24.0;

    SystemAvailabilityManager::ManageSystemAvailability(); // 1st time through just gets input

    DataGlobals::WarmupFlag = true;
    DataGlobals::BeginDayFlag = true; // initialize optimum start data to beginning of day data
    DataGlobals::CurrentTime = 1.0;   // set the current time to 1 AM
    SystemAvailabilityManager::ManageSystemAvailability();
    EXPECT_EQ(3, SystemAvailabilityManager::OptStartSysAvailMgrData(1).ATGWCZoneNumLo); // zone 3 is farthest from heating set point
    EXPECT_EQ(
        1, SystemAvailabilityManager::OptStartSysAvailMgrData(1).ATGWCZoneNumHi); // zone 1 is default for cooling set point when heating load exists
    EXPECT_EQ(-3.0, SystemAvailabilityManager::OptStartSysAvailMgrData(1).TempDiffLo);                       // zone 3 is 3C below set point
    EXPECT_EQ(0.0, SystemAvailabilityManager::OptStartSysAvailMgrData(1).TempDiffHi);                        // cooling data did not get set so is 0
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::OptStartSysAvailMgrData(1).AvailStatus); // avail manager should not yet be set

    EXPECT_EQ(DataHVACGlobals::NoAction,
              SystemAvailabilityManager::OptStartSysAvailMgrData(2).AvailStatus); // avail manager should not be set until 6 AM

    DataGlobals::WarmupFlag = false;
    DataGlobals::BeginDayFlag = false; // start processing temp data to find optimum start time
    DataGlobals::CurrentTime = 2.0;    // set the current time to 2 AM
    SystemAvailabilityManager::ManageSystemAvailability();
    // same data as before since zone temps are unchanged
    EXPECT_EQ(3, SystemAvailabilityManager::OptStartSysAvailMgrData(1).ATGWCZoneNumLo); // zone 3 is farthest from heating set point
    EXPECT_EQ(
        1, SystemAvailabilityManager::OptStartSysAvailMgrData(1).ATGWCZoneNumHi); // zone 1 is default for cooling set point when heating load exists
    EXPECT_EQ(-3.0, SystemAvailabilityManager::OptStartSysAvailMgrData(1).TempDiffLo);                       // zone 3 is 3C below set point
    EXPECT_EQ(0.0, SystemAvailabilityManager::OptStartSysAvailMgrData(1).TempDiffHi);                        // cooling data did not get set so is 0
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::OptStartSysAvailMgrData(1).AvailStatus); // avail manager should not yet be set

    EXPECT_EQ(DataHVACGlobals::NoAction,
              SystemAvailabilityManager::OptStartSysAvailMgrData(2).AvailStatus); // avail manager should not be set until 6 AM

    DataGlobals::CurrentTime = 7.0; // set the current time to 7 AM which is past time to pre-start HVAC
    SystemAvailabilityManager::ManageSystemAvailability();

    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::OptStartSysAvailMgrData(1).AvailStatus); // avail manager should be set to cycle on
    EXPECT_EQ(1.5, SystemAvailabilityManager::OptStartSysAvailMgrData(1).NumHoursBeforeOccupancy); // 1.5 hours = 3C from SP divided by 2C/hour

    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::OptStartSysAvailMgrData(2).AvailStatus); // avail manager should be set at 6 AM
}

TEST_F(EnergyPlusFixture, SysAvailManager_NightCycle_ZoneOutOfTolerance)
{
    int NumZones(4);
    DataHeatBalFanSys::TempControlType.allocate(NumZones);
    DataHeatBalFanSys::TempTstatAir.allocate(NumZones);
    DataHeatBalFanSys::TempZoneThermostatSetPoint.allocate(NumZones);
    DataHeatBalFanSys::ZoneThermostatSetPointHi.allocate(NumZones);
    DataHeatBalFanSys::ZoneThermostatSetPointLo.allocate(NumZones);

    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::SingleCoolingSetPoint;
    DataHeatBalFanSys::TempTstatAir(1) = 30.0;
    DataHeatBalFanSys::TempZoneThermostatSetPoint(1) = 25.0;

    DataHeatBalFanSys::TempControlType(2) = DataHVACGlobals::SingleHeatCoolSetPoint;
    DataHeatBalFanSys::TempTstatAir(2) = 25.0;
    DataHeatBalFanSys::TempZoneThermostatSetPoint(2) = 25.0;

    DataHeatBalFanSys::TempControlType(3) = DataHVACGlobals::SingleHeatingSetPoint;
    DataHeatBalFanSys::TempTstatAir(3) = 10.0;
    DataHeatBalFanSys::TempZoneThermostatSetPoint(3) = 20.0;

    DataHeatBalFanSys::TempControlType(4) = DataHVACGlobals::DualSetPointWithDeadBand;
    DataHeatBalFanSys::TempTstatAir(4) = 30.0;
    DataHeatBalFanSys::ZoneThermostatSetPointHi(4) = 25.0;
    DataHeatBalFanSys::ZoneThermostatSetPointLo(4) = 20.0;

    Real64 TempTol = 0.5;
    Array1D_int ZoneNumList;
    ZoneNumList.allocate(NumZones);
    ZoneNumList(1) = 3;
    ZoneNumList(2) = 2;
    ZoneNumList(3) = 1;
    ZoneNumList(4) = 4;

    // Test 1 - One zone is over cooling setpoint, one zone is under heating setpoint
    EXPECT_TRUE(SystemAvailabilityManager::CoolingZoneOutOfTolerance(ZoneNumList, NumZones, TempTol));
    EXPECT_TRUE(SystemAvailabilityManager::HeatingZoneOutOfTolerance(ZoneNumList, NumZones, TempTol));

    // Test 2 - All zones are within tolerance
    DataHeatBalFanSys::TempTstatAir(1) = 25.1;
    DataHeatBalFanSys::TempTstatAir(2) = 24.9;
    DataHeatBalFanSys::TempTstatAir(3) = 19.8;
    DataHeatBalFanSys::TempTstatAir(4) = 23.0;
    EXPECT_FALSE(SystemAvailabilityManager::CoolingZoneOutOfTolerance(ZoneNumList, NumZones, TempTol));
    EXPECT_FALSE(SystemAvailabilityManager::HeatingZoneOutOfTolerance(ZoneNumList, NumZones, TempTol));

    DataHeatBalFanSys::TempControlType.deallocate();
    DataHeatBalFanSys::TempTstatAir.deallocate();
    DataHeatBalFanSys::TempZoneThermostatSetPoint.deallocate();
    DataHeatBalFanSys::ZoneThermostatSetPointHi.deallocate();
    DataHeatBalFanSys::ZoneThermostatSetPointLo.deallocate();
    ZoneNumList.deallocate();
}

TEST_F(EnergyPlusFixture, SysAvailManager_HybridVentilation_OT_CO2Control)
{

    SystemAvailabilityManager::HybridVentSysAvailMgrData.allocate(1);
    DataHVACGlobals::HybridVentSysAvailVentCtrl.allocate(1);
    DataAirLoop::PriAirSysAvailMgr.allocate(1);
    DataHeatBalance::Zone.allocate(1);
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalance::MRT.allocate(1);
    DataContaminantBalance::ZoneAirCO2.allocate(1);
    DataContaminantBalance::ZoneCO2SetPoint.allocate(1);
    DataAirLoop::PriAirSysAvailMgr.allocate(1);
    SystemAvailabilityManager::SchedSysAvailMgrData.allocate(1);
    ScheduleManager::Schedule.allocate(1);
    DataHVACGlobals::ZoneComp.allocate(DataZoneEquipment::NumValidSysAvailZoneComponents);
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempZoneThermostatSetPoint.allocate(1);

    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).Name = "HybridControl";
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).ActualZoneNum = 1;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).AirLoopNum = 1;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).ControlModeSchedPtr = 1;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).UseRainIndicator = false;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MaxWindSpeed = 40.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MinOutdoorTemp = 15.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MaxOutdoorTemp = 35.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MinOutdoorEnth = 20000.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MaxOutdoorEnth = 30000.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MinOutdoorDewPoint = 15.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MaxOutdoorDewPoint = 35.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MinOASched = 2;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MinOperTime = 10.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).MinVentTime = 10.0;

    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).TimeVentDuration = 0.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).TimeOperDuration = 0.0;

    DataHeatBalance::Zone(1).OutDryBulbTemp = 20.0;
    DataHeatBalance::Zone(1).WindSpeed = 5.0;

    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).ControlMode = 5; // 80% acceptance
    ThermalComfort::runningAverageASH = 20.0;
    DataHeatBalFanSys::MAT(1) = 23.0;
    DataHeatBalance::MRT(1) = 27.0;

    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(1, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // Vent open

    DataHeatBalFanSys::MAT(1) = 26.0;
    DataHeatBalance::MRT(1) = 30.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(2, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // System operation

    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).ControlMode = 6; // 90% acceptance
    DataHeatBalFanSys::MAT(1) = 23.0;
    DataHeatBalance::MRT(1) = 27.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(1, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // Vent open

    DataHeatBalFanSys::MAT(1) = 26.0;
    DataHeatBalance::MRT(1) = 30.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(2, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // System operation

    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).ControlMode = 7; // CO2 control with an AirLoop
    DataContaminantBalance::ZoneAirCO2(1) = 900.0;
    DataContaminantBalance::ZoneCO2SetPoint(1) = 800.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).HybridVentMgrConnectedToAirLoop = true;
    DataAirLoop::PriAirSysAvailMgr(1).NumAvailManagers = 1;
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerType.allocate(1);
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerName.allocate(1);
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerNum.allocate(1);
    DataAirLoop::PriAirSysAvailMgr(1).AvailStatus = 1;
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerType(1) = 1; // Scheduled
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerName(1) = "Avail 1";
    DataAirLoop::PriAirSysAvailMgr(1).AvailManagerNum(1) = 1;
    SystemAvailabilityManager::SchedSysAvailMgrData(1).SchedPtr = 1;
    ScheduleManager::Schedule(1).CurrentValue = 1;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(2, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // System operation
    ScheduleManager::Schedule(1).CurrentValue = 0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(1, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // Vent open

    DataContaminantBalance::ZoneAirCO2(1) = 500.0;
    DataContaminantBalance::ZoneCO2SetPoint(1) = 800.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(0, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // No action

    DataHVACGlobals::ZoneComp(1).TotalNumComp = 1; //  CO2 control with zone equipment
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs.allocate(1);
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).AvailStatus = 2;
    DataContaminantBalance::ZoneAirCO2(1) = 900.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).HybridVentMgrConnectedToAirLoop = false;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).SimHybridVentSysAvailMgr = true;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(2, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // System operation
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).AvailStatus = 1;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(1, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // Vent open

    // time duration test
    DataHeatBalance::Zone(1).OutDryBulbTemp = 40.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).ControlMode = 1;     // Temperature control
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl = 1; // Open
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).TimeOperDuration = 5.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(1, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // No change
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).TimeOperDuration = 11.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(2, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // Can change

    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl = 2; // close
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).TimeOperDuration = 0.0;
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).TimeVentDuration = 5.0;
    DataHeatBalance::Zone(1).OutDryBulbTemp = 20.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(2, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // No change
    SystemAvailabilityManager::HybridVentSysAvailMgrData(1).TimeVentDuration = 11.0;
    DataHeatBalFanSys::TempControlType(1) = 1;
    DataHeatBalFanSys::TempZoneThermostatSetPoint(1) = 25.0;
    SystemAvailabilityManager::CalcHybridVentSysAvailMgr(1, 1);
    EXPECT_EQ(1, SystemAvailabilityManager::HybridVentSysAvailMgrData(1).VentilationCtrl); // Can change

    SystemAvailabilityManager::HybridVentSysAvailMgrData.deallocate();
    DataHVACGlobals::HybridVentSysAvailVentCtrl.deallocate();
    DataAirLoop::PriAirSysAvailMgr.deallocate();
    DataHeatBalance::Zone.deallocate();
    DataHeatBalFanSys::MAT.deallocate();
    DataHeatBalance::MRT.deallocate();
    DataContaminantBalance::ZoneAirCO2.deallocate();
    DataContaminantBalance::ZoneCO2SetPoint.deallocate();
    DataAirLoop::PriAirSysAvailMgr.deallocate();
    SystemAvailabilityManager::SchedSysAvailMgrData.deallocate();
    ScheduleManager::Schedule.deallocate();
    DataHVACGlobals::ZoneComp.deallocate();
    DataHeatBalFanSys::TempControlType.deallocate();
    DataHeatBalFanSys::TempZoneThermostatSetPoint.deallocate();
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

    DataGlobals::NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(); // read schedules
    ScheduleManager::ScheduleInputProcessed = true;
    // get system availability schedule
    SystemAvailabilityManager::GetSysAvailManagerInputs();
    // check the three cycling run time control types
    EXPECT_EQ(3, SystemAvailabilityManager::NumNCycSysAvailMgrs);
    EXPECT_EQ(SystemAvailabilityManager::FixedRunTime, SystemAvailabilityManager::NCycSysAvailMgrData(1).CycRunTimeCntrlType);
    EXPECT_EQ(SystemAvailabilityManager::Thermostat, SystemAvailabilityManager::NCycSysAvailMgrData(2).CycRunTimeCntrlType);
    EXPECT_EQ(SystemAvailabilityManager::ThermostatWithMinimumRunTime, SystemAvailabilityManager::NCycSysAvailMgrData(3).CycRunTimeCntrlType);
}

TEST_F(EnergyPlusFixture, SysAvailManager_NightCycleZone_CalcNCycSysAvailMgr)
{
    int NumZones(1);
    int SysAvailNum = 1;
    int PriAirSysNum = 0;
    int AvailStatus;
    int const ZoneEquipType = 1;
    int const CompNum = 1;

    DataGlobals::NumOfZones = 1;
    DataHeatBalance::Zone.allocate(NumZones);
    DataHeatBalance::Zone(1).Name = "SPACE1-1";
    DataHVACGlobals::ZoneComp.allocate(1);
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs.allocate(1);
    DataHVACGlobals::ZoneComp(1).TotalNumComp = 1;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).AvailStatus = 0;

    DataHeatBalFanSys::TempControlType.allocate(NumZones);
    DataHeatBalFanSys::TempTstatAir.allocate(NumZones);
    DataHeatBalFanSys::TempZoneThermostatSetPoint.allocate(NumZones);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::SingleCoolingSetPoint;
    DataHeatBalFanSys::TempZoneThermostatSetPoint(1) = 25.0;
    DataHeatBalFanSys::TempTstatAir(1) = 25.1;

    SystemAvailabilityManager::NCycSysAvailMgrData.allocate(NumZones);
    SystemAvailabilityManager::NCycSysAvailMgrData(1).Name = "System Avail";
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlType = SystemAvailabilityManager::CycleOnAny;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).SchedPtr = 1;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).FanSchedPtr = 2;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).TempTolRange = 0.4;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CyclingTimeSteps = 4;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlZoneListName = DataHeatBalance::Zone(1).Name;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).NumOfCtrlZones = NumZones;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlZonePtrs.allocate(1);
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlZonePtrs(1) = 1;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CoolingZoneListName = DataHeatBalance::Zone(1).Name;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).NumOfCoolingZones = NumZones;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CoolingZonePtrs = NumZones;
    ScheduleManager::Schedule.allocate(2);
    ScheduleManager::Schedule(1).CurrentValue = 1;
    ScheduleManager::Schedule(2).CurrentValue = 0;

    // Cycling Run Time Control Type = FixedRunTime
    // and current time is within the run time period, starting time is less than stopping time
    DataGlobals::SimTimeSteps = 0;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 0.0;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CycRunTimeCntrlType = SystemAvailabilityManager::FixedRunTime;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 0;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is cycling On
    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);
    // starting time is equal to stopping time
    DataGlobals::SimTimeSteps = 4;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 2;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is no action mode
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = Thermostat,  Run Time has no effect
    // starting time is less than stopping time, control is driven by temp differential
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlType = SystemAvailabilityManager::CycleOnControlZone;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CycRunTimeCntrlType = SystemAvailabilityManager::Thermostat;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 0;
    DataGlobals::SimTimeSteps = 0;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 0.0;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is cycling On, 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);
    // Cycling Run Time Control Type = Thermostat, Run Time has no effect
    // starting time and stopping time are the same, control is driven by temp differential
    DataGlobals::SimTimeSteps = 4;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 0;
    // the unit still cycles on because of high zone air temp
    DataHeatBalFanSys::TempTstatAir(1) = 25.1;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // Check that the system is cycling On, run time has no effect, // 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);
    // Cycling Run Time Control Type = Thermostat, Run Time has no effect
    // Reduce zone air temperature, control is driven by temp differential
    DataGlobals::SimTimeSteps = 4;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 2;
    // Reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    DataHeatBalFanSys::TempTstatAir(1) = 25.04;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // Check that the system is no action mode, 25.04 < 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = ThermostatWithMinimumRunTime and
    // current time is the end of run time period
    DataGlobals::SimTimeSteps = 4;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CycRunTimeCntrlType = SystemAvailabilityManager::ThermostatWithMinimumRunTime;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 0;
    DataHeatBalFanSys::TempTstatAir(1) = 25.1;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is cycling On, zone air temp is outside T tolerance limits of 0.05, 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);
    // Cycling Run Time Control Type = ThermostatWithMinimumRunTime and
    // current time is the end of run time period
    DataGlobals::SimTimeSteps = 4;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StartTime = 4.0;
    DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 2;
    // Reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    DataHeatBalFanSys::TempTstatAir(1) = 25.04;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    // check that the system is no action mode, zone air temp is outside T tolerance limits of 0.05, 25.04 < 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);

    // Test cycle time reset at beginning of day during warmup
    DataGlobals::WarmupFlag = true;
    DataGlobals::BeginDayFlag = true;
    DataGlobals::SimTimeSteps = 96;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);
    EXPECT_EQ(DataGlobals::SimTimeSteps, DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StartTime);
    EXPECT_EQ(DataGlobals::SimTimeSteps, DataHVACGlobals::ZoneComp(1).ZoneCompAvailMgrs(1).StopTime);
}

TEST_F(EnergyPlusFixture, SysAvailManager_NightCycleSys_CalcNCycSysAvailMgr)
{
    int NumZones(1);
    int SysAvailNum = 1;
    int PriAirSysNum = 1;
    int AvailStatus;

    DataGlobals::NumOfZones = 1;
    DataAirLoop::PriAirSysAvailMgr.allocate(PriAirSysNum);
    SystemAvailabilityManager::NCycSysAvailMgrData.allocate(NumZones);
    DataHeatBalFanSys::TempControlType.allocate(NumZones);
    DataHeatBalFanSys::TempTstatAir.allocate(NumZones);
    DataHeatBalFanSys::TempZoneThermostatSetPoint.allocate(NumZones);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::SingleCoolingSetPoint;
    DataHeatBalFanSys::TempZoneThermostatSetPoint(1) = 25.0;
    DataHeatBalFanSys::TempTstatAir(1) = 25.1;
    DataHeatBalance::Zone.allocate(NumZones);
    DataHeatBalance::Zone(1).Name = "SPACE1-1";

    DataAirLoop::AirToZoneNodeInfo.allocate(1);
    DataAirLoop::AirToZoneNodeInfo(1).NumZonesCooled = 1;
    DataAirLoop::AirToZoneNodeInfo(1).CoolCtrlZoneNums.allocate(1);
    DataAirLoop::AirToZoneNodeInfo(1).CoolCtrlZoneNums(1) = 1;
    DataZoneEquipment::ZoneEquipConfig.allocate(DataZoneEquipment::NumOfZones);
    DataZoneEquipment::ZoneEquipConfig(1).ZoneName = "SPACE1-1";
    DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum = 1;
    DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = 1;

    SystemAvailabilityManager::NCycSysAvailMgrData(1).Name = "System Avail";
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlType = SystemAvailabilityManager::CycleOnAny;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).SchedPtr = 1;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).FanSchedPtr = 2;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).TempTolRange = 0.4;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CyclingTimeSteps = 4;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlZoneListName = DataHeatBalance::Zone(1).Name;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).NumOfCtrlZones = NumZones;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlZonePtrs.allocate(1);
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlZonePtrs(1) = 1;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CoolingZoneListName = DataHeatBalance::Zone(1).Name;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).NumOfCoolingZones = NumZones;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CoolingZonePtrs = NumZones;
    ScheduleManager::Schedule.allocate(2);
    ScheduleManager::Schedule(1).CurrentValue = 1;
    ScheduleManager::Schedule(2).CurrentValue = 0;

    // Cycling Run Time Control Type = FixedRunTime
    // and current time is within the run time period, starting time is less than stopping time
    DataGlobals::SimTimeSteps = 0;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StartTime = 0.0;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CycRunTimeCntrlType = SystemAvailabilityManager::FixedRunTime;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).PriorAvailStatus = 2;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is cycling On
    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);
    // Starting time is equal to stopping time
    DataGlobals::SimTimeSteps = 4;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CycRunTimeCntrlType = SystemAvailabilityManager::FixedRunTime;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 2;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is no action mode because of run time limit
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = Thermostat,  Run Time has no effect
    // starting time is less than stopping time, control is driven by temp differential
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CtrlType = SystemAvailabilityManager::CycleOnControlZone;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CycRunTimeCntrlType = SystemAvailabilityManager::Thermostat;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 0;
    DataGlobals::SimTimeSteps = 0;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StartTime = 0.0;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is cycling On, 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = Thermostat, Run Time has no effect
    // starting time and stopping time are the same, control is driven by temp differential
    DataGlobals::SimTimeSteps = 4;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 0;
    // reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    DataHeatBalFanSys::TempTstatAir(1) = 25.1;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is cycling On, run time has no effect, // 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = Thermostat, Run Time has no effect
    // starting time and stopping time are the same, control is driven by temp differential
    DataGlobals::SimTimeSteps = 4;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 2;
    // Reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    DataHeatBalFanSys::TempTstatAir(1) = 25.04;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is no action mode, 25.04 < 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = ThermostatWithMinimumRunTime and
    // starting time and stopping time are the same, control is driven by temp differential
    DataGlobals::SimTimeSteps = 4;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).CycRunTimeCntrlType = SystemAvailabilityManager::ThermostatWithMinimumRunTime;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 0;
    DataHeatBalFanSys::TempTstatAir(1) = 25.1;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is cycling On, zone air temp is outside T tolerance limits of 0.05, 25.1 > 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::CycleOn, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);

    // Cycling Run Time Control Type = ThermostatWithMinimumRunTime and
    // starting time and stopping time are the same, control is driven by temp differential
    DataGlobals::SimTimeSteps = 4;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StartTime = 4.0;
    DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StopTime = 4.0;
    SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus = 2;
    // Reduce zone air temperature within the tolerance (0.05) to turn off night cycling
    DataHeatBalFanSys::TempTstatAir(1) = 25.04;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus);
    // Check that the system is no action mode, zone air temp is within T tolerance limits of 0.05, 25.04 < 25.0 + 0.05
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);

    // Test cycle time reset at beginning of day during warmup
    DataGlobals::WarmupFlag = true;
    DataGlobals::BeginDayFlag = true;
    DataGlobals::SimTimeSteps = 96;
    SystemAvailabilityManager::CalcNCycSysAvailMgr(SysAvailNum, PriAirSysNum, AvailStatus);
    EXPECT_EQ(DataHVACGlobals::NoAction, SystemAvailabilityManager::NCycSysAvailMgrData(1).AvailStatus);
    EXPECT_EQ(DataGlobals::SimTimeSteps, DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StartTime);
    EXPECT_EQ(DataGlobals::SimTimeSteps, DataAirLoop::PriAirSysAvailMgr(PriAirSysNum).StopTime);
}
