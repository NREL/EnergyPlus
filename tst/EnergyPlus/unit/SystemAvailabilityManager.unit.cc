// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>

using namespace EnergyPlus;

TEST_F( EnergyPlusFixture, SysAvailManager_OptimumStart )
{

	std::string const idf_objects = delimited_string( {

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

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	DataHeatBalance::NumOfZoneLists = 1;
	DataHeatBalance::ZoneList.allocate( DataHeatBalance::NumOfZoneLists );
	DataHeatBalance::ZoneList( 1 ).Name = "LIST_ZONES";
	DataHeatBalance::ZoneList( 1 ).NumOfZones = 3;
	DataHeatBalance::ZoneList( 1 ).Zone.allocate( 3 );
	DataHeatBalance::ZoneList( 1 ).Zone( 1 ) = 1;
	DataHeatBalance::ZoneList( 1 ).Zone( 2 ) = 2;
	DataHeatBalance::ZoneList( 1 ).Zone( 3 ) = 3;

	DataHVACGlobals::NumPrimaryAirSys = 2;
	DataAirLoop::PriAirSysAvailMgr.allocate( 2 );
	DataAirLoop::PriAirSysAvailMgr( 1 ).NumAvailManagers = 1;
	DataAirLoop::PriAirSysAvailMgr( 2 ).NumAvailManagers = 1;

	DataAirLoop::PriAirSysAvailMgr( 1 ).AvailManagerType.allocate( 1 );
	DataAirLoop::PriAirSysAvailMgr( 1 ).AvailManagerName.allocate( 1 );
	DataAirLoop::PriAirSysAvailMgr( 1 ).AvailManagerNum.allocate( 1 );
	DataAirLoop::PriAirSysAvailMgr( 2 ).AvailManagerType.allocate( 1 );
	DataAirLoop::PriAirSysAvailMgr( 2 ).AvailManagerName.allocate( 1 );
	DataAirLoop::PriAirSysAvailMgr( 2 ).AvailManagerNum.allocate( 1 );

	DataAirLoop::PriAirSysAvailMgr( 1 ).AvailManagerType( 1 ) = 12; // cValidSysAvailManagerTypes( { ......., "AvailabilityManager:OptimumStart" } );
	DataAirLoop::PriAirSysAvailMgr( 1 ).AvailManagerName( 1 ) = "OptStart Availability 1";
	DataAirLoop::PriAirSysAvailMgr( 1 ).AvailManagerNum( 1 ) = 1;
	DataAirLoop::PriAirSysAvailMgr( 2 ).AvailManagerType( 1 ) = 12; // cValidSysAvailManagerTypes( { ......., "AvailabilityManager:OptimumStart" } );
	DataAirLoop::PriAirSysAvailMgr( 2 ).AvailManagerName( 1 ) = "OptStart Availability 2";
	DataAirLoop::PriAirSysAvailMgr( 2 ).AvailManagerNum( 1 ) = 2;

	DataAirLoop::AirToZoneNodeInfo.allocate( 2 );
	DataAirLoop::AirToZoneNodeInfo( 1 ).NumZonesCooled = 3;
	DataAirLoop::AirToZoneNodeInfo( 1 ).CoolCtrlZoneNums.allocate( 3 );
	DataAirLoop::AirToZoneNodeInfo( 1 ).CoolCtrlZoneNums( 1 ) = 1;
	DataAirLoop::AirToZoneNodeInfo( 1 ).CoolCtrlZoneNums( 2 ) = 2;
	DataAirLoop::AirToZoneNodeInfo( 1 ).CoolCtrlZoneNums( 3 ) = 3;

	DataAirLoop::AirToZoneNodeInfo( 2 ).NumZonesCooled = 2;
	DataAirLoop::AirToZoneNodeInfo( 2 ).CoolCtrlZoneNums.allocate( 2 );
	DataAirLoop::AirToZoneNodeInfo( 2 ).CoolCtrlZoneNums( 1 ) = 4;
	DataAirLoop::AirToZoneNodeInfo( 2 ).CoolCtrlZoneNums( 2 ) = 5;

	DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
	DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
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
	DataEnvironment::DayOfYear_Schedule = General::JulianDay( DataEnvironment::Month, DataEnvironment::DayOfMonth, 1 );
	ScheduleManager::UpdateScheduleValues();

	DataZoneEquipment::ZoneEquipAvail.allocate( 5 );

	DataZoneEquipment::NumOfZones = 5;

	DataHeatBalance::Zone.allocate( DataGlobals::NumOfZones );
	DataHeatBalance::Zone( 1).Name = "ZONE 1";
	DataHeatBalance::Zone( 2).Name = "ZONE 2";
	DataHeatBalance::Zone( 3).Name = "ZONE 3";
	DataHeatBalance::Zone( 4).Name = "ZONE 4";
	DataHeatBalance::Zone( 5).Name = "ZONE 5";

	DataZoneEquipment::ZoneEquipConfig.allocate( DataZoneEquipment::NumOfZones ); 

	DataZoneEquipment::ZoneEquipConfig( 1 ).ZoneName = "Zone 1";
	DataZoneEquipment::ZoneEquipConfig( 1 ).ActualZoneNum = 1;
	DataZoneEquipment::ZoneEquipConfig( 1 ).ZoneNode = 1;
	DataZoneEquipment::ZoneEquipConfig( 1 ).AirLoopNum = 1;

	DataZoneEquipment::ZoneEquipConfig( 2 ).ZoneName = "Zone 2";
	DataZoneEquipment::ZoneEquipConfig( 2 ).ActualZoneNum = 2;
	DataZoneEquipment::ZoneEquipConfig( 2 ).ZoneNode = 2;
	DataZoneEquipment::ZoneEquipConfig( 2 ).AirLoopNum = 1;

	DataZoneEquipment::ZoneEquipConfig( 3 ).ZoneName = "Zone 3";
	DataZoneEquipment::ZoneEquipConfig( 3 ).ActualZoneNum = 3;
	DataZoneEquipment::ZoneEquipConfig( 3 ).ZoneNode = 3;
	DataZoneEquipment::ZoneEquipConfig( 3 ).AirLoopNum = 1;

	DataZoneEquipment::ZoneEquipConfig( 4 ).ZoneName = "Zone 4";
	DataZoneEquipment::ZoneEquipConfig( 4 ).ActualZoneNum = 4;
	DataZoneEquipment::ZoneEquipConfig( 4 ).ZoneNode = 4;
	DataZoneEquipment::ZoneEquipConfig( 4 ).AirLoopNum = 2;

	DataZoneEquipment::ZoneEquipConfig( 5 ).ZoneName = "Zone 5";
	DataZoneEquipment::ZoneEquipConfig( 5 ).ActualZoneNum = 5;
	DataZoneEquipment::ZoneEquipConfig( 5 ).ZoneNode = 5;
	DataZoneEquipment::ZoneEquipConfig( 5 ).AirLoopNum = 2;

	DataZoneEquipment::ZoneEquipInputsFilled = true;


	DataHeatBalFanSys::TempTstatAir.allocate( 5 );
	DataHeatBalFanSys::TempTstatAir( 1 ) = 18.0; // all zones have different space temperature
	DataHeatBalFanSys::TempTstatAir( 2 ) = 17.0;
	DataHeatBalFanSys::TempTstatAir( 3 ) = 16.0;
	DataHeatBalFanSys::TempTstatAir( 4 ) = 15.0;
	DataHeatBalFanSys::TempTstatAir( 5 ) = 14.0;

	DataHeatBalFanSys::ZoneThermostatSetPointLo.allocate( 5 );
	DataHeatBalFanSys::ZoneThermostatSetPointHi.allocate( 5 );

	DataHeatBalFanSys::ZoneThermostatSetPointLo = 19.0; // all zones use same set point temperature
	DataHeatBalFanSys::ZoneThermostatSetPointHi = 24.0;

	DataZoneControls::OccRoomTSetPointHeat.allocate( 5 );
	DataZoneControls::OccRoomTSetPointCool.allocate( 5 );

	DataZoneControls::OccRoomTSetPointHeat = 19.0; // all zones use same set point temperature
	DataZoneControls::OccRoomTSetPointCool = 24.0;

	SystemAvailabilityManager::	ManageSystemAvailability(); // 1st time through just gets input
	
	DataGlobals::WarmupFlag = true;
	DataGlobals::BeginDayFlag = true; // initialize optimum start data to beginning of day data
	DataGlobals::CurrentTime = 1.0; // set the current time to 1 AM
	SystemAvailabilityManager::ManageSystemAvailability();
	EXPECT_EQ( 3, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).ATGWCZoneNumLo ); // zone 3 is farthest from heating set point
	EXPECT_EQ( 1, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).ATGWCZoneNumHi ); // zone 1 is default for cooling set point when heating load exists
	EXPECT_EQ( -3.0, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).TempDiffLo ); // zone 3 is 3C below set point
	EXPECT_EQ(  0.0, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).TempDiffHi ); // cooling data did not get set so is 0
	EXPECT_EQ( DataHVACGlobals::NoAction, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).AvailStatus ); // avail manager should not yet be set

	EXPECT_EQ( DataHVACGlobals::NoAction, SystemAvailabilityManager::OptStartSysAvailMgrData( 2 ).AvailStatus ); // avail manager should not be set until 6 AM

	DataGlobals::WarmupFlag = false;
	DataGlobals::BeginDayFlag = false; // start processing temp data to find optimum start time
	DataGlobals::CurrentTime = 2.0; // set the current time to 2 AM
	SystemAvailabilityManager::	ManageSystemAvailability();
	// same data as before since zone temps are unchanged
	EXPECT_EQ( 3, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).ATGWCZoneNumLo ); // zone 3 is farthest from heating set point
	EXPECT_EQ( 1, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).ATGWCZoneNumHi ); // zone 1 is default for cooling set point when heating load exists
	EXPECT_EQ( -3.0, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).TempDiffLo ); // zone 3 is 3C below set point
	EXPECT_EQ(  0.0, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).TempDiffHi ); // cooling data did not get set so is 0
	EXPECT_EQ( DataHVACGlobals::NoAction, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).AvailStatus ); // avail manager should not yet be set

	EXPECT_EQ( DataHVACGlobals::NoAction, SystemAvailabilityManager::OptStartSysAvailMgrData( 2 ).AvailStatus ); // avail manager should not be set until 6 AM

	DataGlobals::CurrentTime = 7.0; // set the current time to 7 AM which is past time to pre-start HVAC
	SystemAvailabilityManager::	ManageSystemAvailability();

	EXPECT_EQ( DataHVACGlobals::CycleOn, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).AvailStatus ); // avail manager should be set to cycle on
	EXPECT_EQ( 1.5, SystemAvailabilityManager::OptStartSysAvailMgrData( 1 ).NumHoursBeforeOccupancy ); // 1.5 hours = 3C from SP divided by 2C/hour

	EXPECT_EQ( DataHVACGlobals::CycleOn, SystemAvailabilityManager::OptStartSysAvailMgrData( 2 ).AvailStatus ); // avail manager should be set at 6 AM

}

TEST_F( EnergyPlusFixture, SysAvailManager_NightCycle_ZoneOutOfTolerance )
{
	int NumZones( 4 );
	DataHeatBalFanSys::TempControlType.allocate( NumZones );
	DataHeatBalFanSys::TempTstatAir.allocate( NumZones );
	DataHeatBalFanSys::TempZoneThermostatSetPoint.allocate( NumZones );
	DataHeatBalFanSys::ZoneThermostatSetPointHi.allocate( NumZones );
	DataHeatBalFanSys::ZoneThermostatSetPointLo.allocate( NumZones );

	DataHeatBalFanSys::TempControlType( 1 ) = DataHVACGlobals::SingleCoolingSetPoint;
	DataHeatBalFanSys::TempTstatAir( 1 ) = 30.0;
	DataHeatBalFanSys::TempZoneThermostatSetPoint( 1 ) = 25.0;

	DataHeatBalFanSys::TempControlType( 2 ) = DataHVACGlobals::SingleHeatCoolSetPoint;
	DataHeatBalFanSys::TempTstatAir( 2 ) = 25.0;
	DataHeatBalFanSys::TempZoneThermostatSetPoint( 2 ) = 25.0;

	DataHeatBalFanSys::TempControlType( 3 ) = DataHVACGlobals::SingleHeatingSetPoint;
	DataHeatBalFanSys::TempTstatAir( 3 ) = 10.0;
	DataHeatBalFanSys::TempZoneThermostatSetPoint( 3 ) = 20.0;

	DataHeatBalFanSys::TempControlType( 4 ) = DataHVACGlobals::DualSetPointWithDeadBand;
	DataHeatBalFanSys::TempTstatAir( 4 ) = 30.0;
	DataHeatBalFanSys::ZoneThermostatSetPointHi( 4 ) = 25.0;
	DataHeatBalFanSys::ZoneThermostatSetPointLo( 4 ) = 20.0;

	Real64 TempTol = 0.5;
	Array1D_int ZoneNumList;
	ZoneNumList.allocate( NumZones );
	ZoneNumList( 1 ) = 3;
	ZoneNumList( 2 ) = 2;
	ZoneNumList( 3 ) = 1;
	ZoneNumList( 4 ) = 4;

	// Test 1 - One zone is over cooling setpoint, one zone is under heating setpoint
	EXPECT_TRUE( SystemAvailabilityManager::CoolingZoneOutOfTolerance( ZoneNumList, NumZones, TempTol ) );
	EXPECT_TRUE( SystemAvailabilityManager::HeatingZoneOutOfTolerance( ZoneNumList, NumZones, TempTol ) );

	// Test 2 - All zones are within tolerance
	DataHeatBalFanSys::TempTstatAir( 1 ) = 25.1;
	DataHeatBalFanSys::TempTstatAir( 2 ) = 24.9;
	DataHeatBalFanSys::TempTstatAir( 3 ) = 19.8;
	DataHeatBalFanSys::TempTstatAir( 4 ) = 23.0;
	EXPECT_FALSE( SystemAvailabilityManager::CoolingZoneOutOfTolerance( ZoneNumList, NumZones, TempTol ) );
	EXPECT_FALSE( SystemAvailabilityManager::HeatingZoneOutOfTolerance( ZoneNumList, NumZones, TempTol ) );

	DataHeatBalFanSys::TempControlType.deallocate();
	DataHeatBalFanSys::TempTstatAir.deallocate();
	DataHeatBalFanSys::TempZoneThermostatSetPoint.deallocate();
	DataHeatBalFanSys::ZoneThermostatSetPointHi.deallocate();
	DataHeatBalFanSys::ZoneThermostatSetPointLo.deallocate();
	ZoneNumList.deallocate();

}
