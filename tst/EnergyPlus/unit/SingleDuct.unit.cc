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

// EnergyPlus::Standalone ERV Unit Tests

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <DataAirLoop.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalance.hh>
#include <DataLoopNode.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <HeatBalanceManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SimulationManager.hh>
#include <SingleDuct.hh>
#include <ZoneAirLoopEquipmentManager.hh>

using namespace EnergyPlus;
using namespace SimulationManager;
using namespace DataSizing;
using DataHeatBalance::Zone;

TEST_F(EnergyPlusFixture, VAVNoReheatTerminalUnitSchedule)
{
    std::string const idf_objects = delimited_string({
        "Version,8.4;",
        "  Zone,",
        "    Zone 1;                !- Name",
        "ZoneHVAC:EquipmentConnections,",
        "    Zone 1,                !- Zone Name",
        "    Zone 1 Equipment,             !- Zone Conditioning Equipment List Name",
        "    Zone 1 Supply Inlet,       !- Zone Air Inlet Node or NodeList Name",
        "    ,      !- Zone Air Exhaust Node or NodeList Name",
        "    Zone 1 Air Node,           !- Zone Air Node Name",
        "    Zone 1 Return Node;       !- Zone Return Air Node Name",
        "ZoneHVAC:EquipmentList,",
        "    Zone 1 Equipment,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    Zone 1 ADU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "ZoneHVAC:AirDistributionUnit,",
        "    Zone 1 ADU,    !- Name",
        "    Zone 1 Supply Inlet,     !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "    Zone 1 VAV No Reheat;           !- Air Terminal Name",
        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "    Zone 1 VAV No Reheat,    !- Name",
        "    AlwaysOff,               !- Availability Schedule Name",
        "    Zone 1 Supply Inlet,     !- Air Outlet Node Name",
        "    Zone 1 Zone Equip Inlet, !- Air Inlet Node Name",
        "    2.0,                     !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    0.5;                     !- Constant Minimum Air Flow Fraction",
        "Schedule:Constant,",
        "    AlwaysOff,               !- Name",
        "    ,                        !- Schedule Type Limits Name",
        "    0;                       !- Hourly Value",
        "Schedule:Constant,",
        "    AlwaysOn,               !- Name",
        "    ,                        !- Schedule Type Limits Name",
        "    1;                       !- Hourly Value",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(); // read schedules
    ScheduleManager::ScheduleInputProcessed = true;
    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::JulianDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0);
    ScheduleManager::UpdateScheduleValues();

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1();
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    SingleDuct::GetSysInput();
    EXPECT_TRUE(compare_err_stream(""));
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;

    // node number table
    //  1   Zone 1 Air Node
    //  2   Zone 1 Return Node
    //  3   Zone 1 Supply Inlet
    //  4   Zone 1 Zone Equip Inlet

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);

    // Setup for Zone 1 VAV No Reheat
    int SysNum = 1;
    int ZoneNum = 1;
    int ZoneNodeNum = 1;
    int InletNodeNum = 4;
    bool FirstHVACIteration = true;
    Real64 SysMinMassFlow = 1.0 * DataEnvironment::StdRhoAir; // From inputs for Zone 1 VAV
    Real64 SysMaxMassFlow = 2.0 * DataEnvironment::StdRhoAir; // From inputs for Zone 1 VAV

    // Test with heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate

    // First test - AlwaysOff Schedule - expecting no flow
    SingleDuct::Sys(SysNum).SchedPtr = 1;
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMinMassFlow;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlow;
    DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru SingleDuct::InitSys for this terminal unit
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    DataGlobals::BeginEnvrnFlag = false;
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
    SingleDuct::SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    EXPECT_EQ(0.0, SingleDuct::SysOutlet(SysNum).AirMassFlowRateMaxAvail);
    EXPECT_EQ(0.0, SingleDuct::SysOutlet(SysNum).AirMassFlowRate);

    // Second test - AlwaysOn Schedule - expecting flow
    // Reset flows and switch to AlwaysOn Schedule
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMinMassFlow;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlow;
    SingleDuct::Sys(SysNum).SchedPtr = 2;
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
    SingleDuct::SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    EXPECT_EQ(SysMaxMassFlow, SingleDuct::SysOutlet(SysNum).AirMassFlowRateMaxAvail);
    EXPECT_EQ(SysMinMassFlow, SingleDuct::SysOutlet(SysNum).AirMassFlowRate);

    // Test with cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Cooling load - expect max flow rate

    // First test - AlwaysOff Schedule - expecting no flow
    SingleDuct::Sys(SysNum).SchedPtr = 1;
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMinMassFlow;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlow;
    DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru SingleDuct::InitSys for this terminal unit
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    DataGlobals::BeginEnvrnFlag = false;
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
    SingleDuct::SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    EXPECT_EQ(0.0, SingleDuct::SysOutlet(SysNum).AirMassFlowRateMaxAvail);
    EXPECT_EQ(0.0, SingleDuct::SysOutlet(SysNum).AirMassFlowRate);

    // Second test - AlwaysOn Schedule - expecting flow
    // Reset flows and switch to AlwaysOn Schedule
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMinMassFlow;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlow;
    SingleDuct::Sys(SysNum).SchedPtr = 2;
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
    SingleDuct::SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    EXPECT_EQ(SysMaxMassFlow, SingleDuct::SysOutlet(SysNum).AirMassFlowRateMaxAvail);
    EXPECT_EQ(SysMaxMassFlow, SingleDuct::SysOutlet(SysNum).AirMassFlowRate);

    // Cleanup
    DataHeatBalFanSys::TempControlType.deallocate();
    DataZoneEnergyDemands::ZoneSysEnergyDemand.deallocate();
}

TEST_F(EnergyPlusFixture, VAVReheatTerminalUnitSchedule)
{
    std::string const idf_objects = delimited_string({
        "Version,8.4;",
        "  Zone,",
        "    Zone 1;                !- Name",
        "ZoneHVAC:EquipmentConnections,",
        "    Zone 1,                !- Zone Name",
        "    Zone 1 Equipment,             !- Zone Conditioning Equipment List Name",
        "    Zone 1 Supply Inlet,       !- Zone Air Inlet Node or NodeList Name",
        "    ,      !- Zone Air Exhaust Node or NodeList Name",
        "    Zone 1 Air Node,           !- Zone Air Node Name",
        "    Zone 1 Return Node;       !- Zone Return Air Node Name",
        "ZoneHVAC:EquipmentList,",
        "    Zone 1 Equipment,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    Zone 1 ADU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "ZoneHVAC:AirDistributionUnit,",
        "    Zone 1 ADU,    !- Name",
        "    Zone 1 Supply Inlet,     !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:Reheat,  !- Air Terminal Object Type",
        "    Zone 1 VAV Reheat;           !- Air Terminal Name",
        "AirTerminal:SingleDuct:VAV:Reheat,",
        "    Zone 1 VAV Reheat,       !- Name",
        "    AlwaysOff,               !- Availability Schedule Name",
        "    Zone 1 VAV Reheat Coil Air Inlet,  !- Damper Air Outlet Node Name",
        "    Zone 1 Zone Equip Inlet, !- Air Inlet Node Name",
        "    1.0,                     !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    0.25,                     !- Constant Minimum Air Flow Fraction",
        "    ,                        !- Fixed Minimum Air Flow Rate{m3/s}",
        "    ,                        !- Minimum Air Flow Fraction Schedule Name",
        "    Coil:Heating:Electric,   !- Reheat Coil Object Type",
        "    Zone 1 Reheat Coil,      !- Reheat Coil Name",
        "    ,                        !- Maximum Hot Water or Steam Flow Rate{m3/s}",
        "    ,                        !- Minimum Hot Water or Steam Flow Rate{m3/s}",
        "    Zone 1 Supply Inlet,     !- Air Outlet Node Name",
        "    0.001,                   !- Convergence Tolerance",
        "    ,                        !- Damper Heating Action",
        "    ,                        !- Maximum Flow per Zone Floor Area During Reheat",
        "    ;                        !- Maximum Flow Fraction During Reheat",
        "Coil:Heating:Electric,",
        "    Zone 1 Reheat Coil,      !- Name",
        "    ,                        !- Availability Schedule Name",
        "    1,                       !- Efficiency",
        "    100,                     !- Nominal Capacity of the Coil {W}",
        "    Zone 1 VAV Reheat Coil Air Inlet,  !- Air Inlet Node Name",
        "    Zone 1 Supply Inlet,     !- Air Outlet Node Name",
        "    ;                        !- Temperature Setpoint Node Name",
        "Schedule:Constant,",
        "    AlwaysOff,               !- Name",
        "    ,                        !- Schedule Type Limits Name",
        "    0;                       !- Hourly Value",
        "Schedule:Constant,",
        "    AlwaysOn,               !- Name",
        "    ,                        !- Schedule Type Limits Name",
        "    1;                       !- Hourly Value",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(); // read schedules
    ScheduleManager::ScheduleInputProcessed = true;
    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::JulianDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0);
    ScheduleManager::UpdateScheduleValues();

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1();
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    SingleDuct::GetSysInput();
    EXPECT_TRUE(compare_err_stream(""));
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;

    // node number table
    //  1   Zone 1 Air Node
    //  2   Zone 1 Return Node
    //  3   Zone 1 Supply Inlet
    //  4   Zone 1 VAV Reheat Coil Air Inlet
    //  5   Zone 1 Zone Equip Inlet

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);

    // Setup for Zone 1 VAV No Reheat
    int SysNum = 1;
    int ZoneNum = 1;
    int ZoneNodeNum = 1;
    int InletNodeNum = 5;
    bool FirstHVACIteration = true;
    Real64 SysMinMassFlow = 0.25 * DataEnvironment::StdRhoAir; // From inputs for Zone 1 VAV
    Real64 SysMaxMassFlow = 1.0 * DataEnvironment::StdRhoAir;  // From inputs for Zone 1 VAV

    // Test with heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate

    // First test - AlwaysOff Schedule - expecting no flow
    SingleDuct::Sys(SysNum).SchedPtr = 1;
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMinMassFlow;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlow;
    DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru SingleDuct::InitSys for this terminal unit
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    DataGlobals::BeginEnvrnFlag = false;
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
    SingleDuct::SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    EXPECT_EQ(0.0, SingleDuct::SysOutlet(SysNum).AirMassFlowRateMaxAvail);
    EXPECT_EQ(0.0, SingleDuct::SysOutlet(SysNum).AirMassFlowRate);

    // Second test - AlwaysOn Schedule - expecting flow
    // Reset flows and switch to AlwaysOn Schedule
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMinMassFlow;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlow;
    SingleDuct::Sys(SysNum).SchedPtr = 2;
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
    SingleDuct::SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    EXPECT_EQ(SysMaxMassFlow, SingleDuct::SysOutlet(SysNum).AirMassFlowRateMaxAvail);
    EXPECT_EQ(SysMinMassFlow, SingleDuct::SysOutlet(SysNum).AirMassFlowRate);

    // Test with cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Cooling load - expect max flow rate

    // First test - AlwaysOff Schedule - expecting no flow
    SingleDuct::Sys(SysNum).SchedPtr = 1;
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMinMassFlow;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlow;
    DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru SingleDuct::InitSys for this terminal unit
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    DataGlobals::BeginEnvrnFlag = false;
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
    SingleDuct::SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    EXPECT_EQ(0.0, SingleDuct::SysOutlet(SysNum).AirMassFlowRateMaxAvail);
    EXPECT_EQ(0.0, SingleDuct::SysOutlet(SysNum).AirMassFlowRate);

    // Second test - AlwaysOn Schedule - expecting flow
    // Reset flows and switch to AlwaysOn Schedule
    DataLoopNode::Node(InletNodeNum).MassFlowRate = SysMinMassFlow;
    DataLoopNode::Node(InletNodeNum).MassFlowRateMaxAvail = SysMaxMassFlow;
    SingleDuct::Sys(SysNum).SchedPtr = 2;
    FirstHVACIteration = true;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    FirstHVACIteration = false;
    SingleDuct::InitSys(SysNum, FirstHVACIteration); // Run thru init a second time with FirstHVACIteration set to false
    SingleDuct::SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum);
    EXPECT_EQ(SysMaxMassFlow, SingleDuct::SysOutlet(SysNum).AirMassFlowRateMaxAvail);
    EXPECT_EQ(SysMaxMassFlow, SingleDuct::SysOutlet(SysNum).AirMassFlowRate);

    // Cleanup
    DataHeatBalFanSys::TempControlType.deallocate();
    DataZoneEnergyDemands::ZoneSysEnergyDemand.deallocate();
}

TEST_F(EnergyPlusFixture, SingleDuct_ZeroFloorAreaTest)
{
    // AUTHOR: R. Raustad, FSEC
    // DATE WRITTEN: Sep 2015

    std::string const idf_objects = delimited_string({
        "Version,8.5;",

        " Output:Diagnostics, DisplayExtraWarnings;",
        "  Timestep, 4;",

        "BUILDING, SingleDuct_ZeroFloorAreaTest, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",

        "SimulationControl, YES, YES, NO, YES, NO;",

        "  Site:Location,",
        "    Miami Intl Ap FL USA TMY3 WMO=722020E,    !- Name",
        "    25.82,                   !- Latitude {deg}",
        "    -80.30,                  !- Longitude {deg}",
        "    -5.00,                   !- Time Zone {hr}",
        "    11;                      !- Elevation {m}",

        "SizingPeriod:DesignDay,",
        " Miami Intl Ap Ann Clg 1% Condns DB/MCWB, !- Name",
        " 7,                        !- Month",
        " 21,                       !- Day of Month",
        " SummerDesignDay,          !- Day Type",
        " 31.7,                     !- Maximum Dry - Bulb Temperature{ C }",
        " 10.0,                      !- Daily Dry - Bulb Temperature Range{ deltaC }",
        " ,                         !- Dry - Bulb Temperature Range Modifier Type",
        " ,                         !- Dry - Bulb Temperature Range Modifier Day Schedule Name",
        " Wetbulb,                  !- Humidity Condition Type",
        " 22.7,                     !- Wetbulb or DewPoint at Maximum Dry - Bulb{ C }",
        " ,                         !- Humidity Condition Day Schedule Name",
        " ,                         !- Humidity Ratio at Maximum Dry - Bulb{ kgWater / kgDryAir }",
        " ,                         !- Enthalpy at Maximum Dry - Bulb{ J / kg }",
        " ,                         !- Daily Wet - Bulb Temperature Range{ deltaC }",
        " 101217.,                  !- Barometric Pressure{ Pa }",
        " 3.8,                      !- Wind Speed{ m / s }",
        " 340,                      !- Wind Direction{ deg }",
        " No,                       !- Rain Indicator",
        " No,                       !- Snow Indicator",
        " No,                       !- Daylight Saving Time Indicator",
        " ASHRAEClearSky,           !- Solar Model Indicator",
        " ,                         !- Beam Solar Day Schedule Name",
        " ,                         !- Diffuse Solar Day Schedule Name",
        " ,                         !- ASHRAE Clear Sky Optical Depth for Beam Irradiance( taub ) { dimensionless }",
        " ,                         !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance( taud ) { dimensionless }",
        " 1.00;                     !- Sky Clearness",

        "SizingPeriod:DesignDay,",
        " Miami Intl Ap Ann Htg 99.6% Condns DB, !- Name",
        " 1,                        !- Month",
        " 21,                       !- Day of Month",
        " WinterDesignDay,          !- Day Type",
        " 8.7,                      !- Maximum Dry - Bulb Temperature{ C }",
        " 0.0,                      !- Daily Dry - Bulb Temperature Range{ deltaC }",
        " ,                         !- Dry - Bulb Temperature Range Modifier Type",
        " ,                         !- Dry - Bulb Temperature Range Modifier Day Schedule Name",
        " Wetbulb,                  !- Humidity Condition Type",
        " 8.7,                      !- Wetbulb or DewPoint at Maximum Dry - Bulb{ C }",
        " ,                         !- Humidity Condition Day Schedule Name",
        " ,                         !- Humidity Ratio at Maximum Dry - Bulb{ kgWater / kgDryAir }",
        " ,                         !- Enthalpy at Maximum Dry - Bulb{ J / kg }",
        " ,                         !- Daily Wet - Bulb Temperature Range{ deltaC }",
        " 101217.,                  !- Barometric Pressure{ Pa }",
        " 3.8,                      !- Wind Speed{ m / s }",
        " 340,                      !- Wind Direction{ deg }",
        " No,                       !- Rain Indicator",
        " No,                       !- Snow Indicator",
        " No,                       !- Daylight Saving Time Indicator",
        " ASHRAEClearSky,           !- Solar Model Indicator",
        " ,                         !- Beam Solar Day Schedule Name",
        " ,                         !- Diffuse Solar Day Schedule Name",
        " ,                         !- ASHRAE Clear Sky Optical Depth for Beam Irradiance( taub ) { dimensionless }",
        " ,                         !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance( taud ) { dimensionless }",
        " 0.00;                     !- Sky Clearness",

        "Zone,",
        "  Space,                   !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  2.4,                     !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",

        "Zone,",
        "  Space2,                !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  2.4,                     !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",

        "Sizing:Zone,",
        " Space,                    !- Zone or ZoneList Name",
        " SupplyAirTemperature,     !- Zone Cooling Design Supply Air Temperature Input Method",
        " 12.,                      !- Zone Cooling Design Supply Air Temperature{ C }",
        " ,                         !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
        " SupplyAirTemperature,     !- Zone Heating Design Supply Air Temperature Input Method",
        " 50.,                      !- Zone Heating Design Supply Air Temperature{ C }",
        " ,                         !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
        " 0.008,                    !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        " 0.008,                    !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        " SZ DSOA,                  !- Design Specification Outdoor Air Object Name",
        " 0.0,                      !- Zone Heating Sizing Factor",
        " 0.0,                      !- Zone Cooling Sizing Factor",
        " DesignDay,                !- Cooling Design Air Flow Method",
        " 0,                        !- Cooling Design Air Flow Rate{ m3 / s }",
        " ,                         !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " ,                         !- Cooling Minimum Air Flow{ m3 / s }",
        " ,                         !- Cooling Minimum Air Flow Fraction",
        " DesignDay,                !- Heating Design Air Flow Method",
        " 0,                        !- Heating Design Air Flow Rate{ m3 / s }",
        " ,                         !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " ,                         !- Heating Maximum Air Flow{ m3 / s }",
        " ;                         !- Heating Maximum Air Flow Fraction",

        "Sizing:Zone,",
        " Space2,                   !- Zone or ZoneList Name",
        " SupplyAirTemperature,     !- Zone Cooling Design Supply Air Temperature Input Method",
        " 12.,                      !- Zone Cooling Design Supply Air Temperature{ C }",
        " ,                         !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
        " SupplyAirTemperature,     !- Zone Heating Design Supply Air Temperature Input Method",
        " 50.,                      !- Zone Heating Design Supply Air Temperature{ C }",
        " ,                         !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
        " 0.008,                    !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        " 0.008,                    !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        " SZ DSOA,                  !- Design Specification Outdoor Air Object Name",
        " 0.0,                      !- Zone Heating Sizing Factor",
        " 0.0,                      !- Zone Cooling Sizing Factor",
        " DesignDay,                !- Cooling Design Air Flow Method",
        " 0,                        !- Cooling Design Air Flow Rate{ m3 / s }",
        " ,                         !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " ,                         !- Cooling Minimum Air Flow{ m3 / s }",
        " ,                         !- Cooling Minimum Air Flow Fraction",
        " DesignDay,                !- Heating Design Air Flow Method",
        " 0,                        !- Heating Design Air Flow Rate{ m3 / s }",
        " ,                         !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " ,                         !- Heating Maximum Air Flow{ m3 / s }",
        " ;                         !- Heating Maximum Air Flow Fraction",

        "DesignSpecification:OutdoorAir,",
        " SZ DSOA,                  !- Name",
        " flow/zone,                !- Outdoor Air Method",
        " 0.0,                      !- Outdoor Air Flow per Person{ m3 / s - person }",
        " 0.05,                     !- Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " 0.05;                     !- Outdoor Air Flow per Zone{ m3 / s }",

        "  Sizing:System,",
        "    VAV Sys 1,               !- AirLoop Name",
        "    sensible,                !- Type of Load to Size On",
        "    autosize,                !- Design Outdoor Air Flow Rate {m3/s}",
        "    0.3,                     !- Central Heating Maximum System Air Flow Ratio",
        "    4.5,                     !- Preheat Design Temperature {C}",
        "    0.008,                   !- Preheat Design Humidity Ratio {kgWater/kgDryAir}",
        "    11.0,                    !- Precool Design Temperature {C}",
        "    0.008,                   !- Precool Design Humidity Ratio {kgWater/kgDryAir}",
        "    12.8,                    !- Central Cooling Design Supply Air Temperature {C}",
        "    25.0,                    !- Central Heating Design Supply Air Temperature {C}",
        "    noncoincident,           !- Type of Zone Sum to Use",
        "    no,                      !- 100% Outdoor Air in Cooling",
        "    no,                      !- 100% Outdoor Air in Heating",
        "    0.008,                   !- Central Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.008,                   !- Central Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Supply Air Flow Rate Per Floor Area During Cooling Operation {m3/s-m2}",
        "    ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "    ,                        !- Design Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    0,                       !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Supply Air Flow Rate Per Floor Area During Heating Operation {m3/s-m2}",
        "    ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "    ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "    ,                        !- Design Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}",
        "    ,                        !- System Outdoor Air Method",
        "    1.0,                     !- Zone Maximum Outdoor Air Fraction {dimensionless}",
        "    CoolingDesignCapacity,   !- Cooling Design Capacity Method",
        "    autosize,                !- Cooling Design Capacity {W}",
        "    ,                        !- Cooling Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Cooling Design Capacity",
        "    HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "    autosize,                !- Heating Design Capacity {W}",
        "    ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Heating Design Capacity",
        "    VAV;                     !- Central Cooling Capacity Control Method",

        "  ZoneHVAC:EquipmentConnections,",
        "    Space,                    !- Zone Name",
        "    Space Eq,                 !- Zone Conditioning Equipment List Name",
        "    Space In Node,            !- Zone Air Inlet Node or NodeList Name",
        "    Space Out Node,           !- Zone Air Exhaust Node or NodeList Name",
        "    Space Node,               !- Zone Air Node Name",
        "    Space Ret Node;           !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentConnections,",
        "    Space2,                   !- Zone Name",
        "    Space2 Eq,                !- Zone Conditioning Equipment List Name",
        "    Space2 In Node,           !- Zone Air Inlet Node or NodeList Name",
        "    Space2 Out Node,          !- Zone Air Exhaust Node or NodeList Name",
        "    Space2 Node,              !- Zone Air Node Name",
        "    Space2 Ret Node;          !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentList,",
        "    Space Eq,                !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE1-1 ATU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No - Load Sequence",

        "  ZoneHVAC:AirDistributionUnit,",
        "    SPACE1-1 ATU,            !- Name",
        "    Space In Node,           !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:Reheat,  !- Air Terminal Object Type",
        "    SPACE1-1 VAV Reheat;     !- Air Terminal Name",

        "  ZoneHVAC:EquipmentList,",
        "    Space2 Eq,               !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE2-1 ATU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No - Load Sequence",

        "  ZoneHVAC:AirDistributionUnit,",
        "    SPACE2-1 ATU,            !- Name",
        "    Space2 In Node,          !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:Reheat,  !- Air Terminal Object Type",
        "    SPACE2-1 VAV Reheat;     !- Air Terminal Name",

        "  AirTerminal:SingleDuct:VAV:Reheat,",
        "    SPACE1-1 VAV Reheat,     !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    SPACE1-1 Zone Coil Air In Node,  !- Damper Air Outlet Node Name",
        "    SPACE1-1 ATU In Node,    !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    0.3,                     !- Constant Minimum Air Flow Fraction",
        "    ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
        "    ,                        !- Minimum Air Flow Fraction Schedule Name",
        "    Coil:Heating:Fuel,        !- Reheat Coil Object Type",
        "    SPACE1-1 Zone Coil,      !- Reheat Coil Name",
        "    0.0,                     !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        "    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        "    SPACE In Node,           !- Air Outlet Node Name",
        "    0.001,                   !- Convergence Tolerance",
        "    ReverseWithLimits,       !- Damper Heating Action",
        "    AUTOCALCULATE,           !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}",
        "    AUTOCALCULATE;           !- Maximum Flow Fraction During Reheat",

        "  AirTerminal:SingleDuct:VAV:Reheat,",
        "    SPACE2-1 VAV Reheat,     !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    SPACE2-1 Zone Coil Air In Node,  !- Damper Air Outlet Node Name",
        "    SPACE2-1 ATU In Node,    !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    0.3,                     !- Constant Minimum Air Flow Fraction",
        "    ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
        "    ,                        !- Minimum Air Flow Fraction Schedule Name",
        "    Coil:Heating:Fuel,        !- Reheat Coil Object Type",
        "    SPACE2-1 Zone Coil,      !- Reheat Coil Name",
        "    0.0,                     !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        "    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        "    SPACE2 In Node,          !- Air Outlet Node Name",
        "    0.001,                   !- Convergence Tolerance",
        "    ReverseWithLimits,       !- Damper Heating Action",
        "    AUTOCALCULATE,           !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}",
        "    AUTOCALCULATE;           !- Maximum Flow Fraction During Reheat",

        "  BranchList,",
        "    VAV Sys 1 Branches,      !- Name",
        "    VAV Sys 1 Main Branch;   !- Branch 1 Name",

        "  Branch,",
        "    VAV Sys 1 Main Branch,   !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
        "    OA Sys 1,                !- Component 1 Name",
        "    VAV Sys 1 Inlet Node,    !- Component 1 Inlet Node Name",
        "    Mixed Air Node 1,        !- Component 1 Outlet Node Name",
        "    CoilSystem:Cooling:DX,   !- Component 2 Object Type",
        "    DX Cooling Coil System 1,!- Component 2 Name",
        "    Mixed Air Node 1,        !- Component 2 Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Component 2 Outlet Node Name",
        "    Coil:Heating:Fuel,        !- Component 3 Object Type",
        "    Main Heating Coil 1,     !- Component 3 Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Component 3 Inlet Node Name",
        "    Main Heating Coil 1 Outlet Node,  !- Component 3 Outlet Node Name",
        "    Fan:VariableVolume,      !- Component 4 Object Type",
        "    Supply Fan 1,            !- Component 4 Name",
        "    Main Heating Coil 1 Outlet Node,  !- Component 4 Inlet Node Name",
        "    VAV Sys 1 Outlet Node;   !- Component 4 Outlet Node Name",

        "  AirLoopHVAC,",
        "    VAV Sys 1,               !- Name",
        "    ,                        !- Controller List Name",
        "    VAV Sys 1 Avail List,    !- Availability Manager List Name",
        "    autosize,                !- Design Supply Air Flow Rate {m3/s}",
        "    VAV Sys 1 Branches,      !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    VAV Sys 1 Inlet Node,    !- Supply Side Inlet Node Name",
        "    Demand Out Node,         !- Demand Side Outlet Node Name",
        "    Zone Eq In Node,         !- Demand Side Inlet Node Names",
        "    VAV Sys 1 Outlet Node;   !- Supply Side Outlet Node Names",

        "  AirLoopHVAC:SupplyPath,",
        "    Zone Supply Air Path 1,  !- Name",
        "    Zone Eq In Node,         !- Supply Air Path Inlet Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
        "    Zone Supply Air Splitter;  !- Component 1 Name",

        "  AirLoopHVAC:ZoneSplitter,",
        "    Zone Supply Air Splitter,  !- Name",
        "    Zone Eq In Node,         !- Inlet Node Name",
        "    SPACE1-1 ATU In Node,    !- Outlet 1 Node Name",
        "    SPACE2-1 ATU In Node;    !- Outlet 2 Node Name",

        "  AirLoopHVAC:ReturnPath,",
        "    ReturnAirPath1,          !- Name",
        "    Demand Out Node,         !- Return Air Path Outlet Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
        "    Zone Return Air Mixer;   !- Component 1 Name",

        "  AirLoopHVAC:ZoneMixer,",
        "    Zone Return Air Mixer,   !- Name",
        "    Demand Out Node,         !- Outlet Node Name",
        "    Space Ret Node,          !- Inlet 1 Node Name",
        "    Space2 Ret Node;         !- Inlet 2 Node Name",

        "  AirLoopHVAC:ControllerList,",
        "    OA Sys 1 Controllers,    !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    OA Controller 1;         !- Controller 1 Name",

        "  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys 1 Equipment,      !- Name",
        "    OutdoorAir:Mixer,        !- Component 3 Object Type",
        "    OA Mixing Box 1;         !- Component 3 Name",

        "  AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys 1,                !- Name",
        "    OA Sys 1 Controllers,    !- Controller List Name",
        "    OA Sys 1 Equipment,      !- Outdoor Air Equipment List Name",
        "    VAV Sys 1 Avail List;    !- Availability Manager List Name",

        "  OutdoorAir:Node,",
        "    Main Cooling Coil 1 Condenser Node,  !- Name",
        "    -1.0;                    !- Height Above Ground {m}",

        "  OutdoorAir:NodeList,",
        "    OA Sys Inlet Node;       !- Node or NodeList Name 1",

        "  OutdoorAir:Mixer,",
        "    OA Mixing Box 1,         !- Name",
        "    Mixed Air Node 1,        !- Mixed Air Node Name",
        "    OA Sys Inlet Node,       !- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node,  !- Relief Air Stream Node Name",
        "    VAV Sys 1 Inlet Node;    !- Return Air Stream Node Name",

        "  AvailabilityManagerAssignmentList,",
        "    VAV Sys 1 Avail List,    !- Name",
        "    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
        "    VAV Sys 1 Avail;         !- Availability Manager 1 Name",

        "  AvailabilityManager:Scheduled,",
        "    VAV Sys 1 Avail,         !- Name",
        "    AvailSched;              !- Schedule Name",

        "  SetpointManager:Scheduled,",
        "    Supply Air Temp Manager 1,  !- Name",
        "    Temperature,             !- Control Variable",
        "    HTGSETP_SCH,             !- Schedule Name",
        "    VAV Sys 1 Outlet Node; !- Setpoint Node or NodeList Name",

        "  SetpointManager:MixedAir,",
        "    Mixed Air and Coil Exit Temp Manager 1,  !- Name",
        "    Temperature,             !- Control Variable",
        "    VAV Sys 1 Outlet Node,   !- Reference Setpoint Node Name",
        "    Main Heating Coil 1 Outlet Node,  !- Fan Inlet Node Name",
        "    VAV Sys 1 Outlet Node,   !- Fan Outlet Node Name",
        "    Mixed Air Node List;     !- Setpoint Node or NodeList Name",

        "  NodeList,",
        "    Mixed Air Node List,",
        "    Main Heating Coil 1 Outlet Node,",
        "    Main Cooling Coil 1 Outlet Node,",
        "    Mixed Air Node 1;",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
        "    VAV Sys 1 Inlet Node,    !- Return Air Node Name",
        "    Mixed Air Node 1,        !- Mixed Air Node Name",
        "    OA Sys Inlet Node,       !- Actuator Node Name",
        "    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    FixedDryBulb,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    19.,                     !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    4.,                      !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    FixedMinimum,            !- Minimum Limit Type",
        "    AvailSched;              !- Minimum Outdoor Air Schedule Name",

        "  Coil:Heating:Fuel,",
        "    SPACE1-1 Zone Coil,      !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    Gas,                     !- Fuel Type",
        "    0.8,                     !- Gas Burner Efficiency",
        "    autosize,                !- Nominal Capacity {W}",
        "    SPACE1-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
        "    Space In Node;          !- Air Outlet Node Name",

        "  Coil:Heating:Fuel,",
        "    SPACE2-1 Zone Coil,      !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    Gas,                     !- Fuel Type",
        "    0.8,                     !- Gas Burner Efficiency",
        "    autosize,                !- Nominal Capacity {W}",
        "    SPACE2-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
        "    Space2 In Node;          !- Air Outlet Node Name",

        "  CoilSystem:Cooling:DX,",
        "    DX Cooling Coil System 1,!- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    Mixed Air Node 1,        !- DX Cooling Coil System Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- DX Cooling Coil System Outlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- DX Cooling Coil System Sensor Node Name",
        "    Coil:Cooling:DX:SingleSpeed,!- Cooling Coil Object Type",
        "    Main Cooling Coil 1;     !- Cooling Coil Name",

        "  Coil:Cooling:DX:SingleSpeed,",
        "    Main Cooling Coil 1,   !- Name",
        "    AvailSched,            !- Availability Schedule Name",
        "    autosize,              !- Gross Rated Total Cooling Capacity { W }",
        "    autosize,              !- Gross Rated Sensible Heat Ratio",
        "    4.40,                  !- Gross Rated Cooling COP { W / W }",
        "    autosize,              !- Rated Air Flow Rate { m3 / s }",
        "    ,                      !- Rated Evaporator Fan Power Per Volume Flow Rate { W / ( m3 / s ) }",
        "    Mixed Air Node 1,      !- Air Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,    !- Air Outlet Node Name",
        "    Biquadratic,           !- Total Cooling Capacity Function of Temperature Curve Name",
        "    Cubic,                 !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    Biquadratic,           !- Energy Input Ratio Function of Temperature Curve Name",
        "    Cubic,                 !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    Cubic,                 !- Part Load Fraction Correlation Curve Name",
        "   ,                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    0.0,                   !- Nominal Time for Condensate Removal to Begin",
        "    0.0,                   !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
        "    0.0,                   !- Maximum Cycling Rate",
        "    0.0,                   !- Latent Capacity Time Constant",
        "    Main Cooling Coil 1 Condenser Node, !- Condenser Air Inlet Node Name",
        "    AirCooled,             !- Condenser Type",
        "    0.0,                   !- Evaporative Condenser Effectiveness",
        "    ,                      !- Evaporative Condenser Air Flow Rate",
        "    ,                      !- Evaporative Condenser Pump Rated Power Consumption",
        "    0.0,                   !- Crankcase Heater Capacity",
        "    10.0;                  !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",

        "  Coil:Heating:Fuel,",
        "    Main heating Coil 1,     !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    Gas,                     !- Fuel Type",
        "    0.8,                     !- Gas Burner Efficiency",
        "    autosize,                !- Nominal Capacity {W}",
        "    Main Cooling Coil 1 Outlet Node,  !- Air Inlet Node Name",
        "    Main Heating Coil 1 Outlet Node,  !- Air Outlet Node Name",
        "    Main Heating Coil 1 Outlet Node;  !- Temperature Setpoint Node Name",

        "  Fan:VariableVolume,",
        "    Supply Fan 1,            !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    600.0,                   !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    FixedFlowRate,           !- Fan Power Minimum Flow Rate Input Method",
        "    ,                        !- Fan Power Minimum Flow Fraction",
        "    0.35326,                 !- Fan Power Minimum Air Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    0.0015302446,            !- Fan Power Coefficient 1",
        "    0.0052080574,            !- Fan Power Coefficient 2",
        "    1.1086242,               !- Fan Power Coefficient 3",
        "    -0.11635563,             !- Fan Power Coefficient 4",
        "    0.000,                   !- Fan Power Coefficient 5",
        "    Main Heating Coil 1 Outlet Node,  !- Air Inlet Node Name",
        "    VAV Sys 1 Outlet Node;   !- Air Outlet Node Name",

        "Construction,",
        " INT-WALL-1,               !- Name",
        " GP02,                     !- Outside Layer",
        " AL21,                     !- Layer 2",
        " GP02;                     !- Layer 3",

        "Material,",
        " GP02,                     !- Name",
        " MediumSmooth,             !- Roughness",
        " 1.5900001E-02,            !- Thickness{ m }",
        " 0.1600000,                !- Conductivity{ W / m - K }",
        " 801.0000,                 !- Density{ kg / m3 }",
        " 837.0000,                 !- Specific Heat{ J / kg - K }",
        " 0.9000000,                !- Thermal Absorptance",
        " 0.7500000,                !- Solar Absorptance",
        " 0.7500000;                !- Visible Absorptance",

        "Material:AirGap,",
        " AL21,                     !- Name",
        " 0.1570000;                !- Thermal Resistance{ m2 - K / W }",

        "Construction,",
        "FLOOR-SLAB-1,              !- Name",
        "CC03,                      !- Outside Layer",
        "CP01;                      !- Layer 2",

        "Material,",
        " CC03,                     !- Name",
        " MediumRough,              !- Roughness",
        " 0.1016000,                !- Thickness{ m }",
        " 1.310000,                 !- Conductivity{ W / m - K }",
        " 2243.000,                 !- Density{ kg / m3 }",
        " 837.0000,                 !- Specific Heat{ J / kg - K }",
        " 0.9000000,                !- Thermal Absorptance",
        " 0.6500000,                !- Solar Absorptance",
        " 0.6500000;                !- Visible Absorptance",

        "Material:NoMass,",
        " CP01,                     !- Name",
        " Rough,                    !- Roughness",
        " 0.3670000,                !- Thermal Resistance{ m2 - K / W }",
        " 0.9000000,                !- Thermal Absorptance",
        " 0.7500000,                !- Solar Absorptance",
        " 0.7500000;                !- Visible Absorptance",

        "Construction,",
        " CLNG-1,                   !- Name",
        " MAT-CLNG-1;               !- Outside Layer",

        "Material:NoMass,",
        " MAT-CLNG-1,               !- Name",
        " Rough,                    !- Roughness",
        " 0.652259290,              !- Thermal Resistance{ m2 - K / W }",
        " 0.65,                     !- Thermal Absorptance",
        " 0.65,                     !- Solar Absorptance",
        " 0.65;                     !- Visible Absorptance",

        "BuildingSurface:Detailed,",
        " SPACE-W1,                  !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
        " Outdoors,                 !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " SunExposed,               !- Sun Exposure",
        " WindExposed,              !- Wind Exposure",
        " 0.50000,                  !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
        " 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE-C1,                     !- Name",
        " CEILING,                  !- Surface Type",
        " CLNG-1,                   !- Construction Name",
        " Space,                    !- Zone Name",
        " Outdoors,                 !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 2 {m}",
        " 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 3 {m}",
        " 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        // Issue 5273 - zone floor area = 0 causes overflow of TU Design Size Maximum Flow per Zone Floor Area during Reheat
        //"BuildingSurface:Detailed,",
        //" SPACE-F1,                     !- Name",
        //" FLOOR,                    !- Surface Type",
        //" FLOOR-SLAB-1,             !- Construction Name",
        //" Space,                    !- Zone Name",
        //" Ground,                   !- Outside Boundary Condition",
        //" ,                         !- Outside Boundary Condition Object",
        //" NoSun,                    !- Sun Exposure",
        //" NoWind,                   !- Wind Exposure",
        //" 0.0,                      !- View Factor to Ground",
        //" 4,                        !- Number of Vertices",
        //" 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 1 {m}",
        //" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        //" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        //" 3.7, 3.7, 0.0;            !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE-W2,                     !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
        " 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE-W3,                     !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 0.0, 0.0, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE-W4,                     !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 26.8, 3.7, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 3.7, 3.7, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

        "ZoneControl:Thermostat,",
        " Space Thermostat,         !- Name",
        " Space,                    !- Zone or ZoneList Name",
        " Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        " ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        " Space DualSPSched;        !- Control 1 Name",

        "ZoneControl:Thermostat,",
        " Space2 Thermostat,        !- Name",
        " Space2,                   !- Zone or ZoneList Name",
        " Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        " ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        " Space DualSPSched;        !- Control 1 Name",

        "Schedule:Compact,",
        " Dual Zone Control Type Sched,  !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00,4;           !- Field 3",

        "ThermostatSetpoint:DualSetpoint,",
        " Space DualSPSched,        !- Name",
        " HTGSETP_SCH,              !- Heating Setpoint Temperature Schedule Name",
        " CLGSETP_SCH;              !- Cooling Setpoint Temperature Schedule Name",

        "Schedule:Compact,",
        " CLGSETP_SCH,              !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 19",
        " Until: 24:00,24.0;        !- Field 20",

        "Schedule:Compact,",
        " HTGSETP_SCH,              !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 22",
        " Until: 24:00, 20.0;       !- Field 23",

        "BuildingSurface:Detailed,",
        " SPACE2-W1,               !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space2,                   !- Zone Name",
        " Outdoors,                 !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " SunExposed,               !- Sun Exposure",
        " WindExposed,              !- Wind Exposure",
        " 0.50000,                  !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
        " 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-C1,                  !- Name",
        " CEILING,                  !- Surface Type",
        " CLNG-1,                   !- Construction Name",
        " Space2,                   !- Zone Name",
        " Outdoors,                 !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 2 {m}",
        " 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 3 {m}",
        " 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-F1,                  !- Name",
        " FLOOR,                    !- Surface Type",
        " FLOOR-SLAB-1,             !- Construction Name",
        " Space2,                   !- Zone Name",
        " Ground,                   !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 1 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 3.7, 3.7, 0.0;            !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-W2,                  !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space2,                   !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
        " 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-W3,                  !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space2,                   !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 0.0, 0.0, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-W4,                  !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space2,                   !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 26.8, 3.7, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 3.7, 3.7, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

        "OutdoorAir:NodeList,",
        "  OutsideAirInletNodes;    !- Node or NodeList Name 1",

        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",

        "Schedule:Compact,",
        "  AvailSched,              !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 3",
        "  For: AllDays,            !- Field 4",
        "  Until: 24:00,1.0;        !- Field 5",

        "Schedule:Compact,",
        "  FanOpModeSchedule,       !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,1.0;        !- Field 7",

        "Curve:Biquadratic,",
        "  Biquadratic,             !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  5,                       !- Minimum Value of x",
        "  40,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  30,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "Curve:Cubic,",
        "  Cubic,                   !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0,                       !- Coefficient4 x**3",
        "  11,                      !- Minimum Value of x",
        "  30,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    OutputProcessor::TimeValue.allocate(2);

    ManageSimulation(); // run the design day over the warmup period (24 hrs, 25 days)

    // compare_err_stream( "" ); // just for debugging

    // zone floor area of zone 1 = 0, zone 2 > 0. Expect TU MaxAirVolFlowRateDuringReheat = 0 only for zone 1.
    // this test isn't relevant anymore since defaulting is done differently
    Real64 MaxAirVolFlowRateDuringReheatDes = min(FinalZoneSizing(1).DesHeatVolFlowMax, SingleDuct::Sys(1).MaxAirVolFlowRate);
    // Real64 MaxAirVolFlowRateDuringReheatDes = min( 0.002032 * SingleDuct::Sys( 1 ).ZoneFloorArea, SingleDuct::Sys( 1 ).MaxAirVolFlowRate );
    // apply limit based on min stop
    MaxAirVolFlowRateDuringReheatDes =
        max(MaxAirVolFlowRateDuringReheatDes, (SingleDuct::Sys(1).MaxAirVolFlowRate * SingleDuct::Sys(1).ZoneMinAirFrac));

    // This isn't relevant any more since the default is calculated differently
    Real64 MaxAirVolFractionDuringReheatDes = min(1.0, (FinalZoneSizing(1).DesHeatVolFlowMax / SingleDuct::Sys(1).MaxAirVolFlowRate));
    // Real64 MaxAirVolFractionDuringReheatDes = min( 1.0, ( 0.002032 * SingleDuct::Sys( 1 ).ZoneFloorArea / SingleDuct::Sys( 1 ).MaxAirVolFlowRate )
    // ); apply limit based on min stop
    MaxAirVolFractionDuringReheatDes = max(MaxAirVolFractionDuringReheatDes, SingleDuct::Sys(1).ZoneMinAirFrac);
    // apply model math
    MaxAirVolFlowRateDuringReheatDes =
        min(max(MaxAirVolFlowRateDuringReheatDes, MaxAirVolFractionDuringReheatDes * SingleDuct::Sys(1).MaxAirVolFlowRate),
            SingleDuct::Sys(1).MaxAirVolFlowRate);
    // EXPECT zone floor area == 0, others as calculated above
    EXPECT_EQ(SingleDuct::Sys(1).ZoneFloorArea, 0.0);
    EXPECT_NEAR(SingleDuct::Sys(1).MaxAirVolFlowRateDuringReheat, MaxAirVolFlowRateDuringReheatDes, 0.0000000000001);
    EXPECT_NEAR(MaxAirVolFractionDuringReheatDes, SingleDuct::Sys(1).MaxAirVolFractionDuringReheat, 0.0000000000001);

    MaxAirVolFlowRateDuringReheatDes = min(FinalZoneSizing(2).DesHeatVolFlowMax, SingleDuct::Sys(2).MaxAirVolFlowRate);
    MaxAirVolFlowRateDuringReheatDes =
        max(MaxAirVolFlowRateDuringReheatDes, (SingleDuct::Sys(2).MaxAirVolFlowRate * SingleDuct::Sys(2).ZoneMinAirFrac));
    MaxAirVolFractionDuringReheatDes = min(1.0, (FinalZoneSizing(2).DesHeatVolFlowMax / SingleDuct::Sys(2).MaxAirVolFlowRate));
    MaxAirVolFractionDuringReheatDes = max(MaxAirVolFractionDuringReheatDes, SingleDuct::Sys(2).ZoneMinAirFrac);
    MaxAirVolFlowRateDuringReheatDes =
        min(max(MaxAirVolFlowRateDuringReheatDes, MaxAirVolFractionDuringReheatDes * SingleDuct::Sys(2).MaxAirVolFlowRate),
            SingleDuct::Sys(2).MaxAirVolFlowRate);

    // EXPECT zone floor area > 0, others as calculated above
    EXPECT_GT(SingleDuct::Sys(2).ZoneFloorArea, 0.0);
    EXPECT_NEAR(SingleDuct::Sys(2).MaxAirVolFlowRateDuringReheat, MaxAirVolFlowRateDuringReheatDes, 0.0000000000001);
    EXPECT_NEAR(MaxAirVolFractionDuringReheatDes, SingleDuct::Sys(2).MaxAirVolFractionDuringReheat, 0.0000000000001);
}

TEST_F(EnergyPlusFixture, TestOAMassFlowRateUsingStdRhoAir)
{

    // AUTHOR: L. Gu, FSEC
    // DATE WRITTEN: Jul. 2016
    // TEST: #5366

    Real64 SAMassFlow;
    Real64 AirLoopOAFrac;

    SingleDuct::Sys.allocate(1);
    Zone.allocate(1);
    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataAirLoop::AirLoopFlow.allocate(1);
    DataAirLoop::AirLoopControlInfo.allocate(1);
    DataSizing::OARequirements.allocate(1);
    DataHeatBalance::ZoneIntGain.allocate(1);

    Zone(1).FloorArea = 10.0;
    SingleDuct::Sys(1).CtrlZoneNum = 1;
    SingleDuct::Sys(1).ActualZoneNum = 1;
    SingleDuct::Sys(1).NoOAFlowInputFromUser = false;
    SingleDuct::Sys(1).OARequirementsPtr = 1;
    SingleDuct::Sys(1).AirLoopNum = 1;

    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;
    DataAirLoop::AirLoopFlow(1).OAFrac = 0.4;
    DataAirLoop::AirLoopControlInfo(1).AirLoopDCVFlag = true;

    DataSizing::OARequirements(1).Name = "CM DSOA WEST ZONE";
    DataSizing::OARequirements(1).OAFlowMethod = DataSizing::OAFlowSum;
    DataSizing::OARequirements(1).OAFlowPerPerson = 0.003149;
    DataSizing::OARequirements(1).OAFlowPerArea = 0.000407;
    DataEnvironment::StdRhoAir = 1.20;
    DataHeatBalance::ZoneIntGain(1).NOFOCC = 0.1;

    SingleDuct::CalcOAMassFlow(1, SAMassFlow, AirLoopOAFrac);
    EXPECT_NEAR(0.0131547, SAMassFlow, 0.00001);
    EXPECT_NEAR(0.4, AirLoopOAFrac, 0.00001);

    // Cleanup
    SingleDuct::Sys.deallocate();
    Zone.deallocate();
    DataZoneEquipment::ZoneEquipConfig.deallocate();
    DataAirLoop::AirLoopFlow.deallocate();
    DataAirLoop::AirLoopControlInfo.deallocate();
    DataSizing::OARequirements.deallocate();
    DataHeatBalance::ZoneIntGain.deallocate();
}

TEST_F(EnergyPlusFixture, SingleDuct_VAVWaterCoilSizing)
{
    // AUTHOR: R. Raustad, FSEC
    // DATE WRITTEN: Mar 2017

    std::string const idf_objects = delimited_string({

        " Output:Diagnostics, DisplayExtraWarnings;",
        "  Timestep, 4;",

        "BUILDING, SingleDuct_VAVWaterCoilSizing, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",

        "SimulationControl, YES, YES, YES, YES, NO;",

        "  Site:Location,",
        "    Miami Intl Ap FL USA TMY3 WMO=722020E,    !- Name",
        "    25.82,                   !- Latitude {deg}",
        "    -80.30,                  !- Longitude {deg}",
        "    -5.00,                   !- Time Zone {hr}",
        "    11;                      !- Elevation {m}",

        "SizingPeriod:DesignDay,",
        " Miami Intl Ap Ann Clg 1% Condns DB/MCWB, !- Name",
        " 7,                        !- Month",
        " 21,                       !- Day of Month",
        " SummerDesignDay,          !- Day Type",
        " 31.7,                     !- Maximum Dry - Bulb Temperature{ C }",
        " 10.0,                      !- Daily Dry - Bulb Temperature Range{ deltaC }",
        " ,                         !- Dry - Bulb Temperature Range Modifier Type",
        " ,                         !- Dry - Bulb Temperature Range Modifier Day Schedule Name",
        " Wetbulb,                  !- Humidity Condition Type",
        " 22.7,                     !- Wetbulb or DewPoint at Maximum Dry - Bulb{ C }",
        " ,                         !- Humidity Condition Day Schedule Name",
        " ,                         !- Humidity Ratio at Maximum Dry - Bulb{ kgWater / kgDryAir }",
        " ,                         !- Enthalpy at Maximum Dry - Bulb{ J / kg }",
        " ,                         !- Daily Wet - Bulb Temperature Range{ deltaC }",
        " 101217.,                  !- Barometric Pressure{ Pa }",
        " 3.8,                      !- Wind Speed{ m / s }",
        " 340,                      !- Wind Direction{ deg }",
        " No,                       !- Rain Indicator",
        " No,                       !- Snow Indicator",
        " No,                       !- Daylight Saving Time Indicator",
        " ASHRAEClearSky,           !- Solar Model Indicator",
        " ,                         !- Beam Solar Day Schedule Name",
        " ,                         !- Diffuse Solar Day Schedule Name",
        " ,                         !- ASHRAE Clear Sky Optical Depth for Beam Irradiance( taub ) { dimensionless }",
        " ,                         !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance( taud ) { dimensionless }",
        " 1.00;                     !- Sky Clearness",

        "SizingPeriod:DesignDay,",
        " Miami Intl Ap Ann Htg 99.6% Condns DB, !- Name",
        " 1,                        !- Month",
        " 21,                       !- Day of Month",
        " WinterDesignDay,          !- Day Type",
        " 35.0,                     !- Maximum Dry - Bulb Temperature{ C }", // make sure zone heating load does not exist during sizing
        " 0.0,                      !- Daily Dry - Bulb Temperature Range{ deltaC }",
        " ,                         !- Dry - Bulb Temperature Range Modifier Type",
        " ,                         !- Dry - Bulb Temperature Range Modifier Day Schedule Name",
        " Wetbulb,                  !- Humidity Condition Type",
        " 18.7,                     !- Wetbulb or DewPoint at Maximum Dry - Bulb{ C }",
        " ,                         !- Humidity Condition Day Schedule Name",
        " ,                         !- Humidity Ratio at Maximum Dry - Bulb{ kgWater / kgDryAir }",
        " ,                         !- Enthalpy at Maximum Dry - Bulb{ J / kg }",
        " ,                         !- Daily Wet - Bulb Temperature Range{ deltaC }",
        " 101217.,                  !- Barometric Pressure{ Pa }",
        " 2.8,                      !- Wind Speed{ m / s }",
        " 340,                      !- Wind Direction{ deg }",
        " No,                       !- Rain Indicator",
        " No,                       !- Snow Indicator",
        " No,                       !- Daylight Saving Time Indicator",
        " ASHRAEClearSky,           !- Solar Model Indicator",
        " ,                         !- Beam Solar Day Schedule Name",
        " ,                         !- Diffuse Solar Day Schedule Name",
        " ,                         !- ASHRAE Clear Sky Optical Depth for Beam Irradiance( taub ) { dimensionless }",
        " ,                         !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance( taud ) { dimensionless }",
        " 1.00;                     !- Sky Clearness",

        "Zone,",
        "  Space,                   !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  2.4,                     !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",

        "Zone,",
        "  Space2,                !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  2.4,                     !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",

        "Sizing:Zone,",
        " Space,                    !- Zone or ZoneList Name",
        " SupplyAirTemperature,     !- Zone Cooling Design Supply Air Temperature Input Method",
        " 12.,                      !- Zone Cooling Design Supply Air Temperature{ C }",
        " ,                         !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
        " SupplyAirTemperature,     !- Zone Heating Design Supply Air Temperature Input Method",
        " 50.,                      !- Zone Heating Design Supply Air Temperature{ C }",
        " ,                         !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
        " 0.008,                    !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        " 0.008,                    !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        " SZ DSOA,                  !- Design Specification Outdoor Air Object Name",
        " 0.0,                      !- Zone Heating Sizing Factor",
        " 0.0,                      !- Zone Cooling Sizing Factor",
        " DesignDay,                !- Cooling Design Air Flow Method",
        " 0,                        !- Cooling Design Air Flow Rate{ m3 / s }",
        " ,                         !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " ,                         !- Cooling Minimum Air Flow{ m3 / s }",
        " ,                         !- Cooling Minimum Air Flow Fraction",
        " DesignDay,                !- Heating Design Air Flow Method",
        " 0,                        !- Heating Design Air Flow Rate{ m3 / s }",
        " ,                         !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " ,                         !- Heating Maximum Air Flow{ m3 / s }",
        " ;                         !- Heating Maximum Air Flow Fraction",

        "Sizing:Zone,",
        " Space2,                   !- Zone or ZoneList Name",
        " SupplyAirTemperature,     !- Zone Cooling Design Supply Air Temperature Input Method",
        " 12.,                      !- Zone Cooling Design Supply Air Temperature{ C }",
        " ,                         !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
        " SupplyAirTemperature,     !- Zone Heating Design Supply Air Temperature Input Method",
        " 50.,                      !- Zone Heating Design Supply Air Temperature{ C }",
        " ,                         !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
        " 0.008,                    !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        " 0.008,                    !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        " SZ DSOA,                  !- Design Specification Outdoor Air Object Name",
        " 0.0,                      !- Zone Heating Sizing Factor",
        " 0.0,                      !- Zone Cooling Sizing Factor",
        " DesignDay,                !- Cooling Design Air Flow Method",
        " 0,                        !- Cooling Design Air Flow Rate{ m3 / s }",
        " ,                         !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " ,                         !- Cooling Minimum Air Flow{ m3 / s }",
        " ,                         !- Cooling Minimum Air Flow Fraction",
        " DesignDay,                !- Heating Design Air Flow Method",
        " 0,                        !- Heating Design Air Flow Rate{ m3 / s }",
        " ,                         !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " ,                         !- Heating Maximum Air Flow{ m3 / s }",
        " ;                         !- Heating Maximum Air Flow Fraction",

        "DesignSpecification:OutdoorAir,",
        " SZ DSOA,                  !- Name",
        " flow/zone,                !- Outdoor Air Method",
        " 0.0,                      !- Outdoor Air Flow per Person{ m3 / s - person }",
        " 0.05,                     !- Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }",
        " 0.05;                     !- Outdoor Air Flow per Zone{ m3 / s }",

        "  Sizing:System,",
        "    VAV Sys 1,               !- AirLoop Name",
        "    sensible,                !- Type of Load to Size On",
        "    autosize,                !- Design Outdoor Air Flow Rate {m3/s}",
        "    0.3,                     !- Central Heating Maximum System Air Flow Ratio",
        "    4.5,                     !- Preheat Design Temperature {C}",
        "    0.008,                   !- Preheat Design Humidity Ratio {kgWater/kgDryAir}",
        "    11.0,                    !- Precool Design Temperature {C}",
        "    0.008,                   !- Precool Design Humidity Ratio {kgWater/kgDryAir}",
        "    12.8,                    !- Central Cooling Design Supply Air Temperature {C}",
        "    16.0,                    !- Central Heating Design Supply Air Temperature {C}",
        "    noncoincident,           !- Type of Zone Sum to Use",
        "    no,                      !- 100% Outdoor Air in Cooling",
        "    no,                      !- 100% Outdoor Air in Heating",
        "    0.008,                   !- Central Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.008,                   !- Central Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Supply Air Flow Rate Per Floor Area During Cooling Operation {m3/s-m2}",
        "    ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "    ,                        !- Design Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    0,                       !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Supply Air Flow Rate Per Floor Area During Heating Operation {m3/s-m2}",
        "    ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "    ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "    ,                        !- Design Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}",
        "    ,                        !- System Outdoor Air Method",
        "    1.0,                     !- Zone Maximum Outdoor Air Fraction {dimensionless}",
        "    CoolingDesignCapacity,   !- Cooling Design Capacity Method",
        "    autosize,                !- Cooling Design Capacity {W}",
        "    ,                        !- Cooling Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Cooling Design Capacity",
        "    HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "    autosize,                !- Heating Design Capacity {W}",
        "    ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Heating Design Capacity",
        "    VAV;                     !- Central Cooling Capacity Control Method",

        "  ZoneHVAC:EquipmentConnections,",
        "    Space,                    !- Zone Name",
        "    Space Eq,                 !- Zone Conditioning Equipment List Name",
        "    Space In Node,            !- Zone Air Inlet Node or NodeList Name",
        "    Space Out Node,           !- Zone Air Exhaust Node or NodeList Name",
        "    Space Node,               !- Zone Air Node Name",
        "    Space Ret Node;           !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentConnections,",
        "    Space2,                   !- Zone Name",
        "    Space2 Eq,                !- Zone Conditioning Equipment List Name",
        "    Space2 In Node,           !- Zone Air Inlet Node or NodeList Name",
        "    Space2 Out Node,          !- Zone Air Exhaust Node or NodeList Name",
        "    Space2 Node,              !- Zone Air Node Name",
        "    Space2 Ret Node;          !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentList,",
        "    Space Eq,                !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE1-1 ATU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No - Load Sequence",

        "  ZoneHVAC:AirDistributionUnit,",
        "    SPACE1-1 ATU,            !- Name",
        "    Space In Node,           !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:Reheat,  !- Air Terminal Object Type",
        "    SPACE1-1 VAV Reheat;     !- Air Terminal Name",

        "  ZoneHVAC:EquipmentList,",
        "    Space2 Eq,               !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE2-1 ATU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No - Load Sequence",

        "  ZoneHVAC:AirDistributionUnit,",
        "    SPACE2-1 ATU,            !- Name",
        "    Space2 In Node,          !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:Reheat,  !- Air Terminal Object Type",
        "    SPACE2-1 VAV Reheat;     !- Air Terminal Name",

        "  AirTerminal:SingleDuct:VAV:Reheat,",
        "    SPACE1-1 VAV Reheat,     !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    SPACE1-1 Zone Coil Air In Node,  !- Damper Air Outlet Node Name",
        "    SPACE1-1 ATU In Node,    !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    0.3,                     !- Constant Minimum Air Flow Fraction",
        "    ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
        "    ,                        !- Minimum Air Flow Fraction Schedule Name",
        "    Coil:Heating:Water,      !- Reheat Coil Object Type",
        "    SPACE1-1 Zone Coil,      !- Reheat Coil Name",
        "    autosize,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        "    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        "    SPACE In Node,           !- Air Outlet Node Name",
        "    0.001,                   !- Convergence Tolerance",
        "    ReverseWithLimits,       !- Damper Heating Action",
        "    AUTOCALCULATE,           !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}",
        "    AUTOCALCULATE;           !- Maximum Flow Fraction During Reheat",

        "  AirTerminal:SingleDuct:VAV:Reheat,",
        "    SPACE2-1 VAV Reheat,     !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    SPACE2-1 Zone Coil Air In Node,  !- Damper Air Outlet Node Name",
        "    SPACE2-1 ATU In Node,    !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    0.3,                     !- Constant Minimum Air Flow Fraction",
        "    ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
        "    ,                        !- Minimum Air Flow Fraction Schedule Name",
        "    Coil:Heating:Fuel,        !- Reheat Coil Object Type",
        "    SPACE2-1 Zone Coil,      !- Reheat Coil Name",
        "    0.0,                     !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        "    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        "    SPACE2 In Node,          !- Air Outlet Node Name",
        "    0.001,                   !- Convergence Tolerance",
        "    ReverseWithLimits,       !- Damper Heating Action",
        "    AUTOCALCULATE,           !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}",
        "    AUTOCALCULATE;           !- Maximum Flow Fraction During Reheat",

        "  BranchList,",
        "    VAV Sys 1 Branches,      !- Name",
        "    VAV Sys 1 Main Branch;   !- Branch 1 Name",

        "  Branch,",
        "    VAV Sys 1 Main Branch,   !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
        "    OA Sys 1,                !- Component 1 Name",
        "    VAV Sys 1 Inlet Node,    !- Component 1 Inlet Node Name",
        "    Mixed Air Node 1,        !- Component 1 Outlet Node Name",
        "    CoilSystem:Cooling:DX,   !- Component 2 Object Type",
        "    DX Cooling Coil System 1,!- Component 2 Name",
        "    Mixed Air Node 1,        !- Component 2 Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Component 2 Outlet Node Name",
        "    Coil:Heating:Fuel,        !- Component 3 Object Type",
        "    Main Heating Coil 1,     !- Component 3 Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Component 3 Inlet Node Name",
        "    Main Heating Coil 1 Outlet Node,  !- Component 3 Outlet Node Name",
        "    Fan:VariableVolume,      !- Component 4 Object Type",
        "    Supply Fan 1,            !- Component 4 Name",
        "    Main Heating Coil 1 Outlet Node,  !- Component 4 Inlet Node Name",
        "    VAV Sys 1 Outlet Node;   !- Component 4 Outlet Node Name",

        "  AirLoopHVAC,",
        "    VAV Sys 1,               !- Name",
        "    ,                        !- Controller List Name",
        "    VAV Sys 1 Avail List,    !- Availability Manager List Name",
        "    autosize,                !- Design Supply Air Flow Rate {m3/s}",
        "    VAV Sys 1 Branches,      !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    VAV Sys 1 Inlet Node,    !- Supply Side Inlet Node Name",
        "    Demand Out Node,         !- Demand Side Outlet Node Name",
        "    Zone Eq In Node,         !- Demand Side Inlet Node Names",
        "    VAV Sys 1 Outlet Node;   !- Supply Side Outlet Node Names",

        "  AirLoopHVAC:SupplyPath,",
        "    Zone Supply Air Path 1,  !- Name",
        "    Zone Eq In Node,         !- Supply Air Path Inlet Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
        "    Zone Supply Air Splitter;  !- Component 1 Name",

        "  AirLoopHVAC:ZoneSplitter,",
        "    Zone Supply Air Splitter,  !- Name",
        "    Zone Eq In Node,         !- Inlet Node Name",
        "    SPACE1-1 ATU In Node,    !- Outlet 1 Node Name",
        "    SPACE2-1 ATU In Node;    !- Outlet 2 Node Name",

        "  AirLoopHVAC:ReturnPath,",
        "    ReturnAirPath1,          !- Name",
        "    Demand Out Node,         !- Return Air Path Outlet Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
        "    Zone Return Air Mixer;   !- Component 1 Name",

        "  AirLoopHVAC:ZoneMixer,",
        "    Zone Return Air Mixer,   !- Name",
        "    Demand Out Node,         !- Outlet Node Name",
        "    Space Ret Node,          !- Inlet 1 Node Name",
        "    Space2 Ret Node;         !- Inlet 2 Node Name",

        "  AirLoopHVAC:ControllerList,",
        "    OA Sys 1 Controllers,    !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    OA Controller 1;         !- Controller 1 Name",

        "  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys 1 Equipment,      !- Name",
        "    OutdoorAir:Mixer,        !- Component 3 Object Type",
        "    OA Mixing Box 1;         !- Component 3 Name",

        "  AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys 1,                !- Name",
        "    OA Sys 1 Controllers,    !- Controller List Name",
        "    OA Sys 1 Equipment,      !- Outdoor Air Equipment List Name",
        "    VAV Sys 1 Avail List;    !- Availability Manager List Name",

        "  OutdoorAir:Node,",
        "    Main Cooling Coil 1 Condenser Node,  !- Name",
        "    -1.0;                    !- Height Above Ground {m}",

        "  OutdoorAir:NodeList,",
        "    OA Sys Inlet Node;       !- Node or NodeList Name 1",

        "  OutdoorAir:Mixer,",
        "    OA Mixing Box 1,         !- Name",
        "    Mixed Air Node 1,        !- Mixed Air Node Name",
        "    OA Sys Inlet Node,       !- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node,  !- Relief Air Stream Node Name",
        "    VAV Sys 1 Inlet Node;    !- Return Air Stream Node Name",

        "  AvailabilityManagerAssignmentList,",
        "    VAV Sys 1 Avail List,    !- Name",
        "    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
        "    VAV Sys 1 Avail;         !- Availability Manager 1 Name",

        "  AvailabilityManager:Scheduled,",
        "    VAV Sys 1 Avail,         !- Name",
        "    AvailSched;              !- Schedule Name",

        "  SetpointManager:Scheduled,",
        "    Supply Air Temp Manager 1,  !- Name",
        "    Temperature,             !- Control Variable",
        "    HTGSETP_SCH,             !- Schedule Name",
        "    VAV Sys 1 Outlet Node; !- Setpoint Node or NodeList Name",

        "  SetpointManager:MixedAir,",
        "    Mixed Air and Coil Exit Temp Manager 1,  !- Name",
        "    Temperature,             !- Control Variable",
        "    VAV Sys 1 Outlet Node,   !- Reference Setpoint Node Name",
        "    Main Heating Coil 1 Outlet Node,  !- Fan Inlet Node Name",
        "    VAV Sys 1 Outlet Node,   !- Fan Outlet Node Name",
        "    Mixed Air Node List;     !- Setpoint Node or NodeList Name",

        "  NodeList,",
        "    Mixed Air Node List,",
        "    Main Heating Coil 1 Outlet Node,",
        "    Main Cooling Coil 1 Outlet Node,",
        "    Mixed Air Node 1;",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
        "    VAV Sys 1 Inlet Node,    !- Return Air Node Name",
        "    Mixed Air Node 1,        !- Mixed Air Node Name",
        "    OA Sys Inlet Node,       !- Actuator Node Name",
        "    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    FixedDryBulb,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    19.,                     !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    4.,                      !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    FixedMinimum,            !- Minimum Limit Type",
        "    AvailSched;              !- Minimum Outdoor Air Schedule Name",

        "  Coil:Heating:Water,",
        "    SPACE1-1 Zone Coil,                      !- Name",
        "    AvailSched,                              !- Availability Schedule Name",
        "    autosize,                                !- U-Factor Times Area Value",
        "    autosize,                                !- Maximum Water Flow Rate",
        "    Zone Coil Water In Node,                 !- Water Inlet Node Name",
        "    Zone Coil Water Out Node,                !- Water Outlet Node Name",
        "    SPACE1-1 Zone Coil Air In Node,          !- Air Inlet Node Name",
        "    Space In Node,                           !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                                !- Rated Capacity {W}",
        "    82.2,                                    !- Rated Inlet Water Temperature{ C }",
        "    16.6,                                    !- Rated Inlet Air Temperature{ C }",
        "    71.1,                                    !- Rated Outlet Water Temperature{ C }",
        "    32.2,                                    !- Rated Outlet Air Temperature{ C }",
        "    0.5,                                     !- Rated Ratio for Air and Water Convection",
        "    ;                                        !- Design Water Temperature Difference",

        "  Sizing:Plant,",
        "    HeatSys1,                                !- Plant or Condenser Loop Name",
        "    Heating,                                 !- Loop Type",
        "    82.2000,                                 !- Design Loop Exit Temperature{ C }",
        "    11.1000;                                 !- Loop Design Temperature Difference{ deltaC }",

        "  PlantLoop,",
        "    HeatSys1, !- Name",
        "    Water, !- Fluid Type",
        "    , !- User Defined Fluid Type",
        "    HeatSys1 Loop Operation Scheme List,     !- Plant Equipment Operation Scheme Name",
        "    HeatSys1 Supply Outlet Node,             !- Loop Temperature Setpoint Node Name",
        "    100.0,                                   !- Maximum Loop Temperature{ C }",
        "    10.0,                                    !- Minimum Loop Temperature{ C }",
        "    AUTOSIZE,                                !- Maximum Loop Flow Rate{ m3 / s }",
        "    0.0,                                     !- Minimum Loop Flow Rate{ m3 / s }",
        "    AUTOSIZE,                                !- Plant Loop Volume{ m3 }",
        "    HeatSys1 Supply Inlet Node,              !- Plant Side Inlet Node Name",
        "    HeatSys1 Supply Outlet Node,             !- Plant Side Outlet Node Name",
        "    HeatSys1 Supply Branches,                !- Plant Side Branch List Name",
        "    HeatSys1 Supply Connectors,              !- Plant Side Connector List Name",
        "    HeatSys1 Demand Inlet Node,              !- Demand Side Inlet Node Name",
        "    HeatSys1 Demand Outlet Node,             !- Demand Side Outlet Node Name",
        "    HeatSys1 Demand Branches,                !- Demand Side Branch List Name",
        "    HeatSys1 Demand Connectors,              !- Demand Side Connector List Name",
        "    Optimal;                                 !- Load Distribution Scheme",

        "  PlantEquipmentOperationSchemes,",
        "    HeatSys1 Loop Operation Scheme List,     !- Name",
        "    PlantEquipmentOperation:HeatingLoad,     !- Control Scheme 1 Object Type",
        "    HeatSys1 Operation Scheme,               !- Control Scheme 1 Name",
        "    AvailSched;                              !- Control Scheme 1 Schedule Name",

        "  PlantEquipmentOperation:HeatingLoad,",
        "    HeatSys1 Operation Scheme,               !- Name",
        "    0.0,                                     !- Load Range 1 Lower Limit{ W }",
        "    1000000000000000,                        !- Load Range 1 Upper Limit{ W }",
        "    HeatSys1 Equipment List;                 !- Range 1 Equipment List Name",

        "  PlantEquipmentList,",
        "    HeatSys1 Equipment List,                 !- Name",
        "    Boiler:HotWater,                         !- Equipment 1 Object Type",
        "    HeatSys1 Boiler;                         !- Equipment 1 Name",

        "  BranchList,",
        "    HeatSys1 Supply Branches,                !- Name",
        "    HeatSys1 Supply Inlet Branch,            !- Branch 1 Name",
        "    HeatSys1 Supply Equipment Branch,        !- Branch 2 Name",
        "    HeatSys1 Supply Equipment Bypass Branch, !- Branch 3 Name",
        "    HeatSys1 Supply Outlet Branch;           !- Branch 4 Name",

        "  Branch,",
        "    HeatSys1 Supply Inlet Branch,            !- Name",
        "    ,                                        !- Pressure Drop Curve Name",
        "    Pump:VariableSpeed,                      !- Component 1 Object Type",
        "    HeatSys1 Pump,                           !- Component 1 Name",
        "    HeatSys1 Supply Inlet Node,              !- Component 1 Inlet Node Name",
        "    HeatSys1 Pump - HeatSys1 BoilerNodeviaConnector;  !- Component 1 Outlet Node Name",

        "  Branch,",
        "    HeatSys1 Supply Equipment Branch,        !- Name",
        "    ,                                        !- Pressure Drop Curve Name",
        "    Boiler:HotWater,                         !- Component 1 Object Type",
        "    HeatSys1 Boiler,                         !- Component 1 Name",
        "    HeatSys1 Pump - HeatSys1 BoilerNode,     !- Component 1 Inlet Node Name",
        "    HeatSys1 Supply Equipment Outlet Node;   !- Component 1 Outlet Node Name",

        "  Branch,",
        "    HeatSys1 Supply Equipment Bypass Branch,  !- Name",
        "    ,                                         !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,                           !- Component 1 Object Type",
        "    HeatSys1 Supply Equipment Bypass Pipe,    !- Component 1 Name",
        "    HeatSys1 Supply Equip Bypass Inlet Node,  !- Component 1 Inlet Node Name",
        "    HeatSys1 Supply Equip Bypass Outlet Node;  !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    HeatSys1 Supply Equipment Bypass Pipe,    !- Name",
        "    HeatSys1 Supply Equip Bypass Inlet Node,  !- Inlet Node Name",
        "    HeatSys1 Supply Equip Bypass Outlet Node;  !- Outlet Node Name",

        "  Branch,",
        "    HeatSys1 Supply Outlet Branch,            !- Name",
        "    ,                                         !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,                           !- Component 1 Object Type",
        "    HeatSys1 Supply Outlet Pipe,              !- Component 1 Name",
        "    HeatSys1 Supply Mixer - HeatSys1 Supply Outlet Pipe, !- Component 1 Inlet Node Name",
        "    HeatSys1 Supply Outlet Node;              !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    HeatSys1 Supply Outlet Pipe,              !- Name",
        "    HeatSys1 Supply Mixer - HeatSys1 Supply Outlet Pipe, !- Inlet Node Name",
        "    HeatSys1 Supply Outlet Node;              !- Outlet Node Name",

        "  ConnectorList,",
        "    HeatSys1 Supply Connectors,               !- Name",
        "    Connector:Splitter,                       !- Connector 1 Object Type",
        "    HeatSys1 Supply Splitter,                 !- Connector 1 Name",
        "    Connector:Mixer,                          !- Connector 2 Object Type",
        "    HeatSys1 Supply Mixer;                    !- Connector 2 Name",

        "  Connector:Splitter,",
        "    HeatSys1 Supply Splitter,                 !- Name",
        "    HeatSys1 Supply Inlet Branch,             !- Inlet Branch Name",
        "    HeatSys1 Supply Equipment Branch,         !- Outlet Branch 1 Name",
        "    HeatSys1 Supply Equipment Bypass Branch;  !- Outlet Branch 2 Name",

        "  Connector:Mixer,",
        "    HeatSys1 Supply Mixer,                    !- Name",
        "    HeatSys1 Supply Outlet Branch,            !- Outlet Branch Name",
        "    HeatSys1 Supply Equipment Branch,         !- Inlet Branch 1 Name",
        "    HeatSys1 Supply Equipment Bypass Branch;  !- Inlet Branch 2 Name",

        "  BranchList,",
        "    HeatSys1 Demand Branches,      !- Name",
        "    HeatSys1 Demand Inlet Branch,  !- Branch 1 Name",
        "    HeatSys1 Demand Load Branch 1, !- Branch 2 Name",
        "    HeatSys1 Demand Bypass Branch, !- Branch 60 Name",
        "    HeatSys1 Demand Outlet Branch; !- Branch 61 Name",

        "  Branch,",
        "    HeatSys1 Demand Outlet Branch, !- Name",
        "    ,                            !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,              !- Component 1 Object Type",
        "    HeatSys1 Demand Outlet Pipe, !- Component 1 Name",
        "    HeatSys1 Demand Mixer - HeatSys1 Demand Outlet Pipe, !- Component 1 Inlet Node Name",
        "    HeatSys1 Demand Outlet Node;  !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    HeatSys1 Demand Outlet Pipe, !- Name",
        "    HeatSys1 Demand Mixer - HeatSys1 Demand Outlet Pipe, !- Inlet Node Name",
        "    HeatSys1 Demand Outlet Node;  !- Outlet Node Name",

        "  Branch,",
        "    HeatSys1 Demand Bypass Branch, !- Name",
        "    ,                              !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic, !- Component 1 Object Type",
        "    HeatSys1 Demand Bypass Pipe, !- Component 1 Name",
        "    HeatSys1 Demand Bypass Pipe Inlet Node, !- Component 1 Inlet Node Name",
        "    HeatSys1 Demand Bypass Pipe Outlet Node;  !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    HeatSys1 Demand Bypass Pipe, !- Name",
        "    HeatSys1 Demand Bypass Pipe Inlet Node, !- Inlet Node Name",
        "    HeatSys1 Demand Bypass Pipe Outlet Node;  !- Outlet Node Name",

        "  Branch,",
        "    HeatSys1 Demand Inlet Branch, !- Name",
        "    ,                           !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,             !- Component 1 Object Type",
        "    HeatSys1 Demand Inlet Pipe, !- Component 1 Name",
        "    HeatSys1 Demand Inlet Node, !- Component 1 Inlet Node Name",
        "    HeatSys1 Demand Inlet Pipe - HeatSys1 Demand Mixer;  !- Component 1 Outlet Node Name",

        "  Pipe:Adiabatic,",
        "    HeatSys1 Demand Inlet Pipe, !- Name",
        "    HeatSys1 Demand Inlet Node, !- Inlet Node Name",
        "    HeatSys1 Demand Inlet Pipe - HeatSys1 Demand Mixer;  !- Outlet Node Name",

        "  Branch,",
        "    HeatSys1 Demand Load Branch 1, !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Heating:Water,      !- Component 1 Object Type",
        "    SPACE1-1 Zone Coil,      !- Component 1 Name",
        "    Zone Coil Water In Node, !- Component 1 Inlet Node Name",
        "    Zone Coil Water Out Node;  !- Component 1 Outlet Node Name",

        "  ConnectorList,",
        "    HeatSys1 Demand Connectors, !- Name",
        "    Connector:Splitter,      !- Connector 1 Object Type",
        "    HeatSys1 Demand Splitter, !- Connector 1 Name",
        "    Connector:Mixer,         !- Connector 2 Object Type",
        "    HeatSys1 Demand Mixer;   !- Connector 2 Name",

        "  Connector:Splitter,",
        "    HeatSys1 Demand Splitter, !- Name",
        "    HeatSys1 Demand Inlet Branch, !- Inlet Branch Name",
        "    HeatSys1 Demand Load Branch 1; !- Outlet Branch 1 Name",

        "  Connector:Mixer,",
        "    HeatSys1 Demand Mixer,   !- Name",
        "    HeatSys1 Demand Outlet Branch, !- Outlet Branch Name",
        "    HeatSys1 Demand Load Branch 1; !- Inlet Branch 1 Name",

        "  Pump:VariableSpeed,",
        "    HeatSys1 Pump,           !- Name",
        "    HeatSys1 Supply Inlet Node, !- Inlet Node Name",
        "    HeatSys1 Pump - HeatSys1 BoilerNodeviaConnector, !- Outlet Node Name",
        "    AUTOSIZE,                !- Design Maximum Flow Rate{ m3 / s }",
        "    179352,                  !- Design Pump Head{ Pa }",
        "    AUTOSIZE,                !- Design Power Consumption{ W }",
        "    0.9,                     !- Motor Efficiency",
        "    0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
        "    0, 3.2485, -4.7443, 2.5295, !- VariableSpeed Pump Curve Coefficients",
        "    0.0,                     !- Design Minimum Flow Rate{ m3 / s }",
        "    Intermittent;            !- Pump Control Type",

        "  Boiler:HotWater,",
        "    HeatSys1 Boiler,         !- Name",
        "    NATURALGAS,              !- Fuel Type",
        "    48945.2,                 !- Nominal Capacity{ W }",
        "    0.8,                     !- Nominal Thermal Efficiency",
        "    LeavingBoiler,           !- Efficiency Curve Temperature Evaluation Variable",
        "    HeatSys1 Boiler Non-Condensing Boiler Curve, !- Normalized Boiler Efficiency Curve Name",
        "    82.2000,                 !- Design Water Outlet Temperature{ C }",
        "    AUTOSIZE,                !- Design Water Flow Rate{ m3 / s }",
        "    0.0,                     !- Minimum Part Load Ratio",
        "    1.2,                     !- Maximum Part Load Ratio",
        "    1.0,                     !- Optimum Part Load Ratio",
        "    HeatSys1 Pump - HeatSys1 BoilerNode, !- Boiler Water Inlet Node Name",
        "    HeatSys1 Supply Equipment Outlet Node, !- Boiler Water Outlet Node Name",
        "    95.0,                    !- Water Outlet Upper Temperature Limit{ C }",
        "    LeavingSetpointModulated, !- Boiler Flow Mode",
        "    0.0000,                  !- Parasitic Electric Load{ W }",
        "    0.3000;                  !- Sizing Factor",

        "  Curve:Cubic,",
        "    HeatSys1 Boiler Non-Condensing Boiler Curve, !- Name",
        "    0.626428326,             !- Coefficient1 Constant",
        "    0.645643582,             !- Coefficient2 x",
        "    -0.77720685,             !- Coefficient3 x**2",
        "    0.313806701,             !- Coefficient4 x**3",
        "    0.1,                     !- Minimum Value of x",
        "    1;                       !- Maximum Value of x",

        "  SetpointManager:Scheduled,",
        "    HeatSys1 Loop Setpoint Manager, !- Name",
        "    Temperature,                  !- Control Variable",
        "    HeatSys1 Loop Setpoint Sched, !- Schedule Name",
        "    HeatSys1 Supply Outlet Node;  !- Setpoint Node or NodeList Name",

        "  SetpointManager:Scheduled,",
        "    HeatSys1 Boiler Setpoint Manager, !- Name",
        "    Temperature,                  !- Control Variable",
        "    HeatSys1 Loop Setpoint Sched, !- Schedule Name",
        "    HeatSys1 Supply Equipment Outlet Node;  !- Setpoint Node or NodeList Name",

        "  Schedule:Compact,",
        "    HeatSys1 Loop Setpoint Sched, !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00, 82.2000;   !- Field 3",

        "  Coil:Heating:Fuel,",
        "    SPACE2-1 Zone Coil,      !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    Gas,                     !- Fuel Type",
        "    0.8,                     !- Gas Burner Efficiency",
        "    autosize,                !- Nominal Capacity {W}",
        "    SPACE2-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
        "    Space2 In Node;          !- Air Outlet Node Name",

        "  CoilSystem:Cooling:DX,",
        "    DX Cooling Coil System 1,!- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    Mixed Air Node 1,        !- DX Cooling Coil System Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- DX Cooling Coil System Outlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- DX Cooling Coil System Sensor Node Name",
        "    Coil:Cooling:DX:SingleSpeed,!- Cooling Coil Object Type",
        "    Main Cooling Coil 1;     !- Cooling Coil Name",

        "  Coil:Cooling:DX:SingleSpeed,",
        "    Main Cooling Coil 1,     !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    autosize,                !- Gross Rated Total Cooling Capacity { W }",
        "    autosize,                !- Gross Rated Sensible Heat Ratio",
        "    4.40,                    !- Gross Rated Cooling COP { W / W }",
        "    autosize,                !- Rated Air Flow Rate { m3 / s }",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate { W / ( m3 / s ) }",
        "    Mixed Air Node 1,        !- Air Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,    !- Air Outlet Node Name",
        "    Biquadratic,             !- Total Cooling Capacity Function of Temperature Curve Name",
        "    Cubic,                   !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    Biquadratic,             !- Energy Input Ratio Function of Temperature Curve Name",
        "    Cubic,                   !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    Cubic,                   !- Part Load Fraction Correlation Curve Name",
        "    ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    0.0,                     !- Nominal Time for Condensate Removal to Begin",
        "    0.0,                     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
        "    0.0,                     !- Maximum Cycling Rate",
        "    0.0,                     !- Latent Capacity Time Constant",
        "    Main Cooling Coil 1 Condenser Node, !- Condenser Air Inlet Node Name",
        "    AirCooled,               !- Condenser Type",
        "    0.0,                     !- Evaporative Condenser Effectiveness",
        "    ,                        !- Evaporative Condenser Air Flow Rate",
        "    ,                        !- Evaporative Condenser Pump Rated Power Consumption",
        "    0.0,                     !- Crankcase Heater Capacity",
        "    10.0;                    !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",

        "  Coil:Heating:Fuel,",
        "    Main heating Coil 1,     !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    Gas,                     !- Fuel Type",
        "    0.8,                     !- Gas Burner Efficiency",
        "    autosize,                !- Nominal Capacity {W}",
        "    Main Cooling Coil 1 Outlet Node,  !- Air Inlet Node Name",
        "    Main Heating Coil 1 Outlet Node,  !- Air Outlet Node Name",
        "    Main Heating Coil 1 Outlet Node;  !- Temperature Setpoint Node Name",

        "  Fan:VariableVolume,",
        "    Supply Fan 1,            !- Name",
        "    AvailSched,              !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    600.0,                   !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    FixedFlowRate,           !- Fan Power Minimum Flow Rate Input Method",
        "    ,                        !- Fan Power Minimum Flow Fraction",
        "    0.35326,                 !- Fan Power Minimum Air Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    0.0015302446,            !- Fan Power Coefficient 1",
        "    0.0052080574,            !- Fan Power Coefficient 2",
        "    1.1086242,               !- Fan Power Coefficient 3",
        "    -0.11635563,             !- Fan Power Coefficient 4",
        "    0.000,                   !- Fan Power Coefficient 5",
        "    Main Heating Coil 1 Outlet Node,  !- Air Inlet Node Name",
        "    VAV Sys 1 Outlet Node;   !- Air Outlet Node Name",

        "Construction,",
        " INT-WALL-1,               !- Name",
        " GP02,                     !- Outside Layer",
        " AL21,                     !- Layer 2",
        " GP02;                     !- Layer 3",

        "Material,",
        " GP02,                     !- Name",
        " MediumSmooth,             !- Roughness",
        " 1.5900001E-02,            !- Thickness{ m }",
        " 0.1600000,                !- Conductivity{ W / m - K }",
        " 801.0000,                 !- Density{ kg / m3 }",
        " 837.0000,                 !- Specific Heat{ J / kg - K }",
        " 0.9000000,                !- Thermal Absorptance",
        " 0.7500000,                !- Solar Absorptance",
        " 0.7500000;                !- Visible Absorptance",

        "Material:AirGap,",
        " AL21,                     !- Name",
        " 0.1570000;                !- Thermal Resistance{ m2 - K / W }",

        "Construction,",
        "FLOOR-SLAB-1,              !- Name",
        "CC03,                      !- Outside Layer",
        "CP01;                      !- Layer 2",

        "Material,",
        " CC03,                     !- Name",
        " MediumRough,              !- Roughness",
        " 0.1016000,                !- Thickness{ m }",
        " 1.310000,                 !- Conductivity{ W / m - K }",
        " 2243.000,                 !- Density{ kg / m3 }",
        " 837.0000,                 !- Specific Heat{ J / kg - K }",
        " 0.9000000,                !- Thermal Absorptance",
        " 0.6500000,                !- Solar Absorptance",
        " 0.6500000;                !- Visible Absorptance",

        "Material:NoMass,",
        " CP01,                     !- Name",
        " Rough,                    !- Roughness",
        " 0.3670000,                !- Thermal Resistance{ m2 - K / W }",
        " 0.9000000,                !- Thermal Absorptance",
        " 0.7500000,                !- Solar Absorptance",
        " 0.7500000;                !- Visible Absorptance",

        "Construction,",
        " CLNG-1,                   !- Name",
        " MAT-CLNG-1;               !- Outside Layer",

        "Material:NoMass,",
        " MAT-CLNG-1,               !- Name",
        " Rough,                    !- Roughness",
        " 0.652259290,              !- Thermal Resistance{ m2 - K / W }",
        " 0.65,                     !- Thermal Absorptance",
        " 0.65,                     !- Solar Absorptance",
        " 0.65;                     !- Visible Absorptance",

        "BuildingSurface:Detailed,",
        " SPACE-W1,                 !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
        " Outdoors,                 !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " SunExposed,               !- Sun Exposure",
        " WindExposed,              !- Wind Exposure",
        " 0.50000,                  !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
        " 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE-C1,                 !- Name",
        " CEILING,                  !- Surface Type",
        " CLNG-1,                   !- Construction Name",
        " Space,                    !- Zone Name",
        " Outdoors,                 !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 2 {m}",
        " 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 3 {m}",
        " 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE-F1,                 !- Name",
        " FLOOR,                    !- Surface Type",
        " FLOOR-SLAB-1,             !- Construction Name",
        " Space,                    !- Zone Name",
        " Ground,                   !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 1 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 3.7, 3.7, 0.0;            !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE-W2,                 !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
        " 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE-W3,                 !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 0.0, 0.0, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE-W4,                 !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space,                    !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 26.8, 3.7, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 3.7, 3.7, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

        "ZoneControl:Thermostat,",
        " Space Thermostat,         !- Name",
        " Space,                    !- Zone or ZoneList Name",
        " Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        " ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        " Space DualSPSched;        !- Control 1 Name",

        "ZoneControl:Thermostat,",
        " Space2 Thermostat,        !- Name",
        " Space2,                   !- Zone or ZoneList Name",
        " Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        " ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        " Space DualSPSched;        !- Control 1 Name",

        "Schedule:Compact,",
        " Dual Zone Control Type Sched,  !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 2",
        " Until: 24:00,4;           !- Field 3",

        "ThermostatSetpoint:DualSetpoint,",
        " Space DualSPSched,        !- Name",
        " HTGSETP_SCH,              !- Heating Setpoint Temperature Schedule Name",
        " CLGSETP_SCH;              !- Cooling Setpoint Temperature Schedule Name",

        "Schedule:Compact,",
        " CLGSETP_SCH,              !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 19",
        " Until: 7:00,30.0,         !- Field 20",
        " Until: 17:00,24.0,        !- Field 20",
        " Until: 24:00,30.0;        !- Field 20",

        "Schedule:Compact,",
        " HTGSETP_SCH,              !- Name",
        " Any Number,               !- Schedule Type Limits Name",
        " Through: 12/31,           !- Field 1",
        " For: AllDays,             !- Field 22",
        " Until: 7:00, 10.0,        !- Field 23",
        " Until: 17:00, 17.0,       !- Field 23",
        " Until: 24:00, 10.0;       !- Field 23",

        "BuildingSurface:Detailed,",
        " SPACE2-W1,                !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space2,                   !- Zone Name",
        " Outdoors,                 !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " SunExposed,               !- Sun Exposure",
        " WindExposed,              !- Wind Exposure",
        " 0.50000,                  !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
        " 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-C1,                !- Name",
        " CEILING,                  !- Surface Type",
        " CLNG-1,                   !- Construction Name",
        " Space2,                   !- Zone Name",
        " Outdoors,                 !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 2 {m}",
        " 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 3 {m}",
        " 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-F1,                !- Name",
        " FLOOR,                    !- Surface Type",
        " FLOOR-SLAB-1,             !- Construction Name",
        " Space2,                   !- Zone Name",
        " Ground,                   !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 1 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 3.7, 3.7, 0.0;            !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-W2,                !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space2,                   !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 30.5, 0.0, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
        " 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
        " 26.8, 3.7, 2.4;           !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-W3,                !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space2,                   !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 3.7, 3.7, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
        " 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
        " 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 0.0, 0.0, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

        "BuildingSurface:Detailed,",
        " SPACE2-W4,                !- Name",
        " WALL,                     !- Surface Type",
        " INT-WALL-1,               !- Construction Name",
        " Space2,                   !- Zone Name",
        " Adiabatic,                !- Outside Boundary Condition",
        " ,                         !- Outside Boundary Condition Object",
        " NoSun,                    !- Sun Exposure",
        " NoWind,                   !- Wind Exposure",
        " 0.0,                      !- View Factor to Ground",
        " 4,                        !- Number of Vertices",
        " 26.8, 3.7, 2.4,           !- X, Y, Z == > Vertex 1 {m}",
        " 26.8, 3.7, 0.0,           !- X, Y, Z == > Vertex 2 {m}",
        " 3.7, 3.7, 0.0,            !- X, Y, Z == > Vertex 3 {m}",
        " 3.7, 3.7, 2.4;            !- X, Y, Z == > Vertex 4 {m}",

        "OutdoorAir:NodeList,",
        "  OutsideAirInletNodes;    !- Node or NodeList Name 1",

        "ScheduleTypeLimits,",
        "  Any Number;              !- Name",

        "Schedule:Compact,",
        "  AvailSched,              !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 3",
        "  For: AllDays,            !- Field 4",
        "  Until: 24:00,1.0;        !- Field 5",

        "Schedule:Compact,",
        "  FanOpModeSchedule,       !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,1.0;        !- Field 7",

        "Curve:Biquadratic,",
        "  Biquadratic,             !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  5,                       !- Minimum Value of x",
        "  40,                      !- Maximum Value of x",
        "  -5,                      !- Minimum Value of y",
        "  30,                      !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "Curve:Cubic,",
        "  Cubic,                   !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0,                       !- Coefficient4 x**3",
        "  11,                      !- Minimum Value of x",
        "  30,                      !- Maximum Value of x",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature;             !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    OutputProcessor::TimeValue.allocate(2);

    ManageSimulation(); // run the design days

    // size the reheat coil with no design zone heating load
    // DesZoneHeatLoad = 0 from simulating design days and forcing heating load = 0
    // this can still provide a negative coil load if CoilInTemp > ZoneDesTemp
    // CoilInTemp = Sizing:System, 16.0000, !- Central Heating Design Supply Air Temperature{ C }
    // ZoneDesTemp = max of thermostat heating set point temperature = 17.0 C
    // "Schedule:Compact,",
    //	" HTGSETP_SCH,              !- Name",
    //	" Any Number,               !- Schedule Type Limits Name",
    //	" Through: 12/31,           !- Field 1",
    //	" For: AllDays,             !- Field 22",
    //	" Until: 7:00, 10.0,        !- Field 23",
    //	" Until: 17:00, 17.0,       !- Field 23",
    //	" Until: 24:00, 10.0;       !- Field 23",

    DataEnvironment::StdRhoAir = 1.2027389349552706;
    Real64 CoilInTemp = TermUnitFinalZoneSizing(1).DesHeatCoilInTempTU;
    Real64 DesMassFlow = DataEnvironment::StdRhoAir * TermUnitSizing(1).AirVolFlow;
    Real64 DesZoneHeatLoad = FinalZoneSizing(1).DesHeatLoad * FinalZoneSizing(1).HeatSizingFactor;
    Real64 ZoneDesTemp = FinalZoneSizing(1).ZoneTempAtHeatPeak;
    Real64 ZoneDesHumRat = FinalZoneSizing(1).ZoneHumRatAtHeatPeak;

    Real64 DesCoilLoad =
        DesZoneHeatLoad + Psychrometrics::PsyCpAirFnWTdb(ZoneDesHumRat, 0.5 * (CoilInTemp + ZoneDesTemp)) * DesMassFlow * (ZoneDesTemp - CoilInTemp);

    EXPECT_EQ(CoilInTemp, 16.0);
    EXPECT_EQ(DesZoneHeatLoad, 0.0);
    EXPECT_EQ(ZoneDesTemp, 17.0);
    EXPECT_EQ(ZoneDesHumRat, 0.008);
    EXPECT_NEAR(DesCoilLoad, 120.5, 0.1);
}

TEST_F(EnergyPlusFixture, TerminalUnitMixerInitTest)
{

    // Address #6205
    // Address #6241

    using SingleDuct::SysATMixer;
    int ATMixerNum = 1;
    SingleDuct::NumATMixers = 1;
    DataHeatBalance::TotPeople = 1;

    SysATMixer.allocate(ATMixerNum);
    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataAirLoop::AirLoopFlow.allocate(1);
    DataLoopNode::Node.allocate(3);
    DataSizing::OARequirements.allocate(1);
    Zone.allocate(1);
    DataHeatBalance::ZoneIntGain.allocate(1);

    SysATMixer(ATMixerNum).SecInNode = 1;
    SysATMixer(ATMixerNum).PriInNode = 2;
    SysATMixer(ATMixerNum).MixedAirOutNode = 3;
    SysATMixer(ATMixerNum).AirLoopNum = 1;
    SysATMixer(ATMixerNum).ZoneNum = 1;
    SysATMixer(ATMixerNum).ZoneEqNum = 1;
    SysATMixer(ATMixerNum).NoOAFlowInputFromUser = false;
    SysATMixer(ATMixerNum).OARequirementsPtr = 1;

    DataAirLoop::AirLoopFlow(1).OAFrac = 1.0;

    Zone(1).FloorArea = 10.0;
    OARequirements(1).OAFlowMethod = OAFlowSum;
    OARequirements(1).OAFlowPerZone = 0.1;
    OARequirements(1).OAFlowPerPerson = 0.1;

    DataLoopNode::Node(2).Press = 101325.0;
    DataLoopNode::Node(2).Temp = 23.0;
    DataLoopNode::Node(2).HumRat = 0.001;

    DataHeatBalance::ZoneIntGain(1).NOFOCC = 5.0;

    DataEnvironment::StdRhoAir = 1.20;
    SysATMixer(1).MassFlowRateMaxAvail = 1.0;
    // No airloop data exists, so skip these parts of the init
    SysATMixer(1).OneTimeInitFlag = false;
    SysATMixer(1).OneTimeInitFlag2 = false;
    // Current occupancy
    SysATMixer(1).OAPerPersonMode = 1;
    SysATMixer(1).InitATMixer(true);
    EXPECT_NEAR(DataLoopNode::Node(2).MassFlowRate, 0.72, 0.0001);
    // Design occupancy
    SysATMixer(1).OAPerPersonMode = 2;
    Zone(1).TotOccupants = 10;
    SysATMixer(1).InitATMixer(true);
    EXPECT_NEAR(DataLoopNode::Node(2).MassFlowRate, 1.32, 0.0001);

    SysATMixer.deallocate();
    DataZoneEquipment::ZoneEquipConfig.deallocate();
    DataAirLoop::AirLoopFlow.deallocate();
    DataLoopNode::Node.deallocate();
    DataSizing::OARequirements.deallocate();
    Zone.deallocate();
    DataHeatBalance::ZoneIntGain.deallocate();
}
TEST_F(EnergyPlusFixture, TerminalUnitMixerInitTest2)
{

    // Address #6205
    // Address #6241

    using SingleDuct::SysATMixer;
    int ATMixerNum = 1;
    SingleDuct::NumATMixers = 1;
    DataHeatBalance::TotPeople = 1;

    SysATMixer.allocate(ATMixerNum);
    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataAirLoop::AirLoopFlow.allocate(1);
    DataLoopNode::Node.allocate(3);
    DataSizing::OARequirements.allocate(1);
    Zone.allocate(1);
    DataHeatBalance::ZoneIntGain.allocate(1);

    SysATMixer(ATMixerNum).SecInNode = 1;
    SysATMixer(ATMixerNum).PriInNode = 2;
    SysATMixer(ATMixerNum).MixedAirOutNode = 3;
    SysATMixer(ATMixerNum).AirLoopNum = 1;
    SysATMixer(ATMixerNum).ZoneNum = 1;
    SysATMixer(ATMixerNum).ZoneEqNum = 1;
    SysATMixer(ATMixerNum).NoOAFlowInputFromUser = false;
    SysATMixer(ATMixerNum).OARequirementsPtr = 1;

    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;

    DataAirLoop::AirLoopFlow(1).OAFrac = 1.0;

    Zone(1).FloorArea = 10.0;
    OARequirements(1).OAFlowMethod = OAFlowSum;
    OARequirements(1).OAFlowPerZone = 0.5;
    OARequirements(1).OAFlowPerPerson = 0.0;
    OARequirements(1).OAFlowPerArea = 0.0;
    OARequirements(1).OAFlowACH = 0.0;

    DataLoopNode::Node(2).Press = 101325.0;
    DataLoopNode::Node(2).Temp = 23.0;
    DataLoopNode::Node(2).HumRat = 0.001;

    DataHeatBalance::ZoneIntGain(1).NOFOCC = 5.0;

    DataEnvironment::StdRhoAir = 1.0;
    SysATMixer(1).MassFlowRateMaxAvail = 1.0;
    // No airloop data exists, so skip these parts of the init
    SysATMixer(1).OneTimeInitFlag = false;
    SysATMixer(1).OneTimeInitFlag2 = false;
    // Current occupancy
    SysATMixer(1).OAPerPersonMode = 1;

    // InletSideMixer, Mixed air outlet mass flow > OA requirement, expect primary flow to equal OA requirement
    SysATMixer(1).MixerType = DataHVACGlobals::ATMixer_InletSide;
    DataLoopNode::Node(SysATMixer(1).MixedAirOutNode).MassFlowRate = 1.0;
    SysATMixer(1).InitATMixer(true);
    EXPECT_NEAR(DataLoopNode::Node(SysATMixer(1).PriInNode).MassFlowRate, 0.5, 0.0001);

    // InletSideMixer, Mixed air outlet mass flow < OA requirement, expect primary flow to equal mixed air flow
    DataLoopNode::Node(SysATMixer(1).MixedAirOutNode).MassFlowRate = 0.10;
    SysATMixer(1).InitATMixer(true);
    EXPECT_NEAR(DataLoopNode::Node(SysATMixer(1).PriInNode).MassFlowRate, 0.10, 0.0001);

    // SupplySideMixer, Mixed air outlet mass flow > OA requirement, expect primary flow to equal OA requirement
    SysATMixer(1).MixerType = DataHVACGlobals::ATMixer_SupplySide;
    DataLoopNode::Node(SysATMixer(1).MixedAirOutNode).MassFlowRate = 1.0;
    SysATMixer(1).InitATMixer(true);
    EXPECT_NEAR(DataLoopNode::Node(SysATMixer(1).PriInNode).MassFlowRate, 0.5, 0.0001);

    // SupplySideMixer, Mixed air outlet mass flow < OA requirement, expect primary flow to equal OA requirement
    DataLoopNode::Node(SysATMixer(1).MixedAirOutNode).MassFlowRate = 0.10;
    SysATMixer(1).InitATMixer(true);
    EXPECT_NEAR(DataLoopNode::Node(SysATMixer(1).PriInNode).MassFlowRate, 0.5, 0.0001);
    SysATMixer.deallocate();
    DataZoneEquipment::ZoneEquipConfig.deallocate();
    DataAirLoop::AirLoopFlow.deallocate();
    DataLoopNode::Node.deallocate();
    DataSizing::OARequirements.deallocate();
    Zone.deallocate();
    DataHeatBalance::ZoneIntGain.deallocate();
}

TEST_F(EnergyPlusFixture, VAVReheatTerminal_SizeMinFrac)
{
    std::string const idf_objects = delimited_string({
        "Version,8.8;",
        "  Zone,",
        "    Zone 1;                !- Name",
        "ZoneHVAC:EquipmentConnections,",
        "    Zone 1,                !- Zone Name",
        "    Zone 1 Equipment,             !- Zone Conditioning Equipment List Name",
        "    Zone 1 Supply Inlet,       !- Zone Air Inlet Node or NodeList Name",
        "    ,      !- Zone Air Exhaust Node or NodeList Name",
        "    Zone 1 Air Node,           !- Zone Air Node Name",
        "    Zone 1 Return Node;       !- Zone Return Air Node Name",
        "ZoneHVAC:EquipmentList,",
        "    Zone 1 Equipment,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    Zone 1 ADU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "ZoneHVAC:AirDistributionUnit,",
        "    Zone 1 ADU,    !- Name",
        "    Zone 1 Supply Inlet,     !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:Reheat,  !- Air Terminal Object Type",
        "    Zone 1 VAV Reheat;           !- Air Terminal Name",
        "AirTerminal:SingleDuct:VAV:Reheat,",
        "    Zone 1 VAV Reheat,       !- Name",
        "    ,                        !- Availability Schedule Name",
        "    Zone 1 VAV Reheat Coil Air Inlet,  !- Damper Air Outlet Node Name",
        "    Zone 1 Zone Equip Inlet, !- Air Inlet Node Name",
        "    1.0,                     !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    autosize,                !- Constant Minimum Air Flow Fraction",
        "    ,                        !- Fixed Minimum Air Flow Rate{m3/s}",
        "    ,                        !- Minimum Air Flow Fraction Schedule Name",
        "    Coil:Heating:Electric,   !- Reheat Coil Object Type",
        "    Zone 1 Reheat Coil,      !- Reheat Coil Name",
        "    ,                        !- Maximum Hot Water or Steam Flow Rate{m3/s}",
        "    ,                        !- Minimum Hot Water or Steam Flow Rate{m3/s}",
        "    Zone 1 Supply Inlet,     !- Air Outlet Node Name",
        "    0.001,                   !- Convergence Tolerance",
        "    ,                        !- Damper Heating Action",
        "    ,                        !- Maximum Flow per Zone Floor Area During Reheat",
        "    ;                        !- Maximum Flow Fraction During Reheat",
        "Coil:Heating:Electric,",
        "    Zone 1 Reheat Coil,      !- Name",
        "    ,                        !- Availability Schedule Name",
        "    1,                       !- Efficiency",
        "    100,                     !- Nominal Capacity of the Coil {W}",
        "    Zone 1 VAV Reheat Coil Air Inlet,  !- Air Inlet Node Name",
        "    Zone 1 Supply Inlet,     !- Air Outlet Node Name",
        "    ;                        !- Temperature Setpoint Node Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1();
    DataSizing::TermUnitFinalZoneSizing.allocate(1);
    DataSizing::TermUnitSizing.allocate(1);
    DataSizing::FinalZoneSizing.allocate(1);
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    SingleDuct::GetSysInput();
    EXPECT_TRUE(compare_err_stream(""));

    int SysNum = 1;

    // First test -  design min flow < max flow
    ZoneSizingRunDone = true;
    CurZoneEqNum = 1;
    CurTermUnitSizingNum = 1;
    DataSizing::TermUnitFinalZoneSizing(1).DesCoolVolFlowMin = 0.5;
    SingleDuct::SizeSys(SysNum);
    EXPECT_EQ(0.5, SingleDuct::Sys(SysNum).ZoneMinAirFrac);

    // Second test -  design min flow > max flow
    ZoneSizingRunDone = true;
    CurZoneEqNum = 1;
    CurTermUnitSizingNum = 1;
    SingleDuct::Sys(SysNum).ZoneMinAirFrac = AutoSize; // need to reset this so it sizes again
    DataSizing::TermUnitFinalZoneSizing(1).DesCoolVolFlowMin = 1.5;
    SingleDuct::SizeSys(SysNum);
    EXPECT_EQ(1.0, SingleDuct::Sys(SysNum).ZoneMinAirFrac);
}
