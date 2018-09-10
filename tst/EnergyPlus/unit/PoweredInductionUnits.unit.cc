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
#include <Fans.hh>
#include <General.hh>
#include <HeatBalanceManager.hh>
#include <HeatingCoils.hh>
#include <OutputProcessor.hh>
#include <PoweredInductionUnits.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SimulationManager.hh>
#include <ZoneAirLoopEquipmentManager.hh>

using namespace EnergyPlus;
using namespace SimulationManager;
using namespace DataSizing;
using DataHeatBalance::Zone;

TEST_F(EnergyPlusFixture, ParallelPIUTest1)
{
    std::string const idf_objects = delimited_string({
        "Version,8.4;",
        "  Zone,",
        "    SPACE2-1;                !- Name",
        "ZoneHVAC:EquipmentConnections,",
        "    SPACE2-1,                !- Zone Name",
        "    SPACE2-1 Equipment,             !- Zone Conditioning Equipment List Name",
        "    SPACE2-1 In Node,       !- Zone Air Inlet Node or NodeList Name",
        "    SPACE2-1 ATU Sec Node,      !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE2-1 Air Node,           !- Zone Air Node Name",
        "    SPACE2-1 Return Node;       !- Zone Return Air Node Name",
        "ZoneHVAC:EquipmentList,",
        "    SPACE2-1 Equipment,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE2-1 ADU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE2-1 ADU,    !- Name",
        "    SPACE2-1 In Node,     !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:ParallelPIU:Reheat,  !- Air Terminal Object Type",
        "    SPACE2-1 Parallel PIU Reheat;           !- Air Terminal Name",
        " AirTerminal:SingleDuct:ParallelPIU:Reheat,",
        " SPACE2-1 Parallel PIU Reheat,     !- Name",
        " AlwaysOn,    !- Availability Schedule Name",
        " 0.1,                !- Maximum Primary Air Flow Rate {m3/s}",
        " 0.05,                !- Maximum Secondary Air Flow Rate {m3/s}",
        " 0.2,                !- Minimum Primary Air Flow Fraction",
        " 0.1,                !- Fan On Flow Fraction",
        " SPACE2-1 ATU In Node,    !- Supply Air Inlet Node Name",
        " SPACE2-1 ATU Sec Node,   !- Secondary Air Inlet Node Name",
        " SPACE2-1 In Node,        !- Outlet Node Name",
        " SPACE2-1 Zone Coil Air In Node,  !- Reheat Coil Air Inlet Node Name",
        " SPACE2-1 PIU Mixer,      !- Zone Mixer Name",
        " SPACE2-1 PIU Fan,        !- Fan Name",
        " Coil:Heating:Electric,      !- Reheat Coil Object Type",
        " SPACE2-1 Zone Coil,      !- Reheat Coil Name",
        " 0.0,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        " 0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        " 0.0001;                  !- Convergence Tolerance",
        "",
        " Fan:ConstantVolume,",
        " SPACE2-1 PIU Fan,        !- Name",
        " AlwaysOff,           !- Availability Schedule Name",
        " 0.5,                     !- Fan Total Efficiency",
        " 50.0,                    !- Pressure Rise {Pa}",
        " 0.05,                !- Maximum Flow Rate {m3/s}",
        " 0.9,                     !- Motor Efficiency",
        " 1.0,                     !- Motor In Airstream Fraction",
        " SPACE2-1 ATU Sec Node,   !- Air Inlet Node Name",
        " SPACE2-1 ATU Fan Outlet Node;  !- Air Outlet Node Name",
        "",
        " AirLoopHVAC:ZoneMixer,",
        " SPACE2-1 PIU Mixer,      !- Name",
        " SPACE2-1 Zone Coil Air In Node,  !- Outlet Node Name",
        " SPACE2-1 ATU In Node,    !- Inlet 1 Node Name",
        " SPACE2-1 ATU Fan Outlet Node;  !- Inlet 2 Node Name",
        "",
        " Coil:Heating:Electric,",
        " SPACE2-1 Zone Coil,      !- Name",
        " AlwaysOn,    !- Availability Schedule Name",
        " 1.0,                     !- Efficiency",
        " 1000,                !- Nominal Capacity",
        " SPACE2-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
        " SPACE2-1 In Node;       !- Air Outlet Node Name",
        "",
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
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0);
    ScheduleManager::UpdateScheduleValues();

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1();
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    Fans::GetFanInput();
    Fans::GetFanInputFlag = false;
    PoweredInductionUnits::GetPIUs();
    EXPECT_TRUE(compare_err_stream(""));
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;

    // node number table
    //  1   SPACE2-1 Air Node
    //  2   SPACE2-1 Return Node
    //  3   SPACE2-1 In Node
    //  4   SPACE2-1 ATU Sec Node
    //  5   SPACE2-1 ATU Fan Outlet Node
    //  6   SPACE2-1 ATU In Node
    //  7   SPACE2-1 Zone Coil Air In Node

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);

    // Setup for Zone 1 VAV No Reheat
    int ZoneNum = 1;
    int SysNum = 1;
    int ZoneNodeNum = 1;
    int SecNodeNum = PoweredInductionUnits::PIU(SysNum).SecAirInNode;
    int PriNodeNum = PoweredInductionUnits::PIU(SysNum).PriAirInNode;
    bool FirstHVACIteration = true;
    Real64 SecMaxMassFlow = 0.05 * DataEnvironment::StdRhoAir; // From inputs

    DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru InitPIU for this terminal unit
    FirstHVACIteration = true;
    PoweredInductionUnits::InitPIU(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    Fans::InitFan(1, FirstHVACIteration);
    DataGlobals::BeginEnvrnFlag = false;
    FirstHVACIteration = false;

    // Note that the fan schedule is always off, so the PIU fan should only run if the night cycle turn on flag is true

    // First test - Heating load, TurnZoneFansOn is false, no primary flow - expecting no secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = false;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Second test - Heating load, TurnZoneFansOn is true, no primary flow - expecting secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = false;
    DataHVACGlobals::TurnZoneFansOnlyOn = true;
    PoweredInductionUnits::CalcParallelPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Third test - Cooling load TurnZoneFansOn is true, no primary flow - expecting no secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = false;
    DataHVACGlobals::TurnZoneFansOnlyOn = true;
    PoweredInductionUnits::CalcParallelPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Fourth test - Cooling load TurnFansOn is true, no primary flow - expecting no secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Fifth test - Heating load TurnFansOn is true, no primary flow - expecting secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Sixth test - Heating load TurnFansOn is true, yes primary flow, deadbandorsetback is true - expecting no secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = PoweredInductionUnits::PIU(SysNum).MaxPriAirMassFlow;
    DataLoopNode::Node(PriNodeNum).MassFlowRateMaxAvail = PoweredInductionUnits::PIU(SysNum).MaxPriAirMassFlow;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = true;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Seventh test - Heating load TurnFansOn is true, yes primary flow - expecting secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = PoweredInductionUnits::PIU(SysNum).MaxPriAirMassFlow;
    DataLoopNode::Node(PriNodeNum).MassFlowRateMaxAvail = PoweredInductionUnits::PIU(SysNum).MaxPriAirMassFlow;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Eighth test - Cooling load TurnFansOn is true, yes primary flow - expecting no secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = PoweredInductionUnits::PIU(SysNum).MaxPriAirMassFlow;
    DataLoopNode::Node(PriNodeNum).MassFlowRateMaxAvail = PoweredInductionUnits::PIU(SysNum).MaxPriAirMassFlow;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcParallelPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Cleanup
    DataHeatBalFanSys::TempControlType.deallocate();
    DataZoneEnergyDemands::ZoneSysEnergyDemand.deallocate();
    DataZoneEnergyDemands::CurDeadBandOrSetback.deallocate();
}

TEST_F(EnergyPlusFixture, SeriesPIUTest1)
{
    std::string const idf_objects = delimited_string({
        "Version,8.4;",
        "  Zone,",
        "    SPACE2-1;                !- Name",
        "ZoneHVAC:EquipmentConnections,",
        "    SPACE2-1,                !- Zone Name",
        "    SPACE2-1 Equipment,             !- Zone Conditioning Equipment List Name",
        "    SPACE2-1 In Node,       !- Zone Air Inlet Node or NodeList Name",
        "    SPACE2-1 ATU Sec Node,      !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE2-1 Air Node,           !- Zone Air Node Name",
        "    SPACE2-1 Return Node;       !- Zone Return Air Node Name",
        "ZoneHVAC:EquipmentList,",
        "    SPACE2-1 Equipment,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE2-1 ADU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE2-1 ADU,    !- Name",
        "    SPACE2-1 In Node,     !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:SeriesPIU:Reheat,  !- Air Terminal Object Type",
        "    SPACE2-1 Series PIU Reheat;           !- Air Terminal Name",
        " AirTerminal:SingleDuct:SeriesPIU:Reheat,",
        " SPACE2-1 Series PIU Reheat,     !- Name",
        " AlwaysOn,    !- Availability Schedule Name",
        " 0.15,                !- Maximum Air Flow Rate {m3/s}",
        " 0.05,                !- Maximum Primary Air Flow Rate {m3/s}",
        " 0.2,                !- Minimum Primary Air Flow Fraction",
        " SPACE2-1 ATU In Node,    !- Supply Air Inlet Node Name",
        " SPACE2-1 ATU Sec Node,   !- Secondary Air Inlet Node Name",
        " SPACE2-1 In Node,        !- Outlet Node Name",
        " SPACE2-1 Zone Coil Air In Node,  !- Reheat Coil Air Inlet Node Name",
        " SPACE2-1 PIU Mixer,      !- Zone Mixer Name",
        " SPACE2-1 PIU Fan,        !- Fan Name",
        " Coil:Heating:Electric,      !- Reheat Coil Object Type",
        " SPACE2-1 Zone Coil,      !- Reheat Coil Name",
        " 0.0,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        " 0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        " 0.0001;                  !- Convergence Tolerance",
        "",
        " Fan:ConstantVolume,",
        " SPACE2-1 PIU Fan,        !- Name",
        " AlwaysOff,           !- Availability Schedule Name",
        " 0.5,                     !- Fan Total Efficiency",
        " 50.0,                    !- Pressure Rise {Pa}",
        " 0.05,                !- Maximum Flow Rate {m3/s}",
        " 0.9,                     !- Motor Efficiency",
        " 1.0,                     !- Motor In Airstream Fraction",
        " SPACE2-1 ATU Fan Inlet Node,   !- Air Inlet Node Name",
        " SPACE2-1 Zone Coil Air In Node;  !- Air Outlet Node Name",
        "",
        " AirLoopHVAC:ZoneMixer,",
        " SPACE2-1 PIU Mixer,      !- Name",
        " SPACE2-1 ATU Fan Inlet Node,  !- Outlet Node Name",
        " SPACE2-1 ATU In Node,    !- Inlet 1 Node Name",
        " SPACE2-1 ATU Sec Node;  !- Inlet 2 Node Name",
        "",
        " Coil:Heating:Electric,",
        " SPACE2-1 Zone Coil,      !- Name",
        " AlwaysOn,    !- Availability Schedule Name",
        " 1.0,                     !- Efficiency",
        " 1000,                !- Nominal Capacity",
        " SPACE2-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
        " SPACE2-1 In Node;       !- Air Outlet Node Name",
        "",
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
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0);
    ScheduleManager::UpdateScheduleValues();

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData1();
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    Fans::GetFanInput();
    Fans::GetFanInputFlag = false;
    PoweredInductionUnits::GetPIUs();
    EXPECT_TRUE(compare_err_stream(""));
    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;

    // node number table
    //  1   SPACE2-1 Air Node
    //  2   SPACE2-1 Return Node
    //  3   SPACE2-1 In Node
    //  4   SPACE2-1 ATU Sec Node
    //  5   SPACE2-1 ATU Fan Outlet Node
    //  6   SPACE2-1 ATU In Node
    //  7   SPACE2-1 Zone Coil Air In Node

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);

    // Setup for Zone 1 VAV No Reheat
    int ZoneNum = 1;
    int SysNum = 1;
    int ZoneNodeNum = 1;
    int SecNodeNum = PoweredInductionUnits::PIU(SysNum).SecAirInNode;
    int PriNodeNum = PoweredInductionUnits::PIU(SysNum).PriAirInNode;
    bool FirstHVACIteration = true;

    DataGlobals::BeginEnvrnFlag = true; // Must be true for initial pass thru InitPIU for this terminal unit
    FirstHVACIteration = true;
    PoweredInductionUnits::InitPIU(SysNum, FirstHVACIteration); // Run thru init once with FirstHVACIteration set to true
    Fans::InitFan(1, FirstHVACIteration);
    DataGlobals::BeginEnvrnFlag = false;
    FirstHVACIteration = false;

    // From inputs
    Real64 SecMaxMassFlow = PoweredInductionUnits::PIU(SysNum).MaxTotAirMassFlow;
    Real64 PriMaxMassFlow = PoweredInductionUnits::PIU(SysNum).MaxPriAirMassFlow;
    Real64 PriMinMassFlow = PoweredInductionUnits::PIU(SysNum).MaxPriAirMassFlow * PoweredInductionUnits::PIU(SysNum).MinPriAirFlowFrac;
    Real64 SecMassFlowAtPrimMin = PoweredInductionUnits::PIU(SysNum).MaxTotAirMassFlow - PriMinMassFlow;
    Real64 SecMassFlowAtPrimMax = PoweredInductionUnits::PIU(SysNum).MaxTotAirMassFlow - PriMaxMassFlow;

    // Note that the fan schedule is always off, so the PIU fan should only run if the night cycle turn on flag is true

    // First test - Heating load, TurnZoneFansOn is false, no primary flow - expecting no secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = false;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Second test - Heating load, TurnZoneFansOn is true, no primary flow - expecting max secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = false;
    DataHVACGlobals::TurnZoneFansOnlyOn = true;
    PoweredInductionUnits::CalcSeriesPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Third test - Cooling load TurnZoneFansOn is true, no primary flow - expecting no secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = false;
    DataHVACGlobals::TurnZoneFansOnlyOn = true;
    PoweredInductionUnits::CalcSeriesPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Fourth test - Cooling load TurnFansOn is true, no primary flow - expecting no secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(0.0, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Fifth test - Heating load TurnFansOn is true, no primary flow - expecting max secondary flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = 0.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMaxMassFlow, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Sixth test - Heating load TurnFansOn is true, yes min primary flow, deadbandorsetback is true - expecting secondary flow at primary min flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = PriMinMassFlow;
    DataLoopNode::Node(PriNodeNum).MassFlowRateMaxAvail = PriMinMassFlow;
    DataLoopNode::Node(PriNodeNum).MassFlowRateMinAvail = PriMinMassFlow;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = true;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMassFlowAtPrimMin, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Seventh test - Heating load TurnFansOn is true, yes min primary flow - expecting secondary flow at primary min flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = PriMinMassFlow;
    DataLoopNode::Node(PriNodeNum).MassFlowRateMaxAvail = PriMinMassFlow;
    DataLoopNode::Node(PriNodeNum).MassFlowRateMinAvail = PriMinMassFlow;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = 2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMassFlowAtPrimMin, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Eighth test - Cooling load TurnFansOn is true, yes primary flow at max - expecting secondary flow at primary max flow
    DataLoopNode::Node(PriNodeNum).MassFlowRate = PriMaxMassFlow;
    DataLoopNode::Node(PriNodeNum).MassFlowRateMaxAvail = PriMaxMassFlow;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).RemainingOutputRequired = -2000.0; // Heating load - expect min flow rate
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    DataHVACGlobals::TurnFansOn = true;
    DataHVACGlobals::TurnZoneFansOnlyOn = false;
    PoweredInductionUnits::CalcSeriesPIU(SysNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
    EXPECT_EQ(SecMassFlowAtPrimMax, DataLoopNode::Node(SecNodeNum).MassFlowRate);

    // Cleanup
    DataHeatBalFanSys::TempControlType.deallocate();
    DataZoneEnergyDemands::ZoneSysEnergyDemand.deallocate();
    DataZoneEnergyDemands::CurDeadBandOrSetback.deallocate();
}
