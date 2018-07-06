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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirflowNetwork.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/Furnaces.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ScheduleManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::Furnaces;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace DataZoneEnergyDemands;
using namespace DataGlobals;
using namespace ScheduleManager;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::OutputProcessor;
using namespace EnergyPlus::SimulationManager;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, SetVSHPAirFlowTest_VSFurnaceFlowTest)
{

    int FurnaceNum(1);
    Real64 OnOffAirFlowRatio; // This is a return value
    Real64 PartLoadRatio(1.0);
    Node.allocate(2);
    CurDeadBandOrSetback.allocate(1);
    Schedule.allocate(1);

    MSHPMassFlowRateLow = 0.0;
    MSHPMassFlowRateHigh = 0.0;

    Furnace.allocate(1);

    Furnace(FurnaceNum).FurnaceType_Num = UnitarySys_HeatCool;

    Furnace(FurnaceNum).FurnaceInletNodeNum = 1;
    Furnace(FurnaceNum).FurnaceOutletNodeNum = 2;
    Furnace(FurnaceNum).ControlZoneNum = 1;

    Furnace(FurnaceNum).MaxHeatAirMassFlow = 0.5;
    Furnace(FurnaceNum).MaxCoolAirMassFlow = 0.75;

    Furnace(FurnaceNum).HeatMassFlowRate.allocate(3);
    Furnace(FurnaceNum).CoolMassFlowRate.allocate(3);
    Furnace(FurnaceNum).MSHeatingSpeedRatio.allocate(3);
    Furnace(FurnaceNum).MSCoolingSpeedRatio.allocate(3);

    Furnace(FurnaceNum).LastMode = HeatingMode;
    Furnace(FurnaceNum).IdleMassFlowRate = 0.2;
    Furnace(FurnaceNum).IdleSpeedRatio = 0.2;
    Furnace(FurnaceNum).FanAvailSchedPtr = ScheduleAlwaysOn;
    Furnace(FurnaceNum).FurnaceInletNodeNum = 1;

    Furnace(FurnaceNum).HeatMassFlowRate(1) = 0.25;
    Furnace(FurnaceNum).MSHeatingSpeedRatio(1) = 0.25;
    Furnace(FurnaceNum).HeatMassFlowRate(2) = 0.5;
    Furnace(FurnaceNum).MSHeatingSpeedRatio(2) = 0.5;
    Furnace(FurnaceNum).HeatMassFlowRate(3) = 1.0;
    Furnace(FurnaceNum).MSHeatingSpeedRatio(3) = 1.0;

    Furnace(FurnaceNum).CoolMassFlowRate(1) = 0.3;
    Furnace(FurnaceNum).MSCoolingSpeedRatio(1) = 0.3;
    Furnace(FurnaceNum).CoolMassFlowRate(2) = 0.6;
    Furnace(FurnaceNum).MSCoolingSpeedRatio(2) = 0.6;
    Furnace(FurnaceNum).CoolMassFlowRate(3) = 1.2;
    Furnace(FurnaceNum).MSCoolingSpeedRatio(3) = 1.2;

    CurDeadBandOrSetback(1) = false;

    Furnace(FurnaceNum).OpMode = CycFanCycCoil;
    // heating air flow at various speeds

    Furnace(FurnaceNum).NumOfSpeedHeating = 0;
    Furnace(FurnaceNum).NumOfSpeedCooling = 0;
    //	Furnace( FurnaceNum ).SchedPtr = 0; // denotes incorrect schedule name in Furnace input ( returns 0.0 )
    Furnace(FurnaceNum).SchedPtr = -1; // denotes missing schedule name in Furnace input ( returns 1.0 )
    HeatingLoad = true;
    CoolingLoad = false;
    SetVSHPAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);

    EXPECT_DOUBLE_EQ(0.0, CompOffMassFlow);
    EXPECT_DOUBLE_EQ(0.5, CompOnMassFlow);
    EXPECT_DOUBLE_EQ(1.0, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.5, Node(Furnace(FurnaceNum).FurnaceInletNodeNum).MassFlowRate);

    Furnace(FurnaceNum).NumOfSpeedHeating = 1;
    Furnace(FurnaceNum).NumOfSpeedCooling = 0;
    HeatingLoad = true;
    CoolingLoad = false;
    SetVSHPAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.25, MSHPMassFlowRateLow);
    EXPECT_DOUBLE_EQ(0.25, MSHPMassFlowRateHigh);
    EXPECT_DOUBLE_EQ(0.0, CompOffMassFlow);
    EXPECT_DOUBLE_EQ(0.25, CompOnMassFlow);
    EXPECT_DOUBLE_EQ(1.0, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.25, Node(Furnace(FurnaceNum).FurnaceInletNodeNum).MassFlowRate);

    Furnace(FurnaceNum).NumOfSpeedHeating = 2;
    Furnace(FurnaceNum).NumOfSpeedCooling = 0;
    HeatingLoad = true;
    CoolingLoad = false;
    SetVSHPAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.5, MSHPMassFlowRateLow);
    EXPECT_DOUBLE_EQ(0.5, MSHPMassFlowRateHigh);
    EXPECT_DOUBLE_EQ(0.0, CompOffMassFlow);
    EXPECT_DOUBLE_EQ(0.5, CompOnMassFlow);
    EXPECT_DOUBLE_EQ(1.0, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.5, Node(Furnace(FurnaceNum).FurnaceInletNodeNum).MassFlowRate);

    Furnace(FurnaceNum).NumOfSpeedHeating = 3;
    Furnace(FurnaceNum).NumOfSpeedCooling = 0;
    HeatingLoad = true;
    CoolingLoad = false;
    SetVSHPAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(1.0, MSHPMassFlowRateLow);
    EXPECT_DOUBLE_EQ(1.0, MSHPMassFlowRateHigh);
    EXPECT_DOUBLE_EQ(0.0, CompOffMassFlow);
    EXPECT_DOUBLE_EQ(1.0, CompOnMassFlow);
    EXPECT_DOUBLE_EQ(1.0, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(1.0, Node(Furnace(FurnaceNum).FurnaceInletNodeNum).MassFlowRate);

    Furnace(FurnaceNum).NumOfSpeedHeating = 0;
    Furnace(FurnaceNum).NumOfSpeedCooling = 1;
    HeatingLoad = false;
    CoolingLoad = true;
    SetVSHPAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.3, MSHPMassFlowRateLow);
    EXPECT_DOUBLE_EQ(0.3, MSHPMassFlowRateHigh);
    EXPECT_DOUBLE_EQ(0.0, CompOffMassFlow);
    EXPECT_DOUBLE_EQ(0.3, CompOnMassFlow);
    EXPECT_DOUBLE_EQ(1.0, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.3, Node(Furnace(FurnaceNum).FurnaceInletNodeNum).MassFlowRate);

    Furnace(FurnaceNum).NumOfSpeedHeating = 0;
    Furnace(FurnaceNum).NumOfSpeedCooling = 2;
    HeatingLoad = false;
    CoolingLoad = true;
    SetVSHPAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.6, MSHPMassFlowRateLow);
    EXPECT_DOUBLE_EQ(0.6, MSHPMassFlowRateHigh);
    EXPECT_DOUBLE_EQ(0.0, CompOffMassFlow);
    EXPECT_DOUBLE_EQ(0.6, CompOnMassFlow);
    EXPECT_DOUBLE_EQ(1.0, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.6, Node(Furnace(FurnaceNum).FurnaceInletNodeNum).MassFlowRate);

    Furnace(FurnaceNum).NumOfSpeedHeating = 0;
    Furnace(FurnaceNum).NumOfSpeedCooling = 3;
    HeatingLoad = false;
    CoolingLoad = true;
    SetVSHPAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(1.2, MSHPMassFlowRateLow);
    EXPECT_DOUBLE_EQ(1.2, MSHPMassFlowRateHigh);
    EXPECT_DOUBLE_EQ(0.0, CompOffMassFlow);
    EXPECT_DOUBLE_EQ(1.2, CompOnMassFlow);
    EXPECT_DOUBLE_EQ(1.0, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(1.2, Node(Furnace(FurnaceNum).FurnaceInletNodeNum).MassFlowRate);

    // constant fan mode should drop to idle flow rate
    Furnace(FurnaceNum).OpMode = ContFanCycCoil;

    Furnace(FurnaceNum).NumOfSpeedHeating = 0;
    Furnace(FurnaceNum).NumOfSpeedCooling = 0;
    HeatingLoad = true;
    CoolingLoad = false;
    SetVSHPAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(0.0, MSHPMassFlowRateLow);
    EXPECT_EQ(0.0, MSHPMassFlowRateHigh);
    EXPECT_DOUBLE_EQ(0.2, CompOffMassFlow);
    EXPECT_DOUBLE_EQ(0.5, CompOnMassFlow);
    EXPECT_DOUBLE_EQ(1.0, OnOffAirFlowRatio);
    EXPECT_DOUBLE_EQ(0.5, Node(Furnace(FurnaceNum).FurnaceInletNodeNum).MassFlowRate);

    // Clean up
    Node.deallocate();
    Furnace.deallocate();
    CurDeadBandOrSetback.deallocate();
    Schedule.deallocate();
}

TEST_F(EnergyPlusFixture, FurnaceTest_PartLoadRatioTest)
{
    // Test passing variables between Furnace and AirflowNetwork #5134

    using DataAirflowNetwork::AirflowNetworkControlMultiADS;
    using DataAirflowNetwork::SimulateAirflowNetwork;
    using DataAirLoop::AirLoopAFNInfo;
//    using DataAirLoop::LoopOnOffFanPartLoadRatio;
//    using DataAirLoop::LoopSystemOffMassFlowrate;

    AirLoopAFNInfo.allocate(1);
//    LoopOnOffFanPartLoadRatio.allocate(1);

    int FurnaceNum;

    FurnaceNum = 1;
    Furnace.allocate(1);
    Furnace(FurnaceNum).FurnaceType_Num = UnitarySys_HeatPump_AirToAir;

    CompOnMassFlow = 2.0;
    CompOffMassFlow = 0.0;
    Furnace(FurnaceNum).OpMode = 1;
    Furnace(FurnaceNum).MdotFurnace = 2.0;
    Furnace(FurnaceNum).DesignMassFlowRate = 2.2;
    Furnace(FurnaceNum).HeatPartLoadRatio = 1.0;
    Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;

    SimulateAirflowNetwork = AirflowNetworkControlMultiADS;
    ReportFurnace(FurnaceNum, 1);

    EXPECT_EQ(2.0, AirLoopAFNInfo(1).LoopSystemOnMassFlowrate);
    EXPECT_EQ(0.0, AirLoopAFNInfo(1).LoopSystemOffMassFlowrate);
    EXPECT_EQ(1.0, AirLoopAFNInfo(1).LoopFanOperationMode);
    EXPECT_EQ(1.0, AirLoopAFNInfo(1).LoopOnOffFanPartLoadRatio);

    Furnace(FurnaceNum).FurnaceType_Num = UnitarySys_HeatCool;
    Furnace(FurnaceNum).HeatPartLoadRatio = 0.0;
    Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;
    Furnace(FurnaceNum).MaxCoolAirMassFlow = 2.2;
    Furnace(FurnaceNum).MaxHeatAirMassFlow = 2.0;

    ReportFurnace(FurnaceNum, 1);

    EXPECT_EQ(1.0, AirLoopAFNInfo(1).LoopOnOffFanPartLoadRatio);

    SimulateAirflowNetwork = 0;
    Furnace.deallocate();
}

TEST_F(EnergyPlusFixture, UnitaryHeatPumpAirToAir_MaxSuppAirTempTest)
{

    std::string const idf_objects = delimited_string({
        "  Version,8.6;",

        "  Timestep,6;",

        "  SimulationControl,",
        "    Yes,                     !- Do Zone Sizing Calculation",
        "    Yes,                     !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    No;                      !- Run Simulation for Weather File Run Periods",

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

        "  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

        "  Material,",
        "    A1 - 1 IN STUCCO,        !- Name",
        "    Smooth,                  !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    0.6918309,               !- Conductivity {W/m-K}",
        "    1858.142,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    C4 - 4 IN COMMON BRICK,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1922.216,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7600000,               !- Solar Absorptance",
        "    0.7600000;               !- Visible Absorptance",

        "  Material,",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name",
        "    Smooth,                  !- Roughness",
        "    1.9050000E-02,           !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1601.846,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    C6 - 8 IN CLAY TILE,     !- Name",
        "    Smooth,                  !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    0.5707605,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.8200000,               !- Solar Absorptance",
        "    0.8200000;               !- Visible Absorptance",

        "  Material,",
        "    C10 - 8 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Name",
        "    Rough,                   !- Roughness",
        "    1.2710161E-02,           !- Thickness {m}",
        "    1.435549,                !- Conductivity {W/m-K}",
        "    881.0155,                !- Density {kg/m3}",
        "    1673.600,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5500000,               !- Solar Absorptance",
        "    0.5500000;               !- Visible Absorptance",

        "  Material,",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name",
        "    Rough,                   !- Roughness",
        "    9.5402403E-03,           !- Thickness {m}",
        "    0.1902535,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    1673.600,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    B5 - 1 IN DENSE INSULATION,  !- Name",
        "    VeryRough,               !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    4.3239430E-02,           !- Conductivity {W/m-K}",
        "    91.30524,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5000000,               !- Solar Absorptance",
        "    0.5000000;               !- Visible Absorptance",

        "  Material,",
        "    C12 - 2 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    5.0901599E-02,           !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Construction,",
        "    EXTWALL80,               !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    C4 - 4 IN COMMON BRICK,  !- Layer 2",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

        "  Construction,",
        "    FLOOR SLAB 8 IN,         !- Name",
        "    C10 - 8 IN HW CONCRETE;  !- Outside Layer",

        "  Construction,",
        "    ROOF34,                  !- Name",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
        "    B5 - 1 IN DENSE INSULATION,  !- Layer 3",
        "    C12 - 2 IN HW CONCRETE;  !- Layer 4",

        "  Zone,",
        "    EAST ZONE,               !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    12.19200,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn002:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn002:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF34,                  !- Construction Name",
        "    EAST ZONE,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
        "    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
        "    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  ScheduleTypeLimits,",
        "    Any Number;              !- Name",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  ScheduleTypeLimits,",
        "    Temperature,             !- Name",
        "    -60,                     !- Lower Limit Value",
        "    200,                     !- Upper Limit Value",
        "    CONTINUOUS,              !- Numeric Type",
        "    Temperature;             !- Unit Type",

        "  ScheduleTypeLimits,",
        "    Control Type,            !- Name",
        "    0,                       !- Lower Limit Value",
        "    4,                       !- Upper Limit Value",
        "    DISCRETE;                !- Numeric Type",

        "  Schedule:Compact,",
        "    FANANDCOILAVAILSCHED,    !- Name",
        "    FRACTION,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,1.00;       !- Field 3",

        "  Schedule:Compact,",
        "    OUTDOORAIRAVAILSCHED,    !- Name",
        "    FRACTION,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.00;       !- Field 3",

        "  Schedule:Compact,",
        "    OAFRACTIONSCHED,         !- Name",
        "    FRACTION,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,0.30;       !- Field 3",

        "  Schedule:Compact,",
        "    HEATING SETPOINTS,       !- Name",
        "    TEMPERATURE,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,23.00;      !- Field 3",

        "  Schedule:Compact,",
        "    COOLING SETPOINTS,       !- Name",
        "    TEMPERATURE,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,24.00;      !- Field 3",

        "  Schedule:Compact,",
        "    ZONE CONTROL TYPE SCHED, !- Name",
        "    CONTROL TYPE,            !- Schedule Type Limits Name",
        "    Through: 3/31,           !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,1,          !- Field 3",
        "    Through: 9/30,           !- Field 5",
        "    For: Alldays,            !- Field 6",
        "    Until: 24:00,2,          !- Field 7",
        "    Through: 12/31,          !- Field 9",
        "    For: Alldays,            !- Field 10",
        "    Until: 24:00,1;          !- Field 11",

        "  Schedule:Compact,",
        "    CyclingFanSchedule,      !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,0.0;        !- Field 3",

        "  DesignSpecification:OutdoorAir,",
        "    SZ DSOA West Zone,       !- Name",
        "    flow/person,             !- Outdoor Air Method",
        "    0.00944,                 !- Outdoor Air Flow per Person {m3/s-person}",
        "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "    0.0;                     !- Outdoor Air Flow per Zone {m3/s}",

        "  Sizing:Zone,",
        "    EAST ZONE,               !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    14.,                     !- Zone Cooling Design Supply Air Temperature {C}",
        "    ,                        !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    45.,                     !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.009,                   !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.009,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA EAST ZONE,       !- Design Specification Outdoor Air Object Name",
        "    0.0,                     !- Zone Heating Sizing Factor",
        "    0.0,                     !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    0,                       !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    ;                        !- Heating Maximum Air Flow Fraction",

        "  DesignSpecification:OutdoorAir,",
        "    SZ DSOA EAST ZONE,       !- Name",
        "    flow/person,             !- Outdoor Air Method",
        "    0.00944,                 !- Outdoor Air Flow per Person {m3/s-person}",
        "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "    0.0;                     !- Outdoor Air Flow per Zone {m3/s}",

        "  Sizing:System,",
        "    Heat Pump Sys 1,         !- AirLoop Name",
        "    sensible,                !- Type of Load to Size On",
        "    autosize,                !- Design Outdoor Air Flow Rate {m3/s}",
        "    0.0,                     !- Central Heating Maximum System Air Flow Ratio",
        "    7.0,                     !- Preheat Design Temperature {C}",
        "    0.008,                   !- Preheat Design Humidity Ratio {kgWater/kgDryAir}",
        "    11.0,                    !- Precool Design Temperature {C}",
        "    0.008,                   !- Precool Design Humidity Ratio {kgWater/kgDryAir}",
        "    13.,                     !- Central Cooling Design Supply Air Temperature {C}",
        "    45.,                     !- Central Heating Design Supply Air Temperature {C}",
        "    noncoincident,           !- Type of Zone Sum to Use",
        "    no,                      !- 100% Outdoor Air in Cooling",
        "    no,                      !- 100% Outdoor Air in Heating",
        "    0.008,                   !- Central Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.008,                   !- Central Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    DesignDay,               !- Cooling Supply Air Flow Rate Method",
        "    0,                       !- Cooling Supply Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                        !- Cooling Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}",
        "    DesignDay,               !- Heating Supply Air Flow Rate Method",
        "    0,                       !- Heating Supply Air Flow Rate {m3/s}",
        "    ,                        !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                        !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "    ,                        !- Heating Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                        !- Heating Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}",
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

        "  Curve:Biquadratic,",
        "    HPACCoolCapFT,           !- Name",
        "    0.766956,                !- Coefficient1 Constant",
        "    0.0107756,               !- Coefficient2 x",
        "    -0.0000414703,           !- Coefficient3 x**2",
        "    0.00134961,              !- Coefficient4 y",
        "    -0.000261144,            !- Coefficient5 y**2",
        "    0.000457488,             !- Coefficient6 x*y",
        "    12.77778,                !- Minimum Value of x",
        "    23.88889,                !- Maximum Value of x",
        "    18.0,                    !- Minimum Value of y",
        "    46.11111,                !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    HPACCOOLEIRFT,           !- Name",
        "    0.297145,                !- Coefficient1 Constant",
        "    0.0430933,               !- Coefficient2 x",
        "    -0.000748766,            !- Coefficient3 x**2",
        "    0.00597727,              !- Coefficient4 y",
        "    0.000482112,             !- Coefficient5 y**2",
        "    -0.000956448,            !- Coefficient6 x*y",
        "    12.77778,                !- Minimum Value of x",
        "    23.88889,                !- Maximum Value of x",
        "    18.0,                    !- Minimum Value of y",
        "    46.11111,                !- Maximum Value of y",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Cubic,",
        "    HPACHeatCapFT,           !- Name",
        "    0.758746,                !- Coefficient1 Constant",
        "    0.027626,                !- Coefficient2 x",
        "    0.000148716,             !- Coefficient3 x**2",
        "    0.0000034992,            !- Coefficient4 x**3",
        "    -20.0,                   !- Minimum Value of x",
        "    20.0,                    !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Cubic,",
        "    HPACHeatCapFFF,          !- Name",
        "    0.84,                    !- Coefficient1 Constant",
        "    0.16,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Cubic,",
        "    HPACHeatEIRFT,           !- Name",
        "    1.19248,                 !- Coefficient1 Constant",
        "    -0.0300438,              !- Coefficient2 x",
        "    0.00103745,              !- Coefficient3 x**2",
        "    -0.000023328,            !- Coefficient4 x**3",
        "    -20.0,                   !- Minimum Value of x",
        "    20.0,                    !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Quadratic,",
        "    HPACCoolCapFFF,          !- Name",
        "    0.8,                     !- Coefficient1 Constant",
        "    0.2,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACCOOLEIRFFF,          !- Name",
        "    1.156,                   !- Coefficient1 Constant",
        "    -0.1816,                 !- Coefficient2 x",
        "    0.0256,                  !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACCOOLPLFFPLR,         !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACHeatEIRFFF,          !- Name",
        "    1.3824,                  !- Coefficient1 Constant",
        "    -0.4336,                 !- Coefficient2 x",
        "    0.0512,                  !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  BranchList,",
        "    Air Loop Branches,       !- Name",
        "    Air Loop Main Branch;    !- Branch 1 Name",

        "  Branch,",
        "    Air Loop Main Branch,    !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
        "    OA Sys 1,                !- Component 1 Name",
        "    Outdoor Air Mixer Inlet Node,  !- Component 1 Inlet Node Name",
        "    Mixed Air Node,          !- Component 1 Outlet Node Name",
        "    AirLoopHVAC:UnitaryHeatPump:AirToAir,  !- Component 2 Object Type",
        "    DXAC Heat Pump 1,        !- Component 2 Name",
        "    Mixed Air Node,          !- Component 2 Inlet Node Name",
        "    Air Loop Outlet Node;    !- Component 2 Outlet Node Name",

        "  AirLoopHVAC,",
        "    Heat Pump Sys 1,         !- Name",
        "    ,                        !- Controller List Name",
        "    Heat Pump 1 Avail List,  !- Availability Manager List Name",
        "    autosize,                !- Design Supply Air Flow Rate {m3/s}",
        "    Air Loop Branches,       !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    Outdoor Air Mixer Inlet Node,  !- Supply Side Inlet Node Name",
        "    Return Air Mixer Outlet, !- Demand Side Outlet Node Name",
        "    Zone Equipment Inlet Node,  !- Demand Side Inlet Node Names",
        "    Air Loop Outlet Node;    !- Supply Side Outlet Node Names",

        "  AirLoopHVAC:ControllerList,",
        "    OA Sys 1 Controllers,    !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    OA Controller 1;         !- Controller 1 Name",

        "  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys 1 Equipment,      !- Name",
        "    OutdoorAir:Mixer,        !- Component 1 Object Type",
        "    OA Mixing Box 1;         !- Component 1 Name",

        "  AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys 1,                !- Name",
        "    OA Sys 1 Controllers,    !- Controller List Name",
        "    OA Sys 1 Equipment,      !- Outdoor Air Equipment List Name",
        "    Outdoor Air 1 Avail List;!- Availability Manager List Name",

        "  OutdoorAir:NodeList,",
        "    Outside Air Inlet Node;  !- Node or NodeList Name 1",

        "  OutdoorAir:Mixer,",
        "    OA Mixing Box 1,         !- Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node,  !- Relief Air Stream Node Name",
        "    Outdoor Air Mixer Inlet Node;  !- Return Air Stream Node Name",

        "  AvailabilityManagerAssignmentList,",
        "    Heat Pump 1 Avail List,  !- Name",
        "    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
        "    Heat Pump 1 Avail;       !- Availability Manager 1 Name",

        "  AvailabilityManagerAssignmentList,",
        "    Outdoor Air 1 Avail List,!- Name",
        "    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
        "    Outdoor Air 1 Avail;     !- Availability Manager 1 Name",

        "  AvailabilityManager:Scheduled,",
        "    Heat Pump 1 Avail,       !- Name",
        "    FanAndCoilAvailSched;    !- Schedule Name",

        "  AvailabilityManager:Scheduled,",
        "    Outdoor Air 1 Avail,     !- Name",
        "    OutdoorAirAvailSched;    !- Schedule Name",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
        "    Outdoor Air Mixer Inlet Node,  !- Return Air Node Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Actuator Node Name",
        "    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    NoEconomizer,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    ProportionalMinimum,     !- Minimum Limit Type",
        "    OAFractionSched;         !- Minimum Outdoor Air Schedule Name",

        "  ZoneHVAC:EquipmentConnections,",
        "    EAST ZONE,               !- Zone Name",
        "    Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "    Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    Zone 2 Node,             !- Zone Air Node Name",
        "    Zone 2 Outlet Node;      !- Zone Return Air Node Name",

        "  ZoneHVAC:EquipmentList,",
        "    Zone2Equipment,          !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    Zone2DirectAirADU,       !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:AirDistributionUnit,",
        "    Zone2DirectAirADU,       !- Name",
        "    Zone 2 Inlet Node,       !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:ConstantVolume:NoReheat,  !- Air Terminal Object Type",
        "    Zone2DirectAir;      !- Air Terminal Name",

        "  AirTerminal:SingleDuct:ConstantVolume:NoReheat,",
        "    Zone2DirectAir,          !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    Zone 2 Inlet Node 2AT,   !- Air Inlet Node Name",
        "    Zone 2 Inlet Node,       !- Air Outlet Node Name",
        "    autosize;                !- Maximum Air Flow Rate {m3/s}",

        "  AirLoopHVAC:UnitaryHeatPump:AirToAir,",
        "    DXAC Heat Pump 1,        !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    Mixed Air Node,          !- Air Inlet Node Name",
        "    Air Loop Outlet Node,    !- Air Outlet Node Name",
        "    autosize,                !- Cooling Supply Air Flow Rate {m3/s}",
        "    autosize,                !- Heating Supply Air Flow Rate {m3/s}",
        "    autosize,                !- No Load Supply Air Flow Rate {m3/s}",
        "    East Zone,               !- Controlling Zone or Thermostat Location",
        "    Fan:OnOff,               !- Supply Air Fan Object Type",
        "    Supply Fan 1,            !- Supply Air Fan Name",
        "    Coil:Heating:DX:SingleSpeed,  !- Heating Coil Object Type",
        "    Heat Pump DX Heating Coil 1,  !- Heating Coil Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
        "    Heat Pump ACDXCoil 1,    !- Cooling Coil Name",
        "    Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "    Heat Pump DX Supp Heating Coil 1,  !- Supplemental Heating Coil Name",
        "    autosize,                !- Maximum Supply Air Temperature from Supplemental Heater {C}",
        "    21.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "    BlowThrough,             !- Fan Placement",
        "    CyclingFanSchedule;      !- Supply Air Fan Operating Mode Schedule Name",

        "  ZoneControl:Thermostat,",
        "    Zone 2 Thermostat,       !- Name",
        "    EAST ZONE,               !- Zone or ZoneList Name",
        "    Zone Control Type Sched, !- Control Type Schedule Name",
        "    ThermostatSetpoint:SingleHeating,  !- Control 1 Object Type",
        "    Heating Setpoint with SB,!- Control 1 Name",
        "    ThermostatSetpoint:SingleCooling,  !- Control 2 Object Type",
        "    Cooling Setpoint with SB;!- Control 2 Name",

        "  ThermostatSetpoint:SingleHeating,",
        "    Heating Setpoint with SB,!- Name",
        "    Heating Setpoints;       !- Setpoint Temperature Schedule Name",

        "  ThermostatSetpoint:SingleCooling,",
        "    Cooling Setpoint with SB,!- Name",
        "    Cooling Setpoints;       !- Setpoint Temperature Schedule Name",

        "  AirLoopHVAC:SupplyPath,",
        "    HeatPumpSupplyPath,      !- Name",
        "    Zone Equipment Inlet Node,  !- Supply Air Path Inlet Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
        "    Zone Supply Air Splitter;!- Component 1 Name",

        "  AirLoopHVAC:ReturnPath,",
        "    HeatPumpReturnPath,      !- Name",
        "    Return Air Mixer Outlet, !- Return Air Path Outlet Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
        "    Zone Return Air Mixer;   !- Component 1 Name",

        "  AirLoopHVAC:ZoneSplitter,",
        "    Zone Supply Air Splitter,!- Name",
        "    Zone Equipment Inlet Node,  !- Inlet Node Name",
        "    Zone 2 Inlet Node 2AT;   !- Outlet 2 Node Name",

        "  AirLoopHVAC:ZoneMixer,",
        "    Zone Return Air Mixer,   !- Name",
        "    Return Air Mixer Outlet, !- Outlet Node Name",
        "    Zone 2 Outlet Node;      !- Inlet 2 Node Name",

        "  Coil:Heating:Fuel,",
        "    Heat Pump DX Supp Heating Coil 1,  !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    NaturalGas,              !- Fuel Type",
        "    0.8,                     !- Burner Efficiency",
        "    100000.0,                !- Nominal Capacity {W}",
        "    SuppHeating Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    Air Loop Outlet Node;    !- Air Outlet Node Name",

        "  Coil:Cooling:DX:SingleSpeed,",
        "    Heat Pump ACDXCoil 1,    !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    autosize,                !- Gross Rated Total Cooling Capacity {W}",
        "    autosize,                !- Gross Rated Sensible Heat Ratio",
        "    3.0,                     !- Gross Rated Cooling COP {W/W}",
        "    autosize,                !- Rated Air Flow Rate {m3/s}",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "    HPACCoolCapFT,           !- Total Cooling Capacity Function of Temperature Curve Name",
        "    HPACCoolCapFFF,          !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    HPACCOOLEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name",
        "    HPACCOOLEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    HPACCOOLPLFFPLR;         !- Part Load Fraction Correlation Curve Name",

        "  Coil:Heating:DX:SingleSpeed,",
        "    Heat Pump DX Heating Coil 1,  !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    autosize,                !- Gross Rated Heating Capacity {W}",
        "    2.75,                    !- Gross Rated Heating COP {W/W}",
        "    autosize,                !- Rated Air Flow Rate {m3/s}",
        "    ,                        !- Rated Supply Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    Heating Coil Air Inlet Node,  !- Air Inlet Node Name",
        "    SuppHeating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "    HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name",
        "    HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name",
        "    HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name",
        "    HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    HPACCOOLPLFFPLR,         !- Part Load Fraction Correlation Curve Name",
        "    ,                        !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "    -8.0,                    !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    ,                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "    5.0,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "    200.0,                   !- Crankcase Heater Capacity {W}",
        "    10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "    Resistive,               !- Defrost Strategy",
        "    TIMED,                   !- Defrost Control",
        "    0.166667,                !- Defrost Time Period Fraction",
        "    autosize;                !- Resistive Defrost Heater Capacity {W}",

        "  Fan:OnOff,",
        "    Supply Fan 1,            !- Name",
        "    FanAndCoilAvailSched,    !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    300.0,                   !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    Mixed Air Node,          !- Air Inlet Node Name",
        "    DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    int CompIndex(0);
    int AirLoopNum(1);
    bool FirstHVACIteration(false);

    OutputProcessor::TimeValue.allocate(2); //
    ManageSimulation();
    // check the design max air outlet temperature
    EXPECT_DOUBLE_EQ(45.0, Furnace(1).DesignMaxOutletTemp);

    ZoneSysEnergyDemand(1).SequencedOutputRequiredToCoolingSP(1) = 25000.0;
    ZoneSysEnergyDemand(1).SequencedOutputRequiredToHeatingSP(1) = 25000.0;
    SimFurnace(Furnace(1).Name, FirstHVACIteration, AirLoopNum, CompIndex);
    // check the heating mode is On
    EXPECT_TRUE(Furnaces::HeatingLoad);
    // check the cooling mode is Off
    EXPECT_FALSE(Furnaces::CoolingLoad);
    // check if the air-to-air heat pump outlet temperature is capped at 45.0C
    EXPECT_NEAR(45.0, Node(Furnace(1).FurnaceOutletNodeNum).Temp, 0.000001);
}

} // namespace EnergyPlus
