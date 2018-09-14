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

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include <BranchInputManager.hh>
#include <DataEnvironment.hh>
#include <DataLoopNode.hh>
#include <ElectricPowerServiceManager.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <HeatBalanceManager.hh>
#include <HeatPumpWaterToWaterSimple.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Plant/PlantManager.hh>
#include <PlantUtilities.hh>
#include <SimulationManager.hh>
#include <SizingManager.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, PlantLoopSourceSideTest)
{
    // this test is related to issue #4385 and #4972, test change that added plant loop interconnects
    // entire IDF file from defect file used to set up model and run it some.
    std::string const idf_objects =
        delimited_string({"Version,8.3;",
                          "Schedule:Constant,Radiator massflow temporary,Any value sch,1;",
                          "Schedule:Constant,Radiator supply temperature temporary,Any value sch,40;",
                          "Schedule:Constant,Radiator power demand temporary,Any value sch,3000;",

                          "Pump:VariableSpeed,",
                          "    GHEV pump,               !- Name",
                          "    GHEV pump Inlet Node,    !- Inlet Node Name",
                          "    GHEV Pump Outlet Node,   !- Outlet Node Name",
                          "    0.0006,                  !- Rated Flow Rate {m3/s}",
                          "    40000,                   !- Rated Pump Head {Pa}",
                          "    34.18803,                !- Rated Power Consumption {W}",
                          "    0.9,                     !- Motor Efficiency",
                          "    ,                        !- Fraction of Motor Inefficiencies to Fluid Stream",
                          "    ,                        !- Coefficient 1 of the Part Load Performance Curve",
                          "    1,                       !- Coefficient 2 of the Part Load Performance Curve",
                          "    ,                        !- Coefficient 3 of the Part Load Performance Curve",
                          "    ,                        !- Coefficient 4 of the Part Load Performance Curve",
                          "    0,                       !- Minimum Flow Rate {m3/s}",
                          "    Intermittent;            !- Pump Control Type",

                          "Pump:VariableSpeed,",
                          "    RAD HP pump,             !- Name",
                          "    RadHP pump inlet Node,   !- Inlet Node Name",
                          "    RADHP Pump Outlet Node,  !- Outlet Node Name",
                          "    0.0005,                  !- Rated Flow Rate {m3/s}",
                          "    40000,                   !- Rated Pump Head {Pa}",
                          "    28.49003,                !- Rated Power Consumption {W}",
                          "    0.9,                     !- Motor Efficiency",
                          "    ,                        !- Fraction of Motor Inefficiencies to Fluid Stream",
                          "    ,                        !- Coefficient 1 of the Part Load Performance Curve",
                          "    1,                       !- Coefficient 2 of the Part Load Performance Curve",
                          "    ,                        !- Coefficient 3 of the Part Load Performance Curve",
                          "    ,                        !- Coefficient 4 of the Part Load Performance Curve",
                          "    0,                       !- Minimum Flow Rate {m3/s}",
                          "    Intermittent;            !- Pump Control Type",

                          "SetpointManager:FollowGroundTemperature,",
                          "    GHEV setpmgr,            !- Name",
                          "    Temperature,             !- Control Variable",
                          "    Site:GroundTemperature:Deep,  !- Reference Ground Temperature Object Type",
                          "    0,                       !- Offset Temperature Difference {deltaC}",
                          "    20,                      !- Maximum Setpoint Temperature {C}",
                          "    -10,                     !- Minimum Setpoint Temperature {C}",
                          "    GHEV Supply Exit Pipe Outlet Node;  !- Setpoint Node or NodeList Name",

                          "PlantEquipmentOperation:HeatingLoad,",
                          "    Bore hole PEO,           !- Name",
                          "    0,                       !- Load Range 1 Lower Limit {W}",
                          "    1000000,                 !- Load Range 1 Upper Limit {W}",
                          "    Heat sources;            !- Range 1 Equipment List Name",

                          "ScheduleTypeLimits,",
                          "    bore hole brine temp,    !- Name",
                          "    -10,                     !- Lower Limit Value",
                          "    10,                      !- Upper Limit Value",
                          "    Continuous,              !- Numeric Type",
                          "    Temperature;             !- Unit Type",

                          "ScheduleTypeLimits,",
                          "    rad power limit,         !- Name",
                          "    0,                       !- Lower Limit Value",
                          "    10000,                   !- Upper Limit Value",
                          "    Continuous,              !- Numeric Type",
                          "    Power;                   !- Unit Type",

                          "ScheduleTypeLimits,",
                          "    rad massflow,            !- Name",
                          "    0,                       !- Lower Limit Value",
                          "    1,                       !- Upper Limit Value",
                          "    Continuous;              !- Numeric Type",

                          "ScheduleTypeLimits,",
                          "    on/off,                  !- Name",
                          "    0,                       !- Lower Limit Value",
                          "    1,                       !- Upper Limit Value",
                          "    Discrete,                !- Numeric Type",
                          "    Dimensionless;           !- Unit Type",

                          "ScheduleTypeLimits,",
                          "    rad water t,             !- Name",
                          "    10,                      !- Lower Limit Value",
                          "    80,                      !- Upper Limit Value",
                          "    Continuous,              !- Numeric Type",
                          "    Temperature;             !- Unit Type",

                          "ScheduleTypeLimits,",
                          "    Any value sch,           !- Name",
                          "    ,                        !- Lower Limit Value",
                          "    ,                        !- Upper Limit Value",
                          "    Continuous,              !- Numeric Type",
                          "    Dimensionless;           !- Unit Type",

                          "Schedule:Constant,Brine return temp,bore hole brine temp,5;",

                          "Site:Location,",
                          "    STOCKHOLM_ ARLANDA_SWE Design_Conditions,  !- Name",
                          "    59.65,                   !- Latitude {deg}",
                          "    17.95,                   !- Longitude {deg}",
                          "    1.00,                    !- Time Zone {hr}",
                          "    61.00;                   !- Elevation {m}",

                          "SizingPeriod:DesignDay,",
                          "    STOCKHOLM_ ARLANDA Ann Htg 99.6% Condns DB,  !- Name",
                          "    2,                       !- Month",
                          "    21,                      !- Day of Month",
                          "    WinterDesignDay,         !- Day Type",
                          "    -17.8,                   !- Maximum Dry-Bulb Temperature {C}",
                          "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
                          "    DefaultMultipliers,      !- Dry-Bulb Temperature Range Modifier Type",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
                          "    Wetbulb,                 !- Humidity Condition Type",
                          "    -17.8,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
                          "    ,                        !- Humidity Condition Day Schedule Name",
                          "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
                          "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
                          "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
                          "    100594.,                 !- Barometric Pressure {Pa}",
                          "    2.4,                     !- Wind Speed {m/s}",
                          "    250,                     !- Wind Direction {deg}",
                          "    No,                      !- Rain Indicator",
                          "    No,                      !- Snow Indicator",
                          "    No,                      !- Daylight Saving Time Indicator",
                          "    ASHRAEClearSky,          !- Solar Model Indicator",
                          "    ,                        !- Beam Solar Day Schedule Name",
                          "    ,                        !- Diffuse Solar Day Schedule Name",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
                          "    0.00;                    !- Sky Clearness",

                          "Timestep,1;",

                          "Building,",
                          "    MustHaveObject,          !- Name",
                          "    ,                        !- North Axis {deg}",
                          "    Suburbs,                 !- Terrain",
                          "    0.04,                    !- Loads Convergence Tolerance Value",
                          "    0.4,                     !- Temperature Convergence Tolerance Value {deltaC}",
                          "    FullExterior,            !- Solar Distribution",
                          "    25,                      !- Maximum Number of Warmup Days",
                          "    6;                       !- Minimum Number of Warmup Days",

                          "GlobalGeometryRules,",
                          "   UpperLeftCorner,         !- Starting Vertex Position",
                          "   Counterclockwise,        !- Vertex Entry Direction",
                          "    Relative,                !- Coordinate System",
                          "   Relative,                !- Daylighting Reference Point Coordinate System",
                          "   Relative;                !- Rectangular Surface Coordinate System",

                          "PlantEquipmentOperationSchemes,",
                          "    GHEV PEOS,               !- Name",
                          "    PlantEquipmentOperation:HeatingLoad,  !- Control Scheme 1 Object Type",
                          "    Bore hole PEO,           !- Control Scheme 1 Name",
                          "    always on;               !- Control Scheme 1 Schedule Name",

                          "Pipe:Adiabatic,",
                          "    GHEV supp bypass pipe,   !- Name",
                          "    GHEV Supply Bypass pipe Inlet Node,  !- Inlet Node Name",
                          "    GHEV Supply Bypass pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    GHEV supp exit pipe,     !- Name",
                          "    GHEV Supply Exit Pipe Inlet Node,  !- Inlet Node Name",
                          "    GHEV Supply Exit Pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    GHEV dem entrance pipe,  !- Name",
                          "    GHEV Demand Entrance pipe inlet Node,  !- Inlet Node Name",
                          "    GHEV Demand Entrance pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    GHEV dem bypass pipe,    !- Name",
                          "    GHEV Demand Bypass pipe Inlet node,  !- Inlet Node Name",
                          "    GHEV Demand Bypass pipe Outlet node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    GHEV dem exit pipe,      !- Name",
                          "    GHEV Demand Exit pipe inlet Node,  !- Inlet Node Name",
                          "    GHEV Demand Exit pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP supp bypass pipe,  !- Name",
                          "    RADHP Supply Bypass pipe Inlet Node,  !- Inlet Node Name",
                          "    RADHP Supply Bypass pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP supp exit pipe,    !- Name",
                          "    RadHP Supply Exit Pipe Inlet Node,  !- Inlet Node Name",
                          "    RadHP Supply Exit Pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP dem entrance pipe, !- Name",
                          "    RADHP Demand Entrance pipe Inlet Node,  !- Inlet Node Name",
                          "    RADHP Demand Entrance pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP dem bypass pipe,   !- Name",
                          "    RAD HP Demand Bypass pipe Inlet Node,  !- Inlet Node Name",
                          "    RAD HP Demand Bypass pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP dem exit pipe,     !- Name",
                          "    RADHP Demand Exit Pipe Inlet Node,  !- Inlet Node Name",
                          "    RADHP Demand Exit Pipe Outlet Node;  !- Outlet Node Name",

                          "SimulationControl,",
                          "    No,                      !- Do Zone Sizing Calculation",
                          "    No,                      !- Do System Sizing Calculation",
                          "    No,                     !- Do Plant Sizing Calculation",
                          "    YES,                      !- Run Simulation for Sizing Periods",
                          "    NO;                     !- Run Simulation for Weather File Run Periods",

                          "SetpointManager:Scheduled,",
                          "    RAD HP setpointmgr,      !- Name",
                          "    Temperature,             !- Control Variable",
                          "    Radiator supply temperature temporary,  !- Schedule Name",
                          "    RadHP Supply Exit Pipe Outlet Node;  !- Setpoint Node or NodeList Name",

                          "PlantEquipmentList,",
                          "    Rad HPs,                 !- Name",
                          "    HeatPump:WaterToWater:EquationFit:Heating,  !- Equipment 1 Object Type",
                          "    Rad Heat Pump;           !- Equipment 1 Name",

                          "PlantEquipmentOperationSchemes,",
                          "    RAD PEOS,                !- Name",
                          "    PlantEquipmentOperation:HeatingLoad,  !- Control Scheme 1 Object Type",
                          "    RAD heating PEO,         !- Control Scheme 1 Name",
                          "    always on;               !- Control Scheme 1 Schedule Name",

                          "PlantEquipmentList,",
                          "    Heat sources,            !- Name",
                          "    GroundHeatExchanger:System,  !- Equipment 1 Object Type",
                          "    Vertical GHE JL2015;     !- Equipment 1 Name",

                          "PlantEquipmentOperation:HeatingLoad,",
                          "    RAD heating PEO,         !- Name",
                          "    0,                       !- Load Range 1 Lower Limit {W}",
                          "    1000000,                 !- Load Range 1 Upper Limit {W}",
                          "    Rad HPs;                 !- Range 1 Equipment List Name",

                          "Schedule:Compact,",
                          "    always on,               !- Name",
                          "    on/off,                  !- Schedule Type Limits Name",
                          "    THROUGH: 12/31,          !- Field 1",
                          "    FOR: AllDays,            !- Field 2",
                          "    UNTIL: 24:00, 1;         !- Field 4",

                          "  GroundHeatExchanger:System,",
                          "    Vertical GHE JL2015,      !- Name",
                          "    GHEV Borehole Inlet Node, !- Inlet Node Name",
                          "    GHEV Borehole Outlet Node, !- Outlet Node Name",
                          "    0.000303,                 !- Design Flow Rate {m3/s}",
                          "    Site:GroundTemperature:Undisturbed:KusudaAchenbach, !- Undisturbed Ground Temperature Model Type",
                          "    Vertical GHE JL2015 Ground Temps, !- Undisturbed Ground Temperature Model Name",
                          "    2.493,                    !- Ground Thermal Conductivity {W/m-K}",
                          "    2495700,                  !- Ground Thermal Heat Capacity {J/m3-K}",
                          "    Vertical GHE JL2015 g-functions; !- Response Factors Object Name",

                          "  GroundHeatExchanger:Vertical:Properties,",
                          "    Vertical GHE JL2015 Props,        !- Name",
                          "    1,                  !- Depth of Top of Borehole {m}",
                          "    160,                !- Borehole Length {m}",
                          "    0.1143,             !- Borehole Diameter {m}",
                          "    0.744,              !- Grout Thermal Conductivity {W/m-K}",
                          "    3.90E+06,           !- Grout Thermal Heat Capacity {J/m3-K}",
                          "    0.389,              !- Pipe Thermal Conductivity {W/m-K}",
                          "    1.77E+06,           !- Pipe Thermal Heat Capacity {J/m3-K}",
                          "    0.0267,             !- Pipe Outer Diameter {m}",
                          "    0.00243,            !- Pipe Thickness {m}",
                          "    0.04556;            !- U-Tube Distance {m}",

                          "  Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
                          "    Vertical GHE JL2015 Ground Temps, !- Undisturbed Ground Temperature Model Name",
                          "    2.493,                    !- Soil Thermal Conductivity {W/m-K}",
                          "    920,                      !- Soil Density {kg/m3}",
                          "    2712.72,                  !- Soil Specific Heat {J/kg-K}",
                          "    8,                        !- Average Soil Surface Temperature {C}",
                          "    3.2,                      !- Average Amplitude of Surface Temperature {deltaC}",
                          "    8;                        !- Phase Shift of Minimum Surface Temperature {days}",

                          "  GroundHeatExchanger:ResponseFactors,",
                          "    Vertical GHE JL2015 g-functions, !- Name",
                          "    Vertical GHE JL2015 Props, !- Name",
                          "    1,                        !- Number of Bore Holes",
                          "    0.0005,                   !- G-Function Reference Ratio {dimensionless}",
                          "    -4.5,                     !- G-Function Ln(T/Ts) Value 1",
                          "    4.7,                      !- G-Function G Value 1",
                          "    -4,                       !- G-Function Ln(T/Ts) Value 2",
                          "    4.85,                     !- G-Function G Value 2",
                          "    -3.5,                     !- G-Function Ln(T/Ts) Value 3",
                          "    5.1,                      !- G-Function G Value 3",
                          "    -3,                       !- G-Function Ln(T/Ts) Value 4",
                          "    5.3,                      !- G-Function G Value 4",
                          "    -2.5,                     !- G-Function Ln(T/Ts) Value 5",
                          "    5.56,                     !- G-Function G Value 5",
                          "    -2,                       !- G-Function Ln(T/Ts) Value 6",
                          "    5.76,                     !- G-Function G Value 6",
                          "    -1.5,                     !- G-Function Ln(T/Ts) Value 7",
                          "    5.97,                     !- G-Function G Value 7",
                          "    -1,                       !- G-Function Ln(T/Ts) Value 8",
                          "    6.19,                     !- G-Function G Value 8",
                          "    -.5,                      !- G-Function Ln(T/Ts) Value 9",
                          "    6.31,                     !- G-Function G Value 9",
                          "    0,                        !- G-Function Ln(T/Ts) Value 10",
                          "    6.42,                     !- G-Function G Value 10",
                          "    0.5,                      !- G-Function Ln(T/Ts) Value 11",
                          "    6.56,                     !- G-Function G Value 11",
                          "    1,                        !- G-Function Ln(T/Ts) Value 12",
                          "    6.61,                     !- G-Function G Value 12",
                          "    1.5,                      !- G-Function Ln(T/Ts) Value 13",
                          "    6.66,                     !- G-Function G Value 13",
                          "    2,                        !- G-Function Ln(T/Ts) Value 14",
                          "    6.7,                      !- G-Function G Value 14",
                          "    2.5,                      !- G-Function Ln(T/Ts) Value 15",
                          "    6.72,                     !- G-Function G Value 15",
                          "    3,                        !- G-Function Ln(T/Ts) Value 16",
                          "    6.73;                     !- G-Function G Value 16",

                          "LoadProfile:Plant,",
                          "    Radiator load,           !- Name",
                          "    Radiator Demand profile Inlet Node,  !- Inlet Node Name",
                          "    radiator demand profile outlet node,  !- Outlet Node Name",
                          "    Radiator power demand temporary,  !- Load Schedule Name",
                          "    0.00025,                 !- Peak Flow Rate {m3/s}",
                          "    Radiator massflow temporary;  !- Flow Rate Fraction Schedule Name",

                          "HeatPump:WaterToWater:EquationFit:Heating,",
                          "    Rad Heat Pump,           !- Name",
                          "    HPR brine Inlet node,    !- Source Side Inlet Node Name",
                          "    HPR brine Outlet node,   !- Source Side Outlet Node Name",
                          "    HPR radiator water return node,  !- Load Side Inlet Node Name",
                          "    HPR radiator supply node,!- Load Side Outlet Node Name",
                          "    0.00013,                 !- Rated Load Side Flow Rate {m3/s}",
                          "    0.0003,                  !- Rated Source Side Flow Rate {m3/s}",
                          "    5490,                    !- Rated Heating Capacity {W}",
                          "    1640,                    !- Rated Heating Power Consumption {W}",
                          "    -3.01043,                !- Heating Capacity Coefficient 1",
                          "    -.51452,                 !- Heating Capacity Coefficient 2",
                          "    4.515927,                !- Heating Capacity Coefficient 3",
                          "    0.017971,                !- Heating Capacity Coefficient 4",
                          "    0.155798,                !- Heating Capacity Coefficient 5",
                          "    -2.65423,                !- Heating Compressor Power Coefficient 1",
                          "    8.570358,                !- Heating Compressor Power Coefficient 2",
                          "    1.21629,                 !- Heating Compressor Power Coefficient 3",
                          "    -.21629,                 !- Heating Compressor Power Coefficient 4",
                          "    0.033862;                !- Heating Compressor Power Coefficient 5",

                          "PlantLoop,",
                          "    GHEV Loop,               !- Name",
                          "    Water,                   !- Fluid Type",
                          "    ,                        !- User Defined Fluid Type",
                          "    GHEV PEOS,               !- Plant Equipment Operation Scheme Name",
                          "    GHEV Supply Exit Pipe Outlet Node,  !- Loop Temperature Setpoint Node Name",
                          "    13,                      !- Maximum Loop Temperature {C}",
                          "    -5,                      !- Minimum Loop Temperature {C}",
                          "    0.0008,                  !- Maximum Loop Flow Rate {m3/s}",
                          "    0,                       !- Minimum Loop Flow Rate {m3/s}",
                          "    0.15,                    !- Plant Loop Volume {m3}",
                          "    GHEV pump Inlet Node,    !- Plant Side Inlet Node Name",
                          "    GHEV Supply Exit Pipe Outlet Node,  !- Plant Side Outlet Node Name",
                          "    GHEV Supply Branches,    !- Plant Side Branch List Name",
                          "    GHEV Supply Connectors,  !- Plant Side Connector List Name",
                          "    GHEV Demand Entrance pipe inlet Node,  !- Demand Side Inlet Node Name",
                          "    GHEV Demand Exit pipe Outlet Node,  !- Demand Side Outlet Node Name",
                          "    GHEV Demand Branches,    !- Demand Side Branch List Name",
                          "    GHEV Demand Connectors,  !- Demand Side Connector List Name",
                          "    SequentialLoad,          !- Load Distribution Scheme",
                          "    ,                        !- Availability Manager List Name",
                          "    SingleSetpoint,          !- Plant Loop Demand Calculation Scheme",
                          "    ,                        !- Common Pipe Simulation",
                          "    ,                        !- Pressure Simulation Type",
                          "    2.0;                     !- Loop Circulation Time {minutes}",

                          "PlantLoop,",
                          "    RadHP Loop,              !- Name",
                          "    Water,                   !- Fluid Type",
                          "    ,                        !- User Defined Fluid Type",
                          "    RAD PEOS,                !- Plant Equipment Operation Scheme Name",
                          "    RadHP Supply Exit Pipe Outlet Node,  !- Loop Temperature Setpoint Node Name",
                          "   80,                      !- Maximum Loop Temperature {C}",
                          "    22,                      !- Minimum Loop Temperature {C}",
                          "    0.0005,                  !- Maximum Loop Flow Rate {m3/s}",
                          "    0,                       !- Minimum Loop Flow Rate {m3/s}",
                          "    0.06,                    !- Plant Loop Volume {m3}",
                          "    RadHP pump inlet Node,   !- Plant Side Inlet Node Name",
                          "    RadHP Supply Exit Pipe Outlet Node,  !- Plant Side Outlet Node Name",
                          "    RADHP Supply Branches,   !- Plant Side Branch List Name",
                          "    RADHP Supply Connectors, !- Plant Side Connector List Name",
                          "    RADHP Demand Entrance pipe Inlet Node,  !- Demand Side Inlet Node Name",
                          "    RADHP Demand Exit Pipe Outlet Node,  !- Demand Side Outlet Node Name",
                          "    RADHP Demand Branches,   !- Demand Side Branch List Name",
                          "    RADHP Demand Connectors, !- Demand Side Connector List Name",
                          "    Optimal,                 !- Load Distribution Scheme",
                          "    ,                        !- Availability Manager List Name",
                          "    SingleSetpoint,          !- Plant Loop Demand Calculation Scheme",
                          "    ,                        !- Common Pipe Simulation",
                          "    ,                        !- Pressure Simulation Type",
                          "    2.0;                     !- Loop Circulation Time {minutes}",

                          "BranchList,",
                          "    GHEV Supply Branches,    !- Name",
                          "    GHEV Supply Pump Branch, !- Branch 1 Name",
                          "    GHEV Supply Borehole Branch,  !- Branch 2 Name",
                          "    GHEV Supply Bypass Branch,  !- Branch 3 Name",
                          "    GHEV Supply Outlet Branch;  !- Branch 4 Name",

                          "BranchList,",
                          "    GHEV Demand Branches,    !- Name",
                          "    GHEV Demand Inlet Branch,!- Branch 1 Name",
                          "    GHEV Demand HPRad Branch,!- Branch 2 Name",
                          "    GHEV Demand Bypass Branch,  !- Branch 3 Name",
                          "    GHEV Demand Outlet Branch;  !- Branch 4 Name",

                          "BranchList,",
                          "    RADHP Supply Branches,   !- Name",
                          "    RADHP Supply Inlet Pump, !- Branch 1 Name",
                          "    RADHP Supply HPR Branch ,!- Branch 2 Name",
                          "    RADHP Supply Bypass Branch,  !- Branch 3 Name",
                          "    RADHP Supply Outlet Branch;  !- Branch 4 Name",

                          "BranchList,",
                          "    RADHP Demand Branches,   !- Name",
                          "    RADHP Demand Inlet Branch,  !- Branch 1 Name",
                          "    RADHP Demand Radload Branch ,  !- Branch 2 Name",
                          "    RADHP Demand Bypass Branch ,  !- Branch 3 Name",
                          "    RADHP Demand Outlet Branch;  !- Branch 4 Name",

                          "Connector:Splitter,",
                          "    GHEV Supply Splitter,    !- Name",
                          "    GHEV Supply Pump Branch, !- Inlet Branch Name",
                          "    GHEV Supply Borehole Branch,  !- Outlet Branch 1 Name",
                          "    GHEV Supply Bypass Branch;  !- Outlet Branch 2 Name",

                          "Branch,",
                          "    GHEV Supply Pump Branch, !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pump:VariableSpeed,      !- Component 1 Object Type",
                          "    GHEV pump,               !- Component 1 Name",
                          "    GHEV pump Inlet Node,    !- Component 1 Inlet Node Name",
                          "    GHEV Pump Outlet Node;   !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Supply Borehole Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    GroundHeatExchanger:System,  !- Component 1 Object Type",
                          "    Vertical GHE JL2015,     !- Component 1 Name",
                          "    GHEV Borehole Inlet Node,!- Component 1 Inlet Node Name",
                          "    GHEV Borehole Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Supply Bypass Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV supp bypass pipe,   !- Component 1 Name",
                          "    GHEV Supply Bypass pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    GHEV Supply Bypass pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Supply Outlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV supp exit pipe,     !- Component 1 Name",
                          "    GHEV Supply Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    GHEV Supply Exit Pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Demand Inlet Branch,!- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV dem entrance pipe,  !- Component 1 Name",
                          "    GHEV Demand Entrance pipe inlet Node,  !- Component 1 Inlet Node Name",
                          "    GHEV Demand Entrance pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Demand HPRad Branch,!- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    HeatPump:WaterToWater:EquationFit:Heating,  !- Component 1 Object Type",
                          "    Rad Heat Pump,           !- Component 1 Name",
                          "    HPR brine Inlet node,    !- Component 1 Inlet Node Name",
                          "    HPR brine Outlet node;   !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Demand Bypass Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV dem bypass pipe,    !- Component 1 Name",
                          "    GHEV Demand Bypass pipe Inlet node,  !- Component 1 Inlet Node Name",
                          "    GHEV Demand Bypass pipe Outlet node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Demand Outlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV dem exit pipe,      !- Component 1 Name",
                          "    GHEV Demand Exit pipe inlet Node,  !- Component 1 Inlet Node Name",
                          "    GHEV Demand Exit pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Supply Inlet Pump, !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pump:VariableSpeed,      !- Component 1 Object Type",
                          "    RAD HP pump,             !- Component 1 Name",
                          "    RadHP pump inlet Node,   !- Component 1 Inlet Node Name",
                          "    RADHP Pump Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Supply HPR Branch ,!- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    HeatPump:WaterToWater:EquationFit:Heating,  !- Component 1 Object Type",
                          "    Rad Heat Pump,           !- Component 1 Name",
                          "    HPR radiator water return node,  !- Component 1 Inlet Node Name",
                          "    HPR radiator supply node;!- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Supply Bypass Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP supp bypass pipe,  !- Component 1 Name",
                          "    RADHP Supply Bypass pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RADHP Supply Bypass pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Supply Outlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP supp exit pipe,    !- Component 1 Name",
                          "    RadHP Supply Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RadHP Supply Exit Pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Demand Inlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP dem entrance pipe, !- Component 1 Name",
                          "    RADHP Demand Entrance pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RADHP Demand Entrance pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Demand Radload Branch ,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    LoadProfile:Plant,       !- Component 1 Object Type",
                          "    Radiator load,           !- Component 1 Name",
                          "    Radiator Demand profile Inlet Node,  !- Component 1 Inlet Node Name",
                          "    radiator demand profile outlet node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Demand Bypass Branch ,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP dem bypass pipe,   !- Component 1 Name",
                          "    RAD HP Demand Bypass pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RAD HP Demand Bypass pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Demand Outlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP dem exit pipe,     !- Component 1 Name",
                          "    RADHP Demand Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RADHP Demand Exit Pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Connector:Splitter,",
                          "    GHEV Demand Splitter,    !- Name",
                          "    GHEV Demand Inlet Branch,!- Inlet Branch Name",
                          "    GHEV Demand HPRad Branch,!- Outlet Branch 1 Name",
                          "    GHEV Demand Bypass Branch;  !- Outlet Branch 2 Name",

                          "Connector:Splitter,",
                          "    RadHP Supply Splitter,   !- Name",
                          "    RADHP Supply Inlet Pump, !- Inlet Branch Name",
                          "    RADHP Supply HPR Branch ,!- Outlet Branch 1 Name",
                          "    RADHP Supply Bypass Branch;  !- Outlet Branch 2 Name",

                          "Connector:Splitter,",
                          "    RadHP Demand Splitter,   !- Name",
                          "    RADHP Demand Inlet Branch,  !- Inlet Branch Name",
                          "    RADHP Demand Radload Branch ,  !- Outlet Branch 1 Name",
                          "    RADHP Demand Bypass Branch ;  !- Outlet Branch 2 Name",

                          "ConnectorList,",
                          "    GHEV Supply Connectors,  !- Name",
                          "    Connector:Splitter,      !- Connector 1 Object Type",
                          "    GHEV Supply Splitter,    !- Connector 1 Name",
                          "    Connector:Mixer,         !- Connector 2 Object Type",
                          "    GHEV Supply Mixer;       !- Connector 2 Name",

                          "ConnectorList,",
                          "    GHEV Demand Connectors,  !- Name",
                          "    Connector:Splitter,      !- Connector 1 Object Type",
                          "    GHEV Demand Splitter,    !- Connector 1 Name",
                          "    Connector:Mixer,         !- Connector 2 Object Type",
                          "    GHEV Demand Mixer;       !- Connector 2 Name",

                          "ConnectorList,",
                          "    RADHP Supply Connectors, !- Name",
                          "    Connector:Splitter,      !- Connector 1 Object Type",
                          "    RadHP Supply Splitter,   !- Connector 1 Name",
                          "    Connector:Mixer,         !- Connector 2 Object Type",
                          "    RADHP Supply Mixer;      !- Connector 2 Name",

                          "ConnectorList,",
                          "    RADHP Demand Connectors, !- Name",
                          "    Connector:Splitter,      !- Connector 1 Object Type",
                          "    RadHP Demand Splitter,   !- Connector 1 Name",
                          "    Connector:Mixer,         !- Connector 2 Object Type",
                          "    RADHP Demand Mixer;      !- Connector 2 Name",

                          "Connector:Mixer,",
                          "    GHEV Supply Mixer,       !- Name",
                          "    GHEV Supply Outlet Branch,  !- Outlet Branch Name",
                          "    GHEV Supply Borehole Branch,  !- Inlet Branch 1 Name",
                          "    GHEV Supply Bypass Branch;  !- Inlet Branch 2 Name",

                          "Connector:Mixer,",
                          "    GHEV Demand Mixer,       !- Name",
                          "    GHEV Demand Outlet Branch,  !- Outlet Branch Name",
                          "    GHEV Demand HPRad Branch,!- Inlet Branch 1 Name",
                          "    GHEV Demand Bypass Branch;  !- Inlet Branch 2 Name",

                          "Connector:Mixer,",
                          "    RADHP Supply Mixer,      !- Name",
                          "    RADHP Supply Outlet Branch,  !- Outlet Branch Name",
                          "    RADHP Supply HPR Branch ,!- Inlet Branch 1 Name",
                          "    RADHP Supply Bypass Branch;  !- Inlet Branch 2 Name",

                          "Connector:Mixer,",
                          "    RADHP Demand Mixer,      !- Name",
                          "    RADHP Demand Outlet Branch,  !- Outlet Branch Name",
                          "    RADHP Demand Radload Branch ,  !- Inlet Branch 1 Name",
                          "    RADHP Demand Bypass Branch ;  !- Inlet Branch 2 Name"

        });

    ASSERT_TRUE(process_idf(idf_objects));
    SimulationManager::PostIPProcessing();
    bool ErrorsFound = false;

    DataGlobals::BeginSimFlag = true;
    SimulationManager::GetProjectData();

    OutputReportPredefined::SetPredefinedTables();
    HeatBalanceManager::SetPreConstructionInputParameters(); // establish array bounds for constructions early
    OutputProcessor::TimeValue.allocate(2);
    OutputProcessor::SetupTimePointers("Zone", DataGlobals::TimeStepZone); // Set up Time pointer for HB/Zone Simulation
    OutputProcessor::SetupTimePointers("HVAC", DataHVACGlobals::TimeStepSys);
    createFacilityElectricPowerServiceObject();
    OutputProcessor::GetReportVariableInput();
    PlantManager::CheckIfAnyPlant();

    BranchInputManager::ManageBranchInput(); // just gets input and

    DataGlobals::DoingSizing = false;
    DataGlobals::KickOffSimulation = true;

    WeatherManager::ResetEnvironmentCounter();
    SimulationManager::SetupSimulation(ErrorsFound);
    DataGlobals::KickOffSimulation = false;

    int EnvCount = 0;
    DataGlobals::WarmupFlag = true;
    bool Available(true);

    while (Available) {

        WeatherManager::GetNextEnvironment(Available, ErrorsFound);

        if (!Available) break;
        if (ErrorsFound) break;

        ++EnvCount;

        DataGlobals::BeginEnvrnFlag = true;
        DataGlobals::EndEnvrnFlag = false;
        DataEnvironment::EndMonthFlag = false;
        DataGlobals::WarmupFlag = true;
        DataGlobals::DayOfSim = 0;
        DataGlobals::DayOfSimChr = "0";

        while ((DataGlobals::DayOfSim < DataGlobals::NumOfDayInEnvrn) || (DataGlobals::WarmupFlag)) { // Begin day loop ...

            ++DataGlobals::DayOfSim;

            if (!DataGlobals::WarmupFlag) {
                ++DataEnvironment::CurrentOverallSimDay;
            }
            DataGlobals::BeginDayFlag = true;
            DataGlobals::EndDayFlag = false;

            for (DataGlobals::HourOfDay = 1; DataGlobals::HourOfDay <= 24; ++DataGlobals::HourOfDay) { // Begin hour loop ...

                DataGlobals::BeginHourFlag = true;
                DataGlobals::EndHourFlag = false;

                for (DataGlobals::TimeStep = 1; DataGlobals::TimeStep <= DataGlobals::NumOfTimeStepInHour; ++DataGlobals::TimeStep) {

                    DataGlobals::BeginTimeStepFlag = true;

                    // Set the End__Flag variables to true if necessary.  Note that
                    // each flag builds on the previous level.  EndDayFlag cannot be
                    // .TRUE. unless EndHourFlag is also .TRUE., etc.  Note that the
                    // EndEnvrnFlag and the EndSimFlag cannot be set during warmup.
                    // Note also that BeginTimeStepFlag, EndTimeStepFlag, and the
                    // SubTimeStepFlags can/will be set/reset in the HVAC Manager.

                    if (DataGlobals::TimeStep == DataGlobals::NumOfTimeStepInHour) {
                        DataGlobals::EndHourFlag = true;
                        if (DataGlobals::HourOfDay == 24) {
                            DataGlobals::EndDayFlag = true;
                            if ((!DataGlobals::WarmupFlag) && (DataGlobals::DayOfSim == DataGlobals::NumOfDayInEnvrn)) {
                                DataGlobals::EndEnvrnFlag = true;
                            }
                        }
                    }

                    WeatherManager::ManageWeather();

                    HeatBalanceManager::ManageHeatBalance();

                    //  After the first iteration of HeatBalance, all the 'input' has been gotten

                    DataGlobals::BeginHourFlag = false;
                    DataGlobals::BeginDayFlag = false;
                    DataGlobals::BeginEnvrnFlag = false;
                    DataGlobals::BeginSimFlag = false;
                    DataGlobals::BeginFullSimFlag = false;

                } // TimeStep loop

                DataGlobals::PreviousHour = DataGlobals::HourOfDay;

            } // ... End hour loop.

        } // ... End day loop.

    } // ... End environment loop.

    EXPECT_NEAR(DataLoopNode::Node(12).MassFlowRate, 0.3, 0.0001);
}

TEST_F(EnergyPlusFixture, WWHP_AutosizeTest1)
{
    // this test is for checking autosizing of heating WWHP. derived from unit test PlantLoopSourceSideTest
    std::string const idf_objects =
        delimited_string({"Version,8.6;",
                          "Schedule:Constant,Radiator massflow temporary,Any value sch,1;",
                          "Schedule:Constant,Radiator supply temperature temporary,Any value sch,40;",
                          "Schedule:Constant,Radiator power demand temporary,Any value sch,3000;",

                          "Pump:VariableSpeed,",
                          "    GHEV pump,               !- Name",
                          "    GHEV pump Inlet Node,    !- Inlet Node Name",
                          "    GHEV Pump Outlet Node,   !- Outlet Node Name",
                          "    0.0006,                  !- Rated Flow Rate {m3/s}",
                          "    40000,                   !- Rated Pump Head {Pa}",
                          "    34.18803,                !- Rated Power Consumption {W}",
                          "    0.9,                     !- Motor Efficiency",
                          "    ,                        !- Fraction of Motor Inefficiencies to Fluid Stream",
                          "    ,                        !- Coefficient 1 of the Part Load Performance Curve",
                          "    1,                       !- Coefficient 2 of the Part Load Performance Curve",
                          "    ,                        !- Coefficient 3 of the Part Load Performance Curve",
                          "    ,                        !- Coefficient 4 of the Part Load Performance Curve",
                          "    0,                       !- Minimum Flow Rate {m3/s}",
                          "    Intermittent;            !- Pump Control Type",

                          "Pump:VariableSpeed,",
                          "    RAD HP pump,             !- Name",
                          "    RadHP pump inlet Node,   !- Inlet Node Name",
                          "    RADHP Pump Outlet Node,  !- Outlet Node Name",
                          "    0.0005,                  !- Rated Flow Rate {m3/s}",
                          "    40000,                   !- Rated Pump Head {Pa}",
                          "    28.49003,                !- Rated Power Consumption {W}",
                          "    0.9,                     !- Motor Efficiency",
                          "    ,                        !- Fraction of Motor Inefficiencies to Fluid Stream",
                          "    ,                        !- Coefficient 1 of the Part Load Performance Curve",
                          "    1,                       !- Coefficient 2 of the Part Load Performance Curve",
                          "    ,                        !- Coefficient 3 of the Part Load Performance Curve",
                          "    ,                        !- Coefficient 4 of the Part Load Performance Curve",
                          "    0,                       !- Minimum Flow Rate {m3/s}",
                          "    Intermittent;            !- Pump Control Type",

                          "SetpointManager:FollowGroundTemperature,",
                          "    GHEV setpmgr,            !- Name",
                          "    Temperature,             !- Control Variable",
                          "    Site:GroundTemperature:Deep,  !- Reference Ground Temperature Object Type",
                          "    0,                       !- Offset Temperature Difference {deltaC}",
                          "    20,                      !- Maximum Setpoint Temperature {C}",
                          "    -10,                     !- Minimum Setpoint Temperature {C}",
                          "    GHEV Supply Exit Pipe Outlet Node;  !- Setpoint Node or NodeList Name",

                          "PlantEquipmentOperation:HeatingLoad,",
                          "    Bore hole PEO,           !- Name",
                          "    0,                       !- Load Range 1 Lower Limit {W}",
                          "    1000000,                 !- Load Range 1 Upper Limit {W}",
                          "    Heat sources;            !- Range 1 Equipment List Name",

                          "ScheduleTypeLimits,",
                          "    bore hole brine temp,    !- Name",
                          "    -10,                     !- Lower Limit Value",
                          "    10,                      !- Upper Limit Value",
                          "    Continuous,              !- Numeric Type",
                          "    Temperature;             !- Unit Type",

                          "ScheduleTypeLimits,",
                          "    rad power limit,         !- Name",
                          "    0,                       !- Lower Limit Value",
                          "    10000,                   !- Upper Limit Value",
                          "    Continuous,              !- Numeric Type",
                          "    Power;                   !- Unit Type",

                          "ScheduleTypeLimits,",
                          "    rad massflow,            !- Name",
                          "    0,                       !- Lower Limit Value",
                          "    1,                       !- Upper Limit Value",
                          "    Continuous;              !- Numeric Type",

                          "ScheduleTypeLimits,",
                          "    on/off,                  !- Name",
                          "    0,                       !- Lower Limit Value",
                          "    1,                       !- Upper Limit Value",
                          "    Discrete,                !- Numeric Type",
                          "    Dimensionless;           !- Unit Type",

                          "ScheduleTypeLimits,",
                          "    rad water t,             !- Name",
                          "    10,                      !- Lower Limit Value",
                          "    80,                      !- Upper Limit Value",
                          "    Continuous,              !- Numeric Type",
                          "    Temperature;             !- Unit Type",

                          "ScheduleTypeLimits,",
                          "    Any value sch,           !- Name",
                          "    ,                        !- Lower Limit Value",
                          "    ,                        !- Upper Limit Value",
                          "    Continuous,              !- Numeric Type",
                          "    Dimensionless;           !- Unit Type",

                          "Schedule:Constant,Brine return temp,bore hole brine temp,5;",

                          "Site:Location,",
                          "    STOCKHOLM_ ARLANDA_SWE Design_Conditions,  !- Name",
                          "    59.65,                   !- Latitude {deg}",
                          "    17.95,                   !- Longitude {deg}",
                          "    1.00,                    !- Time Zone {hr}",
                          "    61.00;                   !- Elevation {m}",

                          "SizingPeriod:DesignDay,",
                          "    STOCKHOLM_ ARLANDA Ann Htg 99.6% Condns DB,  !- Name",
                          "    2,                       !- Month",
                          "    21,                      !- Day of Month",
                          "    WinterDesignDay,         !- Day Type",
                          "    -17.8,                   !- Maximum Dry-Bulb Temperature {C}",
                          "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
                          "    DefaultMultipliers,      !- Dry-Bulb Temperature Range Modifier Type",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
                          "    Wetbulb,                 !- Humidity Condition Type",
                          "    -17.8,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
                          "    ,                        !- Humidity Condition Day Schedule Name",
                          "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
                          "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
                          "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
                          "    100594.,                 !- Barometric Pressure {Pa}",
                          "    2.4,                     !- Wind Speed {m/s}",
                          "    250,                     !- Wind Direction {deg}",
                          "    No,                      !- Rain Indicator",
                          "    No,                      !- Snow Indicator",
                          "    No,                      !- Daylight Saving Time Indicator",
                          "    ASHRAEClearSky,          !- Solar Model Indicator",
                          "    ,                        !- Beam Solar Day Schedule Name",
                          "    ,                        !- Diffuse Solar Day Schedule Name",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
                          "    0.00;                    !- Sky Clearness",

                          "Timestep,1;",

                          "Building,",
                          "    MustHaveObject,          !- Name",
                          "    ,                        !- North Axis {deg}",
                          "    Suburbs,                 !- Terrain",
                          "    0.04,                    !- Loads Convergence Tolerance Value",
                          "    0.4,                     !- Temperature Convergence Tolerance Value {deltaC}",
                          "    FullExterior,            !- Solar Distribution",
                          "    25,                      !- Maximum Number of Warmup Days",
                          "    6;                       !- Minimum Number of Warmup Days",

                          "GlobalGeometryRules,",
                          "   UpperLeftCorner,         !- Starting Vertex Position",
                          "   Counterclockwise,        !- Vertex Entry Direction",
                          "    Relative,                !- Coordinate System",
                          "   Relative,                !- Daylighting Reference Point Coordinate System",
                          "   Relative;                !- Rectangular Surface Coordinate System",

                          "PlantEquipmentOperationSchemes,",
                          "    GHEV PEOS,               !- Name",
                          "    PlantEquipmentOperation:HeatingLoad,  !- Control Scheme 1 Object Type",
                          "    Bore hole PEO,           !- Control Scheme 1 Name",
                          "    always on;               !- Control Scheme 1 Schedule Name",

                          "Pipe:Adiabatic,",
                          "    GHEV supp bypass pipe,   !- Name",
                          "    GHEV Supply Bypass pipe Inlet Node,  !- Inlet Node Name",
                          "    GHEV Supply Bypass pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    GHEV supp exit pipe,     !- Name",
                          "    GHEV Supply Exit Pipe Inlet Node,  !- Inlet Node Name",
                          "    GHEV Supply Exit Pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    GHEV dem entrance pipe,  !- Name",
                          "    GHEV Demand Entrance pipe inlet Node,  !- Inlet Node Name",
                          "    GHEV Demand Entrance pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    GHEV dem bypass pipe,    !- Name",
                          "    GHEV Demand Bypass pipe Inlet node,  !- Inlet Node Name",
                          "    GHEV Demand Bypass pipe Outlet node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    GHEV dem exit pipe,      !- Name",
                          "    GHEV Demand Exit pipe inlet Node,  !- Inlet Node Name",
                          "    GHEV Demand Exit pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP supp bypass pipe,  !- Name",
                          "    RADHP Supply Bypass pipe Inlet Node,  !- Inlet Node Name",
                          "    RADHP Supply Bypass pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP supp exit pipe,    !- Name",
                          "    RadHP Supply Exit Pipe Inlet Node,  !- Inlet Node Name",
                          "    RadHP Supply Exit Pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP dem entrance pipe, !- Name",
                          "    RADHP Demand Entrance pipe Inlet Node,  !- Inlet Node Name",
                          "    RADHP Demand Entrance pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP dem bypass pipe,   !- Name",
                          "    RAD HP Demand Bypass pipe Inlet Node,  !- Inlet Node Name",
                          "    RAD HP Demand Bypass pipe Outlet Node;  !- Outlet Node Name",

                          "Pipe:Adiabatic,",
                          "    RADHP dem exit pipe,     !- Name",
                          "    RADHP Demand Exit Pipe Inlet Node,  !- Inlet Node Name",
                          "    RADHP Demand Exit Pipe Outlet Node;  !- Outlet Node Name",

                          "SimulationControl,",
                          "    No,                      !- Do Zone Sizing Calculation",
                          "    No,                      !- Do System Sizing Calculation",
                          "    YES,                     !- Do Plant Sizing Calculation",
                          "    YES,                      !- Run Simulation for Sizing Periods",
                          "    NO;                     !- Run Simulation for Weather File Run Periods",

                          "SetpointManager:Scheduled,",
                          "    RAD HP setpointmgr,      !- Name",
                          "    Temperature,             !- Control Variable",
                          "    Radiator supply temperature temporary,  !- Schedule Name",
                          "    RadHP Supply Exit Pipe Outlet Node;  !- Setpoint Node or NodeList Name",

                          "PlantEquipmentList,",
                          "    Rad HPs,                 !- Name",
                          "    HeatPump:WaterToWater:EquationFit:Heating,  !- Equipment 1 Object Type",
                          "    Rad Heat Pump;           !- Equipment 1 Name",

                          "PlantEquipmentOperationSchemes,",
                          "    RAD PEOS,                !- Name",
                          "    PlantEquipmentOperation:HeatingLoad,  !- Control Scheme 1 Object Type",
                          "    RAD heating PEO,         !- Control Scheme 1 Name",
                          "    always on;               !- Control Scheme 1 Schedule Name",

                          "PlantEquipmentList,",
                          "    Heat sources,            !- Name",
                          "    GroundHeatExchanger:System,  !- Equipment 1 Object Type",
                          "    Vertical GHE JL2015;     !- Equipment 1 Name",

                          "PlantEquipmentOperation:HeatingLoad,",
                          "    RAD heating PEO,         !- Name",
                          "    0,                       !- Load Range 1 Lower Limit {W}",
                          "    1000000,                 !- Load Range 1 Upper Limit {W}",
                          "    Rad HPs;                 !- Range 1 Equipment List Name",

                          "Schedule:Compact,",
                          "    always on,               !- Name",
                          "    on/off,                  !- Schedule Type Limits Name",
                          "    THROUGH: 12/31,          !- Field 1",
                          "    FOR: AllDays,            !- Field 2",
                          "    UNTIL: 24:00, 1;         !- Field 4",

                          "  GroundHeatExchanger:System,",
                          "    Vertical GHE JL2015,      !- Name",
                          "    GHEV Borehole Inlet Node, !- Inlet Node Name",
                          "    GHEV Borehole Outlet Node, !- Outlet Node Name",
                          "    0.000303,                 !- Design Flow Rate {m3/s}",
                          "    Site:GroundTemperature:Undisturbed:KusudaAchenbach, !- Undisturbed Ground Temperature Model Type",
                          "    Vertical GHE JL2015 Ground Temps, !- Undisturbed Ground Temperature Model Name",
                          "    2.493,                    !- Ground Thermal Conductivity {W/m-K}",
                          "    2495700,                  !- Ground Thermal Heat Capacity {J/m3-K}",
                          "    Vertical GHE JL2015 g-functions; !- Response Factors Object Name",

                          "  GroundHeatExchanger:Vertical:Properties,",
                          "    Vertical GHE JL2015 Props,        !- Name",
                          "    1,                  !- Depth of Top of Borehole {m}",
                          "    160,                !- Borehole Length {m}",
                          "    0.1143,             !- Borehole Diameter {m}",
                          "    0.744,              !- Grout Thermal Conductivity {W/m-K}",
                          "    3.90E+06,           !- Grout Thermal Heat Capacity {J/m3-K}",
                          "    0.389,              !- Pipe Thermal Conductivity {W/m-K}",
                          "    1.77E+06,           !- Pipe Thermal Heat Capacity {J/m3-K}",
                          "    0.0267,             !- Pipe Outer Diameter {m}",
                          "    0.00243,            !- Pipe Thickness {m}",
                          "    0.04556;            !- U-Tube Distance {m}",

                          "  Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
                          "    Vertical GHE JL2015 Ground Temps, !- Undisturbed Ground Temperature Model Name",
                          "    2.493,                    !- Soil Thermal Conductivity {W/m-K}",
                          "    920,                      !- Soil Density {kg/m3}",
                          "    2712.72,                  !- Soil Specific Heat {J/kg-K}",
                          "    8,                        !- Average Soil Surface Temperature {C}",
                          "    3.2,                      !- Average Amplitude of Surface Temperature {deltaC}",
                          "    8;                        !- Phase Shift of Minimum Surface Temperature {days}",

                          "  GroundHeatExchanger:ResponseFactors,",
                          "    Vertical GHE JL2015 g-functions, !- Name",
                          "    Vertical GHE JL2015 Props, !- Name",
                          "    1,                        !- Number of Bore Holes",
                          "    0.0005,                   !- G-Function Reference Ratio {dimensionless}",
                          "    -4.5,                     !- G-Function Ln(T/Ts) Value 1",
                          "    4.7,                      !- G-Function G Value 1",
                          "    -4,                       !- G-Function Ln(T/Ts) Value 2",
                          "    4.85,                     !- G-Function G Value 2",
                          "    -3.5,                     !- G-Function Ln(T/Ts) Value 3",
                          "    5.1,                      !- G-Function G Value 3",
                          "    -3,                       !- G-Function Ln(T/Ts) Value 4",
                          "    5.3,                      !- G-Function G Value 4",
                          "    -2.5,                     !- G-Function Ln(T/Ts) Value 5",
                          "    5.56,                     !- G-Function G Value 5",
                          "    -2,                       !- G-Function Ln(T/Ts) Value 6",
                          "    5.76,                     !- G-Function G Value 6",
                          "    -1.5,                     !- G-Function Ln(T/Ts) Value 7",
                          "    5.97,                     !- G-Function G Value 7",
                          "    -1,                       !- G-Function Ln(T/Ts) Value 8",
                          "    6.19,                     !- G-Function G Value 8",
                          "    -.5,                      !- G-Function Ln(T/Ts) Value 9",
                          "    6.31,                     !- G-Function G Value 9",
                          "    0,                        !- G-Function Ln(T/Ts) Value 10",
                          "    6.42,                     !- G-Function G Value 10",
                          "    0.5,                      !- G-Function Ln(T/Ts) Value 11",
                          "    6.56,                     !- G-Function G Value 11",
                          "    1,                        !- G-Function Ln(T/Ts) Value 12",
                          "    6.61,                     !- G-Function G Value 12",
                          "    1.5,                      !- G-Function Ln(T/Ts) Value 13",
                          "    6.66,                     !- G-Function G Value 13",
                          "    2,                        !- G-Function Ln(T/Ts) Value 14",
                          "    6.7,                      !- G-Function G Value 14",
                          "    2.5,                      !- G-Function Ln(T/Ts) Value 15",
                          "    6.72,                     !- G-Function G Value 15",
                          "    3,                        !- G-Function Ln(T/Ts) Value 16",
                          "    6.73;                     !- G-Function G Value 16",

                          "LoadProfile:Plant,",
                          "    Radiator load,           !- Name",
                          "    Radiator Demand profile Inlet Node,  !- Inlet Node Name",
                          "    radiator demand profile outlet node,  !- Outlet Node Name",
                          "    Radiator power demand temporary,  !- Load Schedule Name",
                          "    0.00025,                 !- Peak Flow Rate {m3/s}",
                          "    Radiator massflow temporary;  !- Flow Rate Fraction Schedule Name",

                          "HeatPump:WaterToWater:EquationFit:Heating,",
                          "    Rad Heat Pump,           !- Name",
                          "    HPR brine Inlet node,    !- Source Side Inlet Node Name",
                          "    HPR brine Outlet node,   !- Source Side Outlet Node Name",
                          "    HPR radiator water return node,  !- Load Side Inlet Node Name",
                          "    HPR radiator supply node,!- Load Side Outlet Node Name",
                          "    autosize,                 !- Rated Load Side Flow Rate {m3/s}",
                          "    autosize,                  !- Rated Source Side Flow Rate {m3/s}",
                          "    autosize,                    !- Rated Heating Capacity {W}",
                          "    autosize,                    !- Rated Heating Power Consumption {W}",
                          "    -3.01043,                !- Heating Capacity Coefficient 1",
                          "    -.51452,                 !- Heating Capacity Coefficient 2",
                          "    4.515927,                !- Heating Capacity Coefficient 3",
                          "    0.017971,                !- Heating Capacity Coefficient 4",
                          "    0.155798,                !- Heating Capacity Coefficient 5",
                          "    -2.65423,                !- Heating Compressor Power Coefficient 1",
                          "    8.570358,                !- Heating Compressor Power Coefficient 2",
                          "    1.21629,                 !- Heating Compressor Power Coefficient 3",
                          "    -.21629,                 !- Heating Compressor Power Coefficient 4",
                          "    0.033862,                !- Heating Compressor Power Coefficient 5",
                          "    3.3475,                  !- Reference Coefficient of Performance",
                          "    1.0;                     !- Sizing Factor",

                          "PlantLoop,",
                          "    GHEV Loop,               !- Name",
                          "    Water,                   !- Fluid Type",
                          "    ,                        !- User Defined Fluid Type",
                          "    GHEV PEOS,               !- Plant Equipment Operation Scheme Name",
                          "    GHEV Supply Exit Pipe Outlet Node,  !- Loop Temperature Setpoint Node Name",
                          "    13,                      !- Maximum Loop Temperature {C}",
                          "    -5,                      !- Minimum Loop Temperature {C}",
                          "    0.0008,                  !- Maximum Loop Flow Rate {m3/s}",
                          "    0,                       !- Minimum Loop Flow Rate {m3/s}",
                          "    0.15,                    !- Plant Loop Volume {m3}",
                          "    GHEV pump Inlet Node,    !- Plant Side Inlet Node Name",
                          "    GHEV Supply Exit Pipe Outlet Node,  !- Plant Side Outlet Node Name",
                          "    GHEV Supply Branches,    !- Plant Side Branch List Name",
                          "    GHEV Supply Connectors,  !- Plant Side Connector List Name",
                          "    GHEV Demand Entrance pipe inlet Node,  !- Demand Side Inlet Node Name",
                          "    GHEV Demand Exit pipe Outlet Node,  !- Demand Side Outlet Node Name",
                          "    GHEV Demand Branches,    !- Demand Side Branch List Name",
                          "    GHEV Demand Connectors,  !- Demand Side Connector List Name",
                          "    SequentialLoad,          !- Load Distribution Scheme",
                          "    ,                        !- Availability Manager List Name",
                          "    SingleSetpoint,          !- Plant Loop Demand Calculation Scheme",
                          "    , ",
                          "    , ",
                          "    2.0;",

                          "PlantLoop,",
                          "    RadHP Loop,              !- Name",
                          "    Water,                   !- Fluid Type",
                          "    ,                        !- User Defined Fluid Type",
                          "    RAD PEOS,                !- Plant Equipment Operation Scheme Name",
                          "    RadHP Supply Exit Pipe Outlet Node,  !- Loop Temperature Setpoint Node Name",
                          "   80,                      !- Maximum Loop Temperature {C}",
                          "    22,                      !- Minimum Loop Temperature {C}",
                          "    0.0005,                  !- Maximum Loop Flow Rate {m3/s}",
                          "    0,                       !- Minimum Loop Flow Rate {m3/s}",
                          "    0.06,                    !- Plant Loop Volume {m3}",
                          "    RadHP pump inlet Node,   !- Plant Side Inlet Node Name",
                          "    RadHP Supply Exit Pipe Outlet Node,  !- Plant Side Outlet Node Name",
                          "    RADHP Supply Branches,   !- Plant Side Branch List Name",
                          "    RADHP Supply Connectors, !- Plant Side Connector List Name",
                          "    RADHP Demand Entrance pipe Inlet Node,  !- Demand Side Inlet Node Name",
                          "    RADHP Demand Exit Pipe Outlet Node,  !- Demand Side Outlet Node Name",
                          "    RADHP Demand Branches,   !- Demand Side Branch List Name",
                          "    RADHP Demand Connectors, !- Demand Side Connector List Name",
                          "    Optimal,                 !- Load Distribution Scheme",
                          "    ,                        !- Availability Manager List Name",
                          "    SingleSetpoint,          !- Plant Loop Demand Calculation Scheme",
                          "    , ",
                          "    , ",
                          "    2.0;",

                          "Sizing:Plant,",
                          "    RadHP Loop,",
                          "    Heating,",
                          "    40.0,",
                          "    7.0;"

                          "BranchList,",
                          "    GHEV Supply Branches,    !- Name",
                          "    GHEV Supply Pump Branch, !- Branch 1 Name",
                          "    GHEV Supply Borehole Branch,  !- Branch 2 Name",
                          "    GHEV Supply Bypass Branch,  !- Branch 3 Name",
                          "    GHEV Supply Outlet Branch;  !- Branch 4 Name",

                          "BranchList,",
                          "    GHEV Demand Branches,    !- Name",
                          "    GHEV Demand Inlet Branch,!- Branch 1 Name",
                          "    GHEV Demand HPRad Branch,!- Branch 2 Name",
                          "    GHEV Demand Bypass Branch,  !- Branch 3 Name",
                          "    GHEV Demand Outlet Branch;  !- Branch 4 Name",

                          "BranchList,",
                          "    RADHP Supply Branches,   !- Name",
                          "    RADHP Supply Inlet Pump, !- Branch 1 Name",
                          "    RADHP Supply HPR Branch ,!- Branch 2 Name",
                          "    RADHP Supply Bypass Branch,  !- Branch 3 Name",
                          "    RADHP Supply Outlet Branch;  !- Branch 4 Name",

                          "BranchList,",
                          "    RADHP Demand Branches,   !- Name",
                          "    RADHP Demand Inlet Branch,  !- Branch 1 Name",
                          "    RADHP Demand Radload Branch ,  !- Branch 2 Name",
                          "    RADHP Demand Bypass Branch ,  !- Branch 3 Name",
                          "    RADHP Demand Outlet Branch;  !- Branch 4 Name",

                          "Connector:Splitter,",
                          "    GHEV Supply Splitter,    !- Name",
                          "    GHEV Supply Pump Branch, !- Inlet Branch Name",
                          "    GHEV Supply Borehole Branch,  !- Outlet Branch 1 Name",
                          "    GHEV Supply Bypass Branch;  !- Outlet Branch 2 Name",

                          "Branch,",
                          "    GHEV Supply Pump Branch, !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pump:VariableSpeed,      !- Component 1 Object Type",
                          "    GHEV pump,               !- Component 1 Name",
                          "    GHEV pump Inlet Node,    !- Component 1 Inlet Node Name",
                          "    GHEV Pump Outlet Node;   !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Supply Borehole Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    GroundHeatExchanger:System,  !- Component 1 Object Type",
                          "    Vertical GHE JL2015,     !- Component 1 Name",
                          "    GHEV Borehole Inlet Node,!- Component 1 Inlet Node Name",
                          "    GHEV Borehole Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Supply Bypass Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV supp bypass pipe,   !- Component 1 Name",
                          "    GHEV Supply Bypass pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    GHEV Supply Bypass pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Supply Outlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV supp exit pipe,     !- Component 1 Name",
                          "    GHEV Supply Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    GHEV Supply Exit Pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Demand Inlet Branch,!- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV dem entrance pipe,  !- Component 1 Name",
                          "    GHEV Demand Entrance pipe inlet Node,  !- Component 1 Inlet Node Name",
                          "    GHEV Demand Entrance pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Demand HPRad Branch,!- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    HeatPump:WaterToWater:EquationFit:Heating,  !- Component 1 Object Type",
                          "    Rad Heat Pump,           !- Component 1 Name",
                          "    HPR brine Inlet node,    !- Component 1 Inlet Node Name",
                          "    HPR brine Outlet node;   !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Demand Bypass Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV dem bypass pipe,    !- Component 1 Name",
                          "    GHEV Demand Bypass pipe Inlet node,  !- Component 1 Inlet Node Name",
                          "    GHEV Demand Bypass pipe Outlet node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    GHEV Demand Outlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    GHEV dem exit pipe,      !- Component 1 Name",
                          "    GHEV Demand Exit pipe inlet Node,  !- Component 1 Inlet Node Name",
                          "    GHEV Demand Exit pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Supply Inlet Pump, !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pump:VariableSpeed,      !- Component 1 Object Type",
                          "    RAD HP pump,             !- Component 1 Name",
                          "    RadHP pump inlet Node,   !- Component 1 Inlet Node Name",
                          "    RADHP Pump Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Supply HPR Branch ,!- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    HeatPump:WaterToWater:EquationFit:Heating,  !- Component 1 Object Type",
                          "    Rad Heat Pump,           !- Component 1 Name",
                          "    HPR radiator water return node,  !- Component 1 Inlet Node Name",
                          "    HPR radiator supply node;!- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Supply Bypass Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP supp bypass pipe,  !- Component 1 Name",
                          "    RADHP Supply Bypass pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RADHP Supply Bypass pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Supply Outlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP supp exit pipe,    !- Component 1 Name",
                          "    RadHP Supply Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RadHP Supply Exit Pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Demand Inlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP dem entrance pipe, !- Component 1 Name",
                          "    RADHP Demand Entrance pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RADHP Demand Entrance pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Demand Radload Branch ,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    LoadProfile:Plant,       !- Component 1 Object Type",
                          "    Radiator load,           !- Component 1 Name",
                          "    Radiator Demand profile Inlet Node,  !- Component 1 Inlet Node Name",
                          "    radiator demand profile outlet node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Demand Bypass Branch ,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP dem bypass pipe,   !- Component 1 Name",
                          "    RAD HP Demand Bypass pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RAD HP Demand Bypass pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Branch,",
                          "    RADHP Demand Outlet Branch,  !- Name",
                          "    ,                        !- Pressure Drop Curve Name",
                          "    Pipe:Adiabatic,          !- Component 1 Object Type",
                          "    RADHP dem exit pipe,     !- Component 1 Name",
                          "    RADHP Demand Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
                          "    RADHP Demand Exit Pipe Outlet Node;  !- Component 1 Outlet Node Name",

                          "Connector:Splitter,",
                          "    GHEV Demand Splitter,    !- Name",
                          "    GHEV Demand Inlet Branch,!- Inlet Branch Name",
                          "    GHEV Demand HPRad Branch,!- Outlet Branch 1 Name",
                          "    GHEV Demand Bypass Branch;  !- Outlet Branch 2 Name",

                          "Connector:Splitter,",
                          "    RadHP Supply Splitter,   !- Name",
                          "    RADHP Supply Inlet Pump, !- Inlet Branch Name",
                          "    RADHP Supply HPR Branch ,!- Outlet Branch 1 Name",
                          "    RADHP Supply Bypass Branch;  !- Outlet Branch 2 Name",

                          "Connector:Splitter,",
                          "    RadHP Demand Splitter,   !- Name",
                          "    RADHP Demand Inlet Branch,  !- Inlet Branch Name",
                          "    RADHP Demand Radload Branch ,  !- Outlet Branch 1 Name",
                          "    RADHP Demand Bypass Branch ;  !- Outlet Branch 2 Name",

                          "ConnectorList,",
                          "    GHEV Supply Connectors,  !- Name",
                          "    Connector:Splitter,      !- Connector 1 Object Type",
                          "    GHEV Supply Splitter,    !- Connector 1 Name",
                          "    Connector:Mixer,         !- Connector 2 Object Type",
                          "    GHEV Supply Mixer;       !- Connector 2 Name",

                          "ConnectorList,",
                          "    GHEV Demand Connectors,  !- Name",
                          "    Connector:Splitter,      !- Connector 1 Object Type",
                          "    GHEV Demand Splitter,    !- Connector 1 Name",
                          "    Connector:Mixer,         !- Connector 2 Object Type",
                          "    GHEV Demand Mixer;       !- Connector 2 Name",

                          "ConnectorList,",
                          "    RADHP Supply Connectors, !- Name",
                          "    Connector:Splitter,      !- Connector 1 Object Type",
                          "    RadHP Supply Splitter,   !- Connector 1 Name",
                          "    Connector:Mixer,         !- Connector 2 Object Type",
                          "    RADHP Supply Mixer;      !- Connector 2 Name",

                          "ConnectorList,",
                          "    RADHP Demand Connectors, !- Name",
                          "    Connector:Splitter,      !- Connector 1 Object Type",
                          "    RadHP Demand Splitter,   !- Connector 1 Name",
                          "    Connector:Mixer,         !- Connector 2 Object Type",
                          "    RADHP Demand Mixer;      !- Connector 2 Name",

                          "Connector:Mixer,",
                          "    GHEV Supply Mixer,       !- Name",
                          "    GHEV Supply Outlet Branch,  !- Outlet Branch Name",
                          "    GHEV Supply Borehole Branch,  !- Inlet Branch 1 Name",
                          "    GHEV Supply Bypass Branch;  !- Inlet Branch 2 Name",

                          "Connector:Mixer,",
                          "    GHEV Demand Mixer,       !- Name",
                          "    GHEV Demand Outlet Branch,  !- Outlet Branch Name",
                          "    GHEV Demand HPRad Branch,!- Inlet Branch 1 Name",
                          "    GHEV Demand Bypass Branch;  !- Inlet Branch 2 Name",

                          "Connector:Mixer,",
                          "    RADHP Supply Mixer,      !- Name",
                          "    RADHP Supply Outlet Branch,  !- Outlet Branch Name",
                          "    RADHP Supply HPR Branch ,!- Inlet Branch 1 Name",
                          "    RADHP Supply Bypass Branch;  !- Inlet Branch 2 Name",

                          "Connector:Mixer,",
                          "    RADHP Demand Mixer,      !- Name",
                          "    RADHP Demand Outlet Branch,  !- Outlet Branch Name",
                          "    RADHP Demand Radload Branch ,  !- Inlet Branch 1 Name",
                          "    RADHP Demand Bypass Branch ;  !- Inlet Branch 2 Name"

        });

    ASSERT_TRUE(process_idf(idf_objects));
    SimulationManager::PostIPProcessing();
    bool ErrorsFound = false;

    DataGlobals::BeginSimFlag = true;
    SimulationManager::GetProjectData();

    OutputReportPredefined::SetPredefinedTables();
    HeatBalanceManager::SetPreConstructionInputParameters(); // establish array bounds for constructions early
    OutputProcessor::TimeValue.allocate(2);
    OutputProcessor::SetupTimePointers("Zone", DataGlobals::TimeStepZone); // Set up Time pointer for HB/Zone Simulation
    OutputProcessor::SetupTimePointers("HVAC", DataHVACGlobals::TimeStepSys);
    createFacilityElectricPowerServiceObject();
    OutputProcessor::GetReportVariableInput();
    PlantManager::CheckIfAnyPlant();

    BranchInputManager::ManageBranchInput(); // just gets input and
    SizingManager::ManageSizing();
    DataGlobals::DoingSizing = false;
    DataGlobals::KickOffSimulation = true;

    WeatherManager::ResetEnvironmentCounter();
    SimulationManager::SetupSimulation(ErrorsFound);
    DataGlobals::KickOffSimulation = false;

    // should be sized now

    EXPECT_TRUE(HeatPumpWaterToWaterSimple::GSHP(1).ratedLoadVolFlowHeatWasAutoSized);
    EXPECT_TRUE(HeatPumpWaterToWaterSimple::GSHP(1).ratedSourceVolFlowHeatWasAutoSized);
    EXPECT_TRUE(HeatPumpWaterToWaterSimple::GSHP(1).ratedCapHeatWasAutoSized);
    EXPECT_TRUE(HeatPumpWaterToWaterSimple::GSHP(1).ratedPowerHeatWasAutoSized);

    EXPECT_NEAR(HeatPumpWaterToWaterSimple::GSHP(1).RatedLoadVolFlowHeat, 0.00025, 0.0000001);
    EXPECT_NEAR(HeatPumpWaterToWaterSimple::GSHP(1).RatedSourceVolFlowHeat, 0.00025, 0.0000001);
    EXPECT_NEAR(HeatPumpWaterToWaterSimple::GSHP(1).RatedCapHeat, 7200.71, 0.1);
    EXPECT_NEAR(HeatPumpWaterToWaterSimple::GSHP(1).RatedPowerHeat, 2151.07, 0.1);
}
} // namespace EnergyPlus
