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

// EnergyPlus::CondenserLoopTowers unit tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include <CondenserLoopTowers.hh>
#include <OutputProcessor.hh>
#include <SimulationManager.hh>
#include <ElectricPowerServiceManager.hh>
#include <BranchInputManager.hh>
#include <PlantManager.hh>
#include <WeatherManager.hh>
#include <DataHVACGlobals.hh>
#include <OutputReportPredefined.hh>

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, CondenserLoopTowers_MerkelNoCooling ) {
		std::string const idf_objects = delimited_string({
		"  Site:Location,",
		"    USA IL-CHICAGO-OHARE,    !- Name",
		"    41.77,                   !- Latitude {deg}",
		"    -87.75,                  !- Longitude {deg}",
		"    -6.00,                   !- Time Zone {hr}",
		"    190;                     !- Elevation {m}",

		"  SizingPeriod:DesignDay,",
		"    CHICAGO Ann Htg 99.6% Condns DB,  !- Name",
		"    1,                       !- Month",
		"    21,                      !- Day of Month",
		"    WinterDesignDay,         !- Day Type",
		"    -20.6,                   !- Maximum Dry-Bulb Temperature {C}",
		"    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Type",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
		"    Wetbulb,                 !- Humidity Condition Type",
		"    -20.6,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
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
		"    0.00;                    !- Sky Clearness",

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

		"    SimulationControl,",
		"    no,                     !- Do Zone Sizing Calculation",
		"    no,                     !- Do System Sizing Calculation",
		"    no,                     !- Do Plant Sizing Calculation",
		"    Yes,                     !- Run Simulation for Sizing Periods",
		"    no;                     !- Run Simulation for Weather File Run Periods",
		
		"  Timestep,6;",

		"  ScheduleTypeLimits,",
		"    Any Number;              !- Name",

		"  Schedule:Compact,",
		"    ALWAYS_ON,               !- Name",
		"    On/Off,                  !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,1;          !- Field 3",

		"  ScheduleTypeLimits,",
		"    On/Off,                  !- Name",
		"    0,                       !- Lower Limit Value",
		"    1,                       !- Upper Limit Value",
		"    DISCRETE;                !- Numeric Type",

		"  CoolingTower:VariableSpeed:Merkel,",
		"    TowerWaterSys CoolTower1,!- Name",
		"    TowerWaterSys Pump-TowerWaterSys CoolTowerNode,  !- Water Inlet Node Name",
		"    TowerWaterSys Supply Equipment Outlet Node,  !- Water Outlet Node Name",
		"    NominalCapacity,         !- Performance Input Method",
		"    1.25,                    !- Heat Rejection Capacity and Nominal Capacity Sizing Ratio",
		"    10000,                !- Nominal Capacity {W}",
		"    autocalculate,           !- Free Convection Nominal Capacity {W}",
		"    0.1,                     !- Free Convection Nominal Capacity Sizing Factor",
		"    autocalculate,           !- Design Water Flow Rate {m3/s}",
		"    5.382E-8,                !- Design Water Flow Rate per Unit of Nominal Capacity {m3/s-W}",
		"    autocalculate,           !- Design Air Flow Rate {m3/s}",
		"    2.76316E-5,              !- Design Air Flow Rate Per Unit of Nominal Capacity {m3/s-W}",
		"    0.2,                     !- Minimum Air Flow Rate Ratio",
		"    autocalculate,           !- Design Fan Power {W}",
		"    0.0105,                  !- Design Fan Power Per Unit of Nominal Capacity {dimensionless}",
		"    VS tower fan power mod func air flow ratio,  !- Fan Power Modifier Function of Air Flow Rate Ratio Curve Name",
		"    autocalculate,           !- Free Convection Regime Air Flow Rate {m3/s}",
		"    0.1,                     !- Free Convection Regime Air Flow Rate Sizing Factor",
		"    ,                        !- Design Air Flow Rate U-Factor Times Area Value {W/K}",
		"    ,                        !- Free Convection Regime U-Factor Times Area Value {W/K}",
		"    ,                        !- Free Convection U-Factor Times Area Value Sizing Factor",
		"    VS tower UA mod func air flow ratio,  !- U-Factor Times Area Modifier Function of Air Flow Ratio Curve Name",
		"    VS tower UA mod func wetbulb difference,  !- U-Factor Times Area Modifier Function of Wetbulb Temperature Difference Curve Name",
		"    VS tower UA mod func water flow ratio,  !- U-Factor Times Area Modifier Function of Water Flow Ratio Curve Name",
		"    ,                        !- Basin Heater Capacity {W/K}",
		"    ,                        !- Basin Heater Setpoint Temperature {C}",
		"    ,                        !- Basin Heater Operating Schedule Name",
		"    ,                        !- Evaporation Loss Mode",
		"    ,                        !- Evaporation Loss Factor {percent/K}",
		"    ,                        !- Drift Loss Percent {percent}",
		"    ,                        !- Blowdown Calculation Mode",
		"    ,                        !- Blowdown Concentration Ratio",
		"    ,                        !- Blowdown Makeup Water Usage Schedule Name",
		"    ,                        !- Supply Water Storage Tank Name",
		"    ,                        !- Outdoor Air Inlet Node Name",
		"    ,                        !- Number of Cells",
		"    ,                        !- Cell Control",
		"    ,                        !- Cell Minimum  Water Flow Rate Fraction",
		"    ,                        !- Cell Maximum Water Flow Rate Fraction",
		"    0.5;                     !- Sizing Factor",

		"  Curve:Cubic,",
		"    VS tower fan power mod func air flow ratio,  !- Name",
		"    0.02,                    !- Coefficient1 Constant",
		"    0.0,                     !- Coefficient2 x",
		"    0.0,                     !- Coefficient3 x**2",
		"    0.98,                    !- Coefficient4 x**3",
		"    0.2,                     !- Minimum Value of x",
		"    1.0,                     !- Maximum Value of x",
		"    0.0,                     !- Minimum Curve Output",
		"    1.0,                     !- Maximum Curve Output",
		"    Dimensionless,           !- Input Unit Type for X",
		"    Dimensionless;           !- Output Unit Type",

		"  Curve:Quadratic,",
		"    VS tower UA mod func air flow ratio,  !- Name",
		"    0.0,                     !- Coefficient1 Constant",
		"    1.3,                     !- Coefficient2 x",
		"    -0.3,                    !- Coefficient3 x**2",
		"    0.2,                     !- Minimum Value of x",
		"    1.0,                     !- Maximum Value of x",
		"    0.0,                     !- Minimum Curve Output",
		"    1.0,                     !- Maximum Curve Output",
		"    Dimensionless,           !- Input Unit Type for X",
		"    Dimensionless;           !- Output Unit Type",

		"  Curve:Linear,",
		"    VS tower UA mod func wetbulb difference,  !- Name",
		"    1.0,                     !- Coefficient1 Constant",
		"    0.0081,                  !- Coefficient2 x",
		"    -10,                     !- Minimum Value of x",
		"    25.0,                    !- Maximum Value of x",
		"    0.85,                    !- Minimum Curve Output",
		"    1.3,                     !- Maximum Curve Output",
		"    Dimensionless,           !- Input Unit Type for X",
		"    Dimensionless;           !- Output Unit Type",

		"  Curve:Quadratic,",
		"    VS tower UA mod func water flow ratio,  !- Name",
		"    0.1082,                  !- Coefficient1 Constant",
		"    1.667,                   !- Coefficient2 x",
		"    -0.7713,                 !- Coefficient3 x**2",
		"    0.3,                     !- Minimum Value of x",
		"    1.0,                     !- Maximum Value of x",
		"    0.5,                     !- Minimum Curve Output",
		"    1.1,                     !- Maximum Curve Output",
		"    Dimensionless,           !- Input Unit Type for X",
		"    Dimensionless;           !- Output Unit Type",

		"  Pump:ConstantSpeed,",
		"    TowerWaterSys Pump,      !- Name",
		"    TowerWaterSys Supply Inlet Node,  !- Inlet Node Name",
		"    TowerWaterSys Pump-TowerWaterSys CoolTowerNodeviaConnector,  !- Outlet Node Name",
		"    0.002,                !- Rated Flow Rate {m3/s}",
		"    0.1,                  !- Rated Pump Head {Pa}",
		"    200.0,                !- Rated Power Consumption {W}",
		"    0.87,                    !- Motor Efficiency",
		"    0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"    Intermittent;            !- Pump Control Type",

		"  CondenserEquipmentList,",
		"    TowerWaterSys Equipment List,  !- Name",
		"    CoolingTower:VariableSpeed:Merkel,  !- Equipment 1 Object Type",
		"    TowerWaterSys CoolTower1;!- Equipment 1 Name",

		"  CondenserLoop,",
		"    TowerWaterSys,           !- Name",
		"    Water,                   !- Fluid Type",
		"    ,                        !- User Defined Fluid Type",
		"    TowerWaterSys Loop Operation Scheme List,  !- Condenser Equipment Operation Scheme Name",
		"    TowerWaterSys Supply Outlet Node,  !- Condenser Loop Temperature Setpoint Node Name",
		"    80.0,                    !- Maximum Loop Temperature {C}",
		"    5.0,                     !- Minimum Loop Temperature {C}",
		"    0.002,                !- Maximum Loop Flow Rate {m3/s}",
		"    0.0,                     !- Minimum Loop Flow Rate {m3/s}",
		"    0.1,                !- Condenser Loop Volume {m3}",
		"    TowerWaterSys Supply Inlet Node,  !- Condenser Side Inlet Node Name",
		"    TowerWaterSys Supply Outlet Node,  !- Condenser Side Outlet Node Name",
		"    TowerWaterSys Supply Branches,  !- Condenser Side Branch List Name",
		"    TowerWaterSys Supply Connectors,  !- Condenser Side Connector List Name",
		"    TowerWaterSys Demand Inlet Node,  !- Demand Side Inlet Node Name",
		"    TowerWaterSys Demand Outlet Node,  !- Demand Side Outlet Node Name",
		"    TowerWaterSys Demand Branches,  !- Condenser Demand Side Branch List Name",
		"    TowerWaterSys Demand Connectors,  !- Condenser Demand Side Connector List Name",
		"    SequentialLoad;          !- Load Distribution Scheme",

		"  CondenserEquipmentOperationSchemes,",
		"    TowerWaterSys Loop Operation Scheme List,  !- Name",
		"    PlantEquipmentOperation:CoolingLoad,  !- Control Scheme 1 Object Type",
		"    TowerWaterSys Operation Scheme,  !- Control Scheme 1 Name",
		"    ALWAYS_ON;               !- Control Scheme 1 Schedule Name",

		"  PlantEquipmentOperation:CoolingLoad,",
		"    TowerWaterSys Operation Scheme,  !- Name",
		"    0.0,                     !- Load Range 1 Lower Limit {W}",
		"    1000000000000,           !- Load Range 1 Upper Limit {W}",
		"    TowerWaterSys Equipment List;  !- Range 1 Equipment List Name",

		"  SetpointManager:Scheduled,",
		"    TowerWaterSys Setpoint Manager,  !- Name",
		"    Temperature,             !- Control Variable",
		"    TowerWaterSys Temp Sch,      !- Schedule Name",
		"    TowerWaterSys Supply Outlet Node;  !- Setpoint Node or NodeList Name",

		"  Schedule:Compact,",
		"    TowerWaterSys Temp Sch,      !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    THROUGH: 12/31,          !- Field 1",
		"    FOR: AllDays,            !- Field 2",
		"    UNTIL: 24:00,30.0;       !- Field 3",

		"  BranchList,",
		"    TowerWaterSys Demand Branches,  !- Name",
		"    TowerWaterSys Demand Inlet Branch,  !- Branch 1 Name",
		"    TowerWaterSys Demand Load Branch 1,  !- Branch 2 Name",
		"    TowerWaterSys Demand Bypass Branch,  !- Branch 4 Name",
		"    TowerWaterSys Demand Outlet Branch;  !- Branch 5 Name",

		"  BranchList,",
		"    TowerWaterSys Supply Branches,  !- Name",
		"    TowerWaterSys Supply Inlet Branch,  !- Branch 1 Name",
		"    TowerWaterSys Supply Equipment Branch 1,  !- Branch 2 Name",
		"    TowerWaterSys Supply Equipment Bypass Branch,  !- Branch 4 Name",
		"    TowerWaterSys Supply Outlet Branch;  !- Branch 5 Name",

		"  Branch,",
		"    TowerWaterSys Demand Bypass Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    TowerWaterSys Demand Bypass Pipe,  !- Component 1 Name",
		"    TowerWaterSys Demand Bypass Pipe Inlet Node,  !- Component 1 Inlet Node Name",
		"    TowerWaterSys Demand Bypass Pipe Outlet Node,  !- Component 1 Outlet Node Name",
		"    Bypass;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    TowerWaterSys Demand Inlet Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    TowerWaterSys Demand Inlet Pipe,  !- Component 1 Name",
		"    TowerWaterSys Demand Inlet Node,  !- Component 1 Inlet Node Name",
		"    TowerWaterSys Demand Inlet Pipe-TowerWaterSys Demand Mixer,  !- Component 1 Outlet Node Name",
		"    Passive;                 !- Component 1 Branch Control Type",

		"  Branch,",
		"    TowerWaterSys Demand Load Branch 1,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    LoadProfile:Plant,  !- Component 1 Object Type",
		"    Load Profile 1,      !- Component 1 Name",
		"    Demand Load Profile 1 Inlet Node,  !- Component 1 Inlet Node Name",
		"    Demand Load Profile 1 Outlet Node,  !- Component 1 Outlet Node Name",
		"    Active;                  !- Component 1 Branch Control Type",

		"  LoadProfile:Plant,",
		"    Load Profile 1,          !- Name",
		"    Demand Load Profile 1 Inlet Node,  !- Inlet Node Name",
		"    Demand Load Profile 1 Outlet Node,  !- Outlet Node Name",
		"    Load Profile 1 Load Schedule,  !- Load Schedule Name",
		"    0.002,                   !- Peak Flow Rate {m3/s}",
		"    Load Profile 1 Flow Frac Schedule;  !- Flow Rate Fraction Schedule Name",

		"  Schedule:Compact,",
		"    Load Profile 1 Load Schedule,  !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    THROUGH: 12/31,          !- Field 1",
		"    FOR: AllDays,            !- Field 2",
		"    UNTIL: 24:00,0.0;        !- Field 3",

		"  Schedule:Compact,",
		"    Load Profile 1 Flow Frac Schedule,  !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    THROUGH: 12/31,          !- Field 1",
		"    FOR: AllDays,            !- Field 2",
		"    UNTIL: 24:00,1.0;        !- Field 3",

		"  Branch,",
		"    TowerWaterSys Demand Outlet Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    TowerWaterSys Demand Outlet Pipe,  !- Component 1 Name",
		"    TowerWaterSys Demand Mixer-TowerWaterSys Demand Outlet Pipe,  !- Component 1 Inlet Node Name",
		"    TowerWaterSys Demand Outlet Node,  !- Component 1 Outlet Node Name",
		"    Passive;                 !- Component 1 Branch Control Type",

		"  Branch,",
		"    TowerWaterSys Supply Equipment Branch 1,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    CoolingTower:VariableSpeed:Merkel,  !- Component 1 Object Type",
		"    TowerWaterSys CoolTower1,!- Component 1 Name",
		"    TowerWaterSys Pump-TowerWaterSys CoolTowerNode,  !- Component 1 Inlet Node Name",
		"    TowerWaterSys Supply Equipment Outlet Node,  !- Component 1 Outlet Node Name",
		"    Active;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    TowerWaterSys Supply Equipment Bypass Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    TowerWaterSys Supply Equipment Bypass Pipe,  !- Component 1 Name",
		"    TowerWaterSys Supply Equip Bypass Inlet Node,  !- Component 1 Inlet Node Name",
		"    TowerWaterSys Supply Equip Bypass Outlet Node,  !- Component 1 Outlet Node Name",
		"    Bypass;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    TowerWaterSys Supply Inlet Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pump:ConstantSpeed,      !- Component 1 Object Type",
		"    TowerWaterSys Pump,      !- Component 1 Name",
		"    TowerWaterSys Supply Inlet Node,  !- Component 1 Inlet Node Name",
		"    TowerWaterSys Pump-TowerWaterSys CoolTowerNodeviaConnector,  !- Component 1 Outlet Node Name",
    		"Active;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    TowerWaterSys Supply Outlet Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    TowerWaterSys Supply Outlet Pipe,  !- Component 1 Name",
		"    TowerWaterSys Supply Mixer-TowerWaterSys Supply Outlet Pipe,  !- Component 1 Inlet Node Name",
		"    TowerWaterSys Supply Outlet Node,  !- Component 1 Outlet Node Name",
		"    Passive;                 !- Component 1 Branch Control Type",

		"  OutdoorAir:Node,",
		"    TowerWaterSys CoolTower OA ref Node;  !- Name",

		"  ConnectorList,",
		"    TowerWaterSys Demand Connectors,  !- Name",
		"    Connector:Splitter,      !- Connector 1 Object Type",
		"    TowerWaterSys Demand Splitter,  !- Connector 1 Name",
		"    Connector:Mixer,         !- Connector 2 Object Type",
		"    TowerWaterSys Demand Mixer;  !- Connector 2 Name",

		"  ConnectorList,",
		"    TowerWaterSys Supply Connectors,  !- Name",
		"    Connector:Splitter,      !- Connector 1 Object Type",
		"    TowerWaterSys Supply Splitter,  !- Connector 1 Name",
		"    Connector:Mixer,         !- Connector 2 Object Type",
		"    TowerWaterSys Supply Mixer;  !- Connector 2 Name",

		"  Connector:Splitter,",
		"    TowerWaterSys Demand Splitter,  !- Name",
		"    TowerWaterSys Demand Inlet Branch,  !- Inlet Branch Name",
		"    TowerWaterSys Demand Load Branch 1,  !- Outlet Branch 1 Name",
		"    TowerWaterSys Demand Bypass Branch;  !- Outlet Branch 3 Name",

		"  Connector:Splitter,",
		"    TowerWaterSys Supply Splitter,  !- Name",
		"    TowerWaterSys Supply Inlet Branch,  !- Inlet Branch Name",
		"    TowerWaterSys Supply Equipment Branch 1,  !- Outlet Branch 1 Name",
		"    TowerWaterSys Supply Equipment Bypass Branch;  !- Outlet Branch 3 Name",

		"  Connector:Mixer,",
		"    TowerWaterSys Demand Mixer,  !- Name",
		"    TowerWaterSys Demand Outlet Branch,  !- Outlet Branch Name",
		"    TowerWaterSys Demand Load Branch 1,  !- Inlet Branch 1 Name",
		"    TowerWaterSys Demand Bypass Branch;  !- Inlet Branch 3 Name",

		"  Connector:Mixer,",
		"    TowerWaterSys Supply Mixer,  !- Name",
		"    TowerWaterSys Supply Outlet Branch,  !- Outlet Branch Name",
		"    TowerWaterSys Supply Equipment Branch 1,  !- Inlet Branch 1 Name",
		"    TowerWaterSys Supply Equipment Bypass Branch;  !- Inlet Branch 3 Name",

		"  Pipe:Adiabatic,",
		"    TowerWaterSys Demand Bypass Pipe,  !- Name",
		"    TowerWaterSys Demand Bypass Pipe Inlet Node,  !- Inlet Node Name",
		"    TowerWaterSys Demand Bypass Pipe Outlet Node;  !- Outlet Node Name",

		"  Pipe:Adiabatic,",
		"    TowerWaterSys Demand Inlet Pipe,  !- Name",
		"    TowerWaterSys Demand Inlet Node,  !- Inlet Node Name",
		"    TowerWaterSys Demand Inlet Pipe-TowerWaterSys Demand Mixer;  !- Outlet Node Name",

		"  Pipe:Adiabatic,",
		"    TowerWaterSys Demand Outlet Pipe,  !- Name",
		"    TowerWaterSys Demand Mixer-TowerWaterSys Demand Outlet Pipe,  !- Inlet Node Name",
		"    TowerWaterSys Demand Outlet Node;  !- Outlet Node Name",

		"  Pipe:Adiabatic,",
		"    TowerWaterSys Supply Equipment Bypass Pipe,  !- Name",
		"    TowerWaterSys Supply Equip Bypass Inlet Node,  !- Inlet Node Name",
		"    TowerWaterSys Supply Equip Bypass Outlet Node;  !- Outlet Node Name",

		"  Pipe:Adiabatic,",
		"    TowerWaterSys Supply Outlet Pipe,  !- Name",
		"    TowerWaterSys Supply Mixer-TowerWaterSys Supply Outlet Pipe,  !- Inlet Node Name",
		"    TowerWaterSys Supply Outlet Node;  !- Outlet Node Name"


		});
		ASSERT_FALSE( process_idf( idf_objects ) );
		SimulationManager::PostIPProcessing();

		bool ErrorsFound =  false;

		DataGlobals::BeginSimFlag = true;
		SimulationManager::GetProjectData();
		OutputReportPredefined::SetPredefinedTables();

		OutputProcessor::TimeValue.allocate( 2 );
		OutputProcessor::SetupTimePointers( "Zone", DataGlobals::TimeStepZone ); // Set up Time pointer for HB/Zone Simulation
		OutputProcessor::SetupTimePointers( "HVAC", DataHVACGlobals::TimeStepSys );
		createFacilityElectricPowerServiceObject();
		OutputProcessor::GetReportVariableInput();
		PlantManager::CheckIfAnyPlant();
		BranchInputManager::ManageBranchInput(); // just gets input and returns.

		DataGlobals::DoingSizing = false;
		DataGlobals::KickOffSimulation = true;

		WeatherManager::ResetEnvironmentCounter();
		SimulationManager::SetupSimulation( ErrorsFound );
		CondenserLoopTowers::GetTowerInput();

		CondenserLoopTowers::InitTower( 1, false );
		CondenserLoopTowers::SizeVSMerkelTower( 1 );
		CondenserLoopTowers::InitTower( 1, true );
		Real64 MyLoad = 0.0;
		CondenserLoopTowers::CalcMerkelVariableSpeedTower( 1, MyLoad );
		CondenserLoopTowers::UpdateTowers( 1 );
		CondenserLoopTowers::ReportTowers( true, 1 );

		// test that tower is really not cooling with no load so temp in and out is the same issue #4927
		EXPECT_DOUBLE_EQ( DataLoopNode::Node(9).Temp, DataLoopNode::Node(10).Temp);

	}

}
