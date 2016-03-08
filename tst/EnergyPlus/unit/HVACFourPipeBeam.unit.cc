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

// EnergyPlus::HVACFourPipeBeam Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include <AirTerminalUnit.hh>
#include <HVACFourPipeBeam.hh>
#include <DataHeatBalance.hh>
#include <DataZoneEquipment.hh>
#include <DataLoopNode.hh>
#include <NodeInputManager.hh>
#include <DataDefineEquip.hh>
#include <SimulationManager.hh>
#include <ElectricPowerServiceManager.hh>
#include <OutputReportPredefined.hh>
#include <HeatBalanceManager.hh>
#include <OutputProcessor.hh>
#include <DataHVACGlobals.hh>
#include <PlantManager.hh>
#include <BranchInputManager.hh>
#include <SizingManager.hh>
#include <WeatherManager.hh>
#include <PlantUtilities.hh>
#include <DataZoneEnergyDemands.hh>

namespace EnergyPlus {


	TEST_F( EnergyPlusFixture, Beam_FactoryAllAutosize ) {
		std::string const idf_objects = delimited_string( {
		"Version,8.4;",
		"AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam,",
		"    Perimeter_top_ZN_4 4pipe Beam, !- Name",
		"    , !- Primary Air Availability Schedule Name",
		"    , !- Cooling Availability Schedule Name",
		"    , !- Heating Availability Schedule Name",
		"    Perimeter_top_ZN_4 4pipe Beam Inlet Node Name , !- Primary Air Inlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam Outlet Node Name , !- Primary Air Outlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam CW Inlet Node , !- Chilled Water Inlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam CW Outlet Node , !- Chilled Water Outlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam HW Inlet Node , !- Hot Water Inlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam HW Outlet Node, !- Hot Water Outlet Node Name",
		"    AUTOSIZE , !- Design Primary Air Volume Flow Rate",
		"    AUTOSIZE , !- Design Chilled Water Volume Flow Rate",
		"    AUTOSIZE , !- Design Hot Water Volume Flow Rate",
		"    AUTOSIZE , !- Zone Total Beam Length",
		"    0.036 , !- Rated Primary Air Flow Rate per Meter",
		"    597 , !- Rated Beam Cooling Capacity per Meter",
		"    10.0 , !- Rated Cooling Room Air Chilled Water Temperature Difference",
		"    5.2E-5 , !- Rated Chilled Water Volume Flow Rate per Meter",
		"    CapModFuncOfTempDiff, !- Beam Cooling Capacity Temperature Difference Modification Factor Curve or Table Name",
		"    CoolCapModFuncOfSAFlow, !- Beam Cooling Capacity Air Flow Modification Factor Curve or Table Name",
		"    CapModFuncOfWaterFlow, !- Beam Cooling Capacity Chilled Water Flow Modification Factor Curve or Table Name",
		"    1548 , !- Rated Beam Heating Capacity per Meter",
		"    27.8, !- Rated Heating Room Air Hot Water Temperature Difference",
		"    5.2E-5, !- Rated Hot Water Volume Flow Rate per Meter",
		"    CapModFuncOfTempDiff, !- Beam Heating Capacity Temperature Difference Modification Factor Curve or Table Name",
		"    HeatCapModFuncOfSAFlow, !- Beam Heating Capacity Air Flow Modification Factor Curve or Table Name",
		"    CapModFuncOfWaterFlow; !- Beam Heating Capacity Hot Water Flow Modification Factor Curve or Table Name",
		"  Curve:Linear,  ! y = x",
		"    CapModFuncOfTempDiff, !-Name",
		"    0, !_ Coef Const",
		"    1, !- Coef x",
		"    0,  !- min x",
		"    1.5, !- max x",
		"    0.0 , !- min y",
		"    1.5; ! max y",
		"  Table:OneIndependentVariable,",
		"    CoolCapModFuncOfSAFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.714,!- min x",
		"    1.2857,!- max x",
		"    0.8234,!- min y",
		"    1.1256,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"    , !- normalization ref",
		"    0.714286, 0.823403,",
		"    1.0,      1.0,",
		"    1.2857,   1.1256;",
		"  Table:OneIndependentVariable,",
		"    CapModFuncOfWaterFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.0,!- min x",
		"    1.333333,!- max x",
		"    0.0,!- min y",
		"    1.04,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"     , !- normalization ref",
		"    0.0,      0.0,",
		"    0.05,     0.001,",
		"    0.33333,  0.71,",
		"    0.5,      0.85,",
		"    0.666667, 0.92,",
		"    0.833333, 0.97,",
		"    1.0,      1.0,",
		"    1.333333, 1.04;",
		"  Table:OneIndependentVariable,",
		"    HeatCapModFuncOfSAFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.714,!- min x",
		"    1.2857,!- max x",
		"    0.8554,!- min y",
		"    1.0778,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"    , !- normalization ref",
		"    0.714286, 0.8554,",
		"    1.0,      1.0,",
		"    1.2857,   1.0778; ",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		DataGlobals::NumOfZones = 1;

		DataHeatBalance::Zone.allocate( DataGlobals::NumOfZones );

		DataZoneEquipment::ZoneEquipConfig.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).NumInletNodes = 1;
		DataZoneEquipment::ZoneEquipConfig( 1 ).IsControlled = true;
		DataZoneEquipment::ZoneEquipConfig( 1 ).InletNode.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitCool.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitHeat.allocate( 1 );

		DataZoneEquipment::ZoneEquipConfig( 1 ).InletNode( 1 ) = 3;
		bool ErrorsFound =  false;
		DataZoneEquipment::ZoneEquipConfig( 1 ).ZoneNode = NodeInputManager::GetOnlySingleNode( "Zone 1 Node", ErrorsFound, "Zone", "BeamTest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_ZoneNode, 1, DataLoopNode::ObjectIsNotParent, "Test zone node" );

		DataDefineEquip::NumAirDistUnits = 1;
		DataDefineEquip::AirDistUnit.allocate( 1 );
		DataDefineEquip::AirDistUnit( 1 ).EquipName( 1 ) = "PERIMETER_TOP_ZN_4 4PIPE BEAM"; // needs to be uppercased, or item will not be found at line 2488 in IP
		DataDefineEquip::AirDistUnit( 1 ).OutletNodeNum = 3;

		DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr = FourPipeBeam::HVACFourPipeBeam::fourPipeBeamFactory( DataDefineEquip::SingleDuctConstVolFourPipeBeam, DataDefineEquip::AirDistUnit( 1 ).EquipName( 1 ) );


		//EXPECT_EQ( DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->name, "PERIMETER_TOP_ZN_4 4PIPE BEAM");

		EXPECT_EQ( 2, DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitHeat( 1 ).InNode );

		EXPECT_EQ( 3, DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitHeat( 1 ).OutNode );
		//EXPECT_EQ( DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->aDUNum, 1 );

	}


	TEST_F( EnergyPlusFixture, Beam_sizeandSimulateOneZone )
	{
			std::string const idf_objects = delimited_string( {
		"    SimulationControl,",
		"    YES,                     !- Do Zone Sizing Calculation",
		"    YES,                     !- Do System Sizing Calculation",
		"    YES,                     !- Do Plant Sizing Calculation",
		"    Yes,                     !- Run Simulation for Sizing Periods",
		"    yes;                     !- Run Simulation for Weather File Run Periods",

		"    Building,",
		"    Simple One Zone (Wireframe DXF),  !- Name",
		"    0,                       !- North Axis {deg}",
		"    Suburbs,                 !- Terrain",
		"    0.04,                    !- Loads Convergence Tolerance Value",
		"    0.004,                   !- Temperature Convergence Tolerance Value {deltaC}",
		"    MinimalShadowing,        !- Solar Distribution",
		"    30,                      !- Maximum Number of Warmup Days",
		"    6;                       !- Minimum Number of Warmup Days",

		"    RunPeriod,",
		"    annual,                        !- Name",
		"    1,                       !- Begin Month",
		"    1,                       !- Begin Day of Month",
		"    12,                      !- End Month",
		"    31,                      !- End Day of Month",
		"    Sunday,                  !- Day of Week for Start Day",
		"    No,                      !- Use Weather File Holidays and Special Days",
		"    No,                      !- Use Weather File Daylight Saving Period",
		"    No,                      !- Apply Weekend Holiday Rule",
		"    Yes,                     !- Use Weather File Rain Indicators",
		"    Yes,                     !- Use Weather File Snow Indicators",
		"    1.0000;                  !- Number of Times Runperiod to be Repeated",

		"    ScheduleTypeLimits,",
		"    On/Off,                  !- Name",
		"    0,                       !- Lower Limit Value",
		"    1,                       !- Upper Limit Value",
		"    DISCRETE;                !- Numeric Type",

		"    ScheduleTypeLimits,",
		"    Any Number;              !- Name",

		"    ScheduleTypeLimits,",
		"    Temperature,             !- Name",
		"    -60,                     !- Lower Limit Value",
		"    200,                     !- Upper Limit Value",
		"    CONTINUOUS;              !- Numeric Type",

		"    ScheduleTypeLimits,",
		"    Control Type,            !- Name",
		"    0,                       !- Lower Limit Value",
		"    4,                       !- Upper Limit Value",
		"    DISCRETE;                !- Numeric Type",

		"    Schedule:Compact,",
		"    ALWAYS_ON,               !- Name",
		"    On/Off,                  !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,1;          !- Field 3",

		"    Schedule:Compact,",
		"    ACTIVITY_SCH,            !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,120;        !- Field 3",

		"    Schedule:Compact,",
		"    WORK_EFF_SCH,            !- Name",
		"    Fraction,                !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,0.0;        !- Field 3",

		"    Schedule:Compact,",
		"    AIR_VELO_SCH,            !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,0.2;        !- Field 3",

		"    Schedule:Compact,",
		"    CLOTHING_SCH,            !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 04/30,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,1.0,        !- Field 3",
		"    Through: 09/30,          !- Field 5",
		"    For: AllDays,            !- Field 6",
		"    Until: 24:00,0.5,        !- Field 7",
		"    Through: 12/31,          !- Field 9",
		"    For: AllDays,            !- Field 10",
		"    Until: 24:00,1.0;        !- Field 11",

		"    Schedule:Compact,",
		"    BLDG_OCC_SCH,            !- Name",
		"    Fraction,                !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: SummerDesignDay,    !- Field 2",
		"    Until: 06:00,0.0,        !- Field 3",
		"    Until: 22:00,1.0,        !- Field 5",
		"    Until: 24:00,0.05,       !- Field 7",
		"    For: Weekdays,           !- Field 9",
		"    Until: 06:00,0.0,        !- Field 10",
		"    Until: 07:00,0.1,        !- Field 12",
		"    Until: 08:00,0.2,        !- Field 14",
		"    Until: 12:00,0.95,       !- Field 16",
		"    Until: 13:00,0.5,        !- Field 18",
		"    Until: 17:00,0.95,       !- Field 20",
		"    Until: 18:00,0.7,        !- Field 22",
		"    Until: 20:00,0.4,        !- Field 24",
		"    Until: 22:00,0.1,        !- Field 26",
		"    Until: 24:00,0.05,       !- Field 28",
		"    For: Saturday,           !- Field 30",
		"    Until: 06:00,0.0,        !- Field 31",
		"    Until: 08:00,0.1,        !- Field 33",
		"    Until: 14:00,0.5,        !- Field 35",
		"    Until: 17:00,0.1,        !- Field 37",
		"    Until: 24:00,0.0,        !- Field 39",
		"    For: AllOtherDays,       !- Field 41",
		"    Until: 24:00,0.0;        !- Field 42",

		"    Schedule:Compact,",
		"    BLDG_LIGHT_SCH,          !- Name",
		"    Fraction,                !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays,           !- Field 2",
		"    Until: 05:00,0.05,       !- Field 3",
		"    Until: 07:00,0.1,        !- Field 5",
		"    Until: 08:00,0.3,        !- Field 7",
		"    Until: 17:00,0.9,        !- Field 9",
		"    Until: 18:00,0.7,        !- Field 11",
		"    Until: 20:00,0.5,        !- Field 13",
		"    Until: 22:00,0.3,        !- Field 15",
		"    Until: 23:00,0.1,        !- Field 17",
		"    Until: 24:00,0.05,       !- Field 19",
		"    For: Saturday,           !- Field 21",
		"    Until: 06:00,0.05,       !- Field 22",
		"    Until: 08:00,0.1,        !- Field 24",
		"    Until: 14:00,0.5,        !- Field 26",
		"    Until: 17:00,0.15,       !- Field 28",
		"    Until: 24:00,0.05,       !- Field 30",
		"    For: SummerDesignDay,    !- Field 32",
		"    Until: 24:00,1.0,        !- Field 33",
		"    For: WinterDesignDay,    !- Field 35",
		"    Until: 24:00,0.0,        !- Field 36",
		"    For: AllOtherDays,       !- Field 38",
		"    Until: 24:00,0.05;       !- Field 39",

		"    Schedule:Compact,",
		"    BLDG_EQUIP_SCH,          !- Name",
		"    Fraction,                !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays,           !- Field 2",
		"    Until: 08:00,0.40,       !- Field 3",
		"    Until: 12:00,0.90,       !- Field 5",
		"    Until: 13:00,0.80,       !- Field 7",
		"    Until: 17:00,0.90,       !- Field 9",
		"    Until: 18:00,0.80,       !- Field 11",
		"    Until: 20:00,0.60,       !- Field 13",
		"    Until: 22:00,0.50,       !- Field 15",
		"    Until: 24:00,0.40,       !- Field 17",
		"    For: Saturday,           !- Field 19",
		"    Until: 06:00,0.30,       !- Field 20",
		"    Until: 08:00,0.4,        !- Field 22",
		"    Until: 14:00,0.5,        !- Field 24",
		"    Until: 17:00,0.35,       !- Field 26",
		"    Until: 24:00,0.30,       !- Field 28",
		"    For: SummerDesignDay,    !- Field 30",
		"    Until: 24:00,1.0,        !- Field 31",
		"    For: WinterDesignDay,    !- Field 33",
		"    Until: 24:00,0.0,        !- Field 34",
		"    For: AllOtherDays,       !- Field 36",
		"    Until: 24:00,0.30;       !- Field 37",

		"  SurfaceConvectionAlgorithm:Inside,TARP;",

		"  SurfaceConvectionAlgorithm:Outside,DOE-2;",

		"  HeatBalanceAlgorithm,ConductionTransferFunction,200.0000;",

		"  ZoneAirHeatBalanceAlgorithm,",
		"    AnalyticalSolution;      !- Algorithm",

		"  Sizing:Parameters,",
		"    1.33,                    !- Heating Sizing Factor",
		"    1.33,                    !- Cooling Sizing Factor",
		"    6;                       !- Timesteps in Averaging Window",

		"  ConvergenceLimits,",
		"    2,                       !- Minimum System Timestep {minutes}",
		"    25;                      !- Maximum HVAC Iterations",

		"  ShadowCalculation,",
		"    AverageOverDaysInFrequency,  !- Calculation Method",
		"    7,                       !- Calculation Frequency",
		"    15000;                   !- Maximum Figures in Shadow Overlap Calculations",

		"  Timestep,6;",
		"  Site:Location,",
		"    USA IL-CHICAGO-OHARE,    !- Name",
		"    41.77,                   !- Latitude {deg}",
		"    -87.75,                  !- Longitude {deg}",
		"    -6.00,                   !- Time Zone {hr}",
		"    190;                     !- Elevation {m}",

		"! CHICAGO_IL_USA Annual Heating 99.6%, MaxDB=-20.6°C",

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

		"! CHICAGO_IL_USA Annual Cooling (WB=>MDB) .4%, MDB=31.2°C WB=25.5°C",

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

		"  Material:NoMass,",
		"    R13LAYER,                !- Name",
		"    Rough,                   !- Roughness",
		"    2.290965,                !- Thermal Resistance {m2-K/W}",
		"    0.9000000,               !- Thermal Absorptance",
		"    0.7500000,               !- Solar Absorptance",
		"    0.7500000;               !- Visible Absorptance",

		"  Material:NoMass,",
		"    R31LAYER,                !- Name",
		"    Rough,                   !- Roughness",
		"    5.456,                   !- Thermal Resistance {m2-K/W}",
		"    0.9000000,               !- Thermal Absorptance",
		"    0.7500000,               !- Solar Absorptance",
		"    0.7500000;               !- Visible Absorptance",

		"  Material,",
		"    C5 - 4 IN HW CONCRETE,   !- Name",
		"    MediumRough,             !- Roughness",
		"    0.1014984,               !- Thickness {m}",
		"    1.729577,                !- Conductivity {W/m-K}",
		"    2242.585,                !- Density {kg/m3}",
		"    836.8000,                !- Specific Heat {J/kg-K}",
		"    0.9000000,               !- Thermal Absorptance",
		"    0.6500000,               !- Solar Absorptance",
		"    0.6500000;               !- Visible Absorptance",

		"  Construction,",
		"    R13WALL,                 !- Name",
		"    R13LAYER;                !- Outside Layer",

		"  Construction,",
		"   FLOOR,                   !- Name",
		"    C5 - 4 IN HW CONCRETE;   !- Outside Layer",

		"  Construction,",
		"    ROOF31,                  !- Name",
		"    R31LAYER;                !- Outside Layer",

		"  Zone,",
		"    ZONE ONE,                !- Name",
		"    0,                       !- Direction of Relative North {deg}",
		"    0,                       !- X Origin {m}",
		"    0,                       !- Y Origin {m}",
		"    0,                       !- Z Origin {m}",
		"    1,                       !- Type",
		"    1,                       !- Multiplier",
		"    autocalculate,           !- Ceiling Height {m}",
		"    autocalculate;           !- Volume {m3}",

		"  People,",
		"    ZONE ONE People,  !- Name",
		"    ZONE ONE,      !- Zone or ZoneList Name",
		"    BLDG_OCC_SCH,            !- Number of People Schedule Name",
		"    Area/Person,             !- Number of People Calculation Method",
		"    ,                        !- Number of People",
		"    ,                        !- People per Zone Floor Area {person/m2}",
		"    18.58,                   !- Zone Floor Area per Person {m2/person}",
		"    0.3000,                  !- Fraction Radiant",
		"    AUTOCALCULATE,           !- Sensible Heat Fraction",
		"    ACTIVITY_SCH,            !- Activity Level Schedule Name",
		"    ,                        !- Carbon Dioxide Generation Rate {m3/s-W}",
		"    No,                      !- Enable ASHRAE 55 Comfort Warnings",
		"    ZoneAveraged,            !- Mean Radiant Temperature Calculation Type",
		"    ,                        !- Surface Name/Angle Factor List Name",
		"    WORK_EFF_SCH,            !- Work Efficiency Schedule Name",
		"    ClothingInsulationSchedule,  !- Clothing Insulation Calculation Method",
		"    ,                        !- Clothing Insulation Calculation Method Schedule Name",
		"    CLOTHING_SCH,            !- Clothing Insulation Schedule Name",
		"    AIR_VELO_SCH,            !- Air Velocity Schedule Name",
		"    FANGER;                  !- Thermal Comfort Model 1 Type",

		"  Lights,",
		"    ZONE ONE Lights,  !- Name",
		"    ZONE ONE,      !- Zone or ZoneList Name",
		"    BLDG_LIGHT_SCH,          !- Schedule Name",
		"    Watts/Area,              !- Design Level Calculation Method",
		"    ,                        !- Lighting Level {W}",
		"    10.76,                   !- Watts per Zone Floor Area {W/m2}",
		"    ,                        !- Watts per Person {W/person}",
		"    0.0000,                  !- Return Air Fraction",
		"    0.7000,                  !- Fraction Radiant",
		"    0.2000,                  !- Fraction Visible",
		"    1.0000,                  !- Fraction Replaceable",
		"    General,                 !- End-Use Subcategory",
		"    No;                      !- Return Air Fraction Calculated from Plenum Temperature",

		"  ElectricEquipment,",
		"    ZONE ONE Equip,  !- Name",
		"    ZONE ONE,             !- Zone or ZoneList Name",
		"    BLDG_EQUIP_SCH,          !- Schedule Name",
		"    Watts/Area,              !- Design Level Calculation Method",
		"    ,                        !- Design Level {W}",
		"    10.76,                   !- Watts per Zone Floor Area {W/m2}",
		"    ,                        !- Watts per Person {W/person}",
		"    0.0000,                  !- Fraction Latent",
		"    0.5000,                  !- Fraction Radiant",
		"    0.0000,                  !- Fraction Lost",
		"    General;                 !- End-Use Subcategory",

		"  ScheduleTypeLimits,",
		"    Fraction,                !- Name",
		"    0.0,                     !- Lower Limit Value",
		"    1.0,                     !- Upper Limit Value",
		"    CONTINUOUS;              !- Numeric Type",

		"  GlobalGeometryRules,",
		"    UpperLeftCorner,         !- Starting Vertex Position",
		"    CounterClockWise,        !- Vertex Entry Direction",
		"    World;                   !- Coordinate System",

		"  BuildingSurface:Detailed,",
		"    Zn001:Wall001,           !- Name",
		"    Wall,                    !- Surface Type",
		"    R13WALL,                 !- Construction Name",
		"    ZONE ONE,                !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"     ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    0,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    15.24000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    15.24000,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Wall002,           !- Name",
		"    Wall,                    !- Surface Type",
		"    R13WALL,                 !- Construction Name",
		"    ZONE ONE,                !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    15.24000,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    15.24000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Wall003,           !- Name",
		"    Wall,                    !- Surface Type",
		"    R13WALL,                 !- Construction Name",
		"    ZONE ONE,                !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    0,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    0,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Wall004,           !- Name",
		"    Wall,                    !- Surface Type",
		"    R13WALL,                 !- Construction Name",
		"    ZONE ONE,                !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    0,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    0,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    0,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Flr001,            !- Name",
		"    Floor,                   !- Surface Type",
		"    FLOOR,                   !- Construction Name",
		"    ZONE ONE,                !- Zone Name",
		"    Adiabatic,               !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    NoSun,                   !- Sun Exposure",
		"    NoWind,                  !- Wind Exposure",
		"    1.000000,                !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    15.24000,0.000000,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
		"    0.000000,0.000000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    0.000000,15.24000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    15.24000,15.24000,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn001:Roof001,           !- Name",
		"    Roof,                    !- Surface Type",
		"    ROOF31,                  !- Construction Name",
		"    ZONE ONE,                !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0,                       !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    0.000000,15.24000,4.572,  !- X,Y,Z ==> Vertex 1 {m}",
		"    0.000000,0.000000,4.572,  !- X,Y,Z ==> Vertex 2 {m}",
		"    15.24000,0.000000,4.572,  !- X,Y,Z ==> Vertex 3 {m}",
		"    15.24000,15.24000,4.572;  !- X,Y,Z ==> Vertex 4 {m}",

		"  Sizing:Zone,",
		"    Zone One,      !- Zone or ZoneList Name",
		"    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
		"    12.8000,                 !- Zone Cooling Design Supply Air Temperature {C}",
		"    ,                        !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
		"    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
		"    50.0000,                 !- Zone Heating Design Supply Air Temperature {C}",
		"    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
		"    0.0085,                  !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    0.0080,                  !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    SZ DSOA Zone One,  !- Design Specification Outdoor Air Object Name",
		"    ,                        !- Zone Heating Sizing Factor",
		"    ,                        !- Zone Cooling Sizing Factor",
		"    DesignDay,               !- Cooling Design Air Flow Method",
		"    ,                        !- Cooling Design Air Flow Rate {m3/s}",
		"    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Cooling Minimum Air Flow {m3/s}",
		"    ,                        !- Cooling Minimum Air Flow Fraction",
		"    DesignDay,               !- Heating Design Air Flow Method",
		"    ,                        !- Heating Design Air Flow Rate {m3/s}",
		"    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Heating Maximum Air Flow {m3/s}",
		"    ;                        !- Heating Maximum Air Flow Fraction",

		"  DesignSpecification:OutdoorAir,",
		"    SZ DSOA Zone One,  !- Name",
		"    Flow/Person,             !- Outdoor Air Method",
		"    0.0125;                  !- Outdoor Air Flow per Person {m3/s-person}",

		"  ZoneControl:Thermostat,",
		"    Zone One Thermostat,  !- Name",
		"    Zone One,      !- Zone or ZoneList Name",
		"    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
		"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
		"    Zone One DualSPSched;  !- Control 1 Name",

		"  Schedule:Compact,",
		"    Dual Zone Control Type Sched,  !- Name",
		"    Control Type,            !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,4;          !- Field 3",

		"  ThermostatSetpoint:DualSetpoint,",
		"    Zone One DualSPSched,  !- Name",
		"    HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
		"    CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",

		"  Schedule:Compact,",
		"    CLGSETP_SCH,             !- Name",
		"    Temperature,             !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays SummerDesignDay, !- Field 2",
		"    Until: 06:00,26.7,       !- Field 3",
		"    Until: 22:00,24.0,       !- Field 5",
		"    Until: 24:00,26.7,       !- Field 7",
		"    For: Saturday,           !- Field 9",
		"    Until: 06:00,26.7,       !- Field 10",
		"    Until: 18:00,24.0,       !- Field 12",
		"    Until: 24:00,26.7,       !- Field 14",
		"    For WinterDesignDay,     !- Field 16",
		"    Until: 24:00,26.7,       !- Field 17",
		"    For: AllOtherDays,       !- Field 19",
		"    Until: 24:00,26.7;       !- Field 20",

		"  Schedule:Compact,",
		"    HTGSETP_SCH,             !- Name",
		"    Temperature,             !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays,           !- Field 2",
		"    Until: 06:00,15.6,       !- Field 3",
		"    Until: 22:00,21.0,       !- Field 5",
		"    Until: 24:00,15.6,       !- Field 7",
		"    For SummerDesignDay,     !- Field 9",
		"    Until: 24:00,15.6,       !- Field 10",
		"    For: Saturday,           !- Field 12",
		"    Until: 06:00,15.6,       !- Field 13",
		"    Until: 18:00,21.0,       !- Field 15",
		"    Until: 24:00,15.6,       !- Field 17",
		"    For: WinterDesignDay,    !- Field 19",
		"    Until: 24:00,21.0,       !- Field 20",
		"    For: AllOtherDays,       !- Field 22",
		"    Until: 24:00,15.6;       !- Field 23",

		"  Sizing:System,",
		"   CV_1,                   !- AirLoop Name",
		"    Sensible,                !- Type of Load to Size On",
		"    AUTOSIZE,                !- Design Outdoor Air Flow Rate {m3/s}",
		"    0.3,                     !- Central Heating Maximum System Air Flow Ratio",
		"    7.0,                     !- Preheat Design Temperature {C}",
		"    0.008,                   !- Preheat Design Humidity Ratio {kgWater/kgDryAir}",
		"    12.8000,                 !- Precool Design Temperature {C}",
		"    0.008,                   !- Precool Design Humidity Ratio {kgWater/kgDryAir}",
		"    12.8000,                 !- Central Cooling Design Supply Air Temperature {C}",
		"    16.7000,                 !- Central Heating Design Supply Air Temperature {C}",
		"    NonCoincident,           !- Type of Zone Sum to Use",
		"    No,                      !- 100% Outdoor Air in Cooling",
		"    No,                      !- 100% Outdoor Air in Heating",
		"    0.0085,                  !- Central Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    0.0080,                  !- Central Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    DesignDay,               !- Cooling Design Air Flow Method",
		"    ,                        !- Cooling Design Air Flow Rate {m3/s}",
		"    ,                        !- Supply Air Flow Rate Per Floor Area During Cooling Operation {m3/s-m2}",
		"    ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
		"    ,                        !- Design Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}",
		"    DesignDay,               !- Heating Design Air Flow Method",
		"    ,                        !- Heating Design Air Flow Rate {m3/s}",
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
		"    ONOFF;                     !- Central Cooling Capacity Control Method",

		"  ZoneHVAC:EquipmentConnections,",
		"    Zone One,      !- Zone Name",
		"    Zone One Equipment,  !- Zone Conditioning Equipment List Name",
		"    Zone One Inlet Nodes,  !- Zone Air Inlet Node or NodeList Name",
		"    ,                        !- Zone Air Exhaust Node or NodeList Name",
		"    Zone One Air Node,  !- Zone Air Node Name",
		"    Zone One Return Air Node Name;  !- Zone Return Air Node Name",

		"  ZoneHVAC:EquipmentList,",
		"    Zone One Equipment,  !- Name",
		"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
		"    Zone One 4pipe Beam,  !- Zone Equipment 1 Name",
		"    1,                       !- Zone Equipment 1 Cooling Sequence",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

		"  ZoneHVAC:AirDistributionUnit,",
		"    Zone One 4pipe Beam,  !- Name",
		"    Zone One 4pipe Beam Outlet Node Name,  !- Air Distribution Unit Outlet Node Name",
		"    AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam,  !- Air Terminal Object Type",
		"    Zone One 4pipe Beam;  !- Air Terminal Name",

		"  NodeList,",
		"    Zone One Inlet Nodes,  !- Name",
		"    Zone One 4pipe Beam Outlet Node Name;  !- Node 1 Name",

		"  AirLoopHVAC:ZoneSplitter,",
		"    CV_1 Supply Air Splitter,  !- Name",
		"    CV_1 Zone Equipment Inlet Node,  !- Inlet Node Name",
		"    Zone One 4pipe Beam Inlet Node Name;  !- Outlet 1 Node Name",

		"  AirLoopHVAC:ReturnPath,",
		"    CV_1 Return Air Path,   !- Name",
		"    CV_1 Zone Equipment Outlet Node,  !- Return Air Path Outlet Node Name",
		"    AirLoopHVAC:ZoneMixer,   !- Component 2 Object Type",
		"    CV_1 Return Air Mixer;  !- Component 2 Name",

		"  AirLoopHVAC:ZoneMixer,",
		"    CV_1 Return Air Mixer,  !- Name",
		"    CV_1 Zone Equipment Outlet Node,  !- Outlet Node Name",
		"    Zone One Return Air Node Name;  !- Inlet 1 Node Name",

		"  Fan:VariableVolume,",
		"    CV_1_Fan,               !- Name",
		"    always_on,       !- Availability Schedule Name",
		"    0.6045,                  !- Fan Total Efficiency",
		"    1017.592,                !- Pressure Rise {Pa}",
		"    AUTOSIZE,                !- Maximum Flow Rate {m3/s}",
		"    FixedFlowRate,           !- Fan Power Minimum Flow Rate Input Method",
		"    ,                        !- Fan Power Minimum Flow Fraction",
		"    0.0000,                  !- Fan Power Minimum Air Flow Rate {m3/s}",
		"    0.93,                    !- Motor Efficiency",
		"    1.0,                     !- Motor In Airstream Fraction",
		"    0.0407598940,            !- Fan Power Coefficient 1",
		"    0.08804497,              !- Fan Power Coefficient 2",
		"    -0.072926120,            !- Fan Power Coefficient 3",
		"    0.9437398230,            !- Fan Power Coefficient 4",
		"    0,                       !- Fan Power Coefficient 5",
		"    CV_1_HeatC-CV_1_FanNode,  !- Air Inlet Node Name",
		"    CV_1 Supply Equipment Outlet Node,  !- Air Outlet Node Name",
		"    Fan Energy;              !- End-Use Subcategory",

		"  Coil:Heating:Water,",
		"    CV_1_HeatC,             !- Name",
		"    ALWAYS_ON,               !- Availability Schedule Name",
		"    AUTOSIZE,                !- U-Factor Times Area Value {W/K}",
		"    AUTOSIZE,                !- Maximum Water Flow Rate {m3/s}",
		"    CV_1_HeatCDemand Inlet Node,  !- Water Inlet Node Name",
		"    CV_1_HeatCDemand Outlet Node,  !- Water Outlet Node Name",
		"    CV_1_CoolC-CV_1_HeatCNode,  !- Air Inlet Node Name",
		"    CV_1_HeatC-CV_1_FanNode,  !- Air Outlet Node Name",
		"    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
		"    AUTOSIZE,                !- Rated Capacity {W}",
		"    82.2,                    !- Rated Inlet Water Temperature {C}",
		"    16.6,                    !- Rated Inlet Air Temperature {C}",
		"    71.1,                    !- Rated Outlet Water Temperature {C}",
		"    32.2,                    !- Rated Outlet Air Temperature {C}",
		"    ;                        !- Rated Ratio for Air and Water Convection",

		"  Coil:Cooling:Water,",
		"    CV_1_CoolC,             !- Name",
		"    ALWAYS_ON,               !- Availability Schedule Name",
		"    AUTOSIZE,                !- Design Water Flow Rate {m3/s}",
		"    AUTOSIZE,                !- Design Air Flow Rate {m3/s}",
		"    AUTOSIZE,                !- Design Inlet Water Temperature {C}",
		"    AUTOSIZE,                !- Design Inlet Air Temperature {C}",
		"    AUTOSIZE,                !- Design Outlet Air Temperature {C}",
		"    AUTOSIZE,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
		"    AUTOSIZE,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
		"    CV_1_CoolCDemand Inlet Node,  !- Water Inlet Node Name",
		"    CV_1_CoolCDemand Outlet Node,  !- Water Outlet Node Name",
		"    CV_1_OA-CV_1_CoolCNode,!- Air Inlet Node Name",
		"    CV_1_CoolC-CV_1_HeatCNode,  !- Air Outlet Node Name",
		"    SimpleAnalysis,          !- Type of Analysis",
		"    CrossFlow;               !- Heat Exchanger Configuration",

		"  Controller:OutdoorAir,",
		"    CV_1_OA_Controller,     !- Name",
		"    CV_1_OARelief Node,     !- Relief Air Outlet Node Name",
		"    CV_1 Supply Equipment Inlet Node,  !- Return Air Node Name",
		"    CV_1_OA-CV_1_CoolCNode,!- Mixed Air Node Name",
		"    CV_1_OAInlet Node,      !- Actuator Node Name",
		"    AUTOSIZE,                !- Minimum Outdoor Air Flow Rate {m3/s}",
		"    AUTOSIZE,                !- Maximum Outdoor Air Flow Rate {m3/s}",
		"    DifferentialDryBulb,     !- Economizer Control Type",
		"    ModulateFlow,            !- Economizer Control Action Type",
		"    28.0,                    !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
		"    64000.0,                 !- Economizer Maximum Limit Enthalpy {J/kg}",
		"    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
		"    ,                        !- Electronic Enthalpy Limit Curve Name",
		"    -100.0,                  !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
		"    NoLockout,               !- Lockout Type",
		"    FixedMinimum,            !- Minimum Limit Type",
		"    MinOA_MotorizedDamper_Sched;  !- Minimum Outdoor Air Schedule Name",

		"  Schedule:Compact,",
		"    MinOA_MotorizedDamper_Sched,  !- Name",
		"    Fraction,                !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays SummerDesignDay, !- Field 2",
		"    Until: 07:00,0.0,        !- Field 3",
		"    Until: 22:00,1.0,        !- Field 5",
		"    Until: 24:00,0.0,        !- Field 7",
		"    For: Saturday WinterDesignDay, !- Field 9",
		"    Until: 07:00,0.0,        !- Field 10",
		"    Until: 18:00,1.0,        !- Field 12",
		"    Until: 24:00,0.0,        !- Field 14",
		"    For: AllOtherDays,       !- Field 16",
		"    Until: 24:00,0.0;        !- Field 17",

		"  SetpointManager:Scheduled,",
		"    CV_1 SAT setpoint,      !- Name",
		"    Temperature,             !- Control Variable",
		"    Seasonal-Reset-Supply-Air-Temp-Sch,  !- Schedule Name",
		"    CV_1 Supply Equipment Outlet Node;  !- Setpoint Node or NodeList Name",

		"  Schedule:Compact,",
		"    Seasonal-Reset-Supply-Air-Temp-Sch,  !- Name",
		"    Temperature,             !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,12.8;       !- Field 3",

		"  AirLoopHVAC,",
		"    CV_1,                   !- Name",
		"    CV_1_Controllers,       !- Controller List Name",
		"    CV_1 Availability Manager List,  !- Availability Manager List Name",
		"    AUTOSIZE,                !- Design Supply Air Flow Rate {m3/s}",
		"    CV_1 Air Loop Branches, !- Branch List Name",
		"    ,                        !- Connector List Name",
		"    CV_1 Supply Equipment Inlet Node,  !- Supply Side Inlet Node Name",
		"    CV_1 Zone Equipment Outlet Node,  !- Demand Side Outlet Node Name",
		"    CV_1 Zone Equipment Inlet Node,  !- Demand Side Inlet Node Names",
		"    CV_1 Supply Equipment Outlet Node;  !- Supply Side Outlet Node Names",

		"  AvailabilityManagerAssignmentList,",
		"    CV_1 Availability Manager List,  !- Name",
		"    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
		"    CV_1 Avail;   !- Availability Manager 1 Name",

		"  AvailabilityManager:Scheduled,",
		"    CV_1 Avail,   !- Name",
		"    always_on;    !- Schedule Name",

		"  NodeList,",
		"    CV_1_OANode List,       !- Name",
		"    CV_1_OAInlet Node;      !- Node 1 Name",

		"  BranchList,",
		"    CV_1 Air Loop Branches, !- Name",
		"    CV_1 Air Loop Main Branch;  !- Branch 1 Name",

		"  Branch,",
		"    CV_1 Air Loop Main Branch,  !- Name",
		"    AUTOSIZE,                !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
		"    CV_1_OA,                !- Component 1 Name",
		"    CV_1 Supply Equipment Inlet Node,  !- Component 1 Inlet Node Name",
		"    CV_1_OA-CV_1_CoolCNode,!- Component 1 Outlet Node Name",
		"    Passive,                 !- Component 1 Branch Control Type",
		"    Coil:Cooling:Water,      !- Component 2 Object Type",
		"    CV_1_CoolC,             !- Component 2 Name",
		"    CV_1_OA-CV_1_CoolCNode,!- Component 2 Inlet Node Name",
		"    CV_1_CoolC-CV_1_HeatCNode,  !- Component 2 Outlet Node Name",
		"    Passive,                 !- Component 2 Branch Control Type",
		"    Coil:Heating:Water,      !- Component 3 Object Type",
		"    CV_1_HeatC,             !- Component 3 Name",
		"    CV_1_CoolC-CV_1_HeatCNode,  !- Component 3 Inlet Node Name",
		"    CV_1_HeatC-CV_1_FanNode,  !- Component 3 Outlet Node Name",
		"    Passive,                 !- Component 3 Branch Control Type",
		"    Fan:VariableVolume,      !- Component 4 Object Type",
		"    CV_1_Fan,               !- Component 4 Name",
		"    CV_1_HeatC-CV_1_FanNode,  !- Component 4 Inlet Node Name",
		"    CV_1 Supply Equipment Outlet Node,  !- Component 4 Outlet Node Name",
		"    Active;                  !- Component 4 Branch Control Type",

		"  AirLoopHVAC:ControllerList,",
		"    CV_1_Controllers,       !- Name",
		"    Controller:WaterCoil,    !- Controller 1 Object Type",
		"    CV_1_CoolC_Controller,  !- Controller 1 Name",
		"    Controller:WaterCoil,    !- Controller 2 Object Type",
		"    CV_1_HeatC_Controller;  !- Controller 2 Name",

		"  AirLoopHVAC:ControllerList,",
		"    CV_1_OA_Controllers,    !- Name",
		"    Controller:OutdoorAir,   !- Controller 1 Object Type",
		"    CV_1_OA_Controller;     !- Controller 1 Name",

		"  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
		"    CV_1_OA_Equipment,      !- Name",
		"    OutdoorAir:Mixer,        !- Component 1 Object Type",
		"    CV_1_OAMixing Box;      !- Component 1 Name",

		"  AirLoopHVAC:OutdoorAirSystem,",
		"    CV_1_OA,                !- Name",
		"    CV_1_OA_Controllers,    !- Controller List Name",
		"    CV_1_OA_Equipment,      !- Outdoor Air Equipment List Name",
		"    CV_1 Availability Manager List;  !- Availability Manager List Name",

		"  OutdoorAir:NodeList,",
		"    CV_1_OANode List;       !- Node or NodeList Name 1",

		"  OutdoorAir:Mixer,",
		"    CV_1_OAMixing Box,      !- Name",
		"    CV_1_OA-CV_1_CoolCNode,!- Mixed Air Node Name",
		"    CV_1_OAInlet Node,      !- Outdoor Air Stream Node Name",
		"    CV_1_OARelief Node,     !- Relief Air Stream Node Name",
		"    CV_1 Supply Equipment Inlet Node;  !- Return Air Stream Node Name",

		"  SetpointManager:MixedAir,",
		"    CV_1_CoolC SAT Manager, !- Name",
		"    Temperature,             !- Control Variable",
		"    CV_1 Supply Equipment Outlet Node,  !- Reference Setpoint Node Name",
		"    CV_1_HeatC-CV_1_FanNode,  !- Fan Inlet Node Name",
		"    CV_1 Supply Equipment Outlet Node,  !- Fan Outlet Node Name",
		"    CV_1_CoolC-CV_1_HeatCNode;  !- Setpoint Node or NodeList Name",

		"  SetpointManager:MixedAir,",
		"    CV_1_HeatC SAT Manager, !- Name",
		"    Temperature,             !- Control Variable",
		"    CV_1 Supply Equipment Outlet Node,  !- Reference Setpoint Node Name",
		"    CV_1_HeatC-CV_1_FanNode,  !- Fan Inlet Node Name",
		"    CV_1 Supply Equipment Outlet Node,  !- Fan Outlet Node Name",
		"    CV_1_HeatC-CV_1_FanNode;  !- Setpoint Node or NodeList Name",

		"  SetpointManager:MixedAir,",
		"    CV_1_OAMixed Air Temp Manager,  !- Name",
		"    Temperature,             !- Control Variable",
		"    CV_1 Supply Equipment Outlet Node,  !- Reference Setpoint Node Name",
		"    CV_1_HeatC-CV_1_FanNode,  !- Fan Inlet Node Name",
		"    CV_1 Supply Equipment Outlet Node,  !- Fan Outlet Node Name",
		"    CV_1_OA-CV_1_CoolCNode;!- Setpoint Node or NodeList Name",

		"  AirLoopHVAC:SupplyPath,",
		"    CV_1,                   !- Name",
		"    CV_1 Zone Equipment Inlet Node,  !- Supply Air Path Inlet Node Name",
		"    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
		"    CV_1 Supply Air Splitter;  !- Component 1 Name",

		"  Controller:WaterCoil,",
		"    CV_1_CoolC_Controller,  !- Name",
		"    Temperature,             !- Control Variable",
		"    Reverse,                 !- Action",
		"    Flow,                    !- Actuator Variable",
		"    CV_1_CoolC-CV_1_HeatCNode,  !- Sensor Node Name",
		"    CV_1_CoolCDemand Inlet Node,  !- Actuator Node Name",
		"    ,                        !- Controller Convergence Tolerance {deltaC}",
		"    AUTOSIZE,                !- Maximum Actuated Flow {m3/s}",
		"    0.0;                     !- Minimum Actuated Flow {m3/s}",

		"  Controller:WaterCoil,",
		"    CV_1_HeatC_Controller,  !- Name",
		"    Temperature,             !- Control Variable",
		"    Normal,                  !- Action",
		"    Flow,                    !- Actuator Variable",
		"    CV_1_HeatC-CV_1_FanNode,  !- Sensor Node Name",
		"    CV_1_HeatCDemand Inlet Node,  !- Actuator Node Name",
		"    0.0001,                  !- Controller Convergence Tolerance {deltaC}",
		"    AUTOSIZE,                !- Maximum Actuated Flow {m3/s}",
		"    0.0;                     !- Minimum Actuated Flow {m3/s}",

		"  Sizing:Plant,",
		"    Chilled Water Loop,                !- Plant or Condenser Loop Name",
		"    Cooling,                 !- Loop Type",
		"    6.67,                    !- Design Loop Exit Temperature {C}",
		"    6.67,                    !- Loop Design Temperature Difference {deltaC}",
		"    Coincident,              !- Sizing Option",
		"    1,                       !- Zone Timesteps in Averaging Window",
		"    None;                    !- Coincident Sizing Factor Mode",

		"  ConnectorList,",
		"    CoolSys1 Demand Connectors,  !- Name",
		"    Connector:Splitter,      !- Connector 1 Object Type",
		"    CoolSys1 Demand Splitter,!- Connector 1 Name",
		"    Connector:Mixer,         !- Connector 2 Object Type",
		"    CoolSys1 Demand Mixer;   !- Connector 2 Name",

		"  BranchList,",
		"    CoolSys1 Demand Branches,!- Name",
		"    CoolSys1 Demand Inlet Branch,  !- Branch 1 Name",
		"    CoolSys1 Demand Load Branch 1,  !- Branch 2 Name",
		"    CoolSys1 Demand Load Branch 2,  !- Branch 3 Name",
		"    CoolSys1 Demand Bypass Branch,  !- Branch 6 Name",
		"    CoolSys1 Demand Outlet Branch;  !- Branch 7 Name",

		"  Connector:Splitter,",
		"    CoolSys1 Demand Splitter,!- Name",
		"    CoolSys1 Demand Inlet Branch,  !- Inlet Branch Name",
		"    CoolSys1 Demand Load Branch 1,  !- Outlet Branch 1 Name",
		"    CoolSys1 Demand Load Branch 2,  !- Outlet Branch 2 Name",
		"    CoolSys1 Demand Bypass Branch;  !- Branch 6 Name",

		"  Connector:Mixer,",
		"    CoolSys1 Demand Mixer,   !- Name",
		"    CoolSys1 Demand Outlet Branch,  !- Outlet Branch Name",
		"    CoolSys1 Demand Load Branch 1,  !- Inlet Branch 1 Name",
		"    CoolSys1 Demand Load Branch 2,  !- Inlet Branch 2 Name",
		"    CoolSys1 Demand Bypass Branch;  !- Branch 6 Name",

		"  Branch,",
		"    CoolSys1 Demand Inlet Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    CoolSys1 Demand Inlet Pipe,  !- Component 1 Name",
		"    CoolSys1 Demand Inlet Node,  !- Component 1 Inlet Node Name",
		"    CoolSys1 Demand Inlet Pipe-CoolSys1 Demand Mixer,  !- Component 1 Outlet Node Name",
		"    Passive;                 !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    CoolSys1 Demand Inlet Pipe,  !- Name",
		"    CoolSys1 Demand Inlet Node,  !- Inlet Node Name",
		"    CoolSys1 Demand Inlet Pipe-CoolSys1 Demand Mixer;  !- Outlet Node Name",

		"  Pipe:Adiabatic,",
		"    CoolSys1 Demand Outlet Pipe,  !- Name",
		"    CoolSys1 Demand Mixer-CoolSys1 Demand Outlet Pipe,  !- Inlet Node Name",
		"    CoolSys1 Demand Outlet Node;  !- Outlet Node Name",

		"  Pipe:Adiabatic,",
		"    CoolSys1 Demand Bypass Pipe,  !- Name",
		"    CoolSys1 Demand Bypass Pipe Inlet Node,  !- Inlet Node Name",
		"    CoolSys1 Demand Bypass Pipe Outlet Node;  !- Outlet Node Name",

		"  Branch,",
		"    CoolSys1 Demand Load Branch 1,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Coil:Cooling:Water,      !- Component 1 Object Type",
		"    CV_1_CoolC,             !- Component 1 Name",
		"    CV_1_CoolCDemand Inlet Node,  !- Component 1 Inlet Node Name",
		"    CV_1_CoolCDemand Outlet Node,  !- Component 1 Outlet Node Name",
		"    Active;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    CoolSys1 Demand Load Branch 2,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam,      !- Component 1 Object Type",
		"    Zone One 4pipe Beam,  !- Component 1 Name",
		"    Zone One 4pipe Beam CW Inlet Node,  !- Component 1 Inlet Node Name",
		"    Zone One 4pipe Beam CW Outlet Node,  !- Component 1 Outlet Node Name",
		"    Active;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    CoolSys1 Demand Outlet Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    CoolSys1 Demand Outlet Pipe,  !- Component 1 Name",
		"    CoolSys1 Demand Mixer-CoolSys1 Demand Outlet Pipe,  !- Component 1 Inlet Node Name",
		"    CoolSys1 Demand Outlet Node,  !- Component 1 Outlet Node Name",
		"    Passive;                 !- Component 1 Branch Control Type",



		"  PlantLoop,",
		"    Chilled Water Loop,             !- Name",
		"    WATER,                   !- Fluid Type",
		"    ,                        !- User Defined Fluid Type",
		"    SOURCE Loop Operation,   !- Plant Equipment Operation Scheme Name",
		"    SOURCE Supply Outlet Node,  !- Loop Temperature Setpoint Node Name",
		"    100,                     !- Maximum Loop Temperature {C}",
		"    3,                       !- Minimum Loop Temperature {C",
		"    0.003,                   !- Maximum Loop Flow Rate {m3/s}",
		"    0,                       !- Minimum Loop Flow Rate {m3/s}",
		"    autocalculate,           !- Plant Loop Volume {m3}",
		"    SOURCE Supply Inlet Node,!- Plant Side Inlet Node Name",
		"    SOURCE Supply Outlet Node,  !- Plant Side Outlet Node Name",
		"    SOURCE Supply Branches,  !- Plant Side Branch List Name",
		"    SOURCE Supply Connectors,!- Plant Side Connector List Name",
		"    CoolSys1 Demand Inlet Node,  !- Demand Side Inlet Node Name",
		"    CoolSys1 Demand Outlet Node,  !- Demand Side Outlet Node Name",
		"    CoolSys1 Demand Branches,  !- Demand Side Branch List Name",
		"    CoolSys1 Demand Connectors,!- Demand Side Connector List Name",
		"    OPTIMAL;                 !- Load Distribution Scheme",

		"  SetpointManager:Scheduled,",
		"    SOURCE Loop Setpoint Manager,  !- Name",
		"    Temperature,             !- Control Variable",
		"    SOURCE Loop Temp Sch,    !- Schedule Name",
		"    SOURCE Loop Setpoint Node List;  !- Setpoint Node or NodeList Name",

		"  Schedule:Compact,",
		"    SOURCE Loop Temp Sch,    !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    THROUGH: 12/31,          !- Field 1",
		"    FOR: AllDays,            !- Field 2",
		"    UNTIL: 24:00,6.7;        !- Field 3",

		"  NodeList,",
		"    SOURCE Loop Setpoint Node List,  !- Name",
		"    SOURCE Supply Outlet Node;  !- Node 1 Name",

		"  PlantEquipmentOperationSchemes,",
		"    SOURCE Loop Operation,   !- Name",
		"    PlantEquipmentOperation:CoolingLoad,  !- Control Scheme 1 Object Type",
		"    SOURCE Purchased Only,   !- Control Scheme 1 Name",
		"    Always_On;        !- Control Scheme 1 Schedule Name",

		"  PlantEquipmentOperation:CoolingLoad,",
		"    SOURCE Purchased Only,   !- Name",
		"    0,                       !- Load Range 1 Lower Limit {W}",
		"    10000000,                !- Load Range 1 Upper Limit {W}",
		"    SOURCE Cooling Plant;    !- Range 1 Equipment List Name",

		"  PlantEquipmentList,",
		"    SOURCE Cooling Plant,    !- Name",
		"    DistrictCooling,         !- Equipment 1 Object Type",
		"    SOURCE Purchased Cooling;!- Equipment 1 Name",

		"  BranchList,",
		"    SOURCE Supply Branches,  !- Name",
		"    SOURCE Supply Inlet Branch,  !- Branch 1 Name",
		"    SOURCE Cooling Branch,   !- Branch 2 Name",
		"    SOURCE Supply Outlet Branch;  !- Branch 3 Name",

		"  ConnectorList,",
		"    SOURCE Supply Connectors,!- Name",
		"    Connector:Splitter,      !- Connector 1 Object Type",
		"    SOURCE Supply Splitter,  !- Connector 1 Name",
		"    Connector:Mixer,         !- Connector 2 Object Type",
		"    SOURCE Supply Mixer;     !- Connector 2 Name",

		"  Connector:Splitter,",
		"    SOURCE Supply Splitter,  !- Name",
		"    SOURCE Supply Inlet Branch,  !- Inlet Branch Name",
		"    SOURCE Cooling Branch;   !- Outlet Branch 1 Name",

		"  Connector:Mixer,",
		"    SOURCE Supply Mixer,     !- Name",
		"    SOURCE Supply Outlet Branch,  !- Outlet Branch Name",
		"    SOURCE Cooling Branch;   !- Inlet Branch 1 Name",

		"  Branch,",
		"    SOURCE Supply Inlet Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pump:VariableSpeed,      !- Component 1 Object Type",
		"    SOURCE Pump,             !- Component 1 Name",
		"    SOURCE Supply Inlet Node,!- Component 1 Inlet Node Name",
		"    SOURCE Supply Pump-Cooling Node,  !- Component 1 Outlet Node Name",
		"    ;                        !- Component 1 Branch Control Type",

		"  Pump:VariableSpeed,",
		"    SOURCE Pump,             !- Name",
		"    SOURCE Supply Inlet Node,!- Inlet Node Name",
		"    SOURCE Supply Pump-Cooling Node,  !- Outlet Node Name",
		"    0.005,                   !- Rated Flow Rate {m3/s}",
		"    300000,                  !- Rated Pump Head {Pa}",
		"    2250,                    !- Rated Power Consumption {W}",
		"    0.87,                    !- Motor Efficiency",
		"    0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"    0,                       !- Coefficient 1 of the Part Load Performance Curve",
		"    1,                       !- Coefficient 2 of the Part Load Performance Curve",
		"    0,                       !- Coefficient 3 of the Part Load Performance Curve",
		"    0,                       !- Coefficient 4 of the Part Load Performance Curve",
		"    0,                       !- Minimum Flow Rate {m3/s}",
		"    INTERMITTENT;            !- Pump Control Type",

		"  Branch,",
		"    SOURCE Cooling Branch,   !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    DistrictCooling,         !- Component 1 Object Type",
		"    SOURCE Purchased Cooling,!- Component 1 Name",
		"    SOURCE Supply Cooling Inlet Node,  !- Component 1 Inlet Node Name",
		"    SOURCE Supply Cooling Outlet Node,  !- Component 1 Outlet Node Name",
		"    ;                        !- Component 1 Branch Control Type",

		"  DistrictCooling,",
		"    SOURCE Purchased Cooling,!- Name",
		"    SOURCE Supply Cooling Inlet Node,  !- Chilled Water Inlet Node Name",
		"    SOURCE Supply Cooling Outlet Node,  !- Chilled Water Outlet Node Name",
		"    1000000;                 !- Nominal Capacity {W}",

		"  Branch,",
		"    SOURCE Supply Outlet Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    SOURCE Supply Outlet Pipe,  !- Component 1 Name",
		"    SOURCE Supply Cooling-Pipe Node,  !- Component 1 Inlet Node Name",
		"    SOURCE Supply Outlet Node,  !- Component 1 Outlet Node Name",
		"    ;                        !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    SOURCE Supply Outlet Pipe,  !- Name",
		"    SOURCE Supply Cooling-Pipe Node,  !- Inlet Node Name",
		"    SOURCE Supply Outlet Node;  !- Outlet Node Name",

		"  Branch,",
		"    CoolSys1 Demand Bypass Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    CoolSys1 Demand Bypass Pipe,  !- Component 1 Name",
		"    CoolSys1 Demand Bypass Pipe Inlet Node,  !- Component 1 Inlet Node Name",
		"    CoolSys1 Demand Bypass Pipe Outlet Node,  !- Component 1 Outlet Node Name",
		"    Bypass;                  !- Component 1 Branch Control Type",

		"  Sizing:Plant,",
		"    HeatSys1 Loop,                !- Plant or Condenser Loop Name",
		"    Heating,                 !- Loop Type",
		"    60.0,                    !- Design Loop Exit Temperature {C}",
		"    11.1,                    !- Loop Design Temperature Difference {deltaC}",
		"    Coincident,              !- Sizing Option",
		"    2,                       !- Zone Timesteps in Averaging Window",
		"    None;                    !- Coincident Sizing Factor Mode    ",

		"  PlantLoop,",
		"    HeatSys1 Loop,             !- Name",
		"    WATER,                   !- Fluid Type",
		"    ,                        !- User Defined Fluid Type",
		"    HeatSys1 Loop Operation,   !- Plant Equipment Operation Scheme Name",
		"    HeatSys1 Supply Outlet Node,  !- Loop Temperature Setpoint Node Name",
		"    100,                     !- Maximum Loop Temperature {C}",
		"    3,                       !- Minimum Loop Temperature {C}",
		"    0.003,                   !- Maximum Loop Flow Rate {m3/s}",
		"    0,                       !- Minimum Loop Flow Rate {m3/s}",
		"    autocalculate,           !- Plant Loop Volume {m3}",
		"    HeatSys1 Supply Inlet Node,!- Plant Side Inlet Node Name",
		"    HeatSys1 Supply Outlet Node,  !- Plant Side Outlet Node Name",
		"    HeatSys1 Supply Branches,  !- Plant Side Branch List Name",
		"    HeatSys1 Supply Connectors,!- Plant Side Connector List Name",
		"    HeatSys1 Demand Inlet Node,!- Demand Side Inlet Node Name",
		"    HeatSys1 Demand Outlet Node,  !- Demand Side Outlet Node Name",
		"    HeatSys1 Demand Branches,  !- Demand Side Branch List Name",
		"    HeatSys1 Demand Connectors,!- Demand Side Connector List Name",
		"    OPTIMAL;                 !- Load Distribution Scheme",

		"  SetpointManager:Scheduled,",
		"    HeatSys1 Loop Setpoint Manager,  !- Name",
		"    Temperature,             !- Control Variable",
		"    HeatSys1 Loop Temp Sch,    !- Schedule Name",
		"    HeatSys1 Loop Setpoint Node List;  !- Setpoint Node or NodeList Name",

		"  Schedule:Compact,",
		"    HeatSys1 Loop Temp Sch,   !- Name",
		"    Temperature,             !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,60.0;       !- Field 3",

		"  NodeList,",
		"    HeatSys1 Loop Setpoint Node List,  !- Name",
		"    HeatSys1 Supply Outlet Node;  !- Node 1 Name",

		"  PlantEquipmentOperationSchemes,",
		"    HeatSys1 Loop Operation,   !- Name",
		"    PlantEquipmentOperation:HeatingLoad,  !- Control Scheme 1 Object Type",
		"    HeatSys1 Purchased Only,   !- Control Scheme 1 Name",
		"    Always_On;        !- Control Scheme 1 Schedule Name",

		"  PlantEquipmentOperation:HeatingLoad,",
		"    HeatSys1 Purchased Only,   !- Name",
		"    0,                       !- Load Range 1 Lower Limit {W}",
		"    10000000,                !- Load Range 1 Upper Limit {W}",
		"    HeatSys1 Heating Plant;    !- Range 1 Equipment List Name",

		"  PlantEquipmentList,",
		"    HeatSys1 Heating Plant,    !- Name",
		"    DistrictHeating,         !- Equipment 1 Object Type",
		"    HeatSys1 Purchased Heating;!- Equipment 1 Name",

		"  BranchList,",
		"    HeatSys1 Supply Branches,  !- Name",
		"    HeatSys1 Supply Inlet Branch,  !- Branch 1 Name",
		"    HeatSys1 Heating Branch,   !- Branch 2 Name",
		"    HeatSys1 Supply Outlet Branch;  !- Branch 3 Name",

		"  ConnectorList,",
		"    HeatSys1 Supply Connectors,!- Name",
		"    Connector:Splitter,      !- Connector 1 Object Type",
		"    HeatSys1 Supply Splitter,  !- Connector 1 Name",
		"    Connector:Mixer,         !- Connector 2 Object Type",
		"    HeatSys1 Supply Mixer;     !- Connector 2 Name",

		"  Connector:Splitter,",
		"    HeatSys1 Supply Splitter,  !- Name",
		"    HeatSys1 Supply Inlet Branch,  !- Inlet Branch Name",
		"    HeatSys1 Heating Branch;   !- Outlet Branch 1 Name",

		"  Connector:Mixer,",
		"    HeatSys1 Supply Mixer,     !- Name",
		"    HeatSys1 Supply Outlet Branch,  !- Outlet Branch Name",
		"    HeatSys1 Heating Branch;   !- Inlet Branch 1 Name",

		"  Branch,",
		"    HeatSys1 Supply Inlet Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pump:VariableSpeed,      !- Component 1 Object Type",
		"    HeatSys1 Pump,             !- Component 1 Name",
		"    HeatSys1 Supply Inlet Node,!- Component 1 Inlet Node Name",
		"    HeatSys1 Supply Pump-Heating Node,  !- Component 1 Outlet Node Name",
		"    ;                        !- Component 1 Branch Control Type",

		"  Pump:VariableSpeed,",
		"    HeatSys1 Pump,             !- Name",
		"    HeatSys1 Supply Inlet Node,!- Inlet Node Name",
		"    HeatSys1 Supply Pump-Heating Node,  !- Outlet Node Name",
		"    0.005,                   !- Rated Flow Rate {m3/s}",
		"    300000,                  !- Rated Pump Head {Pa}",
		"    2250,                    !- Rated Power Consumption {W}",
		"    0.87,                    !- Motor Efficiency",
		"    0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"    0,                       !- Coefficient 1 of the Part Load Performance Curve",
		"    1,                       !- Coefficient 2 of the Part Load Performance Curve",
		"    0,                       !- Coefficient 3 of the Part Load Performance Curve",
		"    0,                       !- Coefficient 4 of the Part Load Performance Curve",
		"    0,                       !- Minimum Flow Rate {m3/s}",
		"    INTERMITTENT;            !- Pump Control Type",

		"  Branch,",
		"    HeatSys1 Heating Branch,   !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    DistrictHeating,         !- Component 1 Object Type",
		"    HeatSys1 Purchased Heating,!- Component 1 Name",
		"    HeatSys1 Supply Heating Inlet Node,  !- Component 1 Inlet Node Name",
		"    HeatSys1 Supply Heating Outlet Node,  !- Component 1 Outlet Node Name",
		"    ;                        !- Component 1 Branch Control Type",

		"  DistrictHeating,",
		"    HeatSys1 Purchased Heating,!- Name",
		"    HeatSys1 Supply Heating Inlet Node,  !- Hot Water Inlet Node Name",
		"    HeatSys1 Supply Heating Outlet Node,  !- Hot Water Outlet Node Name",
		"    1000000;                 !- Nominal Capacity {W}",

		"  Branch,",
		"    HeatSys1 Supply Outlet Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    HeatSys1 Supply Outlet Pipe,  !- Component 1 Name",
		"    HeatSys1 Supply Heating-Pipe Node,  !- Component 1 Inlet Node Name",
		"    HeatSys1 Supply Outlet Node,  !- Component 1 Outlet Node Name",
		"    ;                        !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    HeatSys1 Supply Outlet Pipe,  !- Name",
		"    HeatSys1 Supply Heating-Pipe Node,  !- Inlet Node Name",
		"    HeatSys1 Supply Outlet Node;  !- Outlet Node Name",

		"  Pipe:Adiabatic,",
		"    HeatSys1 Demand Bypass Pipe,  !- Name",
		"    HeatSys1 Demand Bypass Pipe Inlet Node,  !- Inlet Node Name",
		"    HeatSys1 Demand Bypass Pipe Outlet Node;  !- Outlet Node Name",

		"  ConnectorList,",
		"    HeatSys1 Demand Connectors,!- Name",
		"    Connector:Splitter,      !- Connector 1 Object Type",
		"    HeatSys1 Demand Splitter,  !- Connector 1 Name",
		"    Connector:Mixer,         !- Connector 2 Object Type",
		"    HeatSys1 Demand Mixer;     !- Connector 2 Name",

		"  Branch,",
		"    HeatSys1 Demand Bypass Branch,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    HeatSys1 Demand Bypass Pipe,  !- Component 1 Name",
		"    HeatSys1 Demand Bypass Pipe Inlet Node,  !- Component 1 Inlet Node Name",
		"    HeatSys1 Demand Bypass Pipe Outlet Node,  !- Component 1 Outlet Node Name",
		"    Bypass;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    HeatSys1 Demand Inlet Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    HeatSys1 Demand Inlet Pipe,!- Component 1 Name",
		"    HeatSys1 Demand Inlet Node,!- Component 1 Inlet Node Name",
		"    HeatSys1 Demand Pipe-Load Profile Node,  !- Component 1 Outlet Node Name",
		"    ;                        !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    HeatSys1 Demand Inlet Pipe,!- Name",
		"    HeatSys1 Demand Inlet Node,!- Inlet Node Name",
		"    HeatSys1 Demand Pipe-Load Profile Node;  !- Outlet Node Name",


		"  Branch,",
		"    HeatSys1 Demand Outlet Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    HeatSys1 Demand Outlet Pipe,  !- Component 1 Name",
		"    HeatSys1 Demand Load Profile-Pipe Node,  !- Component 1 Inlet Node Name",
		"    HeatSys1 Demand Outlet Node,  !- Component 1 Outlet Node Name",
		"    PASSIVE;                 !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    HeatSys1 Demand Outlet Pipe,  !- Name",
		"    HeatSys1 Demand Load Profile-Pipe Node,  !- Inlet Node Name",
		"    HeatSys1 Demand Outlet Node;  !- Outlet Node Name",

		"  Connector:Splitter,",
		"    HeatSys1 Demand Splitter,  !- Name",
		"    HeatSys1 Demand Inlet Branch,  !- Inlet Branch Name",
		"    HeatSys1 Demand Load Branch 1,  !- Branch 2 Name",
		"    HeatSys1 Demand Load Branch 2,  !- Branch 3 Name",
		"    HeatSys1 Demand Bypass Branch;  !- Branch 4 Name",

		"  Connector:Mixer,",
		"    HeatSys1 Demand Mixer,     !- Name",
		"    HeatSys1 Demand Outlet Branch,  !- Outlet Branch Name",
		"    HeatSys1 Demand Load Branch 1,  !- Branch 2 Name",
		"    HeatSys1 Demand Load Branch 2,  !- Branch 3 Name",
		"    HeatSys1 Demand Bypass Branch;  !- Branch 4 Name",

		"  BranchList,",
		"    HeatSys1 Demand Branches,!- Name",
		"    HeatSys1 Demand Inlet Branch,  !- Branch 1 Name",
		"    HeatSys1 Demand Load Branch 1,  !- Branch 2 Name",
		"    HeatSys1 Demand Load Branch 2,  !- Branch 3 Name",
		"    HeatSys1 Demand Bypass Branch,  !- Branch 4 Name",
		"    HeatSys1 Demand Outlet Branch;  !- Branch 5 Name",

		"  Branch,",
		"    HeatSys1 Demand Load Branch 1,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Coil:Heating:Water,      !- Component 1 Object Type",
		"    CV_1_HeatC,             !- Component 1 Name",
		"    CV_1_HeatCDemand Inlet Node,  !- Component 1 Inlet Node Name",
		"    CV_1_heatCDemand Outlet Node,  !- Component 1 Outlet Node Name",
		"    Active;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    HeatSys1 Demand Load Branch 2,  !- Name",
		"    ,                        !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam,      !- Component 1 Object Type",
		"    Zone One 4pipe Beam,  !- Component 1 Name",
		"    Zone One 4pipe Beam HW Inlet Node,  !- Component 1 Inlet Node Name",
		"    Zone One 4pipe Beam HW Outlet Node,  !- Component 1 Outlet Node Name",
		"    Active;                  !- Component 1 Branch Control Type",

		"  AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam,",
		"    Zone One 4pipe Beam, !- Name",
		"    ALWAYS_ON , !- Primary Air Availability Schedule Name",
		"    ALWAYS_ON , !- Cooling Availability Schedule Name",
		"    ALWAYS_ON , !- Heating Availability Schedule Name",
		"    Zone One 4pipe Beam Inlet Node Name , !- Primary Air Inlet Node Name",
		"    Zone One 4pipe Beam Outlet Node Name , !- Primary Air Outlet Node Name",
		"    Zone One 4pipe Beam CW Inlet Node , !- Chilled Water Inlet Node Name",
		"    Zone One 4pipe Beam CW Outlet Node , !- Chilled Water Outlet Node Name",
		"    Zone One 4pipe Beam HW Inlet Node , !- Hot Water Inlet Node Name",
		"    Zone One 4pipe Beam HW Outlet Node, !- Hot Water Outlet Node Name",
		"    AUTOSIZE , !- Design Primary Air Volume Flow Rate",
		"    AUTOSIZE , !- Design Chilled Water Volume Flow Rate",
		"    AUTOSIZE , !- Design Hot Water Volume Flow Rate",
		"    AUTOSIZE , !- Zone Total Beam Length",
		"    0.036 , !- Rated Primary Air Flow Rate per Meter",
		"    597 , !- Rated Beam Cooling Capacity per Meter",
		"    10.0 , !- Rated Cooling Room Air Chilled Water Temperature Difference",
		"    5.2E-5 , !- Rated Chilled Water Volume Flow Rate per Meter",
		"    CapModFuncOfTempDiff, !- Beam Cooling Capacity Temperature Difference Modification Factor Curve or Table Name",
		"    CoolCapModFuncOfSAFlow, !- Beam Cooling Capacity Air Flow Modification Factor Curve or Table Name",
		"    CapModFuncOfWaterFlow, !- Beam Cooling Capacity Chilled Water Flow Modification Factor Curve or Table Name",
		"    1548 , !- Rated Beam Heating Capacity per Meter",
		"    27.8, !- Rated Heating Room Air Hot Water Temperature Difference",
		"    5.2E-5, !- Rated Hot Water Volume Flow Rate per Meter",
		"    CapModFuncOfTempDiff, !- Beam Heating Capacity Temperature Difference Modification Factor Curve or Table Name",
		"    HeatCapModFuncOfSAFlow, !- Beam Heating Capacity Air Flow Modification Factor Curve or Table Name",
		"    CapModFuncOfWaterFlow; !- Beam Heating Capacity Hot Water Flow Modification Factor Curve or Table Name",

		"  Curve:Linear,  ! y = x",
		"    CapModFuncOfTempDiff, !-Name",
		"    0, !_ Coef Const",
		"    1, !- Coef x",
		"    0,  !- min x",
		"    1.5, !- max x",
		"    0.0 , !- min y",
		"    1.5; ! max y",

		"  Table:OneIndependentVariable,",
		"    CoolCapModFuncOfSAFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.714,!- min x",
		"    1.2857,!- max x",
		"    0.8234,!- min y",
		"    1.1256,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"    , !- normalization ref",
		"    0.714286, 0.823403,",
		"    1.0,      1.0,",
		"    1.2857,   1.1256;",

		"  Table:OneIndependentVariable,",
		"    CapModFuncOfWaterFlow, !- Name",
		"    quadratic,!- Curve ",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.0,!- min x",
		"    1.333333,!- max x",
		"    0.0,!- min y",
		"    1.04,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
 		"     , !- normalization ref",
		"    0.0,      0.0,",
		"    0.05,     0.001,",
		"    0.33333,  0.71,",
		"    0.5,      0.85,",
		"    0.666667, 0.92,",
		"    0.833333, 0.97,",
		"    1.0,      1.0,",
		"    1.333333, 1.04;",

		"  Table:OneIndependentVariable,",
		"    HeatCapModFuncOfSAFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.714,!- min x",
		"    1.2857,!- max x",
		"    0.8554,!- min y",
		"    1.0778,!- max y",
 		"    dimensionless, !-",
		"    dimensionless, !- ",
 		"   , !- normalization ref",
		"    0.714286, 0.8554,",
		"    1.0,      1.0,",
		"    1.2857,   1.0778; ",
				} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		SimulationManager::PostIPProcessing();

		bool ErrorsFound =  false;

		DataGlobals::BeginSimFlag = true;
		SimulationManager::GetProjectData();

		OutputReportPredefined::SetPredefinedTables();
		HeatBalanceManager::SetPreConstructionInputParameters(); //establish array bounds for constructions early
		OutputProcessor::TimeValue.allocate( 2 );
		OutputProcessor::SetupTimePointers( "Zone", DataGlobals::TimeStepZone ); // Set up Time pointer for HB/Zone Simulation
		OutputProcessor::SetupTimePointers( "HVAC", DataHVACGlobals::TimeStepSys );
		PlantManager::CheckIfAnyPlant();
		createFacilityElectricPowerServiceObject();
		BranchInputManager::ManageBranchInput(); // just gets input and returns.
		DataGlobals::DoingSizing = true;
		SizingManager::ManageSizing();
		DataGlobals::DoingSizing = false;
		DataGlobals::KickOffSimulation = true;

		WeatherManager::ResetEnvironmentCounter();
		SimulationManager::SetupSimulation( ErrorsFound );
		DataGlobals::KickOffSimulation = false;

		DataHVACGlobals::SimZoneEquipmentFlag = true;
		DataHVACGlobals::SimNonZoneEquipmentFlag = false;
		DataHVACGlobals::SimAirLoopsFlag = true;
		DataHVACGlobals::SimPlantLoopsFlag = true;
		DataHVACGlobals::SimElecCircuitsFlag = false;
		bool FirstHVACIteration = true;

		//PlantManager::InitializeLoops( FirstHVACIteration );
		PlantUtilities::SetAllFlowLocks( DataPlant::FlowUnlocked );
		//first run with a sensible cooling load of 5000 W and cold supply air
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputRequired = -5000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = -4000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = -5000.0;

		DataLoopNode::Node( 14 ).Temp = 14.0; // chilled water inlet node
		DataLoopNode::Node( 40 ).HumRat = 0.008; // zone node
		DataLoopNode::Node( 40 ).Temp = 24.0; // zone node
		DataLoopNode::Node( 44 ).HumRat = 0.008; // primary air inlet node
		DataLoopNode::Node( 44 ).Temp = 12.8; // primare air inlet node
		DataLoopNode::Node( 38 ).Temp = 45.0; // hot water inlet node
		Real64 NonAirSysOutput = 0.0;
		DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->simulate(FirstHVACIteration, NonAirSysOutput);

		EXPECT_NEAR( DataLoopNode::Node( 1 ).MassFlowRate, 0.3521952339035046, 0.00001 );
		EXPECT_NEAR( DataLoopNode::Node( 15 ).Temp, 19.191523455437512, 0.00001 );
		EXPECT_NEAR( DataLoopNode::Node( 15 ).MassFlowRate, 0.046199561631265804, 0.00001 );
		EXPECT_DOUBLE_EQ( DataLoopNode::Node( 39 ).Temp, 45.0 );
		EXPECT_DOUBLE_EQ( DataLoopNode::Node( 39 ).MassFlowRate, 0.0 );

		EXPECT_NEAR( NonAirSysOutput, -1004.0437766383318, 0.0001 );

		//next run with a sensible heating load of 5000 W and cold supply air
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputRequired =  5000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 5000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = 6000.0;

		DataLoopNode::Node( 40 ).Temp = 21.0; // zone node
		DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->simulate(FirstHVACIteration, NonAirSysOutput);

		EXPECT_DOUBLE_EQ( DataLoopNode::Node( 15 ).Temp, 14.0 );
		EXPECT_DOUBLE_EQ( DataLoopNode::Node( 15 ).MassFlowRate, 0.0 );
		EXPECT_NEAR( DataLoopNode::Node( 39 ).Temp, 35.064466069323743, 0.00001 );
		EXPECT_NEAR( DataLoopNode::Node( 39 ).MassFlowRate, 0.19320550334974979, 0.00001 );

		EXPECT_NEAR( NonAirSysOutput, 8023.9273066417645, 0.0001);


		// next run with cooling load and neutral supply air
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputRequired = -5000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = -4000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = -5000.0;

		DataLoopNode::Node( 14 ).Temp = 14.0; // chilled water inlet node
		DataLoopNode::Node( 40 ).HumRat = 0.008; // zone node
		DataLoopNode::Node( 40 ).Temp = 24.0; // zone node
		DataLoopNode::Node( 44 ).HumRat = 0.008; // primary air inlet node
		DataLoopNode::Node( 44 ).Temp = 22.0; // primary air inlet node
		DataLoopNode::Node( 38 ).Temp = 45.0; // hot water inlet node

		NonAirSysOutput = 0.0;
		DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->simulate(FirstHVACIteration, NonAirSysOutput);

		EXPECT_NEAR( DataLoopNode::Node( 15 ).Temp, 18.027306264618733, 0.00001 );
		EXPECT_NEAR( DataLoopNode::Node( 15 ).MassFlowRate, 0.25614844309380103, 0.00001);
		EXPECT_DOUBLE_EQ( DataLoopNode::Node( 39 ).Temp, 45.0 );
		EXPECT_DOUBLE_EQ( DataLoopNode::Node( 39 ).MassFlowRate, 0.0 );

		EXPECT_NEAR( NonAirSysOutput, -4318.4346465170929, 0.0001 );

		// next run with heating load and neutral supply air
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputRequired =  5000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 5000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = 6000.0;

		DataLoopNode::Node( 40 ).Temp = 21.0; // zone node

		NonAirSysOutput = 0.0;
		DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->simulate(FirstHVACIteration, NonAirSysOutput);

		EXPECT_DOUBLE_EQ( DataLoopNode::Node( 15 ).Temp, 14.0);
		EXPECT_DOUBLE_EQ( DataLoopNode::Node( 15 ).MassFlowRate, 0.0);
		EXPECT_NEAR( DataLoopNode::Node( 39 ).Temp, 33.836239364981424, 0.00001 );
		EXPECT_NEAR( DataLoopNode::Node( 39 ).MassFlowRate, 0.10040605035467959, 0.00001 );

		EXPECT_NEAR( NonAirSysOutput, 4685.4000901131676, 0.0001 );


	}


}
