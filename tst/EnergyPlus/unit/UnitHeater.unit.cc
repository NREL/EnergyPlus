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

// EnergyPlus::UnitHeater Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/UnitHeater.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace DataGlobals;
using namespace EnergyPlus::UnitHeater;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataSurfaceLists;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::FluidProperties;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::OutputProcessor;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SimulationManager;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::SurfaceGeometry;
using namespace EnergyPlus::WaterCoils;


TEST_F( EnergyPlusFixture, UnitHeater_HWHeatingCoilUAAutoSizingTest ) {

	bool ErrorsFound( false ); // function returns true on error
	bool FirstHVACIteration( true ); // TRUE if 1st HVAC simulation of system timestep
	int UnitHeatNum( 1 ); // unit heat index
	int ZoneNum( 1 ); // zone index
	int CoilNum( 1 ); // heating coil index
	int PltSizHeatNum( 0 ); // hot water plant loop index
	Real64 HWMaxVolFlowRate( 0.0 ); // hot water coil water volume flow rate, m3/s
	Real64 CpHW( 0.0 ); // hot water specific heat, J/kg K
	Real64 HWPlantDeltaTDesign( 0.0 ); // hot water plant loop design temp difference, deltaC
	Real64 HWCoilDesignCapacity( 0.0 ); // hot water heating coil design capacity (W)
	Real64 HWDensity( 0.0 ); // hot water density, kg/m3

	std::string const idf_objects = delimited_string( {
		"  Version,8.4;",

		"  Timestep,4;",

		"  Building,",
		"    NONE,                    !- Name",
		"    0.0000000E+00,           !- North Axis {deg}",
		"    Suburbs,                 !- Terrain",
		"    3.9999999E-02,           !- Loads Convergence Tolerance Value",
		"    0.4000000,               !- Temperature Convergence Tolerance Value {deltaC}",
		"    MinimalShadowing,        !- Solar Distribution",
		"    25,                      !- Maximum Number of Warmup Days",
		"    6;                       !- Minimum Number of Warmup Days",

		"  HeatBalanceAlgorithm,ConductionTransferFunction;",

		"  SurfaceConvectionAlgorithm:Inside,TARP;",

		"  SurfaceConvectionAlgorithm:Outside,DOE-2;",

		"  RunPeriod,",
		"    ,                        !- Name",
		"    1,                       !- Begin Month",
		"    14,                      !- Begin Day of Month",
		"    1,                       !- End Month",
		"    14,                      !- End Day of Month",
		"    Tuesday,                 !- Day of Week for Start Day",
		"    Yes,                     !- Use Weather File Holidays and Special Days",
		"    Yes,                     !- Use Weather File Daylight Saving Period",
		"    No,                      !- Apply Weekend Holiday Rule",
		"    Yes,                     !- Use Weather File Rain Indicators",
		"    Yes;                     !- Use Weather File Snow Indicators",

		"  Site:Location,",
		"    CHICAGO_IL_USA TMY2-94846,  !- Name",
		"    41.78,                   !- Latitude {deg}",
		"    -87.75,                  !- Longitude {deg}",
		"    -6.00,                   !- Time Zone {hr}",
		"    190.00;                  !- Elevation {m}",

		"  SimulationControl,",
		"    Yes,                     !- Do Zone Sizing Calculation",
		"    No,                      !- Do System Sizing Calculation",
		"    No,                      !- Do Plant Sizing Calculation",
		"    No,                      !- Run Simulation for Sizing Periods",
		"    No;                      !- Run Simulation for Weather File Run Periods",

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
		"    1.0;                     !- Sky Clearness",

		"  SizingPeriod:DesignDay,",
		"    CHICAGO_IL_USA Annual Cooling 1% Design Conditions DB/MCWB,  !- Name",
		"    7,                       !- Month",
		"    21,                      !- Day of Month",
		"    SummerDesignDay,         !- Day Type",
		"    31.5,                    !- Maximum Dry-Bulb Temperature {C}",
		"    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Type",
		"    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
		"    Wetbulb,                 !- Humidity Condition Type",
		"    23.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
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
		"    1.0;                     !- Sky Clearness",

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

		"  WindowMaterial:Glazing,",
		"    WIN-LAY-GLASS-LIGHT,     !- Name",
		"    SpectralAverage,         !- Optical Data Type",
		"    ,                        !- Window Glass Spectral Data Set Name",
		"    0.003,                   !- Thickness {m}",
		"    0.90,                    !- Solar Transmittance at Normal Incidence",
		"    0.031,                   !- Front Side Solar Reflectance at Normal Incidence",
		"    0.031,                   !- Back Side Solar Reflectance at Normal Incidence",
		"    0.90,                    !- Visible Transmittance at Normal Incidence",
		"    0.05,                    !- Front Side Visible Reflectance at Normal Incidence",
		"    0.05,                    !- Back Side Visible Reflectance at Normal Incidence",
		"    0.0,                     !- Infrared Transmittance at Normal Incidence",
		"    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
		"    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
		"    0.9;                     !- Conductivity {W/m-K}",

		"  Construction,",
		"    EXTWALL80,               !- Name",
		"    A1 - 1 IN STUCCO,        !- Outside Layer",
		"    C4 - 4 IN COMMON BRICK,  !- Layer 2",
		"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

		"  Construction,",
		"    PARTITION06,             !- Name",
		"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
		"    C6 - 8 IN CLAY TILE,     !- Layer 2",
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

		"  Construction,",
		"    WIN-CON-LIGHT,           !- Name",
		"    WIN-LAY-GLASS-LIGHT;     !- Outside Layer",

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

		"  ScheduleTypeLimits,",
		"    On/Off,                  !- Name",
		"    0,                       !- Lower Limit Value",
		"    1,                       !- Upper Limit Value",
		"    DISCRETE;                !- Numeric Type",

		"  Schedule:Compact,",
		"    Activity Sch,            !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Alldays,            !- Field 2",
		"    Until: 24:00,131.80;     !- Field 3",

		"  Schedule:Compact,",
		"    Work Eff Sch,            !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Alldays,            !- Field 2",
		"    Until: 24:00,0.00;       !- Field 3",

		"  Schedule:Compact,",
		"    Clothing Sch,            !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Alldays,            !- Field 2",
		"    Until: 24:00,1.00;       !- Field 3",

		"  Schedule:Compact,",
		"    Air Velo Sch,            !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Alldays,            !- Field 2",
		"    Until: 24:00,0.137;      !- Field 3",

		"  Schedule:Compact,",
		"    Office Occupancy,        !- Name",
		"    ANY NUMBER,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays,           !- Field 2",
		"    Until: 6:00,0.00,        !- Field 3",
		"    Until: 7:00,0.10,        !- Field 5",
		"    Until: 8:00,0.50,        !- Field 7",
		"    Until: 12:00,1.00,       !- Field 9",
		"    Until: 13:00,0.50,       !- Field 11",
		"    Until: 16:00,1.00,       !- Field 13",
		"    Until: 17:00,0.50,       !- Field 15",
		"    Until: 18:00,0.10,       !- Field 17",
		"    Until: 24:00,0.00,       !- Field 19",
		"    For: Weekends Holidays CustomDay1 CustomDay2, !- Field 21",
		"    Until: 24:00,0.00,       !- Field 22",
		"    For: SummerDesignDay,    !- Field 24",
		"    Until: 24:00,1.00,       !- Field 25",
		"    For: WinterDesignDay,    !- Field 27",
		"    Until: 24:00,5.00E-002;  !- Field 28",

		"  Schedule:Compact,",
		"    Intermittent,            !- Name",
		"    ANY NUMBER,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays,           !- Field 2",
		"    Until: 8:00,0.00,        !- Field 3",
		"    Until: 18:00,1.00,       !- Field 5",
		"    Until: 24:00,0.00,       !- Field 7",
		"    For: Weekends Holidays CustomDay1 CustomDay2, !- Field 9",
		"    Until: 24:00,0.00,       !- Field 10",
		"    For: SummerDesignDay,    !- Field 12",
		"    Until: 24:00,1.00,       !- Field 13",
		"    For: WinterDesignDay,    !- Field 15",
		"    Until: 24:00,0.15;       !- Field 16",

		"  Schedule:Compact,",
		"    Office Lighting,         !- Name",
		"    ANY NUMBER,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays,           !- Field 2",
		"    Until: 6:00,5.00E-002,   !- Field 3",
		"    Until: 7:00,0.20,        !- Field 5",
		"    Until: 17:00,1.00,       !- Field 7",
		"    Until: 18:00,0.50,       !- Field 9",
		"    Until: 24:00,5.00E-002,  !- Field 11",
		"    For: Weekends Holidays CustomDay1 CustomDay2, !- Field 13",
		"    Until: 24:00,5.00E-002,  !- Field 14",
		"    For: SummerDesignDay,    !- Field 16",
		"    Until: 24:00,1.00,       !- Field 17",
		"    For: WinterDesignDay,    !- Field 19",
		"    Until: 24:00,0.15;       !- Field 20",

		"  Schedule:Compact,",
		"    ON,                      !- Name",
		"    Fraction,                !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Alldays,            !- Field 2",
		"    Until: 24:00,1.00;       !- Field 3",

		"  Schedule:Compact,",
		"    HW LOOP TEMP SCHEDULE,   !- Name",
		"    TEMPERATURE,             !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Alldays,            !- Field 2",
		"    Until: 24:00,82.00;      !- Field 3",

		"  Schedule:Compact,",
		"    FANANDCOILAVAILSCHED,    !- Name",
		"    FRACTION,                !- Schedule Type Limits Name",
		"    Through: 3/31,           !- Field 1",
		"    For: Alldays,            !- Field 2",
		"    Until: 24:00,1.00,       !- Field 3",
		"    Through: 9/30,           !- Field 5",
		"    For: Weekdays,           !- Field 6",
		"    Until: 7:00,0.00,        !- Field 7",
		"    Until: 17:00,1.00,       !- Field 9",
		"    Until: 24:00,0.00,       !- Field 11",
		"    For: Weekends Holidays CustomDay1 CustomDay2, !- Field 13",
		"    Until: 24:00,0.00,       !- Field 14",
		"    For: SummerDesignDay WinterDesignDay, !- Field 16",
		"    Until: 24:00,1.00,       !- Field 17",
		"    Through: 12/31,          !- Field 19",
		"    For: Alldays,            !- Field 20",
		"    Until: 24:00,1.00;       !- Field 21",

		"  Schedule:Compact,",
		"    HEATING SETPOINTS,       !- Name",
		"    TEMPERATURE,             !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays Weekends Holidays CustomDay1 CustomDay2, !- Field 2",
		"    Until: 7:00,15.00,       !- Field 3",
		"    Until: 17:00,20.00,      !- Field 5",
		"    Until: 24:00,15.00,      !- Field 7",
		"    For: SummerDesignDay,    !- Field 9",
		"    Until: 24:00,15.00,      !- Field 10",
		"    For: WinterDesignDay,    !- Field 12",
		"    Until: 24:00,20.00;      !- Field 13",

		"  Schedule:Compact,",
		"    COOLING SETPOINTS,       !- Name",
		"    TEMPERATURE,             !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Weekdays Weekends Holidays CustomDay1 CustomDay2, !- Field 2",
		"    Until: 7:00,30.00,       !- Field 3",
		"    Until: 17:00,24.00,      !- Field 5",
		"    Until: 24:00,30.00,      !- Field 7",
		"    For: SummerDesignDay,    !- Field 9",
		"    Until: 24:00,24.00,      !- Field 10",
		"    For: WinterDesignDay,    !- Field 12",
		"    Until: 24:00,50.00;      !- Field 13",

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
		"    UNITHEATAVAILABILITY,    !- Name",
		"    ANY NUMBER,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Alldays,            !- Field 2",
		"    Until: 24:00,1.00;       !- Field 3",

		"  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

		"  Zone,",
		"    EAST ZONE,               !- Name",
		"    0.0000000E+00,           !- Direction of Relative North {deg}",
		"    0.0000000E+00,           !- X Origin {m}",
		"    0.0000000E+00,           !- Y Origin {m}",
		"    0.0000000E+00,           !- Z Origin {m}",
		"    1,                       !- Type",
		"    1,                       !- Multiplier",
		"    autocalculate,           !- Ceiling Height {m}",
		"    autocalculate;           !- Volume {m3}",

		"  People,",
		"    EAST ZONE,               !- Name",
		"    EAST ZONE,               !- Zone or ZoneList Name",
		"    Office Occupancy,        !- Number of People Schedule Name",
		"    people,                  !- Number of People Calculation Method",
		"    3.000000,                !- Number of People",
		"    ,                        !- People per Zone Floor Area {person/m2}",
		"    ,                        !- Zone Floor Area per Person {m2/person}",
		"    0.3000000,               !- Fraction Radiant",
		"    ,                        !- Sensible Heat Fraction",
		"    Activity Sch,            !- Activity Level Schedule Name",
		"    3.82E-8,                 !- Carbon Dioxide Generation Rate {m3/s-W}",
		"    ,                        !- Enable ASHRAE 55 Comfort Warnings",
		"    zoneaveraged,            !- Mean Radiant Temperature Calculation Type",
		"    ,                        !- Surface Name/Angle Factor List Name",
		"    Work Eff Sch,            !- Work Efficiency Schedule Name",
		"    ClothingInsulationSchedule,  !- Clothing Insulation Calculation Method",
		"    ,                        !- Clothing Insulation Calculation Method Schedule Name",
		"    Clothing Sch,            !- Clothing Insulation Schedule Name",
		"    Air Velo Sch,            !- Air Velocity Schedule Name",
		"    FANGER;                  !- Thermal Comfort Model 1 Type",

		"  Lights,",
		"    EAST ZONE Lights 1,      !- Name",
		"    EAST ZONE,               !- Zone or ZoneList Name",
		"    Office Lighting,         !- Schedule Name",
		"    LightingLevel,           !- Design Level Calculation Method",
		"    1464.375,                !- Lighting Level {W}",
		"    ,                        !- Watts per Zone Floor Area {W/m2}",
		"    ,                        !- Watts per Person {W/person}",
		"    0.0000000E+00,           !- Return Air Fraction",
		"    0.2000000,               !- Fraction Radiant",
		"    0.2000000,               !- Fraction Visible",
		"    0.0000000E+00,           !- Fraction Replaceable",
		"    GeneralLights;           !- End-Use Subcategory",

		"  ElectricEquipment,",
		"    EAST ZONE ElecEq 1,      !- Name",
		"    EAST ZONE,               !- Zone or ZoneList Name",
		"    Intermittent,            !- Schedule Name",
		"    EquipmentLevel,          !- Design Level Calculation Method",
		"    1464.375,                !- Design Level {W}",
		"    ,                        !- Watts per Zone Floor Area {W/m2}",
		"    ,                        !- Watts per Person {W/person}",
		"    0.0000000E+00,           !- Fraction Latent",
		"    0.3000000,               !- Fraction Radiant",
		"    0.0000000E+00;           !- Fraction Lost",

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
		"    12.19200,6.096000,0.0000000E+00,  !- X,Y,Z ==> Vertex 2 {m}",
		"    9.144000,6.096000,0.0000000E+00,  !- X,Y,Z ==> Vertex 3 {m}",
		"    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

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
		"    6.096000,0.0000000E+00,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    6.096000,0.0000000E+00,0.0000000E+00,  !- X,Y,Z ==> Vertex 2 {m}",
		"    12.19200,0.0000000E+00,0.0000000E+00,  !- X,Y,Z ==> Vertex 3 {m}",
		"    12.19200,0.0000000E+00,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

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
		"    12.19200,0.0000000E+00,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    12.19200,0.0000000E+00,0.0000000E+00,  !- X,Y,Z ==> Vertex 2 {m}",
		"    12.19200,6.096000,0.0000000E+00,  !- X,Y,Z ==> Vertex 3 {m}",
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
		"    6.096000,6.096000,0.0000000E+00,  !- X,Y,Z ==> Vertex 2 {m}",
		"    6.096000,0.0000000E+00,0.0000000E+00,  !- X,Y,Z ==> Vertex 3 {m}",
		"    6.096000,0.0000000E+00,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn002:Wall005,           !- Name",
		"    Wall,                    !- Surface Type",
		"    EXTWALL80,               !- Construction Name",
		"    EAST ZONE,               !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    9.144000,6.096000,0.0000000E+00,  !- X,Y,Z ==> Vertex 2 {m}",
		"    6.096000,6.096000,0.0000000E+00,  !- X,Y,Z ==> Vertex 3 {m}",
		"    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn002:Flr001,            !- Name",
		"    Floor,                   !- Surface Type",
		"    FLOOR SLAB 8 IN,         !- Construction Name",
		"    EAST ZONE,               !- Zone Name",
		"    Ground,                  !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    NoSun,                   !- Sun Exposure",
		"    NoWind,                  !- Wind Exposure",
		"    1.000000,                !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    6.096000,0.0000000E+00,0.0000000E+00,  !- X,Y,Z ==> Vertex 1 {m}",
		"    6.096000,6.096000,0.0000000E+00,  !- X,Y,Z ==> Vertex 2 {m}",
		"    12.19200,6.096000,0.0000000E+00,  !- X,Y,Z ==> Vertex 3 {m}",
		"    12.19200,0.0000000E+00,0.0000000E+00;  !- X,Y,Z ==> Vertex 4 {m}",

		"  BuildingSurface:Detailed,",
		"    Zn002:Roof001,           !- Name",
		"    Roof,                    !- Surface Type",
		"    ROOF34,                  !- Construction Name",
		"    EAST ZONE,               !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.0000000E+00,           !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    6.096000,0.0000000E+00,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
		"    12.19200,0.0000000E+00,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
		"    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

		"  Sizing:Parameters,",
		"    1.5,                     !- Heating Sizing Factor",
		"    1.5,                     !- Cooling Sizing Factor",
		"    ;                        !- Timesteps in Averaging Window",

		"  ZoneInfiltration:DesignFlowRate,",
		"    EAST ZONE Infiltration,  !- Name",
		"    EAST ZONE,               !- Zone or ZoneList Name",
		"    UNITHEATAVAILABILITY,    !- Schedule Name",
		"    Flow/Zone,               !- Design Flow Rate Calculation Method",
		"    0.00275772902001411,     !- Design Flow Rate {m3/s}",
		"    ,                        !- Flow per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Flow per Exterior Surface Area {m3/s-m2}",
		"    ,                        !- Air Changes per Hour {1/hr}",
		"    ,                        !- Constant Term Coefficient",
		"    ,                        !- Temperature Term Coefficient",
		"    0.224051539,             !- Velocity Term Coefficient",
		"    ;                        !- Velocity Squared Term Coefficient",

		"  Sizing:Zone,",
		"    EAST ZONE,               !- Zone or ZoneList Name",
		"    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
		"    12.78,                     !- Zone Cooling Design Supply Air Temperature {C}",
		"    ,                        !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
		"    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
		"    32.22,                     !- Zone Heating Design Supply Air Temperature {C}",
		"    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
		"    0.009,                   !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    0.009,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    SZ DSOA EAST ZONE,       !- Design Specification Outdoor Air Object Name",
		"    ,                        !- Zone Heating Sizing Factor",
		"    ,                        !- Zone Cooling Sizing Factor",
		"    DesignDay,               !- Cooling Design Air Flow Method",
		"    0,                       !- Cooling Design Air Flow Rate {m3/s}",
		"    0.000762,                !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
		"    0,                       !- Cooling Minimum Air Flow {m3/s}",
		"    0,                       !- Cooling Minimum Air Flow Fraction",
		"    DesignDay,               !- Heating Design Air Flow Method",
		"    0,                       !- Heating Design Air Flow Rate {m3/s}",
		"    0.002032,                !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
		"    0.1415762,               !- Heating Maximum Air Flow {m3/s}",
		"    0.3,                     !- Heating Maximum Air Flow Fraction",
		"    SZ DSOA EAST ZONE Design Spec Zone Air Dist;  !- Design Specification Zone Air Distribution Object Name",

		"  DesignSpecification:ZoneAirDistribution,",
		"    SZ DSOA EAST ZONE Design Spec Zone Air Dist,  !- Name",
		"    1,                       !- Zone Air Distribution Effectiveness in Cooling Mode {dimensionless}",
		"    1;                       !- Zone Air Distribution Effectiveness in Heating Mode {dimensionless}",

		"  DesignSpecification:OutdoorAir,",
		"    SZ DSOA EAST ZONE,       !- Name",
		"    Sum,                     !- Outdoor Air Method",
		"    0.00780950959206505,     !- Outdoor Air Flow per Person {m3/s-person}",
		"    0.012032,                !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
		"    0,                       !- Outdoor Air Flow per Zone {m3/s}",
		"    0;                       !- Outdoor Air Flow Air Changes per Hour {1/hr}",

		"  Sizing:Plant,",
		"    Hot Water Loop,          !- Plant or Condenser Loop Name",
		"    Heating,                 !- Loop Type",
		"    82.2222222222222,        !- Design Loop Exit Temperature {C}",
		"    22.2222222222222;        !- Loop Design Temperature Difference {deltaC}",

		"  PlantLoop,",
		"    Hot Water Loop,          !- Name",
		"    Water,                   !- Fluid Type",
		"    ,                        !- User Defined Fluid Type",
		"    Hot Loop Operation,      !- Plant Equipment Operation Scheme Name",
		"    HW Supply Outlet Node,   !- Loop Temperature Setpoint Node Name",
		"    100,                     !- Maximum Loop Temperature {C}",
		"    10,                      !- Minimum Loop Temperature {C}",
		"    Autosize,                !- Maximum Loop Flow Rate {m3/s}",
		"    0.0,                     !- Minimum Loop Flow Rate {m3/s}",
		"    autocalculate,           !- Plant Loop Volume {m3}",
		"    HW Supply Inlet Node,    !- Plant Side Inlet Node Name",
		"    HW Supply Outlet Node,   !- Plant Side Outlet Node Name",
		"    Heating Supply Side Branches,  !- Plant Side Branch List Name",
		"    Heating Supply Side Connectors,  !- Plant Side Connector List Name",
		"    HW Demand Inlet Node,    !- Demand Side Inlet Node Name",
		"    HW Demand Outlet Node,   !- Demand Side Outlet Node Name",
		"    Heating Demand Side Branches,  !- Demand Side Branch List Name",
		"    Heating Demand Side Connectors,  !- Demand Side Connector List Name",
		"    Optimal;                 !- Load Distribution Scheme",

		"  SetpointManager:Scheduled,",
		"    Hot Water Loop Setpoint Manager,  !- Name",
		"    Temperature,             !- Control Variable",
		"    HW Loop Temp Schedule,   !- Schedule Name",
		"    Hot Water Loop Setpoint Node List;  !- Setpoint Node or NodeList Name",

		"  NodeList,",
		"    Hot Water Loop Setpoint Node List,  !- Name",
		"    HW Supply Outlet Node;   !- Node 1 Name",

		"  BranchList,",
		"    Heating Supply Side Branches,  !- Name",
		"    Heating Supply Inlet Branch,  !- Branch 1 Name",
		"    Heating Purchased Hot Water Branch,  !- Branch 2 Name",
		"    Heating Supply Bypass Branch,  !- Branch 3 Name",
		"    Heating Supply Outlet Branch;  !- Branch 4 Name",

		"  ConnectorList,",
		"    Heating Supply Side Connectors,  !- Name",
		"    Connector:Splitter,      !- Connector 1 Object Type",
		"    Heating Supply Splitter, !- Connector 1 Name",
		"    Connector:Mixer,         !- Connector 2 Object Type",
		"    Heating Supply Mixer;    !- Connector 2 Name",

		"  Connector:Splitter,",
		"    Heating Supply Splitter, !- Name",
		"    Heating Supply Inlet Branch,  !- Inlet Branch Name",
		"    Heating Purchased Hot Water Branch,  !- Outlet Branch 1 Name",
		"    Heating Supply Bypass Branch;  !- Outlet Branch 2 Name",

		"  Connector:Mixer,",
		"    Heating Supply Mixer,    !- Name",
		"    Heating Supply Outlet Branch,  !- Outlet Branch Name",
		"    Heating Purchased Hot Water Branch,  !- Inlet Branch 1 Name",
		"    Heating Supply Bypass Branch;  !- Inlet Branch 2 Name",

		"  Branch,",
		"    Heating Supply Inlet Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pump:VariableSpeed,      !- Component 1 Object Type",
		"    HW Circ Pump,            !- Component 1 Name",
		"    HW Supply Inlet Node,    !- Component 1 Inlet Node Name",
		"    HW Pump Outlet Node,     !- Component 1 Outlet Node Name",
		"    ACTIVE;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    Heating Purchased Hot Water Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    DistrictHeating,         !- Component 1 Object Type",
		"    Purchased Heating,       !- Component 1 Name",
		"    Purchased Heat Inlet Node,  !- Component 1 Inlet Node Name",
		"    Purchased Heat Outlet Node,  !- Component 1 Outlet Node Name",
		"    ACTIVE;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    Heating Supply Bypass Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    Heating Supply Side Bypass,  !- Component 1 Name",
		"    Heating Supply Bypass Inlet Node,  !- Component 1 Inlet Node Name",
		"    Heating Supply Bypass Outlet Node,  !- Component 1 Outlet Node Name",
		"    BYPASS;                  !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    Heating Supply Side Bypass,  !- Name",
		"    Heating Supply Bypass Inlet Node,  !- Inlet Node Name",
		"    Heating Supply Bypass Outlet Node;  !- Outlet Node Name",

		"  Branch,",
		"    Heating Supply Outlet Branch,  !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    Heating Supply Outlet,   !- Component 1 Name",
		"    Heating Supply Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
		"    HW Supply Outlet Node,   !- Component 1 Outlet Node Name",
		"    PASSIVE;                 !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    Heating Supply Outlet,   !- Name",
		"    Heating Supply Exit Pipe Inlet Node,  !- Inlet Node Name",
		"    HW Supply Outlet Node;   !- Outlet Node Name",

		"  Pump:VariableSpeed,",
		"    HW Circ Pump,            !- Name",
		"    HW Supply Inlet Node,    !- Inlet Node Name",
		"    HW Pump Outlet Node,     !- Outlet Node Name",
		"    autosize,                !- Rated Flow Rate {m3/s}",
		"    179352,                  !- Rated Pump Head {Pa}",
		"    autosize,                !- Rated Power Consumption {W}",
		"    0.9,                     !- Motor Efficiency",
		"    0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"    0,                       !- Coefficient 1 of the Part Load Performance Curve",
		"    1,                       !- Coefficient 2 of the Part Load Performance Curve",
		"    0,                       !- Coefficient 3 of the Part Load Performance Curve",
		"    0,                       !- Coefficient 4 of the Part Load Performance Curve",
		"    0,                       !- Minimum Flow Rate {m3/s}",
		"    INTERMITTENT;            !- Pump Control Type",

		"  BranchList,",
		"    Heating Demand Side Branches,  !- Name",
		"    ZonesHWInletBranch,      !- Branch 1 Name",
		"    Zone2HWBranch,           !- Branch 3 Name",
		"    ZonesHWBypassBranch,     !- Branch 5 Name",
		"    ZonesHWOutletBranch;     !- Branch 6 Name",

		"  ConnectorList,",
		"    Heating Demand Side Connectors,  !- Name",
		"    Connector:Splitter,      !- Connector 1 Object Type",
		"    Zones HW Splitter,       !- Connector 1 Name",
		"    Connector:Mixer,         !- Connector 2 Object Type",
		"    Zones HW Mixer;          !- Connector 2 Name",

		"  Connector:Splitter,",
		"    Zones HW Splitter,       !- Name",
		"    ZonesHWInletBranch,      !- Inlet Branch Name",
		"    Zone2HWBranch,           !- Outlet Branch 2 Name",
		"    ZonesHWBypassBranch;     !- Outlet Branch 4 Name",

		"  Connector:Mixer,",
		"    Zones HW Mixer,          !- Name",
		"    ZonesHWOutletBranch,     !- Outlet Branch Name",
		"    Zone2HWBranch,           !- Inlet Branch 2 Name",
		"    ZonesHWBypassBranch;     !- Inlet Branch 4 Name",

		"  Branch,",
		"    ZonesHWInletBranch,      !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    ZonesHWInletPipe,        !- Component 1 Name",
		"    HW Demand Inlet Node,    !- Component 1 Inlet Node Name",
		"    HW Demand Entrance Pipe Outlet Node,  !- Component 1 Outlet Node Name",
		"    PASSIVE;                 !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    ZonesHWInletPipe,        !- Name",
		"    HW Demand Inlet Node,    !- Inlet Node Name",
		"    HW Demand Entrance Pipe Outlet Node;  !- Outlet Node Name",

		"  Branch,",
		"    ZonesHWOutletBranch,     !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    ZonesHWOutletPipe,       !- Component 1 Name",
		"    HW Demand Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name",
		"    HW Demand Outlet Node,   !- Component 1 Outlet Node Name",
		"    PASSIVE;                 !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    ZonesHWOutletPipe,       !- Name",
		"    HW Demand Exit Pipe Inlet Node,  !- Inlet Node Name",
		"    HW Demand Outlet Node;   !- Outlet Node Name",

		"  Branch,",
		"    Zone2HWBranch,           !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Coil:Heating:Water,      !- Component 1 Object Type",
		"    Zone2UnitHeatHeatingCoil,!- Component 1 Name",
		"    Zone2UnitHeatHWInletNode,!- Component 1 Inlet Node Name",
		"    Zone2UnitHeatHWOutletNode,  !- Component 1 Outlet Node Name",
		"    ACTIVE;                  !- Component 1 Branch Control Type",

		"  Branch,",
		"    ZonesHWBypassBranch,     !- Name",
		"    0,                       !- Maximum Flow Rate {m3/s}",
		"    ,                        !- Pressure Drop Curve Name",
		"    Pipe:Adiabatic,          !- Component 1 Object Type",
		"    ZonesHWBypassPipe,       !- Component 1 Name",
		"    ZonesHWBypassInletNode,  !- Component 1 Inlet Node Name",
		"    ZonesHWBypassOutletNode, !- Component 1 Outlet Node Name",
		"    BYPASS;                  !- Component 1 Branch Control Type",

		"  Pipe:Adiabatic,",
		"    ZonesHWBypassPipe,       !- Name",
		"    ZonesHWBypassInletNode,  !- Inlet Node Name",
		"    ZonesHWBypassOutletNode; !- Outlet Node Name",

		"  PlantEquipmentOperationSchemes,",
		"    Hot Loop Operation,      !- Name",
		"    PlantEquipmentOperation:HeatingLoad,  !- Control Scheme 1 Object Type",
		"    Purchased Heating Only,  !- Control Scheme 1 Name",
		"    ON;                      !- Control Scheme 1 Schedule Name",

		"  PlantEquipmentOperation:HeatingLoad,",
		"    Purchased Heating Only,  !- Name",
		"    0,                       !- Load Range 1 Lower Limit {W}",
		"    1000000,                 !- Load Range 1 Upper Limit {W}",
		"    heating plant;           !- Range 1 Equipment List Name",

		"  PlantEquipmentList,",
		"    heating plant,           !- Name",
		"    DistrictHeating,         !- Equipment 1 Object Type",
		"    Purchased Heating;       !- Equipment 1 Name",

		"  DistrictHeating,",
		"    Purchased Heating,       !- Name",
		"    Purchased Heat Inlet Node,  !- Hot Water Inlet Node Name",
		"    Purchased Heat Outlet Node,  !- Hot Water Outlet Node Name",
		"    1000000;                 !- Nominal Capacity {W}",

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

		"  ZoneHVAC:EquipmentConnections,",
		"    EAST ZONE,               !- Zone Name",
		"    Zone2Equipment,          !- Zone Conditioning Equipment List Name",
		"    Zone2Inlets,             !- Zone Air Inlet Node or NodeList Name",
		"    Zone2Exhausts,           !- Zone Air Exhaust Node or NodeList Name",
		"    Zone 2 Node,             !- Zone Air Node Name",
		"    Zone 2 Outlet Node;      !- Zone Return Air Node Name",

		"  ZoneHVAC:EquipmentList,",
		"    Zone2Equipment,          !- Name",
		"    ZoneHVAC:UnitHeater,     !- Zone Equipment 1 Object Type",
		"    Zone2UnitHeat,           !- Zone Equipment 1 Name",
		"    1,                       !- Zone Equipment 1 Cooling Sequence",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

		"  NodeList,",
		"    Zone2Inlets,             !- Name",
		"    Zone2UnitHeatAirOutletNode;  !- Node 1 Name",

		"  NodeList,",
		"    Zone2Exhausts,           !- Name",
		"    Zone2UnitHeatAirInletNode;  !- Node 1 Name",

		"  ZoneHVAC:UnitHeater,",
		"    Zone2UnitHeat,           !- Name",
		"    UnitHeatAvailability,    !- Availability Schedule Name",
		"    Zone2UnitHeatAirInletNode,  !- Air Inlet Node Name",
		"    Zone2UnitHeatAirOutletNode,  !- Air Outlet Node Name",
		"    Fan:ConstantVolume,      !- Supply Air Fan Object Type",
		"    Zone2UnitHeatFan,        !- Supply Air Fan Name",
		"    autosize,                !- Maximum Supply Air Flow Rate {m3/s}",
		"    Coil:Heating:Water,      !- Heating Coil Object Type",
		"    Zone2UnitHeatHeatingCoil,!- Heating Coil Name",
		"    ,                        !- Supply Air Fan Operating Mode Schedule Name",
		"    No,                      !- Supply Air Fan Operation During No Heating",
		"    autosize,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
		"    0,                       !- Minimum Hot Water or Steam Flow Rate {m3/s}",
		"    0.001;                   !- Heating Convergence Tolerance",

		"  Fan:ConstantVolume,",
		"    Zone2UnitHeatFan,        !- Name",
		"    UnitHeatAvailability,    !- Availability Schedule Name",
		"    0.5,                     !- Fan Total Efficiency",
		"    75.0,                    !- Pressure Rise {Pa}",
		"    autosize,                !- Maximum Flow Rate {m3/s}",
		"    0.9,                     !- Motor Efficiency",
		"    1.0,                     !- Motor In Airstream Fraction",
		"    Zone2UnitHeatAirInletNode,  !- Air Inlet Node Name",
		"    Zone2UnitHeatFanOutletNode;  !- Air Outlet Node Name",

		"  Coil:Heating:Water,",
		"    Zone2UnitHeatHeatingCoil,!- Name",
		"    FanAndCoilAvailSched,    !- Availability Schedule Name",
		"    autosize,                !- U-Factor Times Area Value {W/K}",
		"    autosize,                !- Maximum Water Flow Rate {m3/s}",
		"    Zone2UnitHeatHWInletNode,!- Water Inlet Node Name",
		"    Zone2UnitHeatHWOutletNode,  !- Water Outlet Node Name",
		"    Zone2UnitHeatFanOutletNode,  !- Air Inlet Node Name",
		"    Zone2UnitHeatAirOutletNode,  !- Air Outlet Node Name",
		"    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
		"    autosize,                !- Rated Capacity {W}",
		"    82.2,                    !- Rated Inlet Water Temperature {C}",
		"    16.6,                    !- Rated Inlet Air Temperature {C}",
		"    71.1,                    !- Rated Outlet Water Temperature {C}",
		"    32.2,                    !- Rated Outlet Air Temperature {C}",
		"    ;                        !- Rated Ratio for Air and Water Convection",
	} );
		ASSERT_FALSE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 4; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 15; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedule data

		ErrorsFound = false;
		HeatBalanceManager::GetProjectControlData( ErrorsFound ); // read project control data
		EXPECT_FALSE( ErrorsFound );

		OutputProcessor::TimeValue.allocate( 2 );
		DataGlobals::DDOnlySimulation = true;

		GetProjectData();
		OutputReportPredefined::SetPredefinedTables();
		SetPreConstructionInputParameters(); //establish array bounds for constructions early

		BeginSimFlag = true;
		BeginEnvrnFlag = true;
		ZoneSizingCalc = true;
		createFacilityElectricPowerServiceObject();
		SizingManager::ManageSizing();

		ErrorsFound = false;
		GetWaterCoilInput();
		EXPECT_FALSE( ErrorsFound );
		GetWaterCoilsInputFlag = false;

		ErrorsFound = false;
		GetFanInput();
		EXPECT_FALSE( ErrorsFound );

		ErrorsFound = false;
		GetUnitHeaterInput(); // get unit heaters data
		EXPECT_FALSE( ErrorsFound );
		EXPECT_EQ( 1, NumOfUnitHeats );
		EXPECT_EQ( "ZONE2UNITHEAT", UnitHeat( 1 ).Name );

		ErrorsFound = false;
		ZoneEqUnitHeater = true;
		DataSizing::CurZoneEqNum = 1;

		InitUnitHeater( UnitHeatNum, ZoneNum, FirstHVACIteration );
		InitWaterCoil( CoilNum, FirstHVACIteration ); // init hot water heating coil
		
		PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Water", UnitHeat( UnitHeatNum ).HCoilName, WaterCoils::WaterCoil( CoilNum ).WaterInletNodeNum, WaterCoils::WaterCoil( CoilNum ).WaterOutletNodeNum, ErrorsFound );
		EXPECT_FALSE( ErrorsFound );

		HWMaxVolFlowRate = WaterCoils::WaterCoil( CoilNum ).MaxWaterVolFlowRate;
		HWDensity = GetDensityGlycol( PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidName, DataGlobals::InitConvTemp, PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidIndex, "xxx" );
		CpHW = GetSpecificHeatGlycol( PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidIndex, "xxx" );
		HWPlantDeltaTDesign = PlantSizData( PltSizHeatNum ).DeltaT;
		// calculate hot water coil design capacity
		HWCoilDesignCapacity = HWMaxVolFlowRate * HWDensity * CpHW * HWPlantDeltaTDesign;
		EXPECT_NEAR( HWCoilDesignCapacity, WaterCoils::WaterCoil( CoilNum ).DesWaterHeatingCoilRate, 1.0 );
		EXPECT_NEAR( 111.02, WaterCoils::WaterCoil( CoilNum ).UACoil, 0.02 );

}

TEST_F( EnergyPlusFixture, UnitHeater_SimUnitHeaterTest ) {

	bool ErrorsFound( false ); // function returns true on error
	bool FirstHVACIteration( true ); // TRUE if 1st HVAC simulation of system timestep

	int UnitHeatNum( 1 ); // unit heat index
	int ZoneNum( 1 ); // zone index
	int CoilNum( 1 ); // heating coil index
	int WCWaterInletNode( 0 ); // water coil water inlet node
	int WCWaterOutletNode( 0 ); // water coil water outlet node
	int UHAirInletNode( 0 ); // unit heater air inlet node
	int UHAirOutletNode( 0 ); // unit heater air outlet node

	Real64 CpHW( 0.0 ); // hot water specific heat, J/kg K
	Real64 SysOutputProvided( 0.0 ); // heating rate delivered
	Real64 LatOutputProvided( 0.0 ); // latent heating rate delivered
	Real64 ConvTol( 1.0 ); // convergence tolerance
	Real64 HWMassFlowRate( 0.0 ); // hot water coil water mass flow rate, m3/s
	Real64 UHAirMassFlowRate( 0.0 ); // unit heate air mass flow rate 
	Real64 UHHeatingRate( 0.0 ); // unit heate heating rate
	Real64 UHEnteringAirEnthalpy( 0.0 ); // unit heater entering air enthalpy
	Real64 UHLeavingAirEnthalpy( 0.0 ); // unit heater leaving air enthalpy
	Real64 HWCoilHeatingRate( 0.0 ); // hot water heating coil heating rate

	std::string const idf_objects = delimited_string( {
		"  Version,8.4;",

		"  ScheduleTypeLimits,",
		"    Any Number;              !- Name",

		"  Schedule:Compact,",
		"    UNITHEATAVAILABILITY,    !- Name",
		"    ANY NUMBER,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: Alldays,            !- Field 2",
		"    Until: 24:00,1.00;       !- Field 3",

		"  Zone,",
		"    EAST ZONE,               !- Name",
		"    0.0000000E+00,           !- Direction of Relative North {deg}",
		"    0.0000000E+00,           !- X Origin {m}",
		"    0.0000000E+00,           !- Y Origin {m}",
		"    0.0000000E+00,           !- Z Origin {m}",
		"    1,                       !- Type",
		"    1,                       !- Multiplier",
		"    autocalculate,           !- Ceiling Height {m}",
		"    autocalculate;           !- Volume {m3}",

		"  ZoneHVAC:EquipmentConnections,",
		"    EAST ZONE,               !- Zone Name",
		"    Zone2Equipment,          !- Zone Conditioning Equipment List Name",
		"    Zone2Inlets,             !- Zone Air Inlet Node or NodeList Name",
		"    Zone2Exhausts,           !- Zone Air Exhaust Node or NodeList Name",
		"    Zone 2 Node,             !- Zone Air Node Name",
		"    Zone 2 Outlet Node;      !- Zone Return Air Node Name",

		"  ZoneHVAC:EquipmentList,",
		"    Zone2Equipment,          !- Name",
		"    ZoneHVAC:UnitHeater,     !- Zone Equipment 1 Object Type",
		"    Zone2UnitHeat,           !- Zone Equipment 1 Name",
		"    1,                       !- Zone Equipment 1 Cooling Sequence",
		"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

		"  NodeList,",
		"    Zone2Inlets,             !- Name",
		"    Zone2UnitHeatAirOutletNode;  !- Node 1 Name",

		"  NodeList,",
		"    Zone2Exhausts,           !- Name",
		"    Zone2UnitHeatAirInletNode;  !- Node 1 Name",

		"  ZoneHVAC:UnitHeater,",
		"    Zone2UnitHeat,           !- Name",
		"    UnitHeatAvailability,    !- Availability Schedule Name",
		"    Zone2UnitHeatAirInletNode,  !- Air Inlet Node Name",
		"    Zone2UnitHeatAirOutletNode, !- Air Outlet Node Name",
		"    Fan:ConstantVolume,      !- Supply Air Fan Object Type",
		"    Zone2UnitHeatFan,        !- Supply Air Fan Name",
		"    0.47055,                 !- Maximum Supply Air Flow Rate {m3/s}",
		"    Coil:Heating:Water,      !- Heating Coil Object Type",
		"    Zone2UnitHeatHeatingCoil,!- Heating Coil Name",
		"    ,                        !- Supply Air Fan Operating Mode Schedule Name",
		"    No,                      !- Supply Air Fan Operation During No Heating",
		"    7.25956E-005,            !- Maximum Hot Water or Steam Flow Rate {m3/s}",
		"    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
		"    0.001;                   !- Heating Convergence Tolerance",

		"  Fan:ConstantVolume,",
		"    Zone2UnitHeatFan,        !- Name",
		"    UnitHeatAvailability,    !- Availability Schedule Name",
		"    0.5,                     !- Fan Total Efficiency",
		"    75.0,                    !- Pressure Rise {Pa}",
		"    0.47055,                 !- Maximum Flow Rate {m3/s}",
		"    0.9,                     !- Motor Efficiency",
		"    1.0,                     !- Motor In Airstream Fraction",
		"    Zone2UnitHeatAirInletNode,  !- Air Inlet Node Name",
		"    Zone2UnitHeatFanOutletNode;  !- Air Outlet Node Name",

		"  Coil:Heating:Water,",
		"    Zone2UnitHeatHeatingCoil,!- Name",
		"    UnitHeatAvailability,    !- Availability Schedule Name",
		"    84.93609,                !- U-Factor Times Area Value {W/K}",
		"    7.25956E-005,            !- Maximum Water Flow Rate {m3/s}",
		"    Zone2UnitHeatHWInletNode,!- Water Inlet Node Name",
		"    Zone2UnitHeatHWOutletNode,  !- Water Outlet Node Name",
		"    Zone2UnitHeatFanOutletNode, !- Air Inlet Node Name",
		"    Zone2UnitHeatAirOutletNode, !- Air Outlet Node Name",
		"    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
		"    6750.70521,              !- Rated Capacity {W}",
		"    82.2,                    !- Rated Inlet Water Temperature {C}",
		"    16.6,                    !- Rated Inlet Air Temperature {C}",
		"    71.1,                    !- Rated Outlet Water Temperature {C}",
		"    32.2,                    !- Rated Outlet Air Temperature {C}",
		"    ;                        !- Rated Ratio for Air and Water Convection",
	} );
	ASSERT_FALSE( process_idf( idf_objects ) );

	NumOfTimeStepInHour = 4; // must initialize this to get schedules initialized
	MinutesPerTimeStep = 15; // must initialize this to get schedules initialized
	ProcessScheduleInput(); // read schedule data

	ErrorsFound = false;
	HeatBalanceManager::GetZoneData( ErrorsFound ); // read zone data
	EXPECT_FALSE( ErrorsFound );

	ErrorsFound = false;
	GetZoneEquipmentData();
	EXPECT_FALSE( ErrorsFound );

	ErrorsFound = false;
	GetWaterCoilInput();
	EXPECT_FALSE( ErrorsFound );
	GetWaterCoilsInputFlag = false;

	ErrorsFound = false;
	GetFanInput();
	EXPECT_FALSE( ErrorsFound );

	ErrorsFound = false;
	GetUnitHeaterInput(); // get unit heaters data
	EXPECT_FALSE( ErrorsFound );
	EXPECT_EQ( 1, NumOfUnitHeats );
	EXPECT_EQ( "ZONE2UNITHEAT", UnitHeat( 1 ).Name );

	ErrorsFound = false;
	ZoneEqUnitHeater = true;
	DataSizing::CurZoneEqNum = 1;

	TotNumLoops = 1;
	PlantLoop.allocate( TotNumLoops );

	for ( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 2 );
		auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}

	PlantLoop( 1 ).Name = "HotWaterLoop";
	PlantLoop( 1 ).FluidName = "HotWater";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoils::WaterCoil( 1 ).Name;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoils::WaterCoil_SimpleHeating;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoils::WaterCoil( 1 ).WaterInletNodeNum;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = WaterCoils::WaterCoil( 1 ).WaterOutletNodeNum;

	ZoneSysEnergyDemand.allocate( 1 );
	ZoneSysEnergyDemand( 1 ).RemainingOutputRequired = 2000.0;

	ZoneSizingRunDone = true;
	ZoneEqSizing.allocate( 1 );
	ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
	DataGlobals::DoingSizing = true;

	ZoneCompTurnFansOn = true;
	ZoneCompTurnFansOff = false;

	Schedule( 1 ).CurrentValue = 1;
	CurDeadBandOrSetback.allocate( 1 );
	CurDeadBandOrSetback( ZoneNum ) = false;

	Node( Fan( 1 ).InletNodeNum ).Temp = 16.6;
	Node( Fan( 1 ).InletNodeNum ).HumRat = PsyWFnTdbRhPb( 16.6, 0.5, 101325.0, "UnitTest" );
	Node( Fan( 1 ).InletNodeNum ).Enthalpy = Psychrometrics::PsyHFnTdbW( Node( Fan( 1 ).InletNodeNum ).Temp, Node( Fan( 1 ).InletNodeNum ).HumRat );
	DataEnvironment::StdRhoAir = PsyRhoAirFnPbTdbW( 101325.0, 20.0, 0.0 ); 

	WCWaterInletNode = WaterCoils::WaterCoil( CoilNum ).WaterInletNodeNum;
	WCWaterOutletNode = WaterCoils::WaterCoil( CoilNum ).WaterOutletNodeNum;
	Node( WCWaterInletNode ).Temp = 60.0;

	DataGlobals::BeginEnvrnFlag =  true;
	SysSizingCalc = true;

	UHAirInletNode = UnitHeat( UnitHeatNum ).AirInNode;
	UHAirOutletNode = UnitHeat( UnitHeatNum ).AirOutNode;

	SimUnitHeater( UnitHeat( UnitHeatNum ).Name, ZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, CurZoneEqNum );
	// SimUnitHeater does not converge on the first call: the unit heater deliveres more than required heating load. But it meets 
	// on the second call (iteration). I suspect it may be an initialization issue related to ControlCompOutput routine
	SimUnitHeater( UnitHeat( UnitHeatNum ).Name, ZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, CurZoneEqNum );
	// verify the total heat rate deleivered by the unit heater
	UHAirMassFlowRate = Node( UHAirInletNode ).MassFlowRate;
	UHEnteringAirEnthalpy = PsyHFnTdbW( Node( UHAirInletNode ).Temp, Node( UHAirInletNode ).HumRat );
	UHLeavingAirEnthalpy = PsyHFnTdbW( Node( UHAirOutletNode ).Temp, Node( UHAirOutletNode ).HumRat );	
	UHHeatingRate = UHAirMassFlowRate * ( UHLeavingAirEnthalpy - UHEnteringAirEnthalpy );
	EXPECT_NEAR( UHHeatingRate, UnitHeat( UnitHeatNum ).HeatPower, ConvTol );
	// verify the heat rate delivered by the hot water heating coil 
	HWMassFlowRate = WaterCoils::WaterCoil( CoilNum ).InletWaterMassFlowRate;
	CpHW = GetSpecificHeatGlycol( PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidIndex, "UnitTest" );
	HWCoilHeatingRate = HWMassFlowRate * CpHW * ( Node( WCWaterInletNode ).Temp - Node( WCWaterOutletNode ).Temp );
	EXPECT_NEAR( HWCoilHeatingRate, WaterCoils::WaterCoil( CoilNum ).TotWaterHeatingCoilRate, ConvTol );

}
