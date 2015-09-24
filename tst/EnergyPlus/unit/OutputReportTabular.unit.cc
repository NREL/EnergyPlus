// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/HVACFixture.hh"

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::OutputReportTabular;
using namespace SimulationManager;
using namespace ObjexxFCL;

TEST( OutputReportTabularTest, ConfirmSetUnitsStyleFromString )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, ConfirmSetUnitsStyleFromString" );

	EXPECT_EQ( unitsStyleNone, SetUnitsStyleFromString( "None" ) );
	EXPECT_EQ( unitsStyleJtoKWH, SetUnitsStyleFromString( "JTOKWH" ) );
	EXPECT_EQ( unitsStyleJtoMJ, SetUnitsStyleFromString( "JTOMJ" ) );
	EXPECT_EQ( unitsStyleJtoGJ, SetUnitsStyleFromString( "JTOGJ" ) );
	EXPECT_EQ( unitsStyleInchPound, SetUnitsStyleFromString( "INCHPOUND" ) );
	EXPECT_EQ( unitsStyleNotFound, SetUnitsStyleFromString( "qqq" ) );
}

TEST( OutputReportTabularTest, Basic )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, Basic" );

	OutputTableBinned.allocate( 10 );
	EXPECT_TRUE( warningAboutKeyNotFound( 0, 1, "moduleName" ) );
	EXPECT_FALSE( warningAboutKeyNotFound( 100, 1, "moduleName") );
}

TEST( OutputReportTabularTest, RealToStr )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, RealToStr" );
	EXPECT_EQ( "       0.001", RealToStr( 0.0011, 3 ) );
	EXPECT_NE( "       0.001", RealToStr( 0.0019, 3 ) );

	EXPECT_EQ( "          1.", RealToStr( 1.23456789, 0 ) );
	EXPECT_EQ( "         1.2", RealToStr( 1.23456789, 1 ) );
	EXPECT_EQ( "        1.23", RealToStr( 1.23456789, 2 ) );
	EXPECT_EQ( "       1.235", RealToStr( 1.23456789, 3 ) );
	EXPECT_EQ( "      1.2346", RealToStr( 1.23456789, 4 ) );
	EXPECT_EQ( "     1.23457", RealToStr( 1.23456789, 5 ) );
	EXPECT_EQ( "    1.234568", RealToStr( 1.23456789, 6 ) );
	EXPECT_EQ( "   1.2345679", RealToStr( 1.23456789, 7 ) );
	EXPECT_EQ( "  1.23456789", RealToStr( 1.23456789, 8 ) );

	EXPECT_EQ( "    1.234000", RealToStr( 1.234, 6 ) );
	EXPECT_EQ( "   1.2340000", RealToStr( 1.234, 7 ) );
	EXPECT_EQ( "  1.23400000", RealToStr( 1.234, 8 ) );

	EXPECT_EQ( "     123457.", RealToStr( 123456.789, 0 ) );
	EXPECT_EQ( "    123456.8", RealToStr( 123456.789, 1 ) );
	EXPECT_EQ( "   123456.79", RealToStr( 123456.789, 2 ) );
	EXPECT_EQ( "  123456.789", RealToStr( 123456.789, 3 ) );
	EXPECT_EQ( " 123456.7890", RealToStr( 123456.789, 4 ) );

	EXPECT_EQ( "0.123457E+06", RealToStr( 123456.789, 5 ) );

}

TEST(OutputReportTabularTest, ConfirmResourceWarning)
{
	ShowMessage( "Begin Test: OutputReportTabularTest, ConfirmResourceWarning" );

	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Electricity [kWh]",
		ResourceWarningMessage("Electricity [kWh]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Natural Gas [kWh]",
		ResourceWarningMessage("Natural Gas [kWh]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Additional Fuel [kWh]",
		ResourceWarningMessage("Additional Fuel [kWh]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: District Cooling [kBtu]",
		ResourceWarningMessage("District Cooling [kBtu]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: District Heating [kBtu]",
		ResourceWarningMessage("District Heating [kBtu]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Water [GJ]",
		ResourceWarningMessage("Water [GJ]"));
	EXPECT_EQ("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Electricity [GJ]",
		ResourceWarningMessage("Electricity [GJ]"));
	EXPECT_NE("In the Annual Building Utility Performance Summary Report the total row does not match the sum of the column for: Gas [kWh]",
		ResourceWarningMessage("Electricity [kWh]"));
}

TEST(OutputReportTabularTest, ConfirmWaterConversion)
{
	ShowMessage("Begin Test: OutputReportTabularTest, ConfirmWaterConversion");

	EXPECT_EQ(15, WaterConversionFunct(75, 5));
	EXPECT_EQ(1, WaterConversionFunct(1, 1));
	EXPECT_EQ(13.756, WaterConversionFunct(481.46, 35));
	EXPECT_EQ(-2, WaterConversionFunct(-12, 6));
	EXPECT_NE(15, WaterConversionFunct(135, 5));

}

TEST( OutputReportTabularTest, GetUnitConversion )
{
	ShowMessage( "Begin Test: OutputReportTabularTest, GetUnitConversion" );

	int indexUnitConv;
	std::string curUnits;
	Real64 curConversionFactor;
	Real64 curConversionOffset;
	std::string varNameWithUnits;

	SetupUnitConversions();

	varNameWithUnits = "ZONE AIR SYSTEM SENSIBLE COOLING RATE[W]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 70, indexUnitConv );
	EXPECT_EQ( "ton", curUnits );
	EXPECT_EQ( 0.0002843333, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "SITE OUTDOOR AIR DRYBULB TEMPERATURE[C]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 11, indexUnitConv );
	EXPECT_EQ( "F", curUnits );
	EXPECT_EQ( 1.8, curConversionFactor );
	EXPECT_EQ( 32., curConversionOffset );

	varNameWithUnits = "ZONE ELECTRIC EQUIPMENT ELECTRIC ENERGY[J]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 20, indexUnitConv );
	EXPECT_EQ( "kWh", curUnits );
	EXPECT_EQ( 0.000000277778, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "ZONE COOLING SETPOINT NOT MET TIME[hr]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 17, indexUnitConv );
	EXPECT_EQ( "hr", curUnits );
	EXPECT_EQ( 1.0, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "ZONE LIGHTS TOTAL HEATING ENERGY[Invalid/Undefined]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 94, indexUnitConv );
	EXPECT_EQ( "Invalid/Undefined", curUnits );
	EXPECT_EQ( 1.0, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

	varNameWithUnits = "FICTIONAL VARIABLE[qqq]";
	LookupSItoIP( varNameWithUnits, indexUnitConv, curUnits );
	GetUnitConversion( indexUnitConv, curConversionFactor, curConversionOffset, curUnits );
	EXPECT_EQ( 0, indexUnitConv );
	EXPECT_EQ( "", curUnits );
	EXPECT_EQ( 0.0, curConversionFactor );
	EXPECT_EQ( 0.0, curConversionOffset );

}

TEST_F( HVACFixture, OutputReportTabular_ZoneSumTest )
{
	// AUTHOR: R. Raustad, FSEC
	// DATE WRITTEN: Sep 2015

	std::string const idf_objects = delimited_string( {
		"Version,8.3;",

		" Output:Diagnostics, DisplayExtraWarnings;",
		"  Timestep, 4;",

		"BUILDING, Bldg2, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",

		"SimulationControl, YES, NO, NO, YES, NO;",

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
		" Miami Intl Ap Ann Htg 1% Condns DB/MCWB, !- Name",
		" 2,                        !- Month",
		" 11,                       !- Day of Month",
		" WinterDesignDay,          !- Day Type",
		" 3.7,                     !- Maximum Dry - Bulb Temperature{ C }",
		" 0.0,                      !- Daily Dry - Bulb Temperature Range{ deltaC }",
		" ,                         !- Dry - Bulb Temperature Range Modifier Type",
		" ,                         !- Dry - Bulb Temperature Range Modifier Day Schedule Name",
		" Wetbulb,                  !- Humidity Condition Type",
		" 2.7,                      !- Wetbulb or DewPoint at Maximum Dry - Bulb{ C }",
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

		"OutputControl:Table:Style,",
		"  HTML;                    !- Column Separator",

		"Output:Table:SummaryReports,",
		"  AllSummary; !- Report 1 Name",

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

		"ZoneGroup,",
		" Zone Group,               !- Name",
		" Zone List,                !- Zone List Name",
		" 10;                       !- Zone List Multiplier",

		"ZoneList,",
		" Zone List,                !- Name",
		" Spacex10;                 !- Zone 1 Name",

		"Zone,",
		"  Spacex10,                !- Name",
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
		" ,                         !- Design Specification Outdoor Air Object Name",
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
		" Spacex10,                 !- Zone or ZoneList Name",
		" SupplyAirTemperature,     !- Zone Cooling Design Supply Air Temperature Input Method",
		" 12.,                      !- Zone Cooling Design Supply Air Temperature{ C }",
		" ,                         !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
		" SupplyAirTemperature,     !- Zone Heating Design Supply Air Temperature Input Method",
		" 50.,                      !- Zone Heating Design Supply Air Temperature{ C }",
		" ,                         !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
		" 0.008,                    !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
		" 0.008,                    !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
		" ,                         !- Design Specification Outdoor Air Object Name",
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
		" ",

		"People,",
		" Space People,             !- Name",
		" Space,                    !- Zone or ZoneList Name",
		" OnSched,                  !- Number of People Schedule Name",
		" people,                   !- Number of People Calculation Method",
		" 11,                       !- Number of People",
		" ,                         !- People per Zone Floor Area{ person / m2 }",
		" ,                         !- Zone Floor Area per Person{ m2 / person }",
		" 0.3,                      !- Fraction Radiant",
		" AutoCalculate,            !- Sensible Heat Fraction",
		" ActivityLevelSched;       !- Activity Level Schedule Name",

		"People,",
		" Spacex10 People,          !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
		" OnSched,                  !- Number of People Schedule Name",
		" people,                   !- Number of People Calculation Method",
		" 11,                       !- Number of People",
		" ,                         !- People per Zone Floor Area{ person / m2 }",
		" ,                         !- Zone Floor Area per Person{ m2 / person }",
		" 0.3,                      !- Fraction Radiant",
		" AutoCalculate,            !- Sensible Heat Fraction",
		" ActivityLevelSched;       !- Activity Level Schedule Name",

		"Lights,",
		" Space Lights,             !- Name",
		" Space,                    !- Zone or ZoneList Name",
		" OnSched,                  !- Schedule Name",
		" Watts/Area,               !- Design Level Calculation Method",
		" ,                         !- Lighting Level{ W }",
		" 10.0,                     !- Watts per Zone Floor Area{ W / m2 }",
		" ,                         !- Watts per Person{ W / person }",
		" 0.1,                      !- Return Air Fraction",
		" 0.59,                     !- Fraction Radiant",
		" 0.2,                      !- Fraction Visible",
		" 0,                        !- Fraction Replaceable",
		" GeneralLights;            !- End - Use Subcategory",

		"Lights,",
		" Space Lights x10,         !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
		" OnSched,                  !- Schedule Name",
		" Watts/Area,               !- Design Level Calculation Method",
		" ,                         !- Lighting Level{ W }",
		" 10.0,                     !- Watts per Zone Floor Area{ W / m2 }",
		" ,                         !- Watts per Person{ W / person }",
		" 0.1,                      !- Return Air Fraction",
		" 0.59,                     !- Fraction Radiant",
		" 0.2,                      !- Fraction Visible",
		" 0,                        !- Fraction Replaceable",
		" GeneralLights;            !- End - Use Subcategory",

		"ElectricEquipment,",
		" Space ElecEq,             !- Name",
		" Space,                    !- Zone or ZoneList Name",
		" OnSched,                  !- Schedule Name",
		" Watts/Area,               !- Design Level Calculation Method",
		" ,                         !- Design Level{ W }",
		" 20.0,                     !- Watts per Zone Floor Area{ W / m2 }",
		" ,                         !- Watts per Person{ W / person }",
		" 0.1,                      !- Fraction Latent",
		" 0.3,                      !- Fraction Radiant",
		" 0.1;                      !- Fraction Lost",

		"ElectricEquipment,",
		" Space ElecEq x10,         !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
		" OnSched,                  !- Schedule Name",
		" Watts/Area,               !- Design Level Calculation Method",
		" ,                         !- Design Level{ W }",
		" 20.0,                     !- Watts per Zone Floor Area{ W / m2 }",
		" ,                         !- Watts per Person{ W / person }",
		" 0.1,                      !- Fraction Latent",
		" 0.3,                      !- Fraction Radiant",
		" 0.1;                      !- Fraction Lost",

		"Schedule:Compact,",
		" OnSched,                  !- Name",
		" Fraction,                 !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0;        !- Field 26",

		"ScheduleTypeLimits,",
		" Fraction, !- Name",
		" 0.0, !- Lower Limit Value",
		" 1.0, !- Upper Limit Value",
		" CONTINUOUS;              !- Numeric Type",

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
		" FRONT-1,                  !- Name",
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
		" C1-1,                     !- Name",
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
		" F1-1,                     !- Name",
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
		" SB12,                     !- Name",
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
		" SB14,                     !- Name",
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
		" SB15,                     !- Name",
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
		" Spacex10 Thermostat,      !- Name",
		" Spacex10,                 !- Zone or ZoneList Name",
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
		" FRONT-1x10,               !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Spacex10,                 !- Zone Name",
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
		" C1-1x10,                  !- Name",
		" CEILING,                  !- Surface Type",
		" CLNG-1,                   !- Construction Name",
		" Spacex10,                 !- Zone Name",
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
		" F1-1x10,                  !- Name",
		" FLOOR,                    !- Surface Type",
		" FLOOR-SLAB-1,             !- Construction Name",
		" Spacex10,                 !- Zone Name",
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
		" SB12x10,                  !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Spacex10,                 !- Zone Name",
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
		" SB14x10,                  !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Spacex10,                 !- Zone Name",
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
		" SB15x10,                  !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Spacex10,                 !- Zone Name",
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

		"AirLoopHVAC,",
		"  DOAS,                    !- Name",
		"  DOAS Controllers,        !- Controller List Name",
		"  DOAS Availability Managers,  !- Availability Manager List Name",
		"  autosize,                !- Design Supply Air Flow Rate {m3/s}",
		"  DOAS Branches,           !- Branch List Name",
		"  ,                        !- Connector List Name",
		"  DOAS Air Loop Inlet,     !- Supply Side Inlet Node Name",
		"  DOAS Return Air Outlet,  !- Demand Side Outlet Node Name",
		"  DOAS Supply Path Inlet,  !- Demand Side Inlet Node Names",
		"  DOAS Supply Fan Outlet;  !- Supply Side Outlet Node Names",

		"AirLoopHVAC:ControllerList,",
		"  DOAS Controllers,        !- Name",
		"  Controller:WaterCoil,    !- Controller 1 Object Type",
		"  DOAS Cooling Coil Controller;  !- Controller 1 Name",

		"BranchList,",
		"  DOAS Branches,           !- Name",
		"  DOAS Main Branch;        !- Branch 1 Name",

		"Branch,",
		"  DOAS Main Branch,        !- Name",
		"  autosize,                !- Maximum Flow Rate {m3/s}",
		"  ,                        !- Pressure Drop Curve Name",
		"  AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
		"  DOAS OA System,          !- Component 1 Name",
		"  DOAS Air Loop Inlet,     !- Component 1 Inlet Node Name",
		"  DOAS Mixed Air Outlet,   !- Component 1 Outlet Node Name",
		"  Passive,                 !- Component 1 Branch Control Type",
		"  CoilSystem:Cooling:DX,   !- Component 2 Object Type",
		"  DOAS Cooling Coil,       !- Component 2 Name",
		"  DOAS Mixed Air Outlet,   !- Component 2 Inlet Node Name",
		"  DOAS Cooling Coil Outlet,!- Component 2 Outlet Node Name",
		"  Passive,                 !- Component 2 Branch Control Type",
		"  Fan:VariableVolume,      !- Component 3 Object Type",
		"  DOAS Supply Fan,         !- Component 3 Name",
		"  DOAS Cooling Coil Outlet,!- Component 3 Inlet Node Name",
		"  DOAS Supply Fan Outlet,  !- Component 3 Outlet Node Name",
		"  Active;                  !- Component 3 Branch Control Type",

		"AirLoopHVAC:SupplyPath,",
		"  DOAS Supply Path,        !- Name",
		"  DOAS Supply Path Inlet,  !- Supply Air Path Inlet Node Name",
		"  AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
		"  DOAS Zone Splitter;      !- Component 1 Name",

		"AirLoopHVAC:ZoneSplitter,",
		"  DOAS Zone Splitter,      !- Name",
		"  DOAS Supply Path Inlet,  !- Inlet Node Name",
		"  Space ATU In Node,  !- Outlet 1 Node Name",
		"  Spacex10 ATU In Node;  !- Outlet 27 Node Name",

		"AirLoopHVAC:ReturnPath,",
		"  DOAS Return Path,        !- Name",
		"  DOAS Return Air Outlet,  !- Return Air Path Outlet Node Name",
		"  AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
		"  DOAS Zone Mixer;         !- Component 1 Name",

		"AirLoopHVAC:ZoneMixer,",
		"  DOAS Zone Mixer,         !- Name",
		"  DOAS Return Air Outlet,  !- Outlet Node Name",
		"  THERMAL ZONE: FL1 CAFE Return Outlet,  !- Inlet 1 Node Name",
		"  THERMAL ZONE: ROOF_STORAGE Return Outlet;  !- Inlet 27 Node Name",

		"AvailabilityManagerAssignmentList,",
		"  DOAS Availability Managers,  !- Name",
		"  AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
		"  DOAS Availability;       !- Availability Manager 1 Name",

		"AvailabilityManager:Scheduled,",
		"  DOAS Availability,       !- Name",
		"  AvailSched;               !- Schedule Name",

		"NodeList,",
		"  DOAS Cooling Setpoint Nodes,  !- Name",
		"  DOAS Cooling Coil Outlet;!- Node 1 Name",

		"Schedule:Compact,",
		"  Always 16,               !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 1",
		"  For: AllDays,            !- Field 2",
		"  Until: 24:00,16;         !- Field 3",

		"SetpointManager:Scheduled,",
		"  DOAS Cooling Supply Air Temp Manager,  !- Name",
		"  Temperature,             !- Control Variable",
		"  Always 16,               !- Schedule Name",
		"  DOAS Supply Fan Outlet;  !- Setpoint Node or NodeList Name",

		"SetpointManager:MixedAir,",
		"  DOAS Cooling Coil Air Temp Manager,  !- Name",
		"  Temperature,             !- Control Variable",
		"  DOAS Supply Fan Outlet,  !- Reference Setpoint Node Name",
		"  DOAS Cooling Coil Outlet,!- Fan Inlet Node Name",
		"  DOAS Supply Fan Outlet,  !- Fan Outlet Node Name",
		"  DOAS Cooling Setpoint Nodes;  !- Setpoint Node or NodeList Name",

		"CoilSystem:Cooling:DX,",
		"  DOAS Cooling Coil,       !- Name",
		"  AvailSched,              !- Availability Schedule Name",
		"  DOAS Mixed Air Outlet,       !- DX Cooling Coil System Inlet Node Name",
		"  DOAS Cooling Coil Outlet,  !- DX Cooling Coil System Outlet Node Name",
		"  DOAS Cooling Coil Outlet,  !- DX Cooling Coil System Sensor Node Name",
		"  Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
		"  DOAS DX Cooling Coil;    !- Cooling Coil Name",

		"Coil:Cooling:DX:SingleSpeed,",
		"	DOAS DX Cooling Coil,    !- Name",
		" 	AvailSched,           !- Availability Schedule Name",
		"	autosize,             !- Gross Rated Total Cooling Capacity { W }",
		"	autosize,             !- Gross Rated Sensible Heat Ratio",
		"	4.40,                 !- Gross Rated Cooling COP { W / W }",
		"	autosize,             !- Rated Air Flow Rate { m3 / s }",
		"	,                     !- Rated Evaporator Fan Power Per Volume Flow Rate { W / ( m3 / s ) }",
		"	DOAS Mixed Air Outlet, !- Air Inlet Node Name",
		"	DOAS Cooling Coil Outlet,    !- Air Outlet Node Name",
		"	Biquadratic,          !- Total Cooling Capacity Function of Temperature Curve Name",
		"	Cubic,                !- Total Cooling Capacity Function of Flow Fraction Curve Name",
		"	Biquadratic,          !- Energy Input Ratio Function of Temperature Curve Name",
		"	Cubic,                !- Energy Input Ratio Function of Flow Fraction Curve Name",
		"	Cubic,                !- Part Load Fraction Correlation Curve Name",
		"	0.0,                  !- Nominal Time for Condensate Removal to Begin",
		"	0.0,                  !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
		"	0.0,                  !- Maximum Cycling Rate",
		"	0.0,                  !- Latent Capacity Time Constant",
		"	Cooling Coil Condenser Inlet, !- Condenser Air Inlet Node Name",
		"	EvaporativelyCooled,  !- Condenser Type",
		"	0.0,                  !- Evaporative Condenser Effectiveness",
		"	,                     !- Evaporative Condenser Air Flow Rate",
		"	autosize,             !- Evaporative Condenser Pump Rated Power Consumption",
		"	0.0,                  !- Crankcase Heater Capacity",
		"	10.0;                 !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",

		"Fan:VariableVolume,",
		"  DOAS Supply Fan,         !- Name",
		"  AvailSched,                !- Availability Schedule Name",
		"  0.7,                     !- Fan Total Efficiency",
		"  1000,                    !- Pressure Rise {Pa}",
		"  autosize,                !- Maximum Flow Rate {m3/s}",
		"  Fraction,                !- Fan Power Minimum Flow Rate Input Method",
		"  0.0,                     !- Fan Power Minimum Flow Fraction",
		"  ,                        !- Fan Power Minimum Air Flow Rate {m3/s}",
		"  0.9,                     !- Motor Efficiency",
		"  1,                       !- Motor In Airstream Fraction",
		"  0.0015302446,            !- Fan Power Coefficient 1",
		"  0.0052080574,            !- Fan Power Coefficient 2",
		"  1.1086242,               !- Fan Power Coefficient 3",
		"  -0.11635563,             !- Fan Power Coefficient 4",
		"  0,                       !- Fan Power Coefficient 5",
		"  DOAS Cooling Coil Outlet,!- Air Inlet Node Name",
		"  DOAS Supply Fan Outlet;  !- Air Outlet Node Name",

		"OutdoorAir:NodeList,",
		"  DOAS Outdoor Air Inlet;  !- Node or NodeList Name 1",

		"AirLoopHVAC:OutdoorAirSystem,",
		"  DOAS OA System,          !- Name",
		"  DOAS OA System Controllers,  !- Controller List Name",
		"  DOAS OA System Equipment,!- Outdoor Air Equipment List Name",
		"  DOAS Availability Managers;  !- Availability Manager List Name",

		"AirLoopHVAC:ControllerList,",
		"  DOAS OA System Controllers,  !- Name",
		"  Controller:OutdoorAir,   !- Controller 1 Object Type",
		"  DOAS OA Controller;      !- Controller 1 Name",

		"AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
		"  DOAS OA System Equipment,!- Name",
		"  OutdoorAir:Mixer,        !- Component 1 Object Type",
		"  DOAS OA Mixing Box;      !- Component 1 Name",

		"OutdoorAir:Mixer,",
		"  DOAS OA Mixing Box,      !- Name",
		"  DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
		"  DOAS Outdoor Air Inlet,  !- Outdoor Air Stream Node Name",
		"  DOAS Relief Air Outlet,  !- Relief Air Stream Node Name",
		"  DOAS Air Loop Inlet;     !- Return Air Stream Node Name",

		"Controller:OutdoorAir,",
		"  DOAS OA Controller,      !- Name",
		"  DOAS Relief Air Outlet,  !- Relief Air Outlet Node Name",
		"  DOAS Air Loop Inlet,     !- Return Air Node Name",
		"  DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
		"  DOAS Outdoor Air Inlet,  !- Actuator Node Name",
		"  0,                       !- Minimum Outdoor Air Flow Rate {m3/s}",
		"  autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
		"  NoEconomizer,            !- Economizer Control Type",
		"  MinimumFlowWithBypass,   !- Economizer Control Action Type",
		"  ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
		"  ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
		"  ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
		"  ,                        !- Electronic Enthalpy Limit Curve Name",
		"  12.2,                    !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
		"  NoLockout,               !- Lockout Type",
		"  FixedMinimum,            !- Minimum Limit Type",
		"  ,                        !- Minimum Outdoor Air Schedule Name",
		"  ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
		"  ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
		"  DCVObject;               !- Mechanical Ventilation Controller Name",

		"Controller:MechanicalVentilation,",
		"  DCVObject,               !- Name",
		"  AvailSched,                !- Availability Schedule Name",
		"  Yes,                     !- Demand Controlled Ventilation",
		"  VentilationRateProcedure,!- System Outdoor Air Method",
		"  ,                        !- Zone Maximum Outdoor Air Fraction {dimensionless}",
		"  Space,  !- Zone 1 Name",
		"  Space DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name 1",
		"  Space DSOA Design ADE Spec, !- Design Specification Zone Air Distribution Object Name 1",
		"  Spacex10,      !- Zone 2 Name",
		"  Spacex10 DSOA Design OA Spec,  !- Design Specification Outdoor Air Object Name 2",
		"  Space DSOA Design ADE Spec;  !- Design Specification Zone Air Distribution Object Name 2",

		"DesignSpecification:OutdoorAir,",
		"  Space DSOA Design OA Spec,  !- Name",
		"  sum,                     !- Outdoor Air Method",
		"  0.0,                     !- Outdoor Air Flow per Person {m3/s-person}",
		"  0.0009,                  !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
		"  0;                       !- Outdoor Air Flow per Zone {m3/s}",

		"DesignSpecification:OutdoorAir,",
		"  Spacex10 DSOA Design OA Spec,  !- Name",
		"  sum,                     !- Outdoor Air Method",
		"  0.0038,                  !- Outdoor Air Flow per Person {m3/s-person}",
		"  0.0009,                  !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
		"  0;                       !- Outdoor Air Flow per Zone {m3/s}",

		"DesignSpecification:ZoneAirDistribution,",
		"  Space DSOA Design ADE Spec,  !- Name",
		"  1,                       !- Zone Air Distribution Effectiveness in Cooling Mode {dimensionless}",
		"  1;                       !- Zone Air Distribution Effectiveness in Heating Mode {dimensionless}",

		"ZoneHVAC:EquipmentConnections,",
		" Space,                    !- Zone Name",
		" Space Eq,                 !- Zone Conditioning Equipment List Name",
		" Space In Node,            !- Zone Air Inlet Node or NodeList Name",
		" Space Out Node,           !- Zone Air Exhaust Node or NodeList Name",
		" Space Node,               !- Zone Air Node Name",
		" Space Ret Node;           !- Zone Return Air Node Name",

		"ZoneHVAC:EquipmentList,",
		" Space Eq,                 !- Name",
		" ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
		" Space ATU,                   !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

		"ZoneHVAC:AirDistributionUnit,",
		"  Space ATU,  !- Name",
		"  Space In Node,  !- Air Distribution Unit Outlet Node Name",
		"  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
		"  Space Air Terminal;  !- Air Terminal Name",

		"AirTerminal:SingleDuct:VAV:NoReheat,",
		"  Space Air Terminal,       !- Name",
		"  AvailSched,                !- Availability Schedule Name",
		"  Space In Node,              !- Air Outlet Node Name",
		"  Space ATU In Node,           !- Air Inlet Node Name",
		"  autosize,                !- Maximum Air Flow Rate {m3/s}",
		"  Constant,                !- Zone Minimum Air Flow Input Method",
		"  0.0,                     !- Constant Minimum Air Flow Fraction",
		"  ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
		"  ,                        !- Minimum Air Flow Fraction Schedule Name",
		"  ;                        !- Design Specification Outdoor Air Object Name",

		"ZoneHVAC:EquipmentConnections,",
		" Spacex10,                 !- Zone Name",
		" Spacex10 Eq,              !- Zone Conditioning Equipment List Name",
		" Spacex10 In Node,         !- Zone Air Inlet Node or NodeList Name",
		" Spacex10 Out Node,        !- Zone Air Exhaust Node or NodeList Name",
		" Spacex10 Node,            !- Zone Air Node Name",
		" Spacex10 Ret Node;        !- Zone Return Air Node Name",

		"ZoneHVAC:EquipmentList,",
		" Spacex10 Eq,              !- Name",
		" ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
		" Spacex10 ATU,                !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

		"ZoneHVAC:AirDistributionUnit,",
		"  Spacex10 ATU,  !- Name",
		"  Spacex10 In Node,  !- Air Distribution Unit Outlet Node Name",
		"  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
		"  Spacex10 Air Terminal;  !- Air Terminal Name",

		"AirTerminal:SingleDuct:VAV:NoReheat,",
		"  Spacex10 Air Terminal,    !- Name",
		"  AvailSched,                !- Availability Schedule Name",
		"  Spacex10 In Node,           !- Air Outlet Node Name",
		"  Spacex10 ATU In Node,        !- Air Inlet Node Name",
		"  autosize,                !- Maximum Air Flow Rate {m3/s}",
		"  Constant,                !- Zone Minimum Air Flow Input Method",
		"  0.0,                     !- Constant Minimum Air Flow Fraction",
		"  ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
		"  ,                        !- Minimum Air Flow Fraction Schedule Name",
		"  ;                        !- Design Specification Outdoor Air Object Name",

		"ScheduleTypeLimits,",
		"  Any Number;              !- Name",

		"Schedule:Compact,",
		"  AvailSched,              !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 3",
		"  For: AllDays,            !- Field 4",
		"  Until: 24:00,1.0;        !- Field 5",

		"Schedule:Compact,",
		"  ActivityLevelSched,      !- Name",
		"  Any Number,              !- Schedule Type Limits Name",
		"  Through: 12/31,          !- Field 3",
		"  For: AllDays,            !- Field 4",
		"  Until: 24:00,120.0;      !- Field 5",

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
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	OutputProcessor::TimeValue.allocate( 2 ); // ** HELP ** where the heck does this get allocated ??
 
	ManageSimulation(); // run the design day over the warmup period (24 hrs, 25 days)

	EXPECT_EQ( 10.0, ( Zone( 2 ).Volume * Zone( 2 ).Multiplier * Zone( 2 ).ListMultiplier ) / ( Zone( 1 ).Volume * Zone( 1 ).Multiplier * Zone( 1 ).ListMultiplier ) );
	// leaving a little wiggle room on these
	EXPECT_NEAR( 10.0, ( DXCoils::DXCoil( 2 ).RatedTotCap( 1 ) / DXCoils::DXCoil( 1 ).RatedTotCap( 1 ) ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DXCoils::DXCoil( 2 ).RatedAirVolFlowRate( 1 ) / DXCoils::DXCoil( 1 ).RatedAirVolFlowRate( 1 ) ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataZoneEnergyDemands::ZoneSysEnergyDemand( 2 ).TotalOutputRequired / DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).TotalOutputRequired ), 0.00001 );

	DataGlobals::DoWeathSim = true; // flag to trick tabular reports to scan meters
	DataGlobals::KindOfSim = DataGlobals::ksRunPeriodWeather; // fake a weather run since a weather file can't be used (could it?)
	UpdateTabularReports( OutputReportTabular::stepTypeHVAC );

	//zone equipment should report single zone magnitude, multipliers do not apply, should be > 0 or what's the point
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleRadGain, DataHeatBalance::ZnRpt( 2 ).PeopleRadGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleConGain, DataHeatBalance::ZnRpt( 2 ).PeopleConGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleSenGain, DataHeatBalance::ZnRpt( 2 ).PeopleSenGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleNumOcc, DataHeatBalance::ZnRpt( 2 ).PeopleNumOcc );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleLatGain, DataHeatBalance::ZnRpt( 2 ).PeopleLatGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleTotGain, DataHeatBalance::ZnRpt( 2 ).PeopleTotGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleRadGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleRadGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleConGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleConGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleSenGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleSenGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleLatGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleLatGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).PeopleTotGainRate, DataHeatBalance::ZnRpt( 2 ).PeopleTotGainRate );

	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsPower, DataHeatBalance::ZnRpt( 2 ).LtsPower );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsElecConsump, DataHeatBalance::ZnRpt( 2 ).LtsElecConsump );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsRadGain, DataHeatBalance::ZnRpt( 2 ).LtsRadGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsVisGain, DataHeatBalance::ZnRpt( 2 ).LtsVisGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsConGain, DataHeatBalance::ZnRpt( 2 ).LtsConGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsRetAirGain, DataHeatBalance::ZnRpt( 2 ).LtsRetAirGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsTotGain, DataHeatBalance::ZnRpt( 2 ).LtsTotGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsRadGainRate, DataHeatBalance::ZnRpt( 2 ).LtsRadGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsVisGainRate, DataHeatBalance::ZnRpt( 2 ).LtsVisGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsConGainRate, DataHeatBalance::ZnRpt( 2 ).LtsConGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsRetAirGainRate, DataHeatBalance::ZnRpt( 2 ).LtsRetAirGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).LtsTotGainRate, DataHeatBalance::ZnRpt( 2 ).LtsTotGainRate );

	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecPower, DataHeatBalance::ZnRpt( 2 ).ElecPower );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecConsump, DataHeatBalance::ZnRpt( 2 ).ElecConsump );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecRadGain, DataHeatBalance::ZnRpt( 2 ).ElecRadGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecConGain, DataHeatBalance::ZnRpt( 2 ).ElecConGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecLatGain, DataHeatBalance::ZnRpt( 2 ).ElecLatGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecLost, DataHeatBalance::ZnRpt( 2 ).ElecLost );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecTotGain, DataHeatBalance::ZnRpt( 2 ).ElecTotGain );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecRadGainRate, DataHeatBalance::ZnRpt( 2 ).ElecRadGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecConGainRate, DataHeatBalance::ZnRpt( 2 ).ElecConGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecLatGainRate, DataHeatBalance::ZnRpt( 2 ).ElecLatGainRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecLostRate, DataHeatBalance::ZnRpt( 2 ).ElecLostRate );
	EXPECT_EQ( DataHeatBalance::ZnRpt( 1 ).ElecTotGainRate, DataHeatBalance::ZnRpt( 2 ).ElecTotGainRate );

	//expect occupancy time data to be equal
	EXPECT_EQ( DataHeatBalance::ZonePreDefRep( 1 ).NumOccAccumTime, DataHeatBalance::ZonePreDefRep( 2 ).NumOccAccumTime );
	EXPECT_EQ( DataHeatBalance::ZonePreDefRep( 1 ).TotTimeOcc, DataHeatBalance::ZonePreDefRep( 2 ).TotTimeOcc );

	//expect reported occupancy to be based on multipliers
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).NumOccAccum / DataHeatBalance::ZonePreDefRep( 1 ).NumOccAccum ), 0.00001 );

	//expect energy to report according to multipliers
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).MechVentVolTotal / DataHeatBalance::ZonePreDefRep( 1 ).MechVentVolTotal ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).MechVentVolMin / DataHeatBalance::ZonePreDefRep( 1 ).MechVentVolMin ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnHvacCl / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnHvacCl ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnPeoplAdd / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnPeoplAdd ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnLiteAdd / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnLiteAdd ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnEquipAdd / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnEquipAdd ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).SHGSAnOtherRem / DataHeatBalance::ZonePreDefRep( 1 ).SHGSAnOtherRem ), 0.00001 );
	EXPECT_NEAR( 10.0, ( DataHeatBalance::ZonePreDefRep( 2 ).clPeak / DataHeatBalance::ZonePreDefRep( 1 ).clPeak ), 0.00001 );

}
