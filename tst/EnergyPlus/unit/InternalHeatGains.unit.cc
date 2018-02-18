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

#include <exception>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataLoopNode.hh>
#include <DataSizing.hh>
#include <DisplacementVentMgr.hh>
#include <ExteriorEnergyUse.hh>
#include <HeatBalanceManager.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <HVACManager.hh>
#include <InternalHeatGains.hh>
#include <OutputReportTabular.hh>
#include <ScheduleManager.hh>
#include <DataZoneEquipment.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F( EnergyPlusFixture, InternalHeatGains_OtherEquipment_CheckFuelType )
{

	std::string const idf_objects = delimited_string({
		"Version,8.5;",

		"Zone,Zone1;",

		"ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

		"Schedule:Constant,Schedule1,,1.0;",

		"OtherEquipment,",
		"  OtherEq1,",
		"  ,",
		"  Zone1,",
		"  Schedule1,",
		"  EquipmentLevel,",
		"  100.0,,,",
		"  0.1,",
		"  0.2,",
		"  0.05;",

		"OtherEquipment,",
		"  OtherEq2,",
		"  PropaneGas,",
		"  Zone1,",
		"  Schedule1,",
		"  EquipmentLevel,",
		"  100.0,,,",
		"  0.1,",
		"  0.2,",
		"  0.05;",

	} );

	ASSERT_TRUE(process_idf(idf_objects));
	EXPECT_FALSE(has_err_output());

	bool ErrorsFound(false);

	DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
	DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
	ScheduleManager::ProcessScheduleInput(); // read schedules

	HeatBalanceManager::GetZoneData(ErrorsFound);
	ASSERT_FALSE(ErrorsFound);

	InternalHeatGains::GetInternalHeatGainsInput();

	ASSERT_EQ(DataHeatBalance::ZoneOtherEq.size(), 2u);

	for ( unsigned long i=1; i <= DataHeatBalance::ZoneOtherEq.size(); ++i ) {
		const DataHeatBalance::ZoneEquipData & equip = DataHeatBalance::ZoneOtherEq( i );
		if ( equip.Name == "OTHEREQ1" ) {
			ASSERT_EQ(equip.OtherEquipFuelType, 0);
		} else if ( equip.Name == "OTHEREQ2" ) {
			ASSERT_EQ(equip.OtherEquipFuelType, ExteriorEnergyUse::LPGUse);
		}
	}

}

TEST_F( EnergyPlusFixture, InternalHeatGains_OtherEquipment_NegativeDesignLevel ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",

		"Zone,Zone1;",

		"ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

		"Schedule:Constant,Schedule1,,1.0;",

		"OtherEquipment,",
		"  OtherEq1,",
		"  FuelOil#1,",
		"  Zone1,",
		"  Schedule1,",
		"  EquipmentLevel,",
		"  -100.0,,,",
		"  0.1,",
		"  0.2,",
		"  0.05;",

	} );

	ASSERT_TRUE(process_idf(idf_objects));
	EXPECT_FALSE(has_err_output());

	bool ErrorsFound(false);

	DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
	DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
	ScheduleManager::ProcessScheduleInput(); // read schedules

	HeatBalanceManager::GetZoneData(ErrorsFound);
	ASSERT_FALSE(ErrorsFound);

	ASSERT_THROW( InternalHeatGains::GetInternalHeatGainsInput(), std::runtime_error );

	std::string const error_string = delimited_string({
		"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank schedule_type_limits_name input -- will not be validated.",
		"   ** Severe  ** GetInternalHeatGains: OtherEquipment=\"OTHEREQ1\", design_level is not allowed to be negative",
		"   **   ~~~   ** ... when a fuel type of FuelOil#1 is specified.",
		"   **  Fatal  ** GetInternalHeatGains: Errors found in Getting Internal Gains Input, Program Stopped",
		"   ...Summary of Errors that led to program termination:",
		"   ..... Reference severe error count=1",
		"   ..... Last severe error=GetInternalHeatGains: OtherEquipment=\"OTHEREQ1\", design_level is not allowed to be negative"
	});

	EXPECT_TRUE( compare_err_stream( error_string, true ) );

}

TEST_F( EnergyPlusFixture, InternalHeatGains_OtherEquipment_BadFuelType ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",

		"Zone,Zone1;",

		"ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

		"Schedule:Constant,Schedule1,,1.0;",

		"OtherEquipment,",
		"  OtherEq1,",
		"  Water,",
		"  Zone1,",
		"  Schedule1,",
		"  EquipmentLevel,",
		"  100.0,,,",
		"  0.1,",
		"  0.2,",
		"  0.05;",

	} );

	ASSERT_TRUE(process_idf(idf_objects));
	EXPECT_FALSE(has_err_output());

	bool ErrorsFound(false);

	DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
	DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
	ScheduleManager::ProcessScheduleInput(); // read schedules

	HeatBalanceManager::GetZoneData(ErrorsFound);
	ASSERT_FALSE(ErrorsFound);

	ASSERT_THROW( InternalHeatGains::GetInternalHeatGainsInput(), std::runtime_error );

	std::string const error_string = delimited_string({
		"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank schedule_type_limits_name input -- will not be validated.",
		"   ** Severe  ** GetInternalHeatGains: OtherEquipment: invalid fuel_type entered=WATER for name=OTHEREQ1",
		"   **  Fatal  ** GetInternalHeatGains: Errors found in Getting Internal Gains Input, Program Stopped",
		"   ...Summary of Errors that led to program termination:",
		"   ..... Reference severe error count=1",
		"   ..... Last severe error=GetInternalHeatGains: OtherEquipment: invalid fuel_type entered=WATER for name=OTHEREQ1"
	});

	EXPECT_TRUE( compare_err_stream( error_string, true ) );

}

TEST_F( EnergyPlusFixture, InternalHeatGains_AllowBlankFieldsForAdaptiveComfortModel ) {
	// Adaptive comfort model fatal for irrelevant blank fields  #5948

	std::string const idf_objects = delimited_string( {
		"Version,8.5;",

		"ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

		"  Schedule:Compact,",
		"    HOUSE OCCUPANCY,    !- Name",
		"    Fraction,                !- Schedule Type Limits Name",
		"    Through: 12/31,           !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,1.0;        !- Field 3",

		"  Schedule:Compact,",
		"    Activity Sch,    !- Name",
		"    Fraction,                !- Schedule Type Limits Name",
		"    Through: 12/31,           !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,1.0;        !- Field 3",

		"Zone,LIVING ZONE;",

		"People,",
		"LIVING ZONE People, !- Name",
		"LIVING ZONE, !- Zone or ZoneList Name",
		"HOUSE OCCUPANCY, !- Number of People Schedule Name",
		"people, !- Number of People Calculation Method",
		"3.000000, !- Number of People",
		", !- People per Zone Floor Area{ person / m2 }",
		", !- Zone Floor Area per Person{ m2 / person }",
		"0.3000000, !- Fraction Radiant",
		", !- Sensible Heat Fraction",
		"Activity Sch, !- Activity Level Schedule Name",
		"3.82E-8, !- Carbon Dioxide Generation Rate{ m3 / s - W }",
		", !- Enable ASHRAE 55 Comfort Warnings",
		"zoneaveraged, !- Mean Radiant Temperature Calculation Type",
		", !- Surface Name / Angle Factor List Name",
		", !- Work Efficiency Schedule Name",
		", !- Clothing Insulation Calculation Method",
		", !- Clothing Insulation Calculation Method Schedule Name",
		", !- Clothing Insulation Schedule Name",
		", !- Air Velocity Schedule Name",
		"AdaptiveASH55;                  !- Thermal Comfort Model 1 Type",

	} );

	ASSERT_TRUE( process_idf( idf_objects ) );

	bool ErrorsFound1( false );

	ScheduleManager::ProcessScheduleInput( ); // read schedules
	HeatBalanceManager::GetZoneData( ErrorsFound1 );
	ASSERT_FALSE( ErrorsFound1 );

	ScheduleManager::ScheduleInputProcessed = true;
	ScheduleManager::Schedule( 1 ).Used = true;;
	ScheduleManager::Schedule( 1 ).CurrentValue = 1.0;
	ScheduleManager::Schedule( 1 ).MinValue = 1.0;
	ScheduleManager::Schedule( 1 ).MaxValue = 1.0;
	ScheduleManager::Schedule( 1 ).MaxMinSet = true;
	ScheduleManager::Schedule( 2 ).Used = true;;
	ScheduleManager::Schedule( 2 ).CurrentValue = 131.8;
	ScheduleManager::Schedule( 2 ).MinValue = 131.8;
	ScheduleManager::Schedule( 2 ).MaxValue = 131.8;
	ScheduleManager::Schedule( 2 ).MaxMinSet = true;
	InternalHeatGains::GetInternalHeatGainsInput( );

	EXPECT_FALSE( InternalHeatGains::ErrorsFound );

}

TEST_F(EnergyPlusFixture, InternalHeatGains_ElectricEquipITE_BeginEnvironmentReset) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",

		"Zone,Zone1;",

		"ElectricEquipment:ITE:AirCooled,",
		"  Data Center Servers,     !- Name",
		"  Zone1,                   !- Zone Name",
		"  ,",
		"  Watts/Unit,              !- Design Power Input Calculation Method",
		"  500,                     !- Watts per Unit {W}",
		"  100,                     !- Number of Units",
		"  ,                        !- Watts per Zone Floor Area {W/m2}",
		"  ,  !- Design Power Input Schedule Name",
		"  ,  !- CPU Loading  Schedule Name",
		"  Data Center Servers Power fLoadTemp,  !- CPU Power Input Function of Loading and Air Temperature Curve Name",
		"  0.4,                     !- Design Fan Power Input Fraction",
		"  0.0001,                  !- Design Fan Air Flow Rate per Power Input {m3/s-W}",
		"  Data Center Servers Airflow fLoadTemp,  !- Air Flow Function of Loading and Air Temperature Curve Name",
		"  ECM FanPower fFlow,      !- Fan Power Input Function of Flow Curve Name",
		"  15,                      !- Design Entering Air Temperature {C}",
		"  A3,                      !- Environmental Class",
		"  AdjustedSupply,          !- Air Inlet Connection Type",
		"  ,                        !- Air Inlet Room Air Model Node Name",
		"  ,                        !- Air Outlet Room Air Model Node Name",
		"  Main Zone Inlet Node,    !- Supply Air Node Name",
		"  0.1,                     !- Design Recirculation Fraction",
		"  Data Center Recirculation fLoadTemp,  !- Recirculation Function of Loading and Supply Temperature Curve Name",
		"  0.9,                     !- Design Electric Power Supply Efficiency",
		"  UPS Efficiency fPLR,     !- Electric Power Supply Efficiency Function of Part Load Ratio Curve Name",
		"  1,                       !- Fraction of Electric Power Supply Losses to Zone",
		"  ITE-CPU,                 !- CPU End-Use Subcategory",
		"  ITE-Fans,                !- Fan End-Use Subcategory",
		"  ITE-UPS;                 !- Electric Power Supply End-Use Subcategory",
		"",
		"Curve:Quadratic,",
		"  ECM FanPower fFlow,      !- Name",
		"  0.0,                     !- Coefficient1 Constant",
		"  1.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.0,                     !- Minimum Value of x",
		"  99.0;                    !- Maximum Value of x",
		"",
		"Curve:Quadratic,",
		"  UPS Efficiency fPLR,     !- Name",
		"  1.0,                     !- Coefficient1 Constant",
		"  0.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.0,                     !- Minimum Value of x",
		"  99.0;                    !- Maximum Value of x",
		"",
		"Curve:Biquadratic,",
		"  Data Center Servers Power fLoadTemp,  !- Name",
		"  -1.0,                    !- Coefficient1 Constant",
		"  1.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.06667,                 !- Coefficient4 y",
		"  0.0,                     !- Coefficient5 y**2",
		"  0.0,                     !- Coefficient6 x*y",
		"  0.0,                     !- Minimum Value of x",
		"  1.5,                     !- Maximum Value of x",
		"  -10,                     !- Minimum Value of y",
		"  99.0,                    !- Maximum Value of y",
		"  0.0,                     !- Minimum Curve Output",
		"  99.0,                    !- Maximum Curve Output",
		"  Dimensionless,           !- Input Unit Type for X",
		"  Temperature,             !- Input Unit Type for Y",
		"  Dimensionless;           !- Output Unit Type",
		"",
		"Curve:Biquadratic,",
		"  Data Center Servers Airflow fLoadTemp,  !- Name",
		"  -1.4,                    !- Coefficient1 Constant",
		"  0.9,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.1,                     !- Coefficient4 y",
		"  0.0,                     !- Coefficient5 y**2",
		"  0.0,                     !- Coefficient6 x*y",
		"  0.0,                     !- Minimum Value of x",
		"  1.5,                     !- Maximum Value of x",
		"  -10,                     !- Minimum Value of y",
		"  99.0,                    !- Maximum Value of y",
		"  0.0,                     !- Minimum Curve Output",
		"  99.0,                    !- Maximum Curve Output",
		"  Dimensionless,           !- Input Unit Type for X",
		"  Temperature,             !- Input Unit Type for Y",
		"  Dimensionless;           !- Output Unit Type",
		"",
		"Curve:Biquadratic,",
		"  Data Center Recirculation fLoadTemp,  !- Name",
		"  1.0,                     !- Coefficient1 Constant",
		"  0.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.0,                     !- Coefficient4 y",
		"  0.0,                     !- Coefficient5 y**2",
		"  0.0,                     !- Coefficient6 x*y",
		"  0.0,                     !- Minimum Value of x",
		"  1.5,                     !- Maximum Value of x",
		"  -10,                     !- Minimum Value of y",
		"  99.0,                    !- Maximum Value of y",
		"  0.0,                     !- Minimum Curve Output",
		"  99.0,                    !- Maximum Curve Output",
		"  Dimensionless,           !- Input Unit Type for X",
		"  Temperature,             !- Input Unit Type for Y",
		"  Dimensionless;           !- Output Unit Type",
	
	});

	ASSERT_TRUE( process_idf( idf_objects ) );
	EXPECT_FALSE( has_err_output() );

	bool ErrorsFound( false );

	HeatBalanceManager::GetZoneData( ErrorsFound );
	ASSERT_FALSE( ErrorsFound );
	DataHeatBalFanSys::MAT.allocate( 1 );
	DataHeatBalFanSys::ZoneAirHumRat.allocate( 1 );

	DataHeatBalFanSys::MAT( 1 ) = 24.0;
	DataHeatBalFanSys::ZoneAirHumRat( 1 ) = 0.008;

	InternalHeatGains::GetInternalHeatGainsInput();
	InternalHeatGains::CalcZoneITEq();
	Real64 InitialPower = DataHeatBalance::ZoneITEq( 1 ).CPUPower + DataHeatBalance::ZoneITEq( 1 ).FanPower + DataHeatBalance::ZoneITEq( 1 ).UPSPower;

	DataLoopNode::Node( 1 ).Temp = 45.0;
	InternalHeatGains::CalcZoneITEq();
	Real64 NewPower = DataHeatBalance::ZoneITEq( 1 ).CPUPower + DataHeatBalance::ZoneITEq( 1 ).FanPower + DataHeatBalance::ZoneITEq( 1 ).UPSPower;
	ASSERT_NE( InitialPower, NewPower );
	HVACManager::ResetNodeData();

	InternalHeatGains::CalcZoneITEq();
	NewPower = DataHeatBalance::ZoneITEq( 1 ).CPUPower + DataHeatBalance::ZoneITEq( 1 ).FanPower + DataHeatBalance::ZoneITEq( 1 ).UPSPower;
	ASSERT_EQ( InitialPower, NewPower );
}

TEST_F(EnergyPlusFixture, InternalHeatGains_CheckZoneComponentLoadSubtotals) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Zone,Zone1;",
	});

	ASSERT_TRUE( process_idf( idf_objects ) );
	EXPECT_FALSE( has_err_output() );

	bool ErrorsFound( false );
	HeatBalanceManager::GetZoneData( ErrorsFound );
	ASSERT_FALSE( ErrorsFound );
	InternalHeatGains::GetInternalHeatGainsInput();

	// Set up a simple convective gain for each gain type
	int zoneNum = 1;
	int numGainTypes = DataHeatBalance::NumZoneIntGainDeviceTypes;
	Array1D< Real64 > convGains;
	convGains.allocate( numGainTypes );
	convGains = 0.0;
	Real64 totConvGains = 0.0;
	Real64 expectedTotConvGains = 0.0;

	for (int gainType = 1; gainType <= numGainTypes; ++gainType ) {
		convGains( gainType ) = 100 * gainType;
		expectedTotConvGains += convGains( gainType );
		SetupZoneInternalGain( zoneNum, DataHeatBalance::ccZoneIntGainDeviceTypes( gainType ), "Gain", gainType, convGains( gainType ) );
	}

	InternalHeatGains::UpdateInternalGainValues();

	// Check total of all convective gains
	InternalHeatGains::SumAllInternalConvectionGains( zoneNum, totConvGains );
	EXPECT_EQ( totConvGains, expectedTotConvGains );

	// Check subtotals used in zone component loads
	DataEnvironment::TotDesDays = 1;
	DataEnvironment::TotRunDesPersDays = 0;
	DataSizing::CurOverallSimDay = 1;
	DataGlobals::HourOfDay = 1;
	DataGlobals::NumOfTimeStepInHour = 10;
	DataGlobals::TimeStep = 1;
	OutputReportTabular::AllocateLoadComponentArrays();
	int timeStepInDay = ( DataGlobals::HourOfDay - 1) * DataGlobals::NumOfTimeStepInHour + DataGlobals::TimeStep;
		
	DataGlobals::CompLoadReportIsReq = true;
	DataGlobals::isPulseZoneSizing = false;
	InternalHeatGains::GatherComponentLoadsIntGain();
	totConvGains = OutputReportTabular::peopleInstantSeq( DataSizing::CurOverallSimDay, timeStepInDay, zoneNum ) + OutputReportTabular::lightInstantSeq( DataSizing::CurOverallSimDay, timeStepInDay, zoneNum ) + OutputReportTabular::equipInstantSeq( DataSizing::CurOverallSimDay, timeStepInDay, zoneNum ) + OutputReportTabular::refrigInstantSeq( DataSizing::CurOverallSimDay, timeStepInDay, zoneNum ) + OutputReportTabular::waterUseInstantSeq( DataSizing::CurOverallSimDay, timeStepInDay, zoneNum ) + OutputReportTabular::hvacLossInstantSeq( DataSizing::CurOverallSimDay, timeStepInDay, zoneNum ) + OutputReportTabular::powerGenInstantSeq( DataSizing::CurOverallSimDay, timeStepInDay, zoneNum );

	// Legitimate gain types excluded from this total
	expectedTotConvGains -= convGains( DataHeatBalance::IntGainTypeOf_ZoneContaminantSourceAndSinkCarbonDioxide ); // this is only used for CO2
	expectedTotConvGains -= convGains( DataHeatBalance::IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam ); // this is only used for generic contaminants
	expectedTotConvGains -= convGains( DataHeatBalance::IntGainTypeOf_DaylightingDeviceTubular ); // this is included in Fenestration Conduction - Sensible Instant

	// ** NOTE: If this unit test fails, the likely cause is that a new internal gain type was added, but it was not added to one of the subtotal types in InternalHeatGains::GatherComponentLoadsIntGain()
	// this also means that the new type may be missing from other places that collect internal gains by subgroups, such as the room air models and output reporting for zone-level gains
	// search for IntGainTypeOf_Lights for places where these types of subtotals occur and add the new type as appropriate
	EXPECT_EQ( totConvGains, expectedTotConvGains );


	// cleanup
	convGains.deallocate();
}

TEST_F(EnergyPlusFixture, InternalHeatGains_ElectricEquipITE_ApproachTemperatures) {

	std::string const idf_objects = delimited_string({
		"Version,8.8;",

		"Zone,Zone1;",

		"ElectricEquipment:ITE:AirCooled,",
		"  Data Center Servers,     !- Name",
		"  Zone1,                   !- Zone Name",
		"  FlowControlWithApproachTemperatures,    !- Calculation Method",
		"  Watts/Unit,              !- Design Power Input Calculation Method",
		"  500,                     !- Watts per Unit {W}",
		"  100,                     !- Number of Units",
		"  ,                        !- Watts per Zone Floor Area {W/m2}",
		"  ,  !- Design Power Input Schedule Name",
		"  ,  !- CPU Loading  Schedule Name",
		"  Data Center Servers Power fLoadTemp,  !- CPU Power Input Function of Loading and Air Temperature Curve Name",
		"  0.4,                     !- Design Fan Power Input Fraction",
		"  0.0001,                  !- Design Fan Air Flow Rate per Power Input {m3/s-W}",
		"  Data Center Servers Airflow fLoadTemp,  !- Air Flow Function of Loading and Air Temperature Curve Name",
		"  ECM FanPower fFlow,      !- Fan Power Input Function of Flow Curve Name",
		"  15,                      !- Design Entering Air Temperature {C}",
		"  A3,                      !- Environmental Class",
		"  AdjustedSupply,          !- Air Inlet Connection Type",
		"  ,                        !- Air Inlet Room Air Model Node Name",
		"  ,                        !- Air Outlet Room Air Model Node Name",
		"  Main Zone Inlet Node,    !- Supply Air Node Name",
		"  0.1,                     !- Design Recirculation Fraction",
		"  Data Center Recirculation fLoadTemp,  !- Recirculation Function of Loading and Supply Temperature Curve Name",
		"  0.9,                     !- Design Electric Power Supply Efficiency",
		"  UPS Efficiency fPLR,     !- Electric Power Supply Efficiency Function of Part Load Ratio Curve Name",
		"  1,                       !- Fraction of Electric Power Supply Losses to Zone",
		"  ITE-CPU,                 !- CPU End-Use Subcategory",
		"  ITE-Fans,                !- Fan End-Use Subcategory",
		"  ITE-UPS,                 !- Electric Power Supply End-Use Subcategory",
		"  2,                       !- Supply Approach Temperature",
		"  ,                        !- Supply Approach Temperature Schedule",
		"  -2,                      !- Return Approach Temperature",
		"  ;                        !- Return Approach Temperature Schedule",
		"",
		"Curve:Quadratic,",
		"  ECM FanPower fFlow,      !- Name",
		"  0.0,                     !- Coefficient1 Constant",
		"  1.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.0,                     !- Minimum Value of x",
		"  99.0;                    !- Maximum Value of x",
		"",
		"Curve:Quadratic,",
		"  UPS Efficiency fPLR,     !- Name",
		"  1.0,                     !- Coefficient1 Constant",
		"  0.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.0,                     !- Minimum Value of x",
		"  99.0;                    !- Maximum Value of x",
		"",
		"Curve:Biquadratic,",
		"  Data Center Servers Power fLoadTemp,  !- Name",
		"  -1.0,                    !- Coefficient1 Constant",
		"  1.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.06667,                 !- Coefficient4 y",
		"  0.0,                     !- Coefficient5 y**2",
		"  0.0,                     !- Coefficient6 x*y",
		"  0.0,                     !- Minimum Value of x",
		"  1.5,                     !- Maximum Value of x",
		"  -10,                     !- Minimum Value of y",
		"  99.0,                    !- Maximum Value of y",
		"  0.0,                     !- Minimum Curve Output",
		"  99.0,                    !- Maximum Curve Output",
		"  Dimensionless,           !- Input Unit Type for X",
		"  Temperature,             !- Input Unit Type for Y",
		"  Dimensionless;           !- Output Unit Type",
		"",
		"Curve:Biquadratic,",
		"  Data Center Servers Airflow fLoadTemp,  !- Name",
		"  -1.4,                    !- Coefficient1 Constant",
		"  0.9,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.1,                     !- Coefficient4 y",
		"  0.0,                     !- Coefficient5 y**2",
		"  0.0,                     !- Coefficient6 x*y",
		"  0.0,                     !- Minimum Value of x",
		"  1.5,                     !- Maximum Value of x",
		"  -10,                     !- Minimum Value of y",
		"  99.0,                    !- Maximum Value of y",
		"  0.0,                     !- Minimum Curve Output",
		"  99.0,                    !- Maximum Curve Output",
		"  Dimensionless,           !- Input Unit Type for X",
		"  Temperature,             !- Input Unit Type for Y",
		"  Dimensionless;           !- Output Unit Type",
		"",
		"Curve:Biquadratic,",
		"  Data Center Recirculation fLoadTemp,  !- Name",
		"  1.0,                     !- Coefficient1 Constant",
		"  0.0,                     !- Coefficient2 x",
		"  0.0,                     !- Coefficient3 x**2",
		"  0.0,                     !- Coefficient4 y",
		"  0.0,                     !- Coefficient5 y**2",
		"  0.0,                     !- Coefficient6 x*y",
		"  0.0,                     !- Minimum Value of x",
		"  1.5,                     !- Maximum Value of x",
		"  -10,                     !- Minimum Value of y",
		"  99.0,                    !- Maximum Value of y",
		"  0.0,                     !- Minimum Curve Output",
		"  99.0,                    !- Maximum Curve Output",
		"  Dimensionless,           !- Input Unit Type for X",
		"  Temperature,             !- Input Unit Type for Y",
		"  Dimensionless;           !- Output Unit Type",
	
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	EXPECT_FALSE( has_err_output() );

	bool ErrorsFound( false );

	HeatBalanceManager::GetZoneData( ErrorsFound );
	ASSERT_FALSE( ErrorsFound );
	DataHeatBalFanSys::MAT.allocate( 1 );
	DataHeatBalFanSys::ZoneAirHumRat.allocate( 1 );
	DataHeatBalance::ZnRpt.allocate( 1 );
	DataZoneEquipment::ZoneEquipConfig.allocate( 1 );

	DataHeatBalFanSys::MAT( 1 ) = 24.0;
	DataHeatBalFanSys::ZoneAirHumRat( 1 ) = 0.008;

	InternalHeatGains::GetInternalHeatGainsInput();

	DataLoopNode::Node( 1 ).Temp = 45.0;
	InternalHeatGains::CalcZoneITEq();
	ASSERT_DOUBLE_EQ( DataHeatBalance::ZoneITEq( 1 ).AirOutletDryBulbT + DataHeatBalance::ZoneITEq( 1 ).ReturnApproachTemp, DataHeatBalance::Zone( 1 ).AdjustedReturnTempByITE );
	ASSERT_DOUBLE_EQ( DataLoopNode::Node( 1 ).Temp + DataHeatBalance::ZoneITEq( 1 ).SupplyApproachTemp, DataHeatBalance::ZoneITEq( 1 ).AirInletDryBulbT );
}
