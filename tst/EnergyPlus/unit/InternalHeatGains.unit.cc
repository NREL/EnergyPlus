// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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
#include <InternalHeatGains.hh>
#include <HeatBalanceManager.hh>
#include <ScheduleManager.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <ExteriorEnergyUse.hh>

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

	ASSERT_FALSE(process_idf(idf_objects));
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

	ASSERT_FALSE(process_idf(idf_objects));
	EXPECT_FALSE(has_err_output());

	bool ErrorsFound(false);

	DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
	DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
	ScheduleManager::ProcessScheduleInput(); // read schedules

	HeatBalanceManager::GetZoneData(ErrorsFound);
	ASSERT_FALSE(ErrorsFound);

	ASSERT_THROW( InternalHeatGains::GetInternalHeatGainsInput(), std::runtime_error );

	std::string const error_string = delimited_string({
		"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank Schedule Type Limits Name input -- will not be validated.",
		"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank Schedule Type Limits Name input -- will not be validated.",
		"   ** Severe  ** GetInternalHeatGains: OtherEquipment=\"OTHEREQ1\", Design Level is not allowed to be negative",
		"   **   ~~~   ** ... when a fuel type of FuelOil#1 is specified.",
		"   **  Fatal  ** GetInternalHeatGains: Errors found in Getting Internal Gains Input, Program Stopped",
		"   ...Summary of Errors that led to program termination:",
		"   ..... Reference severe error count=1",
		"   ..... Last severe error=GetInternalHeatGains: OtherEquipment=\"OTHEREQ1\", Design Level is not allowed to be negative"
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

	ASSERT_FALSE(process_idf(idf_objects));
	EXPECT_FALSE(has_err_output());

	bool ErrorsFound(false);

	DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
	DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
	ScheduleManager::ProcessScheduleInput(); // read schedules

	HeatBalanceManager::GetZoneData(ErrorsFound);
	ASSERT_FALSE(ErrorsFound);

	ASSERT_THROW( InternalHeatGains::GetInternalHeatGainsInput(), std::runtime_error );

	std::string const error_string = delimited_string({
		"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank Schedule Type Limits Name input -- will not be validated.",
		"   ** Severe  ** GetInternalHeatGains: OtherEquipment: invalid Fuel Type entered=WATER for Name=OTHEREQ1",
		"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank Schedule Type Limits Name input -- will not be validated.",
		"   **  Fatal  ** GetInternalHeatGains: Errors found in Getting Internal Gains Input, Program Stopped",
		"   ...Summary of Errors that led to program termination:",
		"   ..... Reference severe error count=1",
		"   ..... Last severe error=GetInternalHeatGains: OtherEquipment: invalid Fuel Type entered=WATER for Name=OTHEREQ1"
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

	ASSERT_FALSE( process_idf( idf_objects ) );

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
