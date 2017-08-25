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

// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/ScheduleManager.hh>
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::ScheduleManager;
using namespace ObjexxFCL;



TEST_F( EnergyPlusFixture, ScheduleManager_isMinuteMultipleOfTimestep )
{
	// EnergyPlus can accept 1,  2, 3,   4,  5,  6, 10, 12, 15, 20, 30, 60 timesteps per hour which correspond to
	//                      60, 30, 20, 15, 12, 10,  5,  5,  4,  3,  2,  1 minutes per timestep
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 0, 15 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 15, 15 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 30, 15 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 45, 15 ) );

	EXPECT_FALSE( isMinuteMultipleOfTimestep( 22, 15 ) );
	EXPECT_FALSE( isMinuteMultipleOfTimestep( 53, 15 ) );

	EXPECT_TRUE( isMinuteMultipleOfTimestep( 0, 12 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 12, 12 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 24, 12 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 36, 12 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 48, 12 ) );

	EXPECT_FALSE( isMinuteMultipleOfTimestep( 22, 12 ) );
	EXPECT_FALSE( isMinuteMultipleOfTimestep( 53, 12 ) );
}


TEST_F( EnergyPlusFixture, ScheduleAnnualFullLoadHours_test )
{
	// J.Glazer - August 2017

	std::string const idf_objects = delimited_string( {
		"Version,8.8;",
		" ",
		"ScheduleTypeLimits,",
		"  Any Number;              !- Name",
		" ",
		"Schedule:Compact,",
		" OnSched,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" OffSched,                 !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 0.0;        !- Field 3",
		" ",
		"Schedule:Compact,",
		" JanOnSched,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 1/31,            !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0,        !- Field 26",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 0.0;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" HalfOnSched,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 12:00, 1.0,        !- Field 26",
		" Until: 24:00, 0.0;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" HalfOnSched2,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 12:00, 0.75,        !- Field 26",
		" Until: 24:00, 0.25;        !- Field 26",
		" ",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	DataGlobals::NumOfTimeStepInHour = 4;
	DataGlobals::MinutesPerTimeStep = 15;

	int onSchedIndex = GetScheduleIndex("ONSCHED");
	EXPECT_EQ( 8760., ScheduleAnnualFullLoadHours( onSchedIndex, 1, false ) ); 

	int offSchedIndex = GetScheduleIndex( "OFFSCHED" );
	EXPECT_EQ( 0., ScheduleAnnualFullLoadHours( offSchedIndex, 1, false ) );

	int janOnSchedIndex = GetScheduleIndex( "JANONSCHED" );
	EXPECT_EQ( 744., ScheduleAnnualFullLoadHours( janOnSchedIndex, 1, false ) );

	int halfOnSchedIndex = GetScheduleIndex( "HALFONSCHED" );
	EXPECT_EQ( 4380., ScheduleAnnualFullLoadHours( halfOnSchedIndex, 1, false ) );

	int halfOnSched2Index = GetScheduleIndex( "HALFONSCHED2" );
	EXPECT_EQ( 4380., ScheduleAnnualFullLoadHours( halfOnSched2Index, 1, false ) );

}

TEST_F( EnergyPlusFixture, ScheduleAverageHoursPerWeek_test )
{
	// J.Glazer - August 2017

	std::string const idf_objects = delimited_string( {
		"Version,8.8;",
		" ",
		"ScheduleTypeLimits,",
		"  Any Number;              !- Name",
		" ",
		"Schedule:Compact,",
		" OnSched,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" OffSched,                 !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 0.0;        !- Field 3",
		" ",
		"Schedule:Compact,",
		" JanOnSched,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 1/31,            !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0,        !- Field 26",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 0.0;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" HalfOnSched,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 12:00, 1.0,        !- Field 26",
		" Until: 24:00, 0.0;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" HalfOnSched2,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 12:00, 0.75,        !- Field 26",
		" Until: 24:00, 0.25;        !- Field 26",
		" ",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	DataGlobals::NumOfTimeStepInHour = 4;
	DataGlobals::MinutesPerTimeStep = 15;

	int onSchedIndex = GetScheduleIndex( "ONSCHED" );
	EXPECT_EQ( 168., ScheduleAverageHoursPerWeek( onSchedIndex, 1, false ) );

	int offSchedIndex = GetScheduleIndex( "OFFSCHED" );
	EXPECT_EQ( 0., ScheduleAverageHoursPerWeek( offSchedIndex, 1, false ) );

	int janOnSchedIndex = GetScheduleIndex( "JANONSCHED" );
	EXPECT_NEAR( 14.3, ScheduleAverageHoursPerWeek( janOnSchedIndex, 1, false ), 0.1 );

	int halfOnSchedIndex = GetScheduleIndex( "HALFONSCHED" );
	EXPECT_EQ( 84., ScheduleAverageHoursPerWeek( halfOnSchedIndex, 1, false ) );

	int halfOnSched2Index = GetScheduleIndex( "HALFONSCHED2" );
	EXPECT_EQ( 84., ScheduleAverageHoursPerWeek( halfOnSched2Index, 1, false ) );

}

TEST_F( EnergyPlusFixture, ScheduleHoursGT1perc_test )
{
	// J.Glazer - August 2017

	std::string const idf_objects = delimited_string( {
		"Version,8.8;",
		" ",
		"ScheduleTypeLimits,",
		"  Any Number;              !- Name",
		" ",
		"Schedule:Compact,",
		" OnSched,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" OffSched,                 !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 0.0;        !- Field 3",
		" ",
		"Schedule:Compact,",
		" JanOnSched,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 1/31,            !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 1.0,        !- Field 26",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 24:00, 0.0;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" HalfOnSched,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 12:00, 1.0,        !- Field 26",
		" Until: 24:00, 0.0;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" HalfOnSched2,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 12:00, 0.75,        !- Field 26",
		" Until: 24:00, 0.25;        !- Field 26",
		" ",
		"Schedule:Compact,",
		" HalfOnSched3,                  !- Name",
		" Any Number,               !- Schedule Type Limits Name",
		" Through: 12/31,           !- Field 1",
		" For: AllDays,             !- Field 2",
		" Until: 12:00, 0.2,        !- Field 26",
		" Until: 24:00, 0.0;        !- Field 26",
		" ",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	DataGlobals::NumOfTimeStepInHour = 4;
	DataGlobals::MinutesPerTimeStep = 15;
	DataGlobals::TimeStepZone = 0.25;

	int onSchedIndex = GetScheduleIndex( "ONSCHED" );
	EXPECT_EQ( 8760., ScheduleHoursGT1perc( onSchedIndex, 1, false ) );

	int offSchedIndex = GetScheduleIndex( "OFFSCHED" );
	EXPECT_EQ( 0., ScheduleHoursGT1perc( offSchedIndex, 1, false ) );

	int janOnSchedIndex = GetScheduleIndex( "JANONSCHED" );
	EXPECT_EQ( 744., ScheduleHoursGT1perc( janOnSchedIndex, 1, false ) );

	int halfOnSchedIndex = GetScheduleIndex( "HALFONSCHED" );
	EXPECT_EQ( 4380., ScheduleHoursGT1perc( halfOnSchedIndex, 1, false ) );

	int halfOnSched2Index = GetScheduleIndex( "HALFONSCHED2" );
	EXPECT_EQ( 8760., ScheduleHoursGT1perc( halfOnSched2Index, 1, false ) );

	int halfOnSched3Index = GetScheduleIndex( "HALFONSCHED3" );
	EXPECT_EQ( 4380., ScheduleHoursGT1perc( halfOnSched3Index, 1, false ) );

}


