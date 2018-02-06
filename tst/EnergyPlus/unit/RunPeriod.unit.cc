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

// EnergyPlus::RunPeriod Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataSurfaces.hh>
#include <ScheduleManager.hh>
#include <SurfaceGeometry.hh>
#include <WeatherManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::WeatherManager;
using namespace EnergyPlus::ScheduleManager;

TEST_F( EnergyPlusFixture, RunPeriod_SkyTempTest )
{
	std::string const idf_objects = delimited_string({
		"Version,",
		"8.8;",
		"SimulationControl, NO, NO, NO, YES, YES;",
		"Timestep,4;",
		"RunPeriod,",
		",                        !- Name",
		"2,                       !- Begin Month",
		"29,                      !- Begin Day of Month",
		"2016,                    !- Begin Year",
		"3,                       !- End Month",
		"3,                       !- End Day of Month",
		",                        !- End Year",
		"Tuesday,                 !- Day of Week for Start Day",
		"No,                      !- Apply Weekend Holiday Rule",
		"Yes,                     !- Use Weather File Rain Indicators",
		"Yes;                     !- Use Weather File Snow Indicators",
		"BUILDING, Simple One Zone (Wireframe DXF), 0.0, Suburbs, .04, .004, MinimalShadowing, 30, 6;",
		"Schedule:Compact,",
		"TskySchedule,                !- Name",
		",              !- Schedule Type Limits Name",
		"Through: 2/26, For: AllOtherDays,  Until: 24:00, 2.26,",
		"Through: 2/27, For: AllOtherDays,  Until: 24:00, 2.27,",
		"Through: 2/28, For: AllOtherDays,  Until: 24:00, 2.28,",
		"Through: 3/1, For: AllOtherDays,  Until: 24:00, 3.01,",
		"Through: 3/2, For: AllOtherDays,  Until: 24:00, 3.02,",
		"Through: 12/31, For: AllOtherDays,  Until: 24:00, 12.31;",
		"WeatherProperty:SkyTemperature,",
		",                        !- Name",
		"ScheduleValue,           !- Calculation Type",
		"TskySchedule;                  !- Schedule Name",
		"Site:WaterMainsTemperature,",
		"Schedule,             !- Calculation Method",
		"TskySchedule,                        !- Temperature Schedule Name",
		",                   !- Annual Average Outdoor Air Temperature{ C }",
		";                   !- Maximum Difference In Monthly Average Outdoor Air Temperatures{ deltaC }",
		"Output:Variable,*,Schedule Value,hourly;",
		"Output:Variable,*,Site Sky Temperature,hourly;",
		"Output:Variable,*,Site Mains Water Temperature,hourly; !- Zone Average[C]",
		"  Site:Location,",
		"    USA IL-CHICAGO-OHARE,    !- Name",
		"    41.77,                   !- Latitude {deg}",
		"    -87.75,                  !- Longitude {deg}",
		"    -6.00,                   !- Time Zone {hr}",
		"    190;                     !- Elevation {m}",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	bool errors;
	int totalrps( 1 );
	WeatherManager::GetRunPeriodData( totalrps, errors) ;
	EXPECT_EQ( WeatherManager::WeekDay::Tuesday, WeatherManager::RunPeriodInput[ 0 ].startWeekDay );
	EXPECT_EQ( 2007, WeatherManager::RunPeriodInput[ 0 ].startYear );
	EXPECT_EQ( 2454159, WeatherManager::RunPeriodInput[ 0 ].startJulianDate );
	EXPECT_EQ( 2454163, WeatherManager::RunPeriodInput[ 0 ].endJulianDate);

	Array2D< Real64 > TomorrowSkyTemp; // Sky temperature
	DataGlobals::NumOfTimeStepInHour = 4;
	DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
	TomorrowSkyTemp.allocate( DataGlobals::NumOfTimeStepInHour, 24 );
	TomorrowSkyTemp = 0.0;

	//Febuary 27
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 58, 3);
	EXPECT_NEAR(2.27, TomorrowSkyTemp(1,1), .001);

	//Febuary 28
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 59, 4);
	EXPECT_NEAR(2.28, TomorrowSkyTemp(1, 1), .001);

	//March 1
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 60, 5);
	EXPECT_NEAR(3.01, TomorrowSkyTemp(1, 1), .001);

	//Not March 2, this "Day" is ignored unless its a leap year, otherwise same data as March 1
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 61, 6);
	EXPECT_NEAR(3.01, TomorrowSkyTemp(1, 1), .001);

	//March 2
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 62, 6);
	EXPECT_NEAR(3.02, TomorrowSkyTemp(1, 1), .001);
}

TEST_F( EnergyPlusFixture, RunPeriod_BadYears )
{
	std::string const idf_objects = delimited_string({
		"Version,",
		"8.8;",
		"SimulationControl, NO, NO, NO, YES, YES;",
		"Timestep,4;",
		"RunPeriod,",
		",                        !- Name",
		"2,                       !- Begin Month",
		"27,                      !- Begin Day of Month",
		",                        !- Begin Year",
		"3,                       !- End Month",
		"3,                       !- End Day of Month",
		"1997,                    !- End Year",
		"Tuesday,                 !- Day of Week for Start Day",
		"No,                      !- Apply Weekend Holiday Rule",
		"Yes,                     !- Use Weather File Rain Indicators",
		"Yes;                     !- Use Weather File Snow Indicators",
		"BUILDING, Simple One Zone (Wireframe DXF), 0.0, Suburbs, .04, .004, MinimalShadowing, 30, 6;",
		"Schedule:Compact,",
		"TskySchedule,                !- Name",
		",              !- Schedule Type Limits Name",
		"Through: 2/26, For: AllOtherDays,  Until: 24:00, 2.26,",
		"Through: 2/27, For: AllOtherDays,  Until: 24:00, 2.27,",
		"Through: 2/28, For: AllOtherDays,  Until: 24:00, 2.28,",
		"Through: 3/1, For: AllOtherDays,  Until: 24:00, 3.01,",
		"Through: 3/2, For: AllOtherDays,  Until: 24:00, 3.02,",
		"Through: 12/31, For: AllOtherDays,  Until: 24:00, 12.31;",
		"WeatherProperty:SkyTemperature,",
		",                        !- Name",
		"ScheduleValue,           !- Calculation Type",
		"TskySchedule;                  !- Schedule Name",
		"Site:WaterMainsTemperature,",
		"Schedule,             !- Calculation Method",
		"TskySchedule,                        !- Temperature Schedule Name",
		",                   !- Annual Average Outdoor Air Temperature{ C }",
		";                   !- Maximum Difference In Monthly Average Outdoor Air Temperatures{ deltaC }",
		"Output:Variable,*,Schedule Value,hourly;",
		"Output:Variable,*,Site Sky Temperature,hourly;",
		"Output:Variable,*,Site Mains Water Temperature,hourly; !- Zone Average[C]",
		"  Site:Location,",
		"    USA IL-CHICAGO-OHARE,    !- Name",
		"    41.77,                   !- Latitude {deg}",
		"    -87.75,                  !- Longitude {deg}",
		"    -6.00,                   !- Time Zone {hr}",
		"    190;                     !- Elevation {m}",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	bool errors;
	int totalrps( 1 );
	WeatherManager::GetRunPeriodData( totalrps, errors) ;
	EXPECT_EQ( WeatherManager::WeekDay::Tuesday, WeatherManager::RunPeriodInput[ 0 ].startWeekDay );
	EXPECT_EQ( 2007, WeatherManager::RunPeriodInput[ 0 ].startYear );
	EXPECT_EQ( 2454159, WeatherManager::RunPeriodInput[ 0 ].startJulianDate );
	EXPECT_EQ( 2454163, WeatherManager::RunPeriodInput[ 0 ].endJulianDate);

	Array2D< Real64 > TomorrowSkyTemp; // Sky temperature
	DataGlobals::NumOfTimeStepInHour = 4;
	DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
	TomorrowSkyTemp.allocate( DataGlobals::NumOfTimeStepInHour, 24 );
	TomorrowSkyTemp = 0.0;

	//Febuary 27
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 58, 3);
	EXPECT_NEAR(2.27, TomorrowSkyTemp(1,1), .001);

	//Febuary 28
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 59, 4);
	EXPECT_NEAR(2.28, TomorrowSkyTemp(1, 1), .001);

	//March 1
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 60, 5);
	EXPECT_NEAR(3.01, TomorrowSkyTemp(1, 1), .001);

	//Not March 2, this "Day" is ignored unless its a leap year, otherwise same data as March 1
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 61, 6);
	EXPECT_NEAR(3.01, TomorrowSkyTemp(1, 1), .001);

	//March 2
	ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 62, 6);
	EXPECT_NEAR(3.02, TomorrowSkyTemp(1, 1), .001);
}

TEST_F( EnergyPlusFixture, RunPeriod_JGDate_Test )
{
	// used http://aa.usno.navy.mil/data/docs/JulianDate.php
	//
	int julianDate;
	int gregorianYear;
	int gregorianMonth;
	int gregorianDay;

	gregorianYear = 2016;  // when test was made
	gregorianMonth = 5;
	gregorianDay = 25;
	JGDate( GregorianToJulian, julianDate, gregorianYear, gregorianMonth, gregorianDay );
	EXPECT_EQ( 2457534, julianDate );
	JGDate( JulianToGregorian, julianDate, gregorianYear, gregorianMonth, gregorianDay );
	EXPECT_EQ( 2016, gregorianYear );
	EXPECT_EQ( 5, gregorianMonth );
	EXPECT_EQ( 25, gregorianDay );

	gregorianYear = 2015;  // a year before when test was made
	gregorianMonth = 5;
	gregorianDay = 25;
	JGDate( GregorianToJulian, julianDate, gregorianYear, gregorianMonth, gregorianDay );
	EXPECT_EQ( 2457168, julianDate );
	JGDate( JulianToGregorian, julianDate, gregorianYear, gregorianMonth, gregorianDay );
	EXPECT_EQ( 2015, gregorianYear );
	EXPECT_EQ( 5, gregorianMonth );
	EXPECT_EQ( 25, gregorianDay );

	gregorianYear = 1966; // a fine date in history
	gregorianMonth = 7;
	gregorianDay = 16;
	JGDate( GregorianToJulian, julianDate, gregorianYear, gregorianMonth, gregorianDay );
	EXPECT_EQ( 2439323, julianDate );
	JGDate( JulianToGregorian, julianDate, gregorianYear, gregorianMonth, gregorianDay );
	EXPECT_EQ( 1966, gregorianYear );
	EXPECT_EQ( 7, gregorianMonth );
	EXPECT_EQ( 16, gregorianDay );

	gregorianYear = 2000;  //complex leap year
	gregorianMonth = 12;
	gregorianDay = 31;
	JGDate( GregorianToJulian, julianDate, gregorianYear, gregorianMonth, gregorianDay );
	EXPECT_EQ( 2451910, julianDate );
	JGDate( JulianToGregorian, julianDate, gregorianYear, gregorianMonth, gregorianDay );
	EXPECT_EQ( 2000, gregorianYear );
	EXPECT_EQ( 12, gregorianMonth );
	EXPECT_EQ( 31, gregorianDay );

}

