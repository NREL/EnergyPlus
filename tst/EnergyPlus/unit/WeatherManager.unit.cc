// EnergyPlus::WeatherManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <WeatherManager.hh>
#include <ScheduleManager.hh>
#include <DataGlobals.hh>
#include <DataEnvironment.hh>


#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::WeatherManager;
using namespace EnergyPlus::ScheduleManager;

TEST_F(EnergyPlusFixture, SkyTempTest )
{
	std::string const idf_objects = delimited_string({
		"Version,",
		"8.3;",
		"SimulationControl, NO, NO, NO, YES, YES;",
		"Timestep,4;",
		"RunPeriod,",
		",                        !- Name",
		"2,                       !- Begin Month",
		"27,                       !- Begin Day of Month",
		"3,                      !- End Month",
		"3,                      !- End Day of Month",
		"Tuesday,                 !- Day of Week for Start Day",
		"Yes,                     !- Use Weather File Holidays and Special Days",
		"Yes,                     !- Use Weather File Daylight Saving Period",
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
	
	ASSERT_FALSE(process_idf(idf_objects));
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

TEST_F(EnergyPlusFixture, WaterMainsCorrelationTest)
{
	using DataEnvironment::WaterMainsTemp;
	using DataEnvironment::Latitude;
	using DataEnvironment::DayOfYear;

	WaterMainsTempsMethod = WeatherManager::CorrelationMethod;
	WaterMainsTempsAnnualAvgAirTemp = 9.69;
	WaterMainsTempsMaxDiffAirTemp = 28.1;
	DayOfYear = 50;

	Latitude = 40.0;
	CalcWaterMainsTemp();
	EXPECT_NEAR(WaterMainsTemp, 6.6667, 0.0001);

	Latitude = -40.0;
	CalcWaterMainsTemp();
	EXPECT_NEAR(WaterMainsTemp, 19.3799, 0.0001);
}

