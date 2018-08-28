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

// EnergyPlus::WeatherManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataSurfaces.hh>
#include <OutputReports.hh>
#include <ScheduleManager.hh>
#include <SurfaceGeometry.hh>
#include <WeatherManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::WeatherManager;
using namespace EnergyPlus::ScheduleManager;

TEST_F(EnergyPlusFixture, SkyTempTest)
{
    std::string const idf_objects = delimited_string({
        "Version,",
        "8.3;",
        "SimulationControl, NO, NO, NO, YES, YES;",
        "Timestep,4;",
        "RunPeriod,",
        ",                        !- Name",
        "2,                       !- Begin Month",
        "27,                      !- Begin Day of Month",
        ",                        !- Begin Year",
        "3,                       !- End Month",
        "3,                       !- End Day of Month",
        ",                        !- End Year",
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

    ASSERT_TRUE(process_idf(idf_objects));
    Array2D<Real64> TomorrowSkyTemp; // Sky temperature
    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
    TomorrowSkyTemp.allocate(DataGlobals::NumOfTimeStepInHour, 24);
    TomorrowSkyTemp = 0.0;

    // Febuary 27
    ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 58, 3);
    EXPECT_NEAR(2.27, TomorrowSkyTemp(1, 1), .001);

    // Febuary 28
    ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 59, 4);
    EXPECT_NEAR(2.28, TomorrowSkyTemp(1, 1), .001);

    // March 1
    ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 60, 5);
    EXPECT_NEAR(3.01, TomorrowSkyTemp(1, 1), .001);

    // Not March 2, this "Day" is ignored unless its a leap year, otherwise same data as March 1
    ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 61, 6);
    EXPECT_NEAR(3.01, TomorrowSkyTemp(1, 1), .001);

    // March 2
    ScheduleManager::GetScheduleValuesForDay(1, TomorrowSkyTemp, 62, 6);
    EXPECT_NEAR(3.02, TomorrowSkyTemp(1, 1), .001);
}

TEST_F(EnergyPlusFixture, WaterMainsCorrelationTest)
{
    using DataEnvironment::DayOfYear;
    using DataEnvironment::Latitude;
    using DataEnvironment::WaterMainsTemp;

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

TEST_F(EnergyPlusFixture, JGDate_Test)
{
    // used http://aa.usno.navy.mil/data/docs/JulianDate.php
    //
    int julianDate;
    int gregorianYear;
    int gregorianMonth;
    int gregorianDay;

    gregorianYear = 2016; // when test was made
    gregorianMonth = 5;
    gregorianDay = 25;
    JGDate(GregorianToJulian, julianDate, gregorianYear, gregorianMonth, gregorianDay);
    EXPECT_EQ(2457534, julianDate);
    JGDate(JulianToGregorian, julianDate, gregorianYear, gregorianMonth, gregorianDay);
    EXPECT_EQ(2016, gregorianYear);
    EXPECT_EQ(5, gregorianMonth);
    EXPECT_EQ(25, gregorianDay);

    gregorianYear = 2015; // a year before when test was made
    gregorianMonth = 5;
    gregorianDay = 25;
    JGDate(GregorianToJulian, julianDate, gregorianYear, gregorianMonth, gregorianDay);
    EXPECT_EQ(2457168, julianDate);
    JGDate(JulianToGregorian, julianDate, gregorianYear, gregorianMonth, gregorianDay);
    EXPECT_EQ(2015, gregorianYear);
    EXPECT_EQ(5, gregorianMonth);
    EXPECT_EQ(25, gregorianDay);

    gregorianYear = 1966; // a fine date in history
    gregorianMonth = 7;
    gregorianDay = 16;
    JGDate(GregorianToJulian, julianDate, gregorianYear, gregorianMonth, gregorianDay);
    EXPECT_EQ(2439323, julianDate);
    JGDate(JulianToGregorian, julianDate, gregorianYear, gregorianMonth, gregorianDay);
    EXPECT_EQ(1966, gregorianYear);
    EXPECT_EQ(7, gregorianMonth);
    EXPECT_EQ(16, gregorianDay);

    gregorianYear = 2000; // complex leap year
    gregorianMonth = 12;
    gregorianDay = 31;
    JGDate(GregorianToJulian, julianDate, gregorianYear, gregorianMonth, gregorianDay);
    EXPECT_EQ(2451910, julianDate);
    JGDate(JulianToGregorian, julianDate, gregorianYear, gregorianMonth, gregorianDay);
    EXPECT_EQ(2000, gregorianYear);
    EXPECT_EQ(12, gregorianMonth);
    EXPECT_EQ(31, gregorianDay);
}

TEST_F(EnergyPlusFixture, interpolateWindDirectionTest)
{
    // simple test in each quadrant
    EXPECT_EQ(interpolateWindDirection(0, 90, 0.5), 45.);
    EXPECT_EQ(interpolateWindDirection(10, 80, 0.5), 45.);
    EXPECT_EQ(interpolateWindDirection(20, 80, 0.7), 62.);
    EXPECT_EQ(interpolateWindDirection(20, 80, 0.3), 38.);

    EXPECT_EQ(interpolateWindDirection(90, 180, 0.5), 135.);
    EXPECT_EQ(interpolateWindDirection(100, 170, 0.5), 135.);
    EXPECT_EQ(interpolateWindDirection(110, 170, 0.7), 152.);
    EXPECT_EQ(interpolateWindDirection(110, 170, 0.3), 128.);

    EXPECT_EQ(interpolateWindDirection(180, 270, 0.5), 225.);
    EXPECT_EQ(interpolateWindDirection(190, 260, 0.5), 225.);
    EXPECT_EQ(interpolateWindDirection(200, 260, 0.7), 242.);
    EXPECT_EQ(interpolateWindDirection(200, 260, 0.3), 218.);

    EXPECT_EQ(interpolateWindDirection(270, 360, 0.5), 315.);
    EXPECT_EQ(interpolateWindDirection(280, 350, 0.5), 315.);
    EXPECT_EQ(interpolateWindDirection(290, 350, 0.7), 332.);
    EXPECT_EQ(interpolateWindDirection(290, 350, 0.3), 308.);

    // tests across 180 degree angle
    EXPECT_EQ(interpolateWindDirection(170, 190, 0.7), 184.);
    EXPECT_EQ(interpolateWindDirection(170, 190, 0.3), 176.);
    EXPECT_EQ(interpolateWindDirection(100, 260, 0.7), 212.);
    EXPECT_EQ(interpolateWindDirection(100, 260, 0.3), 148.);

    // tests across 0 degree angle (which was issue #5682)
    EXPECT_EQ(interpolateWindDirection(350, 10, 0.7), 4.);
    EXPECT_EQ(interpolateWindDirection(350, 10, 0.3), 356.);
    EXPECT_EQ(interpolateWindDirection(300, 80, 0.7), 38.);
    EXPECT_EQ(interpolateWindDirection(300, 80, 0.3), 342.);

    EXPECT_EQ(interpolateWindDirection(350, 10, 0.5), 0.);
    EXPECT_EQ(interpolateWindDirection(340, 10, 0.5), 355.);
    EXPECT_EQ(interpolateWindDirection(280, 10, 0.5), 325.);
    EXPECT_EQ(interpolateWindDirection(260, 10, 0.5), 315.);
    EXPECT_EQ(interpolateWindDirection(200, 10, 0.7), 319.);
    EXPECT_EQ(interpolateWindDirection(200, 10, 0.3), 251.);
    EXPECT_EQ(interpolateWindDirection(350, 160, 0.7), 109.);
    EXPECT_EQ(interpolateWindDirection(350, 160, 0.3), 41.);

    // tests for new failures
    EXPECT_EQ(interpolateWindDirection(70, 30, 0.25), 60.);

    // tests across 180 degree angle (reversed)
    EXPECT_EQ(interpolateWindDirection(190, 170, 0.3), 184.);
    EXPECT_EQ(interpolateWindDirection(190, 170, 0.7), 176.);
    EXPECT_EQ(interpolateWindDirection(260, 100, 0.3), 212.);
    EXPECT_EQ(interpolateWindDirection(260, 100, 0.7), 148.);

    // tests across 0 degree angle (reversed)
    EXPECT_EQ(interpolateWindDirection(10, 350, 0.3), 4.);
    EXPECT_EQ(interpolateWindDirection(10, 350, 0.7), 356.);
    EXPECT_EQ(interpolateWindDirection(80, 300, 0.3), 38.);
    EXPECT_EQ(interpolateWindDirection(80, 300, 0.7), 342.);

    EXPECT_EQ(interpolateWindDirection(10, 350, 0.5), 0.);
    EXPECT_EQ(interpolateWindDirection(10, 340, 0.5), 355.);
    EXPECT_EQ(interpolateWindDirection(10, 280, 0.5), 325.);
    EXPECT_EQ(interpolateWindDirection(10, 260, 0.5), 315.);
    EXPECT_EQ(interpolateWindDirection(10, 200, 0.3), 319.);
    EXPECT_EQ(interpolateWindDirection(10, 200, 0.7), 251.);
    EXPECT_EQ(interpolateWindDirection(160, 350, 0.3), 109.);
    EXPECT_EQ(interpolateWindDirection(160, 350, 0.7), 41.);
}

TEST_F(EnergyPlusFixture, UnderwaterBoundaryConditionFullyPopulated)
{

    std::string const idf_objects =
        delimited_string({"SurfaceProperty:Underwater, UnderwaterSurfaceName, 31.4159, WaterTempSchedule, WaterVelocitySchedule;",
                          "Schedule:Constant, WaterTempSchedule, , 30;",
                          "Schedule:Constant, WaterVelocitySchedule, , 3.0;"
                          "SurfaceProperty:OtherSideConditionsModel, UnderwaterSurfaceName, ConvectiveUnderwater;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // need to populate the OSCM array by calling the get input for it
    bool errorsFound = false;
    SurfaceGeometry::GetOSCMData(errorsFound);
    EXPECT_FALSE(errorsFound);
    EXPECT_EQ(DataSurfaces::TotOSCM, 1);

    // then process the input for this underwater surface
    bool shouldBeTrue = WeatherManager::CheckIfAnyUnderwaterBoundaries();
    EXPECT_TRUE(shouldBeTrue);
    EXPECT_EQ(WeatherManager::underwaterBoundaries[0].Name, "UNDERWATERSURFACENAME");
    EXPECT_NEAR(WeatherManager::underwaterBoundaries[0].distanceFromLeadingEdge, 31.4159, 0.0001);
    EXPECT_EQ(WeatherManager::underwaterBoundaries[0].OSCMIndex, 1);
    EXPECT_EQ(WeatherManager::underwaterBoundaries[0].WaterTempScheduleIndex, 1);
    EXPECT_EQ(WeatherManager::underwaterBoundaries[0].VelocityScheduleIndex, 2);
}

TEST_F(EnergyPlusFixture, UnderwaterBoundaryConditionMissingVelocityOK)
{

    std::string const idf_objects = delimited_string({"SurfaceProperty:Underwater, UnderwaterSurfaceName, 31.4159, WaterTempSchedule, ;",
                                                      "Schedule:Constant, WaterTempSchedule, , 30;",
                                                      "SurfaceProperty:OtherSideConditionsModel, UnderwaterSurfaceName, ConvectiveUnderwater;"});
    ASSERT_TRUE(process_idf(idf_objects));

    // need to populate the OSCM array by calling the get input for it
    bool errorsFound = false;
    SurfaceGeometry::GetOSCMData(errorsFound);
    EXPECT_FALSE(errorsFound);
    EXPECT_EQ(DataSurfaces::TotOSCM, 1);

    // then process the input for this underwater surface
    bool shouldBeTrue = WeatherManager::CheckIfAnyUnderwaterBoundaries();
    EXPECT_TRUE(shouldBeTrue);
    EXPECT_EQ(WeatherManager::underwaterBoundaries[0].Name, "UNDERWATERSURFACENAME");
    EXPECT_NEAR(WeatherManager::underwaterBoundaries[0].distanceFromLeadingEdge, 31.4159, 0.0001);
    EXPECT_EQ(WeatherManager::underwaterBoundaries[0].OSCMIndex, 1);
    EXPECT_EQ(WeatherManager::underwaterBoundaries[0].WaterTempScheduleIndex, 1);
    EXPECT_EQ(WeatherManager::underwaterBoundaries[0].VelocityScheduleIndex, 0);
}

TEST_F(EnergyPlusFixture, UnderwaterBoundaryConditionConvectionCoefficients)
{
    EXPECT_NEAR(2483.702, WeatherManager::calculateWaterBoundaryConvectionCoefficient(30.0, 3.0, 30.0), 0.01);
    EXPECT_NEAR(2162.188, WeatherManager::calculateWaterBoundaryConvectionCoefficient(30.0, 3.0, 60.0), 0.01);
    EXPECT_NEAR(1993.771, WeatherManager::calculateWaterBoundaryConvectionCoefficient(30.0, 3.0, 90.0), 0.01);
    EXPECT_NEAR(1882.294, WeatherManager::calculateWaterBoundaryConvectionCoefficient(30.0, 3.0, 120.0), 0.01);
    EXPECT_NEAR(1800.136, WeatherManager::calculateWaterBoundaryConvectionCoefficient(30.0, 3.0, 150.0), 0.01);
}

TEST_F(EnergyPlusFixture, WaterMainsCorrelationFromWeatherFileTest)
{
    using DataEnvironment::DayOfYear;
    using DataEnvironment::Latitude;
    using DataEnvironment::WaterMainsTemp;
    using WeatherManager::OADryBulbAverage;

    std::string const idf_objects = delimited_string({
        "   Site:WaterMainsTemperature,",
        "   CorrelationFromWeatherFile,  !- Calculation Method",
        "   ,                            !- Temperature Schedule Name",
        "   9.99,                        !- Annual Average Outdoor Air Temperature {C}",
        "  28.78;                        !- Maximum Difference In Monthly Average Outdoor Air Temperatures {deltaC}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool foundErrors(false);
    WeatherManager::GetWaterMainsTemperatures(foundErrors);
    EXPECT_FALSE(foundErrors); // expect no errors
    EXPECT_EQ(WeatherManager::WaterMainsTempsMethod, WeatherManager::CorrelationFromWeatherFileMethod);
    // for calculation method CorrelationFromWeatherFile these parameters are ignored
    EXPECT_EQ(WeatherManager::WaterMainsTempsAnnualAvgAirTemp, 0.0);
    EXPECT_EQ(WeatherManager::WaterMainsTempsMaxDiffAirTemp, 0.0);

    // set water mains parameters for CorrelationFromWeatherFile method
    OADryBulbAverage.AnnualAvgOADryBulbTemp = 9.99;
    OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff = 28.78;
    OADryBulbAverage.OADryBulbWeatherDataProcessed = true;
    DataEnvironment::Latitude = 42.00; // CHICAGO_IL_USA_WMO_725300

    // January 15th water mains temperature test
    DataEnvironment::DayOfYear = 15; // January 15th
    WeatherManager::CalcWaterMainsTemp();
    EXPECT_NEAR(DataEnvironment::WaterMainsTemp, 7.5145, 0.0001);

    // July 15th water mains temperature test
    DataEnvironment::DayOfYear = 196; // July 15th
    WeatherManager::CalcWaterMainsTemp();
    EXPECT_NEAR(DataEnvironment::WaterMainsTemp, 19.0452, 0.0001);
}

TEST_F(EnergyPlusFixture, WaterMainsCorrelationFromStatFileTest)
{
    using DataEnvironment::DayOfYear;
    using DataEnvironment::Latitude;
    using DataEnvironment::WaterMainsTemp;
    using WeatherManager::OADryBulbAverage;

    int AnnualNumberOfDays(0);
    Real64 MonthlyDailyDryBulbMin(0.0);
    Real64 MonthlyDailyDryBulbMax(0.0);
    Real64 AnnualDailyAverageDryBulbTempSum(0.0);

    std::string const idf_objects = delimited_string({
        "   Site:WaterMainsTemperature,",
        "   CorrelationFromWeatherFile,  !- Calculation Method",
        "   ,                            !- Temperature Schedule Name",
        "   9.99,                        !- Annual Average Outdoor Air Temperature {C}",
        "  28.78;                        !- Maximum Difference In Monthly Average Outdoor Air Temperatures {deltaC}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool foundErrors(false);
    WeatherManager::GetWaterMainsTemperatures(foundErrors);
    EXPECT_FALSE(foundErrors); // expect no errors
    EXPECT_EQ(WeatherManager::WaterMainsTempsMethod, WeatherManager::CorrelationFromWeatherFileMethod);
    // for calculation method CorrelationFromWeatherFile these parameters are ignored
    EXPECT_EQ(WeatherManager::WaterMainsTempsAnnualAvgAirTemp, 0.0);
    EXPECT_EQ(WeatherManager::WaterMainsTempsMaxDiffAirTemp, 0.0);

    Array1D<Real64> MonthlyDryBulbTempFromStatFile(12, {-4.60, -2.50, 3.80, 10.00, 15.30, 21.10, 24.10, 21.80, 18.10, 11.00, 4.70, -3.70});
    OADryBulbAverage.MonthlyDailyAverageDryBulbTemp = MonthlyDryBulbTempFromStatFile;

    // calc water mains parameters for CorrelationFromWeatherFile method
    for (int i = 1; i <= 12; ++i) {
        AnnualDailyAverageDryBulbTempSum += OADryBulbAverage.MonthlyDailyAverageDryBulbTemp(i) * EndDayOfMonth(i);
        MonthlyDailyDryBulbMin = min(MonthlyDailyDryBulbMin, OADryBulbAverage.MonthlyDailyAverageDryBulbTemp(i));
        MonthlyDailyDryBulbMax = max(MonthlyDailyDryBulbMax, OADryBulbAverage.MonthlyDailyAverageDryBulbTemp(i));
        AnnualNumberOfDays += EndDayOfMonth(i);
    }
    OADryBulbAverage.AnnualAvgOADryBulbTemp = AnnualDailyAverageDryBulbTempSum / AnnualNumberOfDays;
    OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff = MonthlyDailyDryBulbMax - MonthlyDailyDryBulbMin;
    // check results
    EXPECT_NEAR(OADryBulbAverage.AnnualAvgOADryBulbTemp, 9.9882, 0.0001);
    EXPECT_NEAR(OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff, 28.7000, 0.0001);

    // test water mains temperature
    // WeatherManager::WaterMainsTempsMethod = WeatherManager::CorrelationFromWeatherFileMethod;
    OADryBulbAverage.OADryBulbWeatherDataProcessed = true;
    DataEnvironment::Latitude = 42.00; // CHICAGO_IL_USA_WMO_725300

    // January 21st water mains temperature test
    DataEnvironment::DayOfYear = 21; // January 21st
    WeatherManager::CalcWaterMainsTemp();
    EXPECT_NEAR(DataEnvironment::WaterMainsTemp, 7.23463, 0.00001);

    // July 21st water mains temperature test
    DataEnvironment::DayOfYear = 202; // July 21st
    WeatherManager::CalcWaterMainsTemp();
    EXPECT_NEAR(DataEnvironment::WaterMainsTemp, 19.33812, 0.00001);
}
TEST_F(EnergyPlusFixture, WaterMainsOutputReports_CorrelationFromWeatherFileTest)
{

    using DataEnvironment::WaterMainsTemp;
    using WeatherManager::OADryBulbAverage;

    std::string const idf_objects = delimited_string({
        "   Site:WaterMainsTemperature,",
        "   CorrelationFromWeatherFile,  !- Calculation Method",
        "   ,                            !- Temperature Schedule Name",
        "   9.99,                        !- Annual Average Outdoor Air Temperature {C}",
        "  28.78;                        !- Maximum Difference In Monthly Average Outdoor Air Temperatures {deltaC}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool foundErrors(false);
    WeatherManager::GetWaterMainsTemperatures(foundErrors);
    EXPECT_FALSE(foundErrors); // expect no errors
    EXPECT_EQ(WeatherManager::WaterMainsTempsMethod, WeatherManager::CorrelationFromWeatherFileMethod);
    // for calculation method CorrelationFromWeatherFile these two parameters are ignored
    EXPECT_EQ(WeatherManager::WaterMainsTempsAnnualAvgAirTemp, 0.0);
    EXPECT_EQ(WeatherManager::WaterMainsTempsMaxDiffAirTemp, 0.0);

    DataGlobals::OutputFileInits = GetNewUnitNumber();
    // set water mains temp parameters for CorrelationFromWeatherFile method
    OADryBulbAverage.AnnualAvgOADryBulbTemp = 9.99;
    OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff = 28.78;
    OADryBulbAverage.OADryBulbWeatherDataProcessed = true;

    // report water mains parameters to eio file
    WeatherManager::ReportWaterMainsTempParameters();

    std::string const eiooutput = delimited_string({"! <Site Water Mains Temperature Information>,"
                                                    "Calculation Method{},"
                                                    "Water Mains Temperature Schedule Name{},"
                                                    "Annual Average Outdoor Air Temperature{C},"
                                                    "Maximum Difference In Monthly Average Outdoor Air Temperatures{deltaC},"
                                                    "Fixed Default Water Mains Temperature{C}",
                                                    "Site Water Mains Temperature Information,CorrelationFromWeatherFile,NA,9.99,28.78,NA"});

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}
TEST_F(EnergyPlusFixture, ASHRAE_Tau2017ModelTest)
{
    std::string const idf_objects = delimited_string({
        "  Version,9.0;",

        "  SizingPeriod:DesignDay,",
        "    Atlanta Jan 21 cooling,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    16.9,                    !- Maximum Dry-Bulb Temperature {C}",
        "    11.6,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    WetBulbProfileDefaultMultipliers,  !- Humidity Condition Type",
        "    13.2,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    8,                       !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    97620,                   !- Barometric Pressure {Pa}",
        "    0.0,                     !- Wind Speed {m/s}",
        "    0.0,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAETau2017,           !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    0.325,                   !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    2.461;                   !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",

        "  SizingPeriod:DesignDay,",
        "    Atlanta Jul 21 cooling,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    33.3,                    !- Maximum Dry-Bulb Temperature {C}",
        "    11.5,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    WetBulbProfileDefaultMultipliers,  !- Humidity Condition Type",
        "    23.5,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    3.5,                     !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    97620,                   !- Barometric Pressure {Pa}",
        "    0.0,                     !- Wind Speed {m/s}",
        "    0.0,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAETau2017,           !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    .556,                    !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    1.779;                   !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    DataEnvironment::TotDesDays = 2;
    // setup environment state
    Environment.allocate(DataEnvironment::TotDesDays);
    DesignDay.allocate(DataEnvironment::TotDesDays);
    Environment(1).DesignDayNum = 1;
    Environment(2).DesignDayNum = 2;
    GetDesignDayData(DataEnvironment::TotDesDays, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    // init local variables
    Real64 ETR = 1367.0;
    Real64 BeamRad(0.0);
    Real64 DiffRad(0.0);
    Real64 GloHorzRad(0.0);

    // EnvrnNum = 1 uses Tau values of January
    int EnvrnNum = 1;
    Real64 CosZenith = 1.0; // assumed zero zenith angle
    Real64 TauB = DesDayInput(EnvrnNum).TauB;
    Real64 TauD = DesDayInput(EnvrnNum).TauD;
    // check tau values
    EXPECT_EQ(4, DesDayInput(EnvrnNum).SolarModel);
    EXPECT_EQ(0.325, TauB);
    EXPECT_EQ(2.461, TauD);
    // calc expected values for environment 1
    Real64 AB = 1.454 - 0.406 * TauB - 0.268 * TauD + 0.021 * TauB * TauD;
    Real64 AD = 0.507 + 0.205 * TauB - 0.080 * TauD - 0.190 * TauB * TauD;
    Real64 M = AirMass(CosZenith);
    Real64 expectedIDirN = ETR * std::exp(-TauB * std::pow(M, AB));
    Real64 expectedIDifH = ETR * std::exp(-TauD * std::pow(M, AD));
    Real64 expectedIGlbH = expectedIDirN * CosZenith + expectedIDifH;
    // calc TauModel
    ASHRAETauModel(DesDayInput(EnvrnNum).SolarModel, ETR, CosZenith, TauB, TauD, BeamRad, DiffRad, GloHorzRad);
    // check the coefficients are correctly applied
    EXPECT_EQ(expectedIDirN, BeamRad);
    EXPECT_EQ(expectedIDifH, DiffRad);
    EXPECT_EQ(expectedIGlbH, GloHorzRad);

    // EnvrnNum = 2 uses Tau values of July
    EnvrnNum = 2;
    CosZenith = 1.0; // assumed zero zenith angle
    TauB = DesDayInput(EnvrnNum).TauB;
    TauD = DesDayInput(EnvrnNum).TauD;
    // check tau values
    EXPECT_EQ(0.556, TauB);
    EXPECT_EQ(1.779, TauD);
    // calc expected values for environment 2
    AB = 1.454 - 0.406 * TauB - 0.268 * TauD + 0.021 * TauB * TauD;
    AD = 0.507 + 0.205 * TauB - 0.080 * TauD - 0.190 * TauB * TauD;
    M = AirMass(CosZenith);
    expectedIDirN = ETR * std::exp(-TauB * std::pow(M, AB));
    expectedIDifH = ETR * std::exp(-TauD * std::pow(M, AD));
    expectedIGlbH = expectedIDirN * CosZenith + expectedIDifH;
    // reset the arguments to zero
    BeamRad = 0.0;
    DiffRad = 0.0;
    GloHorzRad = 0.0;
    // calc TauModel
    ASHRAETauModel(DesDayInput(EnvrnNum).SolarModel, ETR, CosZenith, TauB, TauD, BeamRad, DiffRad, GloHorzRad);
    // check the coefficients are correctly applied
    EXPECT_EQ(expectedIDirN, BeamRad);
    EXPECT_EQ(expectedIDifH, DiffRad);
    EXPECT_EQ(expectedIGlbH, GloHorzRad);
}
