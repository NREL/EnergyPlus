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

TEST_F(EnergyPlusFixture, RunPeriod_Defaults)
{
    RunPeriodData runperiod;
    EXPECT_EQ(WeatherManager::WeekDay::Sunday, runperiod.startWeekDay);

    EXPECT_EQ(1, runperiod.startMonth);
    EXPECT_EQ(1, runperiod.startDay);
    EXPECT_EQ(2017, runperiod.startYear);
    EXPECT_EQ(2457755, runperiod.startJulianDate);

    EXPECT_EQ(12, runperiod.endMonth);
    EXPECT_EQ(31, runperiod.endDay);
    EXPECT_EQ(2017, runperiod.endYear);
    EXPECT_EQ(2458119, runperiod.endJulianDate);

    std::array<Real64, 12> startDays{{1, 4, 4, 7, 2, 5, 7, 3, 6, 1, 4, 6}};
    for (size_t i = 0; i < 12; ++i) {
        EXPECT_EQ(startDays[i], runperiod.monWeekDay[i]);
    }
}

TEST_F(EnergyPlusFixture, RunPeriod_YearTests)
{
    std::string const idf_objects = delimited_string({
        "Version,",
        "8.8;",
        "SimulationControl, NO, NO, NO, YES, YES;",
        "Timestep,4;",
        "RunPeriod,",
        "RP1,                     !- Name",
        "2,                       !- Begin Month",
        "29,                      !- Begin Day of Month",
        "2016,                    !- Begin Year",
        "3,                       !- End Month",
        "3,                       !- End Day of Month",
        ",                        !- End Year",
        "Monday,                  !- Day of Week for Start Day",
        "Yes,                     !- Use Weather File Holidays and Special Days",
        "Yes,                     !- Use Weather File Daylight Saving Period",
        "No,                      !- Apply Weekend Holiday Rule",
        "Yes,                     !- Use Weather File Rain Indicators",
        "Yes;                     !- Use Weather File Snow Indicators",
        "RunPeriod,",
        "RP2,                     !- Name",
        "2,                       !- Begin Month",
        "29,                      !- Begin Day of Month",
        ",                        !- Begin Year",
        "3,                       !- End Month",
        "3,                       !- End Day of Month",
        ",                        !- End Year",
        "Wednesday,               !- Day of Week for Start Day",
        "Yes,                     !- Use Weather File Holidays and Special Days",
        "Yes,                     !- Use Weather File Daylight Saving Period",
        "No,                      !- Apply Weekend Holiday Rule",
        "Yes,                     !- Use Weather File Rain Indicators",
        "Yes;                     !- Use Weather File Snow Indicators",
        "RunPeriod,",
        "RP3,                     !- Name",
        "1,                       !- Begin Month",
        "1,                       !- Begin Day of Month",
        ",                        !- Begin Year",
        "12,                      !- End Month",
        "31,                      !- End Day of Month",
        ",                        !- End Year",
        "Thursday,                !- Day of Week for Start Day",
        "Yes,                     !- Use Weather File Holidays and Special Days",
        "Yes,                     !- Use Weather File Daylight Saving Period",
        "No,                      !- Apply Weekend Holiday Rule",
        "Yes,                     !- Use Weather File Rain Indicators",
        "Yes;                     !- Use Weather File Snow Indicators",
        "RunPeriod,",
        "RP4,                     !- Name",
        "1,                       !- Begin Month",
        "1,                       !- Begin Day of Month",
        ",                        !- Begin Year",
        "12,                      !- End Month",
        "31,                      !- End Day of Month",
        ",                        !- End Year",
        ",                        !- Day of Week for Start Day",
        "Yes,                     !- Use Weather File Holidays and Special Days",
        "Yes,                     !- Use Weather File Daylight Saving Period",
        "No,                      !- Apply Weekend Holiday Rule",
        "Yes,                     !- Use Weather File Rain Indicators",
        "Yes;                     !- Use Weather File Snow Indicators",
        "RunPeriod,",
        "RP5,                     !- Name",
        "8,                       !- Begin Month",
        "18,                      !- Begin Day of Month",
        ",                        !- Begin Year",
        "12,                      !- End Month",
        "31,                      !- End Day of Month",
        ",                        !- End Year",
        "Wednesday,               !- Day of Week for Start Day",
        "Yes,                     !- Use Weather File Holidays and Special Days",
        "Yes,                     !- Use Weather File Daylight Saving Period",
        "No,                      !- Apply Weekend Holiday Rule",
        "Yes,                     !- Use Weather File Rain Indicators",
        "Yes;                     !- Use Weather File Snow Indicators",
        "RunPeriod,",
        "RP6,                     !- Name",
        "2,                       !- Begin Month",
        "29,                      !- Begin Day of Month",
        ",                        !- Begin Year",
        "12,                      !- End Month",
        "31,                      !- End Day of Month",
        ",                        !- End Year",
        "Saturday,                !- Day of Week for Start Day",
        "Yes,                     !- Use Weather File Holidays and Special Days",
        "Yes,                     !- Use Weather File Daylight Saving Period",
        "No,                      !- Apply Weekend Holiday Rule",
        "Yes,                     !- Use Weather File Rain Indicators",
        "Yes;                     !- Use Weather File Snow Indicators",
        "RunPeriod,",
        "RP7,                     !- Name",
        "1,                       !- Begin Month",
        "1,                       !- Begin Day of Month",
        "2016,                    !- Begin Year",
        "3,                       !- End Month",
        "31,                      !- End Day of Month",
        "2020,                    !- End Year",
        ",                        !- Day of Week for Start Day",
        "Yes,                     !- Use Weather File Holidays and Special Days",
        "Yes,                     !- Use Weather File Daylight Saving Period",
        "No,                      !- Apply Weekend Holiday Rule",
        "Yes,                     !- Use Weather File Rain Indicators",
        "Yes;                     !- Use Weather File Snow Indicators",
        "BUILDING, Simple One Zone (Wireframe DXF), 0.0, Suburbs, .04, .004, MinimalShadowing, 30, 6;",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool errors_in_input(false);
    int totalrps(7);
    WeatherManager::GetRunPeriodData(totalrps, errors_in_input);
    EXPECT_FALSE(errors_in_input);

    EXPECT_EQ(WeatherManager::WeekDay::Monday, WeatherManager::RunPeriodInput[0].startWeekDay);
    EXPECT_EQ(2016, WeatherManager::RunPeriodInput[0].startYear);
    EXPECT_EQ(2457448, WeatherManager::RunPeriodInput[0].startJulianDate);
    EXPECT_EQ(2457451, WeatherManager::RunPeriodInput[0].endJulianDate);

    EXPECT_EQ(WeatherManager::WeekDay::Wednesday, WeatherManager::RunPeriodInput[1].startWeekDay);
    EXPECT_EQ(2012, WeatherManager::RunPeriodInput[1].startYear);
    EXPECT_EQ(2455987, WeatherManager::RunPeriodInput[1].startJulianDate);
    EXPECT_EQ(2455990, WeatherManager::RunPeriodInput[1].endJulianDate);

    EXPECT_EQ(WeatherManager::WeekDay::Thursday, WeatherManager::RunPeriodInput[2].startWeekDay);
    EXPECT_EQ(2015, WeatherManager::RunPeriodInput[2].startYear);
    EXPECT_EQ(2457024, WeatherManager::RunPeriodInput[2].startJulianDate);
    EXPECT_EQ(2457388, WeatherManager::RunPeriodInput[2].endJulianDate);

    EXPECT_EQ(WeatherManager::WeekDay::Sunday, WeatherManager::RunPeriodInput[3].startWeekDay);
    EXPECT_EQ(2017, WeatherManager::RunPeriodInput[3].startYear);
    EXPECT_EQ(2457755, WeatherManager::RunPeriodInput[3].startJulianDate);
    EXPECT_EQ(2458119, WeatherManager::RunPeriodInput[3].endJulianDate);
    // This is the default, check that it works properly
    std::array<Real64, 12> startDays{ {1, 4, 4, 7, 2, 5, 7, 3, 6, 1, 4, 6} };
    for (size_t i = 0; i < 12; ++i) {
        EXPECT_EQ(startDays[i], WeatherManager::RunPeriodInput[3].monWeekDay[i]);
    }

    EXPECT_EQ(WeatherManager::WeekDay::Wednesday, WeatherManager::RunPeriodInput[4].startWeekDay);
    EXPECT_EQ(2010, WeatherManager::RunPeriodInput[4].startYear);
    EXPECT_EQ(2455427, WeatherManager::RunPeriodInput[4].startJulianDate);
    EXPECT_EQ(2455562, WeatherManager::RunPeriodInput[4].endJulianDate);

    EXPECT_EQ(WeatherManager::WeekDay::Saturday, WeatherManager::RunPeriodInput[5].startWeekDay);
    EXPECT_EQ(1992, WeatherManager::RunPeriodInput[5].startYear);
    EXPECT_EQ(2448682, WeatherManager::RunPeriodInput[5].startJulianDate);
    EXPECT_EQ(2448988, WeatherManager::RunPeriodInput[5].endJulianDate);

    EXPECT_EQ(WeatherManager::WeekDay::Friday, WeatherManager::RunPeriodInput[6].startWeekDay);
    EXPECT_EQ(2016, WeatherManager::RunPeriodInput[6].startYear);
    EXPECT_EQ(2457389, WeatherManager::RunPeriodInput[6].startJulianDate);
    EXPECT_EQ(2458940, WeatherManager::RunPeriodInput[6].endJulianDate);
}

TEST_F(EnergyPlusFixture, RunPeriod_EndYearOnly)
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
        "Yes,                     !- Use Weather File Holidays and Special Days",
        "Yes,                     !- Use Weather File Daylight Saving Period",
        "No,                      !- Apply Weekend Holiday Rule",
        "Yes,                     !- Use Weather File Rain Indicators",
        "Yes;                     !- Use Weather File Snow Indicators",
        "BUILDING, Simple One Zone (Wireframe DXF), 0.0, Suburbs, .04, .004, MinimalShadowing, 30, 6;",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool errors_in_input(false);
    int totalrps(1);
    WeatherManager::GetRunPeriodData(totalrps, errors_in_input);

    EXPECT_TRUE(errors_in_input);
}
