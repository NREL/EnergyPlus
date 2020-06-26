// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/WeatherManager.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::ScheduleManager;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, ScheduleManager_isMinuteMultipleOfTimestep)
{
    // EnergyPlus can accept 1,  2, 3,   4,  5,  6, 10, 12, 15, 20, 30, 60 timesteps per hour which correspond to
    //                      60, 30, 20, 15, 12, 10,  5,  5,  4,  3,  2,  1 minutes per timestep
    EXPECT_TRUE(isMinuteMultipleOfTimestep(0, 15));
    EXPECT_TRUE(isMinuteMultipleOfTimestep(15, 15));
    EXPECT_TRUE(isMinuteMultipleOfTimestep(30, 15));
    EXPECT_TRUE(isMinuteMultipleOfTimestep(45, 15));

    EXPECT_FALSE(isMinuteMultipleOfTimestep(22, 15));
    EXPECT_FALSE(isMinuteMultipleOfTimestep(53, 15));

    EXPECT_TRUE(isMinuteMultipleOfTimestep(0, 12));
    EXPECT_TRUE(isMinuteMultipleOfTimestep(12, 12));
    EXPECT_TRUE(isMinuteMultipleOfTimestep(24, 12));
    EXPECT_TRUE(isMinuteMultipleOfTimestep(36, 12));
    EXPECT_TRUE(isMinuteMultipleOfTimestep(48, 12));

    EXPECT_FALSE(isMinuteMultipleOfTimestep(22, 12));
    EXPECT_FALSE(isMinuteMultipleOfTimestep(53, 12));
}

TEST_F(EnergyPlusFixture, ScheduleManager_UpdateScheduleValues)
{

    ScheduleInputProcessed = true;
    DataEnvironment::DSTIndicator = 0;
    ScheduleManager::NumSchedules = 1;
    ScheduleManager::Schedule.allocate(1);
    Schedule(1).WeekSchedulePointer.allocate(367);
    WeekSchedule.allocate(3);
    WeekSchedule(1).DaySchedulePointer.allocate(12);
    WeekSchedule(2).DaySchedulePointer.allocate(12);
    WeekSchedule(3).DaySchedulePointer.allocate(12);
    DataGlobals::NumOfTimeStepInHour = 1;
    DaySchedule.allocate(3);
    DaySchedule(1).TSValue.allocate(1, 24);
    DaySchedule(2).TSValue.allocate(1, 24);
    DaySchedule(3).TSValue.allocate(1, 24);

    for (int ScheduleIndex = 1; ScheduleIndex <= ScheduleManager::NumSchedules; ScheduleIndex++) {
        for (int i = 1; i <= 366; i++) {
            int x = 1;
            if (i > 250) {
                x = 3;
            } else if (i > 249) {
                x = 2;
            }
            Schedule(ScheduleIndex).WeekSchedulePointer(i) = x;
        }
    }
    for (int WeekSchedulePointer = 1; WeekSchedulePointer <= 3; WeekSchedulePointer++) {
        for (int dayOfWeek = 1; dayOfWeek <= 12; dayOfWeek++) {
            int y = 1;
            if (WeekSchedulePointer == 2) y = 2;
            if (WeekSchedulePointer == 3) y = 3;
            WeekSchedule(WeekSchedulePointer).DaySchedulePointer(dayOfWeek) = y;
        }
    }
    for (int daySchedulePointer = 1; daySchedulePointer <= 3; daySchedulePointer++) {
        for (int whichHour = 1; whichHour <= 24; whichHour++) {
            Real64 schVal = 1.0;
            if (daySchedulePointer == 2) schVal = 2.0;
            if (daySchedulePointer == 3) schVal = 3.0;
            DaySchedule(daySchedulePointer).TSValue(1, whichHour) = schVal;
        }
    }

    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfWeek = 1;
    DataEnvironment::DayOfWeekTomorrow = 2;
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;

    // check day schedules
    EXPECT_EQ(DaySchedule(1).TSValue(1, 1), 1.0); // day < 250 points to this schedule
    EXPECT_EQ(DaySchedule(1).TSValue(1, 24), 1.0);

    EXPECT_EQ(DaySchedule(2).TSValue(1, 1), 2.0); // day = 250 points to this schedule
    EXPECT_EQ(DaySchedule(2).TSValue(1, 24), 2.0);

    EXPECT_EQ(DaySchedule(3).TSValue(1, 1), 3.0); // day > 250 points to this schedule
    EXPECT_EQ(DaySchedule(3).TSValue(1, 24), 3.0);

    // schedule values are 1 through day 249, 2 for day 250, and 3 for remainder of year
    DataEnvironment::DayOfYear_Schedule = 1;
    UpdateScheduleValues();
    // expect 1.0 on day 1
    EXPECT_EQ(Schedule(1).CurrentValue, 1.0);

    DataEnvironment::DayOfYear_Schedule = 250;
    UpdateScheduleValues();
    // expect 2.0 on day 250
    EXPECT_EQ(Schedule(1).CurrentValue, 2.0);

    // test end of day 250 with daylight savings time active
    DataGlobals::HourOfDay = 24;
    DataEnvironment::DSTIndicator = 1;
    UpdateScheduleValues();
    // expect a 3 on day 251, which on day 250 at midnight with DST of hour 1 of day 251
    EXPECT_EQ(Schedule(1).CurrentValue, 3.0);

    DataGlobals::HourOfDay = 2;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfYear_Schedule = 251;
    UpdateScheduleValues();
    // expect 3.0 for remainder of year regardless of DST
    EXPECT_EQ(Schedule(1).CurrentValue, 3.0);
    DataGlobals::HourOfDay = 24;
    DataEnvironment::DSTIndicator = 1;
    UpdateScheduleValues();
    EXPECT_EQ(Schedule(1).CurrentValue, 3.0);
}

TEST_F(EnergyPlusFixture, ScheduleAnnualFullLoadHours_test)
{
    // J.Glazer - August 2017

    std::string const idf_objects = delimited_string({
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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 15;

    int onSchedIndex = GetScheduleIndex("ONSCHED");
    EXPECT_EQ(8760., ScheduleAnnualFullLoadHours(onSchedIndex, 1, false));

    int offSchedIndex = GetScheduleIndex("OFFSCHED");
    EXPECT_EQ(0., ScheduleAnnualFullLoadHours(offSchedIndex, 1, false));

    int janOnSchedIndex = GetScheduleIndex("JANONSCHED");
    EXPECT_EQ(744., ScheduleAnnualFullLoadHours(janOnSchedIndex, 1, false));

    int halfOnSchedIndex = GetScheduleIndex("HALFONSCHED");
    EXPECT_EQ(4380., ScheduleAnnualFullLoadHours(halfOnSchedIndex, 1, false));

    int halfOnSched2Index = GetScheduleIndex("HALFONSCHED2");
    EXPECT_EQ(4380., ScheduleAnnualFullLoadHours(halfOnSched2Index, 1, false));
}

TEST_F(EnergyPlusFixture, ScheduleAverageHoursPerWeek_test)
{
    // J.Glazer - August 2017

    std::string const idf_objects = delimited_string({
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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 15;

    int onSchedIndex = GetScheduleIndex("ONSCHED");
    EXPECT_EQ(168., ScheduleAverageHoursPerWeek(onSchedIndex, 1, false));

    int offSchedIndex = GetScheduleIndex("OFFSCHED");
    EXPECT_EQ(0., ScheduleAverageHoursPerWeek(offSchedIndex, 1, false));

    int janOnSchedIndex = GetScheduleIndex("JANONSCHED");
    EXPECT_NEAR(14.3, ScheduleAverageHoursPerWeek(janOnSchedIndex, 1, false), 0.1);

    int halfOnSchedIndex = GetScheduleIndex("HALFONSCHED");
    EXPECT_EQ(84., ScheduleAverageHoursPerWeek(halfOnSchedIndex, 1, false));

    int halfOnSched2Index = GetScheduleIndex("HALFONSCHED2");
    EXPECT_EQ(84., ScheduleAverageHoursPerWeek(halfOnSched2Index, 1, false));
}

TEST_F(EnergyPlusFixture, ScheduleHoursGT1perc_test)
{
    // J.Glazer - August 2017

    std::string const idf_objects = delimited_string({
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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 15;
    DataGlobals::TimeStepZone = 0.25;

    int onSchedIndex = GetScheduleIndex("ONSCHED");
    EXPECT_EQ(8760., ScheduleHoursGT1perc(onSchedIndex, 1, false));

    int offSchedIndex = GetScheduleIndex("OFFSCHED");
    EXPECT_EQ(0., ScheduleHoursGT1perc(offSchedIndex, 1, false));

    int janOnSchedIndex = GetScheduleIndex("JANONSCHED");
    EXPECT_EQ(744., ScheduleHoursGT1perc(janOnSchedIndex, 1, false));

    int halfOnSchedIndex = GetScheduleIndex("HALFONSCHED");
    EXPECT_EQ(4380., ScheduleHoursGT1perc(halfOnSchedIndex, 1, false));

    int halfOnSched2Index = GetScheduleIndex("HALFONSCHED2");
    EXPECT_EQ(8760., ScheduleHoursGT1perc(halfOnSched2Index, 1, false));

    int halfOnSched3Index = GetScheduleIndex("HALFONSCHED3");
    EXPECT_EQ(4380., ScheduleHoursGT1perc(halfOnSched3Index, 1, false));
}

TEST_F(EnergyPlusFixture, ScheduleDayInterval_SimpLinearInterp)
{
    // J.Glazer - September 2017

    std::string const idf_objects = delimited_string({
        "Schedule:Year,",
        "  SchYr_A,   !- Name",
        "  AnyNumber, !- Schedule Type Limits Name",
        "  SchWk_A1,  !- Schedule:Week Name 1",
        "  1,         !- Start Month 1",
        "  1,         !- Start Day 1",
        "  12,        !- End Month 1",
        "  31;        !- End Day 1",
        "",
        "Schedule:Week:Daily,",
        "  SchWk_A1,  !- Name",
        "  SchDy_A1a,  !- Sunday Schedule:Day Name",
        "  SchDy_A1a,  !- Monday Schedule:Day Name",
        "  SchDy_A1a,  !- Tuesday Schedule:Day Name",
        "  SchDy_A1a,  !- Wednesday Schedule:Day Name",
        "  SchDy_A1a,  !- Thursday Schedule:Day Name",
        "  SchDy_A1a,  !- Friday Schedule:Day Name",
        "  SchDy_A1a,  !- Saturday Schedule:Day Name",
        "  SchDy_A1a,  !- Holiday Schedule:Day Name",
        "  SchDy_A1a,  !- SummerDesignDay Schedule:Day Name",
        "  SchDy_A1a,  !- WinterDesignDay Schedule:Day Name",
        "  SchDy_A1a,  !- CustomDay1 Schedule:Day Name",
        "  SchDy_A1a;  !- CustomDay2 Schedule:Day Name",
        "  ",
        "Schedule:Day:Interval,",
        "  SchDy_A1a,  !- Name",
        "  AnyNumber,  !- Schedule Type Limits Name",
        "  Linear,     !- Interpolate to Timestep",
        "  07:00,      !- Time 1",
        "  0.001,      !- Value Until Time 1",
        "  08:00,      !- Time 2",
        "  100.001,    !- Value Until Time 2",
        "  10:00,      !- Time 4",
        "  300.001,    !- Value Until Time 4",
        "  14:00,      !- Time 8",
        "  700.001,    !- Value Until Time 8",
        "  15:00,      !- Time 9",
        "  600.001,    !- Value Until Time 9",
        "  19:00,      !- Time 13",
        "  200.001,    !- Value Until Time 13",
        "  24:00,      !- Time 14",
        "  0.001;      !- Value Until Time 14",
        "",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 15;
    DataGlobals::TimeStepZone = 0.25;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 1;
    DataGlobals::HourOfDay = 1;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);

    int ASchedIndex = GetScheduleIndex("SCHYR_A"); // interpolate Linear
    EXPECT_NEAR(0.001, LookUpScheduleValue(ASchedIndex, 7, 4), 0.000001);

    // interpolate over one hour

    EXPECT_NEAR(25.001, LookUpScheduleValue(ASchedIndex, 8, 1), 0.000001);
    EXPECT_NEAR(50.001, LookUpScheduleValue(ASchedIndex, 8, 2), 0.000001);
    EXPECT_NEAR(75.001, LookUpScheduleValue(ASchedIndex, 8, 3), 0.000001);
    EXPECT_NEAR(100.001, LookUpScheduleValue(ASchedIndex, 8, 4), 0.000001);

    // interpolate over two hours

    EXPECT_NEAR(125.001, LookUpScheduleValue(ASchedIndex, 9, 1), 0.000001);
    EXPECT_NEAR(150.001, LookUpScheduleValue(ASchedIndex, 9, 2), 0.000001);
    EXPECT_NEAR(175.001, LookUpScheduleValue(ASchedIndex, 9, 3), 0.000001);
    EXPECT_NEAR(200.001, LookUpScheduleValue(ASchedIndex, 9, 4), 0.000001);

    EXPECT_NEAR(225.001, LookUpScheduleValue(ASchedIndex, 10, 1), 0.000001);
    EXPECT_NEAR(250.001, LookUpScheduleValue(ASchedIndex, 10, 2), 0.000001);
    EXPECT_NEAR(275.001, LookUpScheduleValue(ASchedIndex, 10, 3), 0.000001);
    EXPECT_NEAR(300.001, LookUpScheduleValue(ASchedIndex, 10, 4), 0.000001);

    // interpolate over four hours

    EXPECT_NEAR(325.001, LookUpScheduleValue(ASchedIndex, 11, 1), 0.000001);
    EXPECT_NEAR(350.001, LookUpScheduleValue(ASchedIndex, 11, 2), 0.000001);
    EXPECT_NEAR(375.001, LookUpScheduleValue(ASchedIndex, 11, 3), 0.000001);
    EXPECT_NEAR(400.001, LookUpScheduleValue(ASchedIndex, 11, 4), 0.000001);

    EXPECT_NEAR(525.001, LookUpScheduleValue(ASchedIndex, 13, 1), 0.000001);
    EXPECT_NEAR(550.001, LookUpScheduleValue(ASchedIndex, 13, 2), 0.000001);
    EXPECT_NEAR(575.001, LookUpScheduleValue(ASchedIndex, 13, 3), 0.000001);
    EXPECT_NEAR(600.001, LookUpScheduleValue(ASchedIndex, 13, 4), 0.000001);

    // interpolate over one hour - decreasing

    EXPECT_NEAR(675.001, LookUpScheduleValue(ASchedIndex, 15, 1), 0.000001);
    EXPECT_NEAR(650.001, LookUpScheduleValue(ASchedIndex, 15, 2), 0.000001);
    EXPECT_NEAR(625.001, LookUpScheduleValue(ASchedIndex, 15, 3), 0.000001);
    EXPECT_NEAR(600.001, LookUpScheduleValue(ASchedIndex, 15, 4), 0.000001);

    // interpolate over four hours - decreasing

    EXPECT_NEAR(375.001, LookUpScheduleValue(ASchedIndex, 18, 1), 0.000001);
    EXPECT_NEAR(350.001, LookUpScheduleValue(ASchedIndex, 18, 2), 0.000001);
    EXPECT_NEAR(325.001, LookUpScheduleValue(ASchedIndex, 18, 3), 0.000001);
    EXPECT_NEAR(300.001, LookUpScheduleValue(ASchedIndex, 18, 4), 0.000001);

    EXPECT_NEAR(275.001, LookUpScheduleValue(ASchedIndex, 19, 1), 0.000001);
    EXPECT_NEAR(250.001, LookUpScheduleValue(ASchedIndex, 19, 2), 0.000001);
    EXPECT_NEAR(225.001, LookUpScheduleValue(ASchedIndex, 19, 3), 0.000001);
    EXPECT_NEAR(200.001, LookUpScheduleValue(ASchedIndex, 19, 4), 0.000001);
}

TEST_F(EnergyPlusFixture, ScheduleDayInterval_PartialHourLinearInterp)
{
    // J.Glazer - September 2017

    std::string const idf_objects = delimited_string({
        "Schedule:Year,",
        "  SchYr_A,   !- Name",
        "  AnyNumber, !- Schedule Type Limits Name",
        "  SchWk_A1,  !- Schedule:Week Name 1",
        "  1,         !- Start Month 1",
        "  1,         !- Start Day 1",
        "  12,        !- End Month 1",
        "  31;        !- End Day 1",
        "",
        "Schedule:Week:Daily,",
        "  SchWk_A1,  !- Name",
        "  SchDy_A1a,  !- Sunday Schedule:Day Name",
        "  SchDy_A1a,  !- Monday Schedule:Day Name",
        "  SchDy_A1a,  !- Tuesday Schedule:Day Name",
        "  SchDy_A1a,  !- Wednesday Schedule:Day Name",
        "  SchDy_A1a,  !- Thursday Schedule:Day Name",
        "  SchDy_A1a,  !- Friday Schedule:Day Name",
        "  SchDy_A1a,  !- Saturday Schedule:Day Name",
        "  SchDy_A1a,  !- Holiday Schedule:Day Name",
        "  SchDy_A1a,  !- SummerDesignDay Schedule:Day Name",
        "  SchDy_A1a,  !- WinterDesignDay Schedule:Day Name",
        "  SchDy_A1a,  !- CustomDay1 Schedule:Day Name",
        "  SchDy_A1a;  !- CustomDay2 Schedule:Day Name",
        "  ",
        "Schedule:Day:Interval,",
        "  SchDy_A1a,  !- Name",
        "  AnyNumber,  !- Schedule Type Limits Name",
        "  Linear,     !- Interpolate to Timestep",
        "  07:00,      !- Time 1",
        "  0.001,      !- Value Until Time 1",
        "  07:30,      !- Time 2",
        "  50.001,    !- Value Until Time 2",
        "  08:00,      !- Time 4",
        "  100.001,    !- Value Until Time 2",
        "  24:00,      !- Time 14",
        "  0.001;      !- Value Until Time 14",
        "",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 15;
    DataGlobals::TimeStepZone = 0.25;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 1;
    DataGlobals::HourOfDay = 1;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);

    int ASchedIndex = GetScheduleIndex("SCHYR_A"); // interpolate Linear
    EXPECT_NEAR(0.001, LookUpScheduleValue(ASchedIndex, 7, 4), 0.000001);

    // interpolate over first half hour

    EXPECT_NEAR(25.001, LookUpScheduleValue(ASchedIndex, 8, 1), 0.000001);
    EXPECT_NEAR(50.001, LookUpScheduleValue(ASchedIndex, 8, 2), 0.000001);

    // interpolate over second half hour

    EXPECT_NEAR(75.001, LookUpScheduleValue(ASchedIndex, 8, 3), 0.000001);
    EXPECT_NEAR(100.001, LookUpScheduleValue(ASchedIndex, 8, 4), 0.000001);
}

TEST_F(EnergyPlusFixture, ScheduleDayInterval_LinearInterpIntervalNotTimestep)
{
    // J.Glazer - September 2017

    std::string const idf_objects = delimited_string({
        "Schedule:Year,",
        "  SchYr_A,   !- Name",
        "  AnyNumber, !- Schedule Type Limits Name",
        "  SchWk_A1,  !- Schedule:Week Name 1",
        "  1,         !- Start Month 1",
        "  1,         !- Start Day 1",
        "  12,        !- End Month 1",
        "  31;        !- End Day 1",
        "",
        "Schedule:Week:Daily,",
        "  SchWk_A1,  !- Name",
        "  SchDy_A1a,  !- Sunday Schedule:Day Name",
        "  SchDy_A1a,  !- Monday Schedule:Day Name",
        "  SchDy_A1a,  !- Tuesday Schedule:Day Name",
        "  SchDy_A1a,  !- Wednesday Schedule:Day Name",
        "  SchDy_A1a,  !- Thursday Schedule:Day Name",
        "  SchDy_A1a,  !- Friday Schedule:Day Name",
        "  SchDy_A1a,  !- Saturday Schedule:Day Name",
        "  SchDy_A1a,  !- Holiday Schedule:Day Name",
        "  SchDy_A1a,  !- SummerDesignDay Schedule:Day Name",
        "  SchDy_A1a,  !- WinterDesignDay Schedule:Day Name",
        "  SchDy_A1a,  !- CustomDay1 Schedule:Day Name",
        "  SchDy_A1a;  !- CustomDay2 Schedule:Day Name",
        "  ",
        "Schedule:Day:Interval,",
        "  SchDy_A1a,  !- Name",
        "  AnyNumber,  !- Schedule Type Limits Name",
        "  Linear,     !- Interpolate to Timestep",
        "  07:00,      !- Time 1",
        "  0.0,        !- Value Until Time 1",
        "  07:20,      !- Time 2",
        "  33.33333333,!- Value Until Time 2",
        "  08:00,      !- Time 4",
        "  100.0,      !- Value Until Time 2",
        "  24:00,      !- Time 14",
        "  0.0;        !- Value Until Time 14",
        "",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 15;
    DataGlobals::TimeStepZone = 0.25;

    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 1;
    DataGlobals::HourOfDay = 1;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);

    int ASchedIndex = GetScheduleIndex("SCHYR_A"); // interpolate Linear
    EXPECT_NEAR(0.0, LookUpScheduleValue(ASchedIndex, 7, 4), 0.000001);

    // interpolate over first half hour

    EXPECT_NEAR(25.0, LookUpScheduleValue(ASchedIndex, 8, 1), 0.000001);
    EXPECT_NEAR(50.0, LookUpScheduleValue(ASchedIndex, 8, 2), 0.000001);

    // interpolate over second half hour

    EXPECT_NEAR(75.0, LookUpScheduleValue(ASchedIndex, 8, 3), 0.000001);
    EXPECT_NEAR(100.0, LookUpScheduleValue(ASchedIndex, 8, 4), 0.000001);
}

TEST_F(EnergyPlusFixture, ScheduleYearMaxItems)
{
    std::string const idf_objects = delimited_string({
        "Schedule:Year,",
        "  SchYr_A,   !- Name",
        "  AnyNumber, !- Schedule Type Limits Name",
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31,"
        "  SchWk_A1,1,1,12,31;"
        "",
    });

    ASSERT_FALSE(process_idf(idf_objects, false));

    EXPECT_TRUE(compare_err_stream(delimited_string({"   ** Severe  ** <root>[Schedule:Year][SchYr_A][schedule_weeks] - Array should contain no more than 53 elements."})));

}

TEST_F(EnergyPlusFixture, ScheduleFileColumnSeparator)
{
    std::string const idf_objects = delimited_string({
        "Schedule:File,",
        "  Test1,                   !- Name",
        "  ,                        !- Schedule Type Limits Name",
        "  nofile.txt,              !- File Name",
        "  1,                       !- Column Number",
        "  0,                       !- Rows to Skip at Top",
        "  8760,                    !- Number of Hours of Data",
        "  Space,                   !- Column Separator",
        "  No;                      !- Interpolate to Timestep"
    });

    ASSERT_TRUE(process_idf(idf_objects));

}

TEST_F(EnergyPlusFixture, Schedule_GetCurrentScheduleValue_DST)
{
    std::string const idf_objects = delimited_string({
        "Schedule:Compact,",
        "  Electricity Season Schedule,  !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 5/31,           !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,            !- Field 3",
        "  1,                       !- Field 4",
        "  Through: 9/30,           !- Field 5",
        "  For: AllDays,            !- Field 6",
        "  Until: 24:00,            !- Field 7",
        "  3,                       !- Field 8",
        "  Through: 12/31,          !- Field 9",
        "  For: AllDays,            !- Field 10",
        "  Until: 24:00,            !- Field 11",
        "  1;                       !- Field 12",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 4;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 15;    // must initialize this to get schedules initialized
    DataGlobals::TimeStepZone = 0.25;
    DataGlobals::TimeStepZoneSec = DataGlobals::TimeStepZone * DataGlobals::SecInHour;

    ScheduleManager::ProcessScheduleInput(state.outputFiles); // read schedules

    DataEnvironment::Month = 5;
    DataEnvironment::DayOfMonth = 31;
    DataGlobals::HourOfDay = 24;
    DataEnvironment::DayOfWeek = 4;
    DataEnvironment::DayOfWeekTomorrow = 5;
    DataEnvironment::HolidayIndex = 0;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);

    DataEnvironment::DSTIndicator = 0; // DST IS OFF
    ScheduleManager::UpdateScheduleValues();
    EXPECT_EQ(1.0, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
    EXPECT_EQ(1.0, ScheduleManager::Schedule(1).CurrentValue);
    EXPECT_EQ(1.0, ScheduleManager::GetCurrentScheduleValue(1));

    DataEnvironment::DSTIndicator = 1; // DST IS ON
    ScheduleManager::UpdateScheduleValues();
    // Since DST is on, you're actually on the next day, on 6/1 at 1:00
    // so it **should** return 3.0
    EXPECT_EQ(3.0, ScheduleManager::Schedule(1).CurrentValue);
    EXPECT_EQ(3.0, ScheduleManager::GetCurrentScheduleValue(1));
    EXPECT_EQ(3.0, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
}

TEST_F(EnergyPlusFixture, Schedule_GetCurrentScheduleValue_DST_SouthernHemisphere)
{
    std::string const idf_objects = delimited_string({
        "Schedule:Compact,",
        "  Electricity Season Schedule,  !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 5/31,           !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,            !- Field 3",
        "  1,                       !- Field 4",
        "  Through: 12/31,          !- Field 5",
        "  For: AllDays,            !- Field 6",
        "  Until: 24:00,            !- Field 7",
        "  2;                       !- Field 8",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 4;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 15;    // must initialize this to get schedules initialized
    DataGlobals::TimeStepZone = 0.25;
    DataGlobals::TimeStepZoneSec = DataGlobals::TimeStepZone * DataGlobals::SecInHour;

    ScheduleManager::ProcessScheduleInput(state.outputFiles); // read schedules

    DataEnvironment::Month = 12;
    DataEnvironment::DayOfMonth = 31;
    DataGlobals::HourOfDay = 24;
    DataEnvironment::DayOfWeek = 4;
    DataEnvironment::DayOfWeekTomorrow = 5;
    DataEnvironment::HolidayIndex = 0;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);

    DataEnvironment::DSTIndicator = 0; // DST IS OFF
    ScheduleManager::UpdateScheduleValues();
    EXPECT_EQ(2.0, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
    EXPECT_EQ(2.0, ScheduleManager::Schedule(1).CurrentValue);
    EXPECT_EQ(2.0, ScheduleManager::GetCurrentScheduleValue(1));

    DataEnvironment::DSTIndicator = 1; // DST IS ON
    ScheduleManager::UpdateScheduleValues();
    // Since DST is on, you're actually on the next day, which in this specific case should be 1/1 at 0:15
    // so it **should** return 1.0
    EXPECT_EQ(1.0, ScheduleManager::Schedule(1).CurrentValue);
    EXPECT_EQ(1.0, ScheduleManager::GetCurrentScheduleValue(1));
    EXPECT_EQ(1.0, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
}


TEST_F(EnergyPlusFixture, Schedule_GetCurrentScheduleValue_DST_RampUp_Leap) {

    // So here we'll mimic using a Schedule:Compact that ramps up constantly

    // Schedule:Compact,
    //   RampingUp,                 !- Name
    //   Any Number,                !- Schedule Type Limits Name
    //   Through: 1/1,
    //   For: AllDays,
    //    Until: 1:00, 1,
    //    Until: 2:00, 2,
    //    [...]
    //    Until: 24:00, 24,
    //   Through: 1/2,
    //   For: AllDays,
    //    Until: 1:00, 25,
    //    Until: 2:00, 26,
    //    Until: 3:00, 27,
    //    [...]
    //   Through: 12/31,
    //   For: AllDays,
    //    Until: 1:00, 8761,
    //    [...]
    //    Until: 24:00, 8784;

    // # 'THROUGH" => Number of additional week schedules
    // # 'FOR' => Number of additional day schedules
    // So we use 366 Week Schedules, all with one day (LeapYear)
    DataEnvironment::CurrentYearIsLeapYear = true;
    WeatherManager::WFAllowsLeapYears = true;
    WeatherManager::LeapYearAdd = 1;

    int nDays = 366;
    int NumOfTimeStepInHour = 4;

    ScheduleManager::ScheduleInputProcessed = true;
    EXPECT_TRUE(ScheduleManager::ScheduleInputProcessed);
    ScheduleManager::NumSchedules = 1;
    ScheduleManager::Schedule.allocate(ScheduleManager::NumSchedules);

    ScheduleManager::Schedule(1).WeekSchedulePointer.allocate(nDays);
    ScheduleManager::WeekSchedule.allocate(nDays);
    ScheduleManager::DaySchedule.allocate(nDays);

    for (int ScheduleIndex = 1; ScheduleIndex <= ScheduleManager::NumSchedules; ScheduleIndex++) {
        for (int day = 1; day <= nDays; ++day) {
            // int DayOfWeek = ((day-1) % 7) + 1;
            ScheduleManager::Schedule(ScheduleIndex).WeekSchedulePointer(day) = day;
            ScheduleManager::WeekSchedule(day).DaySchedulePointer.allocate(7);
            for (int d = 1; d <= 7; ++d) {
                ScheduleManager::WeekSchedule(day).DaySchedulePointer(d) = day;
            }
            ScheduleManager::DaySchedule(day).TSValue.allocate(4, 24);
            for (int whichHour = 1; whichHour <= 24; whichHour++) {
                for (int TS = 1; TS <= NumOfTimeStepInHour; ++TS) {
                    ScheduleManager::DaySchedule(day).TSValue(TS, whichHour) = whichHour + (day-1) * 24;
                }
            }
        }
    }

    EXPECT_EQ(366, ScheduleManager::Schedule(1).WeekSchedulePointer(366));
    EXPECT_EQ(366, ScheduleManager::WeekSchedule(366).DaySchedulePointer(2));
    EXPECT_EQ(8784.0, ScheduleManager::DaySchedule(366).TSValue(4, 24));

    DataGlobals::NumOfTimeStepInHour = NumOfTimeStepInHour;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 15;    // must initialize this to get schedules initialized
    DataGlobals::TimeStepZone = 0.25;
    DataGlobals::TimeStepZoneSec = DataGlobals::TimeStepZone * DataGlobals::SecInHour;

    DataEnvironment::Month = 12;
    DataEnvironment::DayOfMonth = 31;
    DataGlobals::HourOfDay = 24;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::DayOfWeekTomorrow = 3;
    DataEnvironment::HolidayIndex = 0;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    EXPECT_EQ(366,  DataEnvironment::DayOfYear_Schedule);

    DataEnvironment::DSTIndicator = 0; // DST IS OFF
    ScheduleManager::UpdateScheduleValues();
    EXPECT_EQ(8784.0, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
    EXPECT_EQ(8784.0, ScheduleManager::Schedule(1).CurrentValue);
    EXPECT_EQ(8784.0, ScheduleManager::GetCurrentScheduleValue(1));

    DataEnvironment::DSTIndicator = 1; // DST IS ON
    ScheduleManager::UpdateScheduleValues();
    // Since DST is on, you're actually on the next day, which in this specific case should be 1/1 at 0:15
    // so it **should** return 1.0
    EXPECT_EQ(1.0, ScheduleManager::Schedule(1).CurrentValue);
    EXPECT_EQ(1.0, ScheduleManager::GetCurrentScheduleValue(1));
    EXPECT_EQ(1.0, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));

    Array1D_int EndDayOfMonth(12, {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31});


    {
        DataEnvironment::DSTIndicator = 0; // DST IS OFF
        DataEnvironment::DayOfWeek = 0;
        DataEnvironment::DayOfWeekTomorrow = 1;

        Real64 HourOfYear = 0.0;

        for (int month=1; month <= 12; ++month) {
            DataEnvironment::Month = month;
            for (int day = 1; day <= EndDayOfMonth(month); ++day) {
                DataEnvironment::DayOfMonth = day;
                ++DataEnvironment::DayOfWeek;
                if (DataEnvironment::DayOfWeek > 7) {
                    DataEnvironment::DayOfWeek = 1;
                }
                ++DataEnvironment::DayOfWeekTomorrow;
                if (DataEnvironment::DayOfWeekTomorrow > 7) {
                    DataEnvironment::DayOfWeekTomorrow = 1;
                }

                DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);

                for (int hr = 1; hr <= 24; ++hr) {
                    ++HourOfYear;
                    DataGlobals::HourOfDay = hr;
                    for (int ts = 1; ts <= 4; ++ts) {
                        DataGlobals::TimeStep = ts;

                        ScheduleManager::UpdateScheduleValues();
                        EXPECT_EQ(HourOfYear, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
                        EXPECT_EQ(HourOfYear, ScheduleManager::Schedule(1).CurrentValue);
                        EXPECT_EQ(HourOfYear, ScheduleManager::GetCurrentScheduleValue(1));
                    }
                }
            }
        }

        EXPECT_EQ(8784.0, HourOfYear);
    }

    {
        DataEnvironment::DSTIndicator = 1; // DST IS ON
        DataEnvironment::DayOfWeek = 0;
        DataEnvironment::DayOfWeekTomorrow = 1;

        Real64 HourOfYear = 0.0;
        for (int month=1; month <= 12; ++month) {
            DataEnvironment::Month = month;
            for (int day = 1; day <= EndDayOfMonth(month); ++day) {
                DataEnvironment::DayOfMonth = day;
                ++DataEnvironment::DayOfWeek;
                if (DataEnvironment::DayOfWeek > 7) {
                    DataEnvironment::DayOfWeek = 1;
                }
                ++DataEnvironment::DayOfWeekTomorrow;
                if (DataEnvironment::DayOfWeekTomorrow > 7) {
                    DataEnvironment::DayOfWeekTomorrow = 1;
                }

                DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);

                for (int hr = 1; hr <= 24; ++hr) {
                    ++HourOfYear;
                    DataGlobals::HourOfDay = hr;
                    for (int ts = 1; ts <= 4; ++ts) {
                        DataGlobals::TimeStep = ts;

                        ScheduleManager::UpdateScheduleValues();
                        int thisHourOfYear = HourOfYear + 1;
                        if (thisHourOfYear > 8784.0) {
                            thisHourOfYear = 1;
                        }
                        EXPECT_EQ(thisHourOfYear, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
                        EXPECT_EQ(thisHourOfYear, ScheduleManager::Schedule(1).CurrentValue);
                        EXPECT_EQ(thisHourOfYear, ScheduleManager::GetCurrentScheduleValue(1));
                    }
                }
            }
        }

        EXPECT_EQ(8784.0, HourOfYear);
    }
}

TEST_F(EnergyPlusFixture, Schedule_GetCurrentScheduleValue_DST_RampUp_NoLeap) {

    // So here we'll mimic using a Schedule:Compact that ramps up constantly

    // Schedule:Compact,
    //   RampingUp,                 !- Name
    //   Any Number,                !- Schedule Type Limits Name
    //   Through: 1/1,
    //   For: AllDays,
    //    Until: 1:00, 1,
    //    Until: 2:00, 2,
    //    [...]
    //    Until: 24:00, 24,
    //   Through: 1/2,
    //   For: AllDays,
    //    Until: 1:00, 25,
    //    Until: 2:00, 26,
    //    Until: 3:00, 27,
    //    [...]
    //   Through: 12/31,
    //   For: AllDays,
    //    Until: 1:00, 8737,
    //    [...]
    //    Until: 24:00, 8760.0;

    // # 'THROUGH" => Number of additional week schedules
    // # 'FOR' => Number of additional day schedules
    // So we use 366 Week Schedules, all with one day (LeapYear)
    DataEnvironment::CurrentYearIsLeapYear = false;
    WeatherManager::WFAllowsLeapYears = false;
    WeatherManager::LeapYearAdd = 0;

    // ScheduleManager always assume LeapYear really.
    int nDays = 365;
    int NumOfTimeStepInHour = 4;

    ScheduleManager::ScheduleInputProcessed = true;
    EXPECT_TRUE(ScheduleManager::ScheduleInputProcessed);
    ScheduleManager::NumSchedules = 1;
    ScheduleManager::Schedule.allocate(ScheduleManager::NumSchedules);

    ScheduleManager::Schedule(1).WeekSchedulePointer.allocate(366);
    ScheduleManager::Schedule(1).WeekSchedulePointer = -1;
    ScheduleManager::WeekSchedule.allocate(366);
    ScheduleManager::DaySchedule.allocate(nDays); // Here only creating 365 ScheduleDays

    Array1D_int EndDayOfMonth(12, {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31});

    int dayOfYear = 0;
    for (int month=1; month <= 12; ++month) {
        for (int day = 1; day <= EndDayOfMonth(month); ++day) {
            ++dayOfYear;
            int DayOfYear_Schedule = General::OrdinalDay(month, day, 1);
            if (month <= 2) {
                EXPECT_EQ(dayOfYear, DayOfYear_Schedule);
            } else {
                EXPECT_EQ(dayOfYear+1, DayOfYear_Schedule);
            }

            ScheduleManager::Schedule(1).WeekSchedulePointer(DayOfYear_Schedule) = DayOfYear_Schedule;
            ScheduleManager::WeekSchedule(DayOfYear_Schedule).DaySchedulePointer.allocate(7);
            for (int d = 1; d <= 7; ++d) {
                ScheduleManager::WeekSchedule(DayOfYear_Schedule).DaySchedulePointer(d) = dayOfYear;
            }
            ScheduleManager::DaySchedule(dayOfYear).TSValue.allocate(4, 24);
            for (int whichHour = 1; whichHour <= 24; whichHour++) {
                for (int TS = 1; TS <= NumOfTimeStepInHour; ++TS) {
                    ScheduleManager::DaySchedule(dayOfYear).TSValue(TS, whichHour) = whichHour + (dayOfYear-1) * 24;
                }
            }
        }
    }

    // Feb 28
    EXPECT_EQ(59, ScheduleManager::Schedule(1).WeekSchedulePointer(59));
    EXPECT_EQ(59, ScheduleManager::WeekSchedule(59).DaySchedulePointer(1));
    EXPECT_EQ(59*24.0, ScheduleManager::DaySchedule(59).TSValue(4, 24));

    // Feb 29: doesn't exist, and I default initialized everything above to -1
    EXPECT_EQ(-1, ScheduleManager::Schedule(1).WeekSchedulePointer(60));
    // ProcessSchedule would have treated the "Until: 3/1" to include the 2/29, so do that too.
    ScheduleManager::Schedule(1).WeekSchedulePointer(60) = 61;

    // March 1
    EXPECT_EQ(61, ScheduleManager::Schedule(1).WeekSchedulePointer(61));
    EXPECT_EQ(60, ScheduleManager::WeekSchedule(61).DaySchedulePointer(1));
    EXPECT_EQ(60*24.0, ScheduleManager::DaySchedule(60).TSValue(4, 24));


    EXPECT_EQ(366, ScheduleManager::Schedule(1).WeekSchedulePointer(366));
    EXPECT_EQ(365, ScheduleManager::WeekSchedule(366).DaySchedulePointer(1));
    EXPECT_EQ(8760.0, ScheduleManager::DaySchedule(365).TSValue(4, 24));

    DataGlobals::NumOfTimeStepInHour = NumOfTimeStepInHour;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 15;    // must initialize this to get schedules initialized
    DataGlobals::TimeStepZone = 0.25;
    DataGlobals::TimeStepZoneSec = DataGlobals::TimeStepZone * DataGlobals::SecInHour;

    DataEnvironment::Month = 12;
    DataEnvironment::DayOfMonth = 31;
    DataGlobals::HourOfDay = 24;
    DataEnvironment::DayOfWeek = 1;
    DataEnvironment::DayOfWeekTomorrow = 2;
    DataEnvironment::HolidayIndex = 0;
    DataGlobals::TimeStep = 1;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);
    EXPECT_EQ(366,  DataEnvironment::DayOfYear_Schedule);

    DataEnvironment::DSTIndicator = 0; // DST IS OFF
    ScheduleManager::UpdateScheduleValues();
    EXPECT_EQ(8760.0, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
    EXPECT_EQ(8760.0, ScheduleManager::Schedule(1).CurrentValue);
    EXPECT_EQ(8760.0, ScheduleManager::GetCurrentScheduleValue(1));

    DataEnvironment::DSTIndicator = 1; // DST IS ON
    ScheduleManager::UpdateScheduleValues();
    // Since DST is on, you're actually on the next day, which in this specific case should be 1/1 at 0:15
    // so it **should** return 1.0
    EXPECT_EQ(1.0, ScheduleManager::Schedule(1).CurrentValue);
    EXPECT_EQ(1.0, ScheduleManager::GetCurrentScheduleValue(1));
    EXPECT_EQ(1.0, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));

    {
        DataEnvironment::DSTIndicator = 0; // DST IS OFF
        DataEnvironment::DayOfWeek = 0;
        DataEnvironment::DayOfWeekTomorrow = 1;

        Real64 HourOfYear = 0.0;
        for (int month=1; month <= 12; ++month) {
            DataEnvironment::Month = month;
            for (int day = 1; day <= EndDayOfMonth(month); ++day) {
                DataEnvironment::DayOfMonth = day;
                ++DataEnvironment::DayOfWeek;
                if (DataEnvironment::DayOfWeek > 7) {
                    DataEnvironment::DayOfWeek = 1;
                }
                ++DataEnvironment::DayOfWeekTomorrow;
                if (DataEnvironment::DayOfWeekTomorrow > 7) {
                    DataEnvironment::DayOfWeekTomorrow = 1;
                }

                DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);

                for (int hr = 1; hr <= 24; ++hr) {
                    ++HourOfYear;
                    DataGlobals::HourOfDay = hr;
                    for (int ts = 1; ts <= 4; ++ts) {
                        DataGlobals::TimeStep = ts;

                        ScheduleManager::UpdateScheduleValues();
                        EXPECT_EQ(HourOfYear, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
                        EXPECT_EQ(HourOfYear, ScheduleManager::Schedule(1).CurrentValue);
                        EXPECT_EQ(HourOfYear, ScheduleManager::GetCurrentScheduleValue(1));
                    }
                }
            }
        }

        EXPECT_EQ(8760.0, HourOfYear);
    }

    {
        DataEnvironment::DSTIndicator = 1; // DST IS ON
        DataEnvironment::DayOfWeek = 0;
        DataEnvironment::DayOfWeekTomorrow = 1;

        Real64 HourOfYear = 0.0;
        for (int month=1; month <= 12; ++month) {
            DataEnvironment::Month = month;
            for (int day = 1; day <= EndDayOfMonth(month); ++day) {
                DataEnvironment::DayOfMonth = day;
                ++DataEnvironment::DayOfWeek;
                if (DataEnvironment::DayOfWeek > 7) {
                    DataEnvironment::DayOfWeek = 1;
                }
                ++DataEnvironment::DayOfWeekTomorrow;
                if (DataEnvironment::DayOfWeekTomorrow > 7) {
                    DataEnvironment::DayOfWeekTomorrow = 1;
                }

                DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(DataEnvironment::Month, DataEnvironment::DayOfMonth, 1);

                for (int hr = 1; hr <= 24; ++hr) {
                    ++HourOfYear;
                    DataGlobals::HourOfDay = hr;
                    for (int ts = 1; ts <= 4; ++ts) {
                        DataGlobals::TimeStep = ts;

                        ScheduleManager::UpdateScheduleValues();
                        int thisHourOfYear = HourOfYear + 1;
                        if (thisHourOfYear > 8760.0) {
                            thisHourOfYear = 1;
                        }
                        EXPECT_EQ(thisHourOfYear, ScheduleManager::LookUpScheduleValue(1, DataGlobals::HourOfDay, DataGlobals::TimeStep));
                        EXPECT_EQ(thisHourOfYear, ScheduleManager::Schedule(1).CurrentValue);
                        EXPECT_EQ(thisHourOfYear, ScheduleManager::GetCurrentScheduleValue(1));
                    }
                }
            }
        }

        EXPECT_EQ(8760.0, HourOfYear);
    }
}
