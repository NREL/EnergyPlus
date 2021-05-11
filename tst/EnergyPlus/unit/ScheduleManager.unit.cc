// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/Data/EnergyPlusData.hh>
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

    state->dataScheduleMgr->ScheduleInputProcessed = true;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataScheduleMgr->NumSchedules = 1;
    state->dataScheduleMgr->Schedule.allocate(1);
    state->dataScheduleMgr->Schedule(1).WeekSchedulePointer.allocate(367);
    state->dataScheduleMgr->WeekSchedule.allocate(3);
    state->dataScheduleMgr->WeekSchedule(1).DaySchedulePointer.allocate(12);
    state->dataScheduleMgr->WeekSchedule(2).DaySchedulePointer.allocate(12);
    state->dataScheduleMgr->WeekSchedule(3).DaySchedulePointer.allocate(12);
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataScheduleMgr->DaySchedule.allocate(3);
    state->dataScheduleMgr->DaySchedule(1).TSValue.allocate(1, 24);
    state->dataScheduleMgr->DaySchedule(2).TSValue.allocate(1, 24);
    state->dataScheduleMgr->DaySchedule(3).TSValue.allocate(1, 24);

    for (int ScheduleIndex = 1; ScheduleIndex <= state->dataScheduleMgr->NumSchedules; ScheduleIndex++) {
        for (int i = 1; i <= 366; i++) {
            int x = 1;
            if (i > 250) {
                x = 3;
            } else if (i > 249) {
                x = 2;
            }
            state->dataScheduleMgr->Schedule(ScheduleIndex).WeekSchedulePointer(i) = x;
        }
    }
    for (int WeekSchedulePointer = 1; WeekSchedulePointer <= 3; WeekSchedulePointer++) {
        for (int dayOfWeek = 1; dayOfWeek <= 12; dayOfWeek++) {
            int y = 1;
            if (WeekSchedulePointer == 2) y = 2;
            if (WeekSchedulePointer == 3) y = 3;
            state->dataScheduleMgr->WeekSchedule(WeekSchedulePointer).DaySchedulePointer(dayOfWeek) = y;
        }
    }
    for (int daySchedulePointer = 1; daySchedulePointer <= 3; daySchedulePointer++) {
        for (int whichHour = 1; whichHour <= 24; whichHour++) {
            Real64 schVal = 1.0;
            if (daySchedulePointer == 2) schVal = 2.0;
            if (daySchedulePointer == 3) schVal = 3.0;
            state->dataScheduleMgr->DaySchedule(daySchedulePointer).TSValue(1, whichHour) = schVal;
        }
    }

    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfWeek = 1;
    state->dataEnvrn->DayOfWeekTomorrow = 2;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->HourOfDay = 1;

    // check day schedules
    EXPECT_EQ(state->dataScheduleMgr->DaySchedule(1).TSValue(1, 1), 1.0); // day < 250 points to this schedule
    EXPECT_EQ(state->dataScheduleMgr->DaySchedule(1).TSValue(1, 24), 1.0);

    EXPECT_EQ(state->dataScheduleMgr->DaySchedule(2).TSValue(1, 1), 2.0); // day = 250 points to this schedule
    EXPECT_EQ(state->dataScheduleMgr->DaySchedule(2).TSValue(1, 24), 2.0);

    EXPECT_EQ(state->dataScheduleMgr->DaySchedule(3).TSValue(1, 1), 3.0); // day > 250 points to this schedule
    EXPECT_EQ(state->dataScheduleMgr->DaySchedule(3).TSValue(1, 24), 3.0);

    // schedule values are 1 through day 249, 2 for day 250, and 3 for remainder of year
    state->dataEnvrn->DayOfYear_Schedule = 1;
    UpdateScheduleValues(*state);
    // expect 1.0 on day 1
    EXPECT_EQ(state->dataScheduleMgr->Schedule(1).CurrentValue, 1.0);

    state->dataEnvrn->DayOfYear_Schedule = 250;
    UpdateScheduleValues(*state);
    // expect 2.0 on day 250
    EXPECT_EQ(state->dataScheduleMgr->Schedule(1).CurrentValue, 2.0);

    // test end of day 250 with daylight savings time active
    state->dataGlobal->HourOfDay = 24;
    state->dataEnvrn->DSTIndicator = 1;
    UpdateScheduleValues(*state);
    // expect a 3 on day 251, which on day 250 at midnight with DST of hour 1 of day 251
    EXPECT_EQ(state->dataScheduleMgr->Schedule(1).CurrentValue, 3.0);

    state->dataGlobal->HourOfDay = 2;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfYear_Schedule = 251;
    UpdateScheduleValues(*state);
    // expect 3.0 for remainder of year regardless of DST
    EXPECT_EQ(state->dataScheduleMgr->Schedule(1).CurrentValue, 3.0);
    state->dataGlobal->HourOfDay = 24;
    state->dataEnvrn->DSTIndicator = 1;
    UpdateScheduleValues(*state);
    EXPECT_EQ(state->dataScheduleMgr->Schedule(1).CurrentValue, 3.0);
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

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;

    int onSchedIndex = GetScheduleIndex(*state, "ONSCHED");
    EXPECT_EQ(8760., ScheduleAnnualFullLoadHours(*state, onSchedIndex, 1, false));

    int offSchedIndex = GetScheduleIndex(*state, "OFFSCHED");
    EXPECT_EQ(0., ScheduleAnnualFullLoadHours(*state, offSchedIndex, 1, false));

    int janOnSchedIndex = GetScheduleIndex(*state, "JANONSCHED");
    EXPECT_EQ(744., ScheduleAnnualFullLoadHours(*state, janOnSchedIndex, 1, false));

    int halfOnSchedIndex = GetScheduleIndex(*state, "HALFONSCHED");
    EXPECT_EQ(4380., ScheduleAnnualFullLoadHours(*state, halfOnSchedIndex, 1, false));

    int halfOnSched2Index = GetScheduleIndex(*state, "HALFONSCHED2");
    EXPECT_EQ(4380., ScheduleAnnualFullLoadHours(*state, halfOnSched2Index, 1, false));
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

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;

    int onSchedIndex = GetScheduleIndex(*state, "ONSCHED");
    EXPECT_EQ(168., ScheduleAverageHoursPerWeek(*state, onSchedIndex, 1, false));

    int offSchedIndex = GetScheduleIndex(*state, "OFFSCHED");
    EXPECT_EQ(0., ScheduleAverageHoursPerWeek(*state, offSchedIndex, 1, false));

    int janOnSchedIndex = GetScheduleIndex(*state, "JANONSCHED");
    EXPECT_NEAR(14.3, ScheduleAverageHoursPerWeek(*state, janOnSchedIndex, 1, false), 0.1);

    int halfOnSchedIndex = GetScheduleIndex(*state, "HALFONSCHED");
    EXPECT_EQ(84., ScheduleAverageHoursPerWeek(*state, halfOnSchedIndex, 1, false));

    int halfOnSched2Index = GetScheduleIndex(*state, "HALFONSCHED2");
    EXPECT_EQ(84., ScheduleAverageHoursPerWeek(*state, halfOnSched2Index, 1, false));
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

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;
    state->dataGlobal->TimeStepZone = 0.25;

    int onSchedIndex = GetScheduleIndex(*state, "ONSCHED");
    EXPECT_EQ(8760., ScheduleHoursGT1perc(*state, onSchedIndex, 1, false));

    int offSchedIndex = GetScheduleIndex(*state, "OFFSCHED");
    EXPECT_EQ(0., ScheduleHoursGT1perc(*state, offSchedIndex, 1, false));

    int janOnSchedIndex = GetScheduleIndex(*state, "JANONSCHED");
    EXPECT_EQ(744., ScheduleHoursGT1perc(*state, janOnSchedIndex, 1, false));

    int halfOnSchedIndex = GetScheduleIndex(*state, "HALFONSCHED");
    EXPECT_EQ(4380., ScheduleHoursGT1perc(*state, halfOnSchedIndex, 1, false));

    int halfOnSched2Index = GetScheduleIndex(*state, "HALFONSCHED2");
    EXPECT_EQ(8760., ScheduleHoursGT1perc(*state, halfOnSched2Index, 1, false));

    int halfOnSched3Index = GetScheduleIndex(*state, "HALFONSCHED3");
    EXPECT_EQ(4380., ScheduleHoursGT1perc(*state, halfOnSched3Index, 1, false));
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

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;
    state->dataGlobal->TimeStepZone = 0.25;

    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

    int ASchedIndex = GetScheduleIndex(*state, "SCHYR_A"); // interpolate Linear
    EXPECT_NEAR(0.001, LookUpScheduleValue(*state, ASchedIndex, 7, 4), 0.000001);

    // interpolate over one hour

    EXPECT_NEAR(25.001, LookUpScheduleValue(*state, ASchedIndex, 8, 1), 0.000001);
    EXPECT_NEAR(50.001, LookUpScheduleValue(*state, ASchedIndex, 8, 2), 0.000001);
    EXPECT_NEAR(75.001, LookUpScheduleValue(*state, ASchedIndex, 8, 3), 0.000001);
    EXPECT_NEAR(100.001, LookUpScheduleValue(*state, ASchedIndex, 8, 4), 0.000001);

    // interpolate over two hours

    EXPECT_NEAR(125.001, LookUpScheduleValue(*state, ASchedIndex, 9, 1), 0.000001);
    EXPECT_NEAR(150.001, LookUpScheduleValue(*state, ASchedIndex, 9, 2), 0.000001);
    EXPECT_NEAR(175.001, LookUpScheduleValue(*state, ASchedIndex, 9, 3), 0.000001);
    EXPECT_NEAR(200.001, LookUpScheduleValue(*state, ASchedIndex, 9, 4), 0.000001);

    EXPECT_NEAR(225.001, LookUpScheduleValue(*state, ASchedIndex, 10, 1), 0.000001);
    EXPECT_NEAR(250.001, LookUpScheduleValue(*state, ASchedIndex, 10, 2), 0.000001);
    EXPECT_NEAR(275.001, LookUpScheduleValue(*state, ASchedIndex, 10, 3), 0.000001);
    EXPECT_NEAR(300.001, LookUpScheduleValue(*state, ASchedIndex, 10, 4), 0.000001);

    // interpolate over four hours

    EXPECT_NEAR(325.001, LookUpScheduleValue(*state, ASchedIndex, 11, 1), 0.000001);
    EXPECT_NEAR(350.001, LookUpScheduleValue(*state, ASchedIndex, 11, 2), 0.000001);
    EXPECT_NEAR(375.001, LookUpScheduleValue(*state, ASchedIndex, 11, 3), 0.000001);
    EXPECT_NEAR(400.001, LookUpScheduleValue(*state, ASchedIndex, 11, 4), 0.000001);

    EXPECT_NEAR(525.001, LookUpScheduleValue(*state, ASchedIndex, 13, 1), 0.000001);
    EXPECT_NEAR(550.001, LookUpScheduleValue(*state, ASchedIndex, 13, 2), 0.000001);
    EXPECT_NEAR(575.001, LookUpScheduleValue(*state, ASchedIndex, 13, 3), 0.000001);
    EXPECT_NEAR(600.001, LookUpScheduleValue(*state, ASchedIndex, 13, 4), 0.000001);

    // interpolate over one hour - decreasing

    EXPECT_NEAR(675.001, LookUpScheduleValue(*state, ASchedIndex, 15, 1), 0.000001);
    EXPECT_NEAR(650.001, LookUpScheduleValue(*state, ASchedIndex, 15, 2), 0.000001);
    EXPECT_NEAR(625.001, LookUpScheduleValue(*state, ASchedIndex, 15, 3), 0.000001);
    EXPECT_NEAR(600.001, LookUpScheduleValue(*state, ASchedIndex, 15, 4), 0.000001);

    // interpolate over four hours - decreasing

    EXPECT_NEAR(375.001, LookUpScheduleValue(*state, ASchedIndex, 18, 1), 0.000001);
    EXPECT_NEAR(350.001, LookUpScheduleValue(*state, ASchedIndex, 18, 2), 0.000001);
    EXPECT_NEAR(325.001, LookUpScheduleValue(*state, ASchedIndex, 18, 3), 0.000001);
    EXPECT_NEAR(300.001, LookUpScheduleValue(*state, ASchedIndex, 18, 4), 0.000001);

    EXPECT_NEAR(275.001, LookUpScheduleValue(*state, ASchedIndex, 19, 1), 0.000001);
    EXPECT_NEAR(250.001, LookUpScheduleValue(*state, ASchedIndex, 19, 2), 0.000001);
    EXPECT_NEAR(225.001, LookUpScheduleValue(*state, ASchedIndex, 19, 3), 0.000001);
    EXPECT_NEAR(200.001, LookUpScheduleValue(*state, ASchedIndex, 19, 4), 0.000001);
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

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;
    state->dataGlobal->TimeStepZone = 0.25;

    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

    int ASchedIndex = GetScheduleIndex(*state, "SCHYR_A"); // interpolate Linear
    EXPECT_NEAR(0.001, LookUpScheduleValue(*state, ASchedIndex, 7, 4), 0.000001);

    // interpolate over first half hour

    EXPECT_NEAR(25.001, LookUpScheduleValue(*state, ASchedIndex, 8, 1), 0.000001);
    EXPECT_NEAR(50.001, LookUpScheduleValue(*state, ASchedIndex, 8, 2), 0.000001);

    // interpolate over second half hour

    EXPECT_NEAR(75.001, LookUpScheduleValue(*state, ASchedIndex, 8, 3), 0.000001);
    EXPECT_NEAR(100.001, LookUpScheduleValue(*state, ASchedIndex, 8, 4), 0.000001);
}

TEST_F(EnergyPlusFixture, ScheduleDayInterval_LinearInterpIntervalHittingIntervals)
{
    // J.Thomas - Feb 2021

    std::string const idf_objects = delimited_string({
        //    This schedule should cause issues if interpolation issue is not fixed. Got the initial schedule from the unmethours issue.
        "ScheduleTypeLimits, ",
        "  Fractional,                     !-Name ",
        "  0.1,                            !-Lower Limit Value",
        "  0.9,                            !-Upper Limit Value",
        "  Continuous;                     !-Numeric Type",

        "Schedule:Day:Interval,",
        "  2LLO Weekday,                   !- Name",
        "  Fractional,                     !- Schedule Type Limits Name",
        "  Linear,                         !- Interpolate to Timestep",
        "  06:00,                          !- Time 1 {hh:mm}",
        "  0.1,                            !- Value Until Time 1",
        "  07:15,                          !- Time 2 {hh:mm}",
        "  0.9,                            !- Value Until Time 2",
        "  16:15,                          !- Time 3 {hh:mm}",
        "  0.1,                            !- Value Until Time 3",
        "  18:15,                          !- Time 4 {hh:mm}",
        "  0.1,                            !- Value Until Time 4",
        "  24:00,                          !- Time 5 {hh:mm}",
        "  0.9;                            !-Value Until Time 5 ",

        "Schedule:Week:Daily,",
        "  Week Rule - Jan1-Dec31,         !- Name",
        "  2LLO Weekday,                   !- Sunday Schedule:Day Name",
        "  2LLO Weekday,                   !- Monday Schedule:Day Name",
        "  2LLO Weekday,                   !- Tuesday Schedule:Day Name",
        "  2LLO Weekday,                   !- Wednesday Schedule:Day Name",
        "  2LLO Weekday,                   !- Thursday Schedule:Day Name",
        "  2LLO Weekday,                   !- Friday Schedule:Day Name",
        "  2LLO Weekday,                   !- Saturday Schedule:Day Name",
        "  2LLO Weekday,                   !- Holiday Schedule:Day Name",
        "  2LLO Weekday,                   !- SummerDesignDay Schedule:Day Name",
        "  2LLO Weekday,                   !- WinterDesignDay Schedule:Day Name",
        "  2LLO Weekday,                   !- CustomDay1 Schedule:Day Name",
        "  2LLO Weekday;                   !- CustomDay2 Schedule:Day Name",

        "Schedule:Year,",
        "  2LLOYEAR,                       !- Name ",
        "  Fractional,                     !-Schedule Type Limits Name",
        "  Week Rule - Jan1-Dec31,         !- Schedule:Week Name 1",
        "  1,                              !- Start Month 1 ",
        "  1,                              !- Start Day 1",
        "  12,                             !- End Month 1",
        "  31;                             !- End Day 1",
        //    End of problem schedule
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;
    state->dataGlobal->TimeStepZone = 0.25;

    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

    int ASchedIndex = GetScheduleIndex(*state, "2LLOYEAR"); // interpolate Linear
    // Timesteps will go 1,2,3,4; Not 0,1,2,3, Hours to go as (actual hour+1) therefore 7:15 is 8,1
    // Check for values specified in schedule (Lower and upper limits)
    EXPECT_NEAR(0.1, LookUpScheduleValue(*state, ASchedIndex, 6, 4), 0.000001);  // at 6:00
    EXPECT_NEAR(0.1, LookUpScheduleValue(*state, ASchedIndex, 17, 1), 0.000001); // at 16:15
    EXPECT_NEAR(0.1, LookUpScheduleValue(*state, ASchedIndex, 19, 1), 0.000001); // at 18:15
    EXPECT_NEAR(0.9, LookUpScheduleValue(*state, ASchedIndex, 24, 4), 0.000001); // at 24:00

    //    Interpolation check
    EXPECT_NEAR(0.4199999, LookUpScheduleValue(*state, ASchedIndex, 7, 2), 0.000001);  // Value at 06:30
    EXPECT_NEAR(0.1000000, LookUpScheduleValue(*state, ASchedIndex, 18, 3), 0.000001); // Value at 06:30
    EXPECT_NEAR(0.8304347, LookUpScheduleValue(*state, ASchedIndex, 24, 2), 0.000001); // Value at 06:30
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

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 15;
    state->dataGlobal->TimeStepZone = 0.25;

    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

    int ASchedIndex = GetScheduleIndex(*state, "SCHYR_A"); // interpolate Linear
    EXPECT_NEAR(0.0, LookUpScheduleValue(*state, ASchedIndex, 7, 4), 0.000001);

    // interpolate over first half hour

    EXPECT_NEAR(25.0, LookUpScheduleValue(*state, ASchedIndex, 8, 1), 0.000001);
    EXPECT_NEAR(50.0, LookUpScheduleValue(*state, ASchedIndex, 8, 2), 0.000001);

    // interpolate over second half hour

    EXPECT_NEAR(75.0, LookUpScheduleValue(*state, ASchedIndex, 8, 3), 0.000001);
    EXPECT_NEAR(100.0, LookUpScheduleValue(*state, ASchedIndex, 8, 4), 0.000001);
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

    EXPECT_TRUE(compare_err_stream(
        delimited_string({"   ** Severe  ** <root>[Schedule:Year][SchYr_A][schedule_weeks] - Array should contain no more than 53 elements."})));
}

TEST_F(EnergyPlusFixture, ScheduleFileColumnSeparator)
{
    std::string const idf_objects = delimited_string({"Schedule:File,",
                                                      "  Test1,                   !- Name",
                                                      "  ,                        !- Schedule Type Limits Name",
                                                      "  nofile.txt,              !- File Name",
                                                      "  1,                       !- Column Number",
                                                      "  0,                       !- Rows to Skip at Top",
                                                      "  8760,                    !- Number of Hours of Data",
                                                      "  Space,                   !- Column Separator",
                                                      "  No;                      !- Interpolate to Timestep"});

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

    state->dataGlobal->NumOfTimeStepInHour = 4; // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 15; // must initialize this to get schedules initialized
    state->dataGlobal->TimeStepZone = 0.25;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour;

    ScheduleManager::ProcessScheduleInput(*state); // read schedules

    state->dataEnvrn->Month = 5;
    state->dataEnvrn->DayOfMonth = 31;
    state->dataGlobal->HourOfDay = 24;
    state->dataEnvrn->DayOfWeek = 4;
    state->dataEnvrn->DayOfWeekTomorrow = 5;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

    state->dataEnvrn->DSTIndicator = 0; // DST IS OFF
    ScheduleManager::UpdateScheduleValues(*state);
    EXPECT_EQ(1.0, ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
    EXPECT_EQ(1.0, state->dataScheduleMgr->Schedule(1).CurrentValue);
    EXPECT_EQ(1.0, ScheduleManager::GetCurrentScheduleValue(*state, 1));

    state->dataEnvrn->DSTIndicator = 1; // DST IS ON
    ScheduleManager::UpdateScheduleValues(*state);
    // Since DST is on, you're actually on the next day, on 6/1 at 1:00
    // so it **should** return 3.0
    EXPECT_EQ(3.0, state->dataScheduleMgr->Schedule(1).CurrentValue);
    EXPECT_EQ(3.0, ScheduleManager::GetCurrentScheduleValue(*state, 1));
    EXPECT_EQ(3.0, ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
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

    state->dataGlobal->NumOfTimeStepInHour = 4; // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 15; // must initialize this to get schedules initialized
    state->dataGlobal->TimeStepZone = 0.25;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour;

    ScheduleManager::ProcessScheduleInput(*state); // read schedules

    state->dataEnvrn->Month = 12;
    state->dataEnvrn->DayOfMonth = 31;
    state->dataGlobal->HourOfDay = 24;
    state->dataEnvrn->DayOfWeek = 4;
    state->dataEnvrn->DayOfWeekTomorrow = 5;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

    state->dataEnvrn->DSTIndicator = 0; // DST IS OFF
    ScheduleManager::UpdateScheduleValues(*state);
    EXPECT_EQ(2.0, ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
    EXPECT_EQ(2.0, state->dataScheduleMgr->Schedule(1).CurrentValue);
    EXPECT_EQ(2.0, ScheduleManager::GetCurrentScheduleValue(*state, 1));

    state->dataEnvrn->DSTIndicator = 1; // DST IS ON
    ScheduleManager::UpdateScheduleValues(*state);
    // Since DST is on, you're actually on the next day, which in this specific case should be 1/1 at 0:15
    // so it **should** return 1.0
    EXPECT_EQ(1.0, state->dataScheduleMgr->Schedule(1).CurrentValue);
    EXPECT_EQ(1.0, ScheduleManager::GetCurrentScheduleValue(*state, 1));
    EXPECT_EQ(1.0, ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
}

TEST_F(EnergyPlusFixture, Schedule_GetCurrentScheduleValue_DST_RampUp_Leap)
{

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
    state->dataEnvrn->CurrentYearIsLeapYear = true;
    state->dataWeatherManager->WFAllowsLeapYears = true;
    state->dataWeatherManager->LeapYearAdd = 1;

    int nDays = 366;
    state->dataGlobal->NumOfTimeStepInHour = 4;

    state->dataScheduleMgr->ScheduleInputProcessed = true;
    EXPECT_TRUE(state->dataScheduleMgr->ScheduleInputProcessed);
    state->dataScheduleMgr->NumSchedules = 1;
    state->dataScheduleMgr->Schedule.allocate(state->dataScheduleMgr->NumSchedules);

    state->dataScheduleMgr->Schedule(1).WeekSchedulePointer.allocate(nDays);
    state->dataScheduleMgr->WeekSchedule.allocate(nDays);
    state->dataScheduleMgr->DaySchedule.allocate(nDays);

    for (int ScheduleIndex = 1; ScheduleIndex <= state->dataScheduleMgr->NumSchedules; ScheduleIndex++) {
        for (int day = 1; day <= nDays; ++day) {
            // int DayOfWeek = ((day-1) % 7) + 1;
            state->dataScheduleMgr->Schedule(ScheduleIndex).WeekSchedulePointer(day) = day;
            state->dataScheduleMgr->WeekSchedule(day).DaySchedulePointer.allocate(7);
            for (int d = 1; d <= 7; ++d) {
                state->dataScheduleMgr->WeekSchedule(day).DaySchedulePointer(d) = day;
            }
            state->dataScheduleMgr->DaySchedule(day).TSValue.allocate(4, 24);
            for (int whichHour = 1; whichHour <= 24; whichHour++) {
                for (int TS = 1; TS <= state->dataGlobal->NumOfTimeStepInHour; ++TS) {
                    state->dataScheduleMgr->DaySchedule(day).TSValue(TS, whichHour) = whichHour + (day - 1) * 24;
                }
            }
        }
    }

    EXPECT_EQ(366, state->dataScheduleMgr->Schedule(1).WeekSchedulePointer(366));
    EXPECT_EQ(366, state->dataScheduleMgr->WeekSchedule(366).DaySchedulePointer(2));
    EXPECT_EQ(8784.0, state->dataScheduleMgr->DaySchedule(366).TSValue(4, 24));

    state->dataGlobal->NumOfTimeStepInHour = state->dataGlobal->NumOfTimeStepInHour; // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 15;                                      // must initialize this to get schedules initialized
    state->dataGlobal->TimeStepZone = 0.25;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour;

    state->dataEnvrn->Month = 12;
    state->dataEnvrn->DayOfMonth = 31;
    state->dataGlobal->HourOfDay = 24;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->DayOfWeekTomorrow = 3;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    EXPECT_EQ(366, state->dataEnvrn->DayOfYear_Schedule);

    state->dataEnvrn->DSTIndicator = 0; // DST IS OFF
    ScheduleManager::UpdateScheduleValues(*state);
    EXPECT_EQ(8784.0, ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
    EXPECT_EQ(8784.0, state->dataScheduleMgr->Schedule(1).CurrentValue);
    EXPECT_EQ(8784.0, ScheduleManager::GetCurrentScheduleValue(*state, 1));

    state->dataEnvrn->DSTIndicator = 1; // DST IS ON
    ScheduleManager::UpdateScheduleValues(*state);
    // Since DST is on, you're actually on the next day, which in this specific case should be 1/1 at 0:15
    // so it **should** return 1.0
    EXPECT_EQ(1.0, state->dataScheduleMgr->Schedule(1).CurrentValue);
    EXPECT_EQ(1.0, ScheduleManager::GetCurrentScheduleValue(*state, 1));
    EXPECT_EQ(1.0, ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));

    Array1D_int EndDayOfMonth(12, {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31});

    {
        state->dataEnvrn->DSTIndicator = 0; // DST IS OFF
        state->dataEnvrn->DayOfWeek = 0;
        state->dataEnvrn->DayOfWeekTomorrow = 1;

        Real64 HourOfYear = 0.0;

        for (int month = 1; month <= 12; ++month) {
            state->dataEnvrn->Month = month;
            for (int day = 1; day <= EndDayOfMonth(month); ++day) {
                state->dataEnvrn->DayOfMonth = day;
                ++state->dataEnvrn->DayOfWeek;
                if (state->dataEnvrn->DayOfWeek > 7) {
                    state->dataEnvrn->DayOfWeek = 1;
                }
                ++state->dataEnvrn->DayOfWeekTomorrow;
                if (state->dataEnvrn->DayOfWeekTomorrow > 7) {
                    state->dataEnvrn->DayOfWeekTomorrow = 1;
                }

                state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

                for (int hr = 1; hr <= 24; ++hr) {
                    ++HourOfYear;
                    state->dataGlobal->HourOfDay = hr;
                    for (int ts = 1; ts <= 4; ++ts) {
                        state->dataGlobal->TimeStep = ts;

                        ScheduleManager::UpdateScheduleValues(*state);
                        EXPECT_EQ(HourOfYear,
                                  ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
                        EXPECT_EQ(HourOfYear, state->dataScheduleMgr->Schedule(1).CurrentValue);
                        EXPECT_EQ(HourOfYear, ScheduleManager::GetCurrentScheduleValue(*state, 1));
                    }
                }
            }
        }

        EXPECT_EQ(8784.0, HourOfYear);
    }

    {
        state->dataEnvrn->DSTIndicator = 1; // DST IS ON
        state->dataEnvrn->DayOfWeek = 0;
        state->dataEnvrn->DayOfWeekTomorrow = 1;

        Real64 HourOfYear = 0.0;
        for (int month = 1; month <= 12; ++month) {
            state->dataEnvrn->Month = month;
            for (int day = 1; day <= EndDayOfMonth(month); ++day) {
                state->dataEnvrn->DayOfMonth = day;
                ++state->dataEnvrn->DayOfWeek;
                if (state->dataEnvrn->DayOfWeek > 7) {
                    state->dataEnvrn->DayOfWeek = 1;
                }
                ++state->dataEnvrn->DayOfWeekTomorrow;
                if (state->dataEnvrn->DayOfWeekTomorrow > 7) {
                    state->dataEnvrn->DayOfWeekTomorrow = 1;
                }

                state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

                for (int hr = 1; hr <= 24; ++hr) {
                    ++HourOfYear;
                    state->dataGlobal->HourOfDay = hr;
                    for (int ts = 1; ts <= 4; ++ts) {
                        state->dataGlobal->TimeStep = ts;

                        ScheduleManager::UpdateScheduleValues(*state);
                        int thisHourOfYear = HourOfYear + 1;
                        if (thisHourOfYear > 8784.0) {
                            thisHourOfYear = 1;
                        }
                        EXPECT_EQ(thisHourOfYear,
                                  ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
                        EXPECT_EQ(thisHourOfYear, state->dataScheduleMgr->Schedule(1).CurrentValue);
                        EXPECT_EQ(thisHourOfYear, ScheduleManager::GetCurrentScheduleValue(*state, 1));
                    }
                }
            }
        }

        EXPECT_EQ(8784.0, HourOfYear);
    }
}

TEST_F(EnergyPlusFixture, Schedule_GetCurrentScheduleValue_DST_RampUp_NoLeap)
{

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
    state->dataEnvrn->CurrentYearIsLeapYear = false;
    state->dataWeatherManager->WFAllowsLeapYears = false;
    state->dataWeatherManager->LeapYearAdd = 0;

    // ScheduleManager always assume LeapYear really.
    int nDays = 365;
    state->dataGlobal->NumOfTimeStepInHour = 4;

    state->dataScheduleMgr->ScheduleInputProcessed = true;
    EXPECT_TRUE(state->dataScheduleMgr->ScheduleInputProcessed);
    state->dataScheduleMgr->NumSchedules = 1;
    state->dataScheduleMgr->Schedule.allocate(state->dataScheduleMgr->NumSchedules);

    state->dataScheduleMgr->Schedule(1).WeekSchedulePointer.allocate(366);
    state->dataScheduleMgr->Schedule(1).WeekSchedulePointer = -1;
    state->dataScheduleMgr->WeekSchedule.allocate(366);
    state->dataScheduleMgr->DaySchedule.allocate(nDays); // Here only creating 365 ScheduleDays

    Array1D_int EndDayOfMonth(12, {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31});

    int dayOfYear = 0;
    for (int month = 1; month <= 12; ++month) {
        for (int day = 1; day <= EndDayOfMonth(month); ++day) {
            ++dayOfYear;
            int DayOfYear_Schedule = General::OrdinalDay(month, day, 1);
            if (month <= 2) {
                EXPECT_EQ(dayOfYear, DayOfYear_Schedule);
            } else {
                EXPECT_EQ(dayOfYear + 1, DayOfYear_Schedule);
            }

            state->dataScheduleMgr->Schedule(1).WeekSchedulePointer(DayOfYear_Schedule) = DayOfYear_Schedule;
            state->dataScheduleMgr->WeekSchedule(DayOfYear_Schedule).DaySchedulePointer.allocate(7);
            for (int d = 1; d <= 7; ++d) {
                state->dataScheduleMgr->WeekSchedule(DayOfYear_Schedule).DaySchedulePointer(d) = dayOfYear;
            }
            state->dataScheduleMgr->DaySchedule(dayOfYear).TSValue.allocate(4, 24);
            for (int whichHour = 1; whichHour <= 24; whichHour++) {
                for (int TS = 1; TS <= state->dataGlobal->NumOfTimeStepInHour; ++TS) {
                    state->dataScheduleMgr->DaySchedule(dayOfYear).TSValue(TS, whichHour) = whichHour + (dayOfYear - 1) * 24;
                }
            }
        }
    }

    // Feb 28
    EXPECT_EQ(59, state->dataScheduleMgr->Schedule(1).WeekSchedulePointer(59));
    EXPECT_EQ(59, state->dataScheduleMgr->WeekSchedule(59).DaySchedulePointer(1));
    EXPECT_EQ(59 * 24.0, state->dataScheduleMgr->DaySchedule(59).TSValue(4, 24));

    // Feb 29: doesn't exist, and I default initialized everything above to -1
    EXPECT_EQ(-1, state->dataScheduleMgr->Schedule(1).WeekSchedulePointer(60));
    // ProcessSchedule would have treated the "Until: 3/1" to include the 2/29, so do that too.
    state->dataScheduleMgr->Schedule(1).WeekSchedulePointer(60) = 61;

    // March 1
    EXPECT_EQ(61, state->dataScheduleMgr->Schedule(1).WeekSchedulePointer(61));
    EXPECT_EQ(60, state->dataScheduleMgr->WeekSchedule(61).DaySchedulePointer(1));
    EXPECT_EQ(60 * 24.0, state->dataScheduleMgr->DaySchedule(60).TSValue(4, 24));

    EXPECT_EQ(366, state->dataScheduleMgr->Schedule(1).WeekSchedulePointer(366));
    EXPECT_EQ(365, state->dataScheduleMgr->WeekSchedule(366).DaySchedulePointer(1));
    EXPECT_EQ(8760.0, state->dataScheduleMgr->DaySchedule(365).TSValue(4, 24));

    state->dataGlobal->NumOfTimeStepInHour = state->dataGlobal->NumOfTimeStepInHour; // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 15;                                      // must initialize this to get schedules initialized
    state->dataGlobal->TimeStepZone = 0.25;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * DataGlobalConstants::SecInHour;

    state->dataEnvrn->Month = 12;
    state->dataEnvrn->DayOfMonth = 31;
    state->dataGlobal->HourOfDay = 24;
    state->dataEnvrn->DayOfWeek = 1;
    state->dataEnvrn->DayOfWeekTomorrow = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataGlobal->TimeStep = 1;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    EXPECT_EQ(366, state->dataEnvrn->DayOfYear_Schedule);

    state->dataEnvrn->DSTIndicator = 0; // DST IS OFF
    ScheduleManager::UpdateScheduleValues(*state);
    EXPECT_EQ(8760.0, ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
    EXPECT_EQ(8760.0, state->dataScheduleMgr->Schedule(1).CurrentValue);
    EXPECT_EQ(8760.0, ScheduleManager::GetCurrentScheduleValue(*state, 1));

    state->dataEnvrn->DSTIndicator = 1; // DST IS ON
    ScheduleManager::UpdateScheduleValues(*state);
    // Since DST is on, you're actually on the next day, which in this specific case should be 1/1 at 0:15
    // so it **should** return 1.0
    EXPECT_EQ(1.0, state->dataScheduleMgr->Schedule(1).CurrentValue);
    EXPECT_EQ(1.0, ScheduleManager::GetCurrentScheduleValue(*state, 1));
    EXPECT_EQ(1.0, ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));

    {
        state->dataEnvrn->DSTIndicator = 0; // DST IS OFF
        state->dataEnvrn->DayOfWeek = 0;
        state->dataEnvrn->DayOfWeekTomorrow = 1;

        Real64 HourOfYear = 0.0;
        for (int month = 1; month <= 12; ++month) {
            state->dataEnvrn->Month = month;
            for (int day = 1; day <= EndDayOfMonth(month); ++day) {
                state->dataEnvrn->DayOfMonth = day;
                ++state->dataEnvrn->DayOfWeek;
                if (state->dataEnvrn->DayOfWeek > 7) {
                    state->dataEnvrn->DayOfWeek = 1;
                }
                ++state->dataEnvrn->DayOfWeekTomorrow;
                if (state->dataEnvrn->DayOfWeekTomorrow > 7) {
                    state->dataEnvrn->DayOfWeekTomorrow = 1;
                }

                state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

                for (int hr = 1; hr <= 24; ++hr) {
                    ++HourOfYear;
                    state->dataGlobal->HourOfDay = hr;
                    for (int ts = 1; ts <= 4; ++ts) {
                        state->dataGlobal->TimeStep = ts;

                        ScheduleManager::UpdateScheduleValues(*state);
                        EXPECT_EQ(HourOfYear,
                                  ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
                        EXPECT_EQ(HourOfYear, state->dataScheduleMgr->Schedule(1).CurrentValue);
                        EXPECT_EQ(HourOfYear, ScheduleManager::GetCurrentScheduleValue(*state, 1));
                    }
                }
            }
        }

        EXPECT_EQ(8760.0, HourOfYear);
    }

    {
        state->dataEnvrn->DSTIndicator = 1; // DST IS ON
        state->dataEnvrn->DayOfWeek = 0;
        state->dataEnvrn->DayOfWeekTomorrow = 1;

        Real64 HourOfYear = 0.0;
        for (int month = 1; month <= 12; ++month) {
            state->dataEnvrn->Month = month;
            for (int day = 1; day <= EndDayOfMonth(month); ++day) {
                state->dataEnvrn->DayOfMonth = day;
                ++state->dataEnvrn->DayOfWeek;
                if (state->dataEnvrn->DayOfWeek > 7) {
                    state->dataEnvrn->DayOfWeek = 1;
                }
                ++state->dataEnvrn->DayOfWeekTomorrow;
                if (state->dataEnvrn->DayOfWeekTomorrow > 7) {
                    state->dataEnvrn->DayOfWeekTomorrow = 1;
                }

                state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);

                for (int hr = 1; hr <= 24; ++hr) {
                    ++HourOfYear;
                    state->dataGlobal->HourOfDay = hr;
                    for (int ts = 1; ts <= 4; ++ts) {
                        state->dataGlobal->TimeStep = ts;

                        ScheduleManager::UpdateScheduleValues(*state);
                        int thisHourOfYear = HourOfYear + 1;
                        if (thisHourOfYear > 8760.0) {
                            thisHourOfYear = 1;
                        }
                        EXPECT_EQ(thisHourOfYear,
                                  ScheduleManager::LookUpScheduleValue(*state, 1, state->dataGlobal->HourOfDay, state->dataGlobal->TimeStep));
                        EXPECT_EQ(thisHourOfYear, state->dataScheduleMgr->Schedule(1).CurrentValue);
                        EXPECT_EQ(thisHourOfYear, ScheduleManager::GetCurrentScheduleValue(*state, 1));
                    }
                }
            }
        }

        EXPECT_EQ(8760.0, HourOfYear);
    }
}
