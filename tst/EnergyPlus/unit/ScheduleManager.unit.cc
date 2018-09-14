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

// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/ScheduleManager.hh>

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

TEST_F(EnergyPlusFixture, ScheduleAnnualFullLoadHours_test)
{
    // J.Glazer - August 2017

    std::string const idf_objects = delimited_string({
        "Version,9.0;",
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
        "Version,9.0;",
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
        "Version,9.0;",
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
