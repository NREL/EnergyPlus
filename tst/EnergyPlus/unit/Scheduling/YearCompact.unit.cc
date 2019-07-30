// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#include <gtest/gtest.h>

#include <nlohmann/json.hpp>

#include <Scheduling/SchedulingFixture.hh>
#include <Scheduling/YearCompact.hh>

namespace EnergyPlus {

TEST_F(SchedulingTestFixture, TestYearCompactFieldProcessingMinimal)
{
    {
        // Minimal input structure
        nlohmann::json fields =
            nlohmann::json::array({
                {{"field", "Through: 12/31"}},
                {{"field", "For: AllDays"}},
                {{"field", "Until: 24:00"}},
                {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(1u, compact.throughs.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.front().untils.size());
        EXPECT_EQ(24, compact.throughs.front().fors.front().untils.front().value);
        EXPECT_EQ(Scheduling::Interpolation::NONE, compact.throughs.front().fors.front().interpolate); // default is to not interpolate
    }
    {
        // Minimal input structure - no colons
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through 12/31"}},
                                      {{"field", "For AllDays"}},
                                      {{"field", "Until 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(1u, compact.throughs.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.front().untils.size());
        EXPECT_EQ(24, compact.throughs.front().fors.front().untils.front().value);
        EXPECT_EQ(Scheduling::Interpolation::NONE, compact.throughs.front().fors.front().interpolate); // default is to not interpolate
    }
}

TEST_F(SchedulingTestFixture, TestYearCompactFieldProcessingInterpolationVariations)
{
    {
        // With average interpolation entry
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Interpolate: Average"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(1u, compact.throughs.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.front().untils.size());
        EXPECT_EQ(24, compact.throughs.front().fors.front().untils.front().value);
        EXPECT_EQ(Scheduling::Interpolation::AVERAGE, compact.throughs.front().fors.front().interpolate);
    }
    {
        // With linear interpolation entry
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Interpolate: LINEar"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(1u, compact.throughs.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.front().untils.size());
        EXPECT_EQ(24, compact.throughs.front().fors.front().untils.front().value);
        EXPECT_EQ(Scheduling::Interpolation::LINEAR, compact.throughs.front().fors.front().interpolate);
    }
    {
        // With intentional NO interpolation entry
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Interpolate: nO"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(1u, compact.throughs.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.front().untils.size());
        EXPECT_EQ(24, compact.throughs.front().fors.front().untils.front().value);
        EXPECT_EQ(Scheduling::Interpolation::NONE, compact.throughs.front().fors.front().interpolate);
    }
}

TEST_F(SchedulingTestFixture, TestYearCompactFieldProcessingMultipleGroups)
{
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 16:00"}},
                                      {{"field", 17}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(1u, compact.throughs.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.size());
        EXPECT_EQ(2u, compact.throughs.front().fors.front().untils.size());
        EXPECT_EQ(24, compact.throughs.front().fors.front().untils.back().value);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: Sundays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 14}},
                                      {{"field", "For: AllOtherDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(1u, compact.throughs.size());
        EXPECT_EQ(2u, compact.throughs.front().fors.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.back().untils.size());
        EXPECT_EQ(14, compact.throughs.front().fors.front().untils.front().value);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/30"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 23}},
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(2u, compact.throughs.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.size());
        EXPECT_EQ(1u, compact.throughs.front().fors.front().untils.size());
        EXPECT_EQ(23, compact.throughs.front().fors.front().untils.front().value);
    }
}

TEST_F(SchedulingTestFixture, TestYearCompactFieldProcessingBadOrdering)
{
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "For: AllDays"}}, // encounter For group outside of a Through
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "Until: 24:00"}}, // encounter Until group outside of a For
                                      {{"field", "For: AllDays"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", 24}}, // encounter Value outside of an Until
                                      {{"field", "Until: 24:00"}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
}

TEST_F(SchedulingTestFixture, TestYearCompactFieldProcessingBadFieldTypes)
{
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "What is this?"}}, // bad field
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
}

TEST_F(SchedulingTestFixture, TestLeapYearInThroughs)
{
    nlohmann::json fields =
        nlohmann::json::array({
                                  {{"field", "Through: 1/31"}}, // includes leap year data
                                  {{"field", "For: AllDays"}},
                                  {{"field", "Until: 24:00"}},
                                  {{"field", 24}},
                                  {{"field", "Through: 02/29"}}, // includes leap year data
                                  {{"field", "For: AllDays"}},
                                  {{"field", "Until: 24:00"}},
                                  {{"field", 24}},
                                  {{"field", "Through: 03/31"}}, // includes leap year data
                                  {{"field", "For: AllDays"}},
                                  {{"field", "Until: 24:00"}},
                                  {{"field", 24}},
                                  {{"field", "Through: 06/21"}}, // includes leap year data
                                  {{"field", "For: AllDays"}},
                                  {{"field", "Until: 24:00"}},
                                  {{"field", 24}},
                                  {{"field", "Through: 12/31"}},
                                  {{"field", "For: AllDays"}},
                                  {{"field", "Until: 24:00"}},
                                  {{"field", 24}}
                              });
    Scheduling::ScheduleCompact compact;
    compact.processFields(fields);
    EXPECT_EQ(5u, compact.throughs.size());
}

TEST_F(SchedulingTestFixture, TestYearCompactFieldProcessingThroughFields)
{
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through:06/30"}}, // no space between Through: and date
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}},
                                      {{"field", "Through: 9/1"}}, // single digit month and day
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 22}},
                                      {{"field", "Through: 12/1"}}, // single digit day
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 22}},
                                      {{"field", "Through:   12/31"}}, // extra space between Through: and date
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}
                                  });
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(4u, compact.throughs.size());
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through:"}}, // missing value on Through block
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 13/AB"}}, // malformed value on Through block
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through:06/30"}}, // insufficient Through coverage
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through:06/30"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}},
                                      {{"field", "Through:06/30"}}, // duplicate Through
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}},
                                      {{"field", "Through:12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through:06/30"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}},
                                      {{"field", "Through:04/30"}}, // bad order
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}},
                                      {{"field", "Through:12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
}


TEST_F(SchedulingTestFixture, TestYearCompactFieldProcessingUntilFields)
{
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until:7:00"}}, // single digit hour
                                      {{"field", 20}},
                                      {{"field", "Until:22:00"}}, // no space between Until: and time
                                      {{"field", 20}},
                                      {{"field", "Until:  24:00"}}, // extra space between Until: and time
                                      {{"field", 24}}
                                  });
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(3u, compact.throughs.front().fors.front().untils.size());
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until:  "}}, // missing value on Until block
                                      {{"field", 24}}
                                  });
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until"}}, // no space at all after Until
                                      {{"field", 24}}
                                  });
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: Qc:00"}}, // malformed value on Until block
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through:12/31"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 22:00"}}, // insufficient Until coverage
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through:06/30"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 21:00"}},
                                      {{"field", 24}},
                                      {{"field", "Until: 21:00"}}, // duplicate Until block
                                      {{"field", 24}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through:06/30"}},
                                      {{"field", "For: AllDays"}},
                                      {{"field", "Until: 21:00"}},
                                      {{"field", 24}},
                                      {{"field", "Until: 19:00"}}, // bad order of Until blocks
                                      {{"field", 24}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
}

TEST_F(SchedulingTestFixture, TestYearCompactFieldProcessingForFields)
{
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: Sunday"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}},
                                      {{"field", "For: AllOtherDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}
            });
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(2u, compact.throughs.front().fors.size());
    }
    {
        // diverse key mix
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: WeekDays SummerDesignDay CustomDay1 CustomDay2"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}},
                                      {{"field", "For: Weekends WinterDesignDay Holiday"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}
                                  });
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(2u, compact.throughs.front().fors.size());
    }
    {
        // allotherdays with other keys
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: SummerDesignDay"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}},
                                      {{"field", "For: WinterDesignDay"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 22}},
                                      {{"field", "For: Weekday Saturday Sunday Holidays AllOtherDays"}},
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 20}}
                                  });
        Scheduling::ScheduleCompact compact;
        compact.processFields(fields);
        EXPECT_EQ(3u, compact.throughs.front().fors.size());
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: "}}, // missing For name
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: BADKEY"}}, // bad key
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: AllOtherDays"}}, // found AllOtherDays on first entry
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
    {
        nlohmann::json fields =
            nlohmann::json::array({
                                      {{"field", "Through: 12/31"}},
                                      {{"field", "For: Sundays"}}, // insufficient coverage
                                      {{"field", "Until: 24:00"}},
                                      {{"field", 24}}});
        Scheduling::ScheduleCompact compact;
        ASSERT_THROW(compact.processFields(fields), std::runtime_error);
    }
}

}
