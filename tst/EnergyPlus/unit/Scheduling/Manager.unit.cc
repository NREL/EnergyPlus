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

#include <Scheduling/Manager.hh>
#include <Scheduling/SchedulingFixture.hh>

namespace EnergyPlus {

TEST_F(SchedulingTestFixture, SchedulingManager_TestClearState)
{
    // as of right now there's really not much to test here, but I can at least confirm that it does have a clear_state method
    Scheduling::clear_state();
}

TEST_F(SchedulingTestFixture, SchedulingManager_TestGetScheduleIndex)
{
    Scheduling::processAllSchedules();
    EXPECT_EQ(-1, Scheduling::GetScheduleIndex("my_missing_schedule"));
    EXPECT_EQ(-1, Scheduling::GetScheduleIndex("always on")); // must be capitalized
    EXPECT_EQ(2, Scheduling::GetScheduleIndex("ALWAYS ON")); // must be capitalized
}

TEST_F(SchedulingTestFixture, SchedulingManager_TestZeroIndexScheduleValue)
{
    Scheduling::processAllSchedules();
    EXPECT_EQ(0.0, Scheduling::GetScheduleValue(0));
}

TEST_F(SchedulingTestFixture, SchedulingManager_TestGetScheduleValue)
{
    Scheduling::processAllSchedules();
    EXPECT_EQ(2, Scheduling::GetScheduleIndex("ALWAYS ON"));
    EXPECT_EQ(1.0, Scheduling::GetScheduleValue(2));
}

TEST_F(SchedulingTestFixture, SchedulingManager_TestGetScheduleReference) {
    std::string schedName;
    Scheduling::ScheduleBase *thisReference;
    Scheduling::processAllSchedules();

    schedName = "ALWAYS ON";
    thisReference = Scheduling::getScheduleReference(schedName);
    EXPECT_NE(thisReference, nullptr);
    schedName = "missing_schedule";
    thisReference = Scheduling::getScheduleReference(schedName);
    EXPECT_EQ(thisReference, nullptr);
}

TEST_F(SchedulingTestFixture, SchedulingManager_TestGetScheduleTime) {
    auto & seconds = Scheduling::getScheduleTime;
    bool noLeapYear = false;
    EXPECT_EQ(0, seconds(1, 1, 1, 0, 0, 0, noLeapYear)); // very beginning of the sim, time 0
    EXPECT_EQ(10, seconds(1, 1, 1, 0, 0, 10, noLeapYear)); // ten seconds into the year
    EXPECT_EQ(63, seconds(1, 1, 1, 0, 1, 3, noLeapYear)); // an hour and 3 seconds into the year
    EXPECT_EQ(43200, seconds(1, 1, 1, 12, 0, 0, noLeapYear)); // noon, 1/1
    EXPECT_EQ(86400, seconds(1, 1, 2, 0, 0, 0, noLeapYear)); // midnight, 1/2 morning
    EXPECT_EQ(1756800, seconds(1, 1, 21, 8, 0, 0, noLeapYear)); // 8am 1/21 (typical winter design day morning)
    EXPECT_EQ(2678400, seconds(1, 2, 1, 0, 0, 0, noLeapYear)); // midnight, 2/1 morning -- instantaneous end of January
    EXPECT_EQ(17424000, seconds(1, 7, 21, 16, 0, 0, noLeapYear)); // 4pm 7/21 (typical summer design day afternoon)
    EXPECT_EQ(31535999, seconds(1, 12, 31, 23, 59, 59, noLeapYear)); // 1 second to midnight, 12/31
    EXPECT_EQ(31579200, seconds(2, 1, 1, 12, 0, 0, noLeapYear)); // noon, 1/1, YEAR 2
}

//TEST_F(SchedulingTestFixture, SchedulingManager_TestGetScheduleTimeLeapYear) {
//    auto & seconds = Scheduling::getScheduleTime;
//    bool leapYear = true;
//    EXPECT_EQ(0, seconds(1, 1, 1, 0, 0, 0, leapYear)); // very beginning of the sim, time 0
//    EXPECT_EQ(10, seconds(1, 1, 1, 0, 0, 10, leapYear)); // ten seconds into the year
//    EXPECT_EQ(63, seconds(1, 1, 1, 0, 1, 3, leapYear)); // an hour and 3 seconds into the year
//    EXPECT_EQ(43200, seconds(1, 1, 1, 12, 0, 0, leapYear)); // noon, 1/1
//    EXPECT_EQ(86400, seconds(1, 1, 2, 0, 0, 0, leapYear)); // midnight, 1/2 morning
//    EXPECT_EQ(1756800, seconds(1, 1, 21, 8, 0, 0, leapYear)); // 8am 1/21 (typical winter design day morning)
//    EXPECT_EQ(2678400, seconds(1, 2, 1, 0, 0, 0, leapYear)); // midnight, 2/1 morning -- instantaneous end of January
//    EXPECT_EQ(0, seconds(1, 2, 28, 0, 0, 0, leapYear)); // midnight, 2/28 morning
//    EXPECT_EQ(0, seconds(1, 2, 28, 12, 0, 0, leapYear)); // noon, 2/28
//    EXPECT_EQ(0, seconds(1, 2, 29, 0, 0, 0, leapYear)); // midnight, 2/29 morning
//    EXPECT_EQ(17424000, seconds(1, 7, 21, 16, 0, 0, leapYear)); // 4pm 7/21 (typical summer design day afternoon)
//    EXPECT_EQ(31535999, seconds(1, 12, 31, 23, 59, 59, leapYear)); // 1 second to midnight, 12/31
//    EXPECT_EQ(31579200, seconds(2, 1, 1, 12, 0, 0, leapYear)); // noon, 1/1, YEAR 2
//}

}
