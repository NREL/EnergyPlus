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

#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/Scheduling/YearConstant.hh>
#include <EnergyPlus/Scheduling/Manager.hh>
#include "../Scheduling/SchedulingFixture.hh"

namespace EnergyPlus {

TEST_F(SchedulingTestFixture, ScheduleConstant_TestProcessInput)
{
    std::string const idf_objects = delimited_string({"Schedule:Constant,Always On,,1.0;"});
    ASSERT_TRUE(process_idf(idf_objects));
    Scheduling::ScheduleConstant::processInput();
    EXPECT_EQ(2u, Scheduling::scheduleConstants.size()); // there's an extra hidden constant schedule to account for here.
}

TEST_F(SchedulingTestFixture, ScheduleConstant_TestGetScheduleIndex)
{
    std::string const idf_objects = delimited_string({"Schedule:Constant,Always On,,1.0;", "Schedule:Constant,Always Off,,0.0;"});
    ASSERT_TRUE(process_idf(idf_objects));
    Scheduling::ScheduleConstant::processInput();
    EXPECT_EQ("ALWAYS OFF", Scheduling::scheduleConstants[1].name); // will be upper case, and lexicographically ordered
    EXPECT_NEAR(0.0, Scheduling::scheduleConstants[1].value, 0.0001);
}

TEST_F(SchedulingTestFixture, ScheduleConstant_TestZeroethSchedule)
{
    std::string const idf_objects = delimited_string({"Schedule:Constant,Always Off,,0.0;"});
    ASSERT_TRUE(process_idf(idf_objects));
    Scheduling::ScheduleConstant::processInput();
    EXPECT_EQ(0.0, Scheduling::scheduleConstants[1].value);
}

TEST_F(SchedulingTestFixture, ScheduleConstant_TestClearState)
{
    std::string const idf_objects = delimited_string({"Schedule:Constant,Always On,,1.0;"});
    ASSERT_TRUE(process_idf(idf_objects));
    Scheduling::ScheduleConstant::processInput();
    Scheduling::ScheduleConstant::clear_state();
    EXPECT_EQ(0u, Scheduling::scheduleConstants.size());
}

TEST_F(SchedulingTestFixture, ScheduleConstant_TestDuplicateNames)
{
    std::string const idf_objects = delimited_string({"Schedule:Constant,Always On,,1.0;", "Schedule:Compact,Always On,,1.0;"});
    ASSERT_TRUE(process_idf(idf_objects));
    ASSERT_THROW(Scheduling::processAllSchedules(EnergyPlus::OutputFiles::getSingleton()), std::runtime_error);
}

TEST_F(SchedulingTestFixture, ScheduleConstant_UpdateValueAndEMS)
{
    std::string const idf_objects = delimited_string({"Schedule:Constant,Always On,,1.0;"});
    ASSERT_TRUE(process_idf(idf_objects));
    auto thisSched = Scheduling::getScheduleReference("ALWAYS ON");
    EXPECT_EQ(1.0, thisSched->value);
    // now override with EMS
    thisSched->emsActuatedOn = true;
    thisSched->emsActuatedValue = 14.0;
    Scheduling::updateAllSchedules(0.0);
    EXPECT_EQ(14.0, thisSched->value);
}

TEST_F(SchedulingTestFixture, ScheduleConstant_TestValidation)
{
    std::string const idf_objects = delimited_string({
        "Schedule:Constant,Always Low,ThisTypeLimit,1.0;",
        "Schedule:Constant,Always Hi,ThisTypeLimit,0.1;",
        "ScheduleTypeLimits,ThisTypeLimit,0.2,0.5,Continuous;"
    });
    ASSERT_TRUE(process_idf(idf_objects));
    Scheduling::processAllSchedules(EnergyPlus::OutputFiles::getSingleton());
    EnergyPlus::DataEnvironment::RunPeriodStartDayOfWeek = 1;
    EnergyPlus::WeatherManager::Envrn = 1;
    EnergyPlus::WeatherManager::Environment.allocate(1);
    EnergyPlus::WeatherManager::Environment(1).KindOfEnvrn = EnergyPlus::DataGlobals::ksRunPeriodWeather;
    ASSERT_THROW(Scheduling::prepareSchedulesForNewEnvironment(), std::runtime_error);
}

} // namespace EnergyPlus
