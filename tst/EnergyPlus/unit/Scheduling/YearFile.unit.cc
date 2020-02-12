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

#include <EnergyPlus/Scheduling/YearFile.hh>
#include "../Scheduling/SchedulingFixture.hh"

namespace EnergyPlus {

TEST_F(SchedulingTestFixture, TestYearFile1)
{
    // as of right now there's really not much to test here, but I can at least confirm that it does have a clear_state method
    EXPECT_TRUE(true);
}
//
//TEST_F(SchedulingTestFixture, TestLineProcessing) {
//    std::vector<std::string> lines = {
//        "Time,Flow,Heat",
//        "1,3.14,2.718",
//        "2,6.28,12"
//    };
//    auto dataSet = Scheduling::ScheduleFile::processCSVLines(lines);
//    EXPECT_EQ(3u, dataSet.size());
//}
//
//TEST_F(SchedulingTestFixture, TestLineProcessingWithTrailingCommas) {
//    std::vector<std::string> lines = {
//        "Time,Flow,Heat,",
//        "1,3.14,2.718,",
//        "2,6.28,12,"
//    };
//    auto dataSet = Scheduling::ScheduleFile::processCSVLines(lines);
//    EXPECT_EQ(4u, dataSet.size()); // expect 3 columns of actual data plus trailing due to the trailing commas
//}
//
//TEST_F(SchedulingTestFixture, TestLineProcessingWithDataGaps) {
//    std::vector<std::string> lines = {
//        "Time,Flow,Heat,State1,State2",
//        "1,3.14,2.718,12,-1",
//        "2,,5.436,24,-2",  // blank flow
//        "3,9.42,8.154,36,-3",
//        "4,12.56,10.872", // completely missing last two
//        "5,15.7,13.59,", // missing last one entirely, but trailing token acts as placeholder for next-to-last one
//        "6,18.84,16.308,72,-6"
//    };
//    auto dataSet = Scheduling::ScheduleFile::processCSVLines(lines);
//    EXPECT_EQ(5u, dataSet.size()); // number of columns is based on header (first) row
//    for (size_t i = 0; i < 5; i++) {
//        EXPECT_EQ(7u, dataSet[i].size()); // each row should have the same length, even with missing data points
//    }
//    EXPECT_EQ("", dataSet[1][2]); // blank flow
//    EXPECT_EQ("5.436", dataSet[2][2]); // data just to the right of the blank flow
//    EXPECT_EQ("9.42", dataSet[1][3]); // data just below the blank flow
//    EXPECT_EQ("", dataSet[3][4]); // completely missing #1
//    EXPECT_EQ("", dataSet[4][4]); // completely missing #2
//    EXPECT_EQ("", dataSet[3][5]); // missing but with trailing token placeholder
//    EXPECT_EQ("", dataSet[4][5]); // completely missing #3
//    EXPECT_EQ("-6", dataSet[4][6]); // final corner value
//}

//TEST_F(SchedulingTestFixture, TestCSVProcessing_TOBEREMOVED) {
//    Scheduling::ScheduleFile::processCSVFile("/tmp/my.csv");
//    EXPECT_NE(Scheduling::fileData.end(), Scheduling::fileData.find("/tmp/my.csv"));
//}

}
