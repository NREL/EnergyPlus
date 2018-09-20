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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, RecurringWarningTest)
{

    // void ShowRecurringSevereErrorAtEnd(std::string const &Message,         // Message automatically written to "error file" at end of simulation
    //    int &MsgIndex,                      // Recurring message index, if zero, next available index is assigned
    //    Optional<Real64 const> ReportMaxOf, // Track and report the max of the values passed to this argument
    //    Optional<Real64 const> ReportMinOf, // Track and report the min of the values passed to this argument
    //    Optional<Real64 const> ReportSumOf, // Track and report the sum of the values passed to this argument
    //    std::string const &ReportMaxUnits,  // optional char string (<=15 length) of units for max value
    //    std::string const &ReportMinUnits,  // optional char string (<=15 length) of units for min value
    //    std::string const &ReportSumUnits   // optional char string (<=15 length) of units for sum value
    //)

    std::string myMessage1 = "Test message 1";
    // proper call to ShowRecurringWarningErrorAtEnd to set up new recurring warning
    int ErrIndex1 = 0;
    ShowRecurringWarningErrorAtEnd(myMessage1, ErrIndex1);
    EXPECT_EQ(ErrIndex1, 1);
    EXPECT_EQ(DataErrorTracking::RecurringErrors.size(), 1u);
    EXPECT_EQ(" ** Warning ** " + myMessage1, DataErrorTracking::RecurringErrors(1).Message);
    EXPECT_EQ(1, DataErrorTracking::RecurringErrors(1).Count);

    std::string myMessage2 = "Test message 2";
    // improper call to ShowRecurringWarningErrorAtEnd to set up new recurring warning
    int ErrIndex2 = 6;
    ShowRecurringWarningErrorAtEnd(myMessage2, ErrIndex2);
    EXPECT_EQ(ErrIndex2, 2); // ShowRecurringWarningErrorAtEnd handles improper index and returns correct value
    EXPECT_EQ(DataErrorTracking::RecurringErrors.size(), 2u);
    EXPECT_EQ(" ** Warning ** " + myMessage2, DataErrorTracking::RecurringErrors(2).Message);
    EXPECT_EQ(1, DataErrorTracking::RecurringErrors(2).Count);

    ErrIndex2 = 6;
    ShowRecurringWarningErrorAtEnd(myMessage2, ErrIndex2);
    EXPECT_EQ(ErrIndex2, 2); // ShowRecurringWarningErrorAtEnd handles improper index and returns correct value
    EXPECT_EQ(DataErrorTracking::RecurringErrors.size(), 2u);
    EXPECT_EQ(" ** Warning ** " + myMessage2, DataErrorTracking::RecurringErrors(2).Message);
    EXPECT_EQ(2, DataErrorTracking::RecurringErrors(2).Count);

    std::string myMessage3 = "Test message 3";
    ShowRecurringContinueErrorAtEnd(myMessage3, ErrIndex1);
    // index gets updated with correct value
    EXPECT_EQ(ErrIndex1, 3);
    EXPECT_EQ(DataErrorTracking::RecurringErrors.size(), 3u);
    EXPECT_EQ(" **   ~~~   ** " + myMessage3, DataErrorTracking::RecurringErrors(3).Message);
    EXPECT_EQ(1, DataErrorTracking::RecurringErrors(3).Count);

    std::string myMessage4 = "Test message 4";
    ShowRecurringSevereErrorAtEnd(myMessage4, ErrIndex1);
    // index gets updated with correct value
    EXPECT_EQ(ErrIndex1, 4);
    EXPECT_EQ(DataErrorTracking::RecurringErrors.size(), 4u);
    EXPECT_EQ(" ** Severe  ** " + myMessage4, DataErrorTracking::RecurringErrors(4).Message);
    EXPECT_EQ(1, DataErrorTracking::RecurringErrors(4).Count);

    // same message for different show message type (changed severe to warning) should be valid
    ShowRecurringWarningErrorAtEnd(myMessage4, ErrIndex1);
    // index gets updated with correct value
    EXPECT_EQ(ErrIndex1, 5);
    EXPECT_EQ(" ** Warning ** " + myMessage4, DataErrorTracking::RecurringErrors(5).Message);
}

TEST_F(EnergyPlusFixture, DisplayMessageTest)
{
    DisplayString("Testing");
    EXPECT_TRUE(has_cout_output(true));
    // Open six files to get unit number beyond 6 - these all get closed later by EnergyPlusFixture
    DataGlobals::OutputFileStandard = FindUnitNumber(DataStringGlobals::outputEsoFileName);
    DataGlobals::OutputFileInits = FindUnitNumber(DataStringGlobals::outputEioFileName);
    DataGlobals::OutputFileMeters = FindUnitNumber(DataStringGlobals::outputMtrFileName);
    DataGlobals::OutputFileBNDetails = FindUnitNumber(DataStringGlobals::outputBndFileName);
    DataGlobals::OutputFileZoneSizing = FindUnitNumber(DataStringGlobals::outputZszCsvFileName);
    DataGlobals::OutputFileSysSizing = FindUnitNumber(DataStringGlobals::outputSszCsvFileName);
    DisplayString("Testing");
    EXPECT_TRUE(has_cout_output(true));
    // repeat this one - before fix, this broke cout_stream
    int unitNum; // found unit number
    unitNum = FindUnitNumber(DataStringGlobals::outputSszCsvFileName);
    EXPECT_FALSE(has_cout_output(true));
    DisplayString("Testing");
    EXPECT_TRUE(has_cout_output(true));
}
