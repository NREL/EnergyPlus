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

// EnergyPlus::SortAndStringUtilities Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/SortAndStringUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::SortAndStringUtilities;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, SortAndStringUtilitiesTest_Basic)
{
    ShowMessage("Begin Test: SortAndStringUtilitiesTest, Basic");

    Array1D_string Alphas({"ZEBRA", "LION", "RACOON", "BOA", "LEMUR"});
    Array1D_int iAlphas(5);
    SetupAndSort(Alphas, iAlphas);
    EXPECT_TRUE(eq(Array1D_int({4, 5, 2, 3, 1}), iAlphas));
}

TEST_F(EnergyPlusFixture, SortAndStringUtilitiesTest_findItemInSortedListUnderscoreTest)
{
    static Array1D_string ListOfObjects; // stored variable names
    static Array1D_int iListOfObjects;
    ListOfObjects.allocate(7);

    ListOfObjects = Array1D_string({
        // list which has been incorrectly sorted
        "SYSTEM NODE STANDARD DENSITY VOLUME FLOW RATE",
        "SYSTEM NODE TEMPERATURE",
        "SYSTEM NODE WETBULB TEMPERATURE",
        "S_CCFRAC",
        "T_TRIG",
        "VRF HEAT PUMP CONDENSER INLET TEMPERATURE",
        "VRF HEAT PUMP COOLING COP",
    });

    int NumObjectDefs = ListOfObjects.size();

    iListOfObjects.allocate(NumObjectDefs);
    SetupAndSort(ListOfObjects, iListOfObjects); // list is resorted

    auto index = UtilityRoutines::FindItemInSortedList("SYSTEM NODE TEMPERATURE", ListOfObjects, NumObjectDefs);

    EXPECT_EQ(3, index);
}
