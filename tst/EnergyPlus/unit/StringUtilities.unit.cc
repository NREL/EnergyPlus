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

#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/StringUtilities.hh>

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, readItem)
{
    int i = 0;

    // should read just 12
    EXPECT_TRUE(EnergyPlus::readItem("12", i));
    EXPECT_EQ(i, 12);

    // should fail if unable to process entire input string
    EXPECT_FALSE(EnergyPlus::readItem("1234fgq", i));
    // value is in an unknown state if read failed
    // EXPECT_EQ(i, 12);

    // should read nothing
    EXPECT_FALSE(EnergyPlus::readItem("abc123", i));
    EXPECT_EQ(i, 0);
}

TEST_F(EnergyPlusFixture, readList)
{
    int i{};
    float f{};
    char c{};
    std::string s;

    // with commas
    EXPECT_TRUE(EnergyPlus::readList("1,3.4,a,hello", i, f, c, s));
    EXPECT_EQ(i, 1);
    EXPECT_FLOAT_EQ(f, 3.4f);
    EXPECT_EQ(c, 'a');
    EXPECT_EQ(s, "hello");

    // without
    EXPECT_TRUE(EnergyPlus::readList("bob q 1.5 10", s, c, f, i));
    EXPECT_EQ(i, 10);
    EXPECT_FLOAT_EQ(f, 1.5f);
    EXPECT_EQ(c, 'q');
    EXPECT_EQ(s, "bob");

    // with errors
    EXPECT_FALSE(EnergyPlus::readList("bob;q;1.5;10", s, c, f, i));
    EXPECT_FALSE(EnergyPlus::readList("1;3.4,a,hello", i, f, c, s));
    EXPECT_FALSE(EnergyPlus::readList("a,hello", i, f, c, s));
}
