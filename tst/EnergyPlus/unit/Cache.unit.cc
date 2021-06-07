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

// EnergyPlus::Cache Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// JSON Header
#include <nlohmann/json.hpp>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Cache.hh>

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, Cache_TestJSONToData)
{
    // test jsonToData
    nlohmann::json j;
    j["pi"] = 3.14159;
    j["answer"] = 42;
    j["happy"] = true;
    j["name"] = "energyplus";

    // test double
    double d;
    Cache::jsonToData(*state, d, j, "pi");
    EXPECT_EQ(d, 3.14159);

    // test int
    int i;
    Cache::jsonToData(*state, i, j, "answer");
    EXPECT_EQ(i, 42);

    // test str
    std::string s;
    Cache::jsonToData(*state, s, j, "name");
    EXPECT_EQ(s, "energyplus");

    // test bool
    bool b;
    Cache::jsonToData(*state, b, j, "happy");
    EXPECT_EQ(b, true);

    // bad key
    EXPECT_ANY_THROW(Cache::jsonToData(*state, b, j, "bad_key"));
}

TEST_F(EnergyPlusFixture, Cache_TestJSONToArray)
{
    nlohmann::json j;
    j["array"] = {0, 1, 2, 3};

    Array1D<Real64> arr_double;
    Cache::jsonToArray(*state, arr_double, j, "array");
    EXPECT_EQ(arr_double(0), 0.0);
    EXPECT_EQ(arr_double(1), 1.0);
    EXPECT_EQ(arr_double(2), 2.0);
    EXPECT_EQ(arr_double(3), 3.0);

    Array1D<int> arr_int;
    Cache::jsonToArray(*state, arr_int, j, "array");
    EXPECT_EQ(arr_int(0), 0);
    EXPECT_EQ(arr_int(1), 1);
    EXPECT_EQ(arr_int(2), 2);
    EXPECT_EQ(arr_int(3), 3);

    j["bool"] = {false, true};
    Array1D<bool> arr_bool;
    Cache::jsonToArray(*state, arr_bool, j, "bool");
    EXPECT_EQ(arr_bool(0), false);
    EXPECT_EQ(arr_bool(1), true);

    // bad key
    EXPECT_ANY_THROW(Cache::jsonToArray(*state, arr_bool, j, "bad_key"));
}

TEST_F(EnergyPlusFixture, Cache_TestJSONToArray1)
{
    nlohmann::json j;
    j["array"] = {0, 1, 2, 3};

    Array1D<Real64> arr_double;
    Cache::jsonToArray1(*state, arr_double, j, "array");
    EXPECT_EQ(arr_double(1), 0.0);
    EXPECT_EQ(arr_double(2), 1.0);
    EXPECT_EQ(arr_double(3), 2.0);
    EXPECT_EQ(arr_double(4), 3.0);

    Array1D<int> arr_int;
    Cache::jsonToArray1(*state, arr_int, j, "array");
    EXPECT_EQ(arr_int(1), 0);
    EXPECT_EQ(arr_int(2), 1);
    EXPECT_EQ(arr_int(3), 2);
    EXPECT_EQ(arr_int(4), 3);

    j["bool"] = {false, true};
    Array1D<bool> arr_bool;
    Cache::jsonToArray1(*state, arr_bool, j, "bool");
    EXPECT_EQ(arr_bool(1), false);
    EXPECT_EQ(arr_bool(2), true);

    // bad key
    EXPECT_ANY_THROW(Cache::jsonToArray1(*state, arr_bool, j, "bad_key"));
}

TEST_F(EnergyPlusFixture, Cache_TestArrayToJSON)
{
    Array1D<double> arr = {0.0, 1.0, 2.0};
    nlohmann::json j;
    Cache::arrayToJSON(arr, j, "data");
    auto val = j.at("data");
    EXPECT_EQ(val[0], 0.0);
}