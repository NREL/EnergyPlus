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

// EnergyPlus::SortAndStringUtilities Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// Objexx Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/General.hh>

namespace EnergyPlus {

using namespace EnergyPlus::General;

TEST_F(EnergyPlusFixture, General_ParseTime)
{
    int Hours;
    int Minutes;
    Real64 Seconds;
    { // Time = 0
        General::ParseTime(0, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(0, Seconds);
    }
    { // Time = 1
        General::ParseTime(1, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(1, Seconds);
    }
    { // Time = 59
        General::ParseTime(59, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(59, Seconds);
    }
    { // Time = 59.9
        General::ParseTime(59.9, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(59.9, Seconds);
    }
    { // Time = 59.99
        General::ParseTime(59.99, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(59.99, Seconds);
    }
    { // Time = 59.999
        General::ParseTime(59.999, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(59.999, Seconds);
    }
    { // Time = 60
        General::ParseTime(60, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(1, Minutes);
        EXPECT_DOUBLE_EQ(0, Seconds);
    }
    { // Time = 61
        General::ParseTime(61, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(1, Minutes);
        EXPECT_DOUBLE_EQ(1, Seconds);
    }
    { // Time = 3599
        General::ParseTime(3599, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(59, Minutes);
        EXPECT_DOUBLE_EQ(59, Seconds);
    }
    { // Time = 3600
        General::ParseTime(3600, Hours, Minutes, Seconds);
        EXPECT_EQ(1, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(0, Seconds);
    }
    { // Time = 3601
        General::ParseTime(3601, Hours, Minutes, Seconds);
        EXPECT_EQ(1, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(1, Seconds);
    }
    { // Time = 3661
        General::ParseTime(3661, Hours, Minutes, Seconds);
        EXPECT_EQ(1, Hours);
        EXPECT_EQ(1, Minutes);
        EXPECT_DOUBLE_EQ(1, Seconds);
    }
    { // Time = 86399
        General::ParseTime(86399, Hours, Minutes, Seconds);
        EXPECT_EQ(23, Hours);
        EXPECT_EQ(59, Minutes);
        EXPECT_DOUBLE_EQ(59, Seconds);
    }
    { // Time = 86400
        General::ParseTime(86400, Hours, Minutes, Seconds);
        EXPECT_EQ(24, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(0, Seconds);
    }
    { // Time = 86401
        // Should probably be a failure
        General::ParseTime(86401, Hours, Minutes, Seconds);
        EXPECT_EQ(24, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(1, Seconds);
    }
    { // Time = -1
        // Should probably be a failure
        General::ParseTime(-1, Hours, Minutes, Seconds);
        EXPECT_EQ(0, Hours);
        EXPECT_EQ(0, Minutes);
        EXPECT_DOUBLE_EQ(-1, Seconds);
    }
}

TEST_F(EnergyPlusFixture, General_CreateTimeString)
{
    { // Time = 0
        EXPECT_EQ("00:00:00.0", General::CreateTimeString(0));
    }
    { // Time = 1
        EXPECT_EQ("00:00:01.0", General::CreateTimeString(1));
    }
    { // Time = 59
        EXPECT_EQ("00:00:59.0", General::CreateTimeString(59));
    }
    { // Time = 59.9
        EXPECT_EQ("00:00:59.9", General::CreateTimeString(59.9));
    }
    { // Time = 59.99
        EXPECT_EQ("00:00:60.0", General::CreateTimeString(59.99));
    }
    { // Time = 59.999
        EXPECT_EQ("00:00:60.0", General::CreateTimeString(59.999));
    }
    { // Time = 60
        EXPECT_EQ("00:01:00.0", General::CreateTimeString(60));
    }
    { // Time = 61
        EXPECT_EQ("00:01:01.0", General::CreateTimeString(61));
    }
    { // Time = 3600
        EXPECT_EQ("01:00:00.0", General::CreateTimeString(3600));
    }
    { // Time = 3599
        EXPECT_EQ("00:59:59.0", General::CreateTimeString(3599));
    }
    { // Time = 3601
        EXPECT_EQ("01:00:01.0", General::CreateTimeString(3601));
    }
    { // Time = 3661
        EXPECT_EQ("01:01:01.0", General::CreateTimeString(3661));
    }
    { // Time = 86399
        EXPECT_EQ("23:59:59.0", General::CreateTimeString(86399));
    }
    { // Time = 86400
        EXPECT_EQ("24:00:00.0", General::CreateTimeString(86400));
    }
    { // Time = 86401
        // Should probably be a failure
        EXPECT_EQ("24:00:01.0", General::CreateTimeString(86401));
    }
    { // Time = -1
        // Should probably be a failure
        EXPECT_EQ("00:00:-1.0", General::CreateTimeString(-1));
    }
}

TEST_F(EnergyPlusFixture, General_CreateTimeIntervalString)
{
    { // Time = 0 - 1
        EXPECT_EQ("00:00:00.0 - 00:00:01.0", General::CreateTimeIntervalString(0, 1));
    }
    { // Time = 0 - 0
        EXPECT_EQ("00:00:00.0 - 00:00:00.0", General::CreateTimeIntervalString(0, 0));
    }
    { // Time = 1 - 0
        EXPECT_EQ("00:00:01.0 - 00:00:00.0", General::CreateTimeIntervalString(1, 0));
    }
    { // Time = 1 - 59
        EXPECT_EQ("00:00:01.0 - 00:00:59.0", General::CreateTimeIntervalString(1, 59));
    }
    { // Time = 59 - 59.9
        EXPECT_EQ("00:00:59.0 - 00:00:59.9", General::CreateTimeIntervalString(59, 59.9));
    }
}

Real64 Residual([[maybe_unused]] EnergyPlusData &state, Real64 const Frac, [[maybe_unused]] std::array<Real64, 1> const &Par)
{
    Real64 Residual;
    Real64 Request = 1.10;
    Real64 Actual;

    Actual = 1.0 + 2.0 * Frac + 10.0 * Frac * Frac;

    Residual = (Actual - Request) / Request;

    return Residual;
}

Real64 ResidualTest([[maybe_unused]] EnergyPlusData &state, Real64 const Frac, [[maybe_unused]] std::array<Real64, 2> const &Par)
{
    Real64 ResidualTest;
    Real64 Request = 1.0 + 1.0e-12;
    Real64 Actual;

    Actual = 1.0 + 2.0 * Frac + 10.0 * Frac * Frac;

    ResidualTest = (Actual - Request) / Request;
    // Request = Par[0] + 1.0e-12;
    return ResidualTest;
}

TEST_F(EnergyPlusFixture, General_SolveRootTest)
{
    // New feature: Multiple solvers

    Real64 ErrorToler = 0.00001;
    int MaxIte = 30;
    int SolFla;
    Real64 Frac;

    std::array<Real64, 1> dummyParameters;

    General::SolveRoot(*state, ErrorToler, MaxIte, SolFla, Frac, Residual, 0.0, 1.0, dummyParameters);
    EXPECT_EQ(-1, SolFla);

    state->dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver = HVACSystemRootSolverAlgorithm::RegulaFalsiThenBisection;
    state->dataRootFinder->HVACSystemRootFinding.NumOfIter = 10;
    General::SolveRoot(*state, ErrorToler, MaxIte, SolFla, Frac, Residual, 0.0, 1.0, dummyParameters);
    EXPECT_EQ(28, SolFla);
    EXPECT_NEAR(0.041420287, Frac, ErrorToler);

    state->dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver = HVACSystemRootSolverAlgorithm::Bisection;
    General::SolveRoot(*state, ErrorToler, 40, SolFla, Frac, Residual, 0.0, 1.0, dummyParameters);
    EXPECT_EQ(17, SolFla);
    EXPECT_NEAR(0.041420287, Frac, ErrorToler);

    state->dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver = HVACSystemRootSolverAlgorithm::BisectionThenRegulaFalsi;
    General::SolveRoot(*state, ErrorToler, 40, SolFla, Frac, Residual, 0.0, 1.0, dummyParameters);
    EXPECT_EQ(12, SolFla);
    EXPECT_NEAR(0.041420287, Frac, ErrorToler);

    state->dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver = HVACSystemRootSolverAlgorithm::Alternation;
    state->dataRootFinder->HVACSystemRootFinding.NumOfIter = 3;
    General::SolveRoot(*state, ErrorToler, 40, SolFla, Frac, Residual, 0.0, 1.0, dummyParameters);
    EXPECT_EQ(15, SolFla);
    EXPECT_NEAR(0.041420287, Frac, ErrorToler);

    // Add a unit test to deal with vary small X value for #6515
    state->dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver = HVACSystemRootSolverAlgorithm::RegulaFalsi;
    Real64 small = 1.0e-11;
    std::array<Real64, 2> Par = {1.0, 1.0}; // Function parameters
    General::SolveRoot(*state, ErrorToler, 40, SolFla, Frac, ResidualTest, 0.0, small, Par);
    EXPECT_EQ(-1, SolFla);
}

TEST_F(EnergyPlusFixture, nthDayOfWeekOfMonth_test)
{
    // J.Glazer - August 2017
    //		nthDayOfWeekOfMonth(
    //			int const & dayOfWeek, // day of week (Sunday=1, Monday=2, ...)
    //			int const & nthTime,   // nth time the day of the week occurs (first monday, third tuesday, ..)
    //			int const & monthNumber // January = 1
    //		)

    state->dataEnvrn->CurrentYearIsLeapYear = false; // based on 2017
    state->dataEnvrn->RunPeriodStartDayOfWeek = 1;   // sunday

    EXPECT_EQ(1, nthDayOfWeekOfMonth(*state, 1, 1, 1));  // first sunday of january
    EXPECT_EQ(8, nthDayOfWeekOfMonth(*state, 1, 2, 1));  // second sunday of january
    EXPECT_EQ(15, nthDayOfWeekOfMonth(*state, 1, 3, 1)); // third sunday of january
    EXPECT_EQ(22, nthDayOfWeekOfMonth(*state, 1, 4, 1)); // fourth sunday of january

    EXPECT_EQ(2, nthDayOfWeekOfMonth(*state, 2, 1, 1));  // first monday of january
    EXPECT_EQ(10, nthDayOfWeekOfMonth(*state, 3, 2, 1)); // second tuesday of january
    EXPECT_EQ(19, nthDayOfWeekOfMonth(*state, 5, 3, 1)); // third thursday of january
    EXPECT_EQ(28, nthDayOfWeekOfMonth(*state, 7, 4, 1)); // fourth saturday of january

    EXPECT_EQ(32, nthDayOfWeekOfMonth(*state, 4, 1, 2)); // first wednesday of february
    EXPECT_EQ(60, nthDayOfWeekOfMonth(*state, 4, 1, 3)); // first wednesday of march

    state->dataEnvrn->CurrentYearIsLeapYear = true;
    state->dataEnvrn->RunPeriodStartDayOfWeek = 1; // sunday

    EXPECT_EQ(32, nthDayOfWeekOfMonth(*state, 4, 1, 2)); // first wednesday of february
    EXPECT_EQ(61, nthDayOfWeekOfMonth(*state, 5, 1, 3)); // first thursday of march
    EXPECT_EQ(67, nthDayOfWeekOfMonth(*state, 4, 1, 3)); // first wednesday of march

    state->dataEnvrn->CurrentYearIsLeapYear = true; // based on 2016
    state->dataEnvrn->RunPeriodStartDayOfWeek = 6;  // friday

    EXPECT_EQ(3, nthDayOfWeekOfMonth(*state, 1, 1, 1));  // first sunday of january
    EXPECT_EQ(10, nthDayOfWeekOfMonth(*state, 1, 2, 1)); // second sunday of january
    EXPECT_EQ(17, nthDayOfWeekOfMonth(*state, 1, 3, 1)); // third sunday of january
    EXPECT_EQ(24, nthDayOfWeekOfMonth(*state, 1, 4, 1)); // fourth sunday of january
    EXPECT_EQ(31, nthDayOfWeekOfMonth(*state, 1, 5, 1)); // fifth sunday of january

    EXPECT_EQ(1, nthDayOfWeekOfMonth(*state, 6, 1, 1));  // first friday of january
    EXPECT_EQ(8, nthDayOfWeekOfMonth(*state, 6, 2, 1));  // second friday of january
    EXPECT_EQ(15, nthDayOfWeekOfMonth(*state, 6, 3, 1)); // third friday of january
    EXPECT_EQ(22, nthDayOfWeekOfMonth(*state, 6, 4, 1)); // fourth friday of january

    EXPECT_EQ(34, nthDayOfWeekOfMonth(*state, 4, 1, 2)); // first wednesday of february
    EXPECT_EQ(62, nthDayOfWeekOfMonth(*state, 4, 1, 3)); // first wednesday of march
}

TEST_F(EnergyPlusFixture, General_EpexpTest)
{
    // Global exp function test
    Real64 x;
    Real64 d(1.0);
    Real64 y;

    // Underflow and near zero tests
    x = -69.0;
    y = epexp(x, d);
    EXPECT_NEAR(0.0, y, 1.0E-20);

    x = -700.0;
    y = epexp(x, d);
    EXPECT_NEAR(0.0, y, 1.0E-20);

    x = -1000.0; // Will cause underflow
    y = epexp(x, d);
    EXPECT_EQ(0.0, y);

    // Divide by zero tests
    d = 0.0;
    x = -1000.0;
    y = epexp(x, d);
    EXPECT_EQ(0.0, y);

    d = 0.0;
    x = 1000.0;
    y = epexp(x, d);
    EXPECT_EQ(0.0, y);

    /*// Overflow and near-overflow tests (Not currently used in code)
    x = 10.0;
    d = 1.0;
    y = epexpOverflow(x, d);
    EXPECT_NEAR(22026.46579480, y, 0.00001);

    x = 800.0;
    d = 1.0;
    y = epexpOverflow(x, d);
    EXPECT_NEAR(1.0142320547350045e+304, y, 1.0E2);
    */
}
} // namespace EnergyPlus
