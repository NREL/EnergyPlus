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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"
using namespace EnergyPlus;
using namespace EnergyPlus::Psychrometrics;

TEST_F(EnergyPlusFixture, Psychrometrics_PsyTsatFnHPb_Test)
{

    InitializePsychRoutines();

    // Test 1: TEMP. IS FROM  20 C  TO   40 C
    Real64 H = 7.5223e4 - 1.78637e4;
    Real64 PB = 1.01325e5;
    Real64 result = PsyTsatFnHPb_raw(H, PB);
    Real64 actual_result = 20.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 2: Cache version of the function - first call
    Real64 cache_miss_result = PsyTsatFnHPb(H, PB);
    EXPECT_NEAR(actual_result, cache_miss_result, 0.001);

    // Test 3: TEMP. IS FROM   0 C  TO   20 C
    H = 2.7298e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = 0.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 4: TEMP. IS FROM   -20 C  TO   0 C
    H = -6.7011e2 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = -20.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 5: TEMP. IS FROM   -40 C  TO   -20 C
    H = -2.21379e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = -40.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 6: TEMP. IS FROM   -60 C  TO   -40 C
    H = -4.2399e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = -60.0;
    EXPECT_NEAR(actual_result, result, 0.1);

    // Test 7: TEMP. IS <  -60 C
    H = -5.2399e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = -60.0;
    EXPECT_NEAR(actual_result, result, 0.1);

    // Test 8: TEMP. IS FROM   40 C  TO   60 C
    H = 1.8379e5 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = 40.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 9: Label90 - TEMP. IS FROM   60 C  TO   80 C
    H = 4.7577e5 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = 60.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 10: Label100 - TEMP. IS FROM   80 C  TO   90 C
    H = 1.5445e6 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = 80.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 11: Label110 - TEMP. IS FROM   90 C  TO   100 C
    H = 3.8353e6 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = 90.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 12: TEMP > 100 C
    H = 4.5866e7 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = 100.0;
    EXPECT_NEAR(actual_result, result, 1);

    // Test 13: PB != 1.0133e5
    H = 7.5223e4 - 1.78637e4;
    PB = 0.91325e5;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = 18.819;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 14: Cache version of the function - hit call
    H = 7.5223e4 - 1.78637e4;
    PB = 1.0133e5;
    actual_result = 20.0;
    Real64 cache_hit_result = PsyTsatFnHPb(H, PB);
    EXPECT_NEAR(actual_result, cache_hit_result, 0.001);
}

TEST_F(EnergyPlusFixture, Psychrometrics_PsyTsatFnPb_Test)
{

    InitializePsychRoutines();

    // Test 1: general
    Real64 PB = 101325.0;
    Real64 result = PsyTsatFnPb_raw(PB);
    Real64 actual_result = 99.974;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 2: Cache version of the function - first call
    PB = 101325.0;
    Real64 cache_result = PsyTsatFnPb(PB);
    EXPECT_NEAR(actual_result, cache_result, 0.001);

    // Test 3: upper bound
    PB = 1555000.0;
    result = PsyTsatFnPb_raw(PB);
    actual_result = 200.0;
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 4: lower bound
    PB = 0.0017;
    result = PsyTsatFnPb_raw(PB);
    actual_result = -100.0;
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 5: zero
    PB = 611.1;
    result = PsyTsatFnPb_raw(PB);
    actual_result = 0.0;
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 6: Cache version of the function - hit call
    PB = 101325.0;
    actual_result = 99.974;
    EXPECT_NEAR(actual_result, cache_result, 0.001);
}

TEST_F(EnergyPlusFixture, Psychrometrics_PsyWFnTdpPb_Test)
{

    Real64 TDP;
    // Sea level pressure
    Real64 PB = 101325.0;
    Real64 W;

    TDP = 99.0;
    W = Psychrometrics::PsyWFnTdpPb(TDP, PB);

    EXPECT_NEAR(17.5250143, W, 0.0001);

    std::string const error_string = delimited_string({
        "   ** Warning ** Calculated partial vapor pressure is greater than the barometric pressure, so that calculated humidity ratio is invalid "
        "(PsyWFnTdpPb).",
        "   **   ~~~   **  Routine=Unknown, Environment=, at Simulation time= 00:00 - 00:00",
        "   **   ~~~   **  Dew-Point= 100.00 Barometric Pressure= 101325.00",
        "   **   ~~~   ** Instead, calculated Humidity Ratio at 99.0 (1 degree less) = 17.5250 will be used. Simulation continues.",
    });

    TDP = 100.0;
    W = Psychrometrics::PsyWFnTdpPb(TDP, PB);
    EXPECT_NEAR(17.5250143, W, 0.0001);
    EXPECT_TRUE(compare_err_stream(error_string, true));

    // Denver barometric pressure
    PB = 81000.0;
    std::string const error_string1 = delimited_string({
        "   ** Warning ** Calculated partial vapor pressure is greater than the barometric pressure, so that calculated humidity ratio is invalid "
        "(PsyWFnTdpPb).",
        "   **   ~~~   **  Routine=Unknown, Environment=, at Simulation time= 00:00 - 00:00",
        "   **   ~~~   **  Dew-Point= 100.00 Barometric Pressure= 81000.00",
        "   **   ~~~   ** Instead, calculated Humidity Ratio at 93.0 (7 degree less) = 20.0794 will be used. Simulation continues.",
    });
    Psychrometrics::iPsyErrIndex(5) = 0;
    W = Psychrometrics::PsyWFnTdpPb(TDP, PB);
    EXPECT_NEAR(20.07942181, W, 0.0001);
    EXPECT_TRUE(compare_err_stream(error_string1, true));
}

TEST_F(EnergyPlusFixture, Psychrometrics_PsyCpAirFn_Test)
{

    InitializePsychRoutines();

    // Test 1: analytical PsyCpAirFnW is independent of temperature
    Real64 W = 0.0080;
    Real64 T = 24.0;
    Real64 local_result = 1.00484e3 + W * 1.85895e3; // PsyCpAirFnW per cp = dh/dT
    Real64 analytic_result = PsyCpAirFnW(W);         // cp = dh/dT
    // check analytical function result
    EXPECT_DOUBLE_EQ(analytic_result, local_result);

    // Test 2: cooling design test condition analytical vs numerical
    W = 0.0085;
    T = 26.0;
    analytic_result = PsyCpAirFnW(W);               // cp = dh/dT
    Real64 numerical_result = PsyCpAirFnWTdb(W, T); // cp = delta_h / delta_T
    // check result
    EXPECT_NEAR(analytic_result, numerical_result, 1.0E-010);

    // Test 3: heating design test condition analytical vs numerical
    W = 0.007;
    T = 10.0;
    analytic_result = PsyCpAirFnW(W);        // cp = dh/dT
    numerical_result = PsyCpAirFnWTdb(W, T); // cp = delta_h / delta_T
    // check result
    EXPECT_NEAR(analytic_result, numerical_result, 1.0E-010);

    // Test 4: dry air test condition analytical vs numerical
    W = 0.0;
    T = 20.0;
    analytic_result = PsyCpAirFnW(W);        // cp = dh/dT
    numerical_result = PsyCpAirFnWTdb(W, T); // cp = delta_h / delta_T
    // check result
    EXPECT_NEAR(analytic_result, numerical_result, 1.0E-010);

    // Test 5: analytical vs numerical cp values for psychomteric chart T and W range
    Real64 SSE = 0.0;
    Real64 Error = 0.0;
    Real64 Error_sum = 0.0;
    Real64 Error_min = 100.0;
    Real64 Error_max = -100.0;
    Real64 Tmax = 50.0;
    Real64 Wmax = 0.030;
    analytic_result = 0.0;
    numerical_result = 0.0;
    for (int TLoop = 0; TLoop <= 100; TLoop++) {
        // update temperature
        T = Tmax - (Tmax / 100.0) * TLoop;
        for (int WLoop = 0; WLoop <= 100; WLoop++) {
            // update humidity ratio
            W = Wmax - (Wmax / 100.0) * WLoop;
            analytic_result = PsyCpAirFnW(W);
            numerical_result = PsyCpAirFnWTdb(W, T);
            Error = numerical_result - analytic_result;
            Error_min = std::min(Error, Error_min);
            Error_max = std::max(Error, Error_max);
            SSE += Error * Error;
            Error_sum += Error;
        }
    }
    Real64 StdError = std::sqrt(SSE / 100);
    Real64 Error_avg = Error_sum / 101;
    // check analytical vs numerical cp values stats
    EXPECT_DOUBLE_EQ(Error_min, -2.8808244678657502e-10);
    EXPECT_DOUBLE_EQ(Error_max, 2.5875124265439808e-10);
    EXPECT_DOUBLE_EQ(Error_avg, 1.5508032789728189e-09);
    EXPECT_DOUBLE_EQ(StdError, 6.7111413639467468e-10);
}

TEST_F(EnergyPlusFixture, Psychrometrics_CpAirValue_Test)
{

    InitializePsychRoutines();

    // Test 1: dry cooling process test, delta enthalpy vs cpair times delta T
    Real64 W1 = 0.0030;
    Real64 T1 = 24.0;
    Real64 W2 = 0.0030;
    Real64 T2 = 20.0;

    // Dry Cooling Test
    Real64 MassFlowRate = 5.0;  // kgDryAir/s
    Real64 CpAir = 1.00484e3 + W1 * 1.85895e3; // PsyCpAirFnW per cp = dh/dT
    Real64 CpAir1 = PsyCpAirFnW(W1);           // PsyCpAirFnW per cp = dh/dT
    Real64 CpAir2 = PsyCpAirFnW(W2);           // PsyCpAirFnW per cp = dh/dT
    // check inputs and intermediate values
    EXPECT_DOUBLE_EQ(W1, W2);
    EXPECT_DOUBLE_EQ(CpAir, CpAir1);
    EXPECT_DOUBLE_EQ(CpAir, CpAir2);
    // check heat transfer rate calc methods
    Real64 Qfrom_mdot_CpAir_DeltaT = MassFlowRate * CpAir * (T1 - T2);

    // get enthalpy at state 1 and 2
    Real64 H1 = PsyHFnTdbW(T1, W1);         // enthaly ait state 1
    Real64 H2 = PsyHFnTdbW(T2, W2);         // enthaly ait state 2
    Real64 Qfrom_mdot_DeltaH = MassFlowRate * (H1 - H2);

    // check heat rate
    EXPECT_DOUBLE_EQ(Qfrom_mdot_CpAir_DeltaT, Qfrom_mdot_DeltaH);

    // Test 2: heating process test, delta enthalpy vs cpair times delta T
    T1 = 10.0;
    T2 = 20.0;
    CpAir = 1.00484e3 + W1 * 1.85895e3;
    Qfrom_mdot_CpAir_DeltaT = MassFlowRate * CpAir * (T2 - T1);

    H1 = PsyHFnTdbW(T1, W1);         // enthaly ait state 1
    H2 = PsyHFnTdbW(T2, W2);         // enthaly ait state 2
    Qfrom_mdot_DeltaH = MassFlowRate * (H2 - H1);

    // check heat transfer rate calc method for heating
    EXPECT_DOUBLE_EQ(Qfrom_mdot_CpAir_DeltaT, Qfrom_mdot_DeltaH);
}
