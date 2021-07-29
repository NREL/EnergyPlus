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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::Psychrometrics;

TEST_F(EnergyPlusFixture, Psychrometrics_PsyTsatFnHPb_Test)
{

    InitializePsychRoutines(*state);

    // Test 1: TEMP. IS FROM  20 C  TO   40 C
    Real64 H = 7.5223e4 - 1.78637e4;
    Real64 PB = 1.01325e5;
    Real64 result = PsyTsatFnHPb_raw(*state, H, PB);
    Real64 actual_result = 20.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 2: Cache version of the function - first call
    Real64 cache_miss_result = PsyTsatFnHPb(*state, H, PB);
    EXPECT_NEAR(actual_result, cache_miss_result, 0.001);

    // Test 3: TEMP. IS FROM   0 C  TO   20 C
    H = 2.7298e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = 0.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 4: TEMP. IS FROM   -20 C  TO   0 C
    H = -6.7011e2 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = -20.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 5: TEMP. IS FROM   -40 C  TO   -20 C
    H = -2.21379e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = -40.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 6: TEMP. IS FROM   -60 C  TO   -40 C
    H = -4.2399e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = -60.0;
    EXPECT_NEAR(actual_result, result, 0.1);

    // Test 7: TEMP. IS <  -60 C
    H = -5.2399e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = -60.0;
    EXPECT_NEAR(actual_result, result, 0.1);

    // Test 8: TEMP. IS FROM   40 C  TO   60 C
    H = 1.8379e5 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = 40.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 9: Label90 - TEMP. IS FROM   60 C  TO   80 C
    H = 4.7577e5 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = 60.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 10: Label100 - TEMP. IS FROM   80 C  TO   90 C
    H = 1.5445e6 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = 80.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 11: Label110 - TEMP. IS FROM   90 C  TO   100 C
    H = 3.8353e6 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = 90.0;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 12: TEMP > 100 C
    H = 4.5866e7 - 1.78637e4;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = 100.0;
    EXPECT_NEAR(actual_result, result, 1);

    // Test 13: PB != 1.0133e5
    H = 7.5223e4 - 1.78637e4;
    PB = 0.91325e5;
    result = PsyTsatFnHPb_raw(*state, H, PB);
    actual_result = 18.819;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 14: Cache version of the function - hit call
    H = 7.5223e4 - 1.78637e4;
    PB = 1.0133e5;
    actual_result = 20.0;
    Real64 cache_hit_result = PsyTsatFnHPb(*state, H, PB);
    EXPECT_NEAR(actual_result, cache_hit_result, 0.001);
}

TEST_F(EnergyPlusFixture, Psychrometrics_PsyTsatFnPb_Test)
{

    InitializePsychRoutines(*state);

    // Test 1: general
    Real64 PB = 101325.0;
    Real64 result = PsyTsatFnPb_raw(*state, PB);
    Real64 actual_result = 99.974;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 2: Cache version of the function - first call
    PB = 101325.0;
    Real64 cache_result = PsyTsatFnPb(*state, PB);
    EXPECT_NEAR(actual_result, cache_result, 0.001);

    // Test 3: upper bound
    PB = 1555000.0;
    result = PsyTsatFnPb_raw(*state, PB);
    actual_result = 200.0;
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 4: lower bound
    PB = 0.0017;
    result = PsyTsatFnPb_raw(*state, PB);
    actual_result = -100.0;
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 5: zero
    PB = 611.1;
    result = PsyTsatFnPb_raw(*state, PB);
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
    W = Psychrometrics::PsyWFnTdpPb(*state, TDP, PB);

    EXPECT_NEAR(17.5250143, W, 0.0001);

    std::string const error_string = delimited_string({
        "   ** Warning ** Calculated partial vapor pressure is greater than the barometric pressure, so that calculated humidity ratio is invalid "
        "(PsyWFnTdpPb).",
        "   **   ~~~   **  Routine=Unknown, Environment=, at Simulation time= 00:00 - 00:00",
        "   **   ~~~   **  Dew-Point= 100.00 Barometric Pressure= 101325.00",
        "   **   ~~~   ** Instead, calculated Humidity Ratio at 99.0 (1 degree less) = 17.5250 will be used. Simulation continues.",
    });

    TDP = 100.0;
    W = Psychrometrics::PsyWFnTdpPb(*state, TDP, PB);
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
    state->dataPsychrometrics->iPsyErrIndex[static_cast<int>(PsychrometricFunction::WFnTdpPb)] = 0;

    W = Psychrometrics::PsyWFnTdpPb(*state, TDP, PB);
    EXPECT_NEAR(20.07942181, W, 0.0001);
    EXPECT_TRUE(compare_err_stream(error_string1, true));
}

inline Real64 PsyCpAirFnWTdb(Real64 const dw, // humidity ratio {kgWater/kgDryAir}
                             Real64 const T   // input temperature {Celsius}
)
{

    //// NOTE: THIS FUNCTION IS DEPRECATED AND USED FOR TESTING PURPOSES ONLY

    // FUNCTION INFORMATION:
    //       AUTHOR         J. C. VanderZee
    //       DATE WRITTEN   Feb. 1994
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function provides the heat capacity of air {J/kg-C} as function of humidity ratio.

    // METHODOLOGY EMPLOYED:
    // take numerical derivative of PsyHFnTdbW function

    // REFERENCES:
    // see PsyHFnTdbW ref. to ASHRAE Fundamentals
    // USAGE:  cpa = PsyCpAirFnWTdb(w,T)

    // Static locals
    static Real64 dwSave(-100.0);
    static Real64 Tsave(-100.0);
    static Real64 cpaSave(-100.0);

    // check if last call had the same input and if it did just use the saved output
    if ((Tsave == T) && (dwSave == dw)) return cpaSave;

    // compute heat capacity of air
    Real64 const w(max(dw, 1.0e-5));
    Real64 const cpa((PsyHFnTdbW(T + 0.1, w) - PsyHFnTdbW(T, w)) * 10.0); // result => heat capacity of air {J/kg-C}

    // save values for next call
    dwSave = dw;
    Tsave = T;
    cpaSave = cpa;

    return cpa;
}

TEST_F(EnergyPlusFixture, Psychrometrics_PsyCpAirFn_Test)
{

    InitializePsychRoutines(*state);

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

    InitializePsychRoutines(*state);

    // Test 1: dry cooling process test, delta enthalpy vs cpair times delta T
    Real64 W1 = 0.0030;
    Real64 T1 = 24.0;
    Real64 W2 = 0.0030;
    Real64 T2 = 20.0;

    // Dry Cooling Test
    Real64 MassFlowRate = 5.0;                 // kgDryAir/s
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
    Real64 H1 = PsyHFnTdbW(T1, W1); // enthaly ait state 1
    Real64 H2 = PsyHFnTdbW(T2, W2); // enthaly ait state 2
    Real64 Qfrom_mdot_DeltaH = MassFlowRate * (H1 - H2);

    // check heat rate
    EXPECT_DOUBLE_EQ(Qfrom_mdot_CpAir_DeltaT, Qfrom_mdot_DeltaH);

    // Test 2: heating process test, delta enthalpy vs cpair times delta T
    T1 = 10.0;
    T2 = 20.0;
    CpAir = 1.00484e3 + W1 * 1.85895e3;
    Qfrom_mdot_CpAir_DeltaT = MassFlowRate * CpAir * (T2 - T1);

    H1 = PsyHFnTdbW(T1, W1); // enthaly ait state 1
    H2 = PsyHFnTdbW(T2, W2); // enthaly ait state 2
    Qfrom_mdot_DeltaH = MassFlowRate * (H2 - H1);

    // check heat transfer rate calc method for heating
    EXPECT_DOUBLE_EQ(Qfrom_mdot_CpAir_DeltaT, Qfrom_mdot_DeltaH);
}

TEST_F(EnergyPlusFixture, Psychrometrics_PsyTwbFnTdbWPb_Test)
{

    InitializePsychRoutines(*state);

    // Test when wet bulb temperature is below zero
    Real64 TDB = 1;   // C
    Real64 W = 0.002; // Kg.water/Kg.dryair
    Real64 Pb = 101325.0;
    Real64 result = PsyTwbFnTdbWPb(*state, TDB, W, Pb);
    Real64 expected_result = -2.200; // expected result from psychrometrics chart
    EXPECT_NEAR(result, expected_result, 0.001);
}

TEST_F(EnergyPlusFixture, Psychrometrics_CpAirAverageValue_Test)
{

    InitializePsychRoutines(*state);

    // Test 1: heating process, constant humidity ratio
    Real64 W1 = 0.0030;
    Real64 W2 = 0.0030;
    Real64 CpAirIn = PsyCpAirFnW(W1);                   // cp of air at state 1
    Real64 CpAirOut = PsyCpAirFnW(W2);                  // cp of air at state 2
    Real64 CpAir_result = PsyCpAirFnW(0.5 * (W1 + W2)); // cp of air at average humidity ratio
    Real64 CpAir_average = (CpAirIn + CpAirOut) / 2;
    ;
    // check heating results
    EXPECT_DOUBLE_EQ(CpAirIn, 1.00484e3 + W1 * 1.85895e3);
    EXPECT_DOUBLE_EQ(CpAirOut, 1.00484e3 + W2 * 1.85895e3);
    EXPECT_DOUBLE_EQ(CpAir_result, CpAir_average);

    // Test 2: cooling Processes, dehumidified air
    W1 = 0.010;
    W2 = 0.008;
    CpAirIn = PsyCpAirFnW(W1);                   // cp of air at state 1
    CpAirOut = PsyCpAirFnW(W2);                  // cp of air at state 2
    CpAir_result = PsyCpAirFnW(0.5 * (W1 + W2)); // cp of air at average humidity ratio
    CpAir_average = (CpAirIn + CpAirOut) / 2;
    ;
    // check cooling results
    EXPECT_DOUBLE_EQ(CpAirIn, 1.00484e3 + W1 * 1.85895e3);
    EXPECT_DOUBLE_EQ(CpAirOut, 1.00484e3 + W2 * 1.85895e3);
    EXPECT_DOUBLE_EQ(CpAir_result, CpAir_average);
}
TEST_F(EnergyPlusFixture, Psychrometrics_Interpolation_Sample_Test)
{
    // verify sample data for interpolation
    InitializePsychRoutines(*state);
    Real64 tsat_psy;
    Real64 error = 0.0;
    int i;
    // sample bin size =64 Pa; sample size =1651 (continous)
    constexpr std::array<Real64, 1651> tsat_fn_pb_pressure = {
        0,      64,     128,    192,    256,    320,    384,    448,    512,    576,    640,    704,    768,    832,    896,    960,    1024,
        1088,   1152,   1216,   1280,   1344,   1408,   1472,   1536,   1600,   1664,   1728,   1792,   1856,   1920,   1984,   2048,   2112,
        2176,   2240,   2304,   2368,   2432,   2496,   2560,   2624,   2688,   2752,   2816,   2880,   2944,   3008,   3072,   3136,   3200,
        3264,   3328,   3392,   3456,   3520,   3584,   3648,   3712,   3776,   3840,   3904,   3968,   4032,   4096,   4160,   4224,   4288,
        4352,   4416,   4480,   4544,   4608,   4672,   4736,   4800,   4864,   4928,   4992,   5056,   5120,   5184,   5248,   5312,   5376,
        5440,   5504,   5568,   5632,   5696,   5760,   5824,   5888,   5952,   6016,   6080,   6144,   6208,   6272,   6336,   6400,   6464,
        6528,   6592,   6656,   6720,   6784,   6848,   6912,   6976,   7040,   7104,   7168,   7232,   7296,   7360,   7424,   7488,   7552,
        7616,   7680,   7744,   7808,   7872,   7936,   8000,   8064,   8128,   8192,   8256,   8320,   8384,   8448,   8512,   8576,   8640,
        8704,   8768,   8832,   8896,   8960,   9024,   9088,   9152,   9216,   9280,   9344,   9408,   9472,   9536,   9600,   9664,   9728,
        9792,   9856,   9920,   9984,   10048,  10112,  10176,  10240,  10304,  10368,  10432,  10496,  10560,  10624,  10688,  10752,  10816,
        10880,  10944,  11008,  11072,  11136,  11200,  11264,  11328,  11392,  11456,  11520,  11584,  11648,  11712,  11776,  11840,  11904,
        11968,  12032,  12096,  12160,  12224,  12288,  12352,  12416,  12480,  12544,  12608,  12672,  12736,  12800,  12864,  12928,  12992,
        13056,  13120,  13184,  13248,  13312,  13376,  13440,  13504,  13568,  13632,  13696,  13760,  13824,  13888,  13952,  14016,  14080,
        14144,  14208,  14272,  14336,  14400,  14464,  14528,  14592,  14656,  14720,  14784,  14848,  14912,  14976,  15040,  15104,  15168,
        15232,  15296,  15360,  15424,  15488,  15552,  15616,  15680,  15744,  15808,  15872,  15936,  16000,  16064,  16128,  16192,  16256,
        16320,  16384,  16448,  16512,  16576,  16640,  16704,  16768,  16832,  16896,  16960,  17024,  17088,  17152,  17216,  17280,  17344,
        17408,  17472,  17536,  17600,  17664,  17728,  17792,  17856,  17920,  17984,  18048,  18112,  18176,  18240,  18304,  18368,  18432,
        18496,  18560,  18624,  18688,  18752,  18816,  18880,  18944,  19008,  19072,  19136,  19200,  19264,  19328,  19392,  19456,  19520,
        19584,  19648,  19712,  19776,  19840,  19904,  19968,  20032,  20096,  20160,  20224,  20288,  20352,  20416,  20480,  20544,  20608,
        20672,  20736,  20800,  20864,  20928,  20992,  21056,  21120,  21184,  21248,  21312,  21376,  21440,  21504,  21568,  21632,  21696,
        21760,  21824,  21888,  21952,  22016,  22080,  22144,  22208,  22272,  22336,  22400,  22464,  22528,  22592,  22656,  22720,  22784,
        22848,  22912,  22976,  23040,  23104,  23168,  23232,  23296,  23360,  23424,  23488,  23552,  23616,  23680,  23744,  23808,  23872,
        23936,  24000,  24064,  24128,  24192,  24256,  24320,  24384,  24448,  24512,  24576,  24640,  24704,  24768,  24832,  24896,  24960,
        25024,  25088,  25152,  25216,  25280,  25344,  25408,  25472,  25536,  25600,  25664,  25728,  25792,  25856,  25920,  25984,  26048,
        26112,  26176,  26240,  26304,  26368,  26432,  26496,  26560,  26624,  26688,  26752,  26816,  26880,  26944,  27008,  27072,  27136,
        27200,  27264,  27328,  27392,  27456,  27520,  27584,  27648,  27712,  27776,  27840,  27904,  27968,  28032,  28096,  28160,  28224,
        28288,  28352,  28416,  28480,  28544,  28608,  28672,  28736,  28800,  28864,  28928,  28992,  29056,  29120,  29184,  29248,  29312,
        29376,  29440,  29504,  29568,  29632,  29696,  29760,  29824,  29888,  29952,  30016,  30080,  30144,  30208,  30272,  30336,  30400,
        30464,  30528,  30592,  30656,  30720,  30784,  30848,  30912,  30976,  31040,  31104,  31168,  31232,  31296,  31360,  31424,  31488,
        31552,  31616,  31680,  31744,  31808,  31872,  31936,  32000,  32064,  32128,  32192,  32256,  32320,  32384,  32448,  32512,  32576,
        32640,  32704,  32768,  32832,  32896,  32960,  33024,  33088,  33152,  33216,  33280,  33344,  33408,  33472,  33536,  33600,  33664,
        33728,  33792,  33856,  33920,  33984,  34048,  34112,  34176,  34240,  34304,  34368,  34432,  34496,  34560,  34624,  34688,  34752,
        34816,  34880,  34944,  35008,  35072,  35136,  35200,  35264,  35328,  35392,  35456,  35520,  35584,  35648,  35712,  35776,  35840,
        35904,  35968,  36032,  36096,  36160,  36224,  36288,  36352,  36416,  36480,  36544,  36608,  36672,  36736,  36800,  36864,  36928,
        36992,  37056,  37120,  37184,  37248,  37312,  37376,  37440,  37504,  37568,  37632,  37696,  37760,  37824,  37888,  37952,  38016,
        38080,  38144,  38208,  38272,  38336,  38400,  38464,  38528,  38592,  38656,  38720,  38784,  38848,  38912,  38976,  39040,  39104,
        39168,  39232,  39296,  39360,  39424,  39488,  39552,  39616,  39680,  39744,  39808,  39872,  39936,  40000,  40064,  40128,  40192,
        40256,  40320,  40384,  40448,  40512,  40576,  40640,  40704,  40768,  40832,  40896,  40960,  41024,  41088,  41152,  41216,  41280,
        41344,  41408,  41472,  41536,  41600,  41664,  41728,  41792,  41856,  41920,  41984,  42048,  42112,  42176,  42240,  42304,  42368,
        42432,  42496,  42560,  42624,  42688,  42752,  42816,  42880,  42944,  43008,  43072,  43136,  43200,  43264,  43328,  43392,  43456,
        43520,  43584,  43648,  43712,  43776,  43840,  43904,  43968,  44032,  44096,  44160,  44224,  44288,  44352,  44416,  44480,  44544,
        44608,  44672,  44736,  44800,  44864,  44928,  44992,  45056,  45120,  45184,  45248,  45312,  45376,  45440,  45504,  45568,  45632,
        45696,  45760,  45824,  45888,  45952,  46016,  46080,  46144,  46208,  46272,  46336,  46400,  46464,  46528,  46592,  46656,  46720,
        46784,  46848,  46912,  46976,  47040,  47104,  47168,  47232,  47296,  47360,  47424,  47488,  47552,  47616,  47680,  47744,  47808,
        47872,  47936,  48000,  48064,  48128,  48192,  48256,  48320,  48384,  48448,  48512,  48576,  48640,  48704,  48768,  48832,  48896,
        48960,  49024,  49088,  49152,  49216,  49280,  49344,  49408,  49472,  49536,  49600,  49664,  49728,  49792,  49856,  49920,  49984,
        50048,  50112,  50176,  50240,  50304,  50368,  50432,  50496,  50560,  50624,  50688,  50752,  50816,  50880,  50944,  51008,  51072,
        51136,  51200,  51264,  51328,  51392,  51456,  51520,  51584,  51648,  51712,  51776,  51840,  51904,  51968,  52032,  52096,  52160,
        52224,  52288,  52352,  52416,  52480,  52544,  52608,  52672,  52736,  52800,  52864,  52928,  52992,  53056,  53120,  53184,  53248,
        53312,  53376,  53440,  53504,  53568,  53632,  53696,  53760,  53824,  53888,  53952,  54016,  54080,  54144,  54208,  54272,  54336,
        54400,  54464,  54528,  54592,  54656,  54720,  54784,  54848,  54912,  54976,  55040,  55104,  55168,  55232,  55296,  55360,  55424,
        55488,  55552,  55616,  55680,  55744,  55808,  55872,  55936,  56000,  56064,  56128,  56192,  56256,  56320,  56384,  56448,  56512,
        56576,  56640,  56704,  56768,  56832,  56896,  56960,  57024,  57088,  57152,  57216,  57280,  57344,  57408,  57472,  57536,  57600,
        57664,  57728,  57792,  57856,  57920,  57984,  58048,  58112,  58176,  58240,  58304,  58368,  58432,  58496,  58560,  58624,  58688,
        58752,  58816,  58880,  58944,  59008,  59072,  59136,  59200,  59264,  59328,  59392,  59456,  59520,  59584,  59648,  59712,  59776,
        59840,  59904,  59968,  60032,  60096,  60160,  60224,  60288,  60352,  60416,  60480,  60544,  60608,  60672,  60736,  60800,  60864,
        60928,  60992,  61056,  61120,  61184,  61248,  61312,  61376,  61440,  61504,  61568,  61632,  61696,  61760,  61824,  61888,  61952,
        62016,  62080,  62144,  62208,  62272,  62336,  62400,  62464,  62528,  62592,  62656,  62720,  62784,  62848,  62912,  62976,  63040,
        63104,  63168,  63232,  63296,  63360,  63424,  63488,  63552,  63616,  63680,  63744,  63808,  63872,  63936,  64000,  64064,  64128,
        64192,  64256,  64320,  64384,  64448,  64512,  64576,  64640,  64704,  64768,  64832,  64896,  64960,  65024,  65088,  65152,  65216,
        65280,  65344,  65408,  65472,  65536,  65600,  65664,  65728,  65792,  65856,  65920,  65984,  66048,  66112,  66176,  66240,  66304,
        66368,  66432,  66496,  66560,  66624,  66688,  66752,  66816,  66880,  66944,  67008,  67072,  67136,  67200,  67264,  67328,  67392,
        67456,  67520,  67584,  67648,  67712,  67776,  67840,  67904,  67968,  68032,  68096,  68160,  68224,  68288,  68352,  68416,  68480,
        68544,  68608,  68672,  68736,  68800,  68864,  68928,  68992,  69056,  69120,  69184,  69248,  69312,  69376,  69440,  69504,  69568,
        69632,  69696,  69760,  69824,  69888,  69952,  70016,  70080,  70144,  70208,  70272,  70336,  70400,  70464,  70528,  70592,  70656,
        70720,  70784,  70848,  70912,  70976,  71040,  71104,  71168,  71232,  71296,  71360,  71424,  71488,  71552,  71616,  71680,  71744,
        71808,  71872,  71936,  72000,  72064,  72128,  72192,  72256,  72320,  72384,  72448,  72512,  72576,  72640,  72704,  72768,  72832,
        72896,  72960,  73024,  73088,  73152,  73216,  73280,  73344,  73408,  73472,  73536,  73600,  73664,  73728,  73792,  73856,  73920,
        73984,  74048,  74112,  74176,  74240,  74304,  74368,  74432,  74496,  74560,  74624,  74688,  74752,  74816,  74880,  74944,  75008,
        75072,  75136,  75200,  75264,  75328,  75392,  75456,  75520,  75584,  75648,  75712,  75776,  75840,  75904,  75968,  76032,  76096,
        76160,  76224,  76288,  76352,  76416,  76480,  76544,  76608,  76672,  76736,  76800,  76864,  76928,  76992,  77056,  77120,  77184,
        77248,  77312,  77376,  77440,  77504,  77568,  77632,  77696,  77760,  77824,  77888,  77952,  78016,  78080,  78144,  78208,  78272,
        78336,  78400,  78464,  78528,  78592,  78656,  78720,  78784,  78848,  78912,  78976,  79040,  79104,  79168,  79232,  79296,  79360,
        79424,  79488,  79552,  79616,  79680,  79744,  79808,  79872,  79936,  80000,  80064,  80128,  80192,  80256,  80320,  80384,  80448,
        80512,  80576,  80640,  80704,  80768,  80832,  80896,  80960,  81024,  81088,  81152,  81216,  81280,  81344,  81408,  81472,  81536,
        81600,  81664,  81728,  81792,  81856,  81920,  81984,  82048,  82112,  82176,  82240,  82304,  82368,  82432,  82496,  82560,  82624,
        82688,  82752,  82816,  82880,  82944,  83008,  83072,  83136,  83200,  83264,  83328,  83392,  83456,  83520,  83584,  83648,  83712,
        83776,  83840,  83904,  83968,  84032,  84096,  84160,  84224,  84288,  84352,  84416,  84480,  84544,  84608,  84672,  84736,  84800,
        84864,  84928,  84992,  85056,  85120,  85184,  85248,  85312,  85376,  85440,  85504,  85568,  85632,  85696,  85760,  85824,  85888,
        85952,  86016,  86080,  86144,  86208,  86272,  86336,  86400,  86464,  86528,  86592,  86656,  86720,  86784,  86848,  86912,  86976,
        87040,  87104,  87168,  87232,  87296,  87360,  87424,  87488,  87552,  87616,  87680,  87744,  87808,  87872,  87936,  88000,  88064,
        88128,  88192,  88256,  88320,  88384,  88448,  88512,  88576,  88640,  88704,  88768,  88832,  88896,  88960,  89024,  89088,  89152,
        89216,  89280,  89344,  89408,  89472,  89536,  89600,  89664,  89728,  89792,  89856,  89920,  89984,  90048,  90112,  90176,  90240,
        90304,  90368,  90432,  90496,  90560,  90624,  90688,  90752,  90816,  90880,  90944,  91008,  91072,  91136,  91200,  91264,  91328,
        91392,  91456,  91520,  91584,  91648,  91712,  91776,  91840,  91904,  91968,  92032,  92096,  92160,  92224,  92288,  92352,  92416,
        92480,  92544,  92608,  92672,  92736,  92800,  92864,  92928,  92992,  93056,  93120,  93184,  93248,  93312,  93376,  93440,  93504,
        93568,  93632,  93696,  93760,  93824,  93888,  93952,  94016,  94080,  94144,  94208,  94272,  94336,  94400,  94464,  94528,  94592,
        94656,  94720,  94784,  94848,  94912,  94976,  95040,  95104,  95168,  95232,  95296,  95360,  95424,  95488,  95552,  95616,  95680,
        95744,  95808,  95872,  95936,  96000,  96064,  96128,  96192,  96256,  96320,  96384,  96448,  96512,  96576,  96640,  96704,  96768,
        96832,  96896,  96960,  97024,  97088,  97152,  97216,  97280,  97344,  97408,  97472,  97536,  97600,  97664,  97728,  97792,  97856,
        97920,  97984,  98048,  98112,  98176,  98240,  98304,  98368,  98432,  98496,  98560,  98624,  98688,  98752,  98816,  98880,  98944,
        99008,  99072,  99136,  99200,  99264,  99328,  99392,  99456,  99520,  99584,  99648,  99712,  99776,  99840,  99904,  99968,  100032,
        100096, 100160, 100224, 100288, 100352, 100416, 100480, 100544, 100608, 100672, 100736, 100800, 100864, 100928, 100992, 101056, 101120,
        101184, 101248, 101312, 101376, 101440, 101504, 101568, 101632, 101696, 101760, 101824, 101888, 101952, 102016, 102080, 102144, 102208,
        102272, 102336, 102400, 102464, 102528, 102592, 102656, 102720, 102784, 102848, 102912, 102976, 103040, 103104, 103168, 103232, 103296,
        103360, 103424, 103488, 103552, 103616, 103680, 103744, 103808, 103872, 103936, 104000, 104064, 104128, 104192, 104256, 104320, 104384,
        104448, 104512, 104576, 104640, 104704, 104768, 104832, 104896, 104960, 105024, 105088, 105152, 105216, 105280, 105344, 105408, 105472,
        105536, 105600};
    constexpr std::array<Real64, 1651> tsat_fn_pb_tsat = {
        -100,        -24.88812836, -17.74197121, -13.36696483, -10.17031904, -7.635747635, -5.528025298, -3.719474549, -2.132789207, -0.717496548,
        0.635182846, 1.961212857,  3.184455749,  4.320585222,  5.381890646,  6.378191532,  7.317464071,  8.206277019,  9.050107781,  9.853572827,
        10.62060139, 11.35456508,  12.05838073,  12.73458802,  13.38541367,  14.01282043,  14.61854774,  15.2041452,   15.77099766,  16.32035156,
        16.85332662, 17.37094188,  17.87411665,  18.36369351,  18.84043883,  19.30505535,  19.75818947,  20.200435,    20.63234161,  21.05441545,
        21.46712703, 21.87091368,  22.26617973,  22.65330582,  23.03264392,  23.40452474,  23.76925695,  24.12713074,  24.4784194,   24.82337925,
        25.16224961, 25.49525941,  25.82262353,  26.14454343,  26.46121131,  26.77280989,  27.07951029,  27.38147718,  27.67886392,  27.97181871,
        28.26048327, 28.54498875,  28.82546493,  29.10203155,  29.37480614,  29.64389783,  29.90941288,  30.17145332,  30.43011544,  30.68549275,
        30.93767354, 31.1867437,   31.43278471,  31.67587535,  31.91609169,  32.15350578,  32.38818672,  32.62020399,  32.84962034,  33.07649705,
        33.30089772, 33.52287753,  33.7424911,   33.95979768,  34.17484337,  34.38768181,  34.59836177,  34.8069286,   35.01342775,  35.21790433,
        35.42039942, 35.62095455,  35.81961149,  36.0164063,   36.21137695,  36.40456083,  36.59599334,  36.78570706,  36.97373442,  37.16011096,
        37.34486543, 37.5280276,   37.70962755,  37.88969498,  38.06825698,  38.24534078,  38.4209727,   38.59517724,  38.7679823,   38.93940873,
        39.10948012, 39.2782229,   39.44565725,  39.6118041,   39.77668652,  39.94032485,  40.10273846,  40.26394796,  40.42397281,  40.58283123,
        40.74054192, 40.89712156,  41.0525873,   41.20696011,  41.3602526,   41.5124819,   41.66366455,  41.81381521,  41.96295168,  42.11108573,
        42.25823182, 42.40440716,  42.54962287,  42.69389419,  42.8372333,   42.97965197,  43.12116511,  43.2617866,   43.40152372,  43.5403934,
        43.67840528, 43.81556931,  43.95189963,  44.0874032,   44.22209605,  44.35598543,  44.48908135,  44.62139435,  44.75293555,  44.88371503,
        45.01374001, 45.14302132,  45.27156964,  45.39939168,  45.52649676,  45.65289264,  45.77859083,  45.90359809,  46.02792097,  46.15157051,
        46.27455279, 46.39687482,  46.51854662,  46.63957407,  46.75996667,  46.87972868,  46.99886779,  47.11739332,  47.23530984,  47.35262368,
        47.46934408, 47.58547585,  47.7010257,   47.81600131,  47.93040599,  48.0442474,   48.15753313,  48.27026594,  48.38245292,  48.49410119,
        48.60521578, 48.71580123,  48.82586296,  48.93540734,  49.04443945,  49.15296424,  49.26098645,  49.36851262,  49.47554597,  49.58209088,
        49.68815438, 49.7937401,   49.89885468,  50.00349843,  50.10767869,  50.21139915,  50.31466562,  50.41748127,  50.51984969,  50.6217774,
        50.72326533, 50.8243191,   50.92494506,  51.02514303,  51.12491978,  51.22427873,  51.32322162,  51.42175505,  51.51988223,  51.61760565,
        51.71492849, 51.81185626,  51.90838995,  52.00453601,  52.10029659,  52.19567395,  52.29067211,  52.38529434,  52.47954517,  52.57342522,
        52.66694094, 52.76009179,  52.85288285,  52.94531781,  53.03739768,  53.12912747,  53.22050752,  53.31154238,  53.40223643,  53.49258938,
        53.58260762, 53.67228883,  53.76164079,  53.85066348,  53.93935919,  54.02773136,  54.11578345,  54.20351687,  54.29093451,  54.37803778,
        54.46483065, 54.55131516,  54.63749308,  54.7233676,   54.80894018,  54.89421491,  54.97919066,  55.0638724,   55.14826341,  55.23236115,
        55.31617223, 55.39969882,  55.48294042,  55.56589968,  55.64857895,  55.7309827,   55.8131092,   55.89496315,  55.97654474,  56.05785774,
        56.1389022,  56.21968099,  56.30019707,  56.38045024,  56.46044318,  56.54017846,  56.61965822,  56.69888215,  56.77785343,  56.85657191,
        56.93504425, 57.01326641,  57.09124313,  57.16897715,  57.24646717,  57.32371609,  57.40072768,  57.4774991,   57.55403662,  57.63033718,
        57.70640793, 57.7822446,   57.85785243,  57.93323193,  58.00838488,  58.08331234,  58.15801692,  58.23249741,  58.30675724,  58.38079823,
        58.4546222,  58.52822764,  58.60161837,  58.67479622,  58.74775927,  58.82051339,  58.89305587,  58.96539129,  59.03751832,  59.1094403,
        59.18115654, 59.25267006,  59.32398161,  59.39509291,  59.46600373,  59.53671502,  59.60723006,  59.67755023,  59.74767505,  59.81760513,
        59.88734393, 59.95688928,  60.02624654,  60.09541405,  60.16439371,  60.2331876,   60.30179567,  60.37021755,  60.43845741,  60.50651412,
        60.57438886, 60.64208387,  60.70960018,  60.77693773,  60.84409885,  60.91108356,  60.97789194,  61.04452769,  61.11098811,  61.1772789,
        61.2433966,  61.30934573,  61.37512456,  61.44073521,  61.50617791,  61.57145516,  61.6365656,   61.70151178,  61.76629472,  61.83091455,
        61.89537314, 61.95967045,  62.02380873,  62.08778598,  62.15160576,  62.21526616,  62.2787729,   62.34212215,  62.40531644,  62.46835564,
        62.53124254, 62.59397652,  62.65655972,  62.7189898,   62.78127202,  62.84340443,  62.90538831,  62.96722333,  63.02891098,  63.09045474,
        63.15185221, 63.21310404,  63.27421125,  63.33517636,  63.395999,    63.45668024,  63.5172196,   63.57761821,  63.63787789,  63.69799922,
        63.75798178, 63.81782639,  63.87753525,  63.93710759,  63.99654311,  64.05584549,  64.11501608,  64.1740502,   64.23294864,  64.29172001,
        64.35035847, 64.40886389,  64.4672399,   64.5254877,   64.5836074,   64.64159658,  64.69945949,  64.75719386,  64.81480044,  64.87228539,
        64.92964246, 64.98687573,  65.04398502,  65.10096852,  65.15782959,  65.21457202,  65.27118885,  65.32768769,  65.38406204,  65.44031895,
        65.49645605, 65.55247365,  65.60837577,  65.66415606,  65.7198208,   65.7753675,   65.8307992,   65.88611471,  65.94131694,  65.99640529,
        66.05137563, 66.10623566,  66.16098047,  66.21561318,  66.27013565,  66.32454189,  66.37884177,  66.43303089,  66.48710748,  66.54107712,
        66.59493603, 66.64868734,  66.70232946,  66.75586325,  66.80929006,  66.86261073,  66.91582873,  66.96893645,  67.02193922,  67.07484084,
        67.12763318, 67.18032374,  67.23291261,  67.2853971,   67.33777697,  67.39005562,  67.44223342,  67.49431315,  67.54628565,  67.59816124,
        67.64993655, 67.70161425,  67.75319106,  67.80466762,  67.85604816,  67.90732765,  67.95851385,  68.00959903,  68.06059149,  68.11148356,
        68.16228279, 68.21298473,  68.26359087,  68.31410431,  68.36452488,  68.41484999,  68.46508104,  68.51521777,  68.56526159,  68.6152136,
        68.6650752,  68.71484646,  68.76452459,  68.81410894,  68.86360503,  68.9130119,   68.96232932,  69.01155648,  69.06069295,  69.10974389,
        69.15870478, 69.20757526,  69.25636202,  69.30505708,  69.35366941,  69.40219039,  69.45062774,  69.49898231,  69.54724573,  69.59542511,
        69.64352333, 69.69153266,  69.73946144,  69.78730174,  69.83506042,  69.8827346,   69.93032771,  69.97783945,  70.02526447,  70.07260834,
        70.11987362, 70.16705659,  70.21415392,  70.26117672,  70.3081124,   70.35497219,  70.40175094,  70.44845033,  70.49506926,  70.5416108,
        70.58807245, 70.63445733,  70.68076429,  70.72698884,  70.77314,     70.81921262,  70.86521029,  70.91112621,  70.956971,    71.0027362,
        71.04842458, 71.09404158,  71.13958183,  71.18504519,  71.23043212,  71.27574987,  71.3209896,   71.36615442,  71.41124924,  71.45626937,
        71.50121362, 71.5460872,   71.59088911,  71.63561453,  71.68027218,  71.72485702,  71.76936872,  71.8138079,   71.85817753,  71.90247673,
        71.94670456, 71.99086376,  72.03495158,  72.07896913,  72.12291519,  72.16679298,  72.21060503,  72.25434249,  72.29801465,  72.34161996,
        72.38515599, 72.42862145,  72.47201901,  72.51535392,  72.5586171,   72.60181527,  72.64494367,  72.68800759,  72.73100471,  72.77393894,
        72.81680315, 72.85960377,  72.90233649,  72.94500537,  72.98760805,  73.03014695,  73.07262276,  73.11503298,  73.1573782,   73.19966128,
        73.24187731, 73.28403304,  73.32612125,  73.36815237,  73.41011791,  73.45201898,  73.49385988,  73.53563549,  73.57735287,  73.61900651,
        73.6605957,  73.70212525,  73.74359587,  73.78500201,  73.82635101,  73.86763719,  73.90886374,  73.95002747,  73.99113361,  74.03217995,
        74.07316582, 74.11409233,  74.15495789,  74.19576541,  74.23651553,  74.27720372,  74.31783561,  74.35840878,  74.39892536,  74.43938115,
        74.47978004, 74.52011928,  74.56040515,  74.60062901,  74.64080092,  74.68091054,  74.72096571,  74.76096771,  74.80090708,  74.84079646,
        74.88062483, 74.9204009,   74.96011942,  74.99977909,  75.03939008,  75.07893992,  75.11843698,  75.15788211,  75.19726977,  75.23660313,
        75.27588023, 75.31510624,  75.3542739,   75.39339345,  75.43245611,  75.47146405,  75.51042254,  75.54932382,  75.58817471,  75.62697021,
        75.66571461, 75.70440632,  75.74304491,  75.78163044,  75.82016525,  75.85865056,  75.89708068,  75.93545883,  75.97378492,  76.0120621,
        76.05028712, 76.08846034,  76.12658418,  76.16465767,  76.2026797,   76.24065195,  76.27857222,  76.31644424,  76.35426512,  76.39203326,
        76.42975531, 76.46742597,  76.50504989,  76.54262349,  76.58014732,  76.6176226,   76.6550495,   76.69242337,  76.72975292,  76.76703263,
        76.80426597, 76.84144989,  76.87858556,  76.91567266,  76.95271451,  76.98970341,  77.02664946,  77.06354801,  77.10039946,  77.13720248,
        77.17395743, 77.21066858,  77.24732868,  77.28394674,  77.32051481,  77.35703974,  77.3935179,   77.4299481,   77.46633278,  77.50267234,
        77.53896767, 77.5752132,   77.6114198,   77.64757673,  77.68368996,  77.71975535,  77.75577955,  77.79175544,  77.82768892,  77.86357885,
        77.89942204, 77.93522269,  77.97097918,  78.00669057,  78.04235789,  78.07798,     78.11356019,  78.14909725,  78.18459127,  78.22004104,
        78.25544735, 78.29081033,  78.32613242,  78.3614084,   78.39664434,  78.43183732,  78.46698385,  78.50209439,  78.53715718,  78.57218022,
        78.60716047, 78.64209878,  78.6769951,   78.71185226,  78.746663,    78.78143682,  78.81616758,  78.85085484,  78.88550274,  78.92010982,
        78.9546777,  78.98920242,  79.02368438,  79.05812941,  79.09253277,  79.12689452,  79.16121566,  79.19549993,  79.22974184,  79.26394059,
        79.29810401, 79.33222714,  79.36630773,  79.40034862,  79.43435261,  79.46831575,  79.50224105,  79.5361278,   79.56997224,  79.60377852,
        79.63754844, 79.6712756,   79.70496707,  79.7386195,   79.77223095,  79.80580692,  79.8393431,   79.87284009,  79.90630324,  79.9397239,
        79.9731062,  80.00645432,  80.03976078,  80.07303132,  80.10626466,  80.13946141,  80.17261862,  80.2057402,   80.23882443,  80.27187067,
        80.30488333, 80.33785562,  80.37079099,  80.40368818,  80.43655134,  80.46937694,  80.50216672,  80.53492197,  80.56763831,  80.60032048,
        80.63296434, 80.66557006,  80.69814406,  80.73067924,  80.7631833,   80.7956466,   80.82807796,  80.86046929,  80.89282813,  80.92515223,
        80.95744088, 80.98969387,  81.02191322,  81.05409659,  81.08624412,  81.11835765,  81.15043579,  81.1824791,   81.21448773,  81.24646338,
        81.27840107, 81.31030725,  81.34217912,  81.3740162,   81.40582266,  81.43759037,  81.4693268,   81.50102878,  81.53269738,  81.564328,
        81.59592988, 81.62749803,  81.65902927,  81.69053248,  81.72199906,  81.75343024,  81.78483194,  81.81620159,  81.84753434,  81.87883589,
        81.91010465, 81.94134123,  81.97254592,  82.0037149,   82.03485388,  82.06595769,  82.09703065,  82.1280729,   82.15908193,  82.19005726,
        82.22100267, 82.25191447,  82.28279684,  82.31364612,  82.34446044,  82.37524835,  82.40600156,  82.43672281,  82.46741359,  82.49807143,
        82.52869953, 82.55929763,  82.58986383,  82.62039574,  82.65090102,  82.68137014,  82.71181314,  82.74222283,  82.77260316,  82.80295069,
        82.83327058, 82.86355667,  82.89381203,  82.92404037,  82.95423709,  82.98440109,  83.0145352,   83.04464057,  83.0747166,   83.10476355,
        83.13477865, 83.16476369,  83.19471663,  83.22464346,  83.25454104,  83.28440557,  83.31424367,  83.34405051,  83.37382643,  83.403574,
        83.43329363, 83.46298145,  83.49264184,  83.52227618,  83.55187566,  83.5814507,   83.61099411,  83.64050958,  83.66999636,  83.69945328,
        83.72888435, 83.75828467,  83.78765457,  83.81699798,  83.84631261,  83.87559999,  83.90485773,  83.93408864,  83.96328773,  83.9924604,
        84.02160824, 84.05072359,  84.07981307,  84.10887322,  84.13790674,  84.16691375,  84.19589213,  84.22484302,  84.25376552,  84.28266125,
        84.31152889, 84.34036894,  84.36918258,  84.39796509,  84.426723,    84.4554566,   84.48416077,  84.51283457,  84.54148605,  84.57010873,
        84.59870416, 84.62727398,  84.65581296,  84.68432935,  84.71281921,  84.74128034,  84.76971427,  84.79812356,  84.82650714,  84.85486414,
        84.88319335, 84.91149363,  84.93977033,  84.96802107,  84.99624746,  85.02444686,  85.05261848,  85.08076234,  85.10888389,  85.13697661,
        85.16504751, 85.19308996,  85.22110697,  85.24909745,  85.27706406,  85.3050058,   85.33291891,  85.36080872,  85.38867288,  85.41650897,
        85.44432122, 85.47211082,  85.49987157,  85.5276116,   85.55532315,  85.58301033,  85.61067074,  85.63830715,  85.66592225,  85.69350747,
        85.72106906, 85.74860734,  85.77612007,  85.80360992,  85.83107309,  85.85851235,  85.88592674,  85.91331883,  85.940685,    85.96802559,
        85.99534052, 86.02263527,  86.04990355,  86.07714803,  86.1043677,   86.13156409,  86.15873472,  86.18588473,  86.21300672,  86.24010603,
        86.267184,   86.29423588,  86.32126306,  86.34826803,  86.37525004,  86.40220887,  86.42914117,  86.45605219,  86.48294074,  86.50980317,
        86.53664208, 86.56346029,  86.59025279,  86.61702366,  86.64376933,  86.6704935,   86.6971967,   86.72387239,  86.75052605,  86.77716019,
        86.80376776, 86.83035345,  86.85691767,  86.88345664,  86.90997465,  86.93646901,  86.96294,     86.98938903,  87.01581375,  87.04222061,
        87.06860086, 87.09495848,  87.12129645,  87.14761207,  87.17390292,  87.20017179,  87.22641888,  87.25264536,  87.27884551,  87.30502537,
        87.33118401, 87.35732172,  87.38343463,  87.40952679,  87.43559834,  87.46164719,  87.48767415,  87.51367842,  87.53966135,  87.56562576,
        87.59156283, 87.61748293,  87.64337717,  87.66925215,  87.69510798,  87.72093736,  87.74674778,  87.77253857,  87.79830413,  87.82405258,
        87.84977774, 87.87547845,  87.90116109,  87.92682487,  87.95246439,  87.9780827,   88.00367984,  88.02925702,  88.05481207,  88.08035027,
        88.10586302, 88.13135623,  88.15683029,  88.18227924,  88.2077092,   88.23311926,  88.25851218,  88.28387904,  88.30922637,  88.33455433,
        88.35986038, 88.38515007,  88.41041472,  88.43566009,  88.46088564,  88.48608997,  88.51127475,  88.53643864,  88.5615843,   88.58670705,
        88.61180925, 88.63689372,  88.66195676,  88.6870023,   88.71202293,  88.73702912,  88.76201161,  88.7869734,   88.81191947,  88.83684387,
        88.86174532, 88.88662923,  88.91149412,  88.93633903,  88.96116308,  88.98596844,  89.01075422,  89.03552234,  89.0602694,   89.08499342,
        89.10970333, 89.1343894,   89.15906041,  89.18370902,  89.20834007,  89.23295171,  89.25754223,  89.28211345,  89.30666784,  89.33119931,
        89.35571584, 89.38021293,  89.40468873,  89.42914701,  89.45358423,  89.47800419,  89.50240334,  89.52678556,  89.55114712,  89.57549205,
        89.59981659, 89.62412467,  89.64841158,  89.67267947,  89.69693248,  89.72116452,  89.74537692,  89.76957083,  89.7937454,   89.81790335,
        89.84204321, 89.86616299,  89.89026483,  89.9143479,   89.93841405,  89.96245975,  89.98648709,  90.01049982,  90.03449049,  90.05846689,
        90.08242149, 90.10635828,  90.13027714,  90.15417848,  90.17806217,  90.20192576,  90.22577482,  90.24960461,  90.27341656,  90.2972106,
        90.32098387, 90.34474149,  90.36848137,  90.39220204,  90.41590903,  90.43959324,  90.46326397,  90.48691589,  90.51054803,  90.53416512,
        90.55776192, 90.58134304,  90.60490873,  90.62845405,  90.65198139,  90.67549356,  90.69898682,  90.72246132,  90.74592103,  90.769361,
        90.79278746, 90.81619179,  90.83958359,  90.86295533,  90.88630908,  90.90964877,  90.93297058,  90.95627204,  90.97955858,  91.00282768,
        91.02608323, 91.04931768,  91.07253502,  91.09573908,  91.11892527,  91.14209171,  91.16524142,  91.1883783,   91.21149485,  91.23459599,
        91.25767976, 91.2807473,   91.30379877,  91.32683136,  91.34984706,  91.37285084,  91.39583217,  91.41880262,  91.44175194,  91.46468678,
        91.48760533, 91.51050867,  91.53339033,  91.55625985,  91.57911433,  91.60195137,  91.62476883,  91.64757164,  91.67036098,  91.69313301,
        91.71588507, 91.73862389,  91.76134768,  91.78405458,  91.80674275,  91.82941719,  91.85207509,  91.87471831,  91.89734523,  91.91995466,
        91.94254665, 91.96512422,  91.98768844,  92.01023542,  92.03276627,  92.05527817,  92.07777696,  92.10025976,  92.12272678,  92.14517897,
        92.16761444, 92.19003427,  92.21243853,  92.23482725,  92.25720356,  92.27955771,  92.30190036,  92.3242278,   92.34654009,  92.36883447,
        92.39111284, 92.41338006,  92.43562863,  92.45786234,  92.48008133,  92.50228177,  92.52446971,  92.54664103,  92.56880174,  92.59094039,
        92.61306856, 92.63517859,  92.65727443,  92.67935617,  92.70142287,  92.72347274,  92.74550864,  92.76753071,  92.7895351,   92.81152687,
        92.83350013, 92.85545971,  92.87740485,  92.89933747,  92.92125288,  92.943154,    92.96503896,  92.98690982,  93.0087646,   93.03060924,
        93.05243319, 93.07424721,  93.09604538,  93.11782401,  93.13959274,  93.16134502,  93.18308465,  93.20480792,  93.22651868,  93.24821416,
        93.26989246, 93.29155854,  93.31320847,  93.3348463,   93.35646639,  93.37807529,  93.3996693,   93.42124759,  93.44281016,  93.46436002,
        93.48589524, 93.50741881,  93.52892603,  93.55041879,  93.57189532,  93.59336136,  93.61480953,  93.63624525,  93.65766882,  93.67907358,
        93.70046721, 93.72184688,  93.74321081,  93.7645638,   93.78590211,  93.80722194,  93.82853104,  93.84982856,  93.8711087,   93.89237653,
        93.91362794, 93.9348689,   93.95609091,  93.9773035,   93.99850109,  94.01968552,  94.04085511,  94.06201077,  94.08315348,  94.10428149,
        94.12539575, 94.14649848,  94.16758545,  94.18865888,  94.20971874,  94.23076523,  94.25179828,  94.2728171,   94.29382356,  94.31481682,
        94.33579601, 94.3567592,   94.37771222,  94.39865154,  94.41957488,  94.44048529,  94.46138296,  94.48226783,  94.50314002,  94.52399573,
        94.54484271, 94.56567332,  94.58649145,  94.60729332,  94.6280858,   94.64886302,  94.6696293,   94.69037824,  94.71111801,  94.73184187,
        94.75255372, 94.77325448,  94.79393861,  94.8146147,   94.8352743,   94.85591916,  94.87655236,  94.89717391,  94.91778139,  94.93837602,
        94.95895446, 94.97952618,  95.00008186,  95.02062236,  95.04115163,  95.06166969,  95.08217276,  95.10266477,  95.12314186,  95.14360713,
        95.16406232, 95.18450036,  95.20493,     95.22534421,  95.24574469,  95.26613366,  95.28651202,  95.30687598,  95.32722563,  95.34756393,
        95.36788887, 95.38820343,  95.40850304,  95.42879168,  95.449067,    95.4693292,   95.48958046,  95.50981786,  95.53004526,  95.55025891,
        95.57045881, 95.59064806,  95.61082454,  95.63098662,  95.65113602,  95.67127496,  95.6914036,   95.71151577,  95.73161845,  95.75170788,
        95.77178414, 95.7918502,   95.81190402,  95.83194392,  95.85197168,  95.87198945,  95.89199053,  95.91198337,  95.93196106,  95.95193138,
        95.97188434, 95.99182926,  96.01176161,  96.03168058,  96.05158784,  96.07148185,  96.09136342,  96.11123649,  96.13109341,  96.15094189,
        96.17077664, 96.19059992,  96.21041192,  96.23021102,  96.24999892,  96.26977403,  96.28953722,  96.30929235,  96.32903185,  96.34875878,
        96.36847475, 96.38818287,  96.40787474,  96.42755733,  96.44722605,  96.46688407,  96.48652908,  96.50616346,  96.5257865,   96.54539743,
        96.5649979,  96.58458635,  96.60416366,  96.62372601,  96.64328114,  96.66282372,  96.68235301,  96.70187136,  96.72137804,  96.74087386,
        96.76035587, 96.77983017,  96.79928993,  96.81874209,  96.83818056,  96.85760851,  96.87702132,  96.89642747,  96.91581871,  96.93520023,
        96.95457142, 96.97393153,  96.99327835,  97.01261497,  97.03194067,  97.05125242,  97.07055411,  97.08984577,  97.10912744,  97.12839538,
        97.14765195, 97.16689939,  97.18613698,  97.20535798,  97.22457231,  97.24377619,  97.26296359,  97.28214521,  97.30131272,  97.32047148,
        97.33962005, 97.35875536,  97.37787753,  97.39699277,  97.41609556,  97.43518605,  97.45426661,  97.47333802,  97.49239649,  97.51144208,
        97.53048245, 97.54950626,  97.56852035,  97.58752557,  97.60651883,  97.62550117,  97.64447612,  97.66343401,  97.68238777,  97.7013245,
        97.72025336, 97.73917295,  97.75808021,  97.77697826,  97.79586097,  97.81473912,  97.83360053,  97.85245612,  97.87129715,  97.89012997,
        97.90895465, 97.92776351,  97.94656357,  97.96535557,  97.98413572,  98.00290411,  98.0216607,   98.04040946,  98.0591458,   98.07787434,
        98.09659062, 98.11529602,  98.13399297,  98.15267543,  98.17135021,  98.19001678,  98.20866815,  98.22731202,  98.24594464,  98.26456913,
        98.28317933, 98.30178152,  98.32037191,  98.33895574,  98.35752401,  98.37608583,  98.39463599,  98.41317593,  98.43170428,  98.45022567,
        98.46873238, 98.48723156,  98.50572064,  98.52419839,  98.54266935,  98.56112833,  98.5795743,   98.59801359,  98.61644162,  98.63485926,
        98.65326612, 98.6716639,   98.69005,     98.70842759,  98.72679422,  98.74515239,  98.76349897,  98.78183652,  98.80016317,  98.818479,
        98.83678649, 98.85508254,  98.87337098,  98.89164802,  98.90991685,  98.92817432,  98.94642114,  98.96465599,  98.98288655,  99.0011027,
        99.0193095,  99.03750706,  99.05569661,  99.07387501,  99.09204163,  99.11020164,  99.12834992,  99.14649098,  99.16462101,  99.18274005,
        99.20084747, 99.21894777,  99.23704034,  99.25511882,  99.2731896,   99.29125019,  99.30930313,  99.32734527,  99.34537664,  99.36340108,
        99.38141098, 99.39941718,  99.41740889,  99.43539377,  99.45336677,  99.4713336,   99.48928668,  99.50723239,  99.5251714,   99.54309608,
        99.56101346, 99.57892103,  99.59682135,  99.61470745,  99.63258635,  99.65045553,  99.66831756,  99.68616804,  99.70400948,  99.72184065,
        99.7396654,  99.75747931,  99.77528304,  99.79307659,  99.81085938,  99.82863649,  99.84640288,  99.8641592,   99.88190929,  99.8996443,
        99.91737375, 99.93509321,  99.95280653,  99.97050611,  99.98819959,  100.0058826,  100.0235557,  100.041219,   100.0588725,  100.0765193,
        100.0941565, 100.1117838,  100.1294041,  100.147012,   100.1646135,  100.1822015,  100.1997837,  100.2173562,  100.2349186,  100.2524759,
        100.2700192, 100.2875567,  100.3050842,  100.3226022,  100.3401102,  100.3576094,  100.3751023,  100.3925827,  100.4100569,  100.4275217,
        100.4449773, 100.462423,   100.4798599,  100.4972871,  100.514705,   100.532117,   100.5495203,  100.5669127,  100.5842934,  100.6016687,
        100.6190344, 100.636391,   100.6537386,  100.6710809,  100.6884104,  100.7057304,  100.7230452,  100.7403472,  100.7576436,  100.7749305,
        100.7922053, 100.809475,   100.8267359,  100.8439874,  100.8612302,  100.8784641,  100.8956893,  100.9129053,  100.9301125,  100.947311,
        100.9645009, 100.9816821,  100.9988569,  101.0160203,  101.0331752,  101.0503248,  101.067462,   101.0845908,  101.101711,   101.1188265,
        101.1359293};
    for (i = 0; i < 1651; ++i) {
        tsat_psy = PsyTsatFnPb(*state, tsat_fn_pb_pressure[i]);
        error = max(abs(tsat_psy - tsat_fn_pb_tsat[i]), error);
    }

    // check error
    EXPECT_LE(error, 1E-7);
}
TEST_F(EnergyPlusFixture, Psychrometrics_CSpline_Test)
{
    // compare the results of Tsat between CSpline interpolation and original psychrometric function for PsychTsatFnPb
    InitializePsychRoutines(*state);
    Real64 tsat_psy;
    Real64 tsat_cspline;
    Real64 Press_test;
    Real64 error = 0.0;
    int i;
    for (i = 0; i <= 700; i++) { //Press =50,000 ~ 120,000 Pascal
        state->dataPsychrometrics->useInterpolationPsychTsatFnPb = false;
        Press_test = 50000 + i * 100;
        tsat_psy = PsyTsatFnPb(*state, Press_test); //Tsat from original psychrometric function for PsychTsatFnPb

        state->dataPsychrometrics->useInterpolationPsychTsatFnPb = true; //change to cspline
        tsat_cspline = PsyTsatFnPb(*state, Press_test); //Tsat from cspline interpolation
        error = max(abs(tsat_psy - tsat_cspline), error);
    }
    // check error
    EXPECT_LE(error, 1E-5);
}
