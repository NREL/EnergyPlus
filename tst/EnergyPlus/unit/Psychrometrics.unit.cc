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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"
using namespace EnergyPlus;
using namespace EnergyPlus::Psychrometrics;

TEST_F(EnergyPlusFixture, PsyTsatFnHPbTest)
{

    InitializePsychRoutines();

    // Test 1: Label70 - TEMP. IS FROM  20 C  TO   40 C
    Real64 H = 7.5223e4;
    Real64 PB = 1.0133e5;
    Real64 result = PsyTsatFnHPb_raw(H, PB);
    Real64 actual_result = F6((H + 1.78637e4), -1.82124e1, 8.31683e-4, -6.16461e-9, 3.06411e-14, -8.60964e-20, 1.03003e-25);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 2: Cache version of the function - first call
    Real64 cache_miss_result = PsyTsatFnHPb(H, PB);
    EXPECT_DOUBLE_EQ(actual_result, cache_miss_result);

    // Test 3: Label60 - TEMP. IS FROM   0 C  TO   20 C
    H = 2.7298e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F6((H + 1.78637e4), -2.01147e1, 9.04936e-4, -6.83305e-9, 2.3261e-14, 7.27237e-20, -6.31939e-25);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 4: Label50 - TEMP. IS FROM   -20 C  TO   0 C
    H = -6.7011e2 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F6((H + 1.78637e4), -1.94224e1, 8.59061e-4, -4.4875e-9, -5.76696e-14, 7.72217e-19, 3.97894e-24);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 5: Label40 - TEMP. IS FROM   -40 C  TO   -20 C
    H = -2.2137e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F6((H + 1.78637e4), -1.94224e1, 8.5892e-4, -4.50709e-9, -6.19492e-14, 8.71734e-20, 8.73051e-24);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 6: Label30 - TEMP. IS FROM   -60 C  TO   -40 C
    H = -4.23e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F6((H + 1.78637e4), -19.44, 8.53675e-4, -5.12637e-9, -9.85546e-14, -1.00102e-18, -4.2705e-24);
    EXPECT_DOUBLE_EQ(actual_result, result);
//
    // Test 7: TEMP. IS <  -60 C
    H = -5.25e4 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F6(-4.24e4, -19.44, 8.53675e-4, -5.12637e-9, -9.85546e-14, -1.00102e-18, -4.2705e-24);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 8: Label80 - TEMP. IS FROM   40 C  TO   60 C
    H = 1.8380e5 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F6((H + 1.78637e4), -1.29419, 3.88538e-4, -1.30237e-9, 2.78254e-15, -3.27225e-21, 1.60969e-27);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 9: Label90 - TEMP. IS FROM   60 C  TO   80 C
    H = 4.7578e5 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F6((H + 1.78637e4), 2.39214e1, 1.27519e-4, -1.52089e-10, 1.1043e-16, -4.33919e-23, 7.05296e-30);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 10: Label100 - TEMP. IS FROM   80 C  TO   90 C
    H = 1.5446e6 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F6((H + 1.78637e4), 4.88446e1, 3.85534e-5, -1.78805e-11, 4.87224e-18, -7.15283e-25, 4.36246e-32);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 11: Label110 - TEMP. IS FROM   90 C  TO   100 C
    H = 3.8354e6 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F7((H + 1.78637e4), 7.60565e11, 5.80534e4, -7.36433e-3, 5.11531e-10, -1.93619e-17, 3.70511e-25, -2.77313e-33);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 12: TEMP > 100 C
    H = 5.5867e7 - 1.78637e4;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = F7(4.5866e7, 7.60565e11, 5.80534e4, -7.36433e-3, 5.11531e-10, -1.93619e-17, 3.70511e-25, -2.77313e-33);
    EXPECT_DOUBLE_EQ(actual_result, result);

    // Test 13: PB != 1.0133e5
    H = 7.5223e4;
    PB = 0.9133e5;
    result = PsyTsatFnHPb_raw(H, PB);
    actual_result = 23.445553;
    EXPECT_NEAR(actual_result, result, 0.00001);

    // Test 14: Cache version of the function - hit call
    H = 7.5223e4 - 1.78637e4;
    PB = 1.0133e5;
    result = PsyTsatFnHPb_raw(H, PB);
    Real64 cache_hit_result = PsyTsatFnHPb(H, PB);
    EXPECT_DOUBLE_EQ(result, cache_hit_result);

}

TEST_F(EnergyPlusFixture, PsyTsatFnPbTest)
{

    InitializePsychRoutines();

    // Test 1: general
    Real64 PB = 101325.0;
    Real64 result = PsyTsatFnPb_raw(PB);
    Real64 actual_result = 99.974;
    EXPECT_NEAR(actual_result, result, 0.001);

    // Test 2: Cache version of the function - first call
    PB = 101325.0;
    result = PsyTsatFnPb_raw(PB);
    Real64 cache_result = PsyTsatFnPb(PB);
    EXPECT_DOUBLE_EQ(result, cache_result);

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
    result = PsyTsatFnPb_raw(PB);
    Real64 cache_hit_result = PsyTsatFnPb(PB);
    EXPECT_DOUBLE_EQ(result, cache_hit_result);

}

