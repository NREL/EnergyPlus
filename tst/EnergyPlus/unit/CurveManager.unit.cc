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
#include <CurveManager.hh>
#include <DataGlobals.hh>
#include <DataIPShortCuts.hh>

#include "EnergyPlus/CurveManager.hh"
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, Curves_Linear)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:Linear,",
                                                      "TestCurve1,              !- Name",
                                                      "2,                       !- Coefficient1 Constant",
                                                      "8,                       !- Coefficient2 x",
                                                      "0 ,                      !- Minimum Value of x",
                                                      "64,                      !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:Linear,",
                                                      "TestCurve2,              !- Name",
                                                      "2,                       !- Coefficient1 Constant",
                                                      "8,                       !- Coefficient2 x",
                                                      "0 ,                      !- Minimum Value of x",
                                                      "64,                      !- Maximum Value of x",
                                                      "12,                      !- Minimum Curve Output",
                                                      "24,                      !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(64, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(CurveManager::CurveType_Linear, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(64, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(CurveManager::CurveType_Linear, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 0));      // In-range value
    EXPECT_DOUBLE_EQ(42.0, CurveManager::CurveValue(1, 5));     // In-range value
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, -10.0));  // Minimum x
    EXPECT_DOUBLE_EQ(514.0, CurveManager::CurveValue(1, 5000)); // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 16, 32);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(16.0, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(32.0, CurveManager::CurveValue(1, 5)); // Value too large

    EXPECT_DOUBLE_EQ(12.0, CurveManager::CurveValue(2, 0)); // Value too small
    EXPECT_DOUBLE_EQ(24.0, CurveManager::CurveValue(2, 5)); // Value too large
    EXPECT_DOUBLE_EQ(18.0, CurveManager::CurveValue(2, 2)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_Quadratic)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:Quadratic,",
                                                      "TestCurve1,              !- Name",
                                                      "2,                       !- Coefficient1 Constant",
                                                      "4,                       !- Coefficient2 x",
                                                      "8,                       !- Coefficient3 x**2",
                                                      "-10,                     !- Minimum Value of x",
                                                      "10,                      !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:Quadratic,",
                                                      "TestCurve2,              !- Name",
                                                      "2,                       !- Coefficient1 Constant",
                                                      "4,                       !- Coefficient2 x",
                                                      "8,                       !- Coefficient3 x**2",
                                                      "-10,                     !- Minimum Value of x",
                                                      "10,                      !- Maximum Value of x",
                                                      "10,                      !- Minimum Curve Output",
                                                      "100,                     !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("QUADRATIC", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(10, max);
    EXPECT_EQ(-10, min);
    EXPECT_EQ(CurveManager::CurveType_Quadratic, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("QUADRATIC", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(10, max);
    EXPECT_EQ(-10, min);
    EXPECT_EQ(CurveManager::CurveType_Quadratic, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 0));        // In-range value
    EXPECT_DOUBLE_EQ(14.0, CurveManager::CurveValue(1, 1));       // In-range value
    EXPECT_DOUBLE_EQ(762.0, CurveManager::CurveValue(1, -100.0)); // Minimum x
    EXPECT_DOUBLE_EQ(842.0, CurveManager::CurveValue(1, 5000));   // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 3, 5);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(3.0, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 1)); // Value too large

    EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(2, 0));     // Value too small
    EXPECT_DOUBLE_EQ(100.0, CurveManager::CurveValue(2, -100)); // Value too large
    EXPECT_DOUBLE_EQ(14.0, CurveManager::CurveValue(2, 1));     // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_Cubic)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:Cubic,",
                                                      "TestCurve1,              !- Name",
                                                      "4,                       !- Coefficient1 Constant",
                                                      "3,                       !- Coefficient2 x",
                                                      "2,                       !- Coefficient3 x**2",
                                                      "1,                       !- Coefficient4 x**3",
                                                      "0,                       !- Minimum Value of x",
                                                      "1,                       !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:Cubic,",
                                                      "TestCurve2,              !- Name",
                                                      "4,                       !- Coefficient1 Constant",
                                                      "3,                       !- Coefficient2 x",
                                                      "2,                       !- Coefficient3 x**2",
                                                      "1,                       !- Coefficient4 x**3",
                                                      "0,                       !- Minimum Value of x",
                                                      "1,                       !- Maximum Value of x",
                                                      "5,                       !- Minimum Curve Output",
                                                      "9,                       !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("CUBIC", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(1, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(CurveManager::CurveType_Cubic, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("CUBIC", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(1, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(CurveManager::CurveType_Cubic, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, 0));    // In-range value
    EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 1));   // In-range value
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, -1.0)); // Minimum x
    EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 5));   // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 5, 9);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(9.0, CurveManager::CurveValue(1, 1)); // Value too large

    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(2, 0));     // Value too small
    EXPECT_DOUBLE_EQ(9.0, CurveManager::CurveValue(2, 1));     // Value too large
    EXPECT_DOUBLE_EQ(6.125, CurveManager::CurveValue(2, 0.5)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_Exponent)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:Exponent,",
                                                      "TestCurve1,              !- Name",
                                                      "4,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 Constant",
                                                      "2,                       !- Coefficient3 Constant",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:Exponent,",
                                                      "TestCurve2,              !- Name",
                                                      "4,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 Constant",
                                                      "2,                       !- Coefficient3 Constant",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "5,                       !- Minimum Curve Output",
                                                      "10,                      !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("EXPONENT", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(2, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(CurveManager::CurveType_Exponent, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("EXPONENT", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(2, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(CurveManager::CurveType_Exponent, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, 0));    // In-range value
    EXPECT_DOUBLE_EQ(12.0, CurveManager::CurveValue(1, 2));   // In-range value
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, -1.0)); // Minimum x
    EXPECT_DOUBLE_EQ(12.0, CurveManager::CurveValue(1, 500)); // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 5, 10);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 0));  // Value too small
    EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 2)); // Value too large

    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(2, 0));  // Value too small
    EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(2, 2)); // Value too large
    EXPECT_DOUBLE_EQ(6.0, CurveManager::CurveValue(2, 1));  // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_ExponentialDecay)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:ExponentialDecay,",
                                                      "TestCurve1,              !- Name",
                                                      "4,                       !- Coefficient1 C1",
                                                      "2,                       !- Coefficient2 C2",
                                                      "2,                       !- Coefficient3 C3",
                                                      "-1,                      !- Minimum Value of x",
                                                      "1,                       !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:ExponentialDecay,",
                                                      "TestCurve2,              !- Name",
                                                      "4,                       !- Coefficient1 C1",
                                                      "2,                       !- Coefficient2 C2",
                                                      "2,                       !- Coefficient3 C3",
                                                      "-1,                      !- Minimum Value of x",
                                                      "1,                       !- Maximum Value of x",
                                                      "5,                       !- Minimum Curve Output",
                                                      "10,                      !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("EXPONENTIALDECAY", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(-1, min);
    EXPECT_EQ(1, max);
    EXPECT_EQ(CurveManager::CurveType_ExponentialDecay, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("EXPONENTIALDECAY", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(-1, min);
    EXPECT_EQ(1, max);
    EXPECT_EQ(CurveManager::CurveType_ExponentialDecay, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(4.270670566473226, CurveManager::CurveValue(1, -1));   // In-range value
    EXPECT_DOUBLE_EQ(18.7781121978613, CurveManager::CurveValue(1, 1));     // In-range value
    EXPECT_DOUBLE_EQ(4.270670566473226, CurveManager::CurveValue(1, -2.0)); // Minimum x
    EXPECT_DOUBLE_EQ(18.7781121978613, CurveManager::CurveValue(1, 500));   // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 5, 10);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, -1)); // Value too small
    EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 1)); // Value too large

    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(2, -1)); // Value too small
    EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(2, 1)); // Value too large
    EXPECT_DOUBLE_EQ(6.0, CurveManager::CurveValue(2, 0));  // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_DoubleExponentialDecay)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:DoubleExponentialDecay,",
                                                      "TestCurve1,              !- Name",
                                                      "4,                       !- Coefficient1 C1",
                                                      "2,                       !- Coefficient2 C2",
                                                      "1,                       !- Coefficient3 C3",
                                                      "2,                       !- Coefficient3 C4",
                                                      "2,                       !- Coefficient3 C5",
                                                      "-1,                      !- Minimum Value of x",
                                                      "1,                       !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:DoubleExponentialDecay,",
                                                      "TestCurve2,              !- Name",
                                                      "4,                       !- Coefficient1 C1",
                                                      "2,                       !- Coefficient2 C2",
                                                      "1,                       !- Coefficient3 C3",
                                                      "2,                       !- Coefficient3 C4",
                                                      "2,                       !- Coefficient3 C5",
                                                      "-1,                      !- Minimum Value of x",
                                                      "1,                       !- Maximum Value of x",
                                                      "6,                       !- Minimum Curve Output",
                                                      "10,                      !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("DOUBLEEXPONENTIALDECAY", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(-1, min);
    EXPECT_EQ(1, max);
    EXPECT_EQ(CurveManager::CurveType_DoubleExponentialDecay, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("DOUBLEEXPONENTIALDECAY", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(-1, min);
    EXPECT_EQ(1, max);
    EXPECT_EQ(CurveManager::CurveType_DoubleExponentialDecay, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(5.00642944881611, CurveManager::CurveValue(1, -1));   // In-range value
    EXPECT_DOUBLE_EQ(24.21467585477939, CurveManager::CurveValue(1, 1));   // In-range value
    EXPECT_DOUBLE_EQ(5.00642944881611, CurveManager::CurveValue(1, -2.0)); // Minimum x
    EXPECT_DOUBLE_EQ(24.21467585477939, CurveManager::CurveValue(1, 500)); // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 6, 10);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(6.0, CurveManager::CurveValue(1, -1)); // Value too small
    EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 1)); // Value too large

    EXPECT_DOUBLE_EQ(6.0, CurveManager::CurveValue(2, -1)); // Value too small
    EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(2, 1)); // Value too large
    EXPECT_DOUBLE_EQ(8.0, CurveManager::CurveValue(2, 0));  // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_Sigmoid)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:Sigmoid,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient1 C1",
                                                      "1,                       !- Coefficient2 C2",
                                                      "1,                       !- Coefficient3 C3",
                                                      "2,                       !- Coefficient3 C4",
                                                      "2,                       !- Coefficient3 C5",
                                                      "-10,                     !- Minimum Value of x",
                                                      "10,                      !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:Sigmoid,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient1 C1",
                                                      "1,                       !- Coefficient2 C2",
                                                      "1,                       !- Coefficient3 C3",
                                                      "2,                       !- Coefficient3 C4",
                                                      "2,                       !- Coefficient3 C5",
                                                      "-10,                     !- Minimum Value of x",
                                                      "10,                      !- Maximum Value of x",
                                                      "1.2,                     !- Minimum Curve Output",
                                                      "1.5,                     !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("SIGMOID", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(-10, min);
    EXPECT_EQ(10, max);
    EXPECT_EQ(CurveManager::CurveType_Sigmoid, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("SIGMOID", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(-10, min);
    EXPECT_EQ(10, max);
    EXPECT_EQ(CurveManager::CurveType_Sigmoid, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(1.002249213446655, CurveManager::CurveValue(1, -5));    // In-range value
    EXPECT_DOUBLE_EQ(1.775803492574376, CurveManager::CurveValue(1, 5));     // In-range value
    EXPECT_DOUBLE_EQ(1.000016566021026, CurveManager::CurveValue(1, -20.0)); // Minimum x
    EXPECT_DOUBLE_EQ(1.978146827647182, CurveManager::CurveValue(1, 500));   // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 1.2, 1.5);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(1.2, CurveManager::CurveValue(1, -5)); // Value too small
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(1, 5));  // Value too large

    EXPECT_DOUBLE_EQ(1.2, CurveManager::CurveValue(2, -5));             // Value too small
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(2, 5));              // Value too large
    EXPECT_DOUBLE_EQ(1.38745561900026, CurveManager::CurveValue(2, 2)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_RectangularHyperbola1)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:RectangularHyperbola1,",
                                                      "TestCurve1,              !- Name",
                                                      "3,                       !- Coefficient1 C1",
                                                      "4,                       !- Coefficient2 C2",
                                                      "5,                       !- Coefficient3 C3",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:RectangularHyperbola1,",
                                                      "TestCurve2,              !- Name",
                                                      "3,                       !- Coefficient1 C1",
                                                      "4,                       !- Coefficient2 C2",
                                                      "5,                       !- Coefficient3 C3",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "5.2,                     !- Minimum Curve Output",
                                                      "5.7,                     !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("RECTANGULARHYPERBOLA1", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(2, max);
    EXPECT_EQ(CurveManager::CurveType_RectangularHyperbola1, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("RECTANGULARHYPERBOLA1", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(2, max);
    EXPECT_EQ(CurveManager::CurveType_RectangularHyperbola1, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(5, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(6, CurveManager::CurveValue(1, 2));     // In-range value
    EXPECT_DOUBLE_EQ(5, CurveManager::CurveValue(1, -20.0)); // Minimum x
    EXPECT_DOUBLE_EQ(6, CurveManager::CurveValue(1, 500));   // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 5.2, 5.7);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(5.2, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(5.7, CurveManager::CurveValue(1, 2)); // Value too large

    EXPECT_DOUBLE_EQ(5.2, CurveManager::CurveValue(2, 0)); // Value too small
    EXPECT_DOUBLE_EQ(5.7, CurveManager::CurveValue(2, 2)); // Value too large
    EXPECT_DOUBLE_EQ(5.6, CurveManager::CurveValue(2, 1)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_RectangularHyperbola2)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:RectangularHyperbola2,",
                                                      "TestCurve1,              !- Name",
                                                      "3,                       !- Coefficient1 C1",
                                                      "4,                       !- Coefficient2 C2",
                                                      "5,                       !- Coefficient3 C3",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:RectangularHyperbola2,",
                                                      "TestCurve2,              !- Name",
                                                      "3,                       !- Coefficient1 C1",
                                                      "4,                       !- Coefficient2 C2",
                                                      "5,                       !- Coefficient3 C3",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "5.2,                     !- Minimum Curve Output",
                                                      "5.7,                     !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("RECTANGULARHYPERBOLA2", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(2, max);
    EXPECT_EQ(CurveManager::CurveType_RectangularHyperbola2, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("RECTANGULARHYPERBOLA2", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(2, max);
    EXPECT_EQ(CurveManager::CurveType_RectangularHyperbola2, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(0, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(11, CurveManager::CurveValue(1, 2));    // In-range value
    EXPECT_DOUBLE_EQ(0, CurveManager::CurveValue(1, -20.0)); // Minimum x
    EXPECT_DOUBLE_EQ(11, CurveManager::CurveValue(1, 500));  // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 5.2, 5.7);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(5.2, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(5.7, CurveManager::CurveValue(1, 2)); // Value too large

    EXPECT_DOUBLE_EQ(5.2, CurveManager::CurveValue(2, 0)); // Value too small
    EXPECT_DOUBLE_EQ(5.7, CurveManager::CurveValue(2, 2)); // Value too large
    EXPECT_DOUBLE_EQ(5.6, CurveManager::CurveValue(2, 1)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_Quartic)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:Quartic,",
                                                      "TestCurve1,              !- Name",
                                                      "5,                       !- Coefficient1 Constant",
                                                      "4,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "2,                       !- Coefficient4 x**3",
                                                      "1,                       !- Coefficient5 x**4",
                                                      "-10,                     !- Minimum Value of x",
                                                      "10,                      !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:Quartic,",
                                                      "TestCurve2,              !- Name",
                                                      "5,                       !- Coefficient1 Constant",
                                                      "4,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "2,                       !- Coefficient4 x**3",
                                                      "1,                       !- Coefficient5 x**4",
                                                      "-10,                     !- Minimum Value of x",
                                                      "10,                      !- Maximum Value of x",
                                                      "8500,                    !- Minimum Curve Output",
                                                      "12000,                   !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(-10, min);
    EXPECT_EQ(10, max);
    EXPECT_EQ(CurveManager::CurveType_Quartic, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(-10, min);
    EXPECT_EQ(10, max);
    EXPECT_EQ(CurveManager::CurveType_Quartic, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(12345, CurveManager::CurveValue(1, 10));   // In-range value
    EXPECT_DOUBLE_EQ(8265, CurveManager::CurveValue(1, -10));   // In-range value
    EXPECT_DOUBLE_EQ(8265, CurveManager::CurveValue(1, -20.0)); // Minimum x
    EXPECT_DOUBLE_EQ(12345, CurveManager::CurveValue(1, 500));  // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 8500, 12000);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(8500, CurveManager::CurveValue(1, -10)); // Value too small
    EXPECT_DOUBLE_EQ(12000, CurveManager::CurveValue(1, 10)); // Value too large

    EXPECT_DOUBLE_EQ(8500, CurveManager::CurveValue(2, -10));       // Value too small
    EXPECT_DOUBLE_EQ(12000, CurveManager::CurveValue(2, 10));       // Value too large
    EXPECT_DOUBLE_EQ(11885.1881, CurveManager::CurveValue(2, 9.9)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_ExponentialSkewNormal)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:ExponentialSkewNormal,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient C1",
                                                      "2,                       !- Coefficient C2",
                                                      "3,                       !- Coefficient C3",
                                                      "4,                       !- Coefficient C4",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:ExponentialSkewNormal,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient C1",
                                                      "2,                       !- Coefficient C2",
                                                      "3,                       !- Coefficient C3",
                                                      "4,                       !- Coefficient C4",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "2,                       !- Minimum Curve Output",
                                                      "3.5,                     !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("EXPONENTIALSKEWNORMAL", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(2, max);
    EXPECT_EQ(CurveManager::CurveType_ExponentialSkewNormal, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("EXPONENTIALSKEWNORMAL", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(2, max);
    EXPECT_EQ(CurveManager::CurveType_ExponentialSkewNormal, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0));                // In-range value
    EXPECT_DOUBLE_EQ(3.67264371702005, CurveManager::CurveValue(1, 1));   // In-range value
    EXPECT_DOUBLE_EQ(3.24109670456697, CurveManager::CurveValue(1, 2));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, -20.0));            // Minimum x
    EXPECT_DOUBLE_EQ(3.24109670456697, CurveManager::CurveValue(1, 500)); // Maximum x
    CurveManager::SetCurveOutputMinMaxValues(1, error, 2, 3.5);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(2, CurveManager::CurveValue(1, 0));   // Value too small
    EXPECT_DOUBLE_EQ(3.5, CurveManager::CurveValue(1, 1)); // Value too large

    EXPECT_DOUBLE_EQ(2, CurveManager::CurveValue(2, 0));                // Value too small
    EXPECT_DOUBLE_EQ(3.5, CurveManager::CurveValue(2, 1));              // Value too large
    EXPECT_DOUBLE_EQ(3.24109670456697, CurveManager::CurveValue(2, 2)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_BiQuadratic)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:Biquadratic,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "4,                       !- Coefficient4 y",
                                                      "5,                       !- Coefficient5 y**2",
                                                      "6,                       !- Coefficient6 x*y",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "-2,                      !- Minimum Value of y",
                                                      "0,                       !- Maximum Value of y",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:Biquadratic,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "4,                       !- Coefficient4 y",
                                                      "5,                       !- Coefficient5 y**2",
                                                      "6,                       !- Coefficient6 x*y",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "-2,                      !- Minimum Value of y",
                                                      "0,                       !- Maximum Value of y",
                                                      "1.5,                     !- Minimum Curve Output",
                                                      "4.5,                     !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("BIQUADRATIC", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(2, max1);
    EXPECT_EQ(-2, min2);
    EXPECT_EQ(0, max2);
    EXPECT_EQ(CurveManager::CurveType_BiQuadratic, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("BIQUADRATIC", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(2, max1);
    EXPECT_EQ(-2, min2);
    EXPECT_EQ(0, max2);
    EXPECT_EQ(CurveManager::CurveType_BiQuadratic, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0, 0));  // In-range value
    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 2, -2)); // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, -1, 0)); // Minimum x
    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 3, -2)); // Maximum x
    EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 2, -3)); // Minimum y
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0, 1));  // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(1, error, 1.5, 4.5);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(1, 0, 0));  // Value too small
    EXPECT_DOUBLE_EQ(4.5, CurveManager::CurveValue(1, 2, -2)); // Value too large

    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(2, 0, 0));    // Value too small
    EXPECT_DOUBLE_EQ(4.5, CurveManager::CurveValue(2, 2, -2));   // Value too large
    EXPECT_DOUBLE_EQ(2.75, CurveManager::CurveValue(2, 0.5, 0)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_BiCubic)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:Bicubic,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "4,                       !- Coefficient4 y",
                                                      "5,                       !- Coefficient5 y**2",
                                                      "6,                       !- Coefficient6 x*y",
                                                      "7,                       !- Coefficient7 x**3",
                                                      "8,                       !- Coefficient8 y**3",
                                                      "9,                       !- Coefficient9 x**2 * y",
                                                      "10,                      !- Coefficient10 x*y**2",
                                                      "0,                       !- Minimum Value of x",
                                                      "4,                       !- Maximum Value of x",
                                                      "4,                       !- Minimum Value of y",
                                                      "6,                       !- Maximum Value of y",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:Bicubic,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "4,                       !- Coefficient4 y",
                                                      "5,                       !- Coefficient5 y**2",
                                                      "6,                       !- Coefficient6 x*y",
                                                      "7,                       !- Coefficient7 x**3",
                                                      "8,                       !- Coefficient8 y**3",
                                                      "9,                       !- Coefficient9 x**2 * y",
                                                      "10,                      !- Coefficient10 x*y**2",
                                                      "0,                       !- Minimum Value of x",
                                                      "4,                       !- Maximum Value of x",
                                                      "4,                       !- Minimum Value of y",
                                                      "6,                       !- Maximum Value of y",
                                                      "610,                     !- Minimum Curve Output",
                                                      "4880,                    !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("BICUBIC", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(4, max1);
    EXPECT_EQ(4, min2);
    EXPECT_EQ(6, max2);
    EXPECT_EQ(CurveManager::CurveType_BiCubic, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("BICUBIC", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(4, max1);
    EXPECT_EQ(4, min2);
    EXPECT_EQ(6, max2);
    EXPECT_EQ(CurveManager::CurveType_BiCubic, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(609.0, CurveManager::CurveValue(1, 0, 4));   // In-range value
    EXPECT_DOUBLE_EQ(4885.0, CurveManager::CurveValue(1, 4, 6));  // In-range value
    EXPECT_DOUBLE_EQ(609.0, CurveManager::CurveValue(1, -1, 4));  // Minimum x
    EXPECT_DOUBLE_EQ(4885.0, CurveManager::CurveValue(1, 5, 6));  // Maximum x
    EXPECT_DOUBLE_EQ(609.0, CurveManager::CurveValue(1, 0, 3));   // Minimum y
    EXPECT_DOUBLE_EQ(4885.0, CurveManager::CurveValue(1, 4, 10)); // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(1, error, 610, 4880);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(610.0, CurveManager::CurveValue(1, 0, 4));  // Value too small
    EXPECT_DOUBLE_EQ(4880.0, CurveManager::CurveValue(1, 4, 6)); // Value too large

    EXPECT_DOUBLE_EQ(610.0, CurveManager::CurveValue(2, 0, 4));  // Value too small
    EXPECT_DOUBLE_EQ(4880.0, CurveManager::CurveValue(2, 4, 6)); // Value too large
    EXPECT_DOUBLE_EQ(3829.0, CurveManager::CurveValue(2, 3, 6)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_QuadraticLinear)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:QuadraticLinear,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "4,                       !- Coefficient4 y",
                                                      "5,                       !- Coefficient5 x*y",
                                                      "6,                       !- Coefficient6 x**2 * y",
                                                      "0,                       !- Minimum Value of x",
                                                      "4,                       !- Maximum Value of x",
                                                      "4,                       !- Minimum Value of y",
                                                      "6,                       !- Maximum Value of y",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:QuadraticLinear,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "4,                       !- Coefficient4 y",
                                                      "5,                       !- Coefficient5 x*y",
                                                      "6,                       !- Coefficient6 x**2 * y",
                                                      "0,                       !- Minimum Value of x",
                                                      "4,                       !- Maximum Value of x",
                                                      "4,                       !- Minimum Value of y",
                                                      "6,                       !- Maximum Value of y",
                                                      "20,                     !- Minimum Curve Output",
                                                      "700,                    !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(4, max1);
    EXPECT_EQ(4, min2);
    EXPECT_EQ(6, max2);
    EXPECT_EQ(CurveManager::CurveType_QuadraticLinear, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(4, max1);
    EXPECT_EQ(4, min2);
    EXPECT_EQ(6, max2);
    EXPECT_EQ(CurveManager::CurveType_QuadraticLinear, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(17.0, CurveManager::CurveValue(1, 0, 4));   // In-range value
    EXPECT_DOUBLE_EQ(777.0, CurveManager::CurveValue(1, 4, 6));  // In-range value
    EXPECT_DOUBLE_EQ(17.0, CurveManager::CurveValue(1, -1, 4));  // Minimum x
    EXPECT_DOUBLE_EQ(777.0, CurveManager::CurveValue(1, 5, 6));  // Maximum x
    EXPECT_DOUBLE_EQ(17.0, CurveManager::CurveValue(1, 0, 3));   // Minimum y
    EXPECT_DOUBLE_EQ(777.0, CurveManager::CurveValue(1, 4, 10)); // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(1, error, 20, 700);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, 0, 4));  // Value too small
    EXPECT_DOUBLE_EQ(700.0, CurveManager::CurveValue(1, 4, 6)); // Value too large

    EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(2, 0, 4));  // Value too small
    EXPECT_DOUBLE_EQ(700.0, CurveManager::CurveValue(2, 4, 6)); // Value too large
    EXPECT_DOUBLE_EQ(472.0, CurveManager::CurveValue(2, 3, 6)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_CubicLinear)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:CubicLinear,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "4,                       !- Coefficient4 x**3",
                                                      "5,                       !- Coefficient5 y",
                                                      "6,                       !- Coefficient6 x*y",
                                                      "0,                       !- Minimum Value of x",
                                                      "4,                       !- Maximum Value of x",
                                                      "4,                       !- Minimum Value of y",
                                                      "6,                       !- Maximum Value of y",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:CubicLinear,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x",
                                                      "3,                       !- Coefficient3 x**2",
                                                      "4,                       !- Coefficient4 x**3",
                                                      "5,                       !- Coefficient5 y",
                                                      "6,                       !- Coefficient6 x*y",
                                                      "0,                       !- Minimum Value of x",
                                                      "4,                       !- Maximum Value of x",
                                                      "4,                       !- Minimum Value of y",
                                                      "6,                       !- Maximum Value of y",
                                                      "30,                     !- Minimum Curve Output",
                                                      "480,                    !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("CUBICLINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(4, max1);
    EXPECT_EQ(4, min2);
    EXPECT_EQ(6, max2);
    EXPECT_EQ(CurveManager::CurveType_CubicLinear, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("CUBICLINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(4, max1);
    EXPECT_EQ(4, min2);
    EXPECT_EQ(6, max2);
    EXPECT_EQ(CurveManager::CurveType_CubicLinear, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(21.0, CurveManager::CurveValue(1, 0, 4));   // In-range value
    EXPECT_DOUBLE_EQ(487.0, CurveManager::CurveValue(1, 4, 6));  // In-range value
    EXPECT_DOUBLE_EQ(21.0, CurveManager::CurveValue(1, -1, 4));  // Minimum x
    EXPECT_DOUBLE_EQ(487.0, CurveManager::CurveValue(1, 5, 6));  // Maximum x
    EXPECT_DOUBLE_EQ(21.0, CurveManager::CurveValue(1, 0, 3));   // Minimum y
    EXPECT_DOUBLE_EQ(487.0, CurveManager::CurveValue(1, 4, 10)); // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(1, error, 30, 480);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(30.0, CurveManager::CurveValue(1, 0, 4));  // Value too small
    EXPECT_DOUBLE_EQ(480.0, CurveManager::CurveValue(1, 4, 6)); // Value too large

    EXPECT_DOUBLE_EQ(30.0, CurveManager::CurveValue(2, 0, 4));  // Value too small
    EXPECT_DOUBLE_EQ(480.0, CurveManager::CurveValue(2, 4, 6)); // Value too large
    EXPECT_DOUBLE_EQ(280.0, CurveManager::CurveValue(2, 3, 6)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_FanPressureRise)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:FanPressureRise,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient1 C1",
                                                      "2,                       !- Coefficient2 C2",
                                                      "3,                       !- Coefficient3 C3",
                                                      "4,                       !- Coefficient4 C4",
                                                      "0,                       !- Minimum Value of Qfan",
                                                      "400,                     !- Maximum Value of Qfan",
                                                      "4000,                    !- Minimum Value of Psm",
                                                      "6000,                    !- Maximum Value of Psm",
                                                      ",                        !- Minimum Curve Output",
                                                      ";                        !- Maximum Curve Output",
                                                      "Curve:FanPressureRise,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient1 C1",
                                                      "2,                       !- Coefficient2 C2",
                                                      "3,                       !- Coefficient3 C3",
                                                      "4,                       !- Coefficient4 C4",
                                                      "0,                       !- Minimum Value of Qfan",
                                                      "400,                     !- Maximum Value of Qfan",
                                                      "4000,                    !- Minimum Value of Psm",
                                                      "6000,                    !- Maximum Value of Psm",
                                                      "20000,                        !- Minimum Curve Output",
                                                      "277000;                        !- Maximum Curve Output"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("FANPRESSURERISE", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(400, max1);
    EXPECT_EQ(4000, min2);
    EXPECT_EQ(6000, max2);
    EXPECT_EQ(CurveManager::CurveType_FanPressureRise, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("FANPRESSURERISE", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(400, max1);
    EXPECT_EQ(4000, min2);
    EXPECT_EQ(6000, max2);
    EXPECT_EQ(CurveManager::CurveType_FanPressureRise, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(16000.0, CurveManager::CurveValue(1, 0, 4000));            // In-range value
    EXPECT_DOUBLE_EQ(277751.600308978, CurveManager::CurveValue(1, 400, 6000)); // In-range value
    EXPECT_DOUBLE_EQ(16000.0, CurveManager::CurveValue(1, -1, 4000));           // Minimum x
    EXPECT_DOUBLE_EQ(277751.600308978, CurveManager::CurveValue(1, 500, 6000)); // Maximum x
    EXPECT_DOUBLE_EQ(16000.0, CurveManager::CurveValue(1, 0, 3000));            // Minimum y
    EXPECT_DOUBLE_EQ(277751.600308978, CurveManager::CurveValue(1, 400, 7000)); // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(1, error, 20000, 277000);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(20000.0, CurveManager::CurveValue(1, 0, 4000));  // Value too small
    EXPECT_DOUBLE_EQ(277000, CurveManager::CurveValue(1, 400, 6000)); // Value too large

    EXPECT_DOUBLE_EQ(20000, CurveManager::CurveValue(2, 0, 4000));              // Value too small
    EXPECT_DOUBLE_EQ(277000, CurveManager::CurveValue(2, 400, 6000));           // Value too large
    EXPECT_DOUBLE_EQ(28086.83298050514, CurveManager::CurveValue(2, 50, 4000)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_TriQuadratic)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:Triquadratic,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x**2",
                                                      "3,                       !- Coefficient3 x",
                                                      "4,                       !- Coefficient4 y**2",
                                                      "5,                       !- Coefficient5 y",
                                                      "6,                       !- Coefficient6 z**2",
                                                      "7,                       !- Coefficient7 z",
                                                      "8,                       !- Coefficient8 x**2 * y**2",
                                                      "9,                       !- Coefficient9 x*y",
                                                      "10,                      !- Coefficient10 x*y**2",
                                                      "11,                      !- Coefficient11 x**2 * y",
                                                      "12,                      !- Coefficient12 x**2 * z**2",
                                                      "13,                      !- Coefficient13 x*z",
                                                      "14,                      !- Coefficient14 x*z**2",
                                                      "15,                      !- Coefficient15 x**2 * z",
                                                      "16,                      !- Coefficient16 y**2 * z**2",
                                                      "17,                      !- Coefficient17 y*z",
                                                      "18,                      !- Coefficient18 y*z**2",
                                                      "19,                      !- Coefficient19 y**2 * z",
                                                      "20,                      !- Coefficient20 x**2 * y**2 * z**2",
                                                      "21,                      !- Coefficient21 x**2 * y**2 * z",
                                                      "22,                      !- Coefficient22 x**2 * y*z**2",
                                                      "23,                      !- Coefficient23 x*y**2 * z**2",
                                                      "24,                      !- Coefficient24 x**2 * y*z",
                                                      "25,                      !- Coefficient25 x*y**2 * z",
                                                      "26,                      !- Coefficient26 x*y*z**2",
                                                      "27,                      !- Coefficient27 x*y*z",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "2,                       !- Minimum Value of y",
                                                      "4,                       !- Maximum Value of y",
                                                      "3,                       !- Minimum Value of z",
                                                      "5,                       !- Maximum Value of z",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Input Unit Type for Z",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:Triquadratic,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 x**2",
                                                      "3,                       !- Coefficient3 x",
                                                      "4,                       !- Coefficient4 y**2",
                                                      "5,                       !- Coefficient5 y",
                                                      "6,                       !- Coefficient6 z**2",
                                                      "7,                       !- Coefficient7 z",
                                                      "8,                       !- Coefficient8 x**2 * y**2",
                                                      "9,                       !- Coefficient9 x*y",
                                                      "10,                      !- Coefficient10 x*y**2",
                                                      "11,                      !- Coefficient11 x**2 * y",
                                                      "12,                      !- Coefficient12 x**2 * z**2",
                                                      "13,                      !- Coefficient13 x*z",
                                                      "14,                      !- Coefficient14 x*z**2",
                                                      "15,                      !- Coefficient15 x**2 * z",
                                                      "16,                      !- Coefficient16 y**2 * z**2",
                                                      "17,                      !- Coefficient17 y*z",
                                                      "18,                      !- Coefficient18 y*z**2",
                                                      "19,                      !- Coefficient19 y**2 * z",
                                                      "20,                      !- Coefficient20 x**2 * y**2 * z**2",
                                                      "21,                      !- Coefficient21 x**2 * y**2 * z",
                                                      "22,                      !- Coefficient22 x**2 * y*z**2",
                                                      "23,                      !- Coefficient23 x*y**2 * z**2",
                                                      "24,                      !- Coefficient24 x**2 * y*z",
                                                      "25,                      !- Coefficient25 x*y**2 * z",
                                                      "26,                      !- Coefficient26 x*y*z**2",
                                                      "27,                      !- Coefficient27 x*y*z",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "2,                       !- Minimum Value of y",
                                                      "4,                       !- Maximum Value of y",
                                                      "3,                       !- Minimum Value of z",
                                                      "5,                       !- Maximum Value of z",
                                                      "3000,                    !- Minimum Curve Output",
                                                      "90000,                   !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Input Unit Type for Z",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("TRIQUADRATIC", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2, min3, max3;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2, min3, max3);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(2, max1);
    EXPECT_EQ(2, min2);
    EXPECT_EQ(4, max2);
    EXPECT_EQ(3, min3);
    EXPECT_EQ(5, max3);
    EXPECT_EQ(CurveManager::CurveType_TriQuadratic, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("TRIQUADRATIC", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2, min3, max3);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(2, max1);
    EXPECT_EQ(2, min2);
    EXPECT_EQ(4, max2);
    EXPECT_EQ(3, min3);
    EXPECT_EQ(5, max3);
    EXPECT_EQ(CurveManager::CurveType_TriQuadratic, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(4228.0, CurveManager::CurveValue(1, 0, 4, 3));  // In-range value
    EXPECT_DOUBLE_EQ(1332.0, CurveManager::CurveValue(1, 0, 2, 3));  // In-range value
    EXPECT_DOUBLE_EQ(91874.0, CurveManager::CurveValue(1, 2, 4, 5)); // In-range value
    EXPECT_DOUBLE_EQ(4228.0, CurveManager::CurveValue(1, -1, 4, 3)); // Minimum x
    EXPECT_DOUBLE_EQ(91874.0, CurveManager::CurveValue(1, 3, 4, 5)); // Maximum x
    EXPECT_DOUBLE_EQ(1332.0, CurveManager::CurveValue(1, 0, 1, 3));  // Minimum y
    EXPECT_DOUBLE_EQ(91874.0, CurveManager::CurveValue(1, 2, 5, 5)); // Maximum y
    EXPECT_DOUBLE_EQ(1332.0, CurveManager::CurveValue(1, 0, 2, 2));  // Minimum z
    EXPECT_DOUBLE_EQ(91874.0, CurveManager::CurveValue(1, 2, 4, 6)); // Maximum z

    CurveManager::SetCurveOutputMinMaxValues(1, error, 3000, 90000);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(3000.0, CurveManager::CurveValue(1, 0, 2, 3));  // Value too small
    EXPECT_DOUBLE_EQ(90000.0, CurveManager::CurveValue(1, 2, 4, 5)); // Value too large

    EXPECT_DOUBLE_EQ(3000.0, CurveManager::CurveValue(2, 0, 2, 3));  // Value too small
    EXPECT_DOUBLE_EQ(90000.0, CurveManager::CurveValue(2, 2, 4, 6)); // Value too large
    EXPECT_DOUBLE_EQ(4228.0, CurveManager::CurveValue(2, 0, 4, 3));  // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_ChillerPartLoadWithLift)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:ChillerPartLoadWithLift,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient1 C1",
                                                      "2,                       !- Coefficient2 C2",
                                                      "3,                       !- Coefficient3 C3",
                                                      "4,                       !- Coefficient4 C4",
                                                      "5,                       !- Coefficient5 C5",
                                                      "6,                       !- Coefficient6 C6",
                                                      "7,                       !- Coefficient7 C7",
                                                      "8,                       !- Coefficient8 C8",
                                                      "9,                       !- Coefficient9 C9",
                                                      "10,                      !- Coefficient10 C10",
                                                      "11,                      !- Coefficient11 C11",
                                                      "12,                      !- Coefficient12 C12",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "2,                       !- Minimum Value of y",
                                                      "4,                       !- Maximum Value of y",
                                                      "3,                       !- Minimum Value of z",
                                                      "5,                       !- Maximum Value of z",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Input Unit Type for Z",
                                                      "Dimensionless;           !- Output Unit Type",
                                                      "Curve:ChillerPartLoadWithLift,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient1 C1",
                                                      "2,                       !- Coefficient2 C2",
                                                      "3,                       !- Coefficient3 C3",
                                                      "4,                       !- Coefficient4 C4",
                                                      "5,                       !- Coefficient5 C5",
                                                      "6,                       !- Coefficient6 C6",
                                                      "7,                       !- Coefficient7 C7",
                                                      "8,                       !- Coefficient8 C8",
                                                      "9,                       !- Coefficient9 C9",
                                                      "10,                      !- Coefficient10 C10",
                                                      "11,                      !- Coefficient11 C11",
                                                      "12,                      !- Coefficient12 C12",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "2,                       !- Minimum Value of y",
                                                      "4,                       !- Maximum Value of y",
                                                      "3,                       !- Minimum Value of z",
                                                      "5,                       !- Maximum Value of z",
                                                      "400,                    !- Minimum Curve Output",
                                                      "4000,                   !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Input Unit Type for Z",
                                                      "Dimensionless;           !- Output Unit Type"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("CHILLERPARTLOADWITHLIFT", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2, min3, max3;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2, min3, max3);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(2, max1);
    EXPECT_EQ(2, min2);
    EXPECT_EQ(4, max2);
    EXPECT_EQ(3, min3);
    EXPECT_EQ(5, max3);
    EXPECT_EQ(CurveManager::CurveType_ChillerPartLoadWithLift, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("CHILLERPARTLOADWITHLIFT", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2, min3, max3);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(2, max1);
    EXPECT_EQ(2, min2);
    EXPECT_EQ(4, max2);
    EXPECT_EQ(3, min3);
    EXPECT_EQ(5, max3);
    EXPECT_EQ(CurveManager::CurveType_ChillerPartLoadWithLift, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(2913.0, CurveManager::CurveValue(1, 0, 4, 3));  // In-range value
    EXPECT_DOUBLE_EQ(381.0, CurveManager::CurveValue(1, 0, 2, 3));   // In-range value
    EXPECT_DOUBLE_EQ(5737.0, CurveManager::CurveValue(1, 2, 4, 5));  // In-range value
    EXPECT_DOUBLE_EQ(2913.0, CurveManager::CurveValue(1, -1, 4, 3)); // Minimum x
    EXPECT_DOUBLE_EQ(5737.0, CurveManager::CurveValue(1, 3, 4, 5));  // Maximum x
    EXPECT_DOUBLE_EQ(381.0, CurveManager::CurveValue(1, 0, 1, 3));   // Minimum y
    EXPECT_DOUBLE_EQ(5737.0, CurveManager::CurveValue(1, 2, 5, 5));  // Maximum y
    EXPECT_DOUBLE_EQ(381.0, CurveManager::CurveValue(1, 0, 2, 2));   // Minimum z
    EXPECT_DOUBLE_EQ(5737.0, CurveManager::CurveValue(1, 2, 4, 6));  // Maximum z

    CurveManager::SetCurveOutputMinMaxValues(1, error, 400, 4000);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(400.0, CurveManager::CurveValue(1, 0, 2, 3));  // Value too small
    EXPECT_DOUBLE_EQ(4000.0, CurveManager::CurveValue(1, 2, 4, 5)); // Value too large

    EXPECT_DOUBLE_EQ(400.0, CurveManager::CurveValue(2, 0, 2, 3));  // Value too small
    EXPECT_DOUBLE_EQ(4000.0, CurveManager::CurveValue(2, 2, 4, 5)); // Value too large
    EXPECT_DOUBLE_EQ(2913.0, CurveManager::CurveValue(2, 0, 4, 3)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_QuadLinear)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Curve:QuadLinear,",
                                                      "TestCurve1,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 w",
                                                      "3,                       !- Coefficient3 x",
                                                      "4,                       !- Coefficient4 y",
                                                      "5,                       !- Coefficient5 z",
                                                      "-2,                      !- Minimum Value of w",
                                                      "0,                       !- Maximum Value of w",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "2,                       !- Minimum Value of y",
                                                      "4,                       !- Maximum Value of y",
                                                      "3,                       !- Minimum Value of z",
                                                      "5,                       !- Maximum Value of z",
                                                      ",                        !- Minimum Curve Output",
                                                      ",                        !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for w",
                                                      "Dimensionless,           !- Input Unit Type for x",
                                                      "Dimensionless,           !- Input Unit Type for y",
                                                      "Dimensionless;           !- Input Unit Type for z",
                                                      "Curve:QuadLinear,",
                                                      "TestCurve2,              !- Name",
                                                      "1,                       !- Coefficient1 Constant",
                                                      "2,                       !- Coefficient2 w",
                                                      "3,                       !- Coefficient3 x",
                                                      "4,                       !- Coefficient4 y",
                                                      "5,                       !- Coefficient5 z",
                                                      "-2,                      !- Minimum Value of w",
                                                      "0,                       !- Maximum Value of w",
                                                      "0,                       !- Minimum Value of x",
                                                      "2,                       !- Maximum Value of x",
                                                      "2,                       !- Minimum Value of y",
                                                      "4,                       !- Maximum Value of y",
                                                      "3,                       !- Minimum Value of z",
                                                      "5,                       !- Maximum Value of z",
                                                      "25,                      !- Minimum Curve Output",
                                                      "40,                      !- Maximum Curve Output",
                                                      "Dimensionless,           !- Input Unit Type for w",
                                                      "Dimensionless,           !- Input Unit Type for x",
                                                      "Dimensionless,           !- Input Unit Type for y",
                                                      "Dimensionless;           !- Input Unit Type for z"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_FALSE(errorsFound);
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);
    EXPECT_EQ("QUADLINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2, min3, max3;                                 // , min4, max4;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2, min3, max3); //, min4, max4);
    EXPECT_EQ(-2, min1);
    EXPECT_EQ(0, max1);
    EXPECT_EQ(0, min2);
    EXPECT_EQ(2, max2);
    EXPECT_EQ(2, min3);
    EXPECT_EQ(4, max3);
    // EXPECT_EQ(3, min4);
    // EXPECT_EQ(5, max4);
    EXPECT_EQ(CurveManager::CurveType_QuadLinear, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_EQ("QUADLINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTCURVE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTCURVE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTCURVE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2, min3, max3); //, min4, max4);
    EXPECT_EQ(-2, min1);
    EXPECT_EQ(0, max1);
    EXPECT_EQ(0, min2);
    EXPECT_EQ(2, max2);
    EXPECT_EQ(2, min3);
    EXPECT_EQ(4, max3);
    // EXPECT_EQ(3, min4);
    // EXPECT_EQ(5, max4);
    EXPECT_EQ(CurveManager::CurveType_QuadLinear, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -2, 0, 2, 3));  // In-range value
    EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 0, 2, 4, 5));   // In-range value
    EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -3, 0, 2, 3));  // Minimum w
    EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 1, 2, 4, 5));   // Maximum w
    EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -2, -1, 2, 3)); // Minimum x
    EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 0, 7, 4, 5));   // Maximum x
    EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -2, 0, 0, 3));  // Minimum y
    EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 0, 2, 5, 5));   // Maximum y
    EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -2, 0, 2, 0));  // Minimum z
    EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 0, 2, 4, 7));   // Maximum z

    CurveManager::SetCurveOutputMinMaxValues(1, error, 25, 40);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(25.0, CurveManager::CurveValue(1, -2, 0, 2, 3)); // Value too small
    EXPECT_DOUBLE_EQ(40.0, CurveManager::CurveValue(1, 0, 2, 4, 5));  // Value too large

    EXPECT_DOUBLE_EQ(25.0, CurveManager::CurveValue(2, -2, 0, 2, 3)); // Value too small
    EXPECT_DOUBLE_EQ(40.0, CurveManager::CurveValue(2, 0, 2, 4, 5));  // Value too large
    EXPECT_DOUBLE_EQ(27.0, CurveManager::CurveValue(2, 0, 1, 2, 3));  // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_TwoIndVar_Malformed)
{

    DataIPShortCuts::lAlphaFieldBlanks = true;
    std::string const idf_objects = delimited_string({
        "Version,8.5;",
        "                                                                      ",
        "  Table:TwoIndependentVariables,                                      ",
        "    AWHP_rCAP,               !- Name                                  ",
        "    BiQuadratic,             !- Curve Type                            ",
        "    LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
        "    -15,                     !- Minimum Value of X                    ",
        "    25,                      !- Maximum Value of X                    ",
        "    35,                      !- Minimum Value of Y                    ",
        "    55,                      !- Maximum Value of Y                    ",
        "    6842105,                 !- Minimum Table Output                  ",
        "    10657895,                !- Maximum Table Output                  ",
        "    Temperature,             !- Input Unit Type for X                 ",
        "    Temperature,             !- Input Unit Type for Y                 ",
        "    Dimensionless,           !- Output Unit Type                      ",
        "    -15,                     !- Normalization Reference               ",
        "    ,                        !- External File Name",
        "    35,                      !- X Value #1                            ",
        "    0.789473684210526,       !- Y Value #1                            ",
        "    -15,                     !- Output Value #1                       ",
        "    40,                      !- X Value #2                            ",
        "    0.763157894736842,       !- Y Value #2                            ",
        "    -15,                     !- Output Value #2                       ",
        "    45,                      !- X Value #3                            ",
        "    0.736842105263158,       !- Y Value #3                            ",
        "    -15,                     !- Output Value #3                       ",
        "    50,                      !- X Value #4                            ",
        "    0.710526315789474,       !- Y Value #4                            ",
        "    -15,                     !- Output Value #4                       ",
        "    55,                      !- <none>                                ",
        "    0.684210526315789,       !- <none>                                ",
        "    -10,                     !- <none>                                ",
        "    35,                      !- <none>                                ",
        "    0.815789473684211,       !- <none>                                ",
        "    -10,                     !- <none>                                ",
        "    40,                      !- <none>                                ",
        "    0.802631578947368,       !- <none>                                ",
        "    -10,                     !- <none>                                ",
        "    45,                      !- <none>                                ",
        "    0.789473684210526,       !- <none>                                ",
        "    -10,                     !- <none>                                ",
        "    50,                      !- <none>                                ",
        "    0.763157894736842,       !- <none>                                ",
        "    -10,                     !- <none>                                ",
        "    55,                      !- <none>                                ",
        "    0.736842105263158,       !- <none>                                ",
        "    -5,                      !- <none>                                ",
        "    35,                      !- <none>                                ",
        "    0.868421052631579,       !- <none>                                ",
        "    -5,                      !- <none>                                ",
        "    40,                      !- <none>                                ",
        "    0.861842105263158,       !- <none>                                ",
        "    -5,                      !- <none>                                ",
        "    45,                      !- <none>                                ",
        "    0.855263157894737,       !- <none>                                ",
        "    -5,                      !- <none>                                ",
        "    50,                      !- <none>                                ",
        "    0.80921052631579,        !- <none>                                ",
        "    -5,                      !- <none>                                ",
        "    55,                      !- <none>                                ",
        "    0.763157894736842,       !- <none>                                ",
        "    0,                       !- <none>                                ",
        "    35,                      !- <none>                                ",
        "    0.934210526315789,       !- <none>                                ",
        "    0,                       !- <none>                                ",
        "    40,                      !- <none>                                ",
        "    0.914473684210526,       !- <none>                                ",
        "    0,                       !- <none>                                ",
        "    45,                      !- <none>                                ",
        "    0.894736842105263,       !- <none>                                ",
        "    0,                       !- <none>                                ",
        "    50,                      !- <none>                                ",
        "    0.855263157894737,       !- <none>                                ",
        "    0,                       !- <none>                                ",
        "    55,                      !- <none>                                ",
        "    0.815789473684211,       !- <none>                                ",
        "    5,                       !- <none>                                ",
        "    35,                      !- <none>                                ",
        "    1,                       !- <none>                                ",
        "    5,                       !- <none>                                ",
        "    40,                      !- <none>                                ",
        "    0.973684210526316,       !- <none>                                ",
        "    5,                       !- <none>                                ",
        "    45,                      !- <none>                                ",
        "    0.947368421052632,       !- <none>                                ",
        "    5,                       !- <none>                                ",
        "    50,                      !- <none>                                ",
        "    0.888157894736842,       !- <none>                                ",
        "    5,                       !- <none>                                ",
        "    55,                      !- <none>                                ",
        "    0.828947368421053,       !- <none>                                ",
        "    10,                      !- <none>                                ",
        "    35,                      !- <none>                                ",
        "    1.02631578947368,        !- <none>                                ",
        "    10,                      !- <none>                                ",
        "    40,                      !- <none>                                ",
        "    1,                       !- <none>                                ",
        "    10,                      !- <none>                                ",
        "    45,                      !- <none>                                ",
        "    0.973684210526316,       !- <none>                                ",
        "    10,                      !- <none>                                ",
        "    50,                      !- <none>                                ",
        "    0.914473684210526,       !- <none>                                ",
        "    10,                      !- <none>                                ",
        "    55,                      !- <none>                                ",
        "    0.855263157894737,       !- <none>                                ",
        "    15,                      !- <none>                                ",
        "    35,                      !- <none>                                ",
        "    1.05263157894737,        !- <none>                                ",
        "    15,                      !- <none>                                ",
        "    40,                      !- <none>                                ",
        "    1.01973684210526,        !- <none>                                ",
        "    15,                      !- <none>                                ",
        "    45,                      !- <none>                                ",
        "    0.986842105263158,       !- <none>                                ",
        "    15,                      !- <none>                                ",
        "    50,                      !- <none>                                ",
        "    0.940789473684211,       !- <none>                                ",
        "    15,                      !- <none>                                ",
        "    55,                      !- <none>                                ",
        "    0.894736842105263,       !- <none>                                ",
        "    20,                      !- <none>                                ",
        "    35,                      !- <none>                                ",
        "    1.06578947368421,        !- <none>                                ",
        "    20,                      !- <none>                                ",
        "    40,                      !- <none>                                ",
        "    1.02631578947368,        !- <none>                                ",
        "    20,                      !- <none>                                ",
        "    45,                      !- <none>                                ",
        "    0.986842105263158,       !- <none>                                ",
        "    20,                      !- <none>                                ",
        "    50,                      !- <none>                                ",
        "    0.947368421052632,       !- <none>                                ",
        "    20,                      !- <none>                                ",
        "    55,                      !- <none>                                ",
        "    0.907894736842105,       !- <none>                                ",
        "    25,                      !- <none>                                ",
        "    35,                      !- <none>                                ",
        "    1.06578947368421,        !- <none>                                ",
        "    25,                      !- <none>                                ",
        "    40,                      !- <none>                                ",
        "    1.02631578947368,        !- <none>                                ",
        "    25,                      !- <none>                                ",
        "    45,                      !- <none>                                ",
        "    0.986842105263158,       !- <none>                                ",
        "    25,                      !- <none>                                ",
        "    50,                      !- <none>                                ",
        "    0.947368421052632,       !- <none>                                ",
        "    25,                      !- <none>                                ",
        "    55,                      !- <none>                                ",
        "    0.907894736842105;       !- <none>                                ",
        "                                                                      ",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool errorsFound = false;
    CurveManager::GetCurveInputData(errorsFound);
    EXPECT_TRUE(errorsFound);

    EXPECT_EQ(1, CurveManager::NumCurves);

    std::string const error_string = delimited_string({
        "   ** Severe  ** GetCurveInput: For Table:TwoIndependentVariables: AWHP_RCAP",
        "   **   ~~~   ** The number of data entries must be evenly divisable by 3. Number of data entries = 134",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));
}
TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_UserDidNotEnterMinMaxXY)
{
    std::string const idf_objects = delimited_string(
        {"Version,8.5;", "Table:OneIndependentVariable,", "TestTableMinMax,         !- Name", "Linear,                  !- Curve Type",
         "LinearInterpolationOfTable,  !- Interpolation Method", ",                        !- Minimum Value of X",
         ",                        !- Maximum Value of X", ",                        !- Minimum Table Output",
         ",                        !- Maximum Table Output", "Dimensionless,           !- Input Unit Type for X",
         "Dimensionless,           !- Output Unit Type", ",                        !- Normalization Reference",
         "0,                       !- X Value #1", "0,                       !- Output Value #1", "1,                       !- X Value #2",
         "1;                       !- Output Value #2"});
    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(1, CurveManager::NumCurves);
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLEMINMAX", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLEMINMAX"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLEMINMAX", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(0.75, CurveManager::CurveValue(1, 0.75)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5000));  // Maximum x
    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_EvaluateToLimits_UserEnteredMinMaxXY)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTableOverwrite,      !- Name",
                                                      "Linear,                  !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "0.1,                     !- Minimum Value of X",
                                                      "4.9,                     !- Maximum Value of X",
                                                      "1.2,                     !- Minimum Table Output",
                                                      "2.5,                     !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- X Value #1",
                                                      "0,                       !- Output Value #1",
                                                      "1,                       !- X Value #2",
                                                      "2,                       !- Output Value #2",
                                                      "2,                       !- X Value #3",
                                                      "2,                       !- Output Value #3",
                                                      "3,                       !- X Value #4",
                                                      "3,                       !- Output Value #4",
                                                      "4,                       !- X Value #5",
                                                      "4,                       !- Output Value #5",
                                                      "5,                       !- X Value #6",
                                                      "2;                       !- Output Value #6"});

    ASSERT_TRUE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(1, CurveManager::NumCurves);

    // Linear curve type, specified min/max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLEOVERWRITE", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLEOVERWRITE"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLEOVERWRITE", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0.1, min); // Minimum Value of X
    EXPECT_EQ(4.9, max); // Maximum Value of X
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(1.2, CurveManager::CurveValue(1, 0.5));               // Value too small, Min Table Output used
    EXPECT_DOUBLE_EQ(2.5, CurveManager::CurveValue(1, 4));                 // Value too large, Max Table Output used
    EXPECT_DOUBLE_EQ(1.438095238095238, CurveManager::CurveValue(1, 1.0)); // In-range value

    CurveManager::SetCurveOutputMinMaxValues(1, error, 1.1, 2.6);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(1.1, CurveManager::CurveValue(1, 0.3)); // Value too small, new Min Table Output used
    EXPECT_DOUBLE_EQ(2.6, CurveManager::CurveValue(1, 4));   // Value too large, new Max Table Output used
    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_Lagrange_UserDidntEnterMinMaxXY)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTableOverwrite,      !- Name",
                                                      "Linear,                  !- Curve Type",
                                                      "LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION,   !- Interpolation Method",
                                                      ",                        !- Minimum Value of X",
                                                      ",                        !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      ",                        !- Normalization Reference",
                                                      "0.21,                    !- X Value #1",
                                                      "0,                       !- Output Value #1",
                                                      "1,                       !- X Value #2",
                                                      "2,                       !- Output Value #2",
                                                      "2,                       !- X Value #3",
                                                      "2,                       !- Output Value #3",
                                                      "3,                       !- X Value #4",
                                                      "3,                       !- Output Value #4",
                                                      "4,                       !- X Value #5",
                                                      "4,                       !- Output Value #5",
                                                      "4.93,                    !- X Value #6",
                                                      "2;                       !- Output Value #6"});

    ASSERT_TRUE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(1, CurveManager::NumCurves);

    // Linear curve type, specified min/max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLEOVERWRITE", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLEOVERWRITE"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLEOVERWRITE", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0.21, min); // Minimum Value of X in data set
    EXPECT_EQ(4.93, max); // Maximum Value of X in data set
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.73417721518987344, CurveManager::CurveValue(1, 0.5)); // Value in range, no Min Table Output used, show actual value
    Real64 curvOut = ((0.5 - 0.21) / (1.0 - 0.21)) * (2.0 - 0.0);            // Check above: delta X divided by X range multiplied by Y range
    EXPECT_DOUBLE_EQ(0.73417721518987344, curvOut);                          // Compare interpolated hand calc
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, 4));                   // Value right at 4,4. Output = 4
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 1.0));                 // Value right at 1,2. Output = 2
    EXPECT_DOUBLE_EQ(2.753, CurveManager::CurveValue(1, 2.753));             // Value in between 2,2 and 3,3. Output = 2.753

    CurveManager::SetCurveOutputMinMaxValues(1, error, 1.1, 2.6);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(1.1, CurveManager::CurveValue(1, 0.5)); // Value too small, new Min Table Output used
    EXPECT_DOUBLE_EQ(2.6, CurveManager::CurveValue(1, 4));   // Value too large, new Max Table Output used
    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_TwoIndependentVariable_EvaluateToLimits_NotAndUserEnteredMinMaxXY)
{

    std::string const idf_objects = delimited_string({

        "Table:TwoIndependentVariables,",
        "  TWOVARS,        !- Name",
        "  BiQuadratic,    !- Curve Type",
        "  EvaluateCurveToLimits, !- Interpolation Method",
        "  12.77778,       !- Minimum Value of X",
        "  23.88889,       !- Maximum Value of X",
        "  18.0,           !- Minimum Value of Y",
        "  46.11111,       !- Maximum Value of Y",
        "  15000,          !- Minimum Table Output",
        "  40000,          !- Maximum Table Output",
        "  Temperature,    !- Input Unit Type for X",
        "  Temperature,    !- Input Unit Type for Y",
        "  Dimensionless,  !- Output Unit Type",
        "  25000.0,        !- Normalization Reference",
        "  ,               !- External File Name",
        "  12.77778,       !- X Value #1",
        "  36,             !- Y Value #1",
        "  19524.15032,    !- Output Value #1",
        "  12.77778,       !- X Value #2",
        "  42,             !- Y Value #2",
        "  18167.25518,    !- Output Value #2",
        "  12.77778,       !- X Value #3",
        "  46.11111,       !- Y Value #3",
        "  16810.36004,    !- Output Value #3",
        "  19.44448943,    !- <none>",
        "  41,             !- <none>",
        "  23375.08713,    !- <none>",
        "  19.44448943,    !- <none>",
        "  41,             !- <none>",
        "  22686.72090,    !- <none>",
        "  19.44448943,    !- <none>",
        "  46.11111,       !- <none>",
        "  21998.35468;    !- <none>",

        "Table:TwoIndependentVariables,",
        "  TWOVARS2,       !- Name",
        "  BiQuadratic,    !- Curve Type",
        "  EvaluateCurveToLimits, !- Interpolation Method",
        "  ,               !- Minimum Value of X",
        "  ,               !- Maximum Value of X",
        "  ,               !- Minimum Value of Y",
        "  ,               !- Maximum Value of Y",
        "  ,               !- Minimum Table Output",
        "  ,               !- Maximum Table Output",
        "  Temperature,    !- Input Unit Type for X",
        "  Temperature,    !- Input Unit Type for Y",
        "  Dimensionless,  !- Output Unit Type",
        "  25000.0,        !- Normalization Reference",
        "  ,               !- External File Name",
        "  12.77778,       !- X Value #1",
        "  36,             !- Y Value #1",
        "  19524.15032,    !- Output Value #1",
        "  12.77778,       !- X Value #2",
        "  42,             !- Y Value #2",
        "  18167.25518,    !- Output Value #2",
        "  12.77778,       !- X Value #3",
        "  46.11111,       !- Y Value #3",
        "  16810.36004,    !- Output Value #3",
        "  19.44448943,    !- <none>",
        "  41,             !- <none>",
        "  23375.08713,    !- <none>",
        "  19.44448943,    !- <none>",
        "  41,             !- <none>",
        "  22686.72090,    !- <none>",
        "  19.44448943,    !- <none>",
        "  46.11111,       !- <none>",
        "  21998.35468;    !- <none>"});

    ASSERT_TRUE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);

    // BiQuadratic curve type, specified min/max
    EXPECT_EQ("BIQUADRATIC", CurveManager::GetCurveType(1));
    EXPECT_EQ("TWOVARS", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TWOVARS"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TWOVARS", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(12.77778, min1); // Minimum Value of X
    EXPECT_EQ(23.88889, max1); // Maximum Value of X
    EXPECT_EQ(18.0, min2);     // Minimum Value of Y
    EXPECT_EQ(46.11111, max2); // Maximum Value of Y
    EXPECT_EQ((15000.0 / 25000.0), CurveManager::PerfCurve(1).CurveMin);
    EXPECT_EQ((40000.0 / 25000.0), CurveManager::PerfCurve(1).CurveMax);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_GT(CurveManager::CurveValue(1, 10.0, 15.0),
              (15000.0 / 25000.0)); // both values too small, Minimum Value of X (12.8) and Y (18) are used, Min Table Output is not used
    Real64 Coeff1 = CurveManager::PerfCurve(1).Coeff1;
    Real64 Coeff2 = CurveManager::PerfCurve(1).Coeff2;
    Real64 Coeff3 = CurveManager::PerfCurve(1).Coeff3;
    Real64 Coeff4 = CurveManager::PerfCurve(1).Coeff4;
    Real64 Coeff5 = CurveManager::PerfCurve(1).Coeff5;
    Real64 Coeff6 = CurveManager::PerfCurve(1).Coeff6;

    // calculate output as if entered data was valid
    Real64 curveOut = Coeff1 + (Coeff2 * 10.0) + (Coeff3 * 10.0 * 10.0) + (Coeff4 * 15.0) + (Coeff5 * 15.0 * 15.0) + (Coeff6 * 10.0 * 15.0);
    // entered data was not valid since 10 < min X1 and 15 < min X2 so expect result to be greater than curveOut
    EXPECT_LT(curveOut, CurveManager::CurveValue(1, 10.0, 15.0)); // result of using less than minimum X1 and X2 limits

    // calculate new value using min X1 and min X2, this value should match curve output
    Real64 curveOutActual =
        Coeff1 + (Coeff2 * 12.77778) + (Coeff3 * 12.77778 * 12.77778) + (Coeff4 * 18.0) + (Coeff5 * 18.0 * 18.0) + (Coeff6 * 12.77778 * 18.0);
    EXPECT_NEAR(0.7662161, curveOutActual, 0.0000001);                              // result of using less than minimum X1 and X2 limits
    EXPECT_NEAR(curveOutActual, CurveManager::CurveValue(1, 10.0, 15.0), 0.000001); // result of using less than minimum X1 and X2 limits
    EXPECT_NEAR(curveOutActual, CurveManager::CurveValue(1, 12.0, 16.0),
                0.000001); // result shouldn't change as long as inputs are below minimum X1 and X2 limits
    curveOutActual = Coeff1 + (Coeff2 * 23.88889) + (Coeff3 * 23.88889 * 23.88889) + (Coeff4 * 46.11111) + (Coeff5 * 46.11111 * 46.11111) +
                     (Coeff6 * 23.88889 * 46.11111);
    EXPECT_LT(CurveManager::CurveValue(1, 40.0, 50.0),
              (40000.0 / 25000.0)); // both values too large, Maximum Value of X1 (23.8) and X2 (46.1) are used, Max Table Output is too high
    EXPECT_NEAR(curveOutActual, CurveManager::CurveValue(1, 40.0, 50.0), 0.000001); // result of using maximum X1 and X2 limits
    EXPECT_NEAR(curveOutActual, CurveManager::CurveValue(1, 25.0, 47.0),
                0.000001);                               // result shouldn't change as long as inputs are above maximum X and Y limits
    EXPECT_NEAR(0.91625045, curveOutActual, 0.00000001); // result is actually less than max ouput since a regression was performed

    // artificially change CurveMin And CurveMax and repeat limit test
    CurveManager::PerfCurve(1).CurveMin = 0.8;               // 0.8 is same as entering 20000 for Minimum Table Output
    CurveManager::PerfCurve(1).CurveMax = 0.9;               // 0.9 is same as entering 22500 for Maximum Table Output
    EXPECT_EQ(0.8, CurveManager::CurveValue(1, 10.0, 15.0)); // result of using minimum X1 and X2 limits when minimum output > result
    EXPECT_EQ(0.9, CurveManager::CurveValue(1, 40.0, 50.0)); // result of using maximum X1 and X2 limits when maximum output < result

    // Evaluate 2nd performance curve
    // BiQuadratic curve type, no specified min/max
    EXPECT_EQ("BIQUADRATIC", CurveManager::GetCurveType(2));
    EXPECT_EQ("TWOVARS2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TWOVARS2"));
    error = false;
    index = CurveManager::GetCurveCheck("TWOVARS2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);

    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2);
    EXPECT_EQ(12.77778, min1);    // Minimum Value of X defaults to lower limit specified in table data
    EXPECT_EQ(19.44448943, max1); // Maximum Value of X defaults to upper limit specified in table data
    EXPECT_EQ(36.0, min2);        // Minimum Value of Y defaults to lower limit specified in table data
    EXPECT_EQ(46.11111, max2);    // Maximum Value of Y defaults to upper limit specified in table data

    // curve min/max output were not entered by user. Expect min/max equal to 0 since they were not initilized
    EXPECT_NEAR(0.0, CurveManager::PerfCurve(2).CurveMin, 0.0000000001);
    EXPECT_FALSE(CurveManager::PerfCurve(2).CurveMinPresent); // min won't be used since value is NOT present
    EXPECT_NEAR(0.0, CurveManager::PerfCurve(2).CurveMax, 0.0000000001);
    EXPECT_FALSE(CurveManager::PerfCurve(2).CurveMaxPresent); // max won't be used since value is NOT present
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_GT(CurveManager::CurveValue(2, 10.0, 15.0),
              (15000.0 / 25000.0)); // both values too small, Minimum Value of X1 (12.8) and X2 (18) are used, Min Table Output is not present
    EXPECT_NEAR(0.780966, CurveManager::CurveValue(2, 10.0, 15.0), 0.0000001); // result of using minimum X1 and X2 limits
    EXPECT_NEAR(0.780966, CurveManager::CurveValue(2, 12.0, 16.0),
                0.0000001); // result shouldn't change as long as inputs are below minimum X1 and X2 limits
    curveOutActual = Coeff1 + (Coeff2 * 19.44448943) + (Coeff3 * 19.44448943 * 19.44448943) + (Coeff4 * 46.11111) + (Coeff5 * 46.11111 * 46.11111) +
                     (Coeff6 * 19.44448943 * 46.11111);
    EXPECT_LT(CurveManager::CurveValue(2, 40.0, 50.0),
              (23375.08713 / 25000.0)); // both values too large, Maximum Value of X1 (23.8) and X2 (46.1) are used, Max Table Output is not present
    EXPECT_NEAR(curveOutActual, CurveManager::CurveValue(2, 40.0, 50.0), 0.000001); // result of using maximum X1 and X2 limits
    EXPECT_NEAR(curveOutActual, CurveManager::CurveValue(2, 25.0, 47.0),
                0.000001); // result shouldn't change as long as inputs are above maximum X1 and X2 limits

    // test capacity entered by user is same as calculated
    EXPECT_NEAR(19524.15032, (CurveManager::CurveValue(2, 12.77778, 36.0) * 25000.0), 0.1); // uses data from first entry in table

    // artificially change CurveMin And CurveMax to be above min and below max table data and repeat limit test
    CurveManager::PerfCurve(2).CurveMin = 0.8;         // 0.8 is same as entering 20000 for Minimum Table Output
    CurveManager::PerfCurve(2).CurveMinPresent = true; // as if user did enter input data
    CurveManager::PerfCurve(2).CurveMax = 0.87;        // 0.87 is same as entering 21750 for Maximum Table Output
    CurveManager::PerfCurve(2).CurveMaxPresent = true; // as if user did enter input data

    EXPECT_EQ(0.8, CurveManager::CurveValue(2, 10.0, 15.0));  // result of using minimum X1 and X2 table data limits, Curve Minimum Output > result
    EXPECT_EQ(0.87, CurveManager::CurveValue(2, 40.0, 50.0)); // result of using maximum X1 and X2 table data limits, Curve Maximum Output < result

    // artificially change CurveMin And CurveMax to be below min and above max table data and repeat limit test
    CurveManager::PerfCurve(2).CurveMin = 0.5; // 0.5 is same as entering 12500 for Minimum Table Output
    CurveManager::PerfCurve(2).CurveMax = 1.2; // 1.2 is same as entering 30000 for Maximum Table Output
                                               // result is lower than before when using 0.8 as Minimum Table Output
    EXPECT_LT(
        0.5, CurveManager::CurveValue(2, 10.0, 15.0)); // new result is lower than 0.8 but not lower than 0.5 since output is limited by X1 X2 min/max
    EXPECT_GT(0.8, CurveManager::CurveValue(2, 10.0, 15.0));
    EXPECT_NEAR(0.780966, CurveManager::CurveValue(2, 10.0, 15.0), 0.000001);
    // result is higher than before when using 0.87 as Maximum Table Output
    EXPECT_LT(0.87, CurveManager::CurveValue(
                        2, 40.0, 50.0)); // new result is higher than 0.87 but not higher than 1.2 since output is limited by X1 X2 min/max
    EXPECT_GT(1.2, CurveManager::CurveValue(2, 40.0, 50.0));
    EXPECT_NEAR(0.8799342, CurveManager::CurveValue(2, 40.0, 50.0), 0.0000001);

    // if CurveMin And CurveMax are blank, result should be same as above
    CurveManager::PerfCurve(2).CurveMinPresent = false;      // as if user did not enter input data
    CurveManager::PerfCurve(2).CurveMaxPresent = false;      // as if user did not enter input data
    EXPECT_LT(0.5, CurveManager::CurveValue(2, 10.0, 15.0)); // curve extrapolates up to
    EXPECT_GT(1.2, CurveManager::CurveValue(2, 40.0, 50.0)); // result of using maximum X and Y limits when maximum output < result

    // if user actually enters min/max values to extrapolate curve, then allow extrapolation
    CurveManager::PerfCurve(2).Var1Min = 10.0;
    CurveManager::PerfCurve(2).Var1Max = 40.0;
    CurveManager::PerfCurve(2).Var2Min = 15.0;
    CurveManager::PerfCurve(2).Var2Max = 50.0;
    Real64 extrapolatedCapacity = CurveManager::CurveValue(2, 10.0, 15.0) * 25000.0;
    EXPECT_LT(extrapolatedCapacity, 16810.36004); // curve extrapolates lower than minimum Y based on table data

    extrapolatedCapacity = CurveManager::CurveValue(2, 40.0, 50.0) * 25000.0;
    Real64 minY = 16810.36004 / 25000.0;
    Real64 maxY = 23375.08713 / 25000.0;

    EXPECT_LT(CurveManager::CurveValue(2, 40.0, 50.0),
              maxY); // results show dangers of extrapolation, large X1 and X2 gives output lower than max Y table data
    EXPECT_LT(CurveManager::CurveValue(2, 40.0, 50.0),
              minY); // results show dangers of extrapolation, large X1 and X2 gives output lower than min Y table data
    Coeff1 = CurveManager::PerfCurve(2).Coeff1;
    Coeff2 = CurveManager::PerfCurve(2).Coeff2;
    Coeff3 = CurveManager::PerfCurve(2).Coeff3;
    Coeff4 = CurveManager::PerfCurve(2).Coeff4;
    Coeff5 = CurveManager::PerfCurve(2).Coeff5;
    Coeff6 = CurveManager::PerfCurve(2).Coeff6;
    curveOutActual = Coeff1 + (Coeff2 * 40.0) + (Coeff3 * 40.0 * 40.0) + (Coeff4 * 50.0) + (Coeff5 * 50.0 * 50.0) + (Coeff6 * 40.0 * 50.0);
    EXPECT_NEAR((curveOutActual * 25000.0), extrapolatedCapacity, 0.001); // result of extrapolation is value less than minimum table data
    EXPECT_NEAR(9358.378, (curveOutActual * 25000.0), 0.001);             // result of extrapolation is value less than minimum table data
}

TEST_F(EnergyPlusFixture, Tables_TwoIndependentVariable_Linear_UserDidNotEnterMinMaxXY)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:TwoIndependentVariables,",
                                                      "TestTableMinMax,         !- Name",
                                                      "QuadraticLinear,         !- Curve Type",
                                                      "LinearInterpolationOfTable, !- Interpolation Method",
                                                      ",                        !- Minimum Value of X",
                                                      ",                        !- Maximum Value of X",
                                                      ",                        !- Minimum Value of Y",
                                                      ",                        !- Maximum Value of Y",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      ",                        !- Normalization Reference",
                                                      "  ,               !- External File Name",
                                                      "0,                       !- X Value #1",
                                                      "1,                       !- Y Value #1",
                                                      "0,                       !- Output Value #1",
                                                      "1,                       !- X Value #2",
                                                      "1,                       !- Y Value #2",
                                                      "2,                       !- Output Value #2",
                                                      "2,                       !- X Value #3",
                                                      "1,                       !- Y Value #3",
                                                      "2,                       !- Output Value #3",
                                                      "3,                       !- X Value #4",
                                                      "1,                       !- Y Value #4",
                                                      "3,                       !- Output Value #4",
                                                      "4,                       !- X Value #5",
                                                      "1,                       !- Y Value #5",
                                                      "4,                       !- Output Value #5",
                                                      "5,                       !- X Value #6",
                                                      "1,                       !- Y Value #6",
                                                      "2,                       !- Output Value #6",
                                                      "0,                       !- X Value #7",
                                                      "2,                       !- Y Value #7",
                                                      "0,                       !- Output Value #7",
                                                      "1,                       !- X Value #8",
                                                      "2,                       !- Y Value #8",
                                                      "2,                       !- Output Value #8",
                                                      "2,                       !- X Value #9",
                                                      "2,                       !- Y Value #9",
                                                      "2,                       !- Output Value #9",
                                                      "3,                       !- X Value #10",
                                                      "2,                       !- Y Value #10",
                                                      "3,                       !- Output Value #10",
                                                      "4,                       !- X Value #11",
                                                      "2,                       !- Y Value #11",
                                                      "4,                       !- Output Value #11",
                                                      "5,                       !- X Value #12",
                                                      "2,                       !- Y Value #12",
                                                      "2;                       !- Output Value #12"});

    ASSERT_TRUE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(1, CurveManager::NumCurves);

    // QuadraticLinear curve type, no min/max for IVs
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLEMINMAX", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLEMINMAX"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLEMINMAX", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0.0, min1); // CurveValue will test against lower boundary and use aray minimum
    EXPECT_EQ(5.0, max1); // CurveValue will test against upper boundary and use aray maximum
    EXPECT_EQ(1.0, min2); // CurveValue will test against lower boundary and use aray minimum
    EXPECT_EQ(2.0, max2); // CurveValue will test against upper boundary and use aray maximum
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0));    // value less than Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000));     // value greater than Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5, 1.5)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_TwoIndependentVariable_Linear_UserEntersInAndOutOfBoundsMinMaxXY)
{

    std::string const idf_objects =
        delimited_string({"Version,8.5;",
                          "Table:TwoIndependentVariables,",
                          "TestTableMinMax,         !- Name",
                          "QuadraticLinear,         !- Curve Type",
                          "LinearInterpolationOfTable, !- Interpolation Method",
                          "-1.0,                    !- Minimum Value of X", // these values exceed the values in the data set
                          "6.0,                     !- Maximum Value of X",
                          "-0.5,                    !- Minimum Value of Y",
                          "4.5,                     !- Maximum Value of Y",
                          ",                        !- Minimum Table Output",
                          ",                        !- Maximum Table Output",
                          "Dimensionless,           !- Input Unit Type for X",
                          "Dimensionless,           !- Input Unit Type for Y",
                          "Dimensionless,           !- Output Unit Type",
                          ",                        !- Normalization Reference",
                          "  ,               !- External File Name",
                          "0,                       !- X Value #1",
                          "1,                       !- Y Value #1",
                          "0,                       !- Output Value #1",
                          "1,                       !- X Value #2",
                          "1,                       !- Y Value #2",
                          "2,                       !- Output Value #2",
                          "2,                       !- X Value #3",
                          "1,                       !- Y Value #3",
                          "2,                       !- Output Value #3",
                          "3,                       !- X Value #4",
                          "1,                       !- Y Value #4",
                          "3,                       !- Output Value #4",
                          "4,                       !- X Value #5",
                          "1,                       !- Y Value #5",
                          "4,                       !- Output Value #5",
                          "5,                       !- X Value #6",
                          "1,                       !- Y Value #6",
                          "2,                       !- Output Value #6",
                          "0,                       !- X Value #7",
                          "2,                       !- Y Value #7",
                          "0,                       !- Output Value #7",
                          "1,                       !- X Value #8",
                          "2,                       !- Y Value #8",
                          "2,                       !- Output Value #8",
                          "2,                       !- X Value #9",
                          "2,                       !- Y Value #9",
                          "2,                       !- Output Value #9",
                          "3,                       !- X Value #10",
                          "2,                       !- Y Value #10",
                          "3,                       !- Output Value #10",
                          "4,                       !- X Value #11",
                          "2,                       !- Y Value #11",
                          "4,                       !- Output Value #11",
                          "5,                       !- X Value #12",
                          "2,                       !- Y Value #12",
                          "2;                       !- Output Value #12",

                          "Table:TwoIndependentVariables,",
                          "TestTableMinMax2,        !- Name",
                          "QuadraticLinear,         !- Curve Type",
                          "LinearInterpolationOfTable, !- Interpolation Method",
                          "1.0,                    !- Minimum Value of X", // these values are within the values in the data set
                          "4.0,                     !- Maximum Value of X",
                          "1.5,                    !- Minimum Value of Y",
                          "3.5,                     !- Maximum Value of Y",
                          ",                        !- Minimum Table Output",
                          ",                        !- Maximum Table Output",
                          "Dimensionless,           !- Input Unit Type for X",
                          "Dimensionless,           !- Input Unit Type for Y",
                          "Dimensionless,           !- Output Unit Type",
                          ",                        !- Normalization Reference",
                          "  ,               !- External File Name",
                          "0,                       !- X Value #1",
                          "1,                       !- Y Value #1",
                          "0,                       !- Output Value #1",
                          "1,                       !- X Value #2",
                          "1,                       !- Y Value #2",
                          "2,                       !- Output Value #2",
                          "2,                       !- X Value #3",
                          "1,                       !- Y Value #3",
                          "2,                       !- Output Value #3",
                          "3,                       !- X Value #4",
                          "1,                       !- Y Value #4",
                          "3,                       !- Output Value #4",
                          "4,                       !- X Value #5",
                          "1,                       !- Y Value #5",
                          "4,                       !- Output Value #5",
                          "5,                       !- X Value #6",
                          "1,                       !- Y Value #6",
                          "2,                       !- Output Value #6",
                          "0,                       !- X Value #7",
                          "2,                       !- Y Value #7",
                          "0,                       !- Output Value #7",
                          "1,                       !- X Value #8",
                          "2,                       !- Y Value #8",
                          "2,                       !- Output Value #8",
                          "2,                       !- X Value #9",
                          "2,                       !- Y Value #9",
                          "2,                       !- Output Value #9",
                          "3,                       !- X Value #10",
                          "2,                       !- Y Value #10",
                          "3,                       !- Output Value #10",
                          "4,                       !- X Value #11",
                          "2,                       !- Y Value #11",
                          "4,                       !- Output Value #11",
                          "5,                       !- X Value #12",
                          "2,                       !- Y Value #12",
                          "2;                       !- Output Value #12"});

    ASSERT_TRUE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(2, CurveManager::NumCurves);

    // QuadraticLinear curve type
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLEMINMAX", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLEMINMAX"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLEMINMAX", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0.0, min1); // user entered value is retained, however, CurveValue will test against lower array boundary and use aray minimum
    EXPECT_EQ(5.0, max1); // user entered value is retained, however, CurveValue will test against upper array boundary and use aray maximum
    EXPECT_EQ(1.0, min2); // user entered value is retained, however, CurveValue will test against lower array boundary and use aray minimum
    EXPECT_EQ(2, max2);   // user entered value is retained, however, CurveValue will test against upper array boundary and use aray maximum
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0));    // value less than Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000));     // value greater than Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5, 1.5)); // In-range value

    EXPECT_TRUE(has_err_output());

    // Test 2nd table with tighter min/max X Y limits than data set

    // QuadraticLinear curve type
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLEMINMAX2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLEMINMAX2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLEMINMAX2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);

    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2);
    EXPECT_EQ(1.0, min1); // user entered value is retained and used since it's greater than lower array boundary
    EXPECT_EQ(4.0, max1); // user entered value is retained and used since it's less than upper array boundary
    EXPECT_EQ(1.5, min2); // user entered value is retained and used since it's greater than lower array boundary
    EXPECT_EQ(2.0, max2); // user entered value is NOT retained since it's greater than upper array boundary
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(2, 0, 1.5));     // In-range value, result is based on tighten Minimum X Value limit
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(2, 0.5, 1.5));   // In-range value, result is based on tighten Maximum X Value limit
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(2, -10.0, 1.5)); // value less than Minimum x, y value in range
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(2, 5000, 1.5));  // value greater than Maximum x, y value in range

    CurveManager::SetCurveOutputMinMaxValues(2, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 0, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 5, 1.5)); // In-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_Linear_EvaluateCurveTypes)
{
    std::string const idf_objects = delimited_string({

        "Table:OneIndependentVariable,",
        "TestTableMinMax,         !- Name",
        "Linear,                  !- Curve Type",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        ",                        !- Minimum Value of X",
        ",                        !- Maximum Value of X",
        ",                        !- Minimum Table Output",
        ",                        !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- Normalization Reference",
        "0,                       !- X Value #1",
        "0,                       !- Output Value #1",
        "4,                       !- X Value #2",
        "4;                       !- Output Value #2",

        "Table:OneIndependentVariable,",
        "TestTableMinMax2,         !- Name",
        "Quadratic,               !- Curve Type",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        ",                        !- Minimum Value of X",
        ",                        !- Maximum Value of X",
        ",                        !- Minimum Table Output",
        ",                        !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- Normalization Reference",
        "0,                       !- X Value #1",
        "0,                       !- Output Value #1",
        "1,                       !- X Value #2",
        "1,                       !- Output Value #2",
        "4,                       !- X Value #3",
        "4;                       !- Output Value #3",

        "Table:OneIndependentVariable,",
        "TestTableMinMax3,         !- Name",
        "Cubic,                    !- Curve Type",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        ",                        !- Minimum Value of X",
        ",                        !- Maximum Value of X",
        ",                        !- Minimum Table Output",
        ",                        !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- Normalization Reference",
        "0,                       !- X Value #1",
        "0,                       !- Output Value #1",
        "1,                       !- X Value #2",
        "1,                       !- Output Value #2",
        "3,                       !- X Value #3",
        "3,                       !- Output Value #3",
        "4,                       !- X Value #4",
        "4;                       !- Output Value #4",

        "Table:OneIndependentVariable,",
        "TestTableMinMax4,         !- Name",
        "Quartic,                  !- Curve Type",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        ",                        !- Minimum Value of X",
        ",                        !- Maximum Value of X",
        ",                        !- Minimum Table Output",
        ",                        !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- Normalization Reference",
        "0,                       !- X Value #1",
        "0,                       !- Output Value #1",
        "1,                       !- X Value #2",
        "1,                       !- Output Value #2",
        "2,                       !- X Value #3",
        "2,                       !- Output Value #3",
        "3,                       !- X Value #4",
        "3,                       !- Output Value #4",
        "4,                       !- X Value #5",
        "4;                       !- Output Value #5"});

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(4, CurveManager::NumCurves);

    // all curve types should show linear output (higher order coefficients should = 0)
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 1));     // In-range value
    EXPECT_DOUBLE_EQ(1.75, CurveManager::CurveValue(1, 1.75)); // In-range value
    EXPECT_DOUBLE_EQ(2.25, CurveManager::CurveValue(1, 2.25)); // In-range value
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, 4));     // In-range value

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(2, 0));     // In-range value
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 1));     // In-range value
    EXPECT_DOUBLE_EQ(1.75, CurveManager::CurveValue(2, 1.75)); // In-range value
    EXPECT_DOUBLE_EQ(2.25, CurveManager::CurveValue(2, 2.25)); // In-range value
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(2, 4));     // In-range value

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, 0));     // In-range value
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 1));     // In-range value
    EXPECT_DOUBLE_EQ(1.75, CurveManager::CurveValue(3, 1.75)); // In-range value
    EXPECT_DOUBLE_EQ(2.25, CurveManager::CurveValue(3, 2.25)); // In-range value
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(3, 4));     // In-range value

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 0));     // In-range value
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 1));     // In-range value
    EXPECT_DOUBLE_EQ(1.75, CurveManager::CurveValue(4, 1.75)); // In-range value
    EXPECT_DOUBLE_EQ(2.25, CurveManager::CurveValue(4, 2.25)); // In-range value
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(4, 4));     // In-range value

    // curves should not extrapolate
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, 6)); // Out-of-range value
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(2, 6)); // Out-of-range value
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(3, 6)); // Out-of-range value
    EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(4, 6)); // Out-of-range value

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, TableLookupObject_ExcessArguments_WarningTest)
{

    int NumOfTables = 5;

    PerfCurve.allocate(NumOfTables);
    TableLookup.allocate(NumOfTables);

    // Table 1 One ind variable
    PerfCurve(1).TableIndex = 1;
    PerfCurve(1).Var1Max = 20.0;
    PerfCurve(1).Var1Min = 0.0;
    PerfCurve(1).Var2Max = 0.0;
    PerfCurve(1).Var2Min = 0.0;
    PerfCurve(1).CurveType = Linear;
    TableLookup(PerfCurve(1).TableIndex).NumIndependentVars = 1;
    PerfCurve(1).ObjectType = CurveType_TableOneIV;
    PerfCurve(1).Name = "Table 1";
    TableLookup(1).NumX1Vars = 2;
    TableLookup(1).InterpolationOrder = 2;
    TableLookup(1).NumX1Vars = 2;
    TableLookup(1).X1Var.allocate(TableLookup(1).NumX1Vars);
    TableLookup(1).X1Var(1) = 0.0;
    TableLookup(1).X1Var(2) = 1.0;
    TableLookup(1).TableLookupZData.allocate(1, 1, 1, 1, 1, 2);
    TableLookup(1).TableLookupZData(1, 1, 1, 1, 1, 1) = 0.0;
    TableLookup(1).TableLookupZData(1, 1, 1, 1, 1, 2) = 1.0;

    TableLookupObject(1, 1.0, 0.0);

    std::string const error_string = delimited_string({
        "   ** Warning ** TableLookupObject: Table:OneIndependentVariable\"Table 1\"",
        "   **   ~~~   ** ...Excess number of independent variables (2) passed to subroutine when only 1 is required. The excess arguments are "
        "ignored.",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));

    // Table 2 Two ind variable
    PerfCurve(2).TableIndex = 2;
    PerfCurve(2).Var1Max = 20.0;
    PerfCurve(2).Var1Min = 0.0;
    PerfCurve(2).Var2Max = 20.0;
    PerfCurve(2).Var2Min = 0.0;
    PerfCurve(2).Var3Max = 0.0;
    PerfCurve(2).Var3Min = 0.0;
    PerfCurve(2).CurveType = Linear;
    TableLookup(PerfCurve(2).TableIndex).NumIndependentVars = 2;
    PerfCurve(2).ObjectType = CurveType_TableTwoIV;
    PerfCurve(2).Name = "Table 2";
    TableLookup(2).NumX1Vars = 2;
    TableLookup(2).NumX2Vars = 1;
    TableLookup(2).InterpolationOrder = 2;
    TableLookup(2).X1Var.allocate(TableLookup(2).NumX1Vars);
    TableLookup(2).X2Var.allocate(TableLookup(2).NumX2Vars);
    TableLookup(2).X1Var(1) = 0.0;
    TableLookup(2).X1Var(2) = 1.0;
    TableLookup(2).X2Var(1) = 1.0;
    TableLookup(2).TableLookupZData.allocate(1, 1, 1, 1, 1, 2);
    TableLookup(2).TableLookupZData(1, 1, 1, 1, 1, 1) = 0.0;
    TableLookup(2).TableLookupZData(1, 1, 1, 1, 1, 2) = 1.0;

    TableLookupObject(2, 1.0, 1.0, 0.0);

    std::string const error_string1 = delimited_string({
        "   ** Warning ** TableLookupObject: Table:TwoIndependentVariables\"Table 2\"",
        "   **   ~~~   ** ...Excess number of independent variables (3) passed to subroutine when 2 or less are required. The excess arguments are "
        "ignored.",
    });

    EXPECT_TRUE(compare_err_stream(error_string1, true));

    // Table 3 Three ind variable
    int Index = 3;
    PerfCurve(Index).TableIndex = 3;
    PerfCurve(Index).Var1Max = 20.0;
    PerfCurve(Index).Var1Min = 0.0;
    PerfCurve(Index).Var2Max = 20.0;
    PerfCurve(Index).Var2Min = 0.0;
    PerfCurve(Index).Var3Max = 0.0;
    PerfCurve(Index).Var3Min = 0.0;
    PerfCurve(Index).CurveType = Linear;
    TableLookup(PerfCurve(Index).TableIndex).NumIndependentVars = 3;
    PerfCurve(Index).ObjectType = CurveType_TableMultiIV;
    PerfCurve(Index).Name = "Table 3";
    TableLookup(Index).NumX1Vars = 2;
    TableLookup(Index).NumX2Vars = 1;
    TableLookup(Index).NumX3Vars = 1;
    TableLookup(Index).InterpolationOrder = 2;
    TableLookup(Index).X1Var.allocate(TableLookup(Index).NumX1Vars);
    TableLookup(Index).X2Var.allocate(TableLookup(Index).NumX2Vars);
    TableLookup(Index).X3Var.allocate(TableLookup(Index).NumX3Vars);
    TableLookup(Index).X1Var(1) = 0.0;
    TableLookup(Index).X1Var(2) = 1.0;
    TableLookup(Index).X2Var(1) = 1.0;
    TableLookup(Index).X3Var(1) = 1.0;
    TableLookup(Index).TableLookupZData.allocate(1, 1, 1, 1, 1, 2);
    TableLookup(Index).TableLookupZData(1, 1, 1, 1, 1, 1) = 0.0;
    TableLookup(Index).TableLookupZData(1, 1, 1, 1, 1, 2) = 1.0;

    TableLookupObject(Index, 1.0, 1.0, 1.0, 0.0);

    std::string const error_string2 = delimited_string({
        "   ** Warning ** TableLookupObject: Table:MultiVariableLookup\"Table 3\"",
        "   **   ~~~   ** ...Excess number of independent variables (4) passed to subroutine when 3 or less are required. The excess arguments are "
        "ignored.",
    });

    EXPECT_TRUE(compare_err_stream(error_string2, true));

    // Table 4 Four ind variable
    Index = 4;
    PerfCurve(Index).TableIndex = Index;
    PerfCurve(Index).Var1Max = 20.0;
    PerfCurve(Index).Var1Min = 0.0;
    PerfCurve(Index).Var2Max = 20.0;
    PerfCurve(Index).Var2Min = 0.0;
    PerfCurve(Index).Var3Max = 20.0;
    PerfCurve(Index).Var3Min = 0.0;
    PerfCurve(Index).Var4Max = 0.0;
    PerfCurve(Index).Var4Min = 0.0;
    PerfCurve(Index).CurveType = Linear;
    TableLookup(PerfCurve(Index).TableIndex).NumIndependentVars = 4;
    PerfCurve(Index).ObjectType = CurveType_TableMultiIV;
    PerfCurve(Index).Name = "Table 4";
    TableLookup(Index).NumX1Vars = 2;
    TableLookup(Index).NumX2Vars = 1;
    TableLookup(Index).NumX3Vars = 1;
    TableLookup(Index).NumX4Vars = 1;
    TableLookup(Index).InterpolationOrder = 2;
    TableLookup(Index).X1Var.allocate(TableLookup(Index).NumX1Vars);
    TableLookup(Index).X2Var.allocate(TableLookup(Index).NumX2Vars);
    TableLookup(Index).X3Var.allocate(TableLookup(Index).NumX3Vars);
    TableLookup(Index).X4Var.allocate(TableLookup(Index).NumX4Vars);
    TableLookup(Index).X1Var(1) = 0.0;
    TableLookup(Index).X1Var(2) = 1.0;
    TableLookup(Index).X2Var(1) = 1.0;
    TableLookup(Index).X3Var(1) = 1.0;
    TableLookup(Index).X4Var(1) = 1.0;
    TableLookup(Index).TableLookupZData.allocate(1, 1, 1, 1, 1, 2);
    TableLookup(Index).TableLookupZData(1, 1, 1, 1, 1, 1) = 0.0;
    TableLookup(Index).TableLookupZData(1, 1, 1, 1, 1, 2) = 1.0;

    TableLookupObject(Index, 1.0, 1.0, 1.0, 1.0, 0.0);

    std::string const error_string3 = delimited_string({
        "   ** Warning ** TableLookupObject: Table:MultiVariableLookup\"Table 4\"",
        "   **   ~~~   ** ...Excess number of independent variables (5) passed to subroutine when 4 or less are required. The excess arguments are "
        "ignored.",
    });

    EXPECT_TRUE(compare_err_stream(error_string3, true));

    PerfCurve.deallocate();
    TableLookup.deallocate();
}
