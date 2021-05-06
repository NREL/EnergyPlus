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
#include <EnergyPlus/ConfiguredFunctions.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/FileSystem.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, CurveExponentialSkewNormal_MaximumCurveOutputTest)
{
    std::string const idf_objects = delimited_string({
        "Curve:ExponentialSkewNormal,",
        "  FanEff120CPLANormal,     !- Name",
        "  0.072613,                !- Coefficient1 C1",
        "  0.833213,                !- Coefficient2 C2",
        "  0.,                      !- Coefficient3 C3",
        "  0.013911,                !- Coefficient4 C4",
        "  -4.,                     !- Minimum Value of x",
        "  5.,                      !- Maximum Value of x",
        "  0.1,                     !- Minimum Curve Output",
        "  1.;                      !- Maximum Curve Output",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, state->dataCurveManager->NumCurves);
    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    ASSERT_EQ(1, state->dataCurveManager->NumCurves);

    EXPECT_EQ(1.0, state->dataCurveManager->PerfCurve(1).CurveMax);
    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMaxPresent);

    EXPECT_EQ(0.1, state->dataCurveManager->PerfCurve(1).CurveMin);
    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMinPresent);
}

TEST_F(EnergyPlusFixture, QuadraticCurve)
{
    std::string const idf_objects = delimited_string({
        "Curve:QuadLinear,",
        "  MinDsnWBCurveName, ! Curve Name",
        "  -3.3333,           ! CoefficientC1",
        "  0.1,               ! CoefficientC2",
        "  38.9,              ! CoefficientC3",
        "  0.1,                ! CoefficientC4",
        "  0.5,                ! CoefficientC5",
        "  -30.,              ! Minimum Value of w",
        "  40.,               ! Maximum Value of w",
        "  0.,                ! Minimum Value of x",
        "  1.,                ! Maximum Value of x",
        "  5.,                ! Minimum Value of y",
        "  38.,               ! Maximum Value of y",
        "  0,                 ! Minimum Value of z",
        "  20,                ! Maximum Value of z",
        "  0.,                ! Minimum Curve Output",
        "  38.;               ! Maximum Curve Output",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, state->dataCurveManager->NumCurves);
    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    ASSERT_EQ(1, state->dataCurveManager->NumCurves);

    EXPECT_EQ(38.0, state->dataCurveManager->PerfCurve(1).CurveMax);
    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMaxPresent);

    EXPECT_EQ(0., state->dataCurveManager->PerfCurve(1).CurveMin);
    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMinPresent);

    double var1 = 1, var2 = 0.1, var3 = 20, var4 = 10;
    double expected_value = -3.3333 + (0.1 * 1) + (38.9 * 0.1) + (0.1 * 20) + (0.5 * 10);
    EXPECT_EQ(expected_value, CurveManager::CurveValue(*state, 1, var1, var2, var3, var4));
}

TEST_F(EnergyPlusFixture, QuintLinearCurve)
{
    std::string const idf_objects = delimited_string({
        "Curve:QuintLinear,",
        "  MinDsnWBCurveName, ! Curve Name",
        "  -3.3333,           ! CoefficientC1",
        "  0.1,               ! CoefficientC2",
        "  38.9,              ! CoefficientC3",
        "  0.1,                ! CoefficientC4",
        "  0.5,                ! CoefficientC5",
        "  1.5,                ! CoefficientC6",
        "  0.,                ! Minimum Value of v",
        "  10.,               ! Maximum Value of v",
        "  -30.,              ! Minimum Value of w",
        "  40.,               ! Maximum Value of w",
        "  0.,                ! Minimum Value of x",
        "  1.,                ! Maximum Value of x",
        "  5.,                ! Minimum Value of y",
        "  38.,               ! Maximum Value of y",
        "  0,                 ! Minimum Value of z",
        "  20,                ! Maximum Value of z",
        "  0.,                ! Minimum Curve Output",
        "  38.;               ! Maximum Curve Output",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, state->dataCurveManager->NumCurves);
    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    ASSERT_EQ(1, state->dataCurveManager->NumCurves);

    EXPECT_EQ(38.0, state->dataCurveManager->PerfCurve(1).CurveMax);
    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMaxPresent);

    EXPECT_EQ(0., state->dataCurveManager->PerfCurve(1).CurveMin);
    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMinPresent);

    double var1 = 1, var2 = 0.1, var3 = 0.5, var4 = 10, var5 = 15;
    double expected_value = -3.3333 + (0.1 * 1) + (38.9 * 0.1) + (0.1 * 0.5) + (0.5 * 10) + (1.5 * 15);
    EXPECT_EQ(expected_value, CurveManager::CurveValue(*state, 1, var1, var2, var3, var4, var5));
}

TEST_F(EnergyPlusFixture, TableLookup)
{
    std::string const idf_objects = delimited_string({"Table:IndependentVariable,",
                                                      "  SAFlow,                    !- Name",
                                                      "  Cubic,                     !- Interpolation Method",
                                                      "  Constant,                  !- Extrapolation Method",
                                                      "  0.714,                     !- Minimum Value",
                                                      "  1.2857,                    !- Maximum Value",
                                                      "  ,                          !- Normalization Reference Value",
                                                      "  Dimensionless,             !- Unit Type",
                                                      "  ,                          !- External File Name",
                                                      "  ,                          !- External File Column Number",
                                                      "  ,                          !- External File Starting Row Number",
                                                      "  0.714286,                  !- Value 1",
                                                      "  1.0,",
                                                      "  1.2857;",
                                                      "Table:IndependentVariableList,",
                                                      "  SAFlow_Variables,          !- Name",
                                                      "  SAFlow;                    !- Independent Variable 1 Name",
                                                      "Table:Lookup,",
                                                      "  CoolCapModFuncOfSAFlow,    !- Name",
                                                      "  SAFlow_Variables,          !- Independent Variable List Name",
                                                      "  ,                          !- Normalization Method",
                                                      "  ,                          !- Normalization Divisor",
                                                      "  0.8234,                    !- Minimum Output",
                                                      "  1.1256,                    !- Maximum Output",
                                                      "  Dimensionless,             !- Output Unit Type",
                                                      "  ,                          !- External File Name",
                                                      "  ,                          !- External File Column Number",
                                                      "  ,                          !- External File Starting Row Number",
                                                      "  0.823403,                  !- Output Value 1",
                                                      "  1.0,",
                                                      "  1.1256;",
                                                      "Table:Lookup,",
                                                      "  HeatCapModFuncOfSAFlow,    !- Name",
                                                      "  SAFlow_Variables,          !- Independent Variable List Name",
                                                      "  ,                          !- Normalization Method",
                                                      "  ,                          !- Normalization Divisor",
                                                      "  0.8554,                    !- Minimum Output",
                                                      "  1.0778,                    !- Maximum Output",
                                                      "  Dimensionless,             !- Output Unit Type",
                                                      "  ,                          !- External File Name",
                                                      "  ,                          !- External File Column Number",
                                                      "  ,                          !- External File Starting Row Number",
                                                      "  0.8554,                    !- Output Value 1",
                                                      "  1.0,",
                                                      "  1.0778;",
                                                      "Table:IndependentVariable,",
                                                      "  WaterFlow,                 !- Name",
                                                      "  Cubic,                     !- Interpolation Method",
                                                      "  Constant,                  !- Extrapolation Method",
                                                      "  0.0,                       !- Minimum Value",
                                                      "  1.333333,                  !- Maximum Value",
                                                      "  ,                          !- Normalization Reference Value",
                                                      "  Dimensionless,             !- Unit Type",
                                                      "  ,                          !- External File Name",
                                                      "  ,                          !- External File Column Number",
                                                      "  ,                          !- External File Starting Row Number",
                                                      "  0.0,                       !- Value 1,",
                                                      "  0.05,",
                                                      "  0.33333,",
                                                      "  0.5,",
                                                      "  0.666667,",
                                                      "  0.833333,",
                                                      "  1.0,",
                                                      "  1.333333;",
                                                      "Table:IndependentVariableList,",
                                                      "  WaterFlow_Variables,       !- Name",
                                                      "  WaterFlow;                 !- Independent Variable 1 Name",
                                                      "Table:Lookup,",
                                                      "  CapModFuncOfWaterFlow,     !- Name",
                                                      "  WaterFlow_Variables,       !- Independent Variable List Name",
                                                      "  ,                          !- Normalization Method",
                                                      "  ,                          !- Normalization Divisor",
                                                      "  0.0,                       !- Minimum Output",
                                                      "  1.04,                      !- Maximum Output",
                                                      "  Dimensionless,             !- Output Unit Type",
                                                      "  ,                          !- External File Name",
                                                      "  ,                          !- External File Column Number",
                                                      "  ,                          !- External File Starting Row Number",
                                                      "  0.0,                       !- Output Value 1",
                                                      "  0.001,",
                                                      "  0.71,",
                                                      "  0.85,",
                                                      "  0.92,",
                                                      "  0.97,",
                                                      "  1.0,",
                                                      "  1.04;"});

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, state->dataCurveManager->NumCurves);

    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    ASSERT_EQ(3, state->dataCurveManager->NumCurves);

    EXPECT_EQ("Table:Lookup", state->dataCurveManager->PerfCurve(1).ObjectType);
    EXPECT_EQ("CAPMODFUNCOFWATERFLOW", state->dataCurveManager->PerfCurve(1).Name);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMinPresent);
    EXPECT_EQ(0.0, state->dataCurveManager->PerfCurve(1).CurveMin);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMaxPresent);
    EXPECT_EQ(1.04, state->dataCurveManager->PerfCurve(1).CurveMax);
}

TEST_F(EnergyPlusFixture, DivisorNormalizationNone)
{
    /*
     *  Test: Normalization Method = None
     *
     *  The Table:Lookup object constructed in this test corresponds directly to the function:
     *      f(x_1, x_2) = x_1 * x_2
     *
     *  The curve of this data is therefore Linear, making interpolated data points easily calculated
     *  This idf will default to Cubic interpolation and Linear extrapolation
     */

    double expected_curve_min{2.0};
    double expected_curve_max{21.0};

    std::vector<std::pair<double, double>> table_data{
        {2.0, 1.0}, // 2.0
        {2.0, 2.0}, // 4.0
        {2.0, 3.0}, // 6.0
        {7.0, 1.0}, // 7.0
        {7.0, 2.0}, // 14.0
        {7.0, 3.0}, // 21.0
        {3.0, 3.0}, // 9.0
        {5.0, 2.0}, // 10.0
    };

    std::string const idf_objects = delimited_string({
        "Table:Lookup,",
        "y_values,                              !- Name",
        "y_values_list,                         !- Independent Variable List Name",
        ",                                      !- Normalization Method",
        ",                                      !- Normalization Divisor",
        "2.0,                                   !- Minimum Output",
        "21.0,                                  !- Maximum Output",
        "Dimensionless,                         !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        "2.0,                                   !- Value 1",
        "4.0,                                   !- Value 2",
        "6.0,                                   !- Value 3",
        "7.0,                                   !- Value 4",
        "14.0,                                  !- Value 5",
        "21.0;                                  !- Value 6",

        "Table:IndependentVariableList,",
        "y_values_list,                         !- Name",
        "x_values_1,                            !- Independent Variable Name 1",
        "x_values_2;                            !- Independent Variable Name 2",

        "Table:IndependentVariable,",
        "x_values_1,                            !- Name",
        ",                                      !- Interpolation Method",
        ",                                      !- Extrapolation Method",
        ",                                      !- Minimum value",
        ",                                      !- Maximum value",
        ",                                      !- Normalization Reference Value",
        "Dimensionless                          !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        ",",
        "2.0,                                   !- Value 1",
        "7.0;                                   !- Value 1",

        "Table:IndependentVariable,",
        "x_values_2,                            !- Name",
        "Linear,                                !- Interpolation Method",
        "Linear,                                !- Extrapolation Method",
        ",                                      !- Minimum value",
        ",                                      !- Maximum value",
        ",                                      !- Normalization Reference Value",
        "Dimensionless                          !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        ",",
        "1.0,                                   !- Value 1",
        "2.0,                                   !- Value 2",
        "3.0;                                   !- Value 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, state->dataCurveManager->NumCurves);

    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    ASSERT_EQ(1, state->dataCurveManager->NumCurves);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMinPresent);
    EXPECT_EQ(expected_curve_min, state->dataCurveManager->PerfCurve(1).CurveMin);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMaxPresent);
    EXPECT_EQ(expected_curve_max, state->dataCurveManager->PerfCurve(1).CurveMax);

    for (auto data_point : table_data) {
        EXPECT_DOUBLE_EQ(data_point.first * data_point.second, CurveManager::CurveValue(*state, 1, data_point.first, data_point.second));
    }
}

TEST_F(EnergyPlusFixture, DivisorNormalizationDivisorOnly)
{
    /*
     *  Test: Normalization Method = DivisorOnly
     *
     *  The Table:Lookup object constructed in this test corresponds directly to the function:
     *      f(x_1, x_2) = x_1 * x_2
     *
     *  The curve of this data is therefore Linear, making interpolated data points easily calculated
     *  This idf will default to Cubic interpolation and Linear extrapolation
     */

    double expected_divisor{3.0};
    double expected_curve_min{2.0 / expected_divisor};
    double expected_curve_max{21.0 / expected_divisor};

    std::vector<std::pair<double, double>> table_data{
        {2.0, 1.0}, // 2.0
        {2.0, 2.0}, // 4.0
        {2.0, 3.0}, // 6.0
        {7.0, 1.0}, // 7.0
        {7.0, 2.0}, // 14.0
        {7.0, 3.0}, // 21.0
        {3.0, 3.0}, // 9.0
        {5.0, 2.0}, // 10.0
    };

    std::string const idf_objects = delimited_string({
        "Table:Lookup,",
        "y_values,                              !- Name",
        "y_values_list,                         !- Independent Variable List Name",
        "DivisorOnly,                           !- Normalization Method",
        "3.0,                                   !- Normalization Divisor",
        "2.0,                                   !- Minimum Output",
        "21.0,                                  !- Maximum Output",
        "Dimensionless,                         !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        "2.0,                                   !- Value 1",
        "4.0,                                   !- Value 2",
        "6.0,                                   !- Value 3",
        "7.0,                                   !- Value 4",
        "14.0,                                  !- Value 5",
        "21.0;                                  !- Value 6",

        "Table:IndependentVariableList,",
        "y_values_list,                         !- Name",
        "x_values_1,                            !- Independent Variable Name 1",
        "x_values_2;                            !- Independent Variable Name 2",

        "Table:IndependentVariable,",
        "x_values_1,                            !- Name",
        ",                                      !- Interpolation Method",
        ",                                      !- Extrapolation Method",
        ",                                      !- Minimum value",
        ",                                      !- Maximum value",
        ",                                      !- Normalization Reference Value",
        "Dimensionless                          !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        ",",
        "2.0,                                   !- Value 1",
        "7.0;                                   !- Value 1",

        "Table:IndependentVariable,",
        "x_values_2,                            !- Name",
        "Linear,                                !- Interpolation Method",
        "Linear,                                !- Extrapolation Method",
        ",                                      !- Minimum value",
        ",                                      !- Maximum value",
        ",                                      !- Normalization Reference Value",
        "Dimensionless                          !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        ",",
        "1.0,                                   !- Value 1",
        "2.0,                                   !- Value 2",
        "3.0;                                   !- Value 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, state->dataCurveManager->NumCurves);

    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    ASSERT_EQ(1, state->dataCurveManager->NumCurves);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMinPresent);
    EXPECT_EQ(expected_curve_min, state->dataCurveManager->PerfCurve(1).CurveMin);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMaxPresent);
    EXPECT_EQ(expected_curve_max, state->dataCurveManager->PerfCurve(1).CurveMax);

    for (auto data_point : table_data) {
        EXPECT_DOUBLE_EQ(data_point.first * data_point.second / expected_divisor,
                         CurveManager::CurveValue(*state, 1, data_point.first, data_point.second));
    }
}

TEST_F(EnergyPlusFixture, DivisorNormalizationAutomaticWithDivisor)
{
    /*
     *  Test: Normalization Method = AutomaticWithDivisor
     *      Case: Default 'Normalization Divisor' Field
     *
     *  The Table:Lookup object constructed in this test corresponds directly to the function:
     *      f(x_1, x_2) = x_1 * x_2
     *
     *  The curve of this data is therefore Linear, making interpolated data points easily calculated
     *  This idf will default to Cubic interpolation and Linear extrapolation
     */

    double expected_auto_divisor{6.0};
    double expected_curve_max{21.0 / expected_auto_divisor};
    double expected_curve_min{2.0 / expected_auto_divisor};

    std::vector<std::pair<double, double>> table_data{
        {2.0, 1.0}, // 2.0
        {2.0, 2.0}, // 4.0
        {2.0, 3.0}, // 6.0
        {7.0, 1.0}, // 7.0
        {7.0, 2.0}, // 14.0
        {7.0, 3.0}, // 21.0
        {3.0, 3.0}, // 9.0
        {5.0, 2.0}, // 10.0
    };

    std::string const idf_objects = delimited_string({
        "Table:Lookup,",
        "y_values,                              !- Name",
        "y_values_list,                         !- Independent Variable List Name",
        "AutomaticWithDivisor,                  !- Normalization Method",
        ",                                      !- Normalization Divisor",
        "2.0,                                   !- Minimum Output",
        "21.0,                                  !- Maximum Output",
        "Dimensionless,                         !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        "2.0,                                   !- Value 1",
        "4.0,                                   !- Value 2",
        "6.0,                                   !- Value 3",
        "7.0,                                   !- Value 4",
        "14.0,                                  !- Value 5",
        "21.0;                                  !- Value 6",

        "Table:IndependentVariableList,",
        "y_values_list,                         !- Name",
        "x_values_1,                            !- Independent Variable Name 1",
        "x_values_2;                            !- Independent Variable Name 2",

        "Table:IndependentVariable,",
        "x_values_1,                            !- Name",
        ",                                      !- Interpolation Method",
        ",                                      !- Extrapolation Method",
        ",                                      !- Minimum value",
        ",                                      !- Maximum value",
        "3.0,                                   !- Normalization Reference Value",
        "Dimensionless                          !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        ",",
        "2.0,                                   !- Value 1",
        "7.0;                                   !- Value 1",

        "Table:IndependentVariable,",
        "x_values_2,                            !- Name",
        ",                                      !- Interpolation Method",
        ",                                      !- Extrapolation Method",
        ",                                      !- Minimum value",
        ",                                      !- Maximum value",
        "2.0,                                   !- Normalization Reference Value",
        "Dimensionless                          !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        ",",
        "1.0,                                   !- Value 1",
        "2.0,                                   !- Value 2",
        "3.0;                                   !- Value 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, state->dataCurveManager->NumCurves);

    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    ASSERT_EQ(1, state->dataCurveManager->NumCurves);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMinPresent);
    EXPECT_EQ(expected_curve_min, state->dataCurveManager->PerfCurve(1).CurveMin);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMaxPresent);
    EXPECT_EQ(expected_curve_max, state->dataCurveManager->PerfCurve(1).CurveMax);

    for (auto data_point : table_data) {
        EXPECT_DOUBLE_EQ(data_point.first * data_point.second / expected_auto_divisor,
                         CurveManager::CurveValue(*state, 1, data_point.first, data_point.second));
    }
}

TEST_F(EnergyPlusFixture, NormalizationAutomaticWithDivisorAndSpecifiedDivisor)
{
    /*
     *  Test: Normalization Method = AutomaticWithDivisor
     *      Case: Additionally Specified Divisor in 'Normalization Divisor' Field
     *
     *  The Table:Lookup object constructed in this test corresponds directly to the function:
     *      f(x_1, x_2) = x_1 * x_2
     *
     *  The curve of this data is therefore Linear, making interpolated data points easily calculated
     *  This idf will default to Cubic interpolation and Linear extrapolation
     */

    double expected_auto_divisor{6.0};
    double normalization_divisor{4.0};
    double expected_curve_max{21.0 / expected_auto_divisor / normalization_divisor};
    double expected_curve_min{2.0 / expected_auto_divisor / normalization_divisor};

    std::vector<std::pair<double, double>> table_data{
        {2.0, 1.0}, // 2.0
        {2.0, 2.0}, // 4.0
        {2.0, 3.0}, // 6.0
        {7.0, 1.0}, // 7.0
        {7.0, 2.0}, // 14.0
        {7.0, 3.0}, // 21.0
        {3.0, 3.0}, // 9.0
        {5.0, 2.0}, // 10.0
    };

    std::string const idf_objects = delimited_string({
        "Table:Lookup,",
        "y_values,                              !- Name",
        "y_values_list,                         !- Independent Variable List Name",
        "AutomaticWithDivisor,                  !- Normalization Method",
        "4.0,                                   !- Normalization Divisor",
        "2.0,                                   !- Minimum Output",
        "21.0,                                  !- Maximum Output",
        "Dimensionless,                         !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        "2.0,                                   !- Value 1",
        "4.0,                                   !- Value 2",
        "6.0,                                   !- Value 3",
        "7.0,                                   !- Value 4",
        "14.0,                                  !- Value 5",
        "21.0;                                  !- Value 6",

        "Table:IndependentVariableList,",
        "y_values_list,                         !- Name",
        "x_values_1,                            !- Independent Variable Name 1",
        "x_values_2;                            !- Independent Variable Name 2",

        "Table:IndependentVariable,",
        "x_values_1,                            !- Name",
        ",                                      !- Interpolation Method",
        ",                                      !- Extrapolation Method",
        ",                                      !- Minimum value",
        ",                                      !- Maximum value",
        "3.0,                                   !- Normalization Reference Value",
        "Dimensionless                          !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        ",",
        "2.0,                                   !- Value 1",
        "7.0;                                   !- Value 1",

        "Table:IndependentVariable,",
        "x_values_2,                            !- Name",
        ",                                      !- Interpolation Method",
        ",                                      !- Extrapolation Method",
        ",                                      !- Minimum value",
        ",                                      !- Maximum value",
        "2.0,                                   !- Normalization Reference Value",
        "Dimensionless                          !- Output Unit Type",
        ",                                      !- External File Name",
        ",                                      !- External File Column Number",
        ",                                      !- External File Starting Row Number",
        ",",
        "1.0,                                   !- Value 1",
        "2.0,                                   !- Value 2",
        "3.0;                                   !- Value 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_EQ(0, state->dataCurveManager->NumCurves);

    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    ASSERT_EQ(1, state->dataCurveManager->NumCurves);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMinPresent);
    EXPECT_EQ(expected_curve_min, state->dataCurveManager->PerfCurve(1).CurveMin);

    EXPECT_TRUE(state->dataCurveManager->PerfCurve(1).CurveMaxPresent);
    EXPECT_EQ(expected_curve_max, state->dataCurveManager->PerfCurve(1).CurveMax);

    for (auto data_point : table_data) {
        EXPECT_DOUBLE_EQ(data_point.first * data_point.second / expected_auto_divisor / normalization_divisor,
                         CurveManager::CurveValue(*state, 1, data_point.first, data_point.second));
    }
}

TEST_F(EnergyPlusFixture, CSV_CarriageReturns_Handling)
{
    CurveManager::TableFile testTableFile = CurveManager::TableFile();
    fs::path testCSV = configured_source_directory() / "tst/EnergyPlus/unit/Resources/TestCarriageReturn.csv";
    testTableFile.filePath = testCSV;
    testTableFile.load(*state, testCSV);
    std::vector<double> TestArray;
    std::size_t col = 2;
    std::size_t row = 1;
    std::size_t expected_length = 168;
    TestArray = testTableFile.getArray(*state, std::make_pair(col, row));
    EXPECT_EQ(TestArray.size(), expected_length);

    for (std::size_t i = 0; i < TestArray.size(); i++) {
        EXPECT_FALSE(std::isnan(TestArray[i]));
    }
}
