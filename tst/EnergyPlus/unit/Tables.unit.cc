// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// EnergyPlus::Curves Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
//#include "EnergyPlus/Curves.hh"
#include "EnergyPlus/CurveManager.hh"

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_Linear)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable1,              !- Name",
                                                      "Linear,                  !- Curve Type",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable2,              !- Name",
                                                      "Linear,                  !- Curve Type",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      "0.5,                     !- Minimum Table Output",
                                                      "1.0,                     !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable3,              !- Name",
                                                      "Quadratic,               !- Curve Type",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                     !- Minimum Table Output",
                                                      ",                     !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable4,              !- Name",
                                                      "Cubic,                   !- Curve Type",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable5,              !- Name",
                                                      "Quartic,                 !- Curve Type",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable6,              !- Name",
                                                      "Exponent,                !- Curve Type",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable7,              !- Name",
                                                      "Exponent,                !- Curve Type",
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
                                                      "2,                       !- Output Value #2",
                                                      "2,                       !- X Value #3",
                                                      "2,                       !- Output Value #3",
                                                      "3,                       !- X Value #4",
                                                      "3,                       !- Output Value #4",
                                                      "4,                       !- X Value #5",
                                                      "4,                       !- Output Value #5",
                                                      "5,                      !- X Value #6",
                                                      "2;                       !- Output Value #6"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(7, CurveManager::NumCurves);

    // Linear curve type
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5)); // Value too large

    // Linear curve type with min and max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0));   // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 5));   // Value too large
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 0.5)); // In-range value

    // Quadratic curve type
    EXPECT_EQ("QUADRATIC", CurveManager::GetCurveType(3));
    EXPECT_EQ("TESTTABLE3", CurveManager::GetCurveName(3));
    EXPECT_EQ(3, CurveManager::GetCurveIndex("TESTTABLE3"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE3", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(3, index);
    CurveManager::GetCurveMinMaxValues(3, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(3));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(3, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(3, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 5)); // Value too large

    // Cubic curve type
    EXPECT_EQ("CUBIC", CurveManager::GetCurveType(4));
    EXPECT_EQ("TESTTABLE4", CurveManager::GetCurveName(4));
    EXPECT_EQ(4, CurveManager::GetCurveIndex("TESTTABLE4"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE4", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(4, index);
    CurveManager::GetCurveMinMaxValues(4, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(4));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(4, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(4, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 5)); // Value too large

    // Quartic curve type
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(5));
    EXPECT_EQ("TESTTABLE5", CurveManager::GetCurveName(5));
    EXPECT_EQ(5, CurveManager::GetCurveIndex("TESTTABLE5"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE5", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(5, index);
    CurveManager::GetCurveMinMaxValues(5, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(5));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(5, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(5, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(5, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 5)); // Value too large

    // Exponent curve type
    EXPECT_EQ("EXPONENT", CurveManager::GetCurveType(6));
    EXPECT_EQ("TESTTABLE6", CurveManager::GetCurveName(6));
    EXPECT_EQ(6, CurveManager::GetCurveIndex("TESTTABLE6"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE6", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(6, index);
    CurveManager::GetCurveMinMaxValues(6, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(6));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(6, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(6, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(6, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 5)); // Value too large

    // Exponent curve type without IV min/max
    EXPECT_EQ("EXPONENT", CurveManager::GetCurveType(7));
    EXPECT_EQ("TESTTABLE7", CurveManager::GetCurveName(7));
    EXPECT_EQ(7, CurveManager::GetCurveIndex("TESTTABLE7"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE7", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(7, index);
    // CurveManager::GetCurveMinMaxValues(7, min, max);
    // EXPECT_EQ(0, min);
    // EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(7));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(7, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(7, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(7, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(7, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(7, 5)); // Value too large

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_Lagrange)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable1,              !- Name",
                                                      "Linear,                  !- Curve Type",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable2,              !- Name",
                                                      "Linear,                  !- Curve Type",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      "0.5,                     !- Minimum Table Output",
                                                      "1.0,                     !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable3,              !- Name",
                                                      "Quadratic,               !- Curve Type",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
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
                                                      "2,                       !- Output Value #2",
                                                      "2,                       !- X Value #3",
                                                      "2,                       !- Output Value #3",
                                                      "3,                       !- X Value #4",
                                                      "3,                       !- Output Value #4",
                                                      "4,                       !- X Value #5",
                                                      "4,                       !- Output Value #5",
                                                      "5,                       !- X Value #6",
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable4,              !- Name",
                                                      "Quadratic,               !- Curve Type",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "-1,                      !- Minimum Value of X",
                                                      "10,                      !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable5,              !- Name",
                                                      "Cubic,                   !- Curve Type",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable6,              !- Name",
                                                      "Quartic,                 !- Curve Type",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable7,              !- Name",
                                                      "Exponent,                !- Curve Type",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(7, CurveManager::NumCurves);

    // Linear curve type
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5)); // Value too large

    // Linear curve type with min and max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0));   // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 5));   // Value too large
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 0.5)); // In-range value

    // Quadratic curve type without min or max
    EXPECT_EQ("QUADRATIC", CurveManager::GetCurveType(3));
    EXPECT_EQ("TESTTABLE3", CurveManager::GetCurveName(3));
    EXPECT_EQ(3, CurveManager::GetCurveIndex("TESTTABLE3"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE3", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(3, index);
    CurveManager::GetCurveMinMaxValues(3, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(3));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, 0));       // In-range value
    EXPECT_DOUBLE_EQ(1.25, CurveManager::CurveValue(3, 0.5));    // In-range value
    EXPECT_DOUBLE_EQ(1.6875, CurveManager::CurveValue(3, 0.75)); // In-range value
    EXPECT_DOUBLE_EQ(2.055, CurveManager::CurveValue(3, 2.1));   // In-range value
    EXPECT_DOUBLE_EQ(2.855, CurveManager::CurveValue(3, 2.9));   // In-range value
    EXPECT_DOUBLE_EQ(3.9, CurveManager::CurveValue(3, 3.9));     // In-range value
    EXPECT_DOUBLE_EQ(3.84, CurveManager::CurveValue(3, 4.2));    // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, -10.0));   // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(3, 5000));    // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(3, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 5)); // Value too large

    // Quadratic curve type with extrapolation
    EXPECT_EQ("QUADRATIC", CurveManager::GetCurveType(4));
    EXPECT_EQ("TESTTABLE4", CurveManager::GetCurveName(4));
    EXPECT_EQ(4, CurveManager::GetCurveIndex("TESTTABLE4"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE4", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(4, index);
    CurveManager::GetCurveMinMaxValues(4, min, max);
    EXPECT_EQ(-1, min);
    EXPECT_EQ(10, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(4));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 0));       // In-range value
    EXPECT_DOUBLE_EQ(1.25, CurveManager::CurveValue(4, 0.5));    // In-range value
    EXPECT_DOUBLE_EQ(1.6875, CurveManager::CurveValue(4, 0.75)); // In-range value
    EXPECT_DOUBLE_EQ(2.055, CurveManager::CurveValue(4, 2.1));   // In-range value
    EXPECT_DOUBLE_EQ(2.855, CurveManager::CurveValue(4, 2.9));   // In-range value
    EXPECT_DOUBLE_EQ(3.9, CurveManager::CurveValue(4, 3.9));     // In-range value
    EXPECT_DOUBLE_EQ(3.84, CurveManager::CurveValue(4, 4.2));    // In-range value
    EXPECT_DOUBLE_EQ(-1.0, CurveManager::CurveValue(4, -0.5));   // Extrapolated value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 6));       // Extrapolated value
    EXPECT_DOUBLE_EQ(-2.0, CurveManager::CurveValue(4, -10.0));  // Minimum x
    EXPECT_DOUBLE_EQ(-8.0, CurveManager::CurveValue(4, 5000));   // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(4, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 5)); // Value too large

    // Cubic curve type
    EXPECT_EQ("CUBIC", CurveManager::GetCurveType(5));
    EXPECT_EQ("TESTTABLE5", CurveManager::GetCurveName(5));
    EXPECT_EQ(5, CurveManager::GetCurveIndex("TESTTABLE5"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE5", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(5, index);
    CurveManager::GetCurveMinMaxValues(5, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(5));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, 0));      // In-range value
    EXPECT_DOUBLE_EQ(1.4375, CurveManager::CurveValue(5, 0.5)); // In-range value
    EXPECT_DOUBLE_EQ(2.4375, CurveManager::CurveValue(5, 2.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, -10.0));  // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(5, 5000));   // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(5, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(5, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 5)); // Value too large

    // Quartic curve type
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(6));
    EXPECT_EQ("TESTTABLE6", CurveManager::GetCurveName(6));
    EXPECT_EQ(6, CurveManager::GetCurveIndex("TESTTABLE6"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE6", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(6, index);
    CurveManager::GetCurveMinMaxValues(6, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(6));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, 0));       // In-range value
    EXPECT_DOUBLE_EQ(1.59375, CurveManager::CurveValue(6, 0.5)); // In-range value
    EXPECT_DOUBLE_EQ(3.7472, CurveManager::CurveValue(6, 3.6));  // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, -10.0));   // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(6, 5000));    // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(6, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(6, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 5)); // Value too large

    // Exponent curve type
    EXPECT_EQ("EXPONENT", CurveManager::GetCurveType(7));
    EXPECT_EQ("TESTTABLE7", CurveManager::GetCurveName(7));
    EXPECT_EQ(7, CurveManager::GetCurveIndex("TESTTABLE7"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE7", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(7, index);
    CurveManager::GetCurveMinMaxValues(7, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(7));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, 0));      // In-range value
    EXPECT_DOUBLE_EQ(1.4375, CurveManager::CurveValue(7, 0.5)); // In-range value
    EXPECT_DOUBLE_EQ(2.4375, CurveManager::CurveValue(7, 2.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, -10.0));  // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(7, 5000));   // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(7, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(7, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(7, 5)); // Value too large

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_MinMaxFailure)
{

    std::string const idf_objects = delimited_string(
        {"Version,8.5;", "Table:OneIndependentVariable,", "TestTableMinMax,         !- Name", "Linear,                  !- Curve Type",
         "LinearInterpolationOfTable,  !- Interpolation Method", ",                        !- Minimum Value of X",
         ",                        !- Maximum Value of X", ",                        !- Minimum Table Output",
         ",                        !- Maximum Table Output", "Dimensionless,           !- Input Unit Type for X",
         "Dimensionless,           !- Output Unit Type", ",                        !- Normalization Reference",
         "0,                       !- X Value #1", "0,                       !- Output Value #1", "1,                       !- X Value #2",
         "1;                       !- Output Value #2"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(1, CurveManager::NumCurves);

    // Super-simple table without IV min/max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLEMINMAX", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLEMINMAX"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLEMINMAX", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    // Real64 min, max;
    // CurveManager::GetCurveMinMaxValues(1, min, max);
    // EXPECT_EQ(0, min);
    // EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(0.75, CurveManager::CurveValue(1, 0.75)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5000));  // Maximum x

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_Linear_BadMinMax)
{

    std::string const idf_objects = delimited_string(
        {"Version,8.5;", "Table:OneIndependentVariable,", "TestTableMinMax,         !- Name", "Linear,                  !- Curve Type",
         "LinearInterpolationOfTable,  !- Interpolation Method", "-1,                      !- Minimum Value of X",
         "6,                       !- Maximum Value of X", ",                        !- Minimum Table Output",
         ",                        !- Maximum Table Output", "Dimensionless,           !- Input Unit Type for X",
         "Dimensionless,           !- Output Unit Type", ",                        !- Normalization Reference",
         "0,                       !- X Value #1", "0,                       !- Output Value #1", "1,                       !- X Value #2",
         "1;                       !- Output Value #2"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(1, CurveManager::NumCurves);

    // Super-simple table without IV min/max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLEMINMAX", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLEMINMAX"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLEMINMAX", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(1, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -0.25)); // Not really in-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5.5));   // Not really in-range value
    EXPECT_DOUBLE_EQ(0.75, CurveManager::CurveValue(1, 0.75)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5000));  // Maximum x

    std::string const error_string = delimited_string({"   ** Warning ** GetCurveInput: For Table:OneIndependentVariable: TESTTABLEMINMAX",
                                                       "   **   ~~~   ** Minimum Value of X exceeds the data range and will not be used.",
                                                       "   **   ~~~   **  Entered value = -1.000000, Minimum data range = 0.000000",
                                                       "   ** Warning ** GetCurveInput: For Table:OneIndependentVariable: TESTTABLEMINMAX",
                                                       "   **   ~~~   ** Maximum Value of X exceeds the data range and will not be used.",
                                                       "   **   ~~~   **  Entered value = 6.000000, Maximum data range = 1.000000"});

    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_EvaluateToLimits)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable1,              !- Name",
                                                      "Linear,                  !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable2,              !- Name",
                                                      "Linear,                  !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      "0.96,                    !- Minimum Table Output",
                                                      "1.5,                     !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable3,              !- Name",
                                                      "Quadratic,               !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable4,              !- Name",
                                                      "Cubic,                   !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable5,              !- Name",
                                                      "Quartic,                 !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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
                                                      "2;                       !- Output Value #6",
                                                      "Table:OneIndependentVariable,",
                                                      "TestTable6,              !- Name",
                                                      "Quartic,                 !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "10,                      !- Maximum Value of X",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
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

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(6, CurveManager::NumCurves);

    // Linear curve type
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.9523809523809523, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.438095238095238, CurveManager::CurveValue(1, 1.0));    // In-range value
    EXPECT_DOUBLE_EQ(0.9523809523809523, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(3.38095238095238, CurveManager::CurveValue(1, 5000));    // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.96, 1.5);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.96, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(1, 5));  // Value too large

    // Linear curve type with min and max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(0.96, CurveManager::CurveValue(2, 0));                // Value too small
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(2, 5));                 // Value too large
    EXPECT_DOUBLE_EQ(1.438095238095238, CurveManager::CurveValue(2, 1.0)); // In-range value

    // Quadratic curve type
    EXPECT_EQ("QUADRATIC", CurveManager::GetCurveType(3));
    EXPECT_EQ("TESTTABLE3", CurveManager::GetCurveName(3));
    EXPECT_EQ(3, CurveManager::GetCurveIndex("TESTTABLE3"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE3", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(3, index);
    CurveManager::GetCurveMinMaxValues(3, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(3));

    EXPECT_NEAR(0.0, CurveManager::CurveValue(3, 0), 1.0e-14);                  // In-range value
    EXPECT_DOUBLE_EQ(1.628571428571429, CurveManager::CurveValue(3, 1));        // In-range value
    EXPECT_NEAR(0.0, CurveManager::CurveValue(3, -10.0), 1.0e-14);              // Minimum x
    EXPECT_NEAR(2.428571428571427, CurveManager::CurveValue(3, 5000), 1.0e-14); // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(3, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 5)); // Value too large

    // Cubic curve type
    EXPECT_EQ("CUBIC", CurveManager::GetCurveType(4));
    EXPECT_EQ("TESTTABLE4", CurveManager::GetCurveName(4));
    EXPECT_EQ(4, CurveManager::GetCurveIndex("TESTTABLE4"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE4", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(4, index);
    CurveManager::GetCurveMinMaxValues(4, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(4));

    EXPECT_NEAR(0.2222222222222222, CurveManager::CurveValue(4, 0), 1.0e-14);     // In-range value
    EXPECT_NEAR(1.317460317460317, CurveManager::CurveValue(4, 1), 1.0e-14);      // In-range value
    EXPECT_NEAR(0.2222222222222222, CurveManager::CurveValue(4, -10.0), 1.0e-14); // Minimum x
    EXPECT_NEAR(2.206349206349206, CurveManager::CurveValue(4, 5000), 1.0e-14);   // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(4, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 5)); // Value too large

    // Quartic curve type
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(5));
    EXPECT_EQ("TESTTABLE5", CurveManager::GetCurveName(5));
    EXPECT_EQ(5, CurveManager::GetCurveIndex("TESTTABLE5"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE5", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(5, index);
    CurveManager::GetCurveMinMaxValues(5, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(5));

    EXPECT_NEAR(0.007936507936507936, CurveManager::CurveValue(5, 0), 1.0e-12);     // In-range value
    EXPECT_NEAR(1.96031746031746, CurveManager::CurveValue(5, 1.0), 1.0e-12);       // In-range value
    EXPECT_NEAR(0.007936507936507936, CurveManager::CurveValue(5, -10.0), 1.0e-12); // Minimum x
    EXPECT_NEAR(1.992063492063494, CurveManager::CurveValue(5, 5000), 1.0e-12);     // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(5, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(5, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 5)); // Value too large

    // Quartic curve type with extrapolation
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(6));
    EXPECT_EQ("TESTTABLE6", CurveManager::GetCurveName(6));
    EXPECT_EQ(6, CurveManager::GetCurveIndex("TESTTABLE6"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE6", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(6, index);
    CurveManager::GetCurveMinMaxValues(6, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(10, max);
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(6));

    EXPECT_NEAR(0.007936507936507936, CurveManager::CurveValue(6, 0), 1.0e-12);     // In-range value
    EXPECT_NEAR(1.96031746031746, CurveManager::CurveValue(6, 1.0), 1.0e-12);       // In-range value
    EXPECT_NEAR(0.007936507936507936, CurveManager::CurveValue(6, -10.0), 1.0e-12); // Minimum x
    EXPECT_NEAR(-386.3015873015872, CurveManager::CurveValue(6, 5000), 1.0e-09);    // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(6, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(6, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 5)); // Value too large

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_EvaluateToLimits_OverwriteLimits)
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

    ASSERT_FALSE(process_idf(idf_objects));

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
    EXPECT_EQ(0.1, min); // Fails
    EXPECT_EQ(4.9, max); // Fails
    EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(1.2, CurveManager::CurveValue(1, 0.5));               // Value should be too small, fails
    EXPECT_DOUBLE_EQ(2.5, CurveManager::CurveValue(1, 4));                 // Value should be too large, fails
    EXPECT_DOUBLE_EQ(1.438095238095238, CurveManager::CurveValue(1, 1.0)); // In-range value

    CurveManager::SetCurveOutputMinMaxValues(1, error, 1.2, 2.5);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(1.2, CurveManager::CurveValue(1, 0.5)); // Value too small
    EXPECT_DOUBLE_EQ(2.5, CurveManager::CurveValue(1, 4));   // Value too large

    EXPECT_FALSE(has_err_output());
}

/*
TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_EvaluateToLimits_Exponent) {

        std::string const idf_objects = delimited_string({
                "Version,8.5;",
                "Table:OneIndependentVariable,",
                "TestTable,              !- Name",
                "Exponent,                !- Curve Type",
                "EvaluateCurveToLimits,   !- Interpolation Method",
                "0,                       !- Minimum Value of X",
                "5,                       !- Maximum Value of X",
                ",                        !- Minimum Table Output",
                ",                        !- Maximum Table Output",
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
                "2;                       !- Output Value #6" });

        ASSERT_FALSE(process_idf(idf_objects));

        EXPECT_EQ(0, CurveManager::NumCurves);
        CurveManager::GetCurveInput();
        CurveManager::GetCurvesInputFlag = false;
        ASSERT_EQ(1, CurveManager::NumCurves);

        // Exponent curve type
        EXPECT_EQ("EXPONENT", CurveManager::GetCurveType(1));
        EXPECT_EQ("TESTTABLE", CurveManager::GetCurveName(1));
        EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE"));
        bool error = false;
        int index = CurveManager::GetCurveCheck("TESTTABLE", error, "TEST");
        EXPECT_FALSE(error);
        EXPECT_EQ(1, index);
        Real64 min, max;
        CurveManager::GetCurveMinMaxValues(1, min, max);
        EXPECT_EQ(0, min);
        EXPECT_EQ(5, max);
        EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

        EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0)); // In-range value
        EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5)); // In-range value
        EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
        EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000)); // Maximum x

        CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
        EXPECT_FALSE(error);
        EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0)); // Value too small
        EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5)); // Value too large

        EXPECT_FALSE(has_err_output());
}
*/

TEST_F(EnergyPlusFixture, Tables_TwoIndependentVariable_Linear)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:TwoIndependentVariables,",
                                                      "TestTable1,              !- Name",
                                                      "Biquadratic,             !- Curve Type",
                                                      "LinearInterpolationOfTable, !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      "1,                       !- Minimum Value of Y",
                                                      "2,                       !- Maximum Value of Y",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      ",                        !- Normalization Reference",
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
                                                      "TestTable2,              !- Name",
                                                      "Bicubic,                 !- Curve Type",
                                                      "LinearInterpolationOfTable, !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      "1,                       !- Minimum Value of Y",
                                                      "2,                       !- Maximum Value of Y",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      ",                        !- Normalization Reference",
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
                                                      "TestTable3,              !- Name",
                                                      "Triquadratic,            !- Curve Type",
                                                      "LinearInterpolationOfTable, !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      "1,                       !- Minimum Value of Y",
                                                      "2,                       !- Maximum Value of Y",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      ",                        !- Normalization Reference",
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
                                                      "TestTable4,              !- Name",
                                                      "QuadraticLinear,         !- Curve Type",
                                                      "LinearInterpolationOfTable, !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "5,                       !- Maximum Value of X",
                                                      "1,                       !- Minimum Value of Y",
                                                      "2,                       !- Maximum Value of Y",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      ",                        !- Normalization Reference",
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
                                                      "TestTable5,              !- Name",
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

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(5, CurveManager::NumCurves);

    // BiQuadratic curve type
    EXPECT_EQ("BIQUADRATIC", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(5, max1);
    EXPECT_EQ(1, min2);
    EXPECT_EQ(2, max2);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0));    // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000));     // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5)); // Value too large

    // BiCubic curve type
    EXPECT_EQ("BICUBIC", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(5, max1);
    EXPECT_EQ(1, min2);
    EXPECT_EQ(2, max2);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(2, 0, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 0.5, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(2, -10.0));    // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(2, 5000));     // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(2, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 5)); // Value too large

    // TriQuadratic curve type
    EXPECT_EQ("TRIQUADRATIC", CurveManager::GetCurveType(3));
    EXPECT_EQ("TESTTABLE3", CurveManager::GetCurveName(3));
    EXPECT_EQ(3, CurveManager::GetCurveIndex("TESTTABLE3"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE3", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(3, index);
    CurveManager::GetCurveMinMaxValues(3, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(5, max1);
    EXPECT_EQ(1, min2);
    EXPECT_EQ(2, max2);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(3));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, 0, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 0.5, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, -10.0));    // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(3, 5000));     // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(3, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 5)); // Value too large

    // QuadraticLinear curve type
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(4));
    EXPECT_EQ("TESTTABLE4", CurveManager::GetCurveName(4));
    EXPECT_EQ(4, CurveManager::GetCurveIndex("TESTTABLE4"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE4", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(4, index);
    CurveManager::GetCurveMinMaxValues(4, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(5, max1);
    EXPECT_EQ(1, min2);
    EXPECT_EQ(2, max2);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(4));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 0, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 0.5, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 0.5, 2.5)); // Not really an in-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, -10.0));    // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(4, 5000));     // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(4, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 5)); // Value too large

    // QuadraticLinear curve type, no min/max for y
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(5));
    EXPECT_EQ("TESTTABLE5", CurveManager::GetCurveName(5));
    EXPECT_EQ(5, CurveManager::GetCurveIndex("TESTTABLE5"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE5", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(5, index);
    CurveManager::GetCurveMinMaxValues(5, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(5, max1);
    EXPECT_EQ(1, min2);
    EXPECT_EQ(2, max2);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(5));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, 0, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 0.5, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, -10.0));    // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(5, 5000));     // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(5, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(5, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 5)); // Value too large

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_TwoIndependentVariable_Linear_GapTest)
{

    std::string const idf_objects = delimited_string(
        {"Version,8.5;", "Table:TwoIndependentVariables,", "TestTableMinMax,         !- Name", "QuadraticLinear,         !- Curve Type",
         "LinearInterpolationOfTable, !- Interpolation Method", ",                        !- Minimum Value of X",
         ",                        !- Maximum Value of X", ",                        !- Minimum Value of Y",
         ",                        !- Maximum Value of Y", ",                        !- Minimum Table Output",
         ",                        !- Maximum Table Output", "Dimensionless,           !- Input Unit Type for X",
         "Dimensionless,           !- Input Unit Type for Y", "Dimensionless,           !- Output Unit Type",
         ",                        !- Normalization Reference", "0,                       !- X Value #1", "1,                       !- Y Value #1",
         "0,                       !- Output Value #1", "1,                       !- X Value #2", "1,                       !- Y Value #2",
         "2,                       !- Output Value #2", "2,                       !- X Value #3", "1,                       !- Y Value #3",
         "2,                       !- Output Value #3", "3,                       !- X Value #4", "1,                       !- Y Value #4",
         "3,                       !- Output Value #4", "4,                       !- X Value #5", "1,                       !- Y Value #5",
         "4,                       !- Output Value #5", "5,                       !- X Value #6", "1,                       !- Y Value #6",
         "2,                       !- Output Value #6", "0,                       !- X Value #7", "2,                       !- Y Value #7",
         "0,                       !- Output Value #7", "1,                       !- X Value #8", "2,                       !- Y Value #8",
         "2,                       !- Output Value #8", "2,                       !- X Value #9", "2,                       !- Y Value #9",
         "2,                       !- Output Value #9",
         //"3,                       !- X Value #10",
         //"2,                       !- Y Value #10",
         //"3,                       !- Output Value #10",
         "4,                       !- X Value #11", "2,                       !- Y Value #11", "4,                       !- Output Value #11",
         "5,                       !- X Value #12", "2,                       !- Y Value #12", "2;                       !- Output Value #12"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(1, CurveManager::NumCurves);

    // QuadraticLinear curve type, no min/max for y
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLEMINMAX", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLEMINMAX"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLEMINMAX", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(5, max1);
    EXPECT_EQ(1, min2);
    EXPECT_EQ(2, max2);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5, 1.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0));    // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000));     // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0, 1.5)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5, 1.5)); // Value too large

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_TwoIndependentVariable_EvaluateToLimits)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:TwoIndependentVariables,",
                                                      "TestTable1,              !- Name",
                                                      "BiQuadratic,             !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "6,                       !- Maximum Value of X",
                                                      "0,                       !- Minimum Value of Y",
                                                      "4,                       !- Maximum Value of Y",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      ",                        !- Normalization Reference",
                                                      "0.0, 0.0, 0.0,           !- X Value, Y Value, Output Value #1",
                                                      "1.0, 1.0, 2.0,           !- X Value, Y Value, Output Value #2",
                                                      "2.0, 0.0, 2.0,           !- X Value, Y Value, Output Value #3",
                                                      "3.0, 1.0, 3.0,           !- X Value, Y Value, Output Value #4",
                                                      "4.0, 0.0, 4.0,           !- X Value, Y Value, Output Value #5",
                                                      "5.0, 2.0, 2.0,           !- X Value, Y Value, Output Value #6",
                                                      "5.0, 1.0, 1.0,           !- X Value, Y Value, Output Value #7",
                                                      "6.0, 1.0, 0.0,           !- X Value, Y Value, Output Value #8",
                                                      "5.0, 1.0, 1.5,           !- X Value, Y Value, Output Value #9",
                                                      "4.0, 1.0, 4.0;           !- X Value, Y Value, Output Value #10",
                                                      "Table:TwoIndependentVariables,",
                                                      "TestTable2,              !- Name",
                                                      "QuadraticLinear,         !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "0,                       !- Minimum Value of X",
                                                      "6,                       !- Maximum Value of X",
                                                      "0,                       !- Minimum Value of Y",
                                                      "4,                       !- Maximum Value of Y",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X",
                                                      "Dimensionless,           !- Input Unit Type for Y",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      ",                        !- Normalization Reference",
                                                      "0.0, 0.0, 0.0,           !- X Value, Y Value, Output Value #1",
                                                      "1.0, 1.0, 2.0,           !- X Value, Y Value, Output Value #2",
                                                      "2.0, 0.0, 2.0,           !- X Value, Y Value, Output Value #3",
                                                      "3.0, 1.0, 3.0,           !- X Value, Y Value, Output Value #4",
                                                      "4.0, 0.0, 4.0,           !- X Value, Y Value, Output Value #5",
                                                      "5.0, 2.0, 2.0,           !- X Value, Y Value, Output Value #6",
                                                      "5.0, 1.0, 1.0,           !- X Value, Y Value, Output Value #7",
                                                      "6.0, 1.0, 0.0,           !- X Value, Y Value, Output Value #8",
                                                      "5.0, 1.0, 1.5,           !- X Value, Y Value, Output Value #9",
                                                      "4.0, 1.0, 4.0;           !- X Value, Y Value, Output Value #10",
                                                      "Table:TwoIndependentVariables,",
                                                      "TestTable3,              !- Name",
                                                      "QuadraticLinear,         !- Curve Type",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
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
                                                      "0.0, 0.0, 0.0,           !- X Value, Y Value, Output Value #1",
                                                      "1.0, 1.0, 2.0,           !- X Value, Y Value, Output Value #2",
                                                      "2.0, 0.0, 2.0,           !- X Value, Y Value, Output Value #3",
                                                      "3.0, 1.0, 3.0,           !- X Value, Y Value, Output Value #4",
                                                      "4.0, 0.0, 4.0,           !- X Value, Y Value, Output Value #5",
                                                      "5.0, 2.0, 2.0,           !- X Value, Y Value, Output Value #6",
                                                      "5.0, 1.0, 1.0,           !- X Value, Y Value, Output Value #7",
                                                      "6.0, 1.0, 0.0,           !- X Value, Y Value, Output Value #8",
                                                      "5.0, 1.0, 1.5,           !- X Value, Y Value, Output Value #9",
                                                      "4.0, 1.0, 4.0;           !- X Value, Y Value, Output Value #10"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(3, CurveManager::NumCurves);

    // BiQuadratic curve type
    EXPECT_EQ("BIQUADRATIC", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min1, max1, min2, max2;
    CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(6, max1);
    EXPECT_EQ(0, min2);
    EXPECT_EQ(4, max2);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_NEAR(-0.40430482838859316, CurveManager::CurveValue(1, 0, 0), 1.0e-14);     // In-range value
    EXPECT_NEAR(2.7755053810354879, CurveManager::CurveValue(1, 0.5, 1.5), 1.0e-13);   // In-range value
    EXPECT_NEAR(-0.40430482838859316, CurveManager::CurveValue(1, -10, 0.0), 1.0e-14); // Minimum x
    EXPECT_NEAR(1.9569517161140251, CurveManager::CurveValue(1, 10, 0.0), 1.0e-13);    // Maximum x
    EXPECT_NEAR(-0.40430482838859316, CurveManager::CurveValue(1, 0, -5000), 1.0e-14); // Minimum y
    EXPECT_NEAR(15.046829552065159, CurveManager::CurveValue(1, 0, 5000), 1.0e-12);    // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0, 0));     // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5, 1.5)); // Value too large

    EXPECT_FALSE(has_err_output());

    // QuadraticLinear curve type
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(6, max1);
    EXPECT_EQ(0, min2);
    EXPECT_EQ(4, max2);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_NEAR(-0.1860688425322607, CurveManager::CurveValue(2, 0, 0), 1.0e-13);   // In-range value
    EXPECT_NEAR(2.726964304960856, CurveManager::CurveValue(2, 1, 3), 1.0e-13);     // In-range value
    EXPECT_NEAR(-0.1860688425322607, CurveManager::CurveValue(2, -10, 0), 1.0e-13); // Minimum x
    EXPECT_NEAR(-2.496754328378683, CurveManager::CurveValue(2, 10, 3), 1.0e-12);   // Maximum x
    EXPECT_NEAR(-0.1860688425322607, CurveManager::CurveValue(2, 0, -5), 1.0e-13);  // Minimum y
    EXPECT_NEAR(3.132055967626569, CurveManager::CurveValue(2, 1, 5), 1.0e-13);     // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(2, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 1, 3)); // Value too large

    // QuadraticLinear curve type with no IV min/max
    EXPECT_EQ("QUADRATICLINEAR", CurveManager::GetCurveType(3));
    EXPECT_EQ("TESTTABLE3", CurveManager::GetCurveName(3));
    EXPECT_EQ(3, CurveManager::GetCurveIndex("TESTTABLE3"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE3", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(3, index);
    CurveManager::GetCurveMinMaxValues(3, min1, max1, min2, max2);
    EXPECT_EQ(0, min1);
    EXPECT_EQ(6, max1);
    EXPECT_EQ(0, min2);
    EXPECT_EQ(2, max2);
    EXPECT_EQ(CurveManager::CurveType_TableTwoIV, CurveManager::GetCurveObjectTypeNum(3));

    EXPECT_NEAR(-0.1860688425322607, CurveManager::CurveValue(3, 0, 0), 1.0e-13);   // In-range value
    EXPECT_NEAR(-0.1860688425322607, CurveManager::CurveValue(3, -10, 0), 1.0e-13); // Minimum x
    EXPECT_NEAR(1.162210094161221, CurveManager::CurveValue(3, 10, 0), 1.0e-12);    // Maximum x
    EXPECT_NEAR(-0.1860688425322607, CurveManager::CurveValue(3, 0, -5), 1.0e-13);  // Minimum y
    EXPECT_NEAR(2.321872642295143, CurveManager::CurveValue(3, 1, 5), 1.0e-13);     // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(2, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 1, 3)); // Value too large

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_MultiVariableLookup_Linear_OneIV)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable1,              !- Name",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Linear,                  !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable2,              !- Name",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Linear,               !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      "0.5,                     !- Minimum Table Output",
                                                      "1,                       !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable3,              !- Name",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Quadratic,               !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable4,              !- Name",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Cubic,                   !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable5,              !- Name",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Quartic,                 !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable6,              !- Name",
                                                      "LinearInterpolationOfTable,  !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Quartic,                 !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      ",                        !- Minimum Value of X1",
                                                      ",                        !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(6, CurveManager::NumCurves);

    // Linear curve type
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5)); // Value too large

    // Linear curve type with min and max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0));   // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 5));   // Value too large
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 0.5)); // In-range value

    // Quadratic curve type
    EXPECT_EQ("QUADRATIC", CurveManager::GetCurveType(3));
    EXPECT_EQ("TESTTABLE3", CurveManager::GetCurveName(3));
    EXPECT_EQ(3, CurveManager::GetCurveIndex("TESTTABLE3"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE3", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(3, index);
    CurveManager::GetCurveMinMaxValues(3, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(3));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(3, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(3, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 5)); // Value too large

    // Cubic curve type
    EXPECT_EQ("CUBIC", CurveManager::GetCurveType(4));
    EXPECT_EQ("TESTTABLE4", CurveManager::GetCurveName(4));
    EXPECT_EQ(4, CurveManager::GetCurveIndex("TESTTABLE4"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE4", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(4, index);
    CurveManager::GetCurveMinMaxValues(4, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(4));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(4, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(4, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 5)); // Value too large

    // Quartic curve type
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(5));
    EXPECT_EQ("TESTTABLE5", CurveManager::GetCurveName(5));
    EXPECT_EQ(5, CurveManager::GetCurveIndex("TESTTABLE5"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE5", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(5, index);
    CurveManager::GetCurveMinMaxValues(5, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(5));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(5, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(5, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(5, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 5)); // Value too large

    // Quartic curve type without IV min/max
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(6));
    EXPECT_EQ("TESTTABLE6", CurveManager::GetCurveName(6));
    EXPECT_EQ(6, CurveManager::GetCurveIndex("TESTTABLE6"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE6", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(6, index);
    CurveManager::GetCurveMinMaxValues(6, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(6));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(6, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(6, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(6, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 5)); // Value too large

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_MultiVariableLookup_EvaluateToLimits_OneIV)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable1,              !- Name",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Linear,                  !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable2,              !- Name",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Linear,               !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      "0.96,                    !- Minimum Table Output",
                                                      "1.5,                     !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable3,              !- Name",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Quadratic,               !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable4,              !- Name",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Cubic,                   !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable5,              !- Name",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Quartic,                 !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable6,              !- Name",
                                                      "EvaluateCurveToLimits,   !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Quartic,                 !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      ",                        !- Minimum Value of X1",
                                                      ",                        !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(6, CurveManager::NumCurves);

    // Linear curve type
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.9523809523809523, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.438095238095238, CurveManager::CurveValue(1, 1.0));    // In-range value
    EXPECT_DOUBLE_EQ(0.9523809523809523, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(3.38095238095238, CurveManager::CurveValue(1, 5000));    // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.96, 1.5);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.96, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(1, 5));  // Value too large

    // Linear curve type with min and max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(0.96, CurveManager::CurveValue(2, 0));                // Value too small
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(2, 5));                 // Value too large
    EXPECT_DOUBLE_EQ(1.438095238095238, CurveManager::CurveValue(2, 1.0)); // In-range value

    // Quadratic curve type
    EXPECT_EQ("QUADRATIC", CurveManager::GetCurveType(3));
    EXPECT_EQ("TESTTABLE3", CurveManager::GetCurveName(3));
    EXPECT_EQ(3, CurveManager::GetCurveIndex("TESTTABLE3"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE3", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(3, index);
    CurveManager::GetCurveMinMaxValues(3, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(3));

    EXPECT_NEAR(0.0, CurveManager::CurveValue(3, 0), 1.0e-14);                  // In-range value
    EXPECT_DOUBLE_EQ(1.628571428571429, CurveManager::CurveValue(3, 1));        // In-range value
    EXPECT_NEAR(0.0, CurveManager::CurveValue(3, -10.0), 1.0e-14);              // Minimum x
    EXPECT_NEAR(2.428571428571427, CurveManager::CurveValue(3, 5000), 1.0e-14); // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(3, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 5)); // Value too large

    // Cubic curve type
    EXPECT_EQ("CUBIC", CurveManager::GetCurveType(4));
    EXPECT_EQ("TESTTABLE4", CurveManager::GetCurveName(4));
    EXPECT_EQ(4, CurveManager::GetCurveIndex("TESTTABLE4"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE4", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(4, index);
    CurveManager::GetCurveMinMaxValues(4, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(4));

    EXPECT_NEAR(0.2222222222222222, CurveManager::CurveValue(4, 0), 1.0e-14);     // In-range value
    EXPECT_NEAR(1.317460317460317, CurveManager::CurveValue(4, 1), 1.0e-14);      // In-range value
    EXPECT_NEAR(0.2222222222222222, CurveManager::CurveValue(4, -10.0), 1.0e-14); // Minimum x
    EXPECT_NEAR(2.206349206349206, CurveManager::CurveValue(4, 5000), 1.0e-14);   // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(4, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 5)); // Value too large

    // Quartic curve type
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(5));
    EXPECT_EQ("TESTTABLE5", CurveManager::GetCurveName(5));
    EXPECT_EQ(5, CurveManager::GetCurveIndex("TESTTABLE5"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE5", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(5, index);
    CurveManager::GetCurveMinMaxValues(5, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(5));

    EXPECT_NEAR(0.007936507936507936, CurveManager::CurveValue(5, 0), 1.0e-12);     // In-range value
    EXPECT_NEAR(1.96031746031746, CurveManager::CurveValue(5, 1.0), 1.0e-12);       // In-range value
    EXPECT_NEAR(0.007936507936507936, CurveManager::CurveValue(5, -10.0), 1.0e-12); // Minimum x
    EXPECT_NEAR(1.992063492063494, CurveManager::CurveValue(5, 5000), 1.0e-12);     // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(5, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(5, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 5)); // Value too large

    // Quartic curve type with no IV min/max
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(6));
    EXPECT_EQ("TESTTABLE6", CurveManager::GetCurveName(6));
    EXPECT_EQ(6, CurveManager::GetCurveIndex("TESTTABLE6"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE6", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(6, index);
    CurveManager::GetCurveMinMaxValues(6, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(6));

    EXPECT_NEAR(0.007936507936507936, CurveManager::CurveValue(6, 0), 1.0e-12);     // In-range value
    EXPECT_NEAR(1.96031746031746, CurveManager::CurveValue(6, 1.0), 1.0e-12);       // In-range value
    EXPECT_NEAR(0.007936507936507936, CurveManager::CurveValue(6, -10.0), 1.0e-12); // Minimum x
    EXPECT_NEAR(1.992063492063494, CurveManager::CurveValue(6, 5000), 1.0e-12);     // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(6, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(6, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 5)); // Value too large

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_MultiVariableLookup_Lagrange_OneIV)
{

    std::string const idf_objects = delimited_string({"Version,8.5;",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable1,              !- Name",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Linear,                  !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable2,              !- Name",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "2,                       !- Number of Interpolation Points",
                                                      "Linear,               !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      "0.5,                     !- Minimum Table Output",
                                                      "1.0,                     !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable3,              !- Name",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "3,                       !- Number of Interpolation Points",
                                                      "Linear,                  !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      ",                        !- Minimum Value of X1",
                                                      ",                        !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable4,              !- Name",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "3,                       !- Number of Interpolation Points",
                                                      "Linear,                  !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "-1,                      !- Minimum Value of X1",
                                                      "10,                      !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>",
                                                      "Table:MultiVariableLookup,",
                                                      "TestTable5,              !- Name",
                                                      "LagrangeInterpolationLinearExtrapolation,  !- Interpolation Method",
                                                      "4,                       !- Number of Interpolation Points",
                                                      "Linear,                  !- Curve Type",
                                                      "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
                                                      ",                        !- External File Name",
                                                      "ASCENDING,               !- X1 Sort Order",
                                                      ",                        !- X2 Sort Order",
                                                      ",                        !- Normalization Reference",
                                                      "0,                       !- Minimum Value of X1",
                                                      "5,                       !- Maximum Value of X1",
                                                      ",                        !- Minimum Value of X2",
                                                      ",                        !- Maximum Value of X2",
                                                      ",                        !- Minimum Value of X3",
                                                      ",                        !- Maximum Value of X3",
                                                      ",                        !- Minimum Value of X4",
                                                      ",                        !- Maximum Value of X4",
                                                      ",                        !- Minimum Value of X5",
                                                      ",                        !- Maximum Value of X5",
                                                      ",                        !- Minimum Table Output",
                                                      ",                        !- Maximum Table Output",
                                                      "Dimensionless,           !- Input Unit Type for X1",
                                                      ",                        !- Input Unit Type for X2",
                                                      ",                        !- Input Unit Type for X3",
                                                      ",                        !- Input Unit Type for X4",
                                                      ",                        !- Input Unit Type for X5",
                                                      "Dimensionless,           !- Output Unit Type",
                                                      "1,                       !- Number of Independent Variables",
                                                      "6,                       !- Number of Values for Independent Variable X1",
                                                      "0,                       !- Field 1 Determined by the Number of Independent Variables",
                                                      "1,                       !- Field 2 Determined by the Number of Independent Variables",
                                                      "2,                       !- Field 3 Determined by the Number of Independent Variables",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "5,                       !- <none>",
                                                      "0,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "2,                       !- <none>",
                                                      "3,                       !- <none>",
                                                      "4,                       !- <none>",
                                                      "2;                       !- <none>"});

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(5, CurveManager::NumCurves);

    // Linear curve type
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0));     // In-range value
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000));  // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5)); // Value too large

    // Linear curve type with min and max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0));   // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 5));   // Value too large
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 0.5)); // In-range value

    // Quadratic curve type without min or max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(3));
    EXPECT_EQ("TESTTABLE3", CurveManager::GetCurveName(3));
    EXPECT_EQ(3, CurveManager::GetCurveIndex("TESTTABLE3"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE3", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(3, index);
    CurveManager::GetCurveMinMaxValues(3, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(3));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, 0));       // In-range value
    EXPECT_DOUBLE_EQ(1.25, CurveManager::CurveValue(3, 0.5));    // In-range value
    EXPECT_DOUBLE_EQ(1.6875, CurveManager::CurveValue(3, 0.75)); // In-range value
    EXPECT_DOUBLE_EQ(2.055, CurveManager::CurveValue(3, 2.1));   // In-range value
    EXPECT_DOUBLE_EQ(2.855, CurveManager::CurveValue(3, 2.9));   // In-range value
    EXPECT_DOUBLE_EQ(3.9, CurveManager::CurveValue(3, 3.9));     // In-range value
    EXPECT_DOUBLE_EQ(3.84, CurveManager::CurveValue(3, 4.2));    // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, -10.0));   // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(3, 5000));    // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(3, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 5)); // Value too large

    // Quadratic curve type with extrapolation
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(4));
    EXPECT_EQ("TESTTABLE4", CurveManager::GetCurveName(4));
    EXPECT_EQ(4, CurveManager::GetCurveIndex("TESTTABLE4"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE4", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(4, index);
    CurveManager::GetCurveMinMaxValues(4, min, max);
    EXPECT_EQ(-1, min);
    EXPECT_EQ(10, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(4));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 0));       // In-range value
    EXPECT_DOUBLE_EQ(1.25, CurveManager::CurveValue(4, 0.5));    // In-range value
    EXPECT_DOUBLE_EQ(1.6875, CurveManager::CurveValue(4, 0.75)); // In-range value
    EXPECT_DOUBLE_EQ(2.055, CurveManager::CurveValue(4, 2.1));   // In-range value
    EXPECT_DOUBLE_EQ(2.855, CurveManager::CurveValue(4, 2.9));   // In-range value
    EXPECT_DOUBLE_EQ(3.9, CurveManager::CurveValue(4, 3.9));     // In-range value
    EXPECT_DOUBLE_EQ(3.84, CurveManager::CurveValue(4, 4.2));    // In-range value
    EXPECT_DOUBLE_EQ(-1.0, CurveManager::CurveValue(4, -0.5));   // Extrapolated value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 6));       // Extrapolated value
    EXPECT_DOUBLE_EQ(-2.0, CurveManager::CurveValue(4, -10.0));  // Minimum x
    EXPECT_DOUBLE_EQ(-8.0, CurveManager::CurveValue(4, 5000));   // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(4, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 5)); // Value too large

    // Cubic curve type
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(5));
    EXPECT_EQ("TESTTABLE5", CurveManager::GetCurveName(5));
    EXPECT_EQ(5, CurveManager::GetCurveIndex("TESTTABLE5"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE5", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(5, index);
    CurveManager::GetCurveMinMaxValues(5, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(5));

    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, 0));      // In-range value
    EXPECT_DOUBLE_EQ(1.4375, CurveManager::CurveValue(5, 0.5)); // In-range value
    EXPECT_DOUBLE_EQ(2.4375, CurveManager::CurveValue(5, 2.5)); // In-range value
    EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, -10.0));  // Minimum x
    EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(5, 5000));   // Maximum x

    CurveManager::SetCurveOutputMinMaxValues(5, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(5, 0)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 5)); // Value too large

    EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_MultiVariableLookup_Linear_TwoIV)
{

    std::string const idf_objects = delimited_string({
        "Version,8.5;",
        "Table:MultiVariableLookup,",
        "TestTable1,              !- Name",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        "2,                       !- Number of Interpolation Points",
        "Linear,               !- Curve Type",
        "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
        ",                        !- External File Name",
        "ASCENDING,               !- X1 Sort Order",
        "ASCENDING,               !- X2 Sort Order",
        ",                        !- Normalization Reference",
        "0,                       !- Minimum Value of X1",
        "5,                       !- Maximum Value of X1",
        "1,                       !- Minimum Value of X2",
        "2,                       !- Maximum Value of X2",
        ",                        !- Minimum Value of X3",
        ",                        !- Maximum Value of X3",
        ",                        !- Minimum Value of X4",
        ",                        !- Maximum Value of X4",
        ",                        !- Minimum Value of X5",
        ",                        !- Maximum Value of X5",
        ",                        !- Minimum Table Output",
        ",                        !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X1",
        "Dimensionless,           !- Input Unit Type for X2",
        ",                        !- Input Unit Type for X3",
        ",                        !- Input Unit Type for X4",
        ",                        !- Input Unit Type for X5",
        "Dimensionless,           !- Output Unit Type",
        "2,                       !- Number of Independent Variables",
        "6,                       !- Number of Values for Independent Variable X1",
        "2,                       !- Field 1 Determined by the Number of Independent Variables",
        "0,                       !- Field 2 Determined by the Number of Independent Variables",
        "1,                       !- Field 3 Determined by the Number of Independent Variables",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "1,                       !- <none>",
        "2,                       !- <none>",
        "0,                       !- <none>",
        "2,                       !- <none>",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "2,                       !- <none>",
        "1,                       !- <none>",
        "3,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "3;                       !- <none>",
        "Table:MultiVariableLookup,",
        "TestTable2,              !- Name",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        "2,                       !- Number of Interpolation Points",
        "Linear,                  !- Curve Type",
        "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
        ",                        !- External File Name",
        "ASCENDING,               !- X1 Sort Order",
        "ASCENDING,               !- X2 Sort Order",
        ",                        !- Normalization Reference",
        "0,                       !- Minimum Value of X1",
        "5,                       !- Maximum Value of X1",
        ",                        !- Minimum Value of X2",
        ",                        !- Maximum Value of X2",
        ",                        !- Minimum Value of X3",
        ",                        !- Maximum Value of X3",
        ",                        !- Minimum Value of X4",
        ",                        !- Maximum Value of X4",
        ",                        !- Minimum Value of X5",
        ",                        !- Maximum Value of X5",
        "0.5,                     !- Minimum Table Output",
        "1,                       !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X1",
        "Dimensionless,           !- Input Unit Type for X2",
        ",                        !- Input Unit Type for X3",
        ",                        !- Input Unit Type for X4",
        ",                        !- Input Unit Type for X5",
        "Dimensionless,           !- Output Unit Type",
        "2,                       !- Number of Independent Variables",
        "6,                       !- Number of Values for Independent Variable X1",
        "2,                       !- Field 1 Determined by the Number of Independent Variables",
        "0,                       !- Field 2 Determined by the Number of Independent Variables",
        "1,                       !- Field 3 Determined by the Number of Independent Variables",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "1,                       !- <none>",
        "2,                       !- <none>",
        "0,                       !- <none>",
        "2,                       !- <none>",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "2,                       !- <none>",
        "1,                       !- <none>",
        "3,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "3;                       !- <none>",
        "Table:MultiVariableLookup,",
        "TestTable3,              !- Name",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        "2,                       !- Number of Interpolation Points",
        "Quadratic,               !- Curve Type",
        "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
        ",                        !- External File Name",
        "ASCENDING,               !- X1 Sort Order",
        "ASCENDING,               !- X2 Sort Order",
        ",                        !- Normalization Reference",
        "0,                       !- Minimum Value of X1",
        "5,                       !- Maximum Value of X1",
        "1,                       !- Minimum Value of X2",
        "2,                       !- Maximum Value of X2",
        ",                        !- Minimum Value of X3",
        ",                        !- Maximum Value of X3",
        ",                        !- Minimum Value of X4",
        ",                        !- Maximum Value of X4",
        ",                        !- Minimum Value of X5",
        ",                        !- Maximum Value of X5",
        ",                        !- Minimum Table Output",
        ",                        !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X1",
        "Dimensionless,           !- Input Unit Type for X2",
        ",                        !- Input Unit Type for X3",
        ",                        !- Input Unit Type for X4",
        ",                        !- Input Unit Type for X5",
        "Dimensionless,           !- Output Unit Type",
        "2,                       !- Number of Independent Variables",
        "6,                       !- Number of Values for Independent Variable X1",
        "2,                       !- Field 1 Determined by the Number of Independent Variables",
        "0,                       !- Field 2 Determined by the Number of Independent Variables",
        "1,                       !- Field 3 Determined by the Number of Independent Variables",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "1,                       !- <none>",
        "2,                       !- <none>",
        "0,                       !- <none>",
        "2,                       !- <none>",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "2,                       !- <none>",
        "1,                       !- <none>",
        "3,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "3;                       !- <none>",
        "Table:MultiVariableLookup,",
        "TestTable4,              !- Name",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        "2,                       !- Number of Interpolation Points",
        "Cubic,                   !- Curve Type",
        "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
        ",                        !- External File Name",
        "ASCENDING,               !- X1 Sort Order",
        "ASCENDING,               !- X2 Sort Order",
        ",                        !- Normalization Reference",
        "0,                       !- Minimum Value of X1",
        "5,                       !- Maximum Value of X1",
        "1,                       !- Minimum Value of X2",
        "2,                       !- Maximum Value of X2",
        ",                        !- Minimum Value of X3",
        ",                        !- Maximum Value of X3",
        ",                        !- Minimum Value of X4",
        ",                        !- Maximum Value of X4",
        ",                        !- Minimum Value of X5",
        ",                        !- Maximum Value of X5",
        ",                        !- Minimum Table Output",
        ",                        !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X1",
        "Dimensionless,           !- Input Unit Type for X2",
        ",                        !- Input Unit Type for X3",
        ",                        !- Input Unit Type for X4",
        ",                        !- Input Unit Type for X5",
        "Dimensionless,           !- Output Unit Type",
        "2,                       !- Number of Independent Variables",
        "6,                       !- Number of Values for Independent Variable X1",
        "2,                       !- Field 1 Determined by the Number of Independent Variables",
        "0,                       !- Field 2 Determined by the Number of Independent Variables",
        "1,                       !- Field 3 Determined by the Number of Independent Variables",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "1,                       !- <none>",
        "2,                       !- <none>",
        "0,                       !- <none>",
        "2,                       !- <none>",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "2,                       !- <none>",
        "1,                       !- <none>",
        "3,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "3;                       !- <none>",
        "Table:MultiVariableLookup,",
        "TestTable5,              !- Name",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        "2,                       !- Number of Interpolation Points",
        "Quartic,                 !- Curve Type",
        "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
        ",                        !- External File Name",
        "ASCENDING,               !- X1 Sort Order",
        "ASCENDING,               !- X2 Sort Order",
        ",                        !- Normalization Reference",
        "0,                       !- Minimum Value of X1",
        "5,                       !- Maximum Value of X1",
        "1,                       !- Minimum Value of X2",
        "2,                       !- Maximum Value of X2",
        ",                        !- Minimum Value of X3",
        ",                        !- Maximum Value of X3",
        ",                        !- Minimum Value of X4",
        ",                        !- Maximum Value of X4",
        ",                        !- Minimum Value of X5",
        ",                        !- Maximum Value of X5",
        ",                        !- Minimum Table Output",
        ",                        !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X1",
        "Dimensionless,           !- Input Unit Type for X2",
        ",                        !- Input Unit Type for X3",
        ",                        !- Input Unit Type for X4",
        ",                        !- Input Unit Type for X5",
        "Dimensionless,           !- Output Unit Type",
        "2,                       !- Number of Independent Variables",
        "6,                       !- Number of Values for Independent Variable X1",
        "2,                       !- Field 1 Determined by the Number of Independent Variables",
        "0,                       !- Field 2 Determined by the Number of Independent Variables",
        "1,                       !- Field 3 Determined by the Number of Independent Variables",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "1,                       !- <none>",
        "2,                       !- <none>",
        "0,                       !- <none>",
        "2,                       !- <none>",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "2,                       !- <none>",
        "1,                       !- <none>",
        "3,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "3;                       !- <none>",
        "Table:MultiVariableLookup,",
        "TestTable6,              !- Name",
        "LinearInterpolationOfTable,  !- Interpolation Method",
        "2,                       !- Number of Interpolation Points",
        "Quartic,                 !- Curve Type",
        "SingleLineIndependentVariableWithMatrix,  !- Table Data Format",
        ",                        !- External File Name",
        "ASCENDING,               !- X1 Sort Order",
        "ASCENDING,               !- X2 Sort Order",
        ",                        !- Normalization Reference",
        ",                        !- Minimum Value of X1",
        ",                        !- Maximum Value of X1",
        ",                        !- Minimum Value of X2",
        ",                        !- Maximum Value of X2",
        ",                        !- Minimum Value of X3",
        ",                        !- Maximum Value of X3",
        ",                        !- Minimum Value of X4",
        ",                        !- Maximum Value of X4",
        ",                        !- Minimum Value of X5",
        ",                        !- Maximum Value of X5",
        ",                        !- Minimum Table Output",
        ",                        !- Maximum Table Output",
        "Dimensionless,           !- Input Unit Type for X1",
        "Dimensionless,           !- Input Unit Type for X2",
        ",                        !- Input Unit Type for X3",
        ",                        !- Input Unit Type for X4",
        ",                        !- Input Unit Type for X5",
        "Dimensionless,           !- Output Unit Type",
        "2,                       !- Number of Independent Variables",
        "6,                       !- Number of Values for Independent Variable X1",
        "2,                       !- Field 1 Determined by the Number of Independent Variables",
        "0,                       !- Field 2 Determined by the Number of Independent Variables",
        "1,                       !- Field 3 Determined by the Number of Independent Variables",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "1,                       !- <none>",
        "2,                       !- <none>",
        "0,                       !- <none>",
        "2,                       !- <none>",
        "2,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "2,                       !- <none>",
        "1,                       !- <none>",
        "3,                       !- <none>",
        "3,                       !- <none>",
        "4,                       !- <none>",
        "5,                       !- <none>",
        "3;                       !- <none>",
    });

    ASSERT_FALSE(process_idf(idf_objects));

    EXPECT_EQ(0, CurveManager::NumCurves);
    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    ASSERT_EQ(6, CurveManager::NumCurves);

    // Linear curve type
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(1));
    EXPECT_EQ("TESTTABLE1", CurveManager::GetCurveName(1));
    EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTTABLE1"));
    bool error = false;
    int index = CurveManager::GetCurveCheck("TESTTABLE1", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(1, index);
    Real64 min, max;
    CurveManager::GetCurveMinMaxValues(1, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(1));

    EXPECT_DOUBLE_EQ(0.25, CurveManager::CurveValue(1, 0, 1.25));   // In-range value
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(1, 0.5, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, -10.0, 1.5)); // Minimum x
    EXPECT_DOUBLE_EQ(2.5, CurveManager::CurveValue(1, 5000, 1.5));  // Maximum x
    EXPECT_DOUBLE_EQ(0, CurveManager::CurveValue(1, 0, 0));         // Minimum y
    EXPECT_DOUBLE_EQ(3, CurveManager::CurveValue(1, 5, 3));         // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(1, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(1, 0, 1.25)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5, 1.5));  // Value too large

    // Linear curve type with min and max
    EXPECT_EQ("LINEAR", CurveManager::GetCurveType(2));
    EXPECT_EQ("TESTTABLE2", CurveManager::GetCurveName(2));
    EXPECT_EQ(2, CurveManager::GetCurveIndex("TESTTABLE2"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE2", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(2, index);
    CurveManager::GetCurveMinMaxValues(2, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(2));

    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0, 1.25));     // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 5, 1.5));      // Value too large
    EXPECT_DOUBLE_EQ(0.75, CurveManager::CurveValue(2, 0.25, 1.25)); // In-range value

    // Quadratic curve type
    EXPECT_EQ("QUADRATIC", CurveManager::GetCurveType(3));
    EXPECT_EQ("TESTTABLE3", CurveManager::GetCurveName(3));
    EXPECT_EQ(3, CurveManager::GetCurveIndex("TESTTABLE3"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE3", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(3, index);
    CurveManager::GetCurveMinMaxValues(3, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(3));

    EXPECT_DOUBLE_EQ(0.25, CurveManager::CurveValue(3, 0, 1.25));   // In-range value
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(3, 0.5, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, -10.0, 1.5)); // Minimum x
    EXPECT_DOUBLE_EQ(2.5, CurveManager::CurveValue(3, 5000, 1.5));  // Maximum x
    EXPECT_DOUBLE_EQ(0, CurveManager::CurveValue(3, 0, 0));         // Minimum y
    EXPECT_DOUBLE_EQ(3, CurveManager::CurveValue(3, 5, 3));         // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(3, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(3, 0, 1.25)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 5, 1.5));  // Value too large

    // Cubic curve type
    EXPECT_EQ("CUBIC", CurveManager::GetCurveType(4));
    EXPECT_EQ("TESTTABLE4", CurveManager::GetCurveName(4));
    EXPECT_EQ(4, CurveManager::GetCurveIndex("TESTTABLE4"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE4", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(4, index);
    CurveManager::GetCurveMinMaxValues(4, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(4));

    EXPECT_DOUBLE_EQ(0.25, CurveManager::CurveValue(4, 0, 1.25));   // In-range value
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(4, 0.5, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, -10.0, 1.5)); // Minimum x
    EXPECT_DOUBLE_EQ(2.5, CurveManager::CurveValue(4, 5000, 1.5));  // Maximum x
    EXPECT_DOUBLE_EQ(0, CurveManager::CurveValue(4, 0, 0));         // Minimum y
    EXPECT_DOUBLE_EQ(3, CurveManager::CurveValue(4, 5, 3));         // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(4, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(4, 0, 1.25)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 5, 1.5));  // Value too large

    // Quartic curve type
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(5));
    EXPECT_EQ("TESTTABLE5", CurveManager::GetCurveName(5));
    EXPECT_EQ(5, CurveManager::GetCurveIndex("TESTTABLE5"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE5", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(5, index);
    CurveManager::GetCurveMinMaxValues(5, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(5));

    EXPECT_DOUBLE_EQ(0.25, CurveManager::CurveValue(5, 0, 1.25));   // In-range value
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(5, 0.5, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(5, -10.0, 1.5)); // Minimum x
    EXPECT_DOUBLE_EQ(2.5, CurveManager::CurveValue(5, 5000, 1.5));  // Maximum x
    EXPECT_DOUBLE_EQ(0, CurveManager::CurveValue(5, 0, 0));         // Minimum y
    EXPECT_DOUBLE_EQ(3, CurveManager::CurveValue(5, 5, 3));         // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(5, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(5, 0, 1.25)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 5, 1.5));  // Value too large

    // Quartic curve type without IV min/max
    EXPECT_EQ("QUARTIC", CurveManager::GetCurveType(6));
    EXPECT_EQ("TESTTABLE6", CurveManager::GetCurveName(6));
    EXPECT_EQ(6, CurveManager::GetCurveIndex("TESTTABLE6"));
    error = false;
    index = CurveManager::GetCurveCheck("TESTTABLE6", error, "TEST");
    EXPECT_FALSE(error);
    EXPECT_EQ(6, index);
    CurveManager::GetCurveMinMaxValues(6, min, max);
    EXPECT_EQ(0, min);
    EXPECT_EQ(5, max);
    EXPECT_EQ(CurveManager::CurveType_TableMultiIV, CurveManager::GetCurveObjectTypeNum(6));

    EXPECT_DOUBLE_EQ(0.25, CurveManager::CurveValue(6, 0, 1.25));   // In-range value
    EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(6, 0.5, 1.5));   // In-range value
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(6, -10.0, 1.5)); // Minimum x
    EXPECT_DOUBLE_EQ(2.5, CurveManager::CurveValue(6, 5000, 1.5));  // Maximum x
    EXPECT_DOUBLE_EQ(0, CurveManager::CurveValue(6, 0, 0));         // Minimum y
    EXPECT_DOUBLE_EQ(3, CurveManager::CurveValue(6, 5, 3));         // Maximum y

    CurveManager::SetCurveOutputMinMaxValues(6, error, 0.5, 1.0);
    EXPECT_FALSE(error);
    EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(6, 0, 1.25)); // Value too small
    EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 5, 1.5));  // Value too large

    EXPECT_FALSE(has_err_output());
}