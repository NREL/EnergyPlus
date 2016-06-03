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

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_Linear) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"2;                       !- Output Value #6",
		"Table:OneIndependentVariable,",
		"TestTable8,              !- Name",
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
		"1,                       !- X Value #2",
		"1;                       !- Output Value #2" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
	CurveManager::GetCurvesInputFlag = false;
	ASSERT_EQ(8, CurveManager::NumCurves);

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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000)); // Maximum x

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

	EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0)); // Value too small
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 5)); // Value too large
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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(3, 5000)); // Maximum x

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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(4, 5000)); // Maximum x

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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(5, 5000)); // Maximum x

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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(6, 5000)); // Maximum x

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
	CurveManager::GetCurveMinMaxValues(7, min, max);
	//EXPECT_EQ(0, min);
	//EXPECT_EQ(5, max);
	EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(7));

	/*
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(7, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(7, 5000)); // Maximum x

	CurveManager::SetCurveOutputMinMaxValues(7, error, 0.5, 1.0);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(7, 0)); // Value too small
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(7, 5)); // Value too large
	*/

	// Super-simple table without IV min/max
	EXPECT_EQ("LINEAR", CurveManager::GetCurveType(8));
	EXPECT_EQ("TESTTABLE8", CurveManager::GetCurveName(8));
	EXPECT_EQ(8, CurveManager::GetCurveIndex("TESTTABLE8"));
	error = false;
	index = CurveManager::GetCurveCheck("TESTTABLE8", error, "TEST");
	EXPECT_FALSE(error);
	EXPECT_EQ(8, index);
	CurveManager::GetCurveMinMaxValues(8, min, max);
	//EXPECT_EQ(0, min);
	//EXPECT_EQ(5, max);
	EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(8));

//	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(8, 0)); // In-range value
//	EXPECT_DOUBLE_EQ(0.75, CurveManager::CurveValue(8, 0.75)); // In-range value
//	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, -10.0)); // Minimum x
//	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(7, 5000)); // Maximum x

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_Lagrange) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"5,                       !- X Value #6",
		"2;                       !- Output Value #6",
		"Table:OneIndependentVariable,",
		"TestTable8,              !- Name",
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
		"1,                       !- X Value #2",
		"1;                       !- Output Value #2" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
	CurveManager::GetCurvesInputFlag = false;
	ASSERT_EQ(8, CurveManager::NumCurves);

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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 5000)); // Maximum x

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

	EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(2, 0)); // Value too small
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(2, 5)); // Value too large
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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(3, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(3, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(3, 5000)); // Maximum x

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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(4, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(4, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(4, 5000)); // Maximum x

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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(5, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(5, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(5, 5000)); // Maximum x

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

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(6, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(6, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(6, 5000)); // Maximum x

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
	CurveManager::GetCurveMinMaxValues(7, min, max);
	//EXPECT_EQ(0, min);
	//EXPECT_EQ(5, max);
	EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(7));

	/*
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, 0)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(7, 0.5)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(7, 5000)); // Maximum x

	CurveManager::SetCurveOutputMinMaxValues(7, error, 0.5, 1.0);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(0.5, CurveManager::CurveValue(7, 0)); // Value too small
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(7, 5)); // Value too large
	*/

	// Super-simple table without IV min/max
	EXPECT_EQ("LINEAR", CurveManager::GetCurveType(8));
	EXPECT_EQ("TESTTABLE8", CurveManager::GetCurveName(8));
	EXPECT_EQ(8, CurveManager::GetCurveIndex("TESTTABLE8"));
	error = false;
	index = CurveManager::GetCurveCheck("TESTTABLE8", error, "TEST");
	EXPECT_FALSE(error);
	EXPECT_EQ(8, index);
	CurveManager::GetCurveMinMaxValues(8, min, max);
	//EXPECT_EQ(0, min);
	//EXPECT_EQ(5, max);
	EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(8));

	//	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(8, 0)); // In-range value
	//	EXPECT_DOUBLE_EQ(0.75, CurveManager::CurveValue(8, 0.75)); // In-range value
	//	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(7, -10.0)); // Minimum x
	//	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(7, 5000)); // Maximum x

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Tables_OneIndependentVariable_MinMaxFailure) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"1,                       !- X Value #2",
		"1;                       !- Output Value #2" });

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
	//Real64 min, max;
	//CurveManager::GetCurveMinMaxValues(1, min, max);
	//EXPECT_EQ(0, min);
	//EXPECT_EQ(5, max);
	EXPECT_EQ(CurveManager::CurveType_TableOneIV, CurveManager::GetCurveObjectTypeNum(1));

	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(0.75, CurveManager::CurveValue(1, 0.75)); // In-range value
	EXPECT_DOUBLE_EQ(0.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 5000)); // Maximum x

	EXPECT_FALSE(has_err_output());
}


