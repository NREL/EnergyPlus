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

TEST_F(EnergyPlusFixture, Curves_Linear) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(42.0, CurveManager::CurveValue(1, 5)); // In-range value
	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, -10.0)); // Minimum x
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

TEST_F(EnergyPlusFixture, Curves_Quadratic) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(2.0, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(14.0, CurveManager::CurveValue(1, 1)); // In-range value
	EXPECT_DOUBLE_EQ(762.0, CurveManager::CurveValue(1, -100.0)); // Minimum x
	EXPECT_DOUBLE_EQ(842.0, CurveManager::CurveValue(1, 5000)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 3, 5);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(3.0, CurveManager::CurveValue(1, 0)); // Value too small
	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 1)); // Value too large

	EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(2, 0)); // Value too small
	EXPECT_DOUBLE_EQ(100.0, CurveManager::CurveValue(2, -100)); // Value too large
	EXPECT_DOUBLE_EQ(14.0, CurveManager::CurveValue(2, 1)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_Cubic) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 1)); // In-range value
	EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, -1.0)); // Minimum x
	EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 5)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 5, 9);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 0)); // Value too small
	EXPECT_DOUBLE_EQ(9.0, CurveManager::CurveValue(1, 1)); // Value too large

	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(2, 0)); // Value too small
	EXPECT_DOUBLE_EQ(9.0, CurveManager::CurveValue(2, 1)); // Value too large
	EXPECT_DOUBLE_EQ(6.125, CurveManager::CurveValue(2, 0.5)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_Exponent) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(12.0, CurveManager::CurveValue(1, 2)); // In-range value
	EXPECT_DOUBLE_EQ(4.0, CurveManager::CurveValue(1, -1.0)); // Minimum x
	EXPECT_DOUBLE_EQ(12.0, CurveManager::CurveValue(1, 500)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 5, 10);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 0)); // Value too small
	EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 2)); // Value too large

	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(2, 0)); // Value too small
	EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(2, 2)); // Value too large
	EXPECT_DOUBLE_EQ(6.0, CurveManager::CurveValue(2, 1)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_ExponentialDecay) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(4.270670566473226, CurveManager::CurveValue(1, -1)); // In-range value
	EXPECT_DOUBLE_EQ(18.7781121978613, CurveManager::CurveValue(1, 1)); // In-range value
	EXPECT_DOUBLE_EQ(4.270670566473226, CurveManager::CurveValue(1, -2.0)); // Minimum x
	EXPECT_DOUBLE_EQ(18.7781121978613, CurveManager::CurveValue(1, 500)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 5, 10);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, -1)); // Value too small
	EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 1)); // Value too large

	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(2, -1)); // Value too small
	EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(2, 1)); // Value too large
	EXPECT_DOUBLE_EQ(6.0, CurveManager::CurveValue(2, 0)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_DoubleExponentialDecay) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(5.00642944881611, CurveManager::CurveValue(1, -1)); // In-range value
	EXPECT_DOUBLE_EQ(24.21467585477939, CurveManager::CurveValue(1, 1)); // In-range value
	EXPECT_DOUBLE_EQ(5.00642944881611, CurveManager::CurveValue(1, -2.0)); // Minimum x
	EXPECT_DOUBLE_EQ(24.21467585477939, CurveManager::CurveValue(1, 500)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 6, 10);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(6.0, CurveManager::CurveValue(1, -1)); // Value too small
	EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(1, 1)); // Value too large

	EXPECT_DOUBLE_EQ(6.0, CurveManager::CurveValue(2, -1)); // Value too small
	EXPECT_DOUBLE_EQ(10.0, CurveManager::CurveValue(2, 1)); // Value too large
	EXPECT_DOUBLE_EQ(8.0, CurveManager::CurveValue(2, 0)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_Sigmoid) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(1.002249213446655, CurveManager::CurveValue(1, -5)); // In-range value
	EXPECT_DOUBLE_EQ(1.775803492574376, CurveManager::CurveValue(1, 5)); // In-range value
	EXPECT_DOUBLE_EQ(1.000016566021026, CurveManager::CurveValue(1, -20.0)); // Minimum x
	EXPECT_DOUBLE_EQ(1.978146827647182, CurveManager::CurveValue(1, 500)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 1.2, 1.5);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(1.2, CurveManager::CurveValue(1, -5)); // Value too small
	EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(1, 5)); // Value too large

	EXPECT_DOUBLE_EQ(1.2, CurveManager::CurveValue(2, -5)); // Value too small
	EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(2, 5)); // Value too large
	EXPECT_DOUBLE_EQ(1.38745561900026, CurveManager::CurveValue(2, 2)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_RectangularHyperbola1) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(5, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(6, CurveManager::CurveValue(1, 2)); // In-range value
	EXPECT_DOUBLE_EQ(5, CurveManager::CurveValue(1, -20.0)); // Minimum x
	EXPECT_DOUBLE_EQ(6, CurveManager::CurveValue(1, 500)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 5.2, 5.7);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(5.2, CurveManager::CurveValue(1, 0)); // Value too small
	EXPECT_DOUBLE_EQ(5.7, CurveManager::CurveValue(1, 2)); // Value too large

	EXPECT_DOUBLE_EQ(5.2, CurveManager::CurveValue(2, 0)); // Value too small
	EXPECT_DOUBLE_EQ(5.7, CurveManager::CurveValue(2, 2)); // Value too large
	EXPECT_DOUBLE_EQ(5.6, CurveManager::CurveValue(2, 1)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_RectangularHyperbola2) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(0, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(11, CurveManager::CurveValue(1, 2)); // In-range value
	EXPECT_DOUBLE_EQ(0, CurveManager::CurveValue(1, -20.0)); // Minimum x
	EXPECT_DOUBLE_EQ(11, CurveManager::CurveValue(1, 500)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 5.2, 5.7);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(5.2, CurveManager::CurveValue(1, 0)); // Value too small
	EXPECT_DOUBLE_EQ(5.7, CurveManager::CurveValue(1, 2)); // Value too large

	EXPECT_DOUBLE_EQ(5.2, CurveManager::CurveValue(2, 0)); // Value too small
	EXPECT_DOUBLE_EQ(5.7, CurveManager::CurveValue(2, 2)); // Value too large
	EXPECT_DOUBLE_EQ(5.6, CurveManager::CurveValue(2, 1)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_Quartic) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(12345, CurveManager::CurveValue(1, 10)); // In-range value
	EXPECT_DOUBLE_EQ(8265, CurveManager::CurveValue(1, -10)); // In-range value
	EXPECT_DOUBLE_EQ(8265, CurveManager::CurveValue(1, -20.0)); // Minimum x
	EXPECT_DOUBLE_EQ(12345, CurveManager::CurveValue(1, 500)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 8500, 12000);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(8500, CurveManager::CurveValue(1, -10)); // Value too small
	EXPECT_DOUBLE_EQ(12000, CurveManager::CurveValue(1, 10)); // Value too large

	EXPECT_DOUBLE_EQ(8500, CurveManager::CurveValue(2, -10)); // Value too small
	EXPECT_DOUBLE_EQ(12000, CurveManager::CurveValue(2, 10)); // Value too large
	EXPECT_DOUBLE_EQ(11885.1881, CurveManager::CurveValue(2, 9.9)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_ExponentialSkewNormal) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0)); // In-range value
	EXPECT_DOUBLE_EQ(3.67264371702005, CurveManager::CurveValue(1, 1)); // In-range value
	EXPECT_DOUBLE_EQ(3.24109670456697, CurveManager::CurveValue(1, 2)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, -20.0)); // Minimum x
	EXPECT_DOUBLE_EQ(3.24109670456697, CurveManager::CurveValue(1, 500)); // Maximum x
	CurveManager::SetCurveOutputMinMaxValues(1, error, 2, 3.5);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(2, CurveManager::CurveValue(1, 0)); // Value too small
	EXPECT_DOUBLE_EQ(3.5, CurveManager::CurveValue(1, 1)); // Value too large

	EXPECT_DOUBLE_EQ(2, CurveManager::CurveValue(2, 0)); // Value too small
	EXPECT_DOUBLE_EQ(3.5, CurveManager::CurveValue(2, 1)); // Value too large
	EXPECT_DOUBLE_EQ(3.24109670456697, CurveManager::CurveValue(2, 2)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_BiQuadratic) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0, 0)); // In-range value
	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 2, -2)); // In-range value
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, -1, 0)); // Minimum x
	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 3, -2)); // Maximum x
	EXPECT_DOUBLE_EQ(5.0, CurveManager::CurveValue(1, 2, -3)); // Minimum y
	EXPECT_DOUBLE_EQ(1.0, CurveManager::CurveValue(1, 0, 1)); // Maximum y

	CurveManager::SetCurveOutputMinMaxValues(1, error, 1.5, 4.5);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(1, 0, 0)); // Value too small
	EXPECT_DOUBLE_EQ(4.5, CurveManager::CurveValue(1, 2, -2)); // Value too large

	EXPECT_DOUBLE_EQ(1.5, CurveManager::CurveValue(2, 0, 0)); // Value too small
	EXPECT_DOUBLE_EQ(4.5, CurveManager::CurveValue(2, 2, -2)); // Value too large
	EXPECT_DOUBLE_EQ(2.75, CurveManager::CurveValue(2, 0.5, 0)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_BiCubic) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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
	
	EXPECT_DOUBLE_EQ(609.0, CurveManager::CurveValue(1, 0, 4)); // In-range value
	EXPECT_DOUBLE_EQ(4885.0, CurveManager::CurveValue(1, 4, 6)); // In-range value
	EXPECT_DOUBLE_EQ(609.0, CurveManager::CurveValue(1, -1, 4)); // Minimum x
	EXPECT_DOUBLE_EQ(4885.0, CurveManager::CurveValue(1, 5, 6)); // Maximum x
	EXPECT_DOUBLE_EQ(609.0, CurveManager::CurveValue(1, 0, 3)); // Minimum y
	EXPECT_DOUBLE_EQ(4885.0, CurveManager::CurveValue(1, 4, 10)); // Maximum y

	CurveManager::SetCurveOutputMinMaxValues(1, error, 610, 4880);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(610.0, CurveManager::CurveValue(1, 0, 4)); // Value too small
	EXPECT_DOUBLE_EQ(4880.0, CurveManager::CurveValue(1, 4, 6)); // Value too large

	EXPECT_DOUBLE_EQ(610.0, CurveManager::CurveValue(2, 0, 4)); // Value too small
	EXPECT_DOUBLE_EQ(4880.0, CurveManager::CurveValue(2, 4, 6)); // Value too large
	EXPECT_DOUBLE_EQ(3829.0, CurveManager::CurveValue(2, 3, 6)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_QuadraticLinear) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(17.0, CurveManager::CurveValue(1, 0, 4)); // In-range value
	EXPECT_DOUBLE_EQ(777.0, CurveManager::CurveValue(1, 4, 6)); // In-range value
	EXPECT_DOUBLE_EQ(17.0, CurveManager::CurveValue(1, -1, 4)); // Minimum x
	EXPECT_DOUBLE_EQ(777.0, CurveManager::CurveValue(1, 5, 6)); // Maximum x
	EXPECT_DOUBLE_EQ(17.0, CurveManager::CurveValue(1, 0, 3)); // Minimum y
	EXPECT_DOUBLE_EQ(777.0, CurveManager::CurveValue(1, 4, 10)); // Maximum y

	CurveManager::SetCurveOutputMinMaxValues(1, error, 20, 700);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, 0, 4)); // Value too small
	EXPECT_DOUBLE_EQ(700.0, CurveManager::CurveValue(1, 4, 6)); // Value too large

	EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(2, 0, 4)); // Value too small
	EXPECT_DOUBLE_EQ(700.0, CurveManager::CurveValue(2, 4, 6)); // Value too large
	EXPECT_DOUBLE_EQ(472.0, CurveManager::CurveValue(2, 3, 6)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_CubicLinear) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(21.0, CurveManager::CurveValue(1, 0, 4)); // In-range value
	EXPECT_DOUBLE_EQ(487.0, CurveManager::CurveValue(1, 4, 6)); // In-range value
	EXPECT_DOUBLE_EQ(21.0, CurveManager::CurveValue(1, -1, 4)); // Minimum x
	EXPECT_DOUBLE_EQ(487.0, CurveManager::CurveValue(1, 5, 6)); // Maximum x
	EXPECT_DOUBLE_EQ(21.0, CurveManager::CurveValue(1, 0, 3)); // Minimum y
	EXPECT_DOUBLE_EQ(487.0, CurveManager::CurveValue(1, 4, 10)); // Maximum y

	CurveManager::SetCurveOutputMinMaxValues(1, error, 30, 480);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(30.0, CurveManager::CurveValue(1, 0, 4)); // Value too small
	EXPECT_DOUBLE_EQ(480.0, CurveManager::CurveValue(1, 4, 6)); // Value too large

	EXPECT_DOUBLE_EQ(30.0, CurveManager::CurveValue(2, 0, 4)); // Value too small
	EXPECT_DOUBLE_EQ(480.0, CurveManager::CurveValue(2, 4, 6)); // Value too large
	EXPECT_DOUBLE_EQ(280.0, CurveManager::CurveValue(2, 3, 6)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_FanPressureRise) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"277000;                        !- Maximum Curve Output" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(16000.0, CurveManager::CurveValue(1, 0, 4000)); // In-range value
	EXPECT_DOUBLE_EQ(277751.600308978, CurveManager::CurveValue(1, 400, 6000)); // In-range value
	EXPECT_DOUBLE_EQ(16000.0, CurveManager::CurveValue(1, -1, 4000)); // Minimum x
	EXPECT_DOUBLE_EQ(277751.600308978, CurveManager::CurveValue(1, 500, 6000)); // Maximum x
	EXPECT_DOUBLE_EQ(16000.0, CurveManager::CurveValue(1, 0, 3000)); // Minimum y
	EXPECT_DOUBLE_EQ(277751.600308978, CurveManager::CurveValue(1, 400, 7000)); // Maximum y

	CurveManager::SetCurveOutputMinMaxValues(1, error, 20000, 277000);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(20000.0, CurveManager::CurveValue(1, 0, 4000)); // Value too small
	EXPECT_DOUBLE_EQ(277000, CurveManager::CurveValue(1, 400, 6000)); // Value too large

	EXPECT_DOUBLE_EQ(20000, CurveManager::CurveValue(2, 0, 4000)); // Value too small
	EXPECT_DOUBLE_EQ(277000, CurveManager::CurveValue(2, 400, 6000)); // Value too large
	EXPECT_DOUBLE_EQ(28086.83298050514, CurveManager::CurveValue(2, 50, 4000)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_TriQuadratic) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(4228.0, CurveManager::CurveValue(1, 0, 4, 3)); // In-range value
	EXPECT_DOUBLE_EQ(1332.0, CurveManager::CurveValue(1, 0, 2, 3)); // In-range value
	EXPECT_DOUBLE_EQ(91874.0, CurveManager::CurveValue(1, 2, 4, 5)); // In-range value
	EXPECT_DOUBLE_EQ(4228.0, CurveManager::CurveValue(1, -1, 4, 3)); // Minimum x
	EXPECT_DOUBLE_EQ(91874.0, CurveManager::CurveValue(1, 3, 4, 5)); // Maximum x
	EXPECT_DOUBLE_EQ(1332.0, CurveManager::CurveValue(1, 0, 1, 3)); // Minimum y
	EXPECT_DOUBLE_EQ(91874.0, CurveManager::CurveValue(1, 2, 5, 5)); // Maximum y
	EXPECT_DOUBLE_EQ(1332.0, CurveManager::CurveValue(1, 0, 2, 2)); // Minimum z
	EXPECT_DOUBLE_EQ(91874.0, CurveManager::CurveValue(1, 2, 4, 6)); // Maximum z

	CurveManager::SetCurveOutputMinMaxValues(1, error, 3000, 90000);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(3000.0, CurveManager::CurveValue(1, 0, 2, 3)); // Value too small
	EXPECT_DOUBLE_EQ(90000.0, CurveManager::CurveValue(1, 2, 4, 5)); // Value too large

	EXPECT_DOUBLE_EQ(3000.0, CurveManager::CurveValue(2, 0, 2, 3)); // Value too small
	EXPECT_DOUBLE_EQ(90000.0, CurveManager::CurveValue(2, 2, 4, 6)); // Value too large
	EXPECT_DOUBLE_EQ(4228.0, CurveManager::CurveValue(2, 0, 4, 3)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_ChillerPartLoadWithLift) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Output Unit Type" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
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

	EXPECT_DOUBLE_EQ(2913.0, CurveManager::CurveValue(1, 0, 4, 3)); // In-range value
	EXPECT_DOUBLE_EQ(381.0, CurveManager::CurveValue(1, 0, 2, 3)); // In-range value
	EXPECT_DOUBLE_EQ(5737.0, CurveManager::CurveValue(1, 2, 4, 5)); // In-range value
	EXPECT_DOUBLE_EQ(2913.0, CurveManager::CurveValue(1, -1, 4, 3)); // Minimum x
	EXPECT_DOUBLE_EQ(5737.0, CurveManager::CurveValue(1, 3, 4, 5)); // Maximum x
	EXPECT_DOUBLE_EQ(381.0, CurveManager::CurveValue(1, 0, 1, 3)); // Minimum y
	EXPECT_DOUBLE_EQ(5737.0, CurveManager::CurveValue(1, 2, 5, 5)); // Maximum y
	EXPECT_DOUBLE_EQ(381.0, CurveManager::CurveValue(1, 0, 2, 2)); // Minimum z
	EXPECT_DOUBLE_EQ(5737.0, CurveManager::CurveValue(1, 2, 4, 6)); // Maximum z

	CurveManager::SetCurveOutputMinMaxValues(1, error, 400, 4000);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(400.0, CurveManager::CurveValue(1, 0, 2, 3)); // Value too small
	EXPECT_DOUBLE_EQ(4000.0, CurveManager::CurveValue(1, 2, 4, 5)); // Value too large

	EXPECT_DOUBLE_EQ(400.0, CurveManager::CurveValue(2, 0, 2, 3)); // Value too small
	EXPECT_DOUBLE_EQ(4000.0, CurveManager::CurveValue(2, 2, 4, 5)); // Value too large
	EXPECT_DOUBLE_EQ(2913.0, CurveManager::CurveValue(2, 0, 4, 3)); // In-range value

	EXPECT_FALSE(has_err_output());
}

TEST_F(EnergyPlusFixture, Curves_QuadLinear) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
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
		"Dimensionless;           !- Input Unit Type for z" });

	ASSERT_FALSE(process_idf(idf_objects));

	EXPECT_EQ(0, CurveManager::NumCurves);
	CurveManager::GetCurveInput();
	CurveManager::GetCurvesInputFlag = false;
	ASSERT_EQ(2, CurveManager::NumCurves);
	EXPECT_EQ("QUADLINEAR", CurveManager::GetCurveType(1));
	EXPECT_EQ("TESTCURVE1", CurveManager::GetCurveName(1));
	EXPECT_EQ(1, CurveManager::GetCurveIndex("TESTCURVE1"));
	bool error = false;
	int index = CurveManager::GetCurveCheck("TESTCURVE1", error, "TEST");
	EXPECT_FALSE(error);
	EXPECT_EQ(1, index);
	Real64 min1, max1, min2, max2, min3, max3; // , min4, max4;
	CurveManager::GetCurveMinMaxValues(1, min1, max1, min2, max2, min3, max3); //, min4, max4);
	EXPECT_EQ(-2, min1);
	EXPECT_EQ(0, max1);
	EXPECT_EQ(0, min2);
	EXPECT_EQ(2, max2);
	EXPECT_EQ(2, min3);
	EXPECT_EQ(4, max3);
	//EXPECT_EQ(3, min4);
	//EXPECT_EQ(5, max4);
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
	//EXPECT_EQ(3, min4);
	//EXPECT_EQ(5, max4);
	EXPECT_EQ(CurveManager::CurveType_QuadLinear, CurveManager::GetCurveObjectTypeNum(2));

	EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -2, 0, 2, 3)); // In-range value
	EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 0, 2, 4, 5)); // In-range value
	EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -3, 0, 2, 3)); // Minimum w
	EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 1, 2, 4, 5)); // Maximum w
	EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -2, -1, 2, 3)); // Minimum x
	EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 0, 7, 4, 5)); // Maximum x
	EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -2, 0, 0, 3)); // Minimum y
	EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 0, 2, 5, 5)); // Maximum y
	EXPECT_DOUBLE_EQ(20.0, CurveManager::CurveValue(1, -2, 0, 2, 0)); // Minimum z
	EXPECT_DOUBLE_EQ(48.0, CurveManager::CurveValue(1, 0, 2, 4, 7)); // Maximum z

	CurveManager::SetCurveOutputMinMaxValues(1, error, 25, 40);
	EXPECT_FALSE(error);
	EXPECT_DOUBLE_EQ(25.0, CurveManager::CurveValue(1, -2, 0, 2, 3)); // Value too small
	EXPECT_DOUBLE_EQ(40.0, CurveManager::CurveValue(1, 0, 2, 4, 5)); // Value too large

	EXPECT_DOUBLE_EQ(25.0, CurveManager::CurveValue(2, -2, 0, 2, 3)); // Value too small
	EXPECT_DOUBLE_EQ(40.0, CurveManager::CurveValue(2, 0, 2, 4, 5)); // Value too large
	EXPECT_DOUBLE_EQ(27.0, CurveManager::CurveValue(2, 0, 1, 2, 3)); // In-range value

	EXPECT_FALSE(has_err_output());
}