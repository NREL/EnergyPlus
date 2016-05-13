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
}
