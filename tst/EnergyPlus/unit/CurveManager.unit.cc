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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <CurveManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::CurveManager;

TEST_F( EnergyPlusFixture, Tables_TwoIndVar_Malformed ) {

	std::string const idf_objects = delimited_string( {
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
		 } );

	ASSERT_FALSE( process_idf( idf_objects ) );

	bool ErrorsFound = false;
	CurveManager::GetCurveInputData( ErrorsFound );
	EXPECT_TRUE( ErrorsFound );

	EXPECT_EQ( 1, CurveManager::NumCurves );

	std::string const error_string = delimited_string( {
		"   ** Severe  ** GetCurveInput: For Table:TwoIndependentVariables: AWHP_RCAP",
		"   **   ~~~   ** The number of data entries must be evenly divisable by 3. Number of data entries = 134",
	} );

	EXPECT_TRUE( compare_err_stream( error_string, true ) );

}




