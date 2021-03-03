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

// EnergyPlus::Low Temperature Radiant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/IceThermalStorage.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::IceThermalStorage;
using namespace ObjexxFCL;
using namespace CurveManager;
using namespace EnergyPlus::General;

TEST_F(EnergyPlusFixture, IceThermalStorage_CalcQstarTest)
{
    int TotDetailedIce = 4;
    int TotCurves = 4;
    enum CurveVars IceStorageCurveType;
    int TestNum;
    Real64 CurveAnswer = 0.0;
    Real64 ExpectedValue = 0.0;
    Real64 Tolerance = 0.001;

    state->dataIceThermalStorage->DetailedIceStorage.allocate(TotDetailedIce);
    state->dataCurveManager->PerfCurve.allocate(TotCurves);
    state->dataCurveManager->NumCurves = TotCurves;
    state->dataGlobal->BeginEnvrnFlag = false;

    // Test 1: CurveVarsFracChargedLMTD Curve is QuadraticLinear
    TestNum = 1;
    IceStorageCurveType = IceThermalStorage::CurveVars::FracChargedLMTD;
    state->dataCurveManager->PerfCurve(TestNum).CurveType = CurveTypeEnum::QuadraticLinear;
    state->dataCurveManager->PerfCurve(TestNum).InterpolationType = CurveManager::InterpTypeEnum::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(TestNum).Var1Max = 1.0;
    state->dataCurveManager->PerfCurve(TestNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(TestNum).Var2Max = 10.0;
    state->dataCurveManager->PerfCurve(TestNum).Var2Min = 0.0;
    state->dataCurveManager->PerfCurve(TestNum).Coeff1 = 0.1;
    state->dataCurveManager->PerfCurve(TestNum).Coeff2 = 0.2;
    state->dataCurveManager->PerfCurve(TestNum).Coeff3 = 0.3;
    state->dataCurveManager->PerfCurve(TestNum).Coeff4 = 0.4;
    state->dataCurveManager->PerfCurve(TestNum).Coeff5 = 0.5;
    state->dataCurveManager->PerfCurve(TestNum).Coeff6 = 0.6;
    CurveAnswer = IceThermalStorage::CalcQstar(*state, TestNum, IceStorageCurveType, 0.5, 1.5, 0.25);
    ExpectedValue = 1.475;
    EXPECT_NEAR(ExpectedValue, CurveAnswer, Tolerance);

    // Test 2: CurveVarsFracDischargedLMTD Curve is BiQuadratic
    TestNum = 2;
    IceStorageCurveType = IceThermalStorage::CurveVars::FracDischargedLMTD;
    state->dataCurveManager->PerfCurve(TestNum).CurveType = CurveTypeEnum::BiQuadratic;
    state->dataCurveManager->PerfCurve(TestNum).InterpolationType = CurveManager::InterpTypeEnum::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(TestNum).Var1Max = 1.0;
    state->dataCurveManager->PerfCurve(TestNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(TestNum).Var2Max = 10.0;
    state->dataCurveManager->PerfCurve(TestNum).Var2Min = 0.0;
    state->dataCurveManager->PerfCurve(TestNum).Coeff1 = 0.1;
    state->dataCurveManager->PerfCurve(TestNum).Coeff2 = 0.2;
    state->dataCurveManager->PerfCurve(TestNum).Coeff3 = 0.3;
    state->dataCurveManager->PerfCurve(TestNum).Coeff4 = 0.4;
    state->dataCurveManager->PerfCurve(TestNum).Coeff5 = 0.5;
    state->dataCurveManager->PerfCurve(TestNum).Coeff6 = 0.6;
    CurveAnswer = IceThermalStorage::CalcQstar(*state, TestNum, IceStorageCurveType, 0.4, 1.2, 0.25);
    ExpectedValue = 1.960;
    EXPECT_NEAR(ExpectedValue, CurveAnswer, Tolerance);

    // Test 3: CurveVarsLMTDMassFlow Curve is CubicLinear
    TestNum = 3;
    IceStorageCurveType = IceThermalStorage::CurveVars::LMTDMassFlow;
    state->dataCurveManager->PerfCurve(TestNum).CurveType = CurveTypeEnum::CubicLinear;
    state->dataCurveManager->PerfCurve(TestNum).InterpolationType = CurveManager::InterpTypeEnum::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(TestNum).Var1Max = 10.0;
    state->dataCurveManager->PerfCurve(TestNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(TestNum).Var2Max = 1.0;
    state->dataCurveManager->PerfCurve(TestNum).Var2Min = 0.0;
    state->dataCurveManager->PerfCurve(TestNum).Coeff1 = 0.1;
    state->dataCurveManager->PerfCurve(TestNum).Coeff2 = 0.2;
    state->dataCurveManager->PerfCurve(TestNum).Coeff3 = 0.3;
    state->dataCurveManager->PerfCurve(TestNum).Coeff4 = 0.4;
    state->dataCurveManager->PerfCurve(TestNum).Coeff5 = 0.5;
    state->dataCurveManager->PerfCurve(TestNum).Coeff6 = 0.6;
    CurveAnswer = IceThermalStorage::CalcQstar(*state, TestNum, IceStorageCurveType, 0.4, 1.2, 0.25);
    ExpectedValue = 1.768;
    EXPECT_NEAR(ExpectedValue, CurveAnswer, Tolerance);

    // Test 4: CurveVarsFracLMTDFracCharged Curve is CubicLinear
    TestNum = 4;
    IceStorageCurveType = IceThermalStorage::CurveVars::LMTDFracCharged;
    state->dataCurveManager->PerfCurve(TestNum).CurveType = CurveTypeEnum::CubicLinear;
    state->dataCurveManager->PerfCurve(TestNum).InterpolationType = CurveManager::InterpTypeEnum::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(TestNum).Var1Max = 10.0;
    state->dataCurveManager->PerfCurve(TestNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(TestNum).Var2Max = 1.0;
    state->dataCurveManager->PerfCurve(TestNum).Var2Min = 0.0;
    state->dataCurveManager->PerfCurve(TestNum).Coeff1 = 0.1;
    state->dataCurveManager->PerfCurve(TestNum).Coeff2 = 0.2;
    state->dataCurveManager->PerfCurve(TestNum).Coeff3 = 0.3;
    state->dataCurveManager->PerfCurve(TestNum).Coeff4 = 0.4;
    state->dataCurveManager->PerfCurve(TestNum).Coeff5 = 0.5;
    state->dataCurveManager->PerfCurve(TestNum).Coeff6 = 0.6;
    CurveAnswer = IceThermalStorage::CalcQstar(*state, TestNum, IceStorageCurveType, 0.4, 1.2, 0.25);
    ExpectedValue = 1.951;
    EXPECT_NEAR(ExpectedValue, CurveAnswer, Tolerance);
}
