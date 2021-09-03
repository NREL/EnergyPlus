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

// EnergyPlus::EarthTube Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/EcoRoofManager.hh>
#include <EnergyPlus/SolarShading.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EcoRoofManager;
using namespace EnergyPlus::SolarShading;
using namespace EnergyPlus::DataEnvironment;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, EcoRoof_CalculateEcoRoofSolarTest)
{
    Real64 resultRS;
    Real64 resultf1;
    Real64 expectedRS;
    Real64 expectedf1;
    int SurfNum = 1;
    state->dataSolarShading->SurfAnisoSkyMult.allocate(SurfNum);

    // Test 1: SOLCOS negative
    state->dataEnvrn->SOLCOS(3) = -1.0;
    state->dataEnvrn->BeamSolarRad = 321.0;
    state->dataSolarShading->SurfAnisoSkyMult(SurfNum) = 0.5;
    state->dataEnvrn->DifSolarRad = 124.0;
    expectedRS = 62.0;
    expectedf1 = 3.9956;
    CalculateEcoRoofSolar(*state, resultRS, resultf1, SurfNum);
    EXPECT_NEAR(resultRS, expectedRS, 0.001);
    EXPECT_NEAR(resultf1, expectedf1, 0.001);

    // Test 2: SOLCOS positive
    state->dataEnvrn->SOLCOS(3) = 0.7;
    state->dataEnvrn->BeamSolarRad = 400.0;
    state->dataSolarShading->SurfAnisoSkyMult(SurfNum) = 0.6;
    state->dataEnvrn->DifSolarRad = 100.0;
    expectedRS = 340.0;
    expectedf1 = 1.4004;
    CalculateEcoRoofSolar(*state, resultRS, resultf1, SurfNum);
    EXPECT_NEAR(resultRS, expectedRS, 0.001);
    EXPECT_NEAR(resultf1, expectedf1, 0.001);

    // Test 3: inverse of f1 greater than 1
    state->dataEnvrn->SOLCOS(3) = 1.0;
    state->dataEnvrn->BeamSolarRad = 1500.0;
    state->dataSolarShading->SurfAnisoSkyMult(SurfNum) = 0.0;
    state->dataEnvrn->DifSolarRad = 0.0;
    expectedRS = 1500.0;
    expectedf1 = 1.0;
    CalculateEcoRoofSolar(*state, resultRS, resultf1, SurfNum);
    EXPECT_NEAR(resultRS, expectedRS, 0.001);
    EXPECT_NEAR(resultf1, expectedf1, 0.001);
}

} // namespace EnergyPlus
