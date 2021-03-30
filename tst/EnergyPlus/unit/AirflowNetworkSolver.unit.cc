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

// EnergyPlus::AirflowNetworkSolver unit tests

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace AirflowNetworkBalanceManager;
using namespace AirflowNetwork;

TEST_F(EnergyPlusFixture, AirflowNetwork_SolverTest_HorizontalOpening)
{

    int i = 1;
    int j = 1;
    int n;
    int m;
    int NF;
    std::array<Real64, 2> F{{0.0, 0.0}};
    std::array<Real64, 2> DF{{0.0, 0.0}};

    n = 1;
    m = 2;

    state->dataAirflowNetwork->AirflowNetworkCompData.allocate(j);
    state->dataAirflowNetwork->AirflowNetworkCompData(j).TypeNum = 1;
    state->dataAirflowNetwork->MultizoneSurfaceData.allocate(i);
    state->dataAirflowNetwork->MultizoneSurfaceData(i).Width = 10.0;
    state->dataAirflowNetwork->MultizoneSurfaceData(i).Height = 5.0;
    state->dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor = 1.0;

    state->dataAFNSolver->solver.properties.clear();
    for (int it = 0; it < 2; ++it)
        state->dataAFNSolver->solver.properties.emplace_back(AirProperties(AIRDENSITY(*state, 20.0, 101325.0, 0.0)));
    state->dataAFNSolver->solver.properties[0].density = 1.2;
    state->dataAFNSolver->solver.properties[1].density = 1.18;

    state->dataAirflowNetwork->MultizoneCompHorOpeningData.allocate(1);
    state->dataAirflowNetwork->MultizoneCompHorOpeningData(1).FlowCoef = 0.1;
    state->dataAirflowNetwork->MultizoneCompHorOpeningData(1).FlowExpo = 0.5;
    state->dataAirflowNetwork->MultizoneCompHorOpeningData(1).Slope = 90.0;
    state->dataAirflowNetwork->MultizoneCompHorOpeningData(1).DischCoeff = 0.2;

    state->dataAirflowNetwork->AirflowNetworkLinkageData.allocate(i);
    state->dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0] = 4.0;
    state->dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1] = 2.0;

    Real64 multiplier = 1.0;
    Real64 control = 1.0;

    NF = state->dataAirflowNetwork->MultizoneCompHorOpeningData(1).calculate(
        *state, 1, 0.05, 1, multiplier, control, state->dataAFNSolver->solver.properties[0], state->dataAFNSolver->solver.properties[1], F, DF);
    EXPECT_NEAR(3.47863, F[0], 0.00001);
    EXPECT_NEAR(34.7863, DF[0], 0.0001);
    EXPECT_NEAR(2.96657, F[1], 0.00001);
    EXPECT_EQ(0.0, DF[1]);

    NF = state->dataAirflowNetwork->MultizoneCompHorOpeningData(1).calculate(
        *state, 1, -0.05, 1, multiplier, control, state->dataAFNSolver->solver.properties[0], state->dataAFNSolver->solver.properties[1], F, DF);
    EXPECT_NEAR(-3.42065, F[0], 0.00001);
    EXPECT_NEAR(34.20649, DF[0], 0.0001);
    EXPECT_NEAR(2.96657, F[1], 0.00001);
    EXPECT_EQ(0.0, DF[1]);

    state->dataAirflowNetwork->AirflowNetworkLinkageData.deallocate();

    state->dataAirflowNetwork->MultizoneCompHorOpeningData.deallocate();
    state->dataAirflowNetwork->MultizoneSurfaceData.deallocate();
    state->dataAirflowNetwork->AirflowNetworkCompData.deallocate();
}

TEST_F(EnergyPlusFixture, AirflowNetwork_SolverTest_Coil)
{

    int NF;
    std::array<Real64, 2> F;
    std::array<Real64, 2> DF;

    state->dataAirflowNetwork->AirflowNetworkCompData.allocate(1);
    state->dataAirflowNetwork->AirflowNetworkCompData[0].TypeNum = 1;

    state->dataAirflowNetwork->DisSysCompCoilData.allocate(1);
    state->dataAirflowNetwork->DisSysCompCoilData[0].hydraulicDiameter = 1.0;
    state->dataAirflowNetwork->DisSysCompCoilData[0].L = 1.0;

    state->dataAFNSolver->solver.properties.clear();
    for (int it = 0; it < 2; ++it)
        state->dataAFNSolver->solver.properties.emplace_back(AirProperties(AIRDENSITY(*state, 20.0, 101325.0, 0.0)));
    state->dataAFNSolver->solver.properties[0].density = 1.2;
    state->dataAFNSolver->solver.properties[1].density = 1.2;

    state->dataAFNSolver->solver.properties[0].viscosity = 1.0e-5;
    state->dataAFNSolver->solver.properties[1].viscosity = 1.0e-5;

    F[1] = DF[1] = 0.0;

    Real64 multiplier = 1.0;
    Real64 control = 1.0;

    NF = state->dataAirflowNetwork->DisSysCompCoilData[0].calculate(
        *state, 1, 0.05, 1, multiplier, control, state->dataAFNSolver->solver.properties[0], state->dataAFNSolver->solver.properties[1], F, DF);
    EXPECT_NEAR(-294.5243112740431, F[0], 0.00001);
    EXPECT_NEAR(5890.4862254808613, DF[0], 0.0001);
    EXPECT_EQ(0.0, F[1]);
    EXPECT_EQ(0.0, DF[1]);

    NF = state->dataAirflowNetwork->DisSysCompCoilData[0].calculate(
        *state, 1, -0.05, 1, multiplier, control, state->dataAFNSolver->solver.properties[0], state->dataAFNSolver->solver.properties[1], F, DF);
    EXPECT_NEAR(294.5243112740431, F[0], 0.00001);
    EXPECT_NEAR(5890.4862254808613, DF[0], 0.0001);
    EXPECT_EQ(0.0, F[1]);
    EXPECT_EQ(0.0, DF[1]);

    state->dataAirflowNetwork->DisSysCompCoilData.deallocate();
    state->dataAirflowNetwork->AirflowNetworkCompData.deallocate();
}
