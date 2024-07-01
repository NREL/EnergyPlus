// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::RoomAirModelUserTempPattern Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::OutAirNodeManager;

TEST_F(EnergyPlusFixture, OutAirNodeManager_OATdbTwbOverrideTest)
{
    state->dataOutAirNodeMgr->NumOutsideAirNodes = 3;
    state->dataOutAirNodeMgr->OutsideAirNodeList.allocate(3);

    auto &dln = state->dataLoopNodes;
    for (int i = 0; i < 3; ++i) dln->nodes.push_back(new Node::NodeData);

    state->dataScheduleMgr->Schedule.allocate(2);

    state->dataEnvrn->OutDryBulbTemp = 25.0;
    state->dataEnvrn->OutWetBulbTemp = 15.0;
    state->dataEnvrn->WindSpeed = 2.0;
    state->dataEnvrn->WindDir = 0.0;
    state->dataEnvrn->OutBaroPress = 101325;
    state->dataEnvrn->OutHumRat =
        Psychrometrics::PsyWFnTdbTwbPb(*state, state->dataEnvrn->OutDryBulbTemp, state->dataEnvrn->OutWetBulbTemp, state->dataEnvrn->OutBaroPress);

    state->dataScheduleMgr->Schedule(1).CurrentValue = 24.0;
    state->dataOutAirNodeMgr->OutsideAirNodeList(1) = 1;
    state->dataOutAirNodeMgr->OutsideAirNodeList(2) = 2;
    state->dataOutAirNodeMgr->OutsideAirNodeList(3) = 3;
    // Scheduled value
    dln->nodes(1)->IsLocalNode = true;
    dln->nodes(1)->OutAirDryBulbSchedNum = 1;
    dln->nodes(1)->OutAirDryBulb = state->dataEnvrn->OutDryBulbTemp;
    dln->nodes(1)->OutAirWetBulb = state->dataEnvrn->OutWetBulbTemp;
    // EMS override value
    dln->nodes(2)->IsLocalNode = true;
    dln->nodes(2)->EMSOverrideOutAirDryBulb = true;
    dln->nodes(2)->EMSOverrideOutAirWetBulb = true;
    dln->nodes(2)->EMSValueForOutAirDryBulb = 26.0;
    dln->nodes(2)->EMSValueForOutAirWetBulb = 16.0;
    dln->nodes(2)->OutAirDryBulb = state->dataEnvrn->OutDryBulbTemp;
    dln->nodes(2)->OutAirWetBulb = state->dataEnvrn->OutWetBulbTemp;
    // No changes
    dln->nodes(3)->OutAirDryBulb = state->dataEnvrn->OutDryBulbTemp;
    dln->nodes(3)->OutAirWetBulb = state->dataEnvrn->OutWetBulbTemp;

    InitOutAirNodes(*state);

    EXPECT_NEAR(14.6467, dln->nodes(1)->OutAirWetBulb, 0.0001);
    EXPECT_NEAR(0.007253013, dln->nodes(2)->HumRat, 0.000001);
    EXPECT_NEAR(0.006543816, dln->nodes(3)->HumRat, 0.000001);
}
