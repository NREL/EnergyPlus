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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/PlantCondLoopOperation.hh>
#include <EnergyPlus/SetPointManager.hh>

using namespace EnergyPlus;

class DistributePlantLoadTest : public EnergyPlusFixture
{

public:
    //    static void SetUpTestCase()
    //    {
    //        EnergyPlusFixture::SetUpTestCase(); // Sets up the base fixture
    //    }
    static void TearDownTestCase()
    {
    }

    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up individual test cases.

        // unit test for plant equipment list load distribution
        // set up one plantloop side with 1 branches, 12 components
        // using 12 components here to test going beyond the old idd limit of 10 pieces of equipment
        state->dataPlnt->PlantLoop.allocate(1);
        state->dataPlnt->PlantLoop(1).OpScheme.allocate(1);
        state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList.allocate(1);
        auto &thisEquipList(state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1));
        thisEquipList.NumComps = 12;
        thisEquipList.Comp.allocate(thisEquipList.NumComps);

        state->dataPlnt->PlantLoop(1).LoopSide.allocate(1);
        state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
        state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(thisEquipList.NumComps);
        auto &thisBranch(state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1));

        for (int compNum = 1; compNum <= state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps; ++compNum) {
            // set up equipment list data
            thisEquipList.Comp(compNum).CompNumPtr = compNum;

            thisEquipList.Comp(compNum).BranchNumPtr = 1;

            // set up individual component data - start with 12 equal size, all available
            thisBranch.Comp(compNum).Available = true;
            thisBranch.Comp(compNum).OptLoad = 90.0;
            thisBranch.Comp(compNum).MaxLoad = 100.0;
            thisBranch.Comp(compNum).MinLoad = 0.0;
            thisBranch.Comp(compNum).MyLoad = 0.0;
        }
    }

    virtual void ResetLoads()
    {
        // reset loads
        auto &thisBranch(state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1));
        for (int compNum = 1; compNum <= state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps; ++compNum) {
            thisBranch.Comp(compNum).MyLoad = 0.0;
        }
    }
    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!
    }
};

TEST_F(DistributePlantLoadTest, DistributePlantLoad_Sequential)
{
    auto &thisBranch(state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1));

    state->dataPlnt->PlantLoop(1).LoadDistribution = DataPlant::iLoadingScheme::Sequential;

    // Loop demand 550W
    DistributePlantLoadTest::ResetLoads();
    Real64 loopDemand = 550.0;
    Real64 remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(6).MyLoad, 50.0);
    EXPECT_EQ(thisBranch.Comp(7).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(8).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(9).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(10).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(11).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(12).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);
    DistributePlantLoadTest::ResetLoads();

    // Loop demand 50W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 50.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 50.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(6).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(7).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(8).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(9).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(10).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(11).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(12).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Loop demand 5000W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 5000.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(6).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(7).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(8).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(9).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(10).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(11).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(12).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 3800.0);

    // Loop demand 550W, even numbered components unavailable
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 550.0;
    remainingLoopDemand = 0.0;
    thisBranch.Comp(2).Available = false;
    thisBranch.Comp(4).Available = false;
    thisBranch.Comp(6).Available = false;
    thisBranch.Comp(8).Available = false;
    thisBranch.Comp(10).Available = false;
    thisBranch.Comp(12).Available = false;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(6).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(7).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(8).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(9).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(10).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(11).MyLoad, 50.0);
    EXPECT_EQ(thisBranch.Comp(12).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Duplicate tests from engineering reference examples for Sequential
    state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 2;
    thisBranch.Comp(1).MaxLoad = 40.0;
    thisBranch.Comp(1).MinLoad = 0.2 * 40.0;
    thisBranch.Comp(1).OptLoad = 0.6 * 40.0;
    thisBranch.Comp(1).Available = true;
    thisBranch.Comp(2).MaxLoad = 100.0;
    thisBranch.Comp(2).MinLoad = 0.15 * 100.0;
    thisBranch.Comp(2).OptLoad = 0.4 * 100.0;
    thisBranch.Comp(2).Available = true;

    // 5W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 5.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 5.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 25W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 25.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 25.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 50W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 50.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 10.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 100W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 100.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 60.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 150W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 150.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);

    // 200W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 200.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 60.0);
}

TEST_F(DistributePlantLoadTest, DistributePlantLoad_Uniform)
{
    auto &thisBranch(state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1));

    state->dataPlnt->PlantLoop(1).LoadDistribution = DataPlant::iLoadingScheme::Uniform;

    // Start with 5 components
    state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 5;

    // Loop demand 550W
    DistributePlantLoadTest::ResetLoads();
    Real64 remainingLoopDemand = 0.0;
    Real64 loopDemand = 550.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 50.0);

    // Loop demand 50W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 50.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 10.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 10.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 10.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 10.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 10.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Loop demand 320W, one smaller equipment
    // "extra" load should be distributed sequentially amongst the other equipment
    // component 3 unavailable
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 320.0;
    thisBranch.Comp(4).MaxLoad = 50.0;
    thisBranch.Comp(3).Available = false;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 90.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 50.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 80.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Duplicate tests from engineering reference examples
    state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 2;
    thisBranch.Comp(1).MaxLoad = 40.0;
    thisBranch.Comp(1).MinLoad = 0.2 * 40.0;
    thisBranch.Comp(1).OptLoad = 0.6 * 40.0;
    thisBranch.Comp(2).MaxLoad = 100.0;
    thisBranch.Comp(2).MinLoad = 0.15 * 100.0;
    thisBranch.Comp(2).OptLoad = 0.4 * 100.0;

    // 10W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 10.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 5.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 5.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 25W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 25.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 12.5);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 12.5);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 50W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 50.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 25.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 25.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 100W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 100.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 60.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 150W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 150.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);

    // 200W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 200.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 60.0);
}

TEST_F(DistributePlantLoadTest, DistributePlantLoad_Optimal)
{
    auto &thisBranch(state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1));

    state->dataPlnt->PlantLoop(1).LoadDistribution = DataPlant::iLoadingScheme::Optimal;

    // Start with 5 components and smaller component 4
    state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 5;
    thisBranch.Comp(4).Available = true;
    thisBranch.Comp(4).OptLoad = 45.0;
    thisBranch.Comp(4).MaxLoad = 50.0;
    thisBranch.Comp(4).MinLoad = 0.0;
    thisBranch.Comp(4).MyLoad = 0.0;

    // Loop demand 550W
    DistributePlantLoadTest::ResetLoads();
    Real64 remainingLoopDemand = 0.0;
    Real64 loopDemand = 550.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 50.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 100.0);

    // Loop demand 50W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 440.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 99.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 97.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 97.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 50.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 97.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Loop demand 320W
    // component 3 unavailable
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 340.0;
    thisBranch.Comp(3).Available = false;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 97.5);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 96.25);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 50.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 96.25);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Duplicate tests from engineering reference examples
    state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 2;
    thisBranch.Comp(1).MaxLoad = 40.0;
    thisBranch.Comp(1).MinLoad = 0.2 * 40.0;
    thisBranch.Comp(1).OptLoad = 0.6 * 40.0;
    thisBranch.Comp(2).MaxLoad = 100.0;
    thisBranch.Comp(2).MinLoad = 0.15 * 100.0;
    thisBranch.Comp(2).OptLoad = 0.4 * 100.0;

    // 5W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 5.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 5.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 25W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 25.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 24.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 1.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 50W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 50.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 24.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 26.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 100W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 100.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 60.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 150W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 150.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);

    // 200W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 200.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 60.0);

    // 200W - no equipment available
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 200.0;
    thisBranch.Comp(1).Available = false;
    thisBranch.Comp(2).Available = false;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 200.0);
}

TEST_F(DistributePlantLoadTest, DistributePlantLoad_UniformPLR)
{
    auto &thisBranch(state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1));

    state->dataPlnt->PlantLoop(1).LoadDistribution = DataPlant::iLoadingScheme::UniformPLR;

    // Start with 5 components and smaller component 4
    state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 5;
    thisBranch.Comp(4).Available = true;
    thisBranch.Comp(4).OptLoad = 45.0;
    thisBranch.Comp(4).MaxLoad = 50.0;
    thisBranch.Comp(4).MinLoad = 0.0;
    thisBranch.Comp(4).MyLoad = 0.0;

    // Loop demand 550W
    DistributePlantLoadTest::ResetLoads();
    Real64 loopDemand = 550.0;
    Real64 remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 50.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 100.0);

    // Loop demand 45W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 45.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 10.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 10.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 10.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 5.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 10.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Loop demand 280W
    // "extra" load should be distributed sequentially amongst the other equipment
    // component 3 unavailable
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 280;
    remainingLoopDemand = 0.0;
    thisBranch.Comp(3).Available = false;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 80.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 80.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 80.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Duplicate tests from engineering reference examples
    state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 2;
    thisBranch.Comp(1).MaxLoad = 40.0;
    thisBranch.Comp(1).MinLoad = 0.2 * 40.0;
    thisBranch.Comp(1).OptLoad = 0.6 * 40.0;
    thisBranch.Comp(2).MaxLoad = 100.0;
    thisBranch.Comp(2).MinLoad = 0.15 * 100.0;
    thisBranch.Comp(2).OptLoad = 0.4 * 100.0;

    // 5W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 5.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 5.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 10W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 10.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 10.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 25W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 25.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisBranch.Comp(1).MyLoad, 25.0, 0.1);
    EXPECT_NEAR(thisBranch.Comp(2).MyLoad, 0.0, 0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 50W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 50.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisBranch.Comp(1).MyLoad, 14.29, 0.1);
    EXPECT_NEAR(thisBranch.Comp(2).MyLoad, 35.71, 0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 100W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 100.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisBranch.Comp(1).MyLoad, 28.57, 0.1);
    EXPECT_NEAR(thisBranch.Comp(2).MyLoad, 71.43, 0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 150W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 150.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);
}

TEST_F(DistributePlantLoadTest, DistributePlantLoad_SequentialUniformPLR)
{
    auto &thisBranch(state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1));

    state->dataPlnt->PlantLoop(1).LoadDistribution = DataPlant::iLoadingScheme::SequentialUniformPLR;

    // Start with 5 components and smaller component 4
    state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 5;
    thisBranch.Comp(4).Available = true;
    thisBranch.Comp(4).OptLoad = 45.0;
    thisBranch.Comp(4).MaxLoad = 50.0;
    thisBranch.Comp(4).MinLoad = 0.0;
    thisBranch.Comp(4).MyLoad = 0.0;

    // Loop demand 550W
    DistributePlantLoadTest::ResetLoads();
    Real64 remainingLoopDemand = 0.0;
    Real64 loopDemand = 550.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 100.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 50.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 100.0);

    // Loop demand 45W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 45.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 45.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Loop demand 225W
    // "extra" load should be distributed sequentially amongst the other equipment
    // component 3 unavailable
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 225;
    remainingLoopDemand = 0.0;
    thisBranch.Comp(3).Available = false;

    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 90.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 90.0);
    EXPECT_EQ(thisBranch.Comp(3).MyLoad, 0.0);
    EXPECT_EQ(thisBranch.Comp(4).MyLoad, 45.0);
    EXPECT_EQ(thisBranch.Comp(5).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // Duplicate tests from engineering reference examples
    state->dataPlnt->PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 2;
    thisBranch.Comp(1).MaxLoad = 40.0;
    thisBranch.Comp(1).MinLoad = 0.2 * 40.0;
    thisBranch.Comp(1).OptLoad = 0.6 * 40.0;
    thisBranch.Comp(2).MaxLoad = 100.0;
    thisBranch.Comp(2).MinLoad = 0.15 * 100.0;
    thisBranch.Comp(2).OptLoad = 0.4 * 100.0;

    // 5W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 5.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 5.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 10W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 10.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 10.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 25W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 25.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 25.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 50W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 50.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisBranch.Comp(1).MyLoad, 14.3, 0.1);
    EXPECT_NEAR(thisBranch.Comp(2).MyLoad, 35.71, 0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 100W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 100.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisBranch.Comp(1).MyLoad, 28.6, 0.1);
    EXPECT_NEAR(thisBranch.Comp(2).MyLoad, 71.43, 0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 150W
    DistributePlantLoadTest::ResetLoads();
    remainingLoopDemand = 0.0;
    loopDemand = 150.0;
    PlantCondLoopOperation::DistributePlantLoad(*state, 1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);
}

TEST_F(EnergyPlusFixture, ThermalEnergyStorageWithIceForceDualOp)
{

    std::string const idf_objects = delimited_string({
        "PlantEquipmentOperation:ThermalEnergyStorage,",
        "  TEST PLANTOP SCHEME,          !- Name",
        "  Ice Thermal Storage On-peak,  !- On-Peak Schedule",
        "  Ice Thermal Storage Charging,  !- Charging Availability Schedule",
        "  7.00,                    !- Non-Charging Chilled Water Temperature {C}",
        "  -5.00,                   !- Charging Chilled Water Temperature {C}",
        "  Chiller:Electric:EIR,    !- Component 1 Object Type",
        "  Chiller,                 !- Component 1 Name",
        "  Primary CHW Loop Pump Water Outlet Node,  !- Component 1 Demand Calculation Node Name",
        "  Chiller CHW Outlet Node, !- Component 1 Setpoint Node Name",
        "  0.001351,                !- Component 1 Flow Rate {m3/s}",
        "  Cooling,                 !- Component 1 Operation Type",
        "  ThermalStorage:Ice:Detailed,  !- Component 2 Object Type",
        "  Ice Thermal Storage,     !- Component 2 Name",
        "  Chiller CHW Outlet Node, !- Component 2 Demand Calculation Node Name",
        "  Ice Thermal Storage Water Outlet Node,  !- Component 2 Setpoint Node Name",
        "  autosize,                !- Component 2 Flow Rate {m3/s}",
        "  Cooling;                 !- Component 2 Operation Type",
        "",

        "Schedule:Compact,",
        "  Ice Thermal Storage On-peak,  !- Name",
        "  Fraction,                !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 08:00,0,          !- Field 3",
        "  Until: 18:00,1,          !- Field 4",
        "  Until: 24:00,0;          !- Field 5",

        "Schedule:Compact,",
        "  Ice Thermal Storage Charging,  !- Name",
        "  Fraction,                !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 08:00,1,          !- Field 3",
        "  Until: 18:00,0,          !- Field 4",
        "  Until: 24:00,1;          !- Field 5",

        "ScheduleTypeLimits,",
        "  Fraction,                !- Name",
        "  0.0,                     !- Lower Limit Value",
        "  1.0,                     !- Upper Limit Value",
        "  CONTINUOUS,              !- Numeric Type",
        "  Dimensionless;           !- Unit Type",

    });

    EXPECT_TRUE(process_idf(idf_objects, false));

    // Setup the plant itself manually
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(1);

    state->dataPlnt->PlantLoop(1).OpScheme.allocate(1);
    state->dataPlnt->PlantLoop(1).OpScheme(1).Name = "TEST PLANTOP SCHEME";

    state->dataSetPointManager->NumAllSetPtMgrs = 0;
    state->dataSetPointManager->NumSchTESSetPtMgrs = 0;

    bool ErrorsFound = false;
    int TESSPBO = 1;
    int LoopNum = 1;
    int SchemeNum = 1;
    std::string CurrentModuleObject = "PlantEquipmentOperation:ThermalEnergyStorage";
    PlantCondLoopOperation::FindCompSPInput(*state, CurrentModuleObject, TESSPBO, LoopNum, SchemeNum, ErrorsFound);

    EXPECT_FALSE(ErrorsFound);

    std::string const error_string = delimited_string({
        "   ** Warning ** Equipment Operation Mode was reset to 'DUAL' for Component 'ICE THERMAL STORAGE' in "
        "PlantEquipmentOperation:ThermalEnergyStorage='TEST PLANTOP SCHEME'.",
        "   **   ~~~   ** Equipment Operation Mode can only be 'DUAL' for THERMALSTORAGE:ICE:DETAILED objects.",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));

    // Might as well check that the Chiller is also Ok
    {
        int CompNum = 1;
        std::string compName = state->dataPlnt->PlantLoop(LoopNum).OpScheme(SchemeNum).EquipList(1).Comp(CompNum).Name;
        EXPECT_EQ(compName, "CHILLER");
        auto CtrlTypeNum = state->dataPlnt->PlantLoop(LoopNum).OpScheme(SchemeNum).EquipList(1).Comp(CompNum).CtrlTypeNum;
        EXPECT_EQ(CtrlTypeNum, DataPlant::iCtrlType::CoolingOp);
    }

    {
        int CompNum = 2;
        std::string compName = state->dataPlnt->PlantLoop(LoopNum).OpScheme(SchemeNum).EquipList(1).Comp(CompNum).Name;
        // Ensure we have the right component (the TES tank)
        EXPECT_EQ(compName, "ICE THERMAL STORAGE");

        auto CtrlTypeNum = state->dataPlnt->PlantLoop(LoopNum).OpScheme(SchemeNum).EquipList(1).Comp(CompNum).CtrlTypeNum;

        // Could just test this, but want to improve reporting
        // EXPECT_EQ(CtrlTypeNum, PlantCondLoopOperation::DualOp);

        std::string ctrlType = "Unknown";
        if (CtrlTypeNum == DataPlant::iCtrlType::CoolingOp) {
            ctrlType = "CoolingOp";
        } else if (CtrlTypeNum == DataPlant::iCtrlType::HeatingOp) {
            ctrlType = "HeatingOp";
        } else if (CtrlTypeNum == DataPlant::iCtrlType::DualOp) {
            ctrlType = "DualOp";
        }

        EXPECT_EQ(ctrlType, "DualOp") << compName << " has a wrong control type = '" << ctrlType << "'.";
    }

    // We should now alos have two TES SPMs created, and that's all of them
    EXPECT_EQ(state->dataSetPointManager->NumSchTESSetPtMgrs, 2);
    EXPECT_EQ(state->dataSetPointManager->NumAllSetPtMgrs, 2);
}
