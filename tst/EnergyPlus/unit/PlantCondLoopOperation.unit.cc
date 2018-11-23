// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <DataPlant.hh>
#include <EnergyPlus/PlantCondLoopOperation.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

class DistributePlantLoadTest : public EnergyPlusFixture
{

public:
    static void SetUpTestCase()
    {
        EnergyPlusFixture::SetUpTestCase(); // Sets up the base fixture
    }
    static void TearDownTestCase()
    {
    }

    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up individual test cases.

                                    // unit test for plant equipment list load distribution
                                    // set up one plantloop side with 1 branches, 12 components
                                    // using 12 components here to test going beyond the old idd limit of 10 pieces of equipment
        DataPlant::PlantLoop.allocate(1);
        DataPlant::PlantLoop(1).OpScheme.allocate(1);
        DataPlant::PlantLoop(1).OpScheme(1).EquipList.allocate(1);
        auto &thisEquipList(DataPlant::PlantLoop(1).OpScheme(1).EquipList(1));
        thisEquipList.NumComps = 12;
        thisEquipList.Comp.allocate(thisEquipList.NumComps);

        DataPlant::PlantLoop(1).LoopSide.allocate(1);
        DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
        DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(thisEquipList.NumComps);
        auto &thisBranch(DataPlant::PlantLoop(1).LoopSide(1).Branch(1));

        for (int compNum = 1; compNum <= DataPlant::PlantLoop(1).OpScheme(1).EquipList(1).NumComps; ++compNum) {
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
        auto &thisBranch(DataPlant::PlantLoop(1).LoopSide(1).Branch(1));
        for (int compNum = 1; compNum <= DataPlant::PlantLoop(1).OpScheme(1).EquipList(1).NumComps; ++compNum) {
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
    // Loop demand 550W
    auto &thisBranch(DataPlant::PlantLoop(1).LoopSide(1).Branch(1));
    DistributePlantLoadTest::ResetLoads();
    Real64 loopDemand = 550.0;
    Real64 remainingLoopDemand = 0.0;
    DataPlant::PlantLoop(1).LoadDistribution = DataPlant::SequentialLoading;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
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

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
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

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
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

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
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


    //Duplicate tests from engineering reference examples for Sequential
    DataPlant::PlantLoop(1).OpScheme(1).EquipList(1).NumComps = 2;
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

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 5.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 25W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 25.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 25.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 50W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 50.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 10.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 100W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 100.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 60.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // 150W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 150.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);

    // 200W
    DistributePlantLoadTest::ResetLoads();
    loopDemand = 200.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisBranch.Comp(1).MyLoad, 40.0);
    EXPECT_EQ(thisBranch.Comp(2).MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 60.0);
}

TEST_F(EnergyPlusFixture, DistributePlantLoad_Uniform)
{
    // unit test for plant equipment list load distribution
    // set up one plantloop side with 1 branches, 5 components
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).OpScheme.allocate(1);
    DataPlant::PlantLoop(1).OpScheme(1).EquipList.allocate(1);
    auto &thisEquipList(DataPlant::PlantLoop(1).OpScheme(1).EquipList(1));
    thisEquipList.NumComps = 5;
    thisEquipList.Comp.allocate(thisEquipList.NumComps);

    DataPlant::PlantLoop(1).LoopSide.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(thisEquipList.NumComps);
    auto &thisBranch(DataPlant::PlantLoop(1).LoopSide(1).Branch(1));

    DataPlant::PlantLoop(1).LoadDistribution = DataPlant::UniformLoading;

    // set up equipment list data
    thisEquipList.Comp(1).CompNumPtr = 1;
    thisEquipList.Comp(2).CompNumPtr = 2;
    thisEquipList.Comp(3).CompNumPtr = 3;
    thisEquipList.Comp(4).CompNumPtr = 4;
    thisEquipList.Comp(5).CompNumPtr = 5;

    thisEquipList.Comp(1).BranchNumPtr = 1;
    thisEquipList.Comp(2).BranchNumPtr = 1;
    thisEquipList.Comp(3).BranchNumPtr = 1;
    thisEquipList.Comp(4).BranchNumPtr = 1;
    thisEquipList.Comp(5).BranchNumPtr = 1;

    // set up individual component data - start with 5 equal size, all available
    auto &thisComp1(thisBranch.Comp(1));
    thisComp1.Available = true;
    thisComp1.OptLoad = 90.0;
    thisComp1.MaxLoad = 100.0;
    thisComp1.MinLoad = 0.0;
    thisComp1.MyLoad = 0.0;

    auto &thisComp2(thisBranch.Comp(2));
    thisComp2.Available = true;
    thisComp2.OptLoad = 90.0;
    thisComp2.MaxLoad = 100.0;
    thisComp2.MinLoad = 0.0;
    thisComp2.MyLoad = 0.0;

    auto &thisComp3(thisBranch.Comp(3));
    thisComp3.Available = true;
    thisComp3.OptLoad = 90.0;
    thisComp3.MaxLoad = 100.0;
    thisComp3.MinLoad = 0.0;
    thisComp3.MyLoad = 0.0;

    auto &thisComp4(thisBranch.Comp(4));
    thisComp4.Available = true;
    thisComp4.OptLoad = 90.0;
    thisComp4.MaxLoad = 100.0;
    thisComp4.MinLoad = 0.0;
    thisComp4.MyLoad = 0.0;

    auto &thisComp5(thisBranch.Comp(5));
    thisComp5.Available = true;
    thisComp5.OptLoad = 90.0;
    thisComp5.MaxLoad = 100.0;
    thisComp5.MinLoad = 0.0;
    thisComp5.MyLoad = 0.0;

    // Loop demand 550W
    Real64 loopDemand = 550.0;
    Real64 remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 100.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(thisComp3.MyLoad, 100.0);
    EXPECT_EQ(thisComp4.MyLoad, 100.0);
    EXPECT_EQ(thisComp5.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 50.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;


    // Loop demand 50W
    loopDemand = 50.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 10.0);
    EXPECT_EQ(thisComp2.MyLoad, 10.0);
    EXPECT_EQ(thisComp3.MyLoad, 10.0);
    EXPECT_EQ(thisComp4.MyLoad, 10.0);
    EXPECT_EQ(thisComp5.MyLoad, 10.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;

    // Loop demand 320W, one smaller equipment
    // "extra" load should be distributed sequentially amongst the other equipment
    // component 3 unavailable
    loopDemand = 320.0;
    remainingLoopDemand = 0.0;
    thisComp4.MaxLoad = 50.0;
    thisComp3.Available = false;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 100.0);
    EXPECT_EQ(thisComp2.MyLoad, 90.0);
    EXPECT_EQ(thisComp3.MyLoad, 0.0);
    EXPECT_EQ(thisComp4.MyLoad, 50.0);
    EXPECT_EQ(thisComp5.MyLoad, 80.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    //Duplicate tests from engineering reference examples
    thisEquipList.NumComps = 2;
    thisComp1.MaxLoad = 40.0;
    thisComp1.MinLoad = 0.2 * 40.0;
    thisComp1.OptLoad = 0.6 * 40.0;
    thisComp2.MaxLoad = 100.0;
    thisComp2.MinLoad = 0.15 * 100.0;
    thisComp2.OptLoad = 0.4 * 100.0;

    // 10W
    loopDemand = 10.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 5.0);
    EXPECT_EQ(thisComp2.MyLoad, 5.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 25W
    loopDemand = 25.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 12.5);
    EXPECT_EQ(thisComp2.MyLoad, 12.5);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 50W
    loopDemand = 50.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 25.0);
    EXPECT_EQ(thisComp2.MyLoad, 25.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 100W
    loopDemand = 100.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 40.0);
    EXPECT_EQ(thisComp2.MyLoad, 60.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 150W
    loopDemand = 150.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 40.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 200W
    loopDemand = 200.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 40.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 60.0);

}

TEST_F(EnergyPlusFixture, DistributePlantLoad_Optimal)
{
    // unit test for plant equipment list load distribution
    // set up one plantloop side with 1 branch, 5 components
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).OpScheme.allocate(1);
    DataPlant::PlantLoop(1).OpScheme(1).EquipList.allocate(1);
    auto &thisEquipList(DataPlant::PlantLoop(1).OpScheme(1).EquipList(1));
    thisEquipList.NumComps = 5;
    thisEquipList.Comp.allocate(thisEquipList.NumComps);

    DataPlant::PlantLoop(1).LoopSide.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(thisEquipList.NumComps);
    auto &thisBranch(DataPlant::PlantLoop(1).LoopSide(1).Branch(1));

    DataPlant::PlantLoop(1).LoadDistribution = DataPlant::OptimalLoading;

    // set up equipment list data
    thisEquipList.Comp(1).CompNumPtr = 1;
    thisEquipList.Comp(2).CompNumPtr = 2;
    thisEquipList.Comp(3).CompNumPtr = 3;
    thisEquipList.Comp(4).CompNumPtr = 4;
    thisEquipList.Comp(5).CompNumPtr = 5;

    thisEquipList.Comp(1).BranchNumPtr = 1;
    thisEquipList.Comp(2).BranchNumPtr = 1;
    thisEquipList.Comp(3).BranchNumPtr = 1;
    thisEquipList.Comp(4).BranchNumPtr = 1;
    thisEquipList.Comp(5).BranchNumPtr = 1;

    // set up individual component data - start with all available
    auto &thisComp1(thisBranch.Comp(1));
    thisComp1.Available = true;
    thisComp1.OptLoad = 90.0;
    thisComp1.MaxLoad = 100.0;
    thisComp1.MinLoad = 0.0;
    thisComp1.MyLoad = 0.0;

    auto &thisComp2(thisBranch.Comp(2));
    thisComp2.Available = true;
    thisComp2.OptLoad = 90.0;
    thisComp2.MaxLoad = 100.0;
    thisComp2.MinLoad = 0.0;
    thisComp2.MyLoad = 0.0;

    auto &thisComp3(thisBranch.Comp(3));
    thisComp3.Available = true;
    thisComp3.OptLoad = 90.0;
    thisComp3.MaxLoad = 100.0;
    thisComp3.MinLoad = 0.0;
    thisComp3.MyLoad = 0.0;

    auto &thisComp4(thisBranch.Comp(4));
    thisComp4.Available = true;
    thisComp4.OptLoad = 45.0;
    thisComp4.MaxLoad = 50.0;
    thisComp4.MinLoad = 0.0;
    thisComp4.MyLoad = 0.0;

    auto &thisComp5(thisBranch.Comp(5));
    thisComp5.Available = true;
    thisComp5.OptLoad = 90.0;
    thisComp5.MaxLoad = 100.0;
    thisComp5.MinLoad = 0.0;
    thisComp5.MyLoad = 0.0;

    // Loop demand 550W
    Real64 loopDemand = 550.0;
    Real64 remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 100.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(thisComp3.MyLoad, 100.0);
    EXPECT_EQ(thisComp4.MyLoad, 50.0);
    EXPECT_EQ(thisComp5.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 100.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;


    // Loop demand 50W
    loopDemand = 440.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 99.0);
    EXPECT_EQ(thisComp2.MyLoad, 97.0);
    EXPECT_EQ(thisComp3.MyLoad, 97.0);
    EXPECT_EQ(thisComp4.MyLoad, 50.0);
    EXPECT_EQ(thisComp5.MyLoad, 97.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;

    // Loop demand 320W
    // component 3 unavailable
    loopDemand = 340.0;
    remainingLoopDemand = 0.0;
    thisComp3.Available = false;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 97.5);
    EXPECT_EQ(thisComp2.MyLoad, 96.25);
    EXPECT_EQ(thisComp3.MyLoad, 0.0);
    EXPECT_EQ(thisComp4.MyLoad, 50.0);
    EXPECT_EQ(thisComp5.MyLoad, 96.25);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    //Duplicate tests from engineering reference examples
    thisEquipList.NumComps = 2;
    thisComp1.MaxLoad = 40.0;
    thisComp1.MinLoad = 0.2 * 40.0;
    thisComp1.OptLoad = 0.6 * 40.0;
    thisComp2.MaxLoad = 100.0;
    thisComp2.MinLoad = 0.15 * 100.0;
    thisComp2.OptLoad = 0.4 * 100.0;

    // 5W
    loopDemand = 5.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 5.0);
    EXPECT_EQ(thisComp2.MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 25W
    loopDemand = 25.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 24.0);
    EXPECT_EQ(thisComp2.MyLoad, 1.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 50W
    loopDemand = 50.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 24.0);
    EXPECT_EQ(thisComp2.MyLoad, 26.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 100W
    loopDemand = 100.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 40.0);
    EXPECT_EQ(thisComp2.MyLoad, 60.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 150W
    loopDemand = 150.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 40.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 200W
    loopDemand = 200.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 40.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 60.0);

}

TEST_F(EnergyPlusFixture, DistributePlantLoad_UniformPLR)
{
    // unit test for plant equipment list load distribution
    // set up one plantloop side with 1 branches, 5 components
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).OpScheme.allocate(1);
    DataPlant::PlantLoop(1).OpScheme(1).EquipList.allocate(1);
    auto &thisEquipList(DataPlant::PlantLoop(1).OpScheme(1).EquipList(1));
    thisEquipList.NumComps = 5;
    thisEquipList.Comp.allocate(thisEquipList.NumComps);

    DataPlant::PlantLoop(1).LoopSide.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(thisEquipList.NumComps);
    auto &thisBranch(DataPlant::PlantLoop(1).LoopSide(1).Branch(1));

    DataPlant::PlantLoop(1).LoadDistribution = DataPlant::UniformPLRLoading;

    // set up equipment list data
    thisEquipList.Comp(1).CompNumPtr = 1;
    thisEquipList.Comp(2).CompNumPtr = 2;
    thisEquipList.Comp(3).CompNumPtr = 3;
    thisEquipList.Comp(4).CompNumPtr = 4;
    thisEquipList.Comp(5).CompNumPtr = 5;

    thisEquipList.Comp(1).BranchNumPtr = 1;
    thisEquipList.Comp(2).BranchNumPtr = 1;
    thisEquipList.Comp(3).BranchNumPtr = 1;
    thisEquipList.Comp(4).BranchNumPtr = 1;
    thisEquipList.Comp(5).BranchNumPtr = 1;

    // set up individual component data - start with all available, one smaller
    auto &thisComp1(thisBranch.Comp(1));
    thisComp1.Available = true;
    thisComp1.OptLoad = 90.0;
    thisComp1.MaxLoad = 100.0;
    thisComp1.MinLoad = 0.0;
    thisComp1.MyLoad = 0.0;

    auto &thisComp2(thisBranch.Comp(2));
    thisComp2.Available = true;
    thisComp2.OptLoad = 90.0;
    thisComp2.MaxLoad = 100.0;
    thisComp2.MinLoad = 0.0;
    thisComp2.MyLoad = 0.0;

    auto &thisComp3(thisBranch.Comp(3));
    thisComp3.Available = true;
    thisComp3.OptLoad = 90.0;
    thisComp3.MaxLoad = 100.0;
    thisComp3.MinLoad = 0.0;
    thisComp3.MyLoad = 0.0;

    auto &thisComp4(thisBranch.Comp(4));
    thisComp4.Available = true;
    thisComp4.OptLoad = 45.0;
    thisComp4.MaxLoad = 50.0;
    thisComp4.MinLoad = 0.0;
    thisComp4.MyLoad = 0.0;

    auto &thisComp5(thisBranch.Comp(5));
    thisComp5.Available = true;
    thisComp5.OptLoad = 90.0;
    thisComp5.MaxLoad = 100.0;
    thisComp5.MinLoad = 0.0;
    thisComp5.MyLoad = 0.0;

    // Loop demand 550W
    Real64 loopDemand = 550.0;
    Real64 remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 100.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(thisComp3.MyLoad, 100.0);
    EXPECT_EQ(thisComp4.MyLoad, 50.0);
    EXPECT_EQ(thisComp5.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 100.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;


    // Loop demand 45W
    loopDemand = 45.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 10.0);
    EXPECT_EQ(thisComp2.MyLoad, 10.0);
    EXPECT_EQ(thisComp3.MyLoad, 10.0);
    EXPECT_EQ(thisComp4.MyLoad, 5.0);
    EXPECT_EQ(thisComp5.MyLoad, 10.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;

    // Loop demand 280W
    // "extra" load should be distributed sequentially amongst the other equipment
    // component 3 unavailable
    loopDemand = 280;
    remainingLoopDemand = 0.0;
    thisComp3.Available = false;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 80.0);
    EXPECT_EQ(thisComp2.MyLoad, 80.0);
    EXPECT_EQ(thisComp3.MyLoad, 0.0);
    EXPECT_EQ(thisComp4.MyLoad, 40.0);
    EXPECT_EQ(thisComp5.MyLoad, 80.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    //Duplicate tests from engineering reference examples
    thisEquipList.NumComps = 2;
    thisComp1.MaxLoad = 40.0;
    thisComp1.MinLoad = 0.2 * 40.0;
    thisComp1.OptLoad = 0.6 * 40.0;
    thisComp2.MaxLoad = 100.0;
    thisComp2.MinLoad = 0.15 * 100.0;
    thisComp2.OptLoad = 0.4 * 100.0;

    // 5W
    loopDemand = 5.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 5.0);
    EXPECT_EQ(thisComp2.MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 10W
    loopDemand = 10.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 10.0);
    EXPECT_EQ(thisComp2.MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 25W
    loopDemand = 25.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisComp1.MyLoad, 25.0,0.1);
    EXPECT_NEAR(thisComp2.MyLoad, 0.0,0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 50W
    loopDemand = 50.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisComp1.MyLoad, 14.29,0.1);
    EXPECT_NEAR(thisComp2.MyLoad, 35.71,0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 100W
    loopDemand = 100.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisComp1.MyLoad, 28.57,0.1);
    EXPECT_NEAR(thisComp2.MyLoad, 71.43,0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 150W
    loopDemand = 150.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 40.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);

}

TEST_F(EnergyPlusFixture, DistributePlantLoad_SequentialUniformPLR)
{
    // unit test for plant equipment list load distribution
    // set up one plantloop side with 1 branches, 5 components
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).OpScheme.allocate(1);
    DataPlant::PlantLoop(1).OpScheme(1).EquipList.allocate(1);
    auto &thisEquipList(DataPlant::PlantLoop(1).OpScheme(1).EquipList(1));
    thisEquipList.NumComps = 5;
    thisEquipList.Comp.allocate(thisEquipList.NumComps);

    DataPlant::PlantLoop(1).LoopSide.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(thisEquipList.NumComps);
    auto &thisBranch(DataPlant::PlantLoop(1).LoopSide(1).Branch(1));

    DataPlant::PlantLoop(1).LoadDistribution = DataPlant::SequentialUniformPLRLoading;

    // set up equipment list data
    thisEquipList.Comp(1).CompNumPtr = 1;
    thisEquipList.Comp(2).CompNumPtr = 2;
    thisEquipList.Comp(3).CompNumPtr = 3;
    thisEquipList.Comp(4).CompNumPtr = 4;
    thisEquipList.Comp(5).CompNumPtr = 5;

    thisEquipList.Comp(1).BranchNumPtr = 1;
    thisEquipList.Comp(2).BranchNumPtr = 1;
    thisEquipList.Comp(3).BranchNumPtr = 1;
    thisEquipList.Comp(4).BranchNumPtr = 1;
    thisEquipList.Comp(5).BranchNumPtr = 1;

    // set up individual component data - start with all available, one smaller
    auto &thisComp1(thisBranch.Comp(1));
    thisComp1.Available = true;
    thisComp1.OptLoad = 90.0;
    thisComp1.MaxLoad = 100.0;
    thisComp1.MinLoad = 0.0;
    thisComp1.MyLoad = 0.0;

    auto &thisComp2(thisBranch.Comp(2));
    thisComp2.Available = true;
    thisComp2.OptLoad = 90.0;
    thisComp2.MaxLoad = 100.0;
    thisComp2.MinLoad = 0.0;
    thisComp2.MyLoad = 0.0;

    auto &thisComp3(thisBranch.Comp(3));
    thisComp3.Available = true;
    thisComp3.OptLoad = 90.0;
    thisComp3.MaxLoad = 100.0;
    thisComp3.MinLoad = 0.0;
    thisComp3.MyLoad = 0.0;

    auto &thisComp4(thisBranch.Comp(4));
    thisComp4.Available = true;
    thisComp4.OptLoad = 45.0;
    thisComp4.MaxLoad = 50.0;
    thisComp4.MinLoad = 0.0;
    thisComp4.MyLoad = 0.0;

    auto &thisComp5(thisBranch.Comp(5));
    thisComp5.Available = true;
    thisComp5.OptLoad = 90.0;
    thisComp5.MaxLoad = 100.0;
    thisComp5.MinLoad = 0.0;
    thisComp5.MyLoad = 0.0;

    // Loop demand 550W
    Real64 loopDemand = 550.0;
    Real64 remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 100.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(thisComp3.MyLoad, 100.0);
    EXPECT_EQ(thisComp4.MyLoad, 50.0);
    EXPECT_EQ(thisComp5.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 100.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;


    // Loop demand 45W
    loopDemand = 45.0;
    remainingLoopDemand = 0.0;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 45.0);
    EXPECT_EQ(thisComp2.MyLoad, 0.0);
    EXPECT_EQ(thisComp3.MyLoad, 0.0);
    EXPECT_EQ(thisComp4.MyLoad, 0.0);
    EXPECT_EQ(thisComp5.MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;

    // Loop demand 225W
    // "extra" load should be distributed sequentially amongst the other equipment
    // component 3 unavailable
    loopDemand = 225;
    remainingLoopDemand = 0.0;
    thisComp3.Available = false;

    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 90.0);
    EXPECT_EQ(thisComp2.MyLoad, 90.0);
    EXPECT_EQ(thisComp3.MyLoad, 0.0);
    EXPECT_EQ(thisComp4.MyLoad, 45.0);
    EXPECT_EQ(thisComp5.MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    thisComp3.MyLoad = 0.0;
    thisComp4.MyLoad = 0.0;
    thisComp5.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    //Duplicate tests from engineering reference examples
    thisEquipList.NumComps = 2;
    thisComp1.MaxLoad = 40.0;
    thisComp1.MinLoad = 0.2 * 40.0;
    thisComp1.OptLoad = 0.6 * 40.0;
    thisComp2.MaxLoad = 100.0;
    thisComp2.MinLoad = 0.15 * 100.0;
    thisComp2.OptLoad = 0.4 * 100.0;

    // 5W
    loopDemand = 5.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 5.0);
    EXPECT_EQ(thisComp2.MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 10W
    loopDemand = 10.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 10.0);
    EXPECT_EQ(thisComp2.MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 25W
    loopDemand = 25.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 25.0);
    EXPECT_EQ(thisComp2.MyLoad, 0.0);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 50W
    loopDemand = 50.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisComp1.MyLoad, 14.3,0.1);
    EXPECT_NEAR(thisComp2.MyLoad, 35.71,0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 100W
    loopDemand = 100.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_NEAR(thisComp1.MyLoad, 28.6,0.1);
    EXPECT_NEAR(thisComp2.MyLoad, 71.43,0.1);
    EXPECT_EQ(remainingLoopDemand, 0.0);

    // reset loads
    thisComp1.MyLoad = 0.0;
    thisComp2.MyLoad = 0.0;
    remainingLoopDemand = 0.0;

    // 150W
    loopDemand = 150.0;
    PlantCondLoopOperation::DistributePlantLoad(1, 1, 1, 1, loopDemand, remainingLoopDemand);
    EXPECT_EQ(thisComp1.MyLoad, 40.0);
    EXPECT_EQ(thisComp2.MyLoad, 100.0);
    EXPECT_EQ(remainingLoopDemand, 10.0);

}
